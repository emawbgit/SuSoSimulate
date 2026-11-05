# ---------------------------------------------------------------------------
# coverage.R -- branch/option coverage accounting and coverage-guided repair
# ---------------------------------------------------------------------------

new_coverage <- function() {
  cov <- new.env()
  cov$counts <- new.env(hash = TRUE)  # "nodeid:T|F" -> n interviews
  cov$opt <- new.env(hash = TRUE)     # var -> named counts by code
  cov$issues <- new.env(hash = TRUE)  # untranslatable / runtime problems
  cov
}

cov_commit <- function(cov, st, sign = 1) {
  for (k in ls(st$hits)) assign(k, (env_get(cov$counts, k) %||% 0) + sign, envir = cov$counts)
  for (v in ls(st$opt)) {
    tb <- table(get(v, envir = st$opt))
    cur <- env_get(cov$opt, v) %||% stats::setNames(numeric(0), character(0))
    nm <- union(names(cur), names(tb))
    new <- stats::setNames(rep(0, length(nm)), nm)
    new[names(cur)] <- cur
    new[names(tb)] <- new[names(tb)] + sign * as.numeric(tb)
    assign(v, new, envir = cov$opt)
  }
}

cond_nodes <- function(qn) {
  Filter(function(nd) !is.null(nd$enc) && nd$kind != "static", lapply(qn$order, function(i) qn$nodes[[i]]))
}

missing_branches <- function(qn, cov) {
  out <- character()
  for (nd in cond_nodes(qn)) for (s in c("T", "F")) {
    k <- paste0(nd$id, ":", s)
    if ((env_get(cov$counts, k) %||% 0) <= 0) out <- c(out, k)
  }
  out
}

repair_coverage <- function(qn, cfg, frame, states, cov, verbose = TRUE, progress = function(...) NULL) {
  # outcomes the questionnaire's own validation bounds rule out can never be hit: don't spend the budget on them
  hopeless <- function(k) nzchar(unreachable_hint(qn, qn$nodes[[sub(":[TF]$", "", k)]], need = grepl(":T$", k)))
  miss0 <- missing_branches(qn, cov)
  dead <- miss0[vapply(miss0, hopeless, TRUE)]
  miss0 <- setdiff(miss0, dead)
  if (!length(miss0)) return(states)
  patience <- max(20L, cfg$repair_budget %/% 10L)        # stop after this many candidates without progress
  sm_msg(verbose, sprintf("Coverage repair: %d uncovered branch outcomes (%d unreachable skipped), budget %d candidates",
                          length(miss0), length(dead), cfg$repair_budget))
  vmap <- list()
  for (id in qn$order) { v <- qn$nodes[[id]]$varname %||% ""; if (nzchar(v)) vmap[[v]] <- id }
  tries <- 0L; swaps <- 0L; last_gain <- 0L
  t0 <- Sys.time(); t_end <- t0 + (cfg$repair_time %||% Inf); shown <- t0; pv <- 0.72
  while (tries < cfg$repair_budget && tries - last_gain < patience && Sys.time() < t_end) {
    miss <- setdiff(missing_branches(qn, cov), dead)
    if (!length(miss)) break
    tries <- tries + 1L
    # time left: the smallest of the time limit, the remaining budget and the patience window
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs")); per <- if (tries > 1) el / (tries - 1) else NA
    left <- min(as.numeric(difftime(t_end, Sys.time(), units = "secs")),
                per * (cfg$repair_budget - tries + 1), per * (patience - (tries - last_gain) + 1), na.rm = TRUE)
    pv <- max(pv, 0.72 + 0.12 * min(1, el / max(1, el + left)))          # never moves backwards
    progress(pv,
             sprintf("Repairing coverage: %d outcomes left, candidate %d - at most %s left", length(miss), tries, fmt_secs(left)))
    if (verbose && difftime(Sys.time(), shown, units = "secs") > 10) {
      message(sprintf("  repair: %d candidates, %d swapped, %d outcomes uncovered, at most %s left", tries - 1, swaps, length(miss), fmt_secs(left)))
      shown <- Sys.time()
    }
    keys <- lapply(states, function(st) ls(st$hits))
    repl <- which(vapply(keys, function(k) all(vapply(k, function(x) env_get(cov$counts, x) >= 2, TRUE)), TRUE))
    if (length(repl)) {
      i <- pick(repl)
      unit <- states[[i]]$unit
    } else if (isTRUE(cfg$allow_augment)) {
      i <- length(states) + 1L
      unit <- states[[sample.int(length(states), 1)]]$unit
      unit$ssu <- max(vapply(states, function(s) s$unit$ssu, 0)) + 1
    } else break
    mid <- unique(sub(":[TF]$", "", miss))
    mid <- mid[(tries - 1L) %% length(mid) + 1L]            # one uncovered item per candidate, in rotation
    focus <- unique(unlist(lapply(mid, function(id) qn$nodes[[id]]$enc$refs)))
    st <- generate_interview(qn, cfg, unit, cov, tune = list(target_prob = 0.9, focus = focus,
                                                             enable = setdiff(focus_vars(qn, vmap, mid), focus)))
    if (length(intersect(ls(st$hits), miss))) {
      if (i <= length(states)) cov_commit(cov, states[[i]], -1)
      cov_commit(cov, st, 1)
      states[[i]] <- st
      swaps <- swaps + 1L; last_gain <- tries
    }
  }
  left <- length(setdiff(missing_branches(qn, cov), dead))
  sm_msg(verbose, sprintf("  %d interviews replaced in %d tries; %d branch outcomes still uncovered%s", swaps, tries, left,
                          if (Sys.time() >= t_end) sprintf(" (stopped at repair_time = %ss)", cfg$repair_time) else ""))
  states
}

# Variables to steer for uncovered node ids: those in their conditions, plus, transitively, those
# that enable them and their ancestors (a long chain of "== 1" gates is otherwise rarely walked to the end)
focus_vars <- function(qn, vmap, ids, depth = 15L) {
  seen_n <- character(); out <- character()
  todo <- ids
  while (length(todo) && depth > 0L) {
    depth <- depth - 1L; nxt <- character()
    for (id in setdiff(todo, seen_n)) {
      seen_n <- c(seen_n, id)
      nd <- qn$nodes[[id]]
      up <- id; p <- nd$parent
      while (length(p) == 1 && !is.na(p) && nzchar(p) && !is.null(qn$nodes[[p]])) { up <- c(up, p); p <- qn$nodes[[p]]$parent }
      r <- unique(unlist(lapply(up, function(u) qn$nodes[[u]]$enc$refs)))
      out <- union(out, r)
      nxt <- c(nxt, unlist(vmap[r], use.names = FALSE))
    }
    todo <- unique(nxt)
  }
  out
}

coverage_report <- function(qn, cov, n) {
  issues <- mget(ls(cov$issues), envir = cov$issues)
  iss_by_id <- tapply(vapply(issues, function(x) paste0(x$kind, ": ", x$msg), ""),
                      vapply(issues, `[[`, "", "id"), paste, collapse = " ; ")
  rows <- lapply(cond_nodes(qn), function(nd) {
    t <- env_get(cov$counts, paste0(nd$id, ":T")) %||% 0
    f <- env_get(cov$counts, paste0(nd$id, ":F")) %||% 0
    lk <- nd$path[length(nd$path)]
    data.frame(
      item = if (nzchar(nd$varname %||% "")) nd$varname else strip_html(nd$text),
      kind = nd$kind,
      level = if (length(nd$path)) qn$levels[[lk]]$file else qn$varname,
      condition = gsub("\\s+", " ", nd$cond_src),
      n_true = t, n_false = f,
      status = if (t > 0 && f > 0) "covered" else if (t > 0) "TRUE only" else if (f > 0) "FALSE only" else "never evaluated",
      note = if (t > 0 && f > 0) "" else unreachable_hint(qn, nd, need = if (t > 0) FALSE else if (f > 0) TRUE else NA),
      issue = if (nd$id %in% names(iss_by_id)) iss_by_id[[nd$id]] else "",
      stringsAsFactors = FALSE)
  })
  br <- if (length(rows)) do.call(rbind, rows) else data.frame()
  opts <- lapply(qn_questions(qn, function(nd) nd$qtype %in% c("single", "multi") && is.null(nd$lnk) && nrow(nd$options) > 0), function(nd) {
    cnt <- env_get(cov$opt, nd$varname) %||% numeric(0)
    used <- names(cnt)[cnt > 0]
    codes <- fmt_num(nd$options$code)
    data.frame(variable = nd$varname, n_options = length(codes), n_used = sum(codes %in% used),
               unused = paste(setdiff(codes, used), collapse = " "), stringsAsFactors = FALSE)
  })
  op <- if (length(opts)) do.call(rbind, opts) else data.frame()
  list(branches = br, options = op,
       summary = c(interviews = n, conditions = nrow(br),
                   covered = if (nrow(br)) sum(br$status == "covered") else 0,
                   branch_coverage = if (nrow(br)) mean(c(br$n_true > 0, br$n_false > 0)) else 1,
                   option_coverage = if (nrow(op)) sum(op$n_used) / sum(op$n_options) else 1))
}

# Heuristic: for `var <op> const` conditions, can the missing outcome occur
# for any value permitted by var's hard validation bounds?
unreachable_hint <- function(qn, nd, need) {
  a <- nd$enc$ast
  if (is.na(need) || is.null(a) || a$k != "bin" || !(a$op %in% c("==", "!=", "<", ">", "<=", ">="))) return("")
  if (a$l$k != "id" || a$r$k != "num" && !(a$r$k == "un" && a$r$x$k == "num")) return("")
  v <- env_get(qn$by_name, a$l$name)
  if (is.null(v) || v$kind != "question" || all(is.na(v$bounds))) return("")
  k <- if (a$r$k == "num") a$r$v else -a$r$x$v
  lo <- v$bounds[1]; hi <- v$bounds[2]
  cand <- c(lo, hi, k - 1, k, k + 1)
  cand <- cand[!is.na(cand) & (is.na(lo) | cand >= lo) & (is.na(hi) | cand <= hi)]
  ok <- vapply(cand, function(x) rt_bin(a$op, x, k), TRUE)
  if (!any(ok == need)) sprintf("unreachable: %s validation bounds [%s, %s] exclude this outcome", v$varname, fmt_num(lo), fmt_num(hi))
  else ""
}
