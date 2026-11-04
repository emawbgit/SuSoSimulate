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

repair_coverage <- function(qn, cfg, frame, states, cov, verbose = TRUE) {
  miss0 <- missing_branches(qn, cov)
  if (!length(miss0)) return(states)
  sm_msg(verbose, sprintf("Coverage repair: %d uncovered branch outcomes, budget %d candidates", length(miss0), cfg$repair_budget))
  tries <- 0L; swaps <- 0L
  while (tries < cfg$repair_budget) {
    miss <- missing_branches(qn, cov)
    if (!length(miss)) break
    tries <- tries + 1L
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
    focus <- unique(unlist(lapply(unique(sub(":[TF]$", "", miss)), function(id) qn$nodes[[id]]$enc$refs)))
    st <- generate_interview(qn, cfg, unit, cov, tune = list(target_prob = 0.9, focus = focus))
    if (length(intersect(ls(st$hits), miss))) {
      if (i <= length(states)) cov_commit(cov, states[[i]], -1)
      cov_commit(cov, st, 1)
      states[[i]] <- st
      swaps <- swaps + 1L
    }
  }
  left <- length(missing_branches(qn, cov))
  sm_msg(verbose, sprintf("  %d interviews replaced in %d tries; %d branch outcomes still uncovered", swaps, tries, left))
  states
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
