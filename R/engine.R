# ---------------------------------------------------------------------------
# engine.R -- generates one fully routed interview.
#
# The questionnaire tree is walked repeatedly ("fill" passes) until the state
# is stable: each pass evaluates enabling conditions with the current answers,
# answers newly enabled questions and clears newly disabled ones. A final
# "record" pass re-walks the stable state to log branch coverage, validation
# errors, exported roster rows and the answer sequence used for paradata.
# ---------------------------------------------------------------------------

new_state <- function(unit) {
  st <- new.env()
  st$vals <- new.env(hash = TRUE)      # "var|rv" -> value
  st$enabled <- new.env(hash = TRUE)   # "var|rv" -> TRUE when enabled
  st$decided <- new.env(hash = TRUE)   # answer/non-response already drawn
  st$rows <- new.env(hash = TRUE)      # "level|parent rv" -> row codes
  st$fallback <- new.env(hash = TRUE)  # stable random outcomes for untranslatable expressions
  st$unit <- unit
  st$now <- unit$start_time
  st$changed <- FALSE
  st$vv <- new.env(hash = TRUE)        # per-variable change counter ("__rows__" for roster rows); keys the memo
  st$memo <- new.env(hash = TRUE)      # "node|rv" -> c(signature, result)
  st$off <- new.env(hash = TRUE)       # disabled groups / rows whose subtree is already cleared
  st
}

make_ctx <- function(G, node, rv, extra = NULL) {
  ctx <- list(st = G$st, qn = G$qn, rv = rv, path = node$path,
              self = if (node$kind %in% c("question", "variable") && nzchar(node$varname %||% "")) node$varname else NULL)
  if (length(extra)) ctx[names(extra)] <- extra
  ctx
}

note_issue <- function(G, node, kind, msg) {
  k <- paste0(kind, "|", node$id)
  if (!env_has(G$cov$issues, k))
    assign(k, list(kind = kind, var = node$varname %||% "", id = node$id, msg = msg), envir = G$cov$issues)
}

# Evaluate a compiled expression to a logical. Unsupported constructs fall back
# to a stable random draw (conditions) or to `unsup` (validations / filters);
# C# runtime exceptions (e.g. `.Value` on null) evaluate to FALSE, as in SuSo.
eval_bool <- function(G, cc, node, rv, extra = NULL, unsup = NULL, fkey = NULL) {
  fb <- function(msg) {
    note_issue(G, node, "unsupported", msg)
    if (!is.null(unsup)) return(unsup)
    v <- env_get(G$st$fallback, fkey)
    if (is.null(v)) { v <- stats::runif(1) < G$cfg$unsupported_p; assign(fkey, v, envir = G$st$fallback) }
    v
  }
  if (!is.null(cc$err)) return(fb(cc$err))
  tryCatch(rt_truthy(cs_eval(cc$call, make_ctx(G, node, rv, extra))),
           sus_unsupported = function(e) fb(conditionMessage(e)),
           error = function(e) { note_issue(G, node, "runtime", conditionMessage(e)); FALSE })
}

eval_value <- function(G, cc, node, rv) {
  if (is.null(cc) || !is.null(cc$err)) { if (!is.null(cc$err)) note_issue(G, node, "unsupported", cc$err); return(NA) }
  tryCatch(cs_eval(cc$call, make_ctx(G, node, rv)),
           sus_unsupported = function(e) { note_issue(G, node, "unsupported", conditionMessage(e)); NA },
           error = function(e) NA)
}

eval_cond <- function(G, node, rv) {
  cc <- node$enc
  if (is.null(cc)) return(TRUE)
  fkey <- paste0(node$id, "|", rv_str(rv))
  # same state as at the last evaluation -> same result (skipped for clock-dependent conditions)
  sig <- sum(unlist(mget(node$memo_refs, envir = G$st$vv, ifnotfound = 0L), use.names = FALSE))
  m <- get0(fkey, envir = G$st$memo, inherits = FALSE)
  if (!is.null(m) && m[1] == sig) res <- as.logical(m[2]) else {
    res <- eval_bool(G, cc, node, rv, fkey = fkey)
    if (!isTRUE(node$clock_dep)) assign(fkey, c(sig, res), envir = G$st$memo)
  }
  if (G$rec) {
    k <- paste0(node$id, if (res) ":T" else ":F")
    assign(k, (env_get(G$st$hits, k) %||% 0) + 1, envir = G$st$hits)
  }
  res
}

# Failing validations: list of (index, msg, severity)
validate <- function(G, node, rv, errors_only = FALSE) {
  out <- list()
  for (i in seq_along(node$valc)) {
    v <- node$valc[[i]]
    if (errors_only && v$severity != 0) next
    if (!eval_bool(G, v, node, rv, unsup = TRUE)) out[[length(out) + 1]] <- list(index = i, msg = v$msg, severity = v$severity)
  }
  out
}

set_changed <- function(G, key = NULL) { G$st$changed <- TRUE; bump(G, key) }
bump <- function(G, key) {                # key "var|rv" -> var; NULL -> roster rows
  v <- if (is.null(key)) "__rows__" else sub("\\|.*$", "", key)
  assign(v, (get0(v, envir = G$st$vv, inherits = FALSE) %||% 0L) + 1L, envir = G$st$vv)
}

# clear a disabled group / roster row once; nothing can be answered inside it until it is enabled again
clear_off <- function(G, node, rv) {
  k <- st_key(node$id, rv)
  if (is.null(get0(k, envir = G$st$off, inherits = FALSE))) { clear_children(G, node, rv); assign(k, TRUE, envir = G$st$off) }
}
set_on <- function(G, node, rv) env_rm(G$st$off, st_key(node$id, rv))

clear_value <- function(G, node, rv) {
  key <- st_key(node$varname, rv)
  if (is.null(get0(key, envir = G$st$enabled, inherits = FALSE))) return(invisible())
  rm(list = key, envir = G$st$enabled); set_changed(G, key)
  env_rm(G$st$vals, key); env_rm(G$st$decided, key)
}

clear_subtree <- function(G, node, rv) {
  switch(node$kind,
    question = , variable = clear_value(G, node, rv),
    group = for (id in node$children) clear_subtree(G, G$qn$nodes[[id]], rv),
    roster = {
      lk <- node$path[length(node$path)]
      for (cd in env_get(G$st$rows, row_key(lk, rv))) clear_children(G, node, c(rv, cd))
    },
    NULL)
}
clear_children <- function(G, node, rv) for (id in node$children) clear_subtree(G, G$qn$nodes[[id]], rv)

walk_nodes <- function(G, ids, rv) {
  for (id in ids) {
    node <- G$qn$nodes[[id]]
    switch(node$kind,
      group = if (eval_cond(G, node, rv)) { set_on(G, node, rv); walk_nodes(G, node$children, rv) } else {
        clear_off(G, node, rv)
        if (G$rec) log_disabled_vars(G, node, rv)
      },
      roster = walk_roster(G, node, rv),
      question = walk_question(G, node, rv),
      variable = walk_variable(G, node, rv),
      static = NULL)
  }
}

roster_codes <- function(G, node, rv) {
  src <- node$roster
  if (src$type == "fixed") return(src$titles$code)
  trig <- env_get(G$qn$by_name, src$trigger)
  v <- state_value(G$st, trig, rv[seq_len(length(trig$path))])
  if (!answered(v)) return(numeric(0))
  codes <- switch(src$type,
    numeric = { n <- min(floor(as.numeric(v)), G$cfg$max_roster_rows); if (n <= 0) numeric(0) else seq_len(n) - 1 + G$cfg$numeric_roster_base },
    multi = codes_of(v),
    yesno = v$yes,
    list = as.numeric(names(v)))
  if (src$type %in% c("multi", "yesno")) codes <- codes[order(match(codes, trig$options$code))]
  as.numeric(codes)
}

walk_roster <- function(G, node, rv) {
  lk <- node$path[length(node$path)]
  rk <- row_key(lk, rv)
  codes <- roster_codes(G, node, rv)
  old <- env_get(G$st$rows, rk)
  if (!identical(old, codes)) {
    assign(rk, codes, envir = G$st$rows); set_changed(G)            # NULL key: rows
    for (cd in setdiff(old, codes)) { clear_children(G, node, c(rv, cd)); set_on(G, node, c(rv, cd)) }
  }
  for (cd in codes) {
    rv2 <- c(rv, cd)
    if (eval_cond(G, node, rv2)) {
      set_on(G, node, rv2)
      if (G$rec) {
        er <- env_get(G$st$export_rows, lk)
        if (is.null(er)) { er <- new.env(); assign(lk, er, envir = G$st$export_rows) }
        assign(rv_str(rv2), rv2, envir = er)
      }
      walk_nodes(G, node$children, rv2)
    } else {
      clear_off(G, node, rv2)
      if (G$rec) log_disabled_vars(G, node, rv2)
    }
  }
}

# Calculated variables inside a disabled section / roster row -> paradata VariableDisabled
log_disabled_vars <- function(G, node, rv) {
  for (id in node$children) {
    ch <- G$qn$nodes[[id]]
    if (ch$kind == "variable") log_add(G$st$vdis, list(id = ch$id, rv = rv))
    else if (ch$kind == "group") log_disabled_vars(G, ch, rv)
  }
}

walk_question <- function(G, node, rv) {
  st <- G$st; cfg <- G$cfg
  key <- st_key(node$varname, rv)
  if (!eval_cond(G, node, rv)) { clear_value(G, node, rv); return(invisible()) }
  if (!env_has(st$enabled, key)) { assign(key, TRUE, envir = st$enabled); set_changed(G, key) }
  if (env_has(st$decided, key)) {
    if (!G$rec && !still_valid(G, node, rv)) { env_rm(st$decided, key); env_rm(st$vals, key); bump(G, key) }
    else { if (G$rec) record_question(G, node, rv, key); return(invisible()) }
  }
  assign(key, TRUE, envir = st$decided)
  val <- NULL
  pre <- st$unit$preload[[node$varname]]
  skip <- (node$scope == 3 && !cfg$answer_hidden) ||
          (node$scope %in% c(1L, 2L) && stats::runif(1) >= cfg$supervisor_answer_rate) ||
          (!isTRUE(node$featured) && !isTRUE(node$is_trigger) && stats::runif(1) < cfg$item_nonresponse) ||
          # questions tested with `== null` / IsAnswered(): leave some unanswered so both paths occur
          (isTRUE(node$null_target) && !isTRUE(node$featured) && !(node$varname %in% G$tune$enable) &&
           stats::runif(1) < (if (node$varname %in% G$tune$focus) 0.5 else G$tune$target_prob * cfg$null_share))
  if (!is.null(pre) && length(rv) == 0) {
    val <- coerce_preload(node, pre)
  } else if (!skip) {
    val <- gen_valid(G, node, rv)
  }
  if (!is.null(val)) { assign(key, val, envir = st$vals); set_changed(G, key) } else { env_rm(st$vals, key); bump(G, key) }
  if (G$rec) record_question(G, node, rv, key)
}

# Answers can become invalid when upstream answers change (cascades, links)
still_valid <- function(G, node, rv) {
  v <- env_get(G$st$vals, st_key(node$varname, rv))
  if (is.null(v)) return(TRUE)
  if (node$qtype == "single" && is.null(node$lnk) && (!is.null(node$cascade_from) || !is.null(node$filterc))) {
    return(v %in% available_options(G, node, rv)$code)
  }
  if (!is.null(node$lnk)) {
    cand <- linked_candidates(G, node, rv)
    keyset <- vapply(cand, function(r) paste(r, collapse = ","), "")
    vv <- if (is.list(v)) v else list(v)
    return(all(vapply(vv, function(r) paste(r, collapse = ",") %in% keyset, TRUE)))
  }
  TRUE
}

record_question <- function(G, node, rv, key) {
  st <- G$st
  v <- env_get(st$vals, key)
  if (is.null(v) || !answered(v)) { st$n_unanswered <- st$n_unanswered + 1L; return(invisible()) }
  st$clock <- st$clock + answer_seconds(node)
  if (isTRUE(node$is_timestamp)) { v <- st$clock; assign(key, v, envir = st$vals); bump(G, key) }
  if (node$qtype == "gps") {   # GPS timestamps are UTC (the clock and timestamp questions are device time)
    v$Timestamp <- st$clock - parse_offset(G$cfg$tz_offset); assign(key, v, envir = st$vals); bump(G, key) }
  log_add(st$alog, list(id = node$id, rv = rv, t = st$clock, kind = "answer"))
  if (node$qtype %in% c("single", "multi") && is.null(node$lnk)) {
    cds <- if (inherits(v, "sus_yesno")) v$yes else codes_of(v)
    cnt <- env_get(st$opt, node$varname) %||% character(0)
    assign(node$varname, c(cnt, fmt_num(cds)), envir = st$opt)
  }
  for (f in validate(G, node, rv)) log_add(st$errors, c(list(id = node$id, rv = rv), f))
}

walk_variable <- function(G, node, rv) {
  key <- st_key(node$varname, rv)
  val <- coerce_variable(eval_value(G, node$exprc, node, rv), node$vtype)
  old <- env_get(G$st$vals, key)
  if (!identical(old, val)) { assign(key, val, envir = G$st$vals); set_changed(G, key) }
  if (!env_has(G$st$enabled, key)) assign(key, TRUE, envir = G$st$enabled)
  if (G$rec) log_add(G$st$alog, list(id = node$id, rv = rv, t = G$st$clock, kind = "variable"))
}

coerce_variable <- function(v, vtype) {
  if (is_na1(v) || (is.list(v) && !is_time(v))) return(NA)
  v <- v[[1]]
  switch(as.character(vtype),
    "1" = as.numeric(trunc(as.numeric(v))), "2" = as.numeric(v), "3" = as.logical(v),
    "4" = v, "5" = as.character(v), v)
}

coerce_preload <- function(node, x) {
  if (node$qtype == "numeric") return(if (node$is_integer) as.integer(x) else as.numeric(x))
  if (node$qtype == "single") return(as.numeric(x))
  if (node$qtype == "multi") return(sus_multi(as.numeric(x)))
  if (node$qtype == "list") return(sus_textlist(seq_along(x), as_utf8(as.character(x))))
  if (node$qtype == "date") return(as.Date(x))
  as_utf8(as.character(x))
}

answer_seconds <- function(node) {
  base <- switch(node$qtype, numeric = 9, single = 7, multi = 14, text = 16, date = 10, gps = 25,
                 list = 25, picture = 35, audio = 40, barcode = 12, area = 60, 8)
  max(1, stats::rlnorm(1, log(base), 0.5))
}

generate_interview <- function(qn, cfg, unit, cov, tune = list()) {
  G <- new.env()
  G$qn <- qn; G$cfg <- cfg; G$cov <- cov; G$rec <- FALSE; G$wide <- FALSE
  G$tune <- utils::modifyList(list(target_prob = cfg$target_prob), tune)
  G$st <- new_state(unit)
  for (p in seq_len(cfg$max_passes)) {
    G$st$changed <- FALSE
    walk_nodes(G, qn$root, numeric(0))
    if (!G$st$changed) break
  }
  st <- G$st
  st$export_rows <- new.env(); st$hits <- new.env(); st$opt <- new.env()
  st$errors <- log_new(); st$alog <- log_new(); st$vdis <- log_new(); st$n_unanswered <- 0L
  st$clock <- unit$start_time
  st$changed <- FALSE
  G$rec <- TRUE
  walk_nodes(G, qn$root, numeric(0))
  st$converged <- !st$changed
  st$end_time <- st$clock
  st
}
