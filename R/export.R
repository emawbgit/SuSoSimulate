# ---------------------------------------------------------------------------
# export.R -- build Survey Solutions-style tables (main file + one file per
# roster level) and write them as Stata .dta.
#
# Conventions reproduced from SuSo 24.x Stata exports:
#   * id columns: interview__key, interview__id, <ancestor>__id ..., <own>__id
#   * rosters on the same trigger AND nesting path share one file, named after
#     the first such roster; each fixed roster is its own file
#   * disabled roster instances are not exported
#   * list-triggered roster files carry the item text (label "Roster list question")
#   * multi-select: var__<code> (negative codes var__n<abs>), 1/0 or rank if ordered
#   * yes/no: 1 yes / 0 no; linked multi: var__0..var__{w-1} holding row codes
#   * text list: var__k (text) + var__kc (code) pairs, padded to MaxAnswerCount
#   * GPS: var__Latitude/Longitude/Accuracy/Altitude (num), var__Timestamp (str)
#   * dates/timestamps as ISO strings
#   * disabled -> system missing / ""; enabled-unanswered -> NA / "##N/A##"
#   * main file ends with sssys_irnd, has__errors, interview__status, assignment__id
# ---------------------------------------------------------------------------

.status_labels <- c(Deleted = -1, Restored = 0, Created = 20, SupervisorAssigned = 40,
  InterviewerAssigned = 60, RejectedBySupervisor = 65, ReadyForInterview = 80, SentToCapi = 85,
  Restarted = 95, Completed = 100, ApprovedBySupervisor = 120, RejectedByHeadquarters = 125,
  ApprovedByHeadquarters = 130)

level_nodes <- function(qn, lk) {
  Filter(function(nd) nd$kind %in% c("question", "variable") &&
           !isTRUE(nd$do_not_export) && nzchar(nd$varname %||% "") &&
           identical(if (length(nd$path)) nd$path[length(nd$path)] else "", lk),
         lapply(qn$order, function(i) qn$nodes[[i]]))
}

# Width of linked multi-select export = size of the linked source (SuSo ignores MaxAllowedAnswers)
link_width <- function(qn, node) {
  src <- env_get(qn$by_name, node$lnk$var)
  if (!is.null(src) && identical(src$kind, "question") && src$qtype == "list") return(as.integer(src$max_answers %||% 40))
  lv <- qn$levels[[node$lnk$path[length(node$lnk$path)]]]
  w <- switch(lv$type %||% "numeric",
    list = env_get(qn$by_name, lv$trigger)$max_answers,
    fixed = length(lv$id_labels), multi = , yesno = length(lv$id_labels),
    numeric = env_get(qn$by_name, lv$trigger)$bounds[2])
  if (is.null(w) || is.na(w) || w > 200) 40L else as.integer(w)
}

col_specs <- function(qn, node) {
  v <- node$varname
  lab <- stata_label(node$label %||% node$text %||% v)
  S <- function(name, type, label = lab, vl = NULL) list(name = name, type = type, label = label, vl = vl)
  if (node$kind == "variable") return(list(S(v, if (node$vtype %in% c(4, 5)) "str" else "num")))
  switch(node$qtype,
    numeric = list(S(v, "num", vl = if (nrow(node$options)) stats::setNames(node$options$code, node$options$label))),
    single = {
      if (is.null(node$lnk)) return(list(S(v, "num", vl = stats::setNames(node$options$code, node$options$label))))
      list(S(v, "num"))
    },
    multi = {
      if (!is.null(node$lnk)) return(lapply(seq_len(link_width(qn, node)) - 1, function(k) S(paste0(v, "__", k), "num", "")))
      has_vl <- nzchar(node$var_label %||% "")
      lapply(seq_len(nrow(node$options)), function(j) S(paste0(v, "__", code_suffix(node$options$code[j])), "num",
        if (has_vl) stata_label(paste0(node$var_label, ":", html_escape(node$options$label[j]))) else lab))
    },
    gps = lapply(c("Latitude", "Longitude", "Accuracy", "Altitude", "Timestamp"), function(p)
      S(paste0(v, "__", p), if (p == "Timestamp") "str" else "num", stata_label(paste0(node$label %||% v, ": ", p)))),
    list = {
      w <- if (is.na(node$max_answers)) 40L else as.integer(node$max_answers)
      unlist(lapply(seq_len(w) - 1, function(k) list(S(paste0(v, "__", k), "str"), S(paste0(v, "__", k, "c"), "num"))), recursive = FALSE)
    },
    list(S(v, "str")))
}

fmt_date_val <- function(x) {
  if (inherits(x, "POSIXct")) format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  else if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
}

# Values of all columns of one node at one roster vector
node_values <- function(qn, node, st, rv, specs, cfg) {
  key <- st_key(node$varname, rv)
  v <- env_get(st$vals, key)
  en <- env_has(st$enabled, key)
  n <- length(specs)
  types <- vapply(specs, `[[`, "", "type")
  miss <- function(status) {
    out <- vector("list", n)
    for (j in seq_len(n)) out[[j]] <- if (types[j] == "num") (if (status == "dis") NA_real_ else cfg$na_numeric) else (if (status == "dis") "" else cfg$na_string)
    out
  }
  if (!en) return(miss("dis"))
  if (is.null(v) || !answered(v)) return(miss("na"))
  if (node$kind == "variable") {
    return(list(if (types[1] == "str") fmt_date_val(v) else as.numeric(v)))
  }
  switch(node$qtype,
    numeric = list(as.numeric(v)),
    single = list(as.numeric(v[[length(v)]])),
    multi = {
      if (!is.null(node$lnk)) {
        cds <- vapply(v, function(r) as.numeric(r[length(r)]), 0)
        return(lapply(seq_len(n), function(j) if (j <= length(cds)) cds[j] else NA_real_))
      }
      oc <- node$options$code
      if (inherits(v, "sus_yesno")) return(lapply(oc, function(cd) if (cd %in% v$yes) 1 else if (cd %in% v$no) 0 else NA_real_))
      cds <- codes_of(v)
      if (isTRUE(node$ordered)) return(lapply(oc, function(cd) { p <- match(cd, cds); if (is.na(p)) 0 else p }))
      lapply(oc, function(cd) as.numeric(cd %in% cds))
    },
    gps = list(v$Latitude, v$Longitude, v$Accuracy, v$Altitude, fmt_date_val(v$Timestamp)),
    list = {
      cd <- as.numeric(names(v)); tx <- unname(unclass(v))
      w <- n / 2
      unlist(lapply(seq_len(w), function(k) if (k <= length(cd)) list(tx[k], cd[k]) else list(cfg$na_string, NA_real_)), recursive = FALSE)
    },
    date = list(fmt_date_val(v)),
    list(as.character(v)))
}

level_rows_of <- function(st, lk) {
  er <- env_get(st$export_rows, lk)
  if (is.null(er)) return(list())
  rvs <- mget(ls(er), envir = er)
  if (!length(rvs)) return(list())
  m <- do.call(rbind, rvs)
  rvs <- unname(rvs[do.call(order, as.data.frame(m))])
  rvs
}

build_tables <- function(qn, states, meta, cfg, verbose = TRUE) {
  out <- list()
  lks <- names(qn$levels)
  for (li in seq_along(lks)) {
    lk <- if (lks[li] == "main") "" else lks[li]
    lv <- qn$levels[[lks[li]]]
    nodes <- level_nodes(qn, lk)
    specs <- lapply(nodes, function(nd) col_specs(qn, nd))
    # id columns
    anc <- character()
    if (nzchar(lk)) {
      chain <- character(); cur <- lk
      while (nzchar(cur)) { chain <- c(cur, chain); cur <- qn$levels[[cur]]$parent %||% "" }
      anc <- vapply(chain, function(k) qn$levels[[k]]$id_col, "")
    }
    list_trig <- lv$list_trigger
    parts <- vector("list", length(states))
    for (i in seq_along(states)) {
      st <- states[[i]]
      rvs <- if (!nzchar(lk)) list(numeric(0)) else level_rows_of(st, lk)
      nr <- length(rvs)
      if (!nr) next
      cols <- list(interview__key = rep(meta$key[i], nr), interview__id = rep(meta$id[i], nr))
      for (d in seq_along(anc)) cols[[anc[d]]] <- vapply(rvs, function(r) r[d], 0)
      if (!is.null(list_trig)) {
        tn <- env_get(qn$by_name, list_trig)
        cols[[list_trig]] <- vapply(rvs, function(r) {
          lv2 <- state_value(st, tn, r[seq_len(length(tn$path))])
          unname(unclass(lv2))[match(r[length(r)], as.numeric(names(lv2)))] %||% ""
        }, "")
      }
      for (j in seq_along(nodes)) {
        vals <- lapply(rvs, function(r) node_values(qn, nodes[[j]], st, r, specs[[j]], cfg))
        for (k in seq_along(specs[[j]])) {
          sp <- specs[[j]][[k]]
          x <- lapply(vals, `[[`, k)
          cols[[sp$name]] <- if (sp$type == "num") vapply(x, function(z) as.numeric(z %||% NA), 0) else vapply(x, function(z) as.character(z %||% ""), "")
        }
      }
      if (!nzchar(lk)) {
        cols$sssys_irnd <- meta$irnd[i]; cols$has__errors <- meta$n_errors[i]
        cols$interview__status <- meta$status[i]; cols$assignment__id <- meta$assignment_id[i]
      }
      parts[[i]] <- cols
    }
    parts <- Filter(Negate(is.null), parts)
    # column template (also for empty files)
    tmpl <- c(list(interview__key = character(), interview__id = character()),
              stats::setNames(rep(list(numeric()), length(anc)), anc))
    if (!is.null(list_trig)) tmpl[[list_trig]] <- character()
    for (sp in unlist(specs, recursive = FALSE)) tmpl[[sp$name]] <- if (sp$type == "num") numeric() else character()
    if (!nzchar(lk)) tmpl <- c(tmpl, list(sssys_irnd = numeric(), has__errors = numeric(), interview__status = numeric(), assignment__id = numeric()))
    df <- as.data.frame(lapply(names(tmpl), function(nm) {
      v <- unlist(lapply(parts, `[[`, nm), use.names = FALSE)
      if (is.null(v)) tmpl[[nm]] else v
    }), stringsAsFactors = FALSE, col.names = names(tmpl), check.names = FALSE)
    # labels
    attr(df$interview__key, "label") <- "Interview key (identifier in XX-XX-XX-XX format)"
    attr(df$interview__id, "label") <- "Unique 32-character long identifier of the interview"
    for (d in seq_along(anc)) {
      k <- names(anc)[d]; nm <- anc[d]
      vl <- if (d == length(anc)) qn$levels[[k]]$id_labels else NULL   # only the own id is labelled
      if (length(vl)) df[[nm]] <- haven::labelled(df[[nm]], labels = dedupe_labels(vl))
      attr(df[[nm]], "label") <- if (d == length(anc)) paste("Id in", qn$levels[[k]]$file) else sprintf('Id in "%s"', qn$levels[[k]]$file)
    }
    if (!is.null(list_trig)) attr(df[[list_trig]], "label") <- "Roster list question"
    for (sp in unlist(specs, recursive = FALSE)) {
      if (length(sp$vl)) df[[sp$name]] <- haven::labelled(df[[sp$name]], labels = dedupe_labels(sp$vl))
      attr(df[[sp$name]], "label") <- sp$label
    }
    if (!nzchar(lk)) {
      df$interview__status <- haven::labelled(df$interview__status, labels = .status_labels, label = "Status of the interview")
      attr(df$sssys_irnd, "label") <- "Random number in the range 0..1 associated with interview"
      attr(df$has__errors, "label") <- "Errors count in the interview"
      attr(df$assignment__id, "label") <- "Assignment id (identifier in numeric format)"
    }
    out[[lv$file]] <- df
  }
  out
}

dedupe_labels <- function(vl) {
  vl <- vl[!is.na(vl)]
  vl <- vl[!duplicated(vl)]
  nm <- strip_html(names(vl)); nm[!nzchar(nm)] <- as.character(vl[!nzchar(nm)])
  nm <- make.unique(substr(nm, 1, 32000))
  stats::setNames(as.numeric(vl), nm)
}

# Stata names: <= 32 chars, unique. Long names are shortened and de-duplicated
# with a numeric suffix that still fits in 32 characters.
stata_names <- function(nm) {
  long <- nchar(nm) > 32
  if (!any(long)) return(nm)
  out <- substr(nm, 1, 32)
  for (i in which(duplicated(out) | (long & out %in% out[duplicated(out)]))) {
    k <- 1L
    repeat {
      suf <- paste0("_", k)
      cand <- paste0(substr(nm[i], 1, 32 - nchar(suf)), suf)
      if (!(cand %in% out[-i])) { out[i] <- cand; break }
      k <- k + 1L
    }
  }
  out
}

stata_safe_names <- function(df) {
  nm <- names(df)
  new <- stata_names(nm)
  if (!identical(nm, new)) {
    ch <- nm != new
    warning("Stata names limited to 32 characters; renamed: ",
            paste(nm[ch], new[ch], sep = " -> ", collapse = ", "), call. = FALSE)
    names(df) <- new
  }
  df
}

write_dta_safe <- function(df, path) {
  df <- stata_safe_names(df)
  haven::write_dta(df, path, version = 14)
}
