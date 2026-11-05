# ---------------------------------------------------------------------------
# validate.R -- cross-file consistency of a mock export
# ---------------------------------------------------------------------------

#' Check that a mock export is internally consistent
#'
#' Cross-checks identifiers and counts across the data files, system files,
#' paradata, sampling frame and assignments, the way they relate in a real
#' Survey Solutions export.
#'
#' @param x Result of [suso_mock()].
#' @return data.frame with one row per check: `check`, `ok`, `detail` (first
#'   offending ids when a check fails). Invisibly `TRUE`/`FALSE` is not returned;
#'   use `all(suso_validate_mock(x)$ok)`.
#' @export
suso_validate_mock <- function(x) {
  qn <- x$questionnaire; m <- as.data.frame(x$data[[qn$varname]]); S <- x$system; p <- x$paradata
  out <- list()
  chk <- function(name, bad) {
    bad <- unique(stats::na.omit(as.character(bad)))
    out[[length(out) + 1]] <<- data.frame(check = name, ok = !length(bad),
      detail = if (length(bad)) paste(utils::head(bad, 5), collapse = ", ") else "", stringsAsFactors = FALSE)
  }
  ids <- m$interview__id
  chk("unique interview__id / interview__key in main file", c(ids[duplicated(ids)], m$interview__key[duplicated(m$interview__key)]))
  chk("interview__key <-> interview__id one-to-one in all files", unlist(lapply(c(x$data, S), function(d) {
    if (is.null(d$interview__id)) return(NULL)
    k <- m$interview__key[match(d$interview__id, ids)]; d$interview__id[is.na(k) | k != d$interview__key]
  })))
  # rosters
  for (lk in setdiff(names(qn$levels), "main")) {
    lv <- qn$levels[[lk]]; d <- as.data.frame(x$data[[lv$file]])
    idc <- grep("__id$", setdiff(names(d), "interview__id"), value = TRUE)
    chk(sprintf("%s: unique row ids", lv$file), do.call(paste, d[c("interview__id", idc)])[duplicated(d[c("interview__id", idc)])])
    if (nzchar(lv$parent %||% "")) {
      pf <- as.data.frame(x$data[[qn$levels[[lv$parent]]$file]])
      pid <- grep("__id$", setdiff(names(pf), "interview__id"), value = TRUE)
      k1 <- do.call(paste, d[c("interview__id", pid)]); k2 <- do.call(paste, pf[c("interview__id", pid)])
      chk(sprintf("%s: every row has a parent row in %s", lv$file, qn$levels[[lv$parent]]$file), k1[!k1 %in% k2])
    } else chk(sprintf("%s: every row belongs to an interview", lv$file), d$interview__id[!d$interview__id %in% ids])
  }
  # system files
  dg <- as.data.frame(S$interview__diagnostics)
  chk("interview__diagnostics: one row per interview", c(setdiff(ids, dg$interview__id), setdiff(dg$interview__id, ids)))
  st_m <- stats::setNames(as.numeric(m$interview__status), ids)
  chk("interview__status equal in main file and diagnostics", dg$interview__id[as.numeric(dg$interview__status) != st_m[dg$interview__id]])
  ia <- as.data.frame(S$interview__actions)
  chk("interview__actions: every interview has Created, InterviewerAssigned and Completed",
      setdiff(ids, Reduce(intersect, lapply(c(12, 1, 3), function(a) ia$interview__id[as.numeric(ia$action) == a]))))
  chk("interview__actions: interviews exist", setdiff(ia$interview__id, ids))
  er <- as.data.frame(S$interview__errors)
  ne <- tapply(do.call(paste, er[intersect(c("roster", "id1", "id2", "id3", "id4", "variable"), names(er))]), er$interview__id,
               function(z) length(unique(z)))
  ne_m <- stats::setNames(as.numeric(m$has__errors), ids)
  got <- ne[ids]; got[is.na(got)] <- 0
  chk("has__errors = questions with errors in interview__errors", ids[got != ne_m])
  chk("entities__errors (diagnostics) = has__errors", dg$interview__id[as.numeric(dg$entities__errors) != ne_m[dg$interview__id]])
  cm <- as.data.frame(S$interview__comments)
  ncm <- tapply(do.call(paste, cm[intersect(c("roster", "id1", "id2", "id3", "id4", "variable"), names(cm))]), cm$interview__id,
                function(z) length(unique(z)))
  got <- ncm[dg$interview__id]; got[is.na(got)] <- 0
  chk("questions__comments = commented questions in interview__comments", dg$interview__id[got != as.numeric(dg$questions__comments)])
  # assignments
  aa <- as.data.frame(S$assignment__actions)
  chk("every assignment__id has assignment__actions", setdiff(m$assignment__id, aa$assignment__id))
  fr <- x$frame
  chk("sample frame <-> interviews", c(setdiff(ids, fr$interview__id), setdiff(fr$interview__id, ids)))
  chk("one interviewer per assignment", names(which(tapply(fr$interviewer, fr$assignment_id, function(z) length(unique(z)) > 1))))
  ints <- stats::setNames(fr$interviewer, fr$interview__id)
  own <- ia[as.numeric(ia$action) == 1, ]
  chk("InterviewerAssigned goes to the assignment's interviewer", own$interview__id[own$responsible__name != ints[own$interview__id]])
  asg <- assignments_table(x)
  for (v in setdiff(names(asg), c("assignment__id", "_responsible", "_quantity"))) {
    a <- tab_cell(asg[[v]])[match(m$assignment__id, asg$assignment__id)]
    chk(sprintf("cover variable %s equals its assignment value", v), ids[a != tab_cell(m[[v]])])
  }
  pre <- x$preload %||% data.frame()
  for (v in intersect(names(pre), names(m))) {
    nd <- env_get(qn$by_name, v)
    if (is.null(nd) || length(nd$path)) next
    a <- tab_cell(pre[[v]])[match(m$interview__id, fr$interview__id)]
    chk(sprintf("preloaded %s equals the sample frame", v), ids[a != tab_cell(m[[v]]) & tab_cell(m[[v]]) != ""])
  }
  iv <- qn$interviewer_var
  if (!is.null(iv) && !is.null(m[[iv]])) {
    ivn <- env_get(qn$by_name, iv)
    lab <- if (ivn$qtype == "single") gsub("[^A-Za-z0-9_.-]", "", strip_html(ivn$options$label[match(as.numeric(m[[iv]]), ivn$options$code)]))
           else as.character(m[[iv]])
    chk(sprintf("interviewer question %s = interviewer login", iv), ids[!is.na(lab) & lab != ints[ids]])
  }
  # paradata
  chk("paradata: interviews exist", setdiff(p$interview__id, ids))
  chk("paradata: every interview has events", setdiff(ids, p$interview__id))
  chk("paradata: order sequential", names(which(!tapply(p$order, p$interview__id, function(o) identical(as.integer(o), seq_along(o))))))
  chk("paradata: Completed for every completed interview", setdiff(ids[st_m >= 100 | st_m == 65], p$interview__id[p$event == "Completed"]))
  chk("paradata: interviewer events by the assigned interviewer",
      p$interview__id[p$role == 1 & p$event %in% c("AnswerSet", "Completed") & p$responsible != ints[p$interview__id]])
  do.call(rbind, out)
}
