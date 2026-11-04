# Structural integrity of a suso_mock result (used by several tests)
check_integrity <- function(res) {
  qn <- res$questionnaire
  m <- res$data[[qn$varname]]
  problems <- character()
  bad <- function(...) problems <<- c(problems, paste0(...))
  if (anyDuplicated(m$interview__id)) bad("duplicate interview__id")
  if (anyDuplicated(m$interview__key)) bad("duplicate interview__key")
  for (lk in setdiff(names(qn$levels), "main")) {
    lv <- qn$levels[[lk]]
    d <- res$data[[lv$file]]
    if (is.null(d)) { bad("missing file ", lv$file); next }
    ids <- grep("__id$", names(d), value = TRUE)
    if (anyDuplicated(as.data.frame(d)[, ids, drop = FALSE])) bad("duplicate ids in ", lv$file)
    if (!all(d$interview__id %in% m$interview__id)) bad("orphan interviews in ", lv$file)
    par <- lv$parent
    if (nzchar(par %||% "")) {
      pf <- res$data[[qn$levels[[par]]$file]]
      pids <- grep("__id$", names(pf), value = TRUE)
      k1 <- do.call(paste, as.data.frame(d)[, pids, drop = FALSE])
      k2 <- do.call(paste, as.data.frame(pf)[, pids, drop = FALSE])
      if (!all(k1 %in% k2)) bad("rows of ", lv$file, " without parent row in ", qn$levels[[par]]$file)
    }
  }
  for (f in names(res$data)) {
    nm <- names(res$data[[f]])
    if (anyDuplicated(nm)) bad("duplicate columns in ", f)
  }
  p <- res$paradata
  if (!all(p$interview__id %in% m$interview__id)) bad("paradata interview not in main")
  ord_ok <- tapply(p$order, p$interview__id, function(o) identical(as.integer(o), seq_along(o)))
  if (!all(ord_ok)) bad("paradata order not sequential")
  ts <- as.POSIXct(p$timestamp_utc, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  if (anyNA(ts)) bad("unparseable paradata timestamps")
  if (nrow(res$system$interview__diagnostics) != nrow(m)) bad("diagnostics rows != interviews")
  problems
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
