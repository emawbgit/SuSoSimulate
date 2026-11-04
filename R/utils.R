# ---------------------------------------------------------------------------
# utils.R -- small shared helpers
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# TRUE for NULL / zero-length / scalar NA (C# null)
is_na1 <- function(x) {
  is.null(x) || length(x) == 0 || (is.atomic(x) && length(x) == 1 && is.na(x))
}

sm_msg <- function(verbose, ...) if (isTRUE(verbose)) message(...)

# 32-char lowercase hex GUID (Survey Solutions interview__id format)
new_guid <- function(n = 1) {
  hex <- c(0:9, letters[1:6])
  vapply(seq_len(n), function(i) paste(sample(hex, 32, TRUE), collapse = ""), "")
}

# Unique interview keys "NN-NN-NN-NN"
new_interview_keys <- function(n) {
  u <- sample.int(1e8, n) - 1
  sprintf("%02d-%02d-%02d-%02d", u %/% 1e6, (u %/% 1e4) %% 100, (u %/% 100) %% 100, u %% 100)
}

strip_html <- function(x) {
  x <- gsub("<[^>]+>", " ", x %||% "")
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}

# SuSo variable labels: HTML tags removed, whitespace kept verbatim, cut at 80 chars
stata_label <- function(x, n = 80) {
  x <- gsub("<[^>]+>", "", x %||% "")
  ifelse(nchar(x) > n, substr(x, 1, n), x)
}

# Option code -> SuSo multi-select column suffix (negative codes become n<abs>)
code_suffix <- function(code) {
  s <- format(abs(code), scientific = FALSE, trim = TRUE)
  ifelse(code < 0, paste0("n", s), s)
}

fmt_num <- function(x) format(x, scientific = FALSE, trim = TRUE, digits = 15)

as_num_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

# Keys used in the interview state
rv_str  <- function(rv) if (length(rv)) paste(sprintf("%.15g", rv), collapse = ",") else ""
st_key  <- function(name, rv) paste0(name, "|", rv_str(rv))
row_key <- function(level, rv) paste0(level, "|", rv_str(rv))

env_get <- function(env, key) get0(key, envir = env, inherits = FALSE)
env_has <- function(env, key) exists(key, envir = env, inherits = FALSE)
env_rm  <- function(env, key) if (exists(key, envir = env, inherits = FALSE)) rm(list = key, envir = env)

# Growable list stored in an environment (amortised O(1) append)
log_new <- function() { e <- new.env(); e$x <- vector("list", 256); e$n <- 0L; e }
log_add <- function(lg, item) {
  if (lg$n >= length(lg$x)) lg$x <- c(lg$x, vector("list", length(lg$x)))
  lg$n <- lg$n + 1L
  lg$x[[lg$n]] <- item
}
log_items <- function(lg) if (lg$n == 0) list() else lg$x[seq_len(lg$n)]

check_dependencies <- function(pdf = FALSE, xlsx = FALSE) {
  req <- c("jsonlite", "haven")
  if (pdf) req <- c(req, "pdftools")
  if (xlsx) req <- c(req, "readxl")
  miss <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("Missing packages: ", paste(miss, collapse = ", "),
         "\nInstall with: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))",
         call. = FALSE)
  }
  invisible(TRUE)
}

fmt_ts <- function(t, ms = FALSE) {
  if (ms) format(t, "%Y-%m-%dT%H:%M:%OS3", tz = "UTC") else format(t, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

# "+03:00" -> seconds
parse_offset <- function(x) {
  m <- regmatches(x, regexec("^([+-])?(\\d{1,2}):?(\\d{2})?", x))[[1]]
  if (!length(m)) return(0)
  s <- if (m[2] == "-") -1 else 1
  s * (as.numeric(m[3]) * 3600 + as.numeric(if (nzchar(m[4])) m[4] else 0) * 60)
}

# .NET TimeSpan style: "03:00:00" / "-05:00:00"
fmt_offset <- function(sec) {
  s <- if (sec < 0) "-" else ""
  a <- abs(sec)
  sprintf("%s%02d:%02d:00", s, a %/% 3600, (a %% 3600) %/% 60)
}

fmt_hms <- function(sec) {
  sec <- round(sec)
  sprintf("%02d:%02d:%02d", sec %/% 3600, (sec %% 3600) %/% 60, sec %% 60)
}

html_escape <- function(x) {
  x <- gsub("&(?![a-zA-Z]+;|#[0-9]+;)", "&amp;", x, perl = TRUE)
  gsub(">", "&gt;", gsub("<", "&lt;", x, fixed = TRUE), fixed = TRUE)
}
