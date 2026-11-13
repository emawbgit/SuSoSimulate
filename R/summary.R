#' Variable-level summary of a mock dataset
#'
#' One row per exported variable: file, type, how many rows were disabled (skipped by
#' routing), enabled but unanswered (`.a` / `##N/A##`), answered, and for numeric
#' variables mean, standard deviation, minimum and maximum. Useful to check the
#' distributions and missingness before building cleaning or analysis code on the mock.
#'
#' @param x A `suso_mock` result.
#' @param files Export files to include (default all), e.g. `"members"`.
#' @return A data frame.
#' @export
suso_summary <- function(x, files = names(x$data)) {
  if (!inherits(x, "suso_mock")) stop("`x` must be a suso_mock() result.", call. = FALSE)
  skip <- "^(interview__key|interview__id|.*__id|sssys_irnd|has__errors|interview__status|assignment__id)$"
  rows <- lapply(intersect(files, names(x$data)), function(f) {
    d <- x$data[[f]]
    vs <- grep(skip, names(d), value = TRUE, invert = TRUE)
    do.call(rbind, lapply(vs, function(v) {
      y <- d[[v]]; num <- is.numeric(y)
      un <- if (num) haven::is_tagged_na(y) | y %in% -999999999 else y %in% "##N/A##"
      dis <- if (num) is.na(y) & !un else is.na(y) | y == ""
      z <- if (num) as.numeric(y)[!is.na(y) & !un] else numeric()
      st <- function(f) if (length(z)) f(z) else NA_real_
      data.frame(file = f, variable = v, type = if (num) "numeric" else "string",
                 label = attr(y, "label") %||% "", n = length(y), disabled = sum(dis), unanswered = sum(un),
                 answered = sum(!dis & !un), distinct = length(unique(y[!dis & !un])),
                 mean = st(mean), sd = st(stats::sd), min = st(min), max = st(max), stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
