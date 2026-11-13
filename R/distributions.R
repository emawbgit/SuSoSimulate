# Numeric answer distributions and correlations (ported from IPA's Synthetic-Data-Generator:
# per-column Normal/Uniform/Exponential/Lognormal and a target correlation). Here they are
# per question, truncated to the question's validation range, and correlated through a
# Gaussian copula within each roster row, so routing and validations still hold.

.dist_par <- list(normal = c("mean", "sd"), uniform = c("min", "max"),
                  exponential = "rate", lognormal = c("meanlog", "sdlog"))

# "lognormal" or list(dist = "lognormal", meanlog = 8) -> list(dist, params...)
norm_dist <- function(s, v) {
  if (is.character(s) && length(s) == 1) s <- list(dist = s)
  if (!is.list(s) || is.null(s$dist) || !tolower(s$dist) %in% names(.dist_par))
    stop(sprintf("`numeric_dist$%s` must be one of %s, or list(dist = ..., <parameters>).",
                 v, paste(names(.dist_par), collapse = ", ")), call. = FALSE)
  s$dist <- tolower(s$dist)
  bad <- setdiff(names(s), c("dist", .dist_par[[s$dist]]))
  if (length(bad)) stop(sprintf("`numeric_dist$%s`: %s takes %s (got %s).", v, s$dist,
                                paste(.dist_par[[s$dist]], collapse = ", "), paste(bad, collapse = ", ")), call. = FALSE)
  s
}

# Value with quantile u in (0,1) of the distribution truncated to [lo, hi]; missing
# parameters come from the validation range (or survey-sized defaults when there is none)
dist_draw <- function(s, lo, hi, u = stats::runif(1)) {
  fin <- is.finite(lo) && is.finite(hi) && hi > lo
  p <- switch(s$dist,
    normal = list(mean = s$mean %||% (if (fin) (lo + hi) / 2 else 50), sd = s$sd %||% (if (fin) (hi - lo) / 6 else 15)),
    uniform = list(min = s$min %||% (if (is.finite(lo)) lo else 0), max = s$max %||% (if (is.finite(hi)) hi else 100)),
    exponential = list(rate = s$rate %||% (if (fin) 4 / (hi - max(lo, 0)) else 1 / 20)),
    lognormal = list(meanlog = s$meanlog %||% log(if (fin) max(1, (max(lo, 0) + hi) / 10) else 20), sdlog = s$sdlog %||% 1))
  q <- switch(s$dist, normal = stats::qnorm, uniform = stats::qunif, exponential = stats::qexp, lognormal = stats::qlnorm)
  pf <- switch(s$dist, normal = stats::pnorm, uniform = stats::punif, exponential = stats::pexp, lognormal = stats::plnorm)
  a <- if (is.finite(lo)) do.call(pf, c(list(lo), p)) else 0
  b <- if (is.finite(hi)) do.call(pf, c(list(hi), p)) else 1
  if (b <= a) return(if (is.finite(lo)) lo else hi)          # range outside the support: take the bound
  x <- do.call(q, c(list(a + u * (b - a)), p))
  min(max(x, lo, na.rm = TRUE), hi, na.rm = TRUE)
}

# correlations: data.frame(var1, var2, rho) or list(c("income", "spend", 0.7), ...)
norm_cor <- function(cr) {
  if (is.null(cr) || !length(cr)) return(data.frame(var1 = character(), var2 = character(), rho = numeric()))
  if (is.list(cr) && !is.data.frame(cr) && all(c("var1", "var2", "rho") %in% names(cr)))
    cr <- as.data.frame(cr[c("var1", "var2", "rho")], stringsAsFactors = FALSE)   # from JSON / app code
  if (!is.data.frame(cr)) cr <- do.call(rbind, lapply(cr, function(p) {
    p <- unlist(p)
    if (length(p) != 3) stop("Each correlation is c(var1, var2, rho), e.g. c(\"income\", \"spend\", 0.7).", call. = FALSE)
    data.frame(var1 = p[[1]], var2 = p[[2]], rho = as.numeric(p[[3]]))
  }))
  names(cr)[1:3] <- c("var1", "var2", "rho")
  cr$rho <- as.numeric(cr$rho)
  if (anyNA(cr$rho) || any(abs(cr$rho) >= 1))
    stop("Correlations must lie strictly between -1 and 1.", call. = FALSE)
  cr[, 1:3]
}

# groups of correlated variables, each with the Cholesky factor of its correlation matrix
# (made positive definite when the pairs are inconsistent)
cor_plan <- function(cr) {
  cr <- norm_cor(cr)
  if (!nrow(cr)) return(list())
  vars <- unique(c(cr$var1, cr$var2)); grp <- stats::setNames(seq_along(vars), vars)
  for (i in seq_len(nrow(cr))) grp[grp == grp[cr$var2[i]]] <- grp[cr$var1[i]]
  lapply(unname(split(names(grp), grp)), function(v) {
    m <- diag(length(v)); dimnames(m) <- list(v, v)
    k <- cr$var1 %in% v
    for (i in which(k)) m[cr$var1[i], cr$var2[i]] <- m[cr$var2[i], cr$var1[i]] <- cr$rho[i]
    e <- eigen(m, symmetric = TRUE)
    if (min(e$values) < 1e-6) {                                # nearest PD, unit diagonal
      m <- e$vectors %*% diag(pmax(e$values, 1e-6)) %*% t(e$vectors); m <- stats::cov2cor(m); dimnames(m) <- list(v, v)
    }
    list(vars = v, L = t(chol(m)))
  })
}

# copula uniform for a correlated variable in one roster row; NULL when not correlated.
# A second draw for the same answer (validation retry) is independent.
cor_u <- function(G, var, rv) {
  for (g in G$cor) if (var %in% g$vars) {
    key <- paste(c(g$vars[1], rv), collapse = "|")
    z <- G$st$latent[[key]]
    if (is.null(z)) { z <- stats::setNames(as.vector(g$L %*% stats::rnorm(length(g$vars))), g$vars); z <- as.list(z) }
    u <- z[[var]]
    z[[var]] <- NULL; G$st$latent[[key]] <- if (length(z)) z else list(.done = TRUE)
    return(if (is.null(u)) stats::runif(1) else stats::pnorm(u))
  }
  NULL
}

# config checks needing the questionnaire: unknown, non-numeric or cross-level variables
check_dist_vars <- function(qn, cfg) {
  ok <- function(v) { n <- env_get(qn$by_name, v); !is.null(n) && n$kind == "question" && identical(n$qtype, "numeric") }
  bad <- Filter(Negate(ok), names(cfg$numeric_dist))
  if (length(bad)) warning("numeric_dist: not numeric questions, ignored: ", paste(bad, collapse = ", "), call. = FALSE)
  cfg$numeric_dist <- cfg$numeric_dist[setdiff(names(cfg$numeric_dist), bad)]
  cr <- norm_cor(cfg$correlations)
  lvl <- function(v) paste(env_get(qn$by_name, v)$path, collapse = "/")
  keep <- vapply(seq_len(nrow(cr)), function(i) ok(cr$var1[i]) && ok(cr$var2[i]) && lvl(cr$var1[i]) == lvl(cr$var2[i]), TRUE)
  if (!all(keep)) warning("correlations: pairs must be numeric questions in the same roster; ignored: ",
                          paste(cr$var1[!keep], cr$var2[!keep], sep = "~", collapse = ", "), call. = FALSE)
  cfg$correlations <- cr[keep, , drop = FALSE]
  cfg
}

# App text box <-> settings. One line each:  income lognormal meanlog=9 sdlog=1
#                                            income ~ spend 0.7
parse_dist_spec <- function(txt) {
  ln <- trimws(unlist(strsplit(txt %||% "", "[\r\n]+")))
  ln <- ln[nzchar(ln) & !startsWith(ln, "#")]
  nd <- list(); cr <- list()
  for (l in ln) {
    if (grepl("~", l, fixed = TRUE)) {
      m <- regmatches(l, regexec("^(\\S+)\\s*~\\s*(\\S+)\\s+(-?[0-9.]+)$", l))[[1]]
      if (!length(m)) stop("Cannot read the correlation line '", l, "'. Write: var1 ~ var2 0.7", call. = FALSE)
      cr[[length(cr) + 1]] <- m[2:4]
    } else {
      p <- strsplit(l, "\\s+")[[1]]
      if (length(p) < 2) stop("Cannot read '", l, "'. Write: variable distribution [parameter=value ...]", call. = FALSE)
      s <- list(dist = p[2])
      for (kv in p[-(1:2)]) {
        kv <- strsplit(kv, "=", fixed = TRUE)[[1]]
        if (length(kv) != 2 || is.na(suppressWarnings(as.numeric(kv[2])))) stop("Cannot read the parameter in '", l, "'.", call. = FALSE)
        s[[kv[1]]] <- as.numeric(kv[2])
      }
      nd[[p[1]]] <- norm_dist(s, p[1])
    }
  }
  list(numeric_dist = nd, correlations = norm_cor(cr))
}

spec_text <- function(nd, cr) {
  cr <- norm_cor(cr)
  c(vapply(names(nd), function(v) { s <- nd[[v]]; p <- s[setdiff(names(s), "dist")]
      trimws(paste(v, s$dist, if (length(p)) paste0(names(p), "=", vapply(p, fmt_num, ""), collapse = " ") else "")) }, ""),
    sprintf("%s ~ %s %s", cr$var1, cr$var2, vapply(cr$rho, fmt_num, "")))
}
