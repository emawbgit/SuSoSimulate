# ---------------------------------------------------------------------------
# expr_runtime.R -- runtime for compiled SuSo expressions.
# Implements C# nullable semantics (null comparisons are false, null == null
# is true, arithmetic with null is null), SuSo extension methods and LINQ.
# ---------------------------------------------------------------------------

# Internal value classes ----------------------------------------------------
sus_multi    <- function(x) structure(as.numeric(x), class = "sus_multi")
sus_yesno    <- function(yes = numeric(0), no = numeric(0), all = numeric(0))
  structure(list(yes = as.numeric(yes), no = as.numeric(no), all = as.numeric(all)), class = "sus_yesno")
sus_textlist <- function(codes, texts)
  structure(stats::setNames(as.character(texts), fmt_num(codes)), class = "sus_textlist")
sus_gps <- function(lat, lon, acc, alt, ts)
  structure(list(Latitude = lat, Longitude = lon, Accuracy = acc, Altitude = alt, Timestamp = ts),
            class = "sus_gps")

codes_of <- function(x) {
  if (inherits(x, "sus_yesno")) return(x$yes)
  if (inherits(x, "sus_textlist")) return(as.numeric(names(x)))
  as.numeric(unclass(x))
}

answered <- function(x) {
  if (inherits(x, "sus_yesno")) return(length(x$yes) + length(x$no) > 0)
  !is_na1(x)
}

# Errors raised for constructs the translator does not support (as opposed
# to C# runtime exceptions such as `.Value` on null).
unsupported <- function(msg) {
  stop(structure(class = c("sus_unsupported", "error", "condition"),
                 list(message = msg, call = NULL)))
}

cs_eval <- function(call, ctx) {
  env <- new.env(parent = baseenv())
  env$.ctx <- ctx
  eval(call, env)
}

.get_ctx <- function(env) get(".ctx", envir = env, inherits = TRUE)

rt_truthy <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (!is.atomic(x)) return(TRUE)
  v <- x[[1]]
  if (is.na(v)) return(FALSE)
  if (is.logical(v)) return(v)
  if (is.numeric(v)) return(v != 0)
  if (is.character(v)) return(tolower(v) %in% c("true", "1"))
  FALSE
}

rt_var <- function(name) ctx_lookup(.get_ctx(parent.frame()), name)

rt_self <- function() {
  ctx <- .get_ctx(parent.frame())
  if (is.null(ctx$self)) unsupported("'self' used outside a question")
  ctx_lookup(ctx, ctx$self)
}

rt_special <- function(name) {
  ctx <- .get_ctx(parent.frame())
  switch(name,
    "@rowcode" = { if (!length(ctx$rv)) unsupported("@rowcode outside roster"); as.integer(ctx$rv[length(ctx$rv)]) },
    "@rowindex" = {
      if (!length(ctx$rv)) unsupported("@rowindex outside roster")
      lk <- ctx$path[length(ctx$path)]
      rows <- env_get(ctx$st$rows, row_key(lk, ctx$rv[-length(ctx$rv)]))
      as.integer(match(ctx$rv[length(ctx$rv)], rows) - 1L)
    },
    "@optioncode" = { if (is.null(ctx$optioncode)) unsupported("@optioncode outside filter"); ctx$optioncode },
    unsupported(paste("Unsupported keyword", name)))
}

# Variable resolution --------------------------------------------------------
# Scalars resolve in the caller's roster scope; rosters resolve to row
# collections; questions in deeper levels resolve to vectors of values.
ctx_lookup <- function(ctx, name) {
  qn <- ctx$qn
  node <- env_get(qn$by_name, name)
  if (is.null(node)) unsupported(paste0("Unknown identifier '", name, "' (lookup table or unsupported reference)"))
  if (node$kind == "roster") return(roster_rows(ctx, node))
  vp <- node$path; cp <- ctx$path; d <- length(vp)
  if (d <= length(cp) && identical(vp, cp[seq_len(d)])) {
    return(state_value(ctx$st, node, ctx$rv[seq_len(d)]))
  }
  if (length(cp) < d && identical(cp, vp[seq_along(cp)])) {
    rvs <- expand_rows(ctx$st, vp, ctx$rv, length(cp))
    vals <- lapply(rvs, function(r) state_value(ctx$st, node, r))
    return(simplify_list(vals))
  }
  # common ancestor reference (e.g. sibling roster on another level)
  k <- 0L
  while (k < min(d, length(cp)) && vp[k + 1] == cp[k + 1]) k <- k + 1L
  rvs <- expand_rows(ctx$st, vp, ctx$rv[seq_len(k)], k)
  simplify_list(lapply(rvs, function(r) state_value(ctx$st, node, r)))
}

# All row vectors of level path `vp` under prefix `rv0` (depth k)
expand_rows <- function(st, vp, rv0, k) {
  rvs <- list(rv0[seq_len(k)])
  if (k >= length(vp)) return(rvs)
  for (lvl in vp[(k + 1):length(vp)]) {
    nxt <- list()
    for (r in rvs) {
      codes <- env_get(st$rows, row_key(lvl, r))
      for (cd in codes) nxt[[length(nxt) + 1]] <- c(r, cd)
    }
    rvs <- nxt
  }
  rvs
}

roster_rows <- function(ctx, node) {
  pp <- node$path[-length(node$path)]
  k <- length(pp)
  cp <- ctx$path
  if (!(k <= length(cp) && identical(pp, cp[seq_len(k)]))) {
    # roster in another branch: collect across the common ancestor
    j <- 0L
    while (j < min(k, length(cp)) && pp[j + 1] == cp[j + 1]) j <- j + 1L
    parents <- expand_rows(ctx$st, pp, ctx$rv[seq_len(j)], j)
  } else parents <- list(ctx$rv[seq_len(k)])
  lk <- node$path[length(node$path)]
  rows <- list()
  for (p in parents) {
    for (cd in env_get(ctx$st$rows, row_key(lk, p)))
      rows[[length(rows) + 1]] <- structure(list(rv = c(p, cd), path = node$path, ctx = ctx), class = "sus_row")
  }
  structure(rows, class = "sus_roster")
}

state_value <- function(st, node, rv) {
  v <- env_get(st$vals, st_key(node$varname, rv))
  if (is.null(v)) empty_value(node) else v
}

empty_value <- function(node) {
  if (identical(node$kind, "question")) {
    if (node$qtype == "multi") {
      if (!is.null(node$lnk)) return(list())
      return(if (isTRUE(node$yesno)) sus_yesno(all = node$options$code) else sus_multi(numeric(0)))
    }
    if (node$qtype == "list") return(sus_textlist(numeric(0), character(0)))
  }
  NA
}

simplify_list <- function(l) {
  if (!length(l)) return(numeric(0))
  if (all(vapply(l, inherits, TRUE, "sus_row"))) return(structure(l, class = "sus_roster"))
  if (all(vapply(l, function(e) is.atomic(e) && length(e) == 1 && !inherits(e, c("sus_multi", "sus_textlist")), TRUE))) {
    cls <- class(l[[1]])
    out <- unlist(l, use.names = FALSE)
    if (inherits(l[[1]], "Date")) out <- as.Date(out, origin = "1970-01-01")
    return(out)
  }
  l
}

as_seq <- function(x) {
  if (inherits(x, "sus_roster")) return(unclass(x))
  if (inherits(x, "sus_textlist")) {
    cd <- as.numeric(names(x)); tx <- unname(unclass(x))
    return(lapply(seq_along(cd), function(i) structure(list(Item1 = cd[i], Item2 = tx[i]), class = "sus_tuple")))
  }
  if (inherits(x, "sus_yesno")) return(as.list(x$yes))
  if (inherits(x, "sus_multi")) return(as.list(as.numeric(unclass(x))))
  if (is.null(x)) return(list())
  if (is.list(x) && !inherits(x, c("sus_row", "sus_gps", "sus_tuple"))) return(x)
  if (is.atomic(x)) { if (length(x) == 1 && is.na(x)) return(list()); return(as.list(x)) }
  list(x)
}

seq_nums <- function(x, f = NULL) {
  s <- as_seq(x)
  if (!is.null(f)) s <- lapply(s, f)
  vapply(s, function(e) if (is_na1(e)) NA_real_ else as.numeric(e[[1]]), numeric(1))
}

# Operators ------------------------------------------------------------------
is_time <- function(x) inherits(x, c("Date", "POSIXct"))

rt_bin <- function(op, a, b) {
  if (op %in% c("+", "-", "*", "/", "%")) {
    if (op == "+" && (is.character(a) || is.character(b))) {
      s <- function(z) if (is_na1(z)) "" else as.character(z)
      return(paste0(s(a), s(b)))
    }
    if (is_na1(a) || is_na1(b)) return(NA)
    if (op == "-" && is_time(a) && is_time(b)) {
      if (inherits(a, "Date") && inherits(b, "POSIXct")) a <- as.POSIXct(a)
      if (inherits(b, "Date") && inherits(a, "POSIXct")) b <- as.POSIXct(b)
      return(structure(as.numeric(difftime(a, b, units = "days")), class = "sus_timespan"))
    }
    if (inherits(b, "sus_timespan")) b <- if (inherits(a, "POSIXct")) unclass(b) * 86400 else unclass(b)
    a1 <- if (inherits(a, c("sus_multi", "sus_timespan"))) unclass(a) else a
    b1 <- if (inherits(b, c("sus_multi", "sus_timespan"))) unclass(b) else b
    r <- switch(op,
      "+" = a1 + b1, "-" = a1 - b1, "*" = a1 * b1,
      "/" = if (is.integer(a1) && is.integer(b1)) as.integer(trunc(a1 / b1)) else a1 / b1,
      "%" = a1 - b1 * trunc(a1 / b1))
    return(r)
  }
  if (op == "??") return(if (is_na1(a)) b else a)
  na_a <- is_na1(a); na_b <- is_na1(b)
  if (op == "==") return(if (na_a || na_b) (na_a && na_b) else isTRUE(all(unclass(a) == unclass(b))))
  if (op == "!=") return(!rt_bin("==", a, b))
  if (na_a || na_b) return(FALSE)
  if (inherits(a, "Date") && inherits(b, "POSIXct")) a <- as.POSIXct(a)
  if (inherits(b, "Date") && inherits(a, "POSIXct")) b <- as.POSIXct(b)
  a <- if (is.list(a)) unlist(a) else unclass(a); b <- if (is.list(b)) unlist(b) else unclass(b)
  isTRUE(switch(op, "<" = a[1] < b[1], ">" = a[1] > b[1], "<=" = a[1] <= b[1], ">=" = a[1] >= b[1],
                unsupported(paste("Operator", op))))
}

rt_neg <- function(x) if (is_na1(x)) NA else -unclass(x)

rt_array <- function(...) {
  l <- list(...)
  if (all(vapply(l, is.atomic, TRUE))) unlist(l, use.names = FALSE) else l
}

rt_new <- function(type, ...) {
  a <- list(...)
  if (type == "DateTime") {
    if (length(a) >= 6) return(as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d", a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]]), tz = "UTC"))
    if (length(a) >= 3) return(as.Date(sprintf("%04d-%02d-%02d", a[[1]], a[[2]], a[[3]])))
  }
  if (type == "TimeSpan" && length(a) == 3) return(structure((a[[1]] + a[[2]] / 60 + a[[3]] / 3600) / 24, class = "sus_timespan"))
  if (grepl("List|HashSet|\\[\\]", type)) return(rt_array(...))
  unsupported(paste("new", type))
}

rt_index <- function(obj, i) {
  if (is_na1(i)) stop("Index is null")
  if (is.character(obj) && length(obj) == 1 && !inherits(obj, "sus_textlist")) return(substr(obj, i + 1, i + 1))
  s <- as_seq(obj)
  if (i + 1 > length(s) || i < 0) stop("Index was outside the bounds of the array")
  s[[i + 1]]
}

rt_contains <- function(obj, v) {
  if (is_na1(v) && !is.list(v)) return(FALSE)
  if (inherits(obj, "sus_yesno")) return(v %in% obj$yes)
  if (inherits(obj, "sus_textlist")) return(v %in% unname(unclass(obj)) || (is.numeric(v) && v %in% as.numeric(names(obj))))
  if (inherits(obj, "sus_roster")) return(any(vapply(unclass(obj), function(r) isTRUE(all(r$rv == v)), TRUE)))
  if (is.character(obj) && length(obj) == 1) return(!is.na(obj) && grepl(as.character(v), obj, fixed = TRUE))
  if (is.list(obj)) return(any(vapply(obj, function(e) isTRUE(all(unlist(e) == v)), TRUE)))
  if (is_na1(obj)) return(FALSE)
  v %in% as.vector(unclass(obj))
}

rt_tostring <- function(x) {
  if (is_na1(x)) return("")
  if (inherits(x, "Date")) return(format(x, "%m/%d/%Y"))
  if (is.logical(x)) return(if (x) "True" else "False")
  if (is.numeric(x)) return(fmt_num(unclass(x)))
  paste(as.character(unlist(x)), collapse = ",")
}

add_months <- function(d, m) {
  if (inherits(d, "POSIXct")) { t <- as.POSIXlt(d); t$mon <- t$mon + m; return(as.POSIXct(t)) }
  t <- as.POSIXlt(d); t$mon <- t$mon + m; as.Date(t)
}

haversine_m <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008.8 * asin(min(1, sqrt(a)))
}

# Members --------------------------------------------------------------------
rt_member <- function(obj, name) {
  if (inherits(obj, "sus_row")) {
    if (name == "@rowcode") return(as.integer(obj$rv[length(obj$rv)]))
    ctx <- obj$ctx; ctx$rv <- obj$rv; ctx$path <- obj$path
    return(ctx_lookup(ctx, name))
  }
  if (inherits(obj, "sus_yesno")) {
    return(switch(name, Yes = sus_multi(obj$yes), No = sus_multi(obj$no),
                  Missing = sus_multi(setdiff(obj$all, c(obj$yes, obj$no))), Length = length(obj$yes), unsupported(paste0(".", name, " on yes/no"))))
  }
  if (inherits(obj, "sus_tuple")) return(obj[[name]] %||% unsupported(paste0(".", name, " on list item")))
  if (inherits(obj, "sus_gps")) {
    if (name %in% names(obj)) return(obj[[name]])
    if (name == "HasValue") return(TRUE)
    if (name == "Value") return(obj)
    unsupported(paste0(".", name, " on GPS"))
  }
  if (inherits(obj, "sus_timespan")) {
    d <- unclass(obj)
    return(switch(name, TotalDays = d, Days = trunc(d), TotalHours = d * 24, Hours = trunc(d * 24) %% 24,
                  TotalMinutes = d * 1440, Minutes = trunc(d * 1440) %% 60, TotalSeconds = d * 86400,
                  unsupported(paste0("TimeSpan.", name))))
  }
  if (name == "HasValue") return(!is_na1(obj))
  if (name == "Value") { if (is_na1(obj)) stop("Nullable object must have a value"); return(obj) }
  if (name %in% c("Length", "Count")) {
    if (is.character(obj) && length(obj) == 1 && !inherits(obj, "sus_textlist")) return(if (is.na(obj)) stop("null") else nchar(obj))
    return(length(as_seq(obj)))
  }
  if (is_time(obj)) {
    if (is_na1(obj)) stop("Nullable object must have a value")
    lt <- as.POSIXlt(obj, tz = "UTC")
    return(switch(name, Year = lt$year + 1900L, Month = lt$mon + 1L, Day = lt$mday, Hour = lt$hour,
                  Minute = lt$min, Second = as.integer(lt$sec), DayOfWeek = lt$wday, DayOfYear = lt$yday + 1L,
                  Date = as.Date(obj), unsupported(paste0("DateTime.", name))))
  }
  if (is_na1(obj)) stop("Object reference not set to an instance of an object")
  unsupported(paste0("Member .", name))
}

# Methods --------------------------------------------------------------------
rt_method <- function(obj, name, ...) {
  a <- list(...)
  f <- if (length(a) && is.function(a[[1]])) a[[1]] else NULL
  pred <- function(s) vapply(s, function(e) rt_truthy(f(e)), TRUE)
  switch(name,
    InRange = { if (is_na1(obj)) FALSE else rt_bin(">=", obj, a[[1]]) && rt_bin("<=", obj, a[[2]]) },
    InList = { if (is_na1(obj)) FALSE else any(vapply(unlist(a), function(v) rt_bin("==", obj, v), TRUE)) },
    Contains = rt_contains(obj, a[[1]]),
    ContainsAny = any(vapply(unlist(a), function(v) rt_contains(obj, v), TRUE)),
    ContainsAll = all(vapply(unlist(a), function(v) rt_contains(obj, v), TRUE)),
    ContainsOnly = { cs <- codes_of(obj); length(cs) > 0 && all(cs %in% unlist(a)) },
    Count = , LongCount = { s <- as_seq(obj); if (is.null(f)) length(s) else sum(pred(s)) },
    Any = { s <- as_seq(obj); if (is.null(f)) length(s) > 0 else any(pred(s)) },
    All = { s <- as_seq(obj); all(pred(s)) },
    Where = { s <- as_seq(obj); out <- s[pred(s)]
              if (inherits(obj, "sus_roster")) structure(out, class = "sus_roster") else simplify_list(out) },
    Select = simplify_list(lapply(as_seq(obj), f)),
    SelectMany = simplify_list(do.call(c, c(list(list()), lapply(as_seq(obj), function(e) as_seq(f(e)))))),
    Sum = { v <- seq_nums(obj, f); if (!length(v)) 0 else sum(v, na.rm = TRUE) },
    Max = { v <- seq_nums(obj, f); v <- v[!is.na(v)]; if (!length(v)) NA else max(v) },
    Min = { v <- seq_nums(obj, f); v <- v[!is.na(v)]; if (!length(v)) NA else min(v) },
    Average = { v <- seq_nums(obj, f); v <- v[!is.na(v)]; if (!length(v)) NA else mean(v) },
    First = , FirstOrDefault = { s <- as_seq(obj); if (!is.null(f)) s <- s[pred(s)]
              if (!length(s)) { if (name == "First") stop("Sequence contains no elements") else NA } else s[[1]] },
    Last = , LastOrDefault = { s <- as_seq(obj); if (!is.null(f)) s <- s[pred(s)]
              if (!length(s)) { if (name == "Last") stop("Sequence contains no elements") else NA } else s[[length(s)]] },
    ElementAt = , ElementAtOrDefault = rt_index(obj, a[[1]]),
    Distinct = simplify_list(unique(as_seq(obj))),
    ToList = , ToArray = , AsEnumerable = obj,
    OrderBy = { s <- as_seq(obj); simplify_list(s[order(seq_nums(obj, f))]) },
    OrderByDescending = { s <- as_seq(obj); simplify_list(s[order(-seq_nums(obj, f))]) },
    Take = simplify_list(utils::head(as_seq(obj), a[[1]])),
    Skip = simplify_list(if (a[[1]] >= length(as_seq(obj))) list() else as_seq(obj)[-seq_len(a[[1]])]),
    Concat = , Union = simplify_list(unique(c(as_seq(obj), as_seq(a[[1]])))),
    Intersect = simplify_list(intersect(as_seq(obj), as_seq(a[[1]]))),
    Except = simplify_list(setdiff(as_seq(obj), as_seq(a[[1]]))),
    # strings
    ToLower = , ToLowerInvariant = tolower(obj), ToUpper = , ToUpperInvariant = toupper(obj),
    Trim = trimws(obj), TrimStart = trimws(obj, "left"), TrimEnd = trimws(obj, "right"),
    StartsWith = !is_na1(obj) && startsWith(obj, a[[1]]),
    EndsWith = !is_na1(obj) && endsWith(obj, a[[1]]),
    Substring = { st <- a[[1]] + 1; if (length(a) > 1) substr(obj, st, st + a[[2]] - 1) else substr(obj, st, nchar(obj)) },
    IndexOf = { p <- regexpr(as.character(a[[1]]), obj, fixed = TRUE); if (p < 0) -1L else as.integer(p) - 1L },
    Replace = gsub(a[[1]], a[[2]], obj, fixed = TRUE),
    Split = strsplit(obj, a[[1]], fixed = TRUE)[[1]],
    ToString = rt_tostring(obj),
    Equals = rt_bin("==", obj, a[[1]]),
    CompareTo = { if (rt_bin("<", obj, a[[1]])) -1L else if (rt_bin(">", obj, a[[1]])) 1L else 0L },
    IsNullOrEmpty = is_na1(obj) || identical(obj, ""),
    # dates
    AddDays = if (inherits(obj, "POSIXct")) obj + a[[1]] * 86400 else obj + a[[1]],
    AddHours = obj + a[[1]] * 3600, AddMinutes = obj + a[[1]] * 60, AddSeconds = obj + a[[1]],
    AddMonths = add_months(obj, a[[1]]), AddYears = add_months(obj, 12 * a[[1]]),
    Subtract = rt_bin("-", obj, a[[1]]),
    # gps
    InRectangle = { if (!inherits(obj, "sus_gps")) FALSE else
      obj$Latitude <= a[[1]] && obj$Longitude >= a[[2]] && obj$Latitude >= a[[3]] && obj$Longitude <= a[[4]] },
    GpsDistance = haversine_m(obj$Latitude, obj$Longitude, a[[1]]$Latitude, a[[1]]$Longitude),
    GpsDistanceKm = haversine_m(obj$Latitude, obj$Longitude, a[[1]]$Latitude, a[[1]]$Longitude) / 1000,
    # nullable
    GetValueOrDefault = if (is_na1(obj)) (if (length(a)) a[[1]] else 0) else obj,
    unsupported(sprintf("Method .%s()", name)))
}

# Static members / calls -----------------------------------------------------
rt_static_member <- function(cls, name) {
  key <- paste0(tolower(cls), ".", tolower(name))
  switch(key,
    "datetime.now" = , "datetime.utcnow" = .get_ctx(parent.frame())$st$now,
    "datetime.today" = as.Date(.get_ctx(parent.frame())$st$now),
    "math.pi" = pi, "math.e" = exp(1),
    "string.empty" = "",
    "int.maxvalue" = , "int32.maxvalue" = .Machine$integer.max,
    "int.minvalue" = , "int32.minvalue" = -.Machine$integer.max,
    "double.nan" = NaN,
    unsupported(paste0(cls, ".", name)))
}

rt_static_call <- function(cls, name, ...) {
  a <- list(...)
  key <- paste0(tolower(cls), ".", tolower(name))
  na_any <- any(vapply(a, is_na1, TRUE))
  num <- function(f) if (na_any) NA else f
  switch(key,
    "math.abs" = num(abs(a[[1]])), "math.max" = num(max(a[[1]], a[[2]])), "math.min" = num(min(a[[1]], a[[2]])),
    "math.round" = num(if (length(a) > 1) round(a[[1]], a[[2]]) else round(a[[1]])),
    "math.floor" = num(floor(a[[1]])), "math.ceiling" = num(ceiling(a[[1]])),
    "math.truncate" = num(trunc(a[[1]])), "math.pow" = num(a[[1]]^a[[2]]), "math.sqrt" = num(sqrt(a[[1]])),
    "math.log" = num(if (length(a) > 1) log(a[[1]], a[[2]]) else log(a[[1]])), "math.log10" = num(log10(a[[1]])),
    "math.exp" = num(exp(a[[1]])), "math.sign" = num(sign(a[[1]])),
    "string.isnullorempty" = is_na1(a[[1]]) || identical(a[[1]], ""),
    "string.isnullorwhitespace" = is_na1(a[[1]]) || !nzchar(trimws(a[[1]])),
    "string.concat" = paste0(vapply(a, rt_tostring, ""), collapse = ""),
    "string.join" = paste(vapply(as_seq(if (length(a) == 2) a[[2]] else a[-1]), rt_tostring, ""), collapse = a[[1]]),
    "datetime.parse" = as.Date(a[[1]]),
    "datetime.daysinmonth" = as.integer(format(as.Date(sprintf("%04d-%02d-01", a[[1]], a[[2]])) + 31 -
                               as.integer(format(as.Date(sprintf("%04d-%02d-01", a[[1]], a[[2]])) + 31, "%d")), "%d")),
    "convert.toint32" = , "convert.toint64" = , "int.parse" = , "long.parse" = , "int32.parse" = as.integer(round(as.numeric(a[[1]]))),
    "convert.todouble" = , "convert.todecimal" = , "double.parse" = , "decimal.parse" = as.numeric(a[[1]]),
    "convert.tostring" = rt_tostring(a[[1]]),
    unsupported(paste0(cls, ".", name, "()")))
}

full_years <- function(d1, d2) {
  if (is_na1(d1) || is_na1(d2)) return(NA)
  a <- as.POSIXlt(d1); b <- as.POSIXlt(d2)
  y <- b$year - a$year
  if (b$mon < a$mon || (b$mon == a$mon && b$mday < a$mday)) y <- y - 1L
  as.integer(y)
}
full_months <- function(d1, d2) {
  if (is_na1(d1) || is_na1(d2)) return(NA)
  a <- as.POSIXlt(d1); b <- as.POSIXlt(d2)
  m <- (b$year - a$year) * 12 + (b$mon - a$mon)
  if (b$mday < a$mday) m <- m - 1L
  as.integer(m)
}

rt_fun <- function(name, ...) {
  a <- list(...)
  switch(name,
    IsAnswered = answered(a[[1]]),
    CountValue = { v <- a[[1]]; sum(vapply(a[-1], function(z) !is_na1(z) && rt_bin("==", z, v), TRUE)) },
    FullYearsBetween = full_years(a[[1]], a[[2]]),
    FullMonthsBetween = full_months(a[[1]], a[[2]]),
    CenturyMonthCode = (a[[2]] - 1900) * 12 + a[[1]],
    IsValidEmail = !is_na1(a[[1]]) && grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", a[[1]]),
    IsMilitaryTime = !is_na1(a[[1]]) && grepl("^([01][0-9]|2[0-3]):?[0-5][0-9]$", a[[1]]),
    unsupported(sprintf("Function %s()", name)))
}
