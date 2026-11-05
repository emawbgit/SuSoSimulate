# ---------------------------------------------------------------------------
# geo.R -- survey boundary (GeoJSON / lon-lat matrix) and point sampling inside it
# ---------------------------------------------------------------------------

# -> list of rings (two-column lon/lat matrices); holes are ignored
read_boundary <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    m <- as.matrix(x)[, 1:2, drop = FALSE]; storage.mode(m) <- "double"
    return(check_rings(list(m)))
  }
  if (is.character(x) && length(x) == 1 && !grepl("^\\s*[{\\[]", x)) {
    if (!file.exists(x)) stop("Boundary file not found: ", x, call. = FALSE)
    x <- read_text_utf8(x)
  }
  g <- if (is.character(x)) tryCatch(jsonlite::fromJSON(as_utf8(x), simplifyVector = FALSE), error = function(e)
    stop("`boundary` is not valid GeoJSON: ", conditionMessage(e), call. = FALSE)) else x
  if (inherits(g, "sus_boundary")) return(g)
  rings <- list()
  walk <- function(o) {
    t <- o$type %||% ""
    if (t == "FeatureCollection") for (f in o$features) walk(f)
    else if (t == "Feature") walk(o$geometry)
    else if (t == "GeometryCollection") for (gg in o$geometries) walk(gg)
    else if (t == "Polygon") rings[[length(rings) + 1]] <<- ring_matrix(o$coordinates[[1]])
    else if (t == "MultiPolygon") for (p in o$coordinates) rings[[length(rings) + 1]] <<- ring_matrix(p[[1]])
  }
  walk(g)
  check_rings(rings)
}

ring_matrix <- function(coords) do.call(rbind, lapply(coords, function(p) as.numeric(unlist(p))[1:2]))

check_rings <- function(rings) {
  rings <- Filter(function(r) !is.null(r) && nrow(r) >= 3 && !anyNA(r), rings)
  if (!length(rings)) stop("`boundary` contains no polygon with at least 3 points.", call. = FALSE)
  all <- do.call(rbind, rings)
  if (any(abs(all[, 2]) > 90) || any(abs(all[, 1]) > 180))
    stop("`boundary` coordinates must be longitude/latitude in decimal degrees.", call. = FALSE)
  structure(rings, class = "sus_boundary")
}

boundary_bbox <- function(b) { a <- do.call(rbind, b); c(min(a[, 1]), min(a[, 2]), max(a[, 1]), max(a[, 2])) }

# ray casting; TRUE if (x, y) is inside any ring
in_boundary <- function(b, x, y) {
  inside <- rep(FALSE, length(x))
  for (r in b) {
    n <- nrow(r); j <- c(n, seq_len(n - 1))
    for (i in seq_len(n)) {
      xi <- r[i, 1]; yi <- r[i, 2]; xj <- r[j[i], 1]; yj <- r[j[i], 2]
      cross <- ((yi > y) != (yj > y)) & (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
      inside <- xor(inside, cross)
    }
  }
  inside
}

# uniform points inside the boundary (rejection sampling in its bounding box)
sample_in_boundary <- function(b, n) {
  bb <- boundary_bbox(b); out <- matrix(numeric(0), 0, 2)
  for (k in 1:200) {
    m <- max(50, 4 * (n - nrow(out)))
    x <- stats::runif(m, bb[1], bb[3]); y <- stats::runif(m, bb[2], bb[4])
    ok <- in_boundary(b, x, y)
    out <- rbind(out, cbind(x[ok], y[ok]))
    if (nrow(out) >= n) return(out[seq_len(n), , drop = FALSE])
  }
  stop("Could not place points inside `boundary` (is it very thin?).", call. = FALSE)
}
