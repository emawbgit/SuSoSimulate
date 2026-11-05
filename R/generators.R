# ---------------------------------------------------------------------------
# generators.R -- type-specific answer generators.
# Values respect validation bounds, cascading parents, option filters and
# linked sources; trigger questions are steered toward the constants used in
# enabling conditions so that every branch gets exercised.
# ---------------------------------------------------------------------------

.first_names <- c("Amina", "John", "Grace", "Peter", "Fatuma", "Joseph", "Mary", "David", "Aisha",
  "Samuel", "Esther", "Moses", "Ruth", "Ibrahim", "Sarah", "Daniel", "Joyce", "Paul", "Halima",
  "James", "Agnes", "Emmanuel", "Rose", "Musa", "Faith", "Patrick", "Mercy", "Isaac", "Janet", "Ali")
.last_names <- c("Okello", "Mwangi", "Nakato", "Banda", "Mensah", "Tesfaye", "Phiri", "Otieno",
  "Asante", "Kamau", "Namubiru", "Chirwa", "Boateng", "Wanjiru", "Mutasa", "Abebe", "Achieng",
  "Owusu", "Kato", "Moyo")
.words <- c("field", "market", "water", "school", "road", "harvest", "rain", "seed", "cattle",
  "village", "season", "price", "store", "clinic", "loan", "family", "work", "land", "river", "town")

# word pools in the questionnaire's language (English upper-case, as most HQ data; FR/ES with accents)
pool <- function(lang) {
  p <- .text_pools[[lang %||% "en"]]
  if (is.null(p)) list(first = .first_names, last = .last_names, words = .words, dir = c("A", "B", "CENTRAL", "EAST", "WEST"), up = TRUE)
  else c(p, up = FALSE)
}
gen_name <- function(lang = "en") { p <- pool(lang); x <- paste(sample(p$first, 1), sample(p$last, 1)); if (p$up) toupper(x) else x }

# keyword heuristics run on accent-folded, lower-case variable name + question text (EN/FR/ES/PT)
.kw <- list(
  phone = "phone|\\btel\\b|telephone|telefone|telefono|mobile|celular|portable|movil",
  email = "e-?mail|courriel|correo",
  barcode = "barcode|serial|\\bqr\\b|code.?barre|codigo de barras",
  code = "code|codigo|\\bid\\b|_id\\b|number|numero|\\bno\\b|\\bnum\\b|identifiant|matricule",
  place = paste0("village|parish|ward|town|locality|community|region|district|county|province|state|\\bea\\b|^ea_|site|station|place|",
                 "location|school|facility|commune|departement|prefecture|quartier|ville|localite|municipio|provincia|distrito|",
                 "comunidad|localidad|barrio|canton|zone|zd\\b|ecole|escuela|centre de sante|aldea|vereda|corregimiento"),
  name = "name|nome|\\bnom\\b|prenom|nombre|apellido|respondent|repondant|encuestado|entrevistado|\\bhead\\b|chef",
  person = "member|name|person|child|people|household|membre|enfant|personne|menage|miembro|nino|persona|hogar|hijo")

gen_text <- function(node, lang = "en") {
  if (nzchar(node$mask %||% "")) return(gen_masked(node$mask))
  nm <- node$fold %||% fold(paste(node$varname, node$text))
  p <- pool(lang)
  if (grepl(.kw$phone, nm)) return(paste0("07", paste(sample(0:9, 8, TRUE), collapse = "")))
  if (grepl(.kw$email, nm)) return(paste0(fold(sample(p$first, 1)), sample(10:99, 1), "@example.org"))
  if (grepl(.kw$barcode, nm)) return(paste(sample(c(LETTERS, 0:9), 10, TRUE), collapse = ""))
  if (grepl(.kw$code, nm)) return(sprintf("%04d", sample(1:9999, 1)))
  if (grepl(.kw$place, nm)) { x <- paste(sample(p$last, 1), sample(p$dir, 1)); return(if (p$up) toupper(x) else x) }
  if (grepl(.kw$name, nm)) return(gen_name(lang))
  paste(sample(p$words, sample(2:5, 1), TRUE), collapse = " ")
}

# SuSo masks: '#' digit, '~' letter, '*' any alphanumeric; other chars literal
gen_masked <- function(mask) {
  ch <- strsplit(mask, "")[[1]]
  paste(vapply(ch, function(c) switch(c, "#" = as.character(sample(0:9, 1)),
    "~" = sample(LETTERS, 1), "*" = sample(c(LETTERS, 0:9), 1), c), ""), collapse = "")
}

choose_codes <- function(G, node, codes, k = 1L) {
  k <- min(k, length(codes))
  if (k <= 0) return(codes[0])
  w <- rep(1, length(codes))
  cnt <- G$cov$opt[[node$varname]]
  if (!is.null(cnt) && stats::runif(1) < G$tune$target_prob) {
    c0 <- cnt[fmt_num(codes)]; c0[is.na(c0)] <- 0
    w <- 1 / (1 + c0)^2
  }
  codes[sample.int(length(codes), k, prob = w)]
}

available_options <- function(G, node, rv) {
  o <- node$options
  if (!is.null(node$cascade_from) && nzchar(node$cascade_from)) {
    pv <- tryCatch(ctx_lookup(make_ctx(G, node, rv), node$cascade_from), error = function(e) NA)
    if (is_na1(pv)) return(o[0, ])
    o <- o[!is.na(o$parent) & o$parent == as.numeric(pv[[1]]), ]
  }
  if (!is.null(node$filterc) && nrow(o)) {
    keep <- vapply(o$code, function(cd) eval_bool(G, node$filterc, node, rv, extra = list(optioncode = as.integer(cd)), unsup = TRUE), TRUE)
    o <- o[keep, ]
  }
  o
}

# Candidate roster vectors (or list item codes) for a linked question
linked_candidates <- function(G, node, rv) {
  lk <- node$lnk
  src <- env_get(G$qn$by_name, lk$var)
  if (!is.null(src) && identical(src$kind, "question") && src$qtype == "list") {
    v <- state_value(G$st, src, rv[seq_len(length(src$path))])
    return(lapply(as.numeric(names(v)), function(x) x))
  }
  vp <- lk$path; cp <- node$path
  k <- 0L
  while (k < min(length(vp), length(cp)) && vp[k + 1] == cp[k + 1]) k <- k + 1L
  rvs <- expand_rows(G$st, vp, rv, k)
  if (!isTRUE(lk$roster)) {
    rvs <- Filter(function(r) answered(state_value(G$st, src, r)), rvs)
  }
  if (!is.null(node$lfilterc) && length(rvs)) {
    pseudo <- list(path = vp, kind = "roster", varname = "", id = node$id)
    rvs <- Filter(function(r) eval_bool(G, node$lfilterc, pseudo, r, unsup = TRUE), rvs)
  }
  rvs
}

gen_count <- function(G, node, lo = NA, hi = NA, cap = NA) {
  rs <- G$cfg$roster_sizes[[node$varname]]
  if (is.function(rs)) return(as.integer(rs()))
  if (length(rs) == 2) return(as.integer(pick(seq(rs[1], rs[2]))))
  if (length(rs) == 1 && is.numeric(rs)) return(as.integer(rs))
  lo <- if (is.na(lo)) 1 else max(0, ceiling(lo))
  hi <- min(c(hi, cap, G$cfg$max_roster_default), na.rm = TRUE)
  if (hi < lo) hi <- lo
  n <- lo + stats::rpois(1, max(G$cfg$roster_mean - lo, 0.5))
  as.integer(min(max(n, lo), hi))
}

gen_numeric <- function(G, node, rv) {
  lo <- node$bounds[1]; hi <- node$bounds[2]
  if (G$wide) { lo <- NA; hi <- NA }
  int <- isTRUE(node$is_integer)
  if (isTRUE(node$is_trigger)) {
    # roster triggers: honour branch constants (e.g. `n > 0` needs some zeros), else realistic counts
    if (!is.null(G$cfg$roster_sizes[[node$varname]])) return(gen_count(G, node, lo, hi))  # user setting wins
    tg <- node$targets
    if (length(tg) && stats::runif(1) < G$tune$target_prob / 2) {
      cand <- unique(round(c(tg - 1, tg, tg + 1)))
      cand <- cand[cand >= 0 & (is.na(lo) | cand >= lo) & (is.na(hi) | cand <= hi) &
                   cand <= G$cfg$max_roster_default]
      if (length(cand)) return(as.integer(pick(cand)))
    }
    return(gen_count(G, node, lo, hi))
  }
  if (length(node$domain)) return(finish_num(pick(node$domain), node))   # see finalize_questionnaire
  # numeric special values (e.g. -98 "Don't know")
  if (nrow(node$options) && stats::runif(1) < G$cfg$special_value_rate) return(finish_num(pick(node$options$code), node))
  tg <- node$targets
  if (length(tg) && stats::runif(1) < G$tune$target_prob) {
    cand <- unique(c(tg - 1, tg, tg + 1))
    if (!int) cand <- unique(c(cand, tg - 0.5, tg + 0.5))
    cand <- cand[(is.na(lo) | cand >= lo) & (is.na(hi) | cand <= hi)]
    if (length(cand)) return(finish_num(pick(cand), node))
  }
  nm <- node$fold %||% fold(node$varname)
  x <- if (!is.na(lo) && !is.na(hi)) {
    if (int && (hi - lo) < 1e6) lo + sample.int(floor(hi - lo) + 1, 1) - 1
    else if (hi - lo > 1000 && lo >= 0) min(hi, lo + stats::rlnorm(1, log(max(10, (hi - lo) / 1e4)), 1.2))
    else stats::runif(1, lo, hi)
  } else {
    base <- if (grepl("\\bage\\b|^age|_age|age_|edad|idade", nm)) sample(0:90, 1)
      else if (grepl("year|yr|annee|\\bano\\b|anio", nm)) sample(1960:2025, 1)
      else if (grepl("price|cost|amount|income|value|exp|wage|pay|ugx|kes|usd|prix|cout|montant|revenu|salaire|precio|costo|monto|ingreso|salario|fcfa|xof|cfa|eur", nm)) stats::rlnorm(1, log(5000), 1.2)
      else if (grepl("area|acre|ha$|hect|superficie|surface", nm)) stats::rlnorm(1, log(1.5), 0.8)
      else if (grepl("size|members|num|nb|count|n_|qty|quant|nombre de|combien|cuant|cantidad|taille", nm)) stats::rpois(1, 3) + 1
      else stats::rlnorm(1, log(20), 1)
    if (!is.na(lo)) base <- max(lo, base)
    if (!is.na(hi)) base <- min(hi, base)
    base
  }
  # decimal questions about years, ages, counts, durations are answered in whole numbers in practice
  if (!int && grepl("year|\\byrs?\\b|\\bage\\b|day|month|number of|count|how many|minute|hour|times|annee|\\bans?\\b|jour|mois|nombre de|combien|heure|fois|\\bano|edad|dia|mes\\b|numero de|cuant|hora|veces", nm))
    x <- round(x)
  finish_num(x, node)
}

finish_num <- function(x, node) {
  if (isTRUE(node$is_integer)) {
    x <- round(x)
    return(if (abs(x) < .Machine$integer.max) as.integer(x) else x)
  }
  d <- suppressWarnings(as.integer(node$decimals))
  round(x, if (is.na(d)) 2 else d)
}

# repair steering: 0.6 for variables in the uncovered condition, 0.9 for the gates leading to it
steer_p <- function(G, node) if (node$varname %in% G$tune$focus) 0.6 else if (!isTRUE(node$is_trigger) && node$varname %in% G$tune$enable) 0.9 else 0

gen_value <- function(G, node, rv) {
  user <- G$cfg$generators[[node$varname]]
  if (is.function(user)) {                 # user generators: any encoding or error is contained here
    v <- tryCatch(user(rv = rv, unit = G$st$unit), error = function(e) {
      note_issue(G, node, "generator", conditionMessage(e)); NULL })
    return(if (is.null(v)) NULL else coerce_preload(node, v))
  }
  switch(node$qtype,
    numeric = gen_numeric(G, node, rv),
    single = {
      if (length(node$targets) && is.null(node$lnk) && stats::runif(1) < steer_p(G, node)) {
        o <- available_options(G, node, rv); tc <- intersect(node$targets, o$code)
        if (length(tc)) return(pick(tc))
      }
      if (!is.null(node$lnk)) {
        cand <- linked_candidates(G, node, rv)
        if (!length(cand)) return(NULL)
        return(as.numeric(cand[[sample.int(length(cand), 1)]]))
      }
      o <- available_options(G, node, rv)
      if (!nrow(o)) return(NULL)
      if (!is.null(node$gate) && node$gate %in% o$code && stats::runif(1) < 0.85) return(node$gate)   # gatekeeper (household found, consent)
      choose_codes(G, node, o$code, 1L)
    },
    multi = {
      if (!is.null(node$lnk)) {
        cand <- linked_candidates(G, node, rv)
        if (!length(cand)) return(NULL)
        m <- if (is.na(node$max_answers)) length(cand) else min(node$max_answers, length(cand))
        k <- min(m, 1 + stats::rgeom(1, 0.5))
        return(lapply(cand[sort(sample.int(length(cand), k))], as.numeric))
      }
      o <- available_options(G, node, rv)
      if (!nrow(o)) return(NULL)
      if (isTRUE(node$yesno)) {
        yes <- o$code[stats::runif(nrow(o)) < 0.4]
        if (!is.na(node$max_answers) && length(yes) > node$max_answers) yes <- pick(yes, node$max_answers)
        return(sus_yesno(yes, setdiff(o$code, yes), node$options$code))
      }
      m <- if (is.na(node$max_answers)) nrow(o) else min(node$max_answers, nrow(o))
      k <- if (isTRUE(node$is_trigger)) gen_count(G, node, 1, m) else min(m, 1 + stats::rgeom(1, 0.55))
      pick <- choose_codes(G, node, o$code, k)
      tc <- intersect(node$targets, o$code)
      if (length(tc) && stats::runif(1) < steer_p(G, node)) {
        t1 <- pick(tc)
        if (!(t1 %in% pick)) pick[length(pick)] <- t1
      }
      sus_multi(pick)
    },
    text = gen_text(node, G$qn$lang),
    barcode = paste(sample(0:9, 13, TRUE), collapse = ""),
    picture = paste0(substr(new_guid(), 1, 12), ".jpg"),
    audio = paste0(substr(new_guid(), 1, 12), ".m4a"),
    area = gen_area(G, node),
    date = {
      if (isTRUE(node$is_timestamp)) return(G$st$now)
      d0 <- as.Date(G$st$unit$start_time)
      nm <- node$fold %||% fold(paste(node$varname, node$text))
      if (grepl("birth|dob|born|naissance|nacimiento|nascimento", nm)) d0 - sample(365:(80 * 365), 1) else d0 - sample(0:540, 1)
    },
    gps = {
      a <- gps_anchor(G)   # GPS answers of one interview cluster around the dwelling (~20 m) ...
      sd <- if (stats::runif(1) < 0.08) 0.006 else 0.0002   # ... with occasional off-site readings
      sus_gps(round(a[1] + stats::rnorm(1, 0, sd), 7), round(a[2] + stats::rnorm(1, 0, sd), 7),
              round(stats::runif(1, 3, 20)), round(stats::runif(1, 50, 1800), 1), G$st$now)
    },
    list = {
      cap <- if (is.na(node$max_answers)) 40 else node$max_answers
      n <- if (isTRUE(node$is_trigger)) gen_count(G, node, 1, cap) else min(cap, sample(1:4, 1))
      if (n <= 0) return(NULL)
      nm <- node$fold %||% fold(paste(node$varname, node$text))
      p <- pool(G$qn$lang)
      tx <- if (grepl(.kw$person, nm)) replicate(n, gen_name(G$qn$lang))
            else replicate(n, { x <- paste(sample(p$words, 2), collapse = " "); if (p$up) toupper(x) else x })
      sus_textlist(seq_len(n), tx)
    },
    NULL)
}

gps_anchor <- function(G) {
  if (is.null(G$st$anchor)) { u <- G$st$unit; G$st$anchor <- c(u$lat + stats::rnorm(1, 0, 0.004), u$lon + stats::rnorm(1, 0, 0.004)) }
  G$st$anchor
}

# Geography (area) answer near the dwelling: polygon / polyline / point(s), with the derived
# measures HQ exports (area m2, length m, number of points, requested accuracy / frequency)
gen_area <- function(G, node) {
  a <- gps_anchor(G)
  c0 <- c(a[2] + stats::rnorm(1, 0, 0.002), a[1] + stats::rnorm(1, 0, 0.002))   # lon, lat
  geom <- node$geometry %||% 0L
  auto <- isTRUE(node$geo_auto)
  n <- switch(as.character(geom), "2" = 1L, "3" = sample(3:8, 1), if (auto) sample(25:90, 1) else sample(4:9, 1))
  r <- sqrt(stats::rlnorm(1, log(800), 0.9) / pi)                                # metres
  ang <- sort(stats::runif(n, 0, 2 * pi))
  if (geom == 1L) ang <- seq(0, pi, length.out = n)
  rad <- r * stats::runif(n, 0.8, 1.2)
  m_lat <- 111320; m_lon <- 111320 * cos(c0[2] * pi / 180)
  xy <- cbind(rad * cos(ang), rad * sin(ang))                                     # metres around c0
  if (geom == 2L) xy[] <- 0
  sus_area(cbind(c0[1] + xy[, 1] / m_lon, c0[2] + xy[, 2] / m_lat), xy, geom, auto)
}

sus_area <- function(lonlat, xy, geom = 0L, auto = FALSE) {
  n <- nrow(xy)
  seg <- function(p) sum(sqrt(rowSums(diff(p)^2)))
  closed <- rbind(xy, xy[1, ])
  area <- if (geom == 0L && n > 2) abs(sum(xy[, 1] * xy[c(2:n, 1), 2] - xy[c(2:n, 1), 1] * xy[, 2])) / 2 else 0
  len <- if (geom == 0L && n > 2) seg(closed) else if (geom == 1L) seg(xy) else 0
  structure(list(coords = lonlat, area = area, len = len, num = n, geom = as.integer(geom),
                 racc = if (auto) 10 else NA_real_, rfrq = if (auto) 5 else NA_real_), class = "sus_area")
}
# Geography export formats (HQ workspace setting): "geometry_string" lon,lat;lon,lat;...,
# "wkt" (no line breaks; polygons closed, written as POLYGON) or "geojson"
area_string <- function(v, fmt = "geometry_string") {
  xy <- v$coords; g <- v$geom %||% 0L
  num <- function(x) trimws(formatC(x, digits = 15, format = "fg", flag = ""))
  if (fmt == "geometry_string") return(paste(paste0(num(xy[, 1]), ",", num(xy[, 2])), collapse = ";"))
  if (g == 0L) xy <- rbind(xy, xy[1, , drop = FALSE])                        # close the ring
  pts <- paste(num(xy[, 1]), num(xy[, 2]))
  if (fmt == "wkt") return(switch(as.character(g),
    "1" = sprintf("LINESTRING (%s)", paste(pts, collapse = ", ")),
    "2" = sprintf("POINT (%s)", pts[1]),
    "3" = sprintf("MULTIPOINT (%s)", paste0("(", pts, ")", collapse = ", ")),
    sprintf("POLYGON ((%s))", paste(pts, collapse = ", "))))
  arr <- paste0("[", num(xy[, 1]), ",", num(xy[, 2]), "]")
  switch(as.character(g),
    "1" = sprintf('{"type":"LineString","coordinates":[%s]}', paste(arr, collapse = ",")),
    "2" = sprintf('{"type":"Point","coordinates":%s}', arr[1]),
    "3" = sprintf('{"type":"MultiPoint","coordinates":[%s]}', paste(arr, collapse = ",")),
    sprintf('{"type":"Polygon","coordinates":[[%s]]}', paste(arr, collapse = ",")))
}

# Generate, then re-draw until hard validations pass (or inject an error)
gen_valid <- function(G, node, rv) {
  key <- st_key(node$varname, rv)
  if (length(node$valc) && stats::runif(1) < G$cfg$error_rate) {
    G$wide <- TRUE
    on.exit(G$wide <- FALSE)
    for (t in 1:15) {
      val <- gen_value(G, node, rv)
      if (is.null(val)) break
      assign(key, val, envir = G$st$vals)
      if (length(validate(G, node, rv, errors_only = TRUE))) return(val)
    }
    G$wide <- FALSE
  }
  val <- NULL
  for (t in seq_len(G$cfg$max_tries)) {
    val <- gen_value(G, node, rv)
    if (is.null(val)) return(NULL)
    assign(key, val, envir = G$st$vals)
    if (!length(node$valc) || !length(validate(G, node, rv, errors_only = TRUE))) return(val)
  }
  val
}
