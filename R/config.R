# ---------------------------------------------------------------------------
# config.R -- generation settings and sampling design
# ---------------------------------------------------------------------------

#' Mock-generation settings
#'
#' @param item_nonresponse Share of enabled, non-trigger questions left unanswered.
#' @param error_rate Probability that a question with validations gets a value
#'   violating them (feeds `has__errors`, `interview__errors`, paradata).
#' @param target_prob Probability a generator steers toward branch constants /
#'   least-used options (drives routing coverage).
#' @param roster_mean Mean rows for numeric/list-triggered rosters.
#' @param max_roster_default Cap for roster sizes when no validation bound exists.
#' @param roster_sizes Named list: trigger variable -> `c(min, max)`, a fixed size, or function().
#' @param generators Named list: variable -> `function(rv, unit)` returning a value.
#' @param numeric_roster_base First row code of numeric-triggered rosters (SuSo: 1).
#' @param answer_hidden Fill hidden-scope questions (usually preloaded in SuSo).
#' @param supervisor_answer_rate Share of supervisor/HQ-scope questions answered.
#' @param status_mix Named shares of final interview status codes
#'   (100 Completed, 120 ApprovedBySupervisor, 130 ApprovedByHeadquarters,
#'   65 RejectedBySupervisor, 125 RejectedByHeadquarters).
#' @param tz_offset Local UTC offset of fieldwork, e.g. "+03:00".
#' @param coverage_repair Regenerate interviews until every reachable branch is hit.
#' @param repair_budget Max candidate interviews tried during repair. Repair also stops after
#'   `max(20, repair_budget / 10)` candidates without progress.
#' @param repair_time Max seconds spent on coverage repair (`Inf`: no limit).
#' @param allow_augment Append (instead of only replace) interviews when repairing.
#' @param unsupported_p TRUE-probability for conditions the translator cannot run.
#' @param na_numeric Value for enabled-but-unanswered numerics. Default: Stata extended
#'   missing `.a` (as HQ exports it); written as -999999999 in Tabular exports.
#' @param na_string Value for enabled-but-unanswered strings (SuSo: "##N/A##").
#' @param null_share Scales how often questions tested with `== null` / `IsAnswered()`
#'   are left unanswered (times `target_prob`), so null branches get covered.
#' @param special_value_rate Share of numeric answers drawn from the question's special
#'   values (e.g. -98 "Don't know"), when it has any (still subject to validations).
#' @param cover_first Put the identifying (cover) questions and the interviewer question
#'   right after `interview__key`/`interview__id` in the main file, in the order of the
#'   assignment. `FALSE` keeps HQ's document order.
#' @param restart_rate Share of interviews the interviewer re-opens after completing
#'   (`Completed > Paused > Restarted > ... > Completed` in paradata).
#' @param rejection_rate Share of interviews rejected once by HQ before their final status
#'   (`RejectedBySupervisor > ReceivedByInterviewer > ... > Completed`).
#' @param pause_rate,correction_rate,comment_rate Paradata realism knobs.
#' @param max_passes Max routing passes per interview until answers are stable.
#' @param max_tries Max re-draws per question to satisfy validations.
#' @param max_roster_rows Hard cap on rows of any roster.
#' @export
suso_mock_config <- function(item_nonresponse = 0, error_rate = 0, target_prob = 0.5,
                             roster_mean = 4, max_roster_default = 12, roster_sizes = list(),
                             generators = list(), numeric_roster_base = 1,
                             answer_hidden = TRUE, supervisor_answer_rate = 0.3,
                             status_mix = c("100" = 0.2, "120" = 0.3, "130" = 0.5),
                             tz_offset = "+03:00", coverage_repair = TRUE, repair_budget = 400, repair_time = 120,
                             allow_augment = FALSE, unsupported_p = 0.5, max_passes = 5,
                             max_tries = 25, max_roster_rows = 200,
                             na_numeric = haven::tagged_na("a"), na_string = "##N/A##",
                             null_share = 0.25, special_value_rate = 0.03, cover_first = TRUE, restart_rate = 0.15, rejection_rate = 0.2,
                             pause_rate = 0.3, correction_rate = 0.04, comment_rate = 0.004) {
  cfg <- structure(as.list(environment()), class = "suso_mock_config")
  validate_config(cfg)
  cfg
}

validate_config <- function(cfg) {
  share <- c("item_nonresponse", "error_rate", "target_prob", "supervisor_answer_rate", "unsupported_p",
             "null_share", "special_value_rate", "restart_rate", "rejection_rate",
             "pause_rate", "correction_rate", "comment_rate")
  for (a in share) {
    v <- cfg[[a]]
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0 || v > 1)
      stop(sprintf("`%s` must be a single number between 0 and 1 (got %s).", a, format(v)), call. = FALSE)
  }
  for (a in c("roster_mean", "max_roster_default", "repair_budget", "repair_time", "max_passes", "max_tries", "max_roster_rows")) {
    v <- cfg[[a]]
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0) stop(sprintf("`%s` must be a non-negative number.", a), call. = FALSE)
  }
  if (cfg$max_passes < 1 || cfg$max_tries < 1) stop("`max_passes` and `max_tries` must be >= 1.", call. = FALSE)
  sm <- cfg$status_mix
  ok_codes <- c("65", "100", "120", "125", "130")
  if (!is.numeric(sm) || is.null(names(sm)) || !all(names(sm) %in% ok_codes) || any(sm < 0) || sum(sm) <= 0)
    stop("`status_mix` must be a named numeric vector with names among ", paste(ok_codes, collapse = ", "),
         " and non-negative shares, e.g. c(\"100\" = .2, \"120\" = .3, \"130\" = .5).", call. = FALSE)
  if (!grepl("^[+-]?\\d{1,2}(:?\\d{2})?$", cfg$tz_offset)) stop("`tz_offset` must look like \"+03:00\" or \"-05:00\".", call. = FALSE)
  if (!is.list(cfg$roster_sizes) || (length(cfg$roster_sizes) && is.null(names(cfg$roster_sizes))))
    stop("`roster_sizes` must be a named list, e.g. list(hhsize = c(1, 12)).", call. = FALSE)
  for (v in names(cfg$roster_sizes)) {
    r <- cfg$roster_sizes[[v]]
    if (!(is.function(r) || (is.numeric(r) && length(r) %in% 1:2 && all(r >= 0) && r[1] <= r[length(r)])))
      stop(sprintf("`roster_sizes$%s` must be c(min, max), a single size or a function.", v), call. = FALSE)
  }
  if (!is.list(cfg$generators) || !all(vapply(cfg$generators, is.function, TRUE)))
    stop("`generators` must be a named list of functions(rv, unit).", call. = FALSE)
  invisible(TRUE)
}

#' Sampling / fieldwork design
#'
#' Builds the sampling frame: strata -> PSUs -> secondary units (households),
#' plus interviewer/supervisor allocation, fieldwork calendar and PSU centroids.
#'
#' @param n_psu Number of primary sampling units.
#' @param ssu_per_psu Secondary units (interviews) per PSU: scalar or vector of length `n_psu`.
#' @param strata_var Identifying question receiving the stratum (single-select: cycles its codes).
#' @param n_strata Number of strata when `strata_var` is not categorical.
#' @param psu_var,ssu_var Identifying questions receiving PSU / SSU ids.
#' @param preload Named list: identifying variable -> function(frame) returning a vector,
#'   or a constant, for other cover-page fields.
#' @param frame Optional user frame (data.frame, one row per interview). Columns named
#'   after questionnaire variables are preloaded; optional `.psu`, `.stratum`.
#' @param n_interviewers,interviewers_per_supervisor Field team structure.
#' @param start_date Fieldwork start date.
#' @param interviews_per_day Interviews per interviewer per day.
#' @param bbox GPS bounding box `c(xmin, ymin, xmax, ymax)` for PSU centroids.
#' @param assignment_per "interview" (one assignment each) or "psu".
#' @param interviewer_var Question recording the interviewer (e.g. a single-select of
#'   enumerator names). It is filled with the interviewer the assignment belongs to; for a
#'   single-select its option labels become the interviewer logins, so data, paradata and
#'   system files agree. `NULL`: detected from names such as "enumerator"/"interviewer";
#'   `NA`: none.
#' @param boundary Survey area for PSU centroids: a GeoJSON file or string (e.g. drawn on
#'   geojson.io or the app's OpenStreetMap), a list parsed from GeoJSON, or a two-column
#'   lon/lat matrix. Overrides `bbox`.
#' @export
suso_design <- function(n_psu = 10, ssu_per_psu = 10, strata_var = NULL, n_strata = 1,
                        psu_var = NULL, ssu_var = NULL, preload = list(), frame = NULL,
                        n_interviewers = NULL, interviewers_per_supervisor = 5,
                        start_date = Sys.Date(), interviews_per_day = 4,
                        bbox = c(29.5, -1.5, 35.0, 4.2), assignment_per = c("interview", "psu"),
                        interviewer_var = NULL, boundary = NULL) {
  assignment_per <- match.arg(assignment_per)
  start_date <- tryCatch(as.Date(start_date), error = function(e) NA)
  if (length(start_date) != 1 || is.na(start_date)) stop("`start_date` must be a date, e.g. \"2025-03-01\".", call. = FALSE)
  if (is.null(frame)) {
    if (!is.numeric(n_psu) || length(n_psu) != 1 || n_psu < 1) stop("`n_psu` must be a positive integer.", call. = FALSE)
    n_psu <- as.integer(n_psu)
    if (!is.numeric(ssu_per_psu) || any(ssu_per_psu < 1) || !(length(ssu_per_psu) %in% c(1, n_psu)))
      stop("`ssu_per_psu` must be a positive integer, or a vector with one value per PSU.", call. = FALSE)
    ssu_per_psu <- as.integer(ssu_per_psu)
  } else {
    if (!is.data.frame(frame) || !nrow(frame)) stop("`frame` must be a data.frame with at least one row.", call. = FALSE)
  }
  if (!is.numeric(bbox) || length(bbox) != 4 || bbox[1] >= bbox[3] || bbox[2] >= bbox[4] ||
      any(abs(bbox[c(2, 4)]) > 90) || any(abs(bbox[c(1, 3)]) > 180))
    stop("`bbox` must be c(xmin, ymin, xmax, ymax) in decimal degrees.", call. = FALSE)
  if (!is.null(n_interviewers) && (!is.numeric(n_interviewers) || n_interviewers < 1)) stop("`n_interviewers` must be >= 1.", call. = FALSE)
  if (!is.numeric(interviews_per_day) || interviews_per_day < 1) stop("`interviews_per_day` must be >= 1.", call. = FALSE)
  if (!is.list(preload)) stop("`preload` must be a named list.", call. = FALSE)
  if (!is.null(boundary)) { boundary <- read_boundary(boundary); bbox <- boundary_bbox(boundary) }
  structure(as.list(environment()), class = "suso_design")
}

build_frame <- function(design, qn) {
  d <- design
  for (v in c("strata_var", "psu_var", "ssu_var")) {
    nm <- d[[v]]
    if (!is.null(nm) && is.null(env_get(qn$by_name, nm))) {
      warning(sprintf("`%s = \"%s\"` is not a variable of this questionnaire; ignored.", v, nm), call. = FALSE)
      d[v] <- list(NULL)
    }
  }
  if (!is.null(d$frame)) {
    fr <- as.data.frame(d$frame, stringsAsFactors = FALSE)
    fr[] <- lapply(fr, function(x) if (is.factor(x)) as.character(x) else x)
    n <- nrow(fr)
    psu <- if (!is.null(fr$.psu)) as.integer(factor(fr$.psu, levels = unique(fr$.psu)))
           else if (!is.null(d$psu_var) && !is.null(fr[[d$psu_var]])) as.integer(factor(fr[[d$psu_var]], levels = unique(fr[[d$psu_var]])))
           else rep(1L, n)
    stratum <- fr$.stratum %||% rep(1L, n)
    ssu <- stats::ave(seq_len(n), psu, FUN = seq_along)
  } else {
    m <- rep_len(d$ssu_per_psu, d$n_psu)
    psu <- rep(seq_len(d$n_psu), m)
    ssu <- unlist(lapply(m, seq_len))
    n <- length(psu)
    fr <- data.frame(row.names = seq_len(n))
    sv <- if (!is.null(d$strata_var)) env_get(qn$by_name, d$strata_var) else NULL
    scodes <- if (!is.null(sv) && nrow(sv$options %||% data.frame())) sv$options$code else seq_len(d$n_strata)
    psu_stratum <- rep_len(scodes, d$n_psu)[sample.int(d$n_psu)]
    stratum <- psu_stratum[psu]
    if (!is.null(d$strata_var)) fr[[d$strata_var]] <- stratum
    if (!is.null(d$psu_var)) fr[[d$psu_var]] <- 100 + psu
    if (!is.null(d$ssu_var)) fr[[d$ssu_var]] <- ssu
  }
  for (v in names(d$preload)) {
    p <- d$preload[[v]]
    fr[[v]] <- if (is.function(p)) p(fr) else rep_len(p, n)
  }
  n_psu <- max(psu)
  n_int <- d$n_interviewers %||% max(1L, ceiling(n_psu / 2))
  n_sup <- max(1L, ceiling(n_int / d$interviewers_per_supervisor))
  ints <- sprintf("int%03d", seq_len(n_int))
  # interviewer question: its option labels are the interviewer logins
  iv <- d$interviewer_var
  if (is.null(iv)) iv <- guess_interviewer_var(qn)
  if (length(iv) != 1 || is.na(iv) || !nzchar(iv)) iv <- NULL
  ivn <- if (!is.null(iv)) env_get(qn$by_name, iv) else NULL
  if (!is.null(iv) && (is.null(ivn) || length(ivn$path))) {
    warning(sprintf("`interviewer_var = \"%s\"` is not a main-level question; ignored.", iv), call. = FALSE)
    iv <- ivn <- NULL
  }
  iv_codes <- seq_len(n_int)
  if (!is.null(ivn) && ivn$qtype == "single" && is.null(ivn$lnk) && nrow(ivn$options)) {
    if (n_int > nrow(ivn$options)) {
      warning(sprintf("%d interviewers but '%s' lists %d names: using %d interviewers.", n_int, iv, nrow(ivn$options),
                      nrow(ivn$options)), call. = FALSE)
      n_int <- nrow(ivn$options); n_sup <- max(1L, ceiling(n_int / d$interviewers_per_supervisor))
    }
    iv_codes <- ivn$options$code[seq_len(n_int)]
    lg <- gsub("[^A-Za-z0-9_.-]", "", strip_html(ivn$options$label[seq_len(n_int)]))
    lg[!nzchar(lg)] <- sprintf("int%03d", which(!nzchar(lg)))
    ints <- make.unique(lg, sep = "")
  }
  sups <- sprintf("sup%02d", seq_len(n_sup))
  psu_int <- rep_len(seq_len(n_int), n_psu)
  # calendar: each interviewer works through own PSUs sequentially
  int_of <- psu_int[psu]
  ord <- order(int_of, psu, ssu)
  day <- integer(n); slot <- integer(n)
  for (i in seq_len(n_int)) {
    idx <- ord[int_of[ord] == i]
    k <- seq_along(idx) - 1L
    day[idx] <- k %/% d$interviews_per_day
    slot[idx] <- k %% d$interviews_per_day
  }
  start <- as.POSIXct(paste(as.Date(d$start_date) + day, "08:00:00"), tz = "UTC") +
    slot * 5400 + round(stats::runif(n, 0, 1800))
  bb <- d$bbox
  if (!is.null(d$boundary)) { pt <- sample_in_boundary(d$boundary, n_psu); clon <- pt[, 1]; clat <- pt[, 2] }
  else { clat <- stats::runif(n_psu, bb[2], bb[4]); clon <- stats::runif(n_psu, bb[1], bb[3]) }
  meta <- data.frame(unit = seq_len(n), stratum = stratum, psu = psu, ssu = ssu,
                     interviewer = ints[int_of], supervisor = sups[(int_of - 1L) %/% d$interviewers_per_supervisor + 1L],
                     lat = clat[psu], lon = clon[psu], stringsAsFactors = FALSE)
  meta$start_time <- start
  meta$irnd <- stats::runif(n)                       # sssys_irnd, also returned by Quest.IRnd()
  if (!is.null(ivn) && is.null(fr[[iv]])) fr[[iv]] <- if (ivn$qtype %in% c("text", "barcode")) ints[int_of] else iv_codes[int_of]
  meta$assignment_id <- if (d$assignment_per == "psu") 1000L + psu else 1000L + seq_len(n)
  list(meta = meta, preload = fr, interviewer_var = iv)
}

guess_interviewer_var <- function(qn) {
  rx <- "enumerator|interviewer|field ?officer|enqu[e\u00ea]teur|^fo(_?name|_?id|_?code)?$|^enum_|^int_?(name|id|code)$"
  cand <- Filter(function(nd) identical(nd$kind, "question") && !length(nd$path) && is.null(nd$lnk) &&
                   nd$qtype %in% c("single", "text", "numeric") &&
                   (grepl(rx, tolower(nd$varname)) || grepl("enumerator|interviewer", tolower(strip_html(nd$text)))),
                 lapply(qn$order, function(i) qn$nodes[[i]]))
  if (!length(cand)) return(NULL)
  sing <- Filter(function(nd) nd$qtype == "single", cand)
  (if (length(sing)) sing[[1]] else cand[[1]])$varname
}

frame_unit <- function(frame, i) {
  m <- frame$meta[i, ]
  pre <- if (ncol(frame$preload)) as.list(frame$preload[i, , drop = FALSE]) else list()
  list(unit = m$unit, psu = m$psu, ssu = m$ssu, stratum = m$stratum, interviewer = m$interviewer,
       supervisor = m$supervisor, lat = m$lat, lon = m$lon, start_time = m$start_time,
       assignment_id = m$assignment_id, irnd = m$irnd, preload = pre)
}
