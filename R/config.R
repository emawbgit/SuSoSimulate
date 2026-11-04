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
#' @param roster_sizes Named list: trigger variable -> `c(min, max)` or function().
#' @param generators Named list: variable -> `function(rv, unit)` returning a value.
#' @param numeric_roster_base First row code of numeric-triggered rosters (SuSo: 1).
#' @param answer_hidden Fill hidden-scope questions (usually preloaded in SuSo).
#' @param supervisor_answer_rate Share of supervisor/HQ-scope questions answered.
#' @param status_mix Named shares of final interview status codes
#'   (100 Completed, 120 ApprovedBySupervisor, 130 ApprovedByHeadquarters,
#'   65 RejectedBySupervisor, 125 RejectedByHeadquarters).
#' @param tz_offset Local UTC offset of fieldwork, e.g. "+03:00".
#' @param coverage_repair Regenerate interviews until every reachable branch is hit.
#' @param repair_budget Max candidate interviews tried during repair.
#' @param allow_augment Append (instead of only replace) interviews when repairing.
#' @param unsupported_p TRUE-probability for conditions the translator cannot run.
#' @param na_numeric Value for enabled-but-unanswered numerics (SuSo Stata: NA).
#' @param na_string Value for enabled-but-unanswered strings (SuSo: "##N/A##").
#' @param null_share Scales how often questions tested with `== null` / `IsAnswered()`
#'   are left unanswered (times `target_prob`), so null branches get covered.
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
                             tz_offset = "+03:00", coverage_repair = TRUE, repair_budget = 400,
                             allow_augment = FALSE, unsupported_p = 0.5, max_passes = 5,
                             max_tries = 25, max_roster_rows = 200,
                             na_numeric = NA_real_, na_string = "##N/A##",
                             null_share = 0.25,
                             pause_rate = 0.3, correction_rate = 0.04, comment_rate = 0.004) {
  structure(as.list(environment()), class = "suso_mock_config")
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
#' @export
suso_design <- function(n_psu = 10, ssu_per_psu = 10, strata_var = NULL, n_strata = 1,
                        psu_var = NULL, ssu_var = NULL, preload = list(), frame = NULL,
                        n_interviewers = NULL, interviewers_per_supervisor = 5,
                        start_date = Sys.Date(), interviews_per_day = 4,
                        bbox = c(29.5, -1.5, 35.0, 4.2), assignment_per = c("interview", "psu")) {
  assignment_per <- match.arg(assignment_per)
  structure(as.list(environment()), class = "suso_design")
}

build_frame <- function(design, qn) {
  d <- design
  if (!is.null(d$frame)) {
    fr <- as.data.frame(d$frame, stringsAsFactors = FALSE)
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
  clat <- stats::runif(n_psu, bb[2], bb[4]); clon <- stats::runif(n_psu, bb[1], bb[3])
  meta <- data.frame(unit = seq_len(n), stratum = stratum, psu = psu, ssu = ssu,
                     interviewer = ints[int_of], supervisor = sups[(int_of - 1L) %/% d$interviewers_per_supervisor + 1L],
                     lat = clat[psu], lon = clon[psu], stringsAsFactors = FALSE)
  meta$start_time <- start
  meta$assignment_id <- if (d$assignment_per == "psu") 1000L + psu else 1000L + seq_len(n)
  list(meta = meta, preload = fr)
}

frame_unit <- function(frame, i) {
  m <- frame$meta[i, ]
  pre <- if (ncol(frame$preload)) as.list(frame$preload[i, , drop = FALSE]) else list()
  list(unit = m$unit, psu = m$psu, ssu = m$ssu, stratum = m$stratum, interviewer = m$interviewer,
       supervisor = m$supervisor, lat = m$lat, lon = m$lon, start_time = m$start_time,
       assignment_id = m$assignment_id, preload = pre)
}
