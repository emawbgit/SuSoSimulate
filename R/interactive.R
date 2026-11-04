# ---------------------------------------------------------------------------
# interactive.R -- console wizard
# ---------------------------------------------------------------------------

#' Interactive mock-data wizard
#'
#' Prompts for the questionnaire, sample design (strata / PSUs / secondary
#' units), roster dimensions, fieldwork settings and seed, then runs
#' [suso_mock()]. Every answer has a default (press Enter).
#'
#' @param answers Optional named list of pre-filled answers (keys as shown in
#'   brackets in each prompt), used for scripted / non-interactive runs.
#' @return The `suso_mock` result, invisibly.
#' @export
suso_mock_interactive <- function(answers = list()) {
  ask <- function(key, prompt, default = "") {
    if (!is.null(answers[[key]])) return(as.character(answers[[key]]))
    if (!interactive()) return(as.character(default))
    x <- readline(sprintf("%s [%s] (default: %s): ", prompt, key, default))
    if (nzchar(trimws(x))) trimws(x) else as.character(default)
  }
  num <- function(key, prompt, default) {
    v <- suppressWarnings(as.numeric(ask(key, prompt, default)))
    if (is.na(v)) default else v
  }
  yn <- function(key, prompt, default = "y") tolower(substr(ask(key, prompt, default), 1, 1)) == "y"
  cat("\n== Survey Solutions mock data generator ==\n\n")

  path <- ask("questionnaire", "Questionnaire (document.json, Designer/HQ export .zip, folder, or .pdf)", "")
  if (!nzchar(path) && interactive()) path <- file.choose()
  path <- gsub('^"|"$', "", path)
  qn <- suso_read_questionnaire(path)
  print(qn)

  featured <- vapply(qn_questions(qn, function(n) isTRUE(n$featured) && !length(n$path)), `[[`, "", "varname")
  if (length(featured)) cat("\nIdentifying (cover) questions:", paste(featured, collapse = ", "), "\n")
  pick_var <- function(key, prompt) {
    v <- ask(key, prompt, "")
    if (!nzchar(v)) return(NULL)
    if (is.null(env_get(qn$by_name, v))) { cat("  ! unknown variable '", v, "', ignored\n", sep = ""); return(NULL) }
    v
  }
  cat("\n-- Sample design --\n")
  n_psu <- num("n_psu", "Number of PSUs (clusters / EAs)", 10)
  ssu <- num("ssu_per_psu", "Secondary units (interviews) per PSU", 10)
  strata_var <- pick_var("strata_var", "Variable receiving the stratum (blank = none)")
  n_strata <- if (is.null(strata_var) || !nrow(env_get(qn$by_name, strata_var)$options %||% data.frame())) num("n_strata", "Number of strata", 1) else 1
  psu_var <- pick_var("psu_var", "Variable receiving the PSU id (blank = none)")
  ssu_var <- pick_var("ssu_var", "Variable receiving the household/SSU id (blank = none)")
  frame_path <- ask("frame", "Optional preload frame CSV (one row per interview; blank = none)", "")
  frame <- if (nzchar(frame_path)) utils::read.csv(frame_path, stringsAsFactors = FALSE) else NULL

  cat("\n-- Roster dimensions --\n")
  trig <- qn_questions(qn, function(n) isTRUE(n$is_trigger) && n$qtype %in% c("numeric", "list"))
  sizes <- list()
  for (tq in trig) {
    b <- tq$bounds
    r <- ask(paste0("roster_", tq$varname), sprintf("Rows for roster trigger '%s' as min-max (bounds %s..%s)", tq$varname,
             fmt_num(b[1]), fmt_num(b[2])), "auto")
    if (grepl("^[0-9]+ *- *[0-9]+$", r)) sizes[[tq$varname]] <- as.integer(strsplit(gsub(" ", "", r), "-")[[1]])
  }
  roster_mean <- num("roster_mean", "Default mean rows for other variable-size rosters", 4)

  cat("\n-- Fieldwork --\n")
  n_int <- num("n_interviewers", "Number of interviewers", max(1, ceiling(n_psu / 2)))
  start <- as.Date(ask("start_date", "Fieldwork start date (YYYY-MM-DD)", as.character(Sys.Date())))
  tz <- ask("tz_offset", "Local UTC offset", "+03:00")
  bbox <- as.numeric(strsplit(ask("bbox", "GPS bounding box xmin,ymin,xmax,ymax", "29.5,-1.5,35,4.2"), ",")[[1]])
  mix <- ask("status_mix", "Status shares Completed,ApprovedBySupervisor,ApprovedByHQ", "0.2,0.3,0.5")
  mixv <- as.numeric(strsplit(mix, ",")[[1]]); names(mixv) <- c("100", "120", "130")[seq_along(mixv)]

  cat("\n-- Data realism --\n")
  nr <- num("item_nonresponse", "Item non-response share", 0)
  er <- num("error_rate", "Share of validated questions given invalid values", 0)
  rep <- yn("coverage_repair", "Repair coverage so every reachable branch is hit? (y/n)", "y")
  budget <- if (rep) num("repair_budget", "Max candidate interviews for repair", 400) else 0
  seed <- num("seed", "Random seed", 12345)
  out <- ask("out_dir", "Output folder", "mock_export")
  zip <- yn("zip", "Also create SuSo-style zip archives? (y/n)", "n")

  des <- suso_design(n_psu = n_psu, ssu_per_psu = ssu, strata_var = strata_var, n_strata = n_strata,
                     psu_var = psu_var, ssu_var = ssu_var, frame = frame, n_interviewers = n_int,
                     start_date = start, bbox = bbox)
  cfg <- suso_mock_config(item_nonresponse = nr, error_rate = er, roster_sizes = sizes, roster_mean = roster_mean,
                          tz_offset = tz, status_mix = mixv, coverage_repair = rep, repair_budget = budget)
  n_tot <- if (is.null(frame)) n_psu * ssu else nrow(frame)
  cat(sprintf("\nAbout to generate %d interviews into '%s' (seed %s).\n", n_tot, out, seed))
  if (!yn("confirm", "Proceed? (y/n)", "y")) { cat("Cancelled.\n"); return(invisible(NULL)) }
  res <- suso_mock(qn, design = des, config = cfg, out_dir = out, seed = seed, zip = zip)
  print(res)
  cat("\nReproduce non-interactively with the settings saved in ", file.path(out, "mock_config.json"), "\n", sep = "")
  invisible(res)
}
