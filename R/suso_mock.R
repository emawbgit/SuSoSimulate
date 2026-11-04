# ---------------------------------------------------------------------------
# suso_mock.R -- main entry point
# ---------------------------------------------------------------------------

#' Generate mock Survey Solutions data and paradata
#'
#' @param questionnaire Path (Designer JSON, Designer/HQ-export zip, folder, PDF)
#'   or a `suso_questionnaire` object.
#' @param design A [suso_design()] object (sampling frame, teams, calendar).
#' @param config A [suso_mock_config()] object.
#' @param out_dir Output folder. `NULL` returns data without writing.
#' @param seed Random seed for reproducibility.
#' @param zip Also write `<questionnaire>_STATA_All.zip` and `<questionnaire>_Paradata_All.zip`.
#' @param verbose Print progress.
#' @param progress Optional callback `function(value, detail)` (value in 0..1), used by the Shiny app.
#' @return Object of class `suso_mock`: `$data` (named list of data frames, one
#'   per export file), `$system`, `$paradata`, `$coverage`, `$frame`.
#' @export
suso_mock <- function(questionnaire, design = suso_design(), config = suso_mock_config(),
                      out_dir = "mock_export", seed = NULL, zip = FALSE, verbose = TRUE, progress = NULL) {
  check_dependencies()
  if (!inherits(design, "suso_design")) stop("`design` must be created with suso_design().", call. = FALSE)
  if (!inherits(config, "suso_mock_config")) stop("`config` must be created with suso_mock_config().", call. = FALSE)
  validate_config(config)
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) stop("`seed` must be a single number.", call. = FALSE)
    set.seed(seed)
  }
  progress <- if (is.function(progress)) progress else function(...) invisible(NULL)
  qn <- if (inherits(questionnaire, "suso_questionnaire")) questionnaire else suso_read_questionnaire(questionnaire, verbose = verbose)
  frame <- build_frame(design, qn)
  n <- nrow(frame$meta)
  sm_msg(verbose, sprintf("Generating %d interviews (%d PSUs)...", n, length(unique(frame$meta$psu))))
  cov <- new_coverage()
  states <- vector("list", n)
  t0 <- Sys.time()
  step <- max(1L, n %/% 10L)
  for (i in seq_len(n)) {
    states[[i]] <- generate_interview(qn, config, frame_unit(frame, i), cov)
    cov_commit(cov, states[[i]])
    progress(0.7 * i / n, sprintf("Interview %d of %d", i, n))
    if (verbose && (i %% step == 0 || i == n))
      message(sprintf("  %d/%d  (%.1fs)", i, n, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  if (isTRUE(config$coverage_repair)) {
    progress(0.72, "Repairing branch coverage")
    states <- repair_coverage(qn, config, frame, states, cov, verbose)
  }
  n <- length(states)
  nonconv <- sum(!vapply(states, `[[`, TRUE, "converged"))
  if (nonconv) sm_msg(verbose, sprintf("  ! %d interviews did not reach a stable state in %d passes (circular conditions?)", nonconv, config$max_passes))

  st_mix <- config$status_mix / sum(config$status_mix)
  meta <- data.frame(
    key = new_interview_keys(n), id = new_guid(n),
    status = as.numeric(sample(names(st_mix), n, TRUE, prob = st_mix)),
    assignment_id = vapply(states, function(s) as.numeric(s$unit$assignment_id), 0),
    irnd = stats::runif(n),
    n_errors = vapply(states, function(s) {
      e <- log_items(s$errors); e <- Filter(function(x) x$severity == 0, e)
      length(unique(vapply(e, function(x) paste(x$id, rv_str(x$rv)), "")))
    }, 0),
    stringsAsFactors = FALSE)

  progress(0.85, "Building export tables")
  sm_msg(verbose, "Building export tables...")
  tables <- build_tables(qn, states, meta, config, verbose)
  progress(0.93, "Building paradata and system files")
  sm_msg(verbose, "Building paradata and system files...")
  para <- build_paradata(qn, states, meta, config, design)
  sysf <- build_system_files(qn, states, meta, para, config)
  cr <- coverage_report(qn, cov, n)

  res <- structure(list(data = tables, system = sysf, paradata = para$paradata, coverage = cr,
                        frame = cbind(frame$meta[vapply(states, function(s) s$unit$unit, 0), ],
                                      interview__key = meta$key, interview__id = meta$id),
                        questionnaire = qn, config = config, design = design, out_dir = out_dir),
                   class = "suso_mock")
  if (!is.null(out_dir)) write_suso_mock(res, out_dir, zip = zip, verbose = verbose)
  progress(1, "Done")
  res
}

#' Write a `suso_mock` result to disk
#' @param x Result of [suso_mock()].
#' @param out_dir Output folder.
#' @param zip Also create SuSo-style zip archives.
#' @param verbose Print progress.
#' @export
write_suso_mock <- function(x, out_dir, zip = FALSE, verbose = TRUE) {
  qn <- x$questionnaire
  dd <- file.path(out_dir, paste0(qn$varname, "_STATA_All"))
  pd <- file.path(out_dir, paste0(qn$varname, "_Paradata_All"))
  dir.create(dd, recursive = TRUE, showWarnings = FALSE); dir.create(pd, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(x$data)) write_dta_safe(x$data[[nm]], file.path(dd, paste0(nm, ".dta")))
  for (nm in names(x$system)) write_dta_safe(x$system[[nm]], file.path(dd, paste0(nm, ".dta")))
  # readme in the SuSo layout: file name followed by its question variables
  rd <- c(sprintf("Generated by SuSoSimulate (mock Survey Solutions export) on %s", format(Sys.time(), "%A, %d %B %Y")),
          sprintf("The data in this download are SYNTHETIC, generated from the questionnaire \"%s\".", qn$title), "")
  idc <- c("interview__key", "interview__id", "sssys_irnd", "has__errors", "interview__status", "assignment__id")
  for (nm in names(x$data)) {
    v <- setdiff(names(x$data[[nm]]), c(idc, grep("__id$", names(x$data[[nm]]), value = TRUE)))
    rd <- c(rd, paste0(nm, ".dta"), paste(unique(sub("__.*$", "", v)), collapse = ", "), "")
  }
  writeLines(rd, file.path(dd, "export__readme.txt"))
  # paradata.tab: UTF-8 with BOM, tab-separated, as exported by HQ
  write_paradata_tab(x$paradata, file.path(pd, "paradata.tab"))
  writeLines(c('insheet using "paradata.tab", tab case names',
    'label variable interview__id `"Unique 32-character long identifier of the interview"\'',
    'label variable order `"Sequential event number within each interview"\'',
    'label variable event `"Type of event happened"\'',
    'label variable responsible `"Login name of the person who initiated the event"\'',
    'label define role 0 `"<UNKNOWN ROLE>"\' 1 `"Interviewer"\' 2 `"Supervisor"\' 3 `"Headquarter"\' 4 `"Administrator"\' 5 `"API User"\' ',
    'label values role role',
    'label variable role `"System role of the person who initiated the event"\'',
    'label variable timestamp_utc `"Date and time when the event happened"\'',
    'label variable tz_offset `"Timezone offset relative to UTC"\'',
    'label variable parameters `"Event-specific parameters"\''), file.path(pd, "paradata.do"))
  utils::write.csv(x$coverage$branches, file.path(out_dir, "coverage_branches.csv"), row.names = FALSE)
  utils::write.csv(x$coverage$options, file.path(out_dir, "coverage_options.csv"), row.names = FALSE)
  utils::write.csv(x$frame, file.path(out_dir, "sample_frame.csv"), row.names = FALSE)
  cfg <- unclass(x$config); cfg$generators <- names(cfg$generators); cfg$roster_sizes <- lapply(cfg$roster_sizes, function(r) if (is.function(r)) "<function>" else r)
  des <- unclass(x$design); des$frame <- if (is.null(des$frame)) NULL else "<user frame>"; des$preload <- names(des$preload)
  des$start_date <- as.character(des$start_date)
  jsonlite::write_json(list(questionnaire = qn$title, config = cfg, design = des), file.path(out_dir, "mock_config.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  if (zip) for (d in c(dd, pd)) zip_dir(d, paste0(d, ".zip"))
  sm_msg(verbose, sprintf("Written to %s  (%d data files, %d system files, %d paradata events)",
                          normalizePath(out_dir), length(x$data), length(x$system), nrow(x$paradata)))
  invisible(out_dir)
}

#' @export
print.suso_mock <- function(x, ...) {
  s <- x$coverage$summary
  cat(sprintf("<suso_mock> %s\n", x$questionnaire$title))
  cat(sprintf("  Interviews: %d | PSUs: %d | Files: %d data + %d system | Paradata events: %d\n",
              nrow(x$data[[1]]), length(unique(x$frame$psu)), length(x$data), length(x$system), nrow(x$paradata)))
  cat(sprintf("  Branch coverage: %.1f%% of outcomes (%d/%d conditions both ways) | Option coverage: %.1f%%\n",
              100 * s[["branch_coverage"]], as.integer(s[["covered"]]), as.integer(s[["conditions"]]), 100 * s[["option_coverage"]]))
  b <- x$coverage$branches
  if (nrow(b) && any(b$status != "covered")) {
    cat("  Not fully covered:\n")
    print(utils::head(b[b$status != "covered", c("item", "level", "status", "n_true", "n_false", "note")], 20), row.names = FALSE)
  }
  iss <- if (nrow(b)) sum(nzchar(b$issue)) else 0
  if (iss) cat(sprintf("  %d conditions had translation/runtime issues (see $coverage$branches$issue)\n", iss))
  invisible(x)
}

#' Coverage report of a `suso_mock` result
#' @param x Result of [suso_mock()].
#' @export
suso_coverage_report <- function(x) x$coverage

# paradata.tab exactly as HQ writes it: UTF-8 with BOM, tab-separated, CRLF line ends
write_paradata_tab <- function(p, path) {
  cols <- lapply(p, function(x) { x <- as.character(x); x[is.na(x)] <- ""; gsub("[\t\r\n]", " ", enc2utf8(x)) })
  lines <- c(paste(names(p), collapse = "\t"), do.call(paste, c(cols, sep = "\t")))
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
  invisible(path)
}

# Cross-platform zip (no external zip binary needed)
zip_dir <- function(dir, zipfile) {
  zipfile <- normalizePath(zipfile, mustWork = FALSE)
  if (file.exists(zipfile)) file.remove(zipfile)
  zip::zipr(zipfile, list.files(dir, full.names = TRUE, all.files = FALSE, no.. = TRUE))
  invisible(zipfile)
}
