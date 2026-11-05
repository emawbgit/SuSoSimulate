# ---------------------------------------------------------------------------
# suso_mock.R -- main entry point
# ---------------------------------------------------------------------------

#' Generate mock Survey Solutions data and paradata
#'
#' @param questionnaire Path (Designer JSON, Designer/HQ-export zip, folder, PDF)
#'   or a `suso_questionnaire` object.
#' @param design A [suso_design()] object (sampling frame, teams, calendar).
#' @param config A [suso_mock_config()] object.
#' @param out_dir Folder in which the export folder `<Questionnaire>_<version>_SuSoMock_<STATA|Tabular>`
#'   is created (default: working directory). `NULL` returns data without writing.
#' @param format Export format, as chosen in HQ: `"stata"` (`.dta`, `<Q>_<v>_STATA_All`)
#'   or `"tabular"` (`.tab` + `.do`, `<Q>_<v>_Tabular_All`).
#' @param version Questionnaire version number used in the export names
#'   (`<Questionnaire>_<version>_STATA_All`), as HQ numbers each imported version.
#'   Default: the version detected from an HQ export input, otherwise 1.
#' @param seed Random seed for reproducibility.
#' @param zip Write `<Q>_<version>_STATA_All.zip` and `<Q>_<version>_Paradata_All.zip`
#'   (as downloaded from HQ) next to the unzipped folders.
#' @param verbose Print progress.
#' @param progress Optional callback `function(value, detail)` (value in 0..1), used by the Shiny app.
#' @return Object of class `suso_mock`: `$data` (named list of data frames, one
#'   per export file), `$system`, `$paradata`, `$coverage`, `$frame`.
#' @export
suso_mock <- function(questionnaire, design = suso_design(), config = suso_mock_config(),
                      out_dir = ".", version = NULL, format = c("stata", "tabular"), seed = NULL, zip = TRUE,
                      verbose = TRUE, progress = NULL) {
  check_dependencies()
  format <- match.arg(format)
  if (!inherits(design, "suso_design")) stop("`design` must be created with suso_design().", call. = FALSE)
  if (!inherits(config, "suso_mock_config")) stop("`config` must be created with suso_mock_config().", call. = FALSE)
  validate_config(config)
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) stop("`seed` must be a single number.", call. = FALSE)
    set.seed(seed)
  }
  progress <- if (is.function(progress)) progress else function(...) invisible(NULL)
  if (!is.null(out_dir)) out_dir <- check_out_dir(out_dir)      # fail before generating, not after
  qn <- if (inherits(questionnaire, "suso_questionnaire")) questionnaire else suso_read_questionnaire(questionnaire, verbose = verbose)
  version <- resolve_version(version, qn)
  frame <- build_frame(design, qn)
  qn$interviewer_var <- frame$interviewer_var
  n <- nrow(frame$meta)
  sm_msg(verbose, sprintf("Generating %d interviews (%d PSUs)...", n, length(unique(frame$meta$psu))))
  cov <- new_coverage()
  states <- vector("list", n)
  t0 <- Sys.time(); last <- t0
  step <- max(1L, n %/% 10L)
  for (i in seq_len(n)) {
    states[[i]] <- tryCatch(generate_interview(qn, config, frame_unit(frame, i), cov), error = function(e)
      stop(sprintf("Interview %d of %d failed: %s", i, n, conditionMessage(e)), call. = FALSE))
    cov_commit(cov, states[[i]])
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    eta <- el / i * (n - i)
    progress(0.7 * i / n, sprintf("Interview %d of %d - %s elapsed, about %s left", i, n, fmt_secs(el), fmt_secs(eta)))
    if (verbose && (i %% step == 0 || i == n || difftime(Sys.time(), last, units = "secs") > 10)) {
      message(sprintf("  %d/%d  %s elapsed, ETA %s", i, n, fmt_secs(el), if (i == n) "done" else fmt_secs(eta)))
      last <- Sys.time()
    }
  }
  if (isTRUE(config$coverage_repair)) {
    progress(0.72, "Repairing branch coverage")
    states <- repair_coverage(qn, config, frame, states, cov, verbose, progress = progress)
  }
  n <- length(states)
  nonconv <- sum(!vapply(states, `[[`, TRUE, "converged"))
  if (nonconv) sm_msg(verbose, sprintf("  ! %d interviews did not reach a stable state in %d passes (circular conditions?)", nonconv, config$max_passes))

  st_mix <- config$status_mix / sum(config$status_mix)
  meta <- data.frame(
    key = new_interview_keys(n), id = new_guid(n),
    status = as.numeric(sample(names(st_mix), n, TRUE, prob = st_mix)),
    assignment_id = vapply(states, function(s) as.numeric(s$unit$assignment_id), 0),
    irnd = vapply(states, function(s) s$unit$irnd %||% stats::runif(1), 0),
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

  ui <- vapply(states, function(s) s$unit$unit, 0)
  res <- structure(list(data = tables, system = sysf, paradata = para$paradata, coverage = cr,
                        frame = cbind(frame$meta[ui, ], interview__key = meta$key, interview__id = meta$id),
                        preload = frame$preload[ui, , drop = FALSE],
                        questionnaire = qn, config = config, design = design, version = version,
                        format = format, export_dir = NULL),
                   class = "suso_mock")
  if (!is.null(out_dir)) res$export_dir <- write_suso_mock(res, out_dir, format = format, zip = zip, verbose = verbose)
  progress(1, "Done")
  res
}

# Output folder: created if needed and tested for writing, with a clear message when the
# path cannot be used (e.g. accented folder names in a session running a C/ASCII locale)
check_out_dir <- function(out_dir) {
  if (!is.character(out_dir) || length(out_dir) != 1 || is.na(out_dir) || !nzchar(out_dir))
    stop("`out_dir` must be a folder path.", call. = FALSE)
  out_dir <- as_utf8(out_dir)
  ok <- tryCatch({
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    f <- tempfile("write_test_", tmpdir = out_dir); ok <- file.create(f, showWarnings = FALSE); unlink(f); ok && dir.exists(out_dir)
  }, error = function(e) e, warning = function(w) w)
  if (!isTRUE(ok)) {
    why <- if (inherits(ok, "condition")) conditionMessage(ok) else "not writable"
    hint <- if (!l10n_info()[["UTF-8"]] && any(utf8ToInt(out_dir) > 127L))
      sprintf(" The path has non-ASCII characters and this R session's locale (%s) cannot represent them: use an ASCII folder name, or start R in a UTF-8 locale.", Sys.getlocale("LC_CTYPE")) else ""
    stop(sprintf("Cannot write to the output folder '%s' (%s).%s", out_dir, why, hint), call. = FALSE)
  }
  normalizePath(out_dir, mustWork = FALSE)
}

# <Q>_<version>_SuSoMock_STATA | <Q>_<version>_SuSoMock_Tabular (the export format, as chosen)
mock_dir_name <- function(q, version, format = "stata")
  sprintf("%s_%d_SuSoMock_%s", safe_file_name(q), as.integer(version), if (identical(format, "tabular")) "Tabular" else "STATA")

# "Thursday, 05 November 2026" whatever the session's LC_TIME (HQ readmes are in English)
english_date <- function(t) {
  lt <- as.POSIXlt(t)
  sprintf("%s, %02d %s %d", c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[lt$wday + 1],
          lt$mday, month.name[lt$mon + 1], lt$year + 1900)
}

# Assignments as uploaded to HQ (tab-delimited preload file): one row per assignment
# with its preloaded cover values, responsible interviewer and quantity
assignments_table <- function(x) {
  qn <- x$questionnaire; m <- x$data[[qn$varname]]; fr <- x$frame
  pre <- Filter(function(nd) isTRUE(nd$featured) || identical(nd$varname, qn$interviewer_var),
                lapply(qn$order, function(i) qn$nodes[[i]]))
  pre <- Filter(function(nd) !length(nd$path) && nd$kind == "question", pre)
  cols <- intersect(unlist(lapply(pre, function(nd) vapply(col_specs(qn, nd), `[[`, "", "name"))), names(m))
  a <- data.frame(assignment__id = as.numeric(m$assignment__id), check.names = FALSE)
  for (c in cols) a[[c]] <- m[[c]]
  # only values shared by every interview of the assignment can be preloaded
  keep <- vapply(cols, function(c) all(tapply(tab_cell(a[[c]]), a$assignment__id, function(v) length(unique(v)) == 1)), TRUE)
  a <- a[, c("assignment__id", cols[keep]), drop = FALSE]
  a$`_responsible` <- fr$interviewer[match(m$interview__id, fr$interview__id)]
  a$`_quantity` <- as.vector(table(a$assignment__id)[as.character(a$assignment__id)])
  a[!duplicated(a$assignment__id), , drop = FALSE]
}

resolve_version <- function(version, qn) {
  v <- version %||% (if (!is.na(qn$hq_version %||% NA)) qn$hq_version else 1L)
  if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 1 || v != round(v))
    stop("`version` must be a positive whole number (the HQ questionnaire version, e.g. 18).", call. = FALSE)
  as.integer(v)
}

#' Write a `suso_mock` result to disk, laid out like a Survey Solutions HQ download
#'
#' Creates `<out_dir>/<Q>_<version>_SuSoMock_STATA/` (or `..._Tabular/`) containing
#' `<Q>_<version>_STATA_All` and `<Q>_<version>_Paradata_All` (folders and, with
#' `zip = TRUE`, the matching `.zip` archives with files at the archive root,
#' as HQ produces them), plus coverage reports, the sample frame and settings.
#' `<Q>` is the questionnaire variable name, which also names the main `.dta`.
#'
#' @param x Result of [suso_mock()].
#' @param out_dir Folder in which the export folder is created.
#' @param version Questionnaire version (default: the one used by [suso_mock()]).
#' @param format `"stata"` or `"tabular"` (default: the one used by [suso_mock()]).
#' @param zip Also create the HQ-style zip archives.
#' @param verbose Print progress.
#' @return The export folder path, invisibly.
#' @export
write_suso_mock <- function(x, out_dir = ".", version = x$version, format = x$format %||% "stata", zip = TRUE,
                            verbose = TRUE) {
  qn <- x$questionnaire
  q <- safe_file_name(qn$varname)                  # ASCII, valid on every OS
  version <- resolve_version(version, qn)
  format <- match.arg(format, c("stata", "tabular"))
  tab <- format == "tabular"
  out_dir <- check_out_dir(out_dir)
  root <- file.path(out_dir, mock_dir_name(q, version, format))   # an existing export is never overwritten: _2, _3, ...
  k <- 1L; base <- root
  while (dir.exists(root)) { k <- k + 1L; root <- paste0(base, "_", k) }
  dir.create(root)
  stem <- sprintf("%s_%d", q, version)
  dd <- file.path(root, paste0(stem, if (tab) "_Tabular_All" else "_STATA_All")); pd <- file.path(root, paste0(stem, "_Paradata_All"))
  dir.create(dd); dir.create(pd)
  qid <- tolower(gsub("-", "", qn$id))
  gen <- sprintf("Generated by SuSoSimulate %s (mock Survey Solutions export) on %s",
                 utils::packageVersion("SuSoSimulate"), english_date(Sys.time()))
  head <- c(gen,
    sprintf("The data in this download are SYNTHETIC, generated from the Survey Solutions questionnaire \"%s\" (version %d). ", qn$title, version),
    sprintf("You can open the questionnaire in the Survey Solutions Designer online by that link: https://designer.mysurvey.solutions/questionnaire/details/%s%s",
            qid, if (is.na(qn$revision)) "" else paste0("$", qn$revision)))

  # STATA_All / Tabular_All: data + system files + readme + Questionnaire/content.zip
  ext <- if (tab) ".tab" else ".dta"
  main <- names(x$data)[1]
  for (nm in c(names(x$data), names(x$system))) {
    df <- sanitize_df(x$data[[nm]] %||% x$system[[nm]])      # last guard: valid UTF-8 everywhere
    fn <- safe_file_name(nm)
    tryCatch({
      if (!tab) write_dta_safe(df, file.path(dd, paste0(fn, ext)))
      else {
        df <- stata_safe_names(df)
        write_tab(df, file.path(dd, paste0(fn, ext)))
        kind <- if (nm == main) "main" else if (nm %in% names(x$data)) "roster" else "system"
        write_text_raw(write_do(df, fn, kind), file.path(dd, paste0(fn, ".do")))
      }
    }, error = function(e) stop(sprintf("Could not write %s%s: %s", fn, ext, conditionMessage(e)), call. = FALSE))
  }
  rd <- c(head, "")
  idc <- c("interview__key", "interview__id", "sssys_irnd", "has__errors", "interview__status", "assignment__id")
  for (nm in names(x$data)) {
    v <- setdiff(names(x$data[[nm]]), c(idc, grep("__id$", names(x$data[[nm]]), value = TRUE)))
    rd <- c(rd, paste0(safe_file_name(nm), ext), paste(unique(sub("__.*$", "", v)), collapse = ", "), "")
  }
  write_lines_utf8(rd, file.path(dd, "export__readme.txt"))
  write_questionnaire_content(qn, file.path(dd, "Questionnaire"))

  # Paradata_All: export__info.json + readme + paradata.tab + paradata.do
  jsonlite::write_json(list(HeadquartersVersion = paste("SuSoSimulate", utils::packageVersion("SuSoSimulate")),
                            ExportServiceVersion = paste("SuSoSimulate", utils::packageVersion("SuSoSimulate")),
                            QuestionnaireId = sprintf("%s$%d", qid, version)),
                       file.path(pd, "export__info.json"), auto_unbox = TRUE, pretty = TRUE)
  write_lines_utf8(head, file.path(pd, "export__readme.txt"))
  write_paradata_tab(sanitize_df(x$paradata), file.path(pd, "paradata.tab"))
  write_lines_utf8(c('insheet using "paradata.tab", tab case names',
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

  # reports and settings beside the two exports
  utils::write.csv(x$coverage$branches, file.path(root, "coverage_branches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$coverage$options, file.path(root, "coverage_options.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$frame, file.path(root, "sample_frame.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  asg <- assignments_table(x)                       # HQ upload format: cover variables, _responsible, _quantity
  write_tab(asg[setdiff(names(asg), "assignment__id")], file.path(root, "assignments.tab"))
  cfg <- unclass(x$config); cfg$generators <- names(cfg$generators); cfg$roster_sizes <- lapply(cfg$roster_sizes, function(r) if (is.function(r)) "<function>" else r)
  des <- unclass(x$design); des$frame <- if (is.null(des$frame)) NULL else "<user frame>"; des$preload <- names(des$preload)
  des$start_date <- as.character(des$start_date)
  if (!is.null(des$boundary)) des$boundary <- lapply(unclass(des$boundary), unname)
  jsonlite::write_json(list(questionnaire = qn$title, questionnaire_variable = q, version = version, format = format,
                            config = cfg, design = des),
                       file.path(root, "mock_config.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  if (zip) for (d in c(dd, pd)) zip_dir(d, paste0(d, ".zip"))
  sm_msg(verbose, sprintf("Written to %s  (%d data files, %d system files, %d paradata events)",
                          root, length(x$data), length(x$system), nrow(x$paradata)))
  invisible(root)
}

# Questionnaire/content.zip as in HQ exports: document.json + Categories/<id>.xlsx
write_questionnaire_content <- function(qn, dir) {
  if (is.null(qn$document_json)) return(invisible())
  tmp <- tempfile("content_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  writeBin(charToRaw(enc2utf8(qn$document_json)), file.path(tmp, "document.json"))
  if (length(qn$category_raw)) {
    dir.create(file.path(tmp, "Categories"))
    for (f in names(qn$category_raw)) writeBin(qn$category_raw[[f]], file.path(tmp, "Categories", f))
  }
  if (length(qn$lookup_raw)) {
    dir.create(file.path(tmp, "Lookup Tables"))
    for (f in names(qn$lookup_raw)) writeBin(qn$lookup_raw[[f]], file.path(tmp, "Lookup Tables", f))
  }
  dir.create(dir, showWarnings = FALSE)
  zip_dir(tmp, file.path(dir, "content.zip"))
}

#' Read a Survey Solutions paradata export (real or mock)
#'
#' Reads `paradata.tab` from a file, an unzipped `<Q>_<v>_Paradata_All` folder or
#' its `.zip`, and splits `parameters` into `variable`, `value` and `rv` (roster
#' vector) following the HQ 24.x layout: `AnswerSet`/`VariableSet` `var||value||rv`;
#' `AnswerRemoved`/`QuestionDeclaredValid`/`VariableDisabled` `var||rv`;
#' `QuestionDeclaredInvalid` `var||rv||rule`; `CommentSet` `var||comment[||rv]`.
#'
#' @param path `paradata.tab`, a folder containing it, or a Paradata zip.
#' @return data.frame with the eight paradata columns plus `variable`, `value`,
#'   `rv`, `rule` (0-based validation index) and `time` (POSIXct, UTC).
#' @export
suso_read_paradata <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (dir.exists(path)) path <- file.path(path, "paradata.tab")
  else if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    td <- tempfile("para_"); utils::unzip(path, files = "paradata.tab", exdir = td)
    path <- file.path(td, "paradata.tab")
  }
  if (!file.exists(path)) stop("paradata.tab not found", call. = FALSE)
  p <- utils::read.delim(path, quote = "", colClasses = "character", fileEncoding = "UTF-8-BOM",
                         na.strings = character(), comment.char = "")
  p$order <- as.integer(p$order); p$role <- as.integer(p$role)
  parts <- strsplit(p$parameters, "||", fixed = TRUE)
  f <- function(j) vapply(parts, function(x) if (length(x) >= j) x[j] else "", "")
  f1 <- f(1); f2 <- f(2); f3 <- f(3)
  ev <- p$event
  with_val <- ev %in% c("AnswerSet", "VariableSet")
  var_rv <- ev %in% c("AnswerRemoved", "QuestionDeclaredValid", "QuestionDeclaredInvalid", "VariableDisabled", "QuestionEnabled",
                      "QuestionDisabled", "VariableEnabled")
  # var||value||rv: the value itself may contain "|" (yes/no "1|", lists "a|b"), so split on the outer "||"
  re <- "^([^|]*)\\|\\|(.*)\\|\\|([^|]*)$"
  vv <- with_val & grepl(re, p$parameters)
  p$variable <- ifelse(with_val | var_rv | ev == "CommentSet", f1, NA_character_)
  p$value <- ifelse(vv, sub(re, "\\2", p$parameters), ifelse(with_val | ev == "CommentSet", f2, NA_character_))
  p$rv <- ifelse(vv, sub(re, "\\3", p$parameters), ifelse(with_val | ev == "CommentSet", f3, ifelse(var_rv, f2, NA_character_)))
  p$rule <- ifelse(ev == "QuestionDeclaredInvalid", suppressWarnings(as.integer(f3)), NA_integer_)
  p$time <- as.POSIXct(p$timestamp_utc, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  p
}

#' @export
print.suso_mock <- function(x, ...) {
  s <- x$coverage$summary
  cat(sprintf("<suso_mock> %s (%s, version %s)\n", x$questionnaire$title, x$questionnaire$varname, x$version %||% 1))
  if (!is.null(x$export_dir)) cat("  Export:", x$export_dir, "\n")
  cat(sprintf("  Interviews: %d | PSUs: %d | Files: %d data + %d system | Paradata events: %d\n",
              nrow(x$data[[1]]), length(unique(x$frame$psu)), length(x$data), length(x$system), nrow(x$paradata)))
  cat(sprintf("  Branch coverage: %.1f%% of outcomes (%d/%d conditions both ways) | Option coverage: %.1f%%\n",
              100 * s[["branch_coverage"]], as.integer(s[["covered"]]), as.integer(s[["conditions"]]), 100 * s[["option_coverage"]]))
  b <- x$coverage$branches
  un <- if (nrow(b)) b$status != "covered" & startsWith(b$note, "unreachable") else logical()
  if (any(un)) cat(sprintf("  %d conditions cannot be covered (constant switches, validation bounds): see $coverage$branches$note\n", sum(un)))
  if (nrow(b) && any(b$status != "covered" & !un)) {
    cat("  Not fully covered:\n")
    print(utils::head(b[b$status != "covered" & !un, c("item", "level", "status", "n_true", "n_false", "note")], 20), row.names = FALSE)
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
