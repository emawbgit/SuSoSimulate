fx <- testthat::test_path("fixtures", "localized")
u <- function(x) SuSoSimulate:::as_utf8(x)          # \u literals are not marked UTF-8 in a C locale
env_get_opts <- function(q, v) get(v, envir = q$by_name)$options$label

test_that("text helpers: UTF-8 repair, folding, safe names", {
  latin1 <- rawToChar(as.raw(c(0x43, 0x61, 0x66, 0xE9)))                 # "Cafe" + e-acute in Latin-1
  expect_identical(u(latin1), u("Café"))
  expect_true(validUTF8(u(latin1)))
  cp1252 <- rawToChar(as.raw(c(0x93, 0x4F, 0x75, 0x69, 0x94)))             # curly quotes (Windows-1252)
  expect_identical(u(cp1252), u("“Oui”"))
  expect_identical(u("a\001b\033c"), "abc")
  expect_identical(SuSoSimulate:::fold(u("Région «Été» – ¿Año? Ærø Łódź")),
                   'region "ete" - ano? aero lodz')
  expect_identical(SuSoSimulate:::safe_file_name(u("enquête ménages / Año")), "enquete_menages_Ano")
  expect_identical(SuSoSimulate:::safe_file_name(u("北京")), "questionnaire")
  expect_identical(SuSoSimulate:::safe_login(u(c("José Peña", "Chloé N’Guessan"))), c("JosePena", "ChloeNGuessan"))
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old))
  suppressWarnings(Sys.setlocale("LC_TIME", "fr_FR.UTF-8"))
  expect_identical(SuSoSimulate:::english_date(as.POSIXct("2026-11-05 10:00", tz = "UTC")), "Thursday, 05 November 2026")
  expect_identical(SuSoSimulate:::fmt_secs(75), "1m 15s")
})

test_that("French/Spanish questionnaire with special characters: Stata and Tabular", {
  qn <- suso_read_questionnaire(fx, verbose = FALSE)
  expect_true(validUTF8(qn$title))
  expect_match(qn$title, u("¿Encuesta"))
  expect_true(qn$lang %in% c("fr", "es"))
  out <- file.path(tempfile(), if (l10n_info()[["UTF-8"]]) u("Données_été_ñ") else "Donnees")
  for (fmt in c("stata", "tabular")) {
    r <- suso_mock(qn, design = suso_design(n_psu = 2, ssu_per_psu = 3, n_interviewers = 2), out_dir = out,
                   format = fmt, seed = 1, verbose = FALSE)
    v <- suso_validate_mock(r)
    expect_true(all(v$ok), info = paste(fmt, paste(v$check[!v$ok], collapse = "; ")))
    expect_equal(r$questionnaire$interviewer_var, "enqueteur")
    expect_true(all(r$frame$interviewer %in% c("JosePena", "FrancoisKouassi")))
    expect_true(dir.exists(r$export_dir))
  }
  st <- list.files(dirname(r$export_dir), "STATA_All$", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)[1]
  d <- haven::read_dta(file.path(st, "enquete_menages.dta"))
  expect_equal(names(attr(d$langue, "labels"))[c(1, 4)], u(c("Français", "Baoulé")))
  expect_equal(names(attr(d$region, "labels"))[3], u("Sud-Comoé « côtier »"))
  do <- SuSoSimulate:::read_text_utf8(file.path(r$export_dir, "enquete_menages_1_Tabular_All", "enquete_menages.do"))
  expect_true(validUTF8(do))
  expect_match(do, u("label define langues 1 `\"Français\"' 2 `\"Español\"'"), fixed = TRUE)
  expect_match(do, u("label variable consent `\"Le ménage accepte-t-il l’entretien ? ¿Acepta el hogar?\"'"), fixed = TRUE)
  tab <- SuSoSimulate:::read_text_utf8(file.path(r$export_dir, "enquete_menages_1_Tabular_All", "enquete_menages.tab"))
  expect_true(validUTF8(tab))
})

test_that("unusable output folders fail early with a clear message", {
  skip_if(l10n_info()[["UTF-8"]], "only a non-UTF-8 locale cannot represent the path")
  expect_error(suso_mock(fx, out_dir = file.path(tempdir(), u("Donn\u00e9es")), verbose = FALSE), "locale")
})

test_that("Latin-1 / Windows-1252 inputs are converted, not fatal", {
  f <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(charToRaw("region,commune\n1,Abobo-Gare\n2,Yopougon-Atti"), as.raw(0xE9), charToRaw("\n"))), f)
  fr <- SuSoSimulate:::read_csv_utf8(f)
  expect_identical(fr$commune[2], u("Yopougon-Attié"))
  raw_fr <- utils::read.csv(f, stringsAsFactors = FALSE)                  # as a user would read it
  r <- suso_mock(fx, design = suso_design(frame = raw_fr), out_dir = NULL, seed = 1, verbose = FALSE)
  expect_identical(as.vector(r$data$enquete_menages$commune), u(c("Abobo-Gare", "Yopougon-Attié")))
  td <- tempfile(); dir.create(file.path(td, "Categories"), recursive = TRUE)
  file.copy(file.path(fx, "document.json"), td)
  writeBin(as.raw(c(charToRaw("id\ttext\n1\tFran"), as.raw(0xE7), charToRaw("ais\n2\tEspa"), as.raw(0xF1), charToRaw("ol\n"))),
           file.path(td, "Categories", "cccccccc0000000000000000000000c1.tsv"))
  q <- suso_read_questionnaire(td, verbose = FALSE)
  expect_identical(env_get_opts(q, "langue"), u(c("Français", "Español")))
})

test_that("generator errors, odd strings and options(OutDec = \",\") do not break exports", {
  old <- options(OutDec = ","); on.exit(options(old))
  bad <- rawToChar(as.raw(c(0x4E, 0x69, 0xF1, 0x6F, 0x09, 0x0D, 0x0A, 0x22, 0x01)))   # Latin-1, tab, CR LF, quote, control
  r <- suso_mock(fx, design = suso_design(n_psu = 1, ssu_per_psu = 2), out_dir = tempfile(), format = "tabular", seed = 2,
                 config = suso_mock_config(generators = list(commune = function(rv, unit) bad,
                                                             hh_id = function(rv, unit) stop("boom"))), verbose = FALSE)
  expect_true(all(validUTF8(as.vector(r$data$enquete_menages$commune))))
  expect_true(all(is.na(as.numeric(r$data$enquete_menages$hh_id))))          # failing generator -> unanswered
  tab <- readLines(file.path(r$export_dir, "enquete_menages_1_Tabular_All", "enquete_menages.tab"), encoding = "UTF-8")
  expect_false(any(grepl("[0-9],[0-9]", sub("^[^\t]*\t[^\t]*\t", "", tab[-1]))))        # decimals keep "."
  expect_equal(SuSoSimulate:::fmt_int(12345), "12,345")
})

test_that("app: accented zip name and Latin-1 frame upload", {
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("DT")
  z <- tempfile(fileext = ".zip"); zip::zipr(z, list.files(fx, full.names = TRUE))
  csv <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(charToRaw("region,commune\n1,Abobo\n2,Atti"), as.raw(0xE9), charToRaw("\n"))), csv)
  shiny::testServer(SuSoSimulate:::app_server, {
    session$setInputs(n_psu = 2, ssu_per_psu = 3, n_strata = 1, n_interviewers = 1, ipsup = 5, start_date = as.Date("2025-03-01"),
                      ipd = 4, assignment_per = "interview", tz_offset = "+00:00", xmin = -6, xmax = -5, ymin = 9, ymax = 10,
                      roster_mean = 3, item_nonresponse = 0, error_rate = 0, special_value_rate = 0, target_prob = 0.5,
                      coverage_repair = FALSE, repair_budget = 0, repair_time = 5, allow_augment = FALSE, st100 = 1, st120 = 0,
                      st130 = 0, st65 = 0, st125 = 0, seed = 1, strata_var = "", psu_var = "", ssu_var = "", version = 3,
                      format = "tabular", interviewer_var = "__auto__")
    session$setInputs(qfile = data.frame(name = u("Enquête ménages.zip"), datapath = z, stringsAsFactors = FALSE))
    expect_null(rv$err)
    session$setInputs(frame_file = list(datapath = csv, name = "frame.csv"))
    expect_identical(rv$frame$commune[2], u("Attié"))
    session$setInputs(go = 1)
    expect_equal(nrow(rv$res$data[[1]]), 2)
    expect_true(any(grepl("enquete_menages_3_Tabular_All\\.zip$", zip::zip_list(output$dl_zip)$filename)))
  })
})
