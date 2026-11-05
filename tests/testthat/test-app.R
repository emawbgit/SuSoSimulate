test_that("Shiny app: full workflow on the demo questionnaire", {
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("DT")
  expect_s3_class(suso_app_object(), "shiny.appobj")
  shiny::testServer(SuSoSimulate:::app_server, {
    session$setInputs(n_psu = 2, ssu_per_psu = 3, n_strata = 1, n_interviewers = 1, ipsup = 5,
                      start_date = as.Date("2025-03-01"), ipd = 4, assignment_per = "interview", tz_offset = "+03:00",
                      xmin = 29.5, xmax = 35, ymin = -1.5, ymax = 4.2, roster_mean = 4, item_nonresponse = 0,
                      error_rate = 0, special_value_rate = 0.03, target_prob = 0.5, coverage_repair = TRUE,
                      repair_budget = 30, allow_augment = FALSE, st100 = 1, st120 = 0, st130 = 0, st65 = 0, st125 = 0,
                      seed = 3, strata_var = "region", psu_var = "ea_id", ssu_var = "hh_id")
    expect_equal(output$vb_q, "-")
    session$setInputs(use_demo = 1)
    expect_equal(rv$qn$varname, "demo_hh")
    expect_equal(output$vb_q, "34")
    session$setInputs(rs_hhsize = c(2, 4))
    session$setInputs(go = 1)
    expect_false(is.null(rv$res))
    expect_true(all(rv$res$data$demo_hh$hhsize %in% c(2:4, NA)))
    expect_equal(output$vb_n, "6")
    z <- output$dl_zip
    files <- zip::zip_list(z)$filename
    expect_true(any(grepl("^demo_hh_1_SuSoMock_\\d{8}_\\d{6}/demo_hh_1_STATA_All\\.zip$", files)))
    expect_true(any(grepl("demo_hh_1_STATA_All/demo_hh.dta$", files)))
    expect_true(any(grepl("paradata.tab$", files)))
    expect_true(any(grepl("reproduce_mock.R$", files)))
    session$setInputs(format = "tabular")
    files_t <- zip::zip_list(output$dl_zip)$filename
    expect_true(any(grepl("demo_hh_1_Tabular_All\\.zip$", files_t)))
    expect_match(paste(output$run_status$html), "Generated 6")
    session$setInputs(n_psu = 5)
    expect_match(paste(output$run_status$html), "Settings changed since this run \\(target now 15")
    expect_match(paste(output$n_total$html), "15")
    session$setInputs(n_psu = NA)
    expect_match(paste(output$n_total$html), "Enter the number")
    session$setInputs(n_psu = 2)
    gj <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[32,0],[33,0],[32.5,1],[32,0]]]}}]}'
    session$setInputs(boundary_geojson = gj)
    expect_equal(rv$boundary, gj)
    expect_false(is.null(settings()$design$boundary))
    session$setInputs(boundary_geojson = '{"type":"FeatureCollection","features":[]}')
    expect_null(rv$boundary)
    sd <- tempfile(); session$setInputs(save_dir = sd, version = 7, save_btn = 1)
    expect_true(file.exists(file.path(rv$saved, "demo_hh_7_Paradata_All.zip")))
    expect_true(file.exists(file.path(rv$saved, "demo_hh_7_Tabular_All.zip")))
    p <- jsonlite::fromJSON(output$dl_params)
    expect_equal(p$seed, 3)
    session$setInputs(n_psu = 9)
    session$setInputs(ul_params = list(datapath = output$dl_params, name = "s.json"))
    bad <- tempfile(fileext = ".json"); writeLines("{oops", bad)
    session$setInputs(qfile = list(datapath = bad, name = "bad.json"))
    expect_true(nzchar(rv$err))
  })
})

test_that("template R code produced by the app runs", {
  demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")
  s <- list(design = list(n_psu = 1, ssu_per_psu = 2, strata_var = "region", n_strata = 1, n_interviewers = 1,
                          interviewers_per_supervisor = 5, start_date = "2025-03-01", interviews_per_day = 4,
                          bbox = c(29.5, -1.5, 35, 4.2), assignment_per = "interview"),
            config = list(item_nonresponse = 0, error_rate = 0, target_prob = 0.5, roster_sizes = list(hhsize = c(1L, 3L)),
                          status_mix = c("100" = 1), tz_offset = "+03:00", coverage_repair = FALSE), seed = 2)
  code <- SuSoSimulate:::app_r_code(s, demo)
  code <- sub('out_dir = "."', "out_dir = NULL, verbose = FALSE", code, fixed = TRUE)
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_s3_class(env$res, "suso_mock")
  do <- SuSoSimulate:::app_do_code(suso_read_questionnaire(demo, verbose = FALSE))
  expect_true(any(grepl('merge m:1 interview__key members__id using "\\$mock/members.dta"', do)))
})
