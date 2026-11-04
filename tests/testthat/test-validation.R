test_that("invalid arguments give clear errors", {
  expect_error(suso_mock_config(error_rate = 2), "between 0 and 1")
  expect_error(suso_mock_config(status_mix = c(a = 1)), "status_mix")
  expect_error(suso_mock_config(tz_offset = "Rome"), "tz_offset")
  expect_error(suso_mock_config(roster_sizes = list(5)), "named list")
  expect_error(suso_mock_config(roster_sizes = list(x = c(5, 2))), "roster_sizes")
  expect_error(suso_mock_config(generators = list(x = 1)), "functions")
  expect_error(suso_design(n_psu = 0), "n_psu")
  expect_error(suso_design(n_psu = 3, ssu_per_psu = c(1, 2)), "ssu_per_psu")
  expect_error(suso_design(bbox = c(1, 2)), "bbox")
  expect_error(suso_design(start_date = "not a date"), "start_date")
  expect_error(suso_design(frame = data.frame()), "frame")
  expect_error(suso_read_questionnaire("does_not_exist.json"), "not found")
  demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")
  expect_error(suso_mock(demo, seed = "x", out_dir = NULL, verbose = FALSE), "seed")
  expect_error(suso_mock(demo, design = list(), out_dir = NULL, verbose = FALSE), "suso_design")
  expect_warning(suso_mock(demo, design = suso_design(n_psu = 1, ssu_per_psu = 2, strata_var = "nope"),
                           out_dir = NULL, verbose = FALSE), "not a variable")
})

test_that("malformed questionnaire files fail cleanly", {
  bad <- tempfile(fileext = ".json"); writeLines("{not json", bad)
  expect_error(suso_read_questionnaire(bad, verbose = FALSE))
  txt <- tempfile(fileext = ".txt"); writeLines("x", txt)
  expect_error(suso_read_questionnaire(txt, verbose = FALSE), "Unsupported input")
})

test_that("helpers are safe", {
  set.seed(1)
  expect_identical(SuSoSimulate:::pick(7), 7)              # base sample(7, 1) would draw from 1:7
  expect_length(SuSoSimulate:::pick(numeric(0)), 0)
  nm <- c(paste0(strrep("a", 31), "__Latitude"), paste0(strrep("a", 31), "__Longitude"), "short")
  out <- SuSoSimulate:::stata_names(nm)
  expect_true(all(nchar(out) <= 32)); expect_false(anyDuplicated(out) > 0); expect_identical(out[3], "short")
})

test_that("frames with factor columns and scalar roster sizes work", {
  demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")
  fr <- data.frame(region = factor(c("2", "2", "3")), ea_id = c(7, 7, 8), .psu = c(1, 1, 2))
  res <- suso_mock(demo, design = suso_design(frame = fr), config = suso_mock_config(roster_sizes = list(hhsize = 3)),
                   out_dir = NULL, seed = 1, verbose = FALSE)
  expect_equal(as.numeric(res$data$demo_hh$region), c(2, 2, 3))
  expect_true(all(res$data$demo_hh$hhsize %in% c(3, NA)))
})

test_that("paradata.tab is UTF-8 with BOM and CRLF, as exported by HQ", {
  demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")
  out <- tempfile()
  suso_mock(demo, design = suso_design(n_psu = 1, ssu_per_psu = 2), out_dir = out, seed = 1, zip = TRUE, verbose = FALSE)
  f <- file.path(out, "demo_hh_Paradata_All", "paradata.tab")
  b <- readBin(f, "raw", 2000)
  expect_identical(b[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  expect_true(any(b == as.raw(0x0D)))
  expect_true(file.exists(file.path(out, "demo_hh_STATA_All.zip")))
  expect_gt(nrow(zip::zip_list(file.path(out, "demo_hh_STATA_All.zip"))), 5)
})
