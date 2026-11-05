demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")

test_that("demo questionnaire: structure, routing and full coverage", {
  out <- tempfile()
  r <- suso_mock(demo, design = suso_design(n_psu = 3, ssu_per_psu = 5, strata_var = "region",
                 psu_var = "ea_id", ssu_var = "hh_id"), out_dir = out, seed = 1, verbose = FALSE)
  expect_s3_class(r, "suso_mock")
  expect_setequal(names(r$data), c("demo_hh", "members", "jobs", "crop_r", "kid_r", "livestock"))
  m <- r$data$demo_hh; mem <- r$data$members; jb <- r$data$jobs
  expect_equal(names(m)[1:2], c("interview__key", "interview__id"))
  expect_equal(utils::tail(names(m), 4), c("sssys_irnd", "has__errors", "interview__status", "assignment__id"))
  expect_equal(names(jb)[1:4], c("interview__key", "interview__id", "members__id", "jobs__id"))
  expect_true(all(grepl("^\\d{2}-\\d{2}-\\d{2}-\\d{2}$", m$interview__key)))
  expect_true(all(nchar(m$interview__id) == 32))
  # routing
  expect_true(all((m$consent == 2) == (m$refusal_reason != "")))
  expect_true(all(((mem$age >= 5) & (mem$age <= 24)) == !is.na(mem$school)))
  expect_true(all((jb$jobs__id == 2) == !is.na(jb$job_wage)))
  expect_true(all(tapply(mem$members__id, mem$interview__id, function(x) all(x == seq_along(x)))))
  # coverage
  expect_equal(unname(r$coverage$summary[["branch_coverage"]]), 1)
  # files on disk
  expect_match(basename(r$export_dir), "^demo_hh_SuSoMock_\\d{8}_\\d{6}$")
  expect_equal(dirname(r$export_dir), normalizePath(out))
  dd <- file.path(r$export_dir, "demo_hh_1_STATA_All")
  expect_true(all(file.exists(file.path(dd, c("demo_hh.dta", "members.dta", "interview__actions.dta",
                                              "interview__diagnostics.dta", "interview__errors.dta")))))
  back <- haven::read_dta(file.path(dd, "members.dta"))
  expect_equal(attr(back$members__id, "label"), "Id in members")
  p <- readLines(file.path(r$export_dir, "demo_hh_1_Paradata_All", "paradata.tab"), n = 2, encoding = "UTF-8")
  expect_match(p[1], "interview__id\torder\tevent\tresponsible\trole\ttimestamp_utc\ttz_offset\tparameters")
})

test_that("seed makes output reproducible", {
  a <- suso_mock(demo, design = suso_design(n_psu = 2, ssu_per_psu = 3), out_dir = NULL, seed = 9, verbose = FALSE)
  b <- suso_mock(demo, design = suso_design(n_psu = 2, ssu_per_psu = 3), out_dir = NULL, seed = 9, verbose = FALSE)
  expect_identical(a$data$members$age, b$data$members$age)
})

test_that("scripted interactive wizard runs", {
  out <- tempfile()
  r <- suso_mock_interactive(answers = list(questionnaire = demo, n_psu = 2, ssu_per_psu = 3, seed = 3,
                                            out_dir = out, roster_hhsize = "2-4", confirm = "y"))
  expect_true(all(r$data$demo_hh$hhsize %in% c(2:4, NA)))
})
