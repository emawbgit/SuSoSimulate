demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")
fx <- testthat::test_path("fixtures", "edge_cases.json")

test_that("export folder, zip names and contents follow the HQ layout", {
  out <- tempfile()
  r <- suso_mock(demo, design = suso_design(n_psu = 1, ssu_per_psu = 3), out_dir = out, version = 18, seed = 1, verbose = FALSE)
  root <- r$export_dir
  expect_true(all(file.exists(file.path(root, c("demo_hh_18_STATA_All.zip", "demo_hh_18_Paradata_All.zip",
                                                "mock_config.json", "coverage_branches.csv")))))
  zs <- zip::zip_list(file.path(root, "demo_hh_18_STATA_All.zip"))$filename
  expect_true(all(c("demo_hh.dta", "members.dta", "interview__actions.dta", "export__readme.txt",
                    "Questionnaire/content.zip") %in% zs))
  expect_false(any(grepl("^demo_hh_18_STATA_All/", zs)))     # files at the zip root, as HQ
  zp <- zip::zip_list(file.path(root, "demo_hh_18_Paradata_All.zip"))$filename
  expect_setequal(zp, c("export__info.json", "export__readme.txt", "paradata.tab", "paradata.do"))
  info <- jsonlite::fromJSON(file.path(root, "demo_hh_18_Paradata_All", "export__info.json"))
  expect_match(info$QuestionnaireId, "^[0-9a-f]{32}\\$18$")
  # a second write never overwrites the first
  r2 <- write_suso_mock(r, out, timestamp = as.POSIXct(sub("^.*_SuSoMock_", "", basename(root)), format = "%Y%m%d_%H%M%S"),
                        verbose = FALSE)
  expect_false(identical(r2, root))
  expect_error(suso_mock(demo, out_dir = NULL, version = 0, verbose = FALSE), "version")
})

test_that("exported STATA zip reads back with its version and questionnaire", {
  out <- tempfile()
  r <- suso_mock(demo, design = suso_design(n_psu = 1, ssu_per_psu = 2), out_dir = out, version = 4, seed = 2, verbose = FALSE)
  q <- suso_read_questionnaire(file.path(r$export_dir, "demo_hh_4_STATA_All.zip"), verbose = FALSE)
  expect_equal(q$hq_version, 4L)
  expect_equal(q$varname, "demo_hh")
  expect_equal(length(q$nodes), length(r$questionnaire$nodes))
  r3 <- suso_mock(q, design = suso_design(n_psu = 1, ssu_per_psu = 1), out_dir = NULL, seed = 1, verbose = FALSE)
  expect_equal(r3$version, 4L)
})

test_that("paradata event structure matches HQ 24.x", {
  r <- suppressWarnings(suso_mock(fx, design = suso_design(n_psu = 2, ssu_per_psu = 10),
                 config = suso_mock_config(restart_rate = 0.5, rejection_rate = 0.5), out_dir = tempfile(), seed = 5,
                 verbose = FALSE))
  p <- suso_read_paradata(file.path(r$export_dir, "edge_1_Paradata_All.zip"))
  expect_equal(nrow(p), nrow(r$paradata))
  for (id in unique(p$interview__id)) {
    e <- p$event[p$interview__id == id]
    expect_equal(e[1:4], c("InterviewCreated", "SupervisorAssigned", "InterviewModeChanged", "InterviewerAssigned"))
    last <- max(which(e == "Completed"))
    sys <- e[e %in% c("VariableDisabled", "QuestionDeclaredValid", "QuestionDeclaredInvalid", "VariableSet")]
    if (length(sys)) {
      first <- min(which(e %in% unique(sys)))
      blk <- e[first:(last - 1)]
      ord <- match(blk, c("VariableDisabled", "QuestionDeclaredValid", "QuestionDeclaredInvalid", "VariableSet"))
      expect_false(anyNA(ord)); expect_false(is.unsorted(ord))
    }
  }
  expect_true(all(p$tz_offset[p$event %in% c("Restarted", "ReceivedBySupervisor")] == "00:00:00"))
  expect_true(all(p$role[p$event %in% c("QuestionDeclaredValid", "VariableSet")] == 0))
  a <- p[p$event == "AnswerSet", ]
  expect_true(all(nzchar(a$variable)))
  expect_true(all(grepl("^[0-9]+(,[0-9]+)*$|^$", a$rv)))
})
