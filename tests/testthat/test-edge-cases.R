fx <- test_path("fixtures", "edge_cases.json")

test_that("edge-case questionnaire generates, stays consistent and routes correctly", {
  res <- suppressWarnings(suso_mock(fx, design = suso_design(n_psu = 4, ssu_per_psu = 8), out_dir = NULL,
                                    seed = 5, verbose = FALSE))
  expect_identical(check_integrity(res), character())
  expect_setequal(names(res$data), c("edge", "kids", "symptom_r", "treat_r", "plot_r"))
  m <- res$data$edge; k <- res$data$kids; s <- res$data$symptom_r; tr <- res$data$treat_r; p <- res$data$plot_r
  # calculated variables: DateTime.Today, LINQ over rosters, SelectMany, string building
  expect_false(all(is.na(m$resp_age)))
  expect_true(all(m$is_old == as.numeric(m$resp_age >= 60), na.rm = TRUE))
  expect_true(all(m$tag == paste0(toupper(m$village), "-", m$hhid)))
  kmax <- vapply(m$interview__id, function(i) { a <- k$kid_age[k$interview__id == i]; if (length(a)) max(a) else NA_real_ }, 0)
  expect_equal(as.vector(m$kid_max), unname(kmax))
  expect_equal(as.vector(m$n_sick), unname(vapply(m$interview__id, function(i) sum(s$interview__id == i), 0)))
  # yes/no-triggered roster rows == number of "yes" answers
  expect_equal(sum(as.matrix(as.data.frame(k)[, grep("^symptoms__", names(k))]) == 1, na.rm = TRUE), nrow(s))
  # fixed roster with per-row condition: disabled instances are not exported
  j <- merge(as.data.frame(tr), as.data.frame(s), by = c("interview__id", "kids__id", "symptom_r__id"))
  expect_true(all(j$treat_r__id == 1 | j$days_sick > 3))
  # option filter
  expect_true(all(p$plot_crop != 3 | p$plot_size > 1, na.rm = TRUE))
  # zero-row branch and special values occur
  expect_true(any(m$n_kids == 0))
  expect_true(all(is.na(m$oldest[m$n_kids == 0])))
  # circular conditions never produce the contradictory state
  expect_false(any(m$circ_a %in% 1 & m$circ_b %in% 1))
  # untranslatable expressions are flagged, not fatal
  iss <- res$coverage$branches$issue
  expect_true(any(grepl("lookup table", iss)))
  expect_true(any(grepl("Unexpected token", iss)))
  expect_false(any(grepl("DateTime", iss)))
})

test_that("long variable names are shortened safely for Stata", {
  out <- tempfile()
  expect_warning(r <- suso_mock(fx, design = suso_design(n_psu = 1, ssu_per_psu = 2), out_dir = out, seed = 1, zip = FALSE,
                                verbose = FALSE), "32 characters")
  d <- haven::read_dta(file.path(r$export_dir, "edge_1_STATA_All", "edge.dta"))
  expect_true(all(nchar(names(d)) <= 32))
  expect_false(anyDuplicated(names(d)) > 0)
})

test_that("noisy settings still give a consistent export", {
  cfg <- suso_mock_config(item_nonresponse = 0.4, error_rate = 0.5, comment_rate = 0.2, correction_rate = 0.5,
                          pause_rate = 1, status_mix = c("65" = 1, "125" = 1, "130" = 1), allow_augment = TRUE, repair_budget = 30)
  res <- suppressWarnings(suso_mock(fx, design = suso_design(n_psu = 2, ssu_per_psu = 4), config = cfg,
                                    out_dir = NULL, seed = 2, verbose = FALSE))
  expect_identical(check_integrity(res), character())
  expect_gt(nrow(res$system$interview__errors), 0)
  expect_true(all(res$data$edge$has__errors >= 0))
})

test_that("user roster sizes take precedence over branch steering", {
  res <- suppressWarnings(suso_mock(fx, design = suso_design(n_psu = 2, ssu_per_psu = 5),
                                    config = suso_mock_config(roster_sizes = list(n_kids = c(2, 3)), target_prob = 1),
                                    out_dir = NULL, seed = 4, verbose = FALSE))
  expect_true(all(res$data$edge$n_kids %in% 2:3))
})
