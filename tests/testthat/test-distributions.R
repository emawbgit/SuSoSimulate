# Ported from IPA's Synthetic-Data-Generator: distributions, correlations, data summary,
# fake addresses; plus the readxl-free .xlsx reader and the base-R installer

demo <- function() system.file("extdata", "demo_household.json", package = "SuSoSimulate")

test_that("distributions are truncated to the validation range and keep their shape", {
  u <- seq(0.001, 0.999, length.out = 200)
  for (d in c("normal", "uniform", "exponential", "lognormal")) {
    x <- vapply(u, function(p) dist_draw(norm_dist(d, "x"), 1, 20, p), 0)
    expect_true(all(x >= 1 & x <= 20), info = d)
    expect_true(all(diff(x) >= 0), info = d)                      # quantile function: monotone in u
  }
  expect_equal(dist_draw(list(dist = "normal", mean = 30, sd = 5), -Inf, Inf, 0.5), 30)
  expect_equal(dist_draw(list(dist = "uniform"), 0, 10, 0.25), 2.5)
  expect_error(norm_dist("gamma", "x"), "must be one of")
  expect_error(norm_dist(list(dist = "normal", rate = 1), "x"), "takes mean, sd")
  expect_error(suso_mock_config(correlations = list(c("a", "b", 1.2))), "between -1 and 1")
})

test_that("the copula gives the target rank correlation; inconsistent pairs are made valid", {
  set.seed(1)
  G <- new.env(); G$cor <- cor_plan(list(c("a", "b", 0.8), c("b", "c", -0.5))); G$st <- new.env(); G$st$latent <- new.env()
  uu <- t(vapply(1:3000, function(i) c(cor_u(G, "a", i), cor_u(G, "b", i), cor_u(G, "c", i)), numeric(3)))
  expect_equal(cor(uu[, 1], uu[, 2], method = "spearman"), 6 / pi * asin(0.8 / 2), tolerance = 0.03)
  expect_lt(cor(uu[, 2], uu[, 3], method = "spearman"), -0.35)
  expect_null(cor_u(G, "zzz", 1))
  u2 <- cor_u(G, "a", 1); expect_true(u2 > 0 && u2 < 1)         # a retry is an independent draw
  bad <- cor_plan(list(c("a", "b", 0.9), c("b", "c", 0.9), c("a", "c", -0.9)))   # not positive definite
  expect_length(bad, 1); expect_true(all(is.finite(bad[[1]]$L)))
})

test_that("numeric_dist and correlations drive the mock data; bad variables are ignored with a warning", {
  cfg <- suso_mock_config(coverage_repair = FALSE,
    numeric_dist = list(age = list(dist = "normal", mean = 40, sd = 8), job_wage = "exponential"),
    correlations = data.frame(var1 = c("crop_area", "age"), var2 = c("harvest_kg", "job_wage"), rho = c(0.8, 0.5)))
  expect_warning(r <- suso_mock(demo(), design = suso_design(n_psu = 15, ssu_per_psu = 10), config = cfg,
                                out_dir = NULL, seed = 2, verbose = FALSE), "same roster.*age~job_wage")
  age <- r$data$members$age
  expect_equal(mean(age), 40, tolerance = 0.1); expect_true(all(age >= 0 & age <= 100))
  expect_true(all(r$data$jobs$job_wage[!is.na(r$data$jobs$job_wage)] > 0))
  expect_warning(check_dist_vars(suso_read_questionnaire(demo(), verbose = FALSE),
                                 suso_mock_config(numeric_dist = list(sex = "normal"))), "not numeric questions.*sex")
  expect_true(all(suso_validate_mock(r)$ok))
})

test_that("app text spec round-trips; summary counts add up; addresses look like addresses", {
  s <- parse_dist_spec("income lognormal meanlog=9 sdlog=1\n# comment\nage normal\nincome ~ spend 0.7")
  expect_equal(s$numeric_dist$income$meanlog, 9); expect_equal(s$correlations$rho, 0.7)
  expect_equal(parse_dist_spec(paste(spec_text(s$numeric_dist, s$correlations), collapse = "\n")), s)
  expect_error(parse_dist_spec("income ~ spend"), "var1 ~ var2")
  r <- suso_mock(demo(), design = suso_design(n_psu = 2, ssu_per_psu = 3), config = suso_mock_config(coverage_repair = FALSE),
                 out_dir = NULL, seed = 3, verbose = FALSE)
  v <- suso_summary(r)
  expect_true(all(v$n == v$disabled + v$unanswered + v$answered))
  expect_true(all(c("members", "demo_hh") %in% v$file) && !"interview__key" %in% v$variable)
  expect_equal(v$mean[v$variable == "age"], mean(r$data$members$age))
  expect_match(gen_text(list(varname = "addr", text = "Address of the dwelling"), "en"), "^PLOT \\d+, .+ ROAD$")
  expect_match(gen_text(list(varname = "adr", text = "Adresse du ménage"), "fr"), "^\\d+ rue ")
})

test_that("reusable categories load from .xlsx without readxl", {
  d <- tempfile(); dir.create(file.path(d, "xl", "worksheets"), recursive = TRUE); dir.create(file.path(d, "_rels"))
  w <- function(f, x) writeBin(charToRaw(enc2utf8(x)), file.path(d, f))
  w("[Content_Types].xml", '<?xml version="1.0"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"/>')
  w("xl/sharedStrings.xml", paste0('﻿<?xml version="1.0" encoding="utf-8"?><x:sst xmlns:x="m"><x:si><x:t>id</x:t></x:si>',
    '<x:si><x:t>text</x:t></x:si><x:si><x:t>parentid</x:t></x:si><x:si><x:t>Côte &amp; d\'Ivoire</x:t></x:si>',
    '<x:si><x:r><x:t>Lac </x:t></x:r><x:r><x:t xml:space="preserve">Tchad&#233;\nNord</x:t></x:r></x:si></x:sst>'))
  w("xl/worksheets/sheet1.xml", paste0('<?xml version="1.0"?><x:worksheet xmlns:x="m"><x:sheetData>',
    '<x:row r="1"><x:c r="A1" t="s"><x:v>0</x:v></x:c><x:c r="B1" t="s"><x:v>1</x:v></x:c><x:c r="C1" t="s"><x:v>2</x:v></x:c></x:row>',
    '<x:row r="2"><x:c r="A2"><x:v>1</x:v></x:c><x:c r="B2" t="s"><x:v>3</x:v></x:c><x:c r="C2" s="1" /></x:row>',
    '<x:row r="3"><x:c r="A3"><x:v>2</x:v></x:c><x:c r="B3" t="s"><x:v>4</x:v></x:c><x:c r="C3"><x:v>1</x:v></x:c></x:row>',
    '<x:row r="4"><x:c r="A4"><x:v>3</x:v></x:c><x:c r="B4" t="inlineStr"><x:is><x:t>Ségou</x:t></x:is></x:c></x:row>',
    '</x:sheetData></x:worksheet>'))
  f <- tempfile(fileext = ".xlsx"); zip::zip(f, c("[Content_Types].xml", "xl"), root = d)
  x <- read_xlsx_min(f)
  expect_equal(names(x), c("id", "text", "parentid"))
  expect_equal(x$id, 1:3); expect_equal(x$parentid, c(NA, 1L, NA))
  expect_equal(x$text, c("Côte & d'Ivoire", "Lac Tchadé\nNord", "Ségou"))
})

test_that("the base-R installer installs from a source folder into a personal library", {
  skip_on_cran()
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  inst <- file.path(root, "install_SuSoSimulate.R")
  skip_if_not(file.exists(inst) && file.exists(file.path(root, "DESCRIPTION")), "not in a source checkout")
  e <- new.env(); sys.source(inst, envir = e)
  lib <- tempfile("lib"); dir.create(lib); old <- .libPaths(); on.exit(.libPaths(old))
  expect_message(e$install_susosimulate(root, optional = character(), lib = lib), "Done: SuSoSimulate")
  expect_true(file.exists(file.path(lib, "SuSoSimulate", "DESCRIPTION")))
})
