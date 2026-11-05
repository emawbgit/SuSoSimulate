test_that("PDF reader: integer types, wrapped options, conditions and validations", {
  L <- c("COVER", "No sub-sections", "APPENDIX A — CATEGORIES",
         "COVER",
         "Region code NUMERIC: INTEGER reg_code",
         "Plot area NUMERIC: DECIMAL area",
         "Confirm that you are at the s SINGLE-SELECT confirm",
         "ite %site% 01 Yes",
         "E IsAnswered(reg_code) && IsAnswered(are 00 No",
         "a)",
         "Have you connected the panel? SINGLE-SELECT solar",
         "he device? 01 Yes",
         "I Link the solar with the cable 00 No",
         "20 centimeters below the sensor",
         "E confirm==1",
         "Land owner/responsible name TEXT owner",
         "V1 self.ConsistsOf(\"ABC",
         "\") && self.Length>0",
         "APPENDIX A — CATEGORIES")
  testthat::local_mocked_bindings(pdf_lines = function(...) L, .package = "SuSoSimulate")
  f <- tempfile(fileext = ".pdf"); file.create(f)
  q <- suso_read_questionnaire(f, verbose = FALSE)
  nd <- function(v) Filter(function(n) identical(n$varname, v), lapply(q$order, function(i) q$nodes[[i]]))[[1]]
  expect_true(nd("reg_code")$is_integer)
  expect_false(nd("area")$is_integer)
  expect_equal(nd("confirm")$options$code, c(1, 0))
  expect_equal(nd("confirm")$cond_src, "IsAnswered(reg_code) && IsAnswered(area)")
  expect_equal(nd("solar")$options$code, c(1, 0))                 # "20 centimeters" is instruction text
  expect_equal(nd("owner")$kind, "question")                        # "/" in text is not a breadcrumb
  expect_length(q$unsupported_parse, 0)
})

test_that("ConsistsOf and code-like text answers", {
  expect_true(SuSoSimulate:::rt_truthy(SuSoSimulate:::cs_eval(suso_compile_expression('"AB A".ConsistsOf("AB ")')$call, list())))
  g <- SuSoSimulate:::gen_text
  expect_match(g(list(varname = "ea_code", text = "EA code")), "^[0-9]{4}$")
  expect_false(grepl("[0-9]", g(list(varname = "district_name", text = "District name"))))
})
