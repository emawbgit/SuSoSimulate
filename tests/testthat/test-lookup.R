# Lookup tables, HQ 26.x "$type" dictionaries, comma-list macros, multi-select combo
# boxes, filter-driven id domains and constant switches (patterns of a real 26.07 export)

make_lookup_fixture <- function() {
  d <- jsonlite::fromJSON(system.file("extdata", "demo_household.json", package = "SuSoSimulate"), simplifyVector = FALSE)
  d$Macros <- c(list(`$type` = "System.Collections.Generic.Dictionary`2[...Macro...]"), d$Macros,
                list(m2 = list(`$type` = "Macro", Name = "codes", Content = "1,2", Description = "")))
  tid <- "aaaaaaaa-0000-1111-2222-333333333333"
  d$LookupTables <- stats::setNames(list("System.Collections.Generic.Dictionary`2[...LookupTable...]",
                                         list(`$type` = "LookupTable", TableName = "rates", FileName = "rates.txt")),
                                    c("$type", tid))
  q <- function(type, id, var, ...) c(list(`$type` = type, PublicKey = sprintf("f00000000000000000000000000000%02d", id), VariableName = var), list(...))
  opts <- function(codes, parent = NULL) lapply(seq_along(codes), function(i)
    c(list(AnswerText = paste("Option", codes[i]), AnswerValue = as.character(codes[i])),
      if (!is.null(parent)) list(ParentValue = as.character(parent[i]))))
  hh <- d$Children[[2]]$Children
  hh[[2]]$Children <- c(hh[[2]]$Children, list(
    list(`$type` = "Variable", PublicKey = "f0000000000000000000000000000001", Name = "w_rate", Type = 2,
         Expression = "rates[@rowindex % 3 + 1].factor")))
  hh <- c(hh, list(
    list(`$type` = "Variable", PublicKey = "f0000000000000000000000000000002", Name = "rate_two", Type = 2,
         Expression = "rates.ContainsKey(2) ? rates[2].factor + rates.Values.Sum(x => x.factor) : -1"),
    list(`$type` = "Variable", PublicKey = "f0000000000000000000000000000003", Name = "off", Type = 1, Expression = "0"),
    q("NumericQuestion", 4, "listed", QuestionType = 4, QuestionText = "Region listed?", IsInteger = TRUE,
      VariableLabel = "Region <= 2 & listed",
      ConditionExpression = "region.InList($codes)"),
    q("NumericQuestion", 5, "switched", QuestionType = 4, QuestionText = "Switched off", IsInteger = TRUE,
      ConditionExpression = "off == 1"),
    q("MultyOptionsQuestion", 6, "items", QuestionType = 3, QuestionText = "Items", IsFilteredCombobox = TRUE,
      MaxAllowedAnswers = 3, VariableLabel = "Items held", Answers = opts(1:6)),
    q("NumericQuestion", 7, "hh_key", QuestionType = 4, QuestionText = "Household key", IsInteger = TRUE),
    q("SingleQuestion", 8, "pick", QuestionType = 0, QuestionText = "Pick a listed member", IsFilteredCombobox = TRUE,
      Answers = opts(c(101:103, 201:202, 501), c(1, 1, 1, 2, 2, 5)),
      Properties = list(OptionsFilterExpression = "@optioncode >= hh_key*100+1 && @optioncode <= hh_key*100+3"))))
  d$Children[[2]]$Children <- hh
  dir <- tempfile("lookup_q"); dir.create(file.path(dir, "Lookup Tables"), recursive = TRUE)
  jsonlite::write_json(d, file.path(dir, "document.json"), auto_unbox = TRUE, null = "null")
  writeLines(c("rowcode\tfactor", "1\t10", "2\t20", "3\t30"), file.path(dir, "Lookup Tables", "aaaaaaaa000011112222333333333333.txt"))
  dir
}

test_that("lookup tables, $type dictionaries, list macros, combo boxes, filter domains, constant switches", {
  qn <- suso_read_questionnaire(make_lookup_fixture(), verbose = FALSE)
  expect_equal(qn$lookup$rates$factor, c(10, 20, 30))
  expect_equal(unname(qn$macros["codes"]), "1,2")
  expect_length(qn$unsupported_parse, 0)
  r <- suso_mock(qn, design = suso_design(n_psu = 2, ssu_per_psu = 5), config = suso_mock_config(coverage_repair = FALSE),
                 out_dir = tempfile(), seed = 5, verbose = FALSE)
  m <- r$data$demo_hh; mem <- r$data$members
  k <- m$consent %in% 1                                            # the household section needs consent
  expect_true(all(mem$w_rate[!is.na(mem$w_rate)] %in% c(10, 20, 30)))
  expect_true(any(!is.na(mem$w_rate)))
  expect_true(all(m$rate_two[k] == 80) && any(k))
  # list macro: InList(1,2) parses and runs
  b <- r$coverage$branches
  expect_equal(b$issue[b$item == "listed"], "")
  expect_match(b$note[b$item == "switched"], "unreachable: off is the constant 0")
  # combo box multi-select: items__0..items__2 holding codes, labelled "Items held:k"
  expect_true(all(paste0("items__", 0:2) %in% names(m)) && !"items__6" %in% names(m))
  expect_equal(attr(m$items__1, "label"), "Items held:1")
  vals <- unlist(m[paste0("items__", 0:2)]); vals <- vals[!is.na(vals)]
  expect_true(all(vals %in% 1:6))
  # filter domain: hh_key drawn from the parent codes, so pick always has an allowed option
  expect_true(all(m$hh_key[k] %in% c(1, 2, 5)))
  expect_true(all(!is.na(m$pick[k]) & floor(m$pick[k] / 100) == m$hh_key[k]))
  # Stata labels as in a 26.07 export: & < > escaped; list columns "label:k" / "label:kc"
  expect_equal(attr(m$listed, "label"), "Region &lt;= 2 &amp; listed")
  expect_match(attr(m$kids__0, "label"), ":0$"); expect_match(attr(m$kids__1c, "label"), ":1c$")
  expect_true(all(c("TargetAreaChanged", "Reopened") %in% names(attr(r$system$assignment__actions$action, "labels"))))
  # the lookup table travels in Questionnaire/content.zip and is read back
  cz <- file.path(r$export_dir, "demo_hh_1_STATA_All", "Questionnaire", "content.zip")
  expect_true(any(grepl("^Lookup Tables/", zip::zip_list(cz)$filename)))
  expect_equal(suso_read_questionnaire(cz, verbose = FALSE)$lookup$rates$factor, c(10, 20, 30))
})
