demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")

# demo questionnaire + an enumerator question (single-select of names) on the cover page
demo_with_enumerator <- function() {
  d <- jsonlite::fromJSON(demo, simplifyVector = FALSE)
  q <- list(`$type` = "SingleQuestion", PublicKey = "aaaaaaaa-0000-0000-0000-00000000e001", VariableName = "enumerator",
            QuestionText = "Enumerator name", QuestionType = 0, Featured = FALSE, ConditionExpression = "",
            ValidationConditions = list(),
            Answers = lapply(1:6, function(k) list(AnswerValue = as.character(k), AnswerText = c("Auma", "Rukaka", "Magino",
                                                                                                 "Ayo", "Olwa", "Kirya")[k])))
  d$Children[[1]]$Children <- c(d$Children[[1]]$Children[1:4], list(q), d$Children[[1]]$Children[-(1:4)])
  f <- tempfile(fileext = ".json"); jsonlite::write_json(d, f, auto_unbox = TRUE, null = "null"); f
}

test_that("Tabular export: .tab + .do per file, HQ encodings, same data as Stata", {
  out <- tempfile()
  cfg <- suso_mock_config(item_nonresponse = 0.2, coverage_repair = FALSE)
  rt <- suso_mock(demo, design = suso_design(n_psu = 2, ssu_per_psu = 3), config = cfg, out_dir = out, version = 9,
                  format = "tabular", seed = 11, verbose = FALSE)
  expect_match(basename(rt$export_dir), "^demo_hh_9_SuSoMock_\\d{8}_\\d{6}$")
  td <- file.path(rt$export_dir, "demo_hh_9_Tabular_All")
  expect_true(file.exists(paste0(td, ".zip")))
  files <- c(names(rt$data), names(rt$system))
  expect_true(all(file.exists(file.path(td, paste0(files, ".tab")))))
  expect_true(all(file.exists(file.path(td, paste0(files, ".do")))))
  b <- readBin(file.path(td, "demo_hh.tab"), "raw", 3)
  expect_identical(b, as.raw(c(0xEF, 0xBB, 0xBF)))
  do <- readChar(file.path(td, "members.do"), 200)
  expect_match(do, '^clear\r\ninsheet using "members.tab", tab case names\n\nlabel variable members__id `"Id in members"\'')
  m <- utils::read.delim(file.path(td, "demo_hh.tab"), colClasses = "character", fileEncoding = "UTF-8-BOM", na.strings = character())
  expect_equal(m$interview__id, as.vector(rt$data$demo_hh$interview__id))
  # enabled but unanswered numerics: -999999999 in .tab, Stata .a in .dta
  num <- vapply(rt$data$demo_hh, is.numeric, TRUE)
  tagged <- vapply(rt$data$demo_hh[num], function(x) sum(haven::is_tagged_na(as.numeric(x), "a")), 0)
  expect_gt(sum(tagged), 0)
  expect_equal(sum(m[names(tagged)] == "-999999999"), sum(tagged))
  rs <- suso_mock(demo, design = suso_design(n_psu = 2, ssu_per_psu = 3), config = cfg, out_dir = out, version = 9,
                  format = "stata", seed = 11, verbose = FALSE)
  back <- haven::read_dta(file.path(rs$export_dir, "demo_hh_9_STATA_All", "demo_hh.dta"))
  expect_equal(sum(vapply(back[names(tagged)], function(x) sum(haven::is_tagged_na(as.numeric(x), "a")), 0)), sum(tagged))
})

test_that("cross-file consistency holds and the checker catches breaks", {
  for (ap in c("interview", "psu")) {
    r <- suso_mock(demo_with_enumerator(), design = suso_design(n_psu = 3, ssu_per_psu = 4, assignment_per = ap),
                   config = suso_mock_config(error_rate = 0.1, comment_rate = 0.05), out_dir = NULL, seed = 3, verbose = FALSE)
    v <- suso_validate_mock(r)
    expect_true(all(v$ok), info = paste(ap, paste(v$check[!v$ok], collapse = "; ")))
  }
  bad <- r
  bad$data$members$interview__id[1] <- "ffffffffffffffffffffffffffffffff"
  bad$system$interview__diagnostics$interview__status[1] <- 999
  v <- suso_validate_mock(bad)
  expect_false(all(v$ok))
  expect_true(any(grepl("diagnostics", v$check[!v$ok])))
})

test_that("interviewer question: logins = its option labels, cover fields first", {
  r <- suso_mock(demo_with_enumerator(), design = suso_design(n_psu = 4, ssu_per_psu = 2, n_interviewers = 3),
                 out_dir = NULL, seed = 2, verbose = FALSE)
  expect_equal(r$questionnaire$interviewer_var, "enumerator")
  m <- r$data$demo_hh
  expect_equal(names(m)[3:7], c("region", "district", "ea_id", "hh_id", "enumerator"))
  lab <- c("Auma", "Rukaka", "Magino")[as.numeric(m$enumerator)]
  expect_equal(lab, r$frame$interviewer)
  ia <- r$system$interview__actions
  expect_true(all(ia$originator[as.numeric(ia$action) == 3] %in% c("Auma", "Rukaka", "Magino")))
  r2 <- suso_mock(demo_with_enumerator(), design = suso_design(n_psu = 1, ssu_per_psu = 2), out_dir = NULL,
                  config = suso_mock_config(cover_first = FALSE), seed = 2, verbose = FALSE)
  expect_equal(names(r2$data$demo_hh)[3:5], c("region", "district", "ea_id"))
  expect_warning(suso_mock(demo_with_enumerator(), design = suso_design(n_psu = 8, ssu_per_psu = 1, n_interviewers = 8),
                           out_dir = NULL, seed = 1, verbose = FALSE), "lists 6 names")
})

test_that("survey boundary: GeoJSON polygon keeps PSU centroids inside", {
  gj <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon",
          "coordinates":[[[32,0],[33,0],[32.5,1],[32,0]]]}}]}'
  d <- suso_design(n_psu = 30, ssu_per_psu = 1, boundary = gj)
  expect_equal(d$bbox, c(32, 0, 33, 1))
  r <- suso_mock(demo, design = d, config = suso_mock_config(coverage_repair = FALSE), out_dir = NULL, seed = 1, verbose = FALSE)
  b <- SuSoSimulate:::read_boundary(gj)
  expect_true(all(SuSoSimulate:::in_boundary(b, r$frame$lon, r$frame$lat)))
  expect_error(suso_design(boundary = '{"type":"Point","coordinates":[1,2]}'), "no polygon")
  sq <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10), ncol = 2, byrow = TRUE)
  a <- SuSoSimulate:::sus_area(sq, sq, 0L, TRUE)
  expect_equal(c(a$area, a$len, a$num, a$racc, a$rfrq), c(100, 40, 4, 10, 5))
})

test_that("nullable typed arrays, Quest.IRnd() and RosterVector", {
  ev <- function(src, st) SuSoSimulate:::cs_eval(suso_compile_expression(src)$call,
                                                 list(st = st, qn = NULL, rv = numeric(0), path = character(0)))
  st <- new.env(); st$unit <- list(irnd = 0.25)
  expect_true(ev("new decimal?[]{1,2,3,4}.Contains(3)", st))
  expect_false(ev("new int?[] {1,2}.Contains(5)", st))
  expect_equal(ev("(long)Math.Floor(Quest.IRnd()*8)", st), 2)
})
