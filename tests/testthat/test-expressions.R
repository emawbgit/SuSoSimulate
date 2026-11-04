demo <- system.file("extdata", "demo_household.json", package = "SuSoSimulate")
qn <- suso_read_questionnaire(demo, verbose = FALSE)

ev <- function(src, vals = list(), rv = numeric(0), path = character(0), self = NULL) {
  st <- SuSoSimulate:::new_state(list(start_time = as.POSIXct("2024-05-01 10:00:00", tz = "UTC")))
  for (k in names(vals)) assign(k, vals[[k]], envir = st$vals)
  ce <- suso_compile_expression(src, qn$macros)
  expect_null(ce$err)
  SuSoSimulate:::cs_eval(ce$call, list(st = st, qn = qn, rv = rv, path = path, self = self))
}

test_that("C# null semantics", {
  expect_false(SuSoSimulate:::rt_truthy(ev("hhsize > 3")))
  expect_true(ev("hhsize == null"))
  expect_true(ev("hhsize != 3"))
  expect_true(is.na(ev("hhsize + 1")))
  expect_equal(ev("hhsize / 2", list("hhsize|" = 7L)), 3L)          # integer division
  expect_equal(ev("hhsize % 3", list("hhsize|" = 7L)), 1L)
})

test_that("SuSo methods, macros and ternary", {
  expect_true(ev("hhsize.InRange(1, 15)", list("hhsize|" = 4L)))
  expect_true(ev("consent.InList(1, 3)", list("consent|" = 1)))
  expect_equal(ev("consent == 1 ? \"yes\" : \"no\"", list("consent|" = 2)), "no")
  lvl <- qn$by_name$age$path
  expect_true(SuSoSimulate:::rt_truthy(ev("$isAdult", list("age|1" = 30L), rv = 1, path = lvl)))
})

test_that("roster LINQ and multi-select", {
  v <- list("hhsize|" = 3L, "age|1" = 40L, "age|2" = 12L, "age|3" = 70L)
  st_rows <- function(vals) vals
  st <- SuSoSimulate:::new_state(list(start_time = Sys.time()))
  for (k in names(v)) assign(k, v[[k]], envir = st$vals)
  lk <- qn$by_name$members$path
  assign(SuSoSimulate:::row_key(lk[1], numeric(0)), c(1, 2, 3), envir = st$rows)
  ce <- suso_compile_expression("members.Count(x => x.age >= 18)")
  expect_equal(SuSoSimulate:::cs_eval(ce$call, list(st = st, qn = qn, rv = numeric(0), path = character(0))), 2)
  ce <- suso_compile_expression("members.Where(x => x.age < 18).Select(x => x.@rowcode).First()")
  ce2 <- suso_compile_expression("members.Any(x => x.age > 65)")
  expect_true(SuSoSimulate:::cs_eval(ce2$call, list(st = st, qn = qn, rv = numeric(0), path = character(0))))
  m <- SuSoSimulate:::sus_multi(c(2, -98))
  assign("work|1", m, envir = st$vals)
  ce <- suso_compile_expression("work.Contains(-98) && work.Length == 2")
  expect_true(SuSoSimulate:::cs_eval(ce$call, list(st = st, qn = qn, rv = 1, path = lk, self = "work")))
})

test_that("comments, generics and casts parse", {
  for (s in c("/* c */ a == 1 // tail", "x.Sum<int>(y => y) > 2", "(int)a + (double?)b > 0",
              "members.Count (x=>x. age ==2)", "new[] {1,2}.Contains(a)", "a ?? 0"))
    expect_null(suso_compile_expression(s)$err, info = s)
})
