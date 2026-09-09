test_that("confint returns the stored interval", {
  d <- odds_ratio(epi2x2(36, 14, 30, 25))
  expect_equal(confint(d), d$ci)
})

test_that("confint warns when level differs from the level built", {
  d <- odds_ratio(epi2x2(36, 14, 30, 25))          # 95%
  expect_warning(confint(d, level = 0.99), "`level` is ignored")
  expect_warning(confint(d, level = 0.99), "conf_level = 0.99")
})

test_that("confint is silent when level matches", {
  d <- odds_ratio(epi2x2(36, 14, 30, 25), conf_level = 0.99)
  expect_silent(confint(d, level = 0.99))
  expect_silent(confint(d))
})

test_that("the warning still returns the stored interval", {
  d <- odds_ratio(epi2x2(36, 14, 30, 25))
  expect_equal(suppressWarnings(confint(d, level = 0.5)), d$ci)
})

test_that("conf_level on the measure is the way to change the level", {
  narrow <- odds_ratio(epi2x2(36, 14, 30, 25), conf_level = 0.80)
  wide   <- odds_ratio(epi2x2(36, 14, 30, 25), conf_level = 0.99)
  expect_lt(confint(wide)[1], confint(narrow)[1])
  expect_gt(confint(wide)[2], confint(narrow)[2])
})
