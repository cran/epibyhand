test_that("default labels read as prose", {
  d <- odds_ratio(epi2x2(36, 14, 30, 25))
  labs <- steps_table(d)$label
  expect_true(any(grepl("among the exposed", labs, fixed = TRUE)))
  expect_true(any(grepl("among the unexposed", labs, fixed = TRUE)))
})

test_that("custom labels are parenthesised, not pasted after 'the'", {
  d <- odds_ratio(epi2x2(54, 16, 8, 42,
                         exposure = c("Ate potato salad", "Did not eat"),
                         outcome  = c("Ill", "Well")))
  labs <- steps_table(d)$label

  expect_false(any(grepl("the ate potato salad", labs, fixed = TRUE)))
  expect_true(any(grepl("among the exposed (Ate potato salad)", labs,
                        fixed = TRUE)))
  expect_true(any(grepl("among the unexposed (Did not eat)", labs,
                        fixed = TRUE)))
})

test_that("risk measures use the same phrasing", {
  d <- risk_ratio(epi2x2(54, 16, 8, 42,
                         exposure = c("Ate potato salad", "Did not eat"),
                         outcome  = c("Ill", "Well")))
  labs <- steps_table(d)$label
  expect_true(any(grepl("Risk of Ill among the exposed (Ate potato salad)",
                        labs, fixed = TRUE)))
})
