scr <- epi2x2(90, 180, 10, 1720,
              exposure = c("Test positive", "Test negative"),
              outcome  = c("Diseased", "Healthy"))

test_that("PPV matches the direct proportion when prevalence is not supplied", {
  d <- predictive_value(scr)
  expect_equal(estimate(d), 90 / (90 + 180))
})

test_that("NPV matches the direct proportion", {
  d <- predictive_value(scr, which = "negative")
  expect_equal(estimate(d), 1720 / (10 + 1720))
})

test_that("Bayes and the direct proportion agree at the observed prevalence", {
  d <- predictive_value(scr)
  bayes <- steps_table(d)$result[steps_table(d)$symbol == "PPV"]
  expect_equal(bayes, 90 / (90 + 180), tolerance = 1e-12)
})

test_that("sensitivity and specificity are recovered", {
  d <- predictive_value(scr)
  expect_equal(d$sensitivity, 90 / (90 + 10))
  expect_equal(d$specificity, 1720 / (180 + 1720))
  expect_equal(d$prevalence, 100 / 2000)
})

test_that("supplying the observed prevalence reproduces the observed answer", {
  observed <- 100 / 2000
  expect_equal(estimate(predictive_value(scr, prevalence = observed)),
               estimate(predictive_value(scr)), tolerance = 1e-12)
})

test_that("PPV rises with prevalence and NPV falls", {
  ppv <- vapply(c(0.01, 0.05, 0.20, 0.50),
                function(p) estimate(predictive_value(scr, prevalence = p)),
                numeric(1))
  npv <- vapply(c(0.01, 0.05, 0.20, 0.50),
                function(p) estimate(predictive_value(scr, which = "negative",
                                                      prevalence = p)),
                numeric(1))
  expect_true(all(diff(ppv) > 0))
  expect_true(all(diff(npv) < 0))
})

test_that("a supplied prevalence suppresses the confidence interval", {
  expect_null(predictive_value(scr, prevalence = 0.4)$ci)
  expect_false(is.null(predictive_value(scr)$ci))
})

test_that("prevalence is validated", {
  expect_error(predictive_value(scr, prevalence = 1.5), "between 0 and 1")
  expect_error(predictive_value(scr, prevalence = -0.1), "between 0 and 1")
  expect_error(predictive_value(scr, prevalence = "high"), "between 0 and 1")
})

test_that("which is validated", {
  expect_error(predictive_value(scr, which = "sideways"))
})

test_that("the interval brackets the estimate and stays inside [0, 1]", {
  d <- predictive_value(scr)
  expect_lt(confint(d)[1], estimate(d))
  expect_gt(confint(d)[2], estimate(d))
  expect_gte(confint(d)[1], 0)
  expect_lte(confint(d)[2], 1)
})

test_that("a perfect test gives predictive values of 1", {
  perfect <- epi2x2(50, 0, 0, 50)
  expect_equal(estimate(predictive_value(perfect)), 1)
  expect_equal(estimate(predictive_value(perfect, which = "negative")), 1)
})

test_that("check_work reaches into a screening derivation", {
  d <- predictive_value(scr)
  expect_true(check_work(d, 0.90, step = "Sens"))
  expect_true(check_work(d, 0.3333))
})
