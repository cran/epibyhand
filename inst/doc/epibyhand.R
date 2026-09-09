## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(epibyhand)

## -----------------------------------------------------------------------------
crude <- epi2x2(139, 443, 230, 502,
                exposure = c("Smoker", "Non-smoker"),
                outcome  = c("Dead", "Alive"))
crude

## -----------------------------------------------------------------------------
odds_ratio(crude)

## -----------------------------------------------------------------------------
options(epibyhand.verbose = 1)
risk_ratio(crude)
risk_difference(crude)

## -----------------------------------------------------------------------------
smoking <- epi_strata(
  c(15, 270,  12, 327),
  c(80, 167,  53, 147),
  c(44,   6, 165,  28),
  labels   = c("18-44", "45-64", "65+"),
  exposure = c("Smoker", "Non-smoker"),
  outcome  = c("Dead", "Alive")
)
smoking

## -----------------------------------------------------------------------------
round(mh_odds_ratio(smoking)$stratum_estimates, 3)

## -----------------------------------------------------------------------------
options(epibyhand.verbose = 2)
mh_odds_ratio(smoking)

## -----------------------------------------------------------------------------
options(epibyhand.verbose = 1)
homogeneity(smoking)

## -----------------------------------------------------------------------------
middle <- epi2x2(80, 167, 53, 147,
                 exposure = c("Smoker", "Non-smoker"),
                 outcome  = c("Dead", "Alive"))

options(epibyhand.verbose = 2)
attributable_fraction(middle, among = "population")

## -----------------------------------------------------------------------------
options(epibyhand.verbose = 0)
d <- odds_ratio(crude)

check_work(d, 0.3137)

## -----------------------------------------------------------------------------
steps_table(mh_odds_ratio(smoking))[, c("symbol", "label", "result")]

## ----include = FALSE----------------------------------------------------------
options(epibyhand.verbose = 2, epibyhand.digits = 4)

## -----------------------------------------------------------------------------
options(epibyhand.verbose = 1)

screening <- epi2x2(90, 180, 10, 1720,
                    exposure = c("Test positive", "Test negative"),
                    outcome  = c("Diseased", "Healthy"))

predictive_value(screening)

## -----------------------------------------------------------------------------
options(epibyhand.verbose = 0)
vapply(c(0.01, 0.05, 0.20, 0.50),
       function(p) estimate(predictive_value(screening, prevalence = p)),
       numeric(1))

## ----include = FALSE----------------------------------------------------------
options(epibyhand.verbose = 2)

