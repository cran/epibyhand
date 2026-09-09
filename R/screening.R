## Screening and diagnostic testing -----------------------------------------
##
## The teaching payload here is that a predictive value is not a property of
## the test. Sensitivity and specificity are; PPV and NPV depend on how common
## the disease is in the population you apply the test to.

#' Predictive value of a diagnostic test, with worked derivation
#'
#' Computes the positive or negative predictive value, showing sensitivity,
#' specificity and prevalence as separate steps so that the dependence on
#' prevalence is visible rather than asserted.
#'
#' The table is expected in screening orientation, which is the same shape as
#' every other table in this package once you read "exposed" as "test
#' positive" and "case" as "diseased":
#'
#' \tabular{lcc}{
#'                     \tab Diseased \tab Healthy \cr
#'   \strong{Test +}   \tab a (TP)   \tab b (FP)  \cr
#'   \strong{Test -}   \tab c (FN)   \tab d (TN)
#' }
#'
#' Supplying `prevalence` applies the test's sensitivity and specificity to a
#' population with that prevalence instead of the one in the table, via Bayes'
#' theorem. This is how you answer "what would this test do in a screening
#' programme?" using validation data from a high-prevalence clinical sample.
#'
#' @param x An `epi2x2`, or the first of four cell counts.
#' @param ... Further cell counts if `x` is a bare count.
#' @param which `"positive"` (the default) or `"negative"`.
#' @param prevalence Optional. Apply the test to a population with this
#'   prevalence rather than the one observed in the table. No confidence
#'   interval is produced in that case, because the result is not a proportion
#'   estimated from these data.
#' @param conf_level Confidence level for the interval.
#'
#' @return An `epibyhand_derivation`.
#'
#' @references
#' Altman, D. G. and Bland, J. M. (1994). Diagnostic tests 2: predictive
#' values. \emph{BMJ}, 309, 102.
#'
#' @examples
#' # A test with 90% sensitivity and 90% specificity, 5% prevalence
#' scr <- epi2x2(90, 180, 10, 1720,
#'               exposure = c("Test positive", "Test negative"),
#'               outcome  = c("Diseased", "Healthy"))
#' predictive_value(scr)
#' predictive_value(scr, which = "negative")
#'
#' # The same test in a population where 40% are diseased
#' predictive_value(scr, prevalence = 0.40)
#' @export
predictive_value <- function(x, ..., which = c("positive", "negative"),
                             prevalence = NULL, conf_level = 0.95) {
  which <- match.arg(which)
  x <- epi2x2(x, ...)
  digits <- getOption("epibyhand.digits", 4L)
  z <- zcrit(conf_level)

  TP <- x$a; FP <- x$b; FN <- x$c; TN <- x$d
  n <- TP + FP + FN + TN

  sens <- TP / (TP + FN)
  spec <- TN / (FP + TN)
  observed_prev <- (TP + FN) / n

  supplied <- !is.null(prevalence)
  if (supplied) {
    if (!is.numeric(prevalence) || length(prevalence) != 1L ||
        is.na(prevalence) || prevalence < 0 || prevalence > 1) {
      stop("`prevalence` must be a single number between 0 and 1.",
           call. = FALSE)
    }
  }
  prev <- if (supplied) prevalence else observed_prev

  common <- list(
    derivation_step(
      label = paste0("Sensitivity: of those who are ", x$outcome[1L],
                     ", the proportion the test finds"),
      symbol = "Sens", formula = "TP / (TP + FN)",
      substituted = paste0(fmt(TP, digits), " / (", fmt(TP, digits), " + ",
                           fmt(FN, digits), ")"),
      result = sens
    ),
    derivation_step(
      label = paste0("Specificity: of those who are ", x$outcome[2L],
                     ", the proportion the test clears"),
      symbol = "Spec", formula = "TN / (FP + TN)",
      substituted = paste0(fmt(TN, digits), " / (", fmt(FP, digits), " + ",
                           fmt(TN, digits), ")"),
      result = spec
    ),
    derivation_step(
      label = if (supplied) "Prevalence in the target population" else
        "Prevalence in this table",
      symbol = "Prev",
      formula = if (supplied) "supplied" else "(TP + FN) / n",
      substituted = if (supplied) {
        paste0("given as ", fmt(prevalence, digits), "; the table itself has ",
               fmt(observed_prev, digits))
      } else {
        paste0("(", fmt(TP, digits), " + ", fmt(FN, digits), ") / ",
               fmt(n, digits))
      },
      result = prev,
      note = "Sensitivity and specificity are properties of the test. Everything below also depends on this number."
    )
  )

  if (which == "positive") {
    num <- sens * prev
    den <- sens * prev + (1 - spec) * (1 - prev)
    pv_bayes <- num / den
    pv <- if (supplied) pv_bayes else TP / (TP + FP)

    steps <- c(common, list(
      derivation_step(
        label = "Positive predictive value, by Bayes' theorem",
        symbol = "PPV",
        formula = "Sens*Prev / (Sens*Prev + (1-Spec)*(1-Prev))",
        substituted = paste0(
          fmt(sens, digits), "*", fmt(prev, digits), " / (",
          fmt(sens, digits), "*", fmt(prev, digits), " + (1-",
          fmt(spec, digits), ")*(1-", fmt(prev, digits), "))"),
        result = pv_bayes,
        note = if (supplied) {
          "With a supplied prevalence this is the only route available: the table's own positives no longer describe the target population."
        } else {
          paste0("Equivalently TP / (TP + FP) = ", fmt(TP, digits), " / (",
                 fmt(TP, digits), " + ", fmt(FP, digits), ") = ",
                 fmt(TP / (TP + FP), digits),
                 ". The two agree because the prevalence used is the one in this table.")
        }
      )
    ))
    label <- "Positive predictive value"
    denom_n <- TP + FP
  } else {
    num <- spec * (1 - prev)
    den <- spec * (1 - prev) + (1 - sens) * prev
    pv_bayes <- num / den
    pv <- if (supplied) pv_bayes else TN / (FN + TN)

    steps <- c(common, list(
      derivation_step(
        label = "Negative predictive value, by Bayes' theorem",
        symbol = "NPV",
        formula = "Spec*(1-Prev) / (Spec*(1-Prev) + (1-Sens)*Prev)",
        substituted = paste0(
          fmt(spec, digits), "*(1-", fmt(prev, digits), ") / (",
          fmt(spec, digits), "*(1-", fmt(prev, digits), ") + (1-",
          fmt(sens, digits), ")*", fmt(prev, digits), ")"),
        result = pv_bayes,
        note = if (supplied) {
          "With a supplied prevalence this is the only route available: the table's own negatives no longer describe the target population."
        } else {
          paste0("Equivalently TN / (FN + TN) = ", fmt(TN, digits), " / (",
                 fmt(FN, digits), " + ", fmt(TN, digits), ") = ",
                 fmt(TN / (FN + TN), digits), ".")
        }
      )
    ))
    label <- "Negative predictive value"
    denom_n <- FN + TN
  }

  ci <- NULL
  if (!supplied && denom_n > 0 && !is.na(pv)) {
    se <- sqrt(pv * (1 - pv) / denom_n)
    lo <- max(0, pv - z * se)
    hi <- min(1, pv + z * se)
    ci <- c(lo, hi)
    steps <- c(steps, list(
      derivation_step(
        label = "Standard error of the proportion",
        symbol = "SE", formula = "sqrt(p(1 - p) / n)",
        substituted = paste0("sqrt(", fmt(pv, digits), " * (1 - ",
                             fmt(pv, digits), ") / ", fmt(denom_n, digits), ")"),
        result = se,
        note = "A predictive value is a proportion of those the test called, so n here is the number the test called, not the whole table."
      ),
      derivation_step(
        label = paste0(round(conf_level * 100), "% confidence interval"),
        symbol = "CI", formula = "p +/- z * SE",
        substituted = paste0(fmt(pv, digits), " +/- ", fmt(z, digits), " * ",
                             fmt(se, digits)),
        result = NA_real_,
        table = data.frame(limit = c("lower", "upper"), value = c(lo, hi),
                           stringsAsFactors = FALSE),
        note = "Normal approximation, clamped to [0, 1]. It is the version you can compute by hand; near 0 or 1 an exact or Wilson interval is better behaved."
      )
    ))
  }

  notes <- c(
    check_zero_cells(x),
    paste0(
      "Read as: of everyone this test calls ",
      if (which == "positive") "positive" else "negative", ", ",
      fmt(100 * pv, 1L), "% ",
      if (which == "positive") {
        paste0("really are ", tolower(x$outcome[1L]), ".")
      } else {
        paste0("really are ", tolower(x$outcome[2L]), ".")
      }
    ),
    paste(
      "Predictive values are not properties of the test. Move the same test to",
      "a population with different prevalence and this number changes, while",
      "sensitivity and specificity do not. That is why a test that performs",
      "well in a clinic can be useless as a population screen."
    )
  )
  if (supplied) {
    notes <- c(notes, paste0(
      "Computed at a supplied prevalence of ", fmt(prevalence, digits),
      ", not the ", fmt(observed_prev, digits), " observed in this table. ",
      "No confidence interval is given, because this is not a proportion ",
      "estimated from these data."))
  }

  out <- derivation(
    method = label,
    estimate = pv, symbol = if (which == "positive") "PPV" else "NPV",
    ci = ci, conf_level = conf_level,
    data = x, steps = steps, notes = notes
  )
  out$sensitivity <- sens
  out$specificity <- spec
  out$prevalence <- prev
  out
}
