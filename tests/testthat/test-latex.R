## Every built-in derivation, across every branch the measures can take.
## If a new measure or branch is added, add it here: the drift tests below
## are only as complete as this list.
all_derivations <- function() {
  t <- epi2x2(54, 16, 8, 42)
  z <- epi2x2(10, 0, 5, 20)
  s <- epi_strata(c(15, 270, 12, 327), c(80, 167, 53, 147), c(44, 6, 165, 28))
  scr <- epi2x2(90, 180, 10, 1720)
  list(
    risk_ratio(t), odds_ratio(t), risk_difference(t),
    risk_ratio(z), odds_ratio(z), risk_difference(z),
    attributable_fraction(t), attributable_fraction(t, among = "population"),
    mh_odds_ratio(s), mh_risk_ratio(s),
    homogeneity(s), homogeneity(s, tarone = FALSE),
    predictive_value(scr), predictive_value(scr, which = "negative"),
    predictive_value(scr, prevalence = 0.2),
    predictive_value(scr, which = "negative", prevalence = 0.2)
  )
}

all_steps <- function() unlist(lapply(all_derivations(), `[[`, "steps"),
                               recursive = FALSE)

## ---- drift: the plain-text and LaTeX formulas must stay paired ------------

test_that("every built-in formula has a hand-written LaTeX form", {
  formulas <- unique(vapply(all_steps(), `[[`, "", "formula"))
  missing <- setdiff(formulas, names(epibyhand:::latex_formulas))
  expect_equal(missing, character(0),
               info = paste(
                 "A formula was added or edited without updating",
                 "R/latex-formulas.R. Add or update the entry keyed by",
                 "the exact plain-text formula."))
})

test_that("no LaTeX entry is left over from a formula that no longer exists", {
  formulas <- unique(vapply(all_steps(), `[[`, "", "formula"))
  stale <- setdiff(names(epibyhand:::latex_formulas), formulas)
  expect_equal(stale, character(0),
               info = paste(
                 "R/latex-formulas.R has an entry no built-in step emits.",
                 "Either a formula was edited (update the key) or the",
                 "branch that emits it is missing from all_derivations()."))
})

test_that("every built-in symbol typesets as mathematics, not escaped text", {
  syms <- unique(unlist(lapply(all_steps(), `[[`, "symbol")))
  bad <- syms[grepl("\\\\text\\{.*\\\\_", vapply(syms, epibyhand:::latex_symbol, ""))]
  expect_equal(bad, character(0))
})

test_that("LaTeX braces balance in every built-in formula and derivation", {
  balanced <- function(x) {
    x <- gsub("\\\\[{}]", "", x)
    chars <- strsplit(x, "")[[1L]]
    depth <- cumsum((chars == "{") - (chars == "}"))
    all(depth >= 0) && depth[length(depth)] == 0
  }
  for (f in epibyhand:::latex_formulas) expect_true(balanced(f), info = f)
  for (d in all_derivations()) expect_true(balanced(to_latex(d)), info = d$method)
})

## ---- the renderers --------------------------------------------------------

test_that("to_latex compiles for every built-in derivation", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pdflatex")), "pdflatex not available")
  dir <- tempfile("epibyhand-tex")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  body <- paste(vapply(all_derivations(), to_latex, ""), collapse = "\n\\clearpage\n")
  tex <- file.path(dir, "all.tex")
  writeLines(c("\\documentclass{article}", "\\usepackage{amsmath}",
               "\\begin{document}", body, "\\end{document}"), tex)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  status <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error",
                                  "all.tex"), stdout = FALSE, stderr = FALSE)
  expect_equal(status, 0L)
})

test_that("standalone LaTeX is a complete document", {
  out <- to_latex(odds_ratio(epi2x2(36, 14, 30, 25)), standalone = TRUE)
  expect_match(out, "^\\\\documentclass")
  expect_match(out, "\\\\usepackage\\{amsmath\\}")
  expect_match(out, "\\\\end\\{document\\}$")
  expect_false(grepl("documentclass", to_latex(odds_ratio(epi2x2(36, 14, 30, 25)))))
})

test_that("Markdown carries the formulas as display mathematics", {
  md <- to_markdown(odds_ratio(epi2x2(36, 14, 30, 25)))
  expect_match(md, "\\\\frac\\{a\\}\\{b\\}")
  expect_match(md, "$$", fixed = TRUE)
  expect_match(md, "| Exposed |", fixed = TRUE)
})

test_that("verbosity is honoured in both renderers", {
  d <- odds_ratio(epi2x2(36, 14, 30, 25))
  for (f in list(to_markdown, to_latex)) {
    v0 <- f(d, verbose = 0)
    v1 <- f(d, verbose = 1)
    v2 <- f(d, verbose = 2)
    expect_false(grepl("frac", v0))       # result only
    expect_true(grepl("frac", v1))        # formulas appear
    expect_false(grepl("36 / 14", v1))    # but not the numbers
    expect_true(grepl("36 / 14", v2))     # full working
    expect_lt(nchar(v0), nchar(v1))
    expect_lt(nchar(v1), nchar(v2))
  }
})

test_that("the numbers in the rendered output match the console", {
  d <- mh_odds_ratio(epi_strata(c(15, 270, 12, 327), c(80, 167, 53, 147)))
  md <- to_markdown(d)
  for (s in d$steps) {
    if (!is.null(s$result) && !is.na(s$result)) {
      expect_true(grepl(epibyhand:::fmt(s$result), md, fixed = TRUE),
                  info = s$label)
    }
  }
})

test_that("a step without a symbol has no stray equals sign", {
  d <- mh_odds_ratio(epi_strata(c(15, 270, 12, 327), c(80, 167, 53, 147)))
  expect_false(grepl("\\\\begin\\{aligned\\}\n&=", to_latex(d)))
})

## ---- substituted lines ----------------------------------------------------

test_that("substituted arithmetic converts operators and function names", {
  f <- epibyhand:::latex_inline
  expect_equal(f("exp(2.8746 - 1.96 * 0.4794)"),
               "\\exp(2.8746 - 1.96 \\times 0.4794)")
  expect_equal(f("0.3333 +/- 1.96 * 0.0287"), "0.3333 \\pm 1.96 \\times 0.0287")
  expect_match(f("sqrt(1/54 + 1/16)"), "^\\\\operatorname\\{sqrt\\}")
  expect_false(grepl("text\\{sqrt", f("sqrt(1/54 + 1/16)")))
})

test_that("named quantities in substituted lines become symbols", {
  expect_equal(epibyhand:::latex_inline("psi = OR_MH = 1.3499"),
               "\\psi = OR_{MH} = 1.3499")
})

test_that("prose in substituted lines stays together as upright text", {
  f <- epibyhand:::latex_inline
  expect_equal(f("K = 3, so df = 2"), "K = 3, \\text{ so df } = 2")
  expect_equal(f("given as 0.2; the table itself has 0.05"),
               "\\text{ given as } 0.2; \\text{ the table itself has } 0.05")
})

## ---- user-defined measures -------------------------------------------------

test_that("a user-supplied latex argument is used", {
  d <- derivation(
    method = "Custom", estimate = 2, symbol = "LRp",
    steps = list(derivation_step("Ratio", "x / y", "4 / 2", 2,
                                 symbol = "LRp", latex = "\\frac{x}{y}")))
  expect_match(to_latex(d), "\\frac{x}{y}", fixed = TRUE)
})

test_that("a user formula without latex falls back to upright text", {
  d <- derivation(
    method = "Custom", estimate = 2,
    steps = list(derivation_step("Ratio", "x_1 / y", "4 / 2", 2)))
  expect_match(to_latex(d), "\\text{x\\_1 / y}", fixed = TRUE)
})

test_that("derivation_step validates latex", {
  expect_error(derivation_step("a", "b", "c", 1, latex = 3))
  expect_error(derivation_step("a", "b", "c", 1, latex = c("x", "y")))
})

## ---- knitr -----------------------------------------------------------------

test_that("knit_print renders Markdown by default", {
  skip_if_not_installed("knitr")
  out <- knitr::knit_print(odds_ratio(epi2x2(36, 14, 30, 25)))
  expect_s3_class(out, "knit_asis")
  expect_match(out, "\\frac", fixed = TRUE)
})

test_that("knit_print can be switched to console style", {
  skip_if_not_installed("knitr")
  old <- options(epibyhand.knit = "console")
  on.exit(options(old), add = TRUE)
  out <- knitr::knit_print(odds_ratio(epi2x2(36, 14, 30, 25)))
  expect_match(out, "^```")
  expect_false(grepl("\\frac", out, fixed = TRUE))
})

test_that("console printing is unchanged by the renderers", {
  out <- capture.output(print(odds_ratio(epi2x2(36, 14, 30, 25))))
  expect_true(any(grepl("odds1 = a / b", out, fixed = TRUE)))
  expect_false(any(grepl("frac", out, fixed = TRUE)))
})
