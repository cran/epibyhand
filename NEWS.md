# epibyhand 0.3.0

## New features

* Derivations now render as typeset mathematics. In a Quarto or R Markdown
  document, a derivation that is the value of a code chunk is displayed with
  fractions, square roots and subscripts instead of console text, and its
  tables become real tables. This works for HTML, PDF and Word output and
  needs no change to existing code. Set `options(epibyhand.knit = "console")`
  to keep the old console style in documents.

* `to_latex()` returns a derivation as a LaTeX fragment for problem sets,
  exams and lecture notes written in LaTeX directly. It needs only
  `\usepackage{amsmath}`. `standalone = TRUE` gives a complete document.

* `to_markdown()` returns the Markdown that documents display, for use outside
  knitr.

* `derivation_step()` gains a `latex` argument, so measures written with
  `derivation()` and `derivation_step()` can typeset their formulas too.

## Internal

* knitr remains in Suggests. The `knit_print()` method is registered lazily and
  exists only when knitr is loaded; `Imports` is still `stats` alone.

* The LaTeX for every built-in formula is written by hand and keyed by the
  exact plain-text formula, so the console and typeset versions cannot drift
  apart silently: editing a formula without updating its LaTeX fails the test
  suite.

# epibyhand 0.2.0

## New features

* `predictive_value()` computes the positive or negative predictive value of a
  diagnostic test, showing sensitivity, specificity and prevalence as separate
  steps. Supplying `prevalence` applies the test to a population other than the
  one in the table, via Bayes' theorem, which is how a test validated in a
  clinic is assessed as a population screen.

## Bug fixes

* `confint()` no longer silently ignores its `level` argument. Intervals are
  computed when the measure is computed, because the derivation displays the
  arithmetic that produced them, so `level` still cannot change the result --
  but it now warns and names the fix (`conf_level` on the measure) instead of
  returning an interval at a different level than the one asked for.

* Step labels no longer paste a custom exposure label directly after "the",
  which produced text such as "Odds of Ill among the ate potato salad". Labels
  now name the epidemiological role and give the user's label in parentheses:
  "Odds of Ill among the exposed (Ate potato salad)". Output is unchanged when
  the default `exposure` labels are used.

# epibyhand 0.1.0

First release.

* `epi2x2()` builds a 2 x 2 table in standard epidemiological orientation.
* `risk_ratio()`, `odds_ratio()` and `risk_difference()` return the measure
  together with the full worked derivation.
* `attributable_fraction()` computes the fraction among the exposed or in the
  population, showing that the direct, Levin and Miettinen formulas agree.
* `epi_strata()` and `collapse_strata()` handle stratified data.
* `mh_odds_ratio()` and `mh_risk_ratio()` pool across strata, printing the
  stratum weights and the crude estimate alongside the adjusted one.
* `homogeneity()` implements the Breslow-Day test with Tarone's correction.
* `check_work()` compares a hand calculation against every step of a
  derivation and reports where it diverged.
* `steps_table()` returns a derivation as a data frame.
* Output detail is controlled with `options(epibyhand.verbose = )` and
  `options(epibyhand.digits = )`.
