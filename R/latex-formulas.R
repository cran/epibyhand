## LaTeX for every built-in formula -------------------------------------------
##
## Each built-in step carries its formula as plain text, which is what the
## console prints. The renderers need the same formula as mathematics. This
## file holds that second representation, written by hand, keyed by the exact
## plain-text string.
##
## Keying on the plain text is deliberate. If someone edits a formula in a
## measure function and forgets this file, the key no longer matches, and
## tests/testthat/test-latex.R fails. The two representations cannot drift
## apart silently.
##
## Values are the right-hand side only. The step's symbol is typeset
## separately by latex_symbol().

#' @keywords internal
#' @noRd
latex_formulas <- c(
  # ---- risks, odds, 2 x 2 measures ---------------------------------------
  "a / (a + b)"  = "\\frac{a}{a + b}",
  "c / (c + d)"  = "\\frac{c}{c + d}",
  "a / b"        = "\\frac{a}{b}",
  "c / d"        = "\\frac{c}{d}",
  "R1 / R0"      = "\\frac{R_1}{R_0}",
  "R1 - R0"      = "R_1 - R_0",
  "odds1 / odds0" = "\\frac{\\text{odds}_1}{\\text{odds}_0}",
  "(a + c) / n"  = "\\frac{a + c}{n}",

  # ---- standard errors and limits -----------------------------------------
  "sqrt(1/a + 1/b + 1/c + 1/d)" =
    "\\sqrt{\\frac{1}{a} + \\frac{1}{b} + \\frac{1}{c} + \\frac{1}{d}}",
  "sqrt(1/a - 1/(a+b) + 1/c - 1/(c+d))" =
    "\\sqrt{\\frac{1}{a} - \\frac{1}{a+b} + \\frac{1}{c} - \\frac{1}{c+d}}",
  "sqrt(R1(1-R1)/(a+b) + R0(1-R0)/(c+d))" =
    "\\sqrt{\\frac{R_1(1-R_1)}{a+b} + \\frac{R_0(1-R_0)}{c+d}}",
  "exp(log(OR) - z * SE)"    = "\\exp\\left(\\log OR - z \\cdot SE\\right)",
  "exp(log(OR) + z * SE)"    = "\\exp\\left(\\log OR + z \\cdot SE\\right)",
  "exp(log(RR) - z * SE)"    = "\\exp\\left(\\log RR - z \\cdot SE\\right)",
  "exp(log(RR) + z * SE)"    = "\\exp\\left(\\log RR + z \\cdot SE\\right)",
  "RD - z * SE"              = "RD - z \\cdot SE",
  "RD + z * SE"              = "RD + z \\cdot SE",

  # ---- attributable fractions ---------------------------------------------
  "(R1 - R0) / R1" = "\\frac{R_1 - R_0}{R_1}",
  "(RR - 1) / RR"  = "\\frac{RR - 1}{RR}",
  "(Rt - R0) / Rt" = "\\frac{R_t - R_0}{R_t}",
  "p(RR - 1) / (1 + p(RR - 1))" = "\\frac{p(RR - 1)}{1 + p(RR - 1)}",
  "pc * (RR - 1) / RR"          = "p_c \\cdot \\frac{RR - 1}{RR}",
  "1 - 1/RR  applied to each limit of the RR interval" =
    "1 - \\frac{1}{RR} \\quad \\text{applied to each limit of the } RR \\text{ interval}",
  "Levin's formula applied to each limit of the RR interval" =
    "\\text{Levin's formula applied to each limit of the } RR \\text{ interval}",

  # ---- Mantel-Haenszel odds ratio -----------------------------------------
  "(a_i * d_i) / (b_i * c_i)" = "\\frac{a_i d_i}{b_i c_i}",
  "R_i = a_i*d_i/n_i     S_i = b_i*c_i/n_i" =
    "R_i = \\frac{a_i d_i}{n_i} \\qquad S_i = \\frac{b_i c_i}{n_i}",
  "sum(R_i) / sum(S_i)" = "\\frac{\\sum_i R_i}{\\sum_i S_i}",
  "sqrt( sum(P_i R_i)/(2R^2) + sum(P_i S_i + Q_i R_i)/(2RS) + sum(Q_i S_i)/(2S^2) )" =
    paste0("\\sqrt{\\frac{\\sum_i P_i R_i}{2R^2} + ",
           "\\frac{\\sum_i (P_i S_i + Q_i R_i)}{2RS} + ",
           "\\frac{\\sum_i Q_i S_i}{2S^2}}"),
  "exp(log(OR_MH) - z * SE)" = "\\exp\\left(\\log OR_{MH} - z \\cdot SE\\right)",
  "exp(log(OR_MH) + z * SE)" = "\\exp\\left(\\log OR_{MH} + z \\cdot SE\\right)",
  "(A * D) / (B * C)  on the collapsed table" =
    "\\frac{A D}{B C} \\quad \\text{on the collapsed table}",

  # ---- Mantel-Haenszel risk ratio -----------------------------------------
  "(a_i/(a_i+b_i)) / (c_i/(c_i+d_i))" =
    "\\frac{a_i / (a_i + b_i)}{c_i / (c_i + d_i)}",
  "num_i = a_i*n0_i/n_i     den_i = c_i*n1_i/n_i" =
    paste0("\\text{num}_i = \\frac{a_i n_{0i}}{n_i} \\qquad ",
           "\\text{den}_i = \\frac{c_i n_{1i}}{n_i}"),
  "sum(num_i) / sum(den_i)" =
    "\\frac{\\sum_i \\text{num}_i}{\\sum_i \\text{den}_i}",
  "sqrt( sum((n1_i*n0_i*(a_i+c_i) - a_i*c_i*n_i)/n_i^2) / (sum(num_i)*sum(den_i)) )" =
    paste0("\\sqrt{\\frac{\\sum_i \\left(n_{1i} n_{0i} (a_i + c_i) - a_i c_i n_i\\right) / n_i^2}",
           "{\\sum_i \\text{num}_i \\cdot \\sum_i \\text{den}_i}}"),
  "exp(log(RR_MH) - z * SE)" = "\\exp\\left(\\log RR_{MH} - z \\cdot SE\\right)",
  "exp(log(RR_MH) + z * SE)" = "\\exp\\left(\\log RR_{MH} + z \\cdot SE\\right)",
  "(A/(A+B)) / (C/(C+D))  on the collapsed table" =
    "\\frac{A / (A + B)}{C / (C + D)} \\quad \\text{on the collapsed table}",

  # ---- Breslow-Day homogeneity --------------------------------------------
  "root of  (1-psi)A^2 + (N - n1 - m1 + psi(n1+m1))A - psi*n1*m1 = 0" =
    paste0("\\text{root of } (1-\\psi)A^2 + ",
           "\\left(N - n_1 - m_1 + \\psi(n_1 + m_1)\\right)A - \\psi n_1 m_1 = 0"),
  "(a_i - A_i)^2 / Var(A_i)" = "\\frac{(a_i - A_i)^2}{\\operatorname{Var}(A_i)}",
  "sum(X2_i)" = "\\sum_i X^2_i",
  "sum(X2_i) - (sum(a_i) - sum(A_i))^2 / sum(Var(A_i))" =
    paste0("\\sum_i X^2_i - \\frac{\\left(\\sum_i a_i - \\sum_i A_i\\right)^2}",
           "{\\sum_i \\operatorname{Var}(A_i)}"),
  "P(chi-squared with K - 1 df > X2)" =
    "P\\left(\\chi^2_{K-1} > X^2\\right)",

  # ---- screening -----------------------------------------------------------
  "TP / (TP + FN)" = "\\frac{TP}{TP + FN}",
  "TN / (FP + TN)" = "\\frac{TN}{FP + TN}",
  "(TP + FN) / n"  = "\\frac{TP + FN}{n}",
  "supplied"       = "\\text{supplied}",
  "Sens*Prev / (Sens*Prev + (1-Spec)*(1-Prev))" =
    paste0("\\frac{\\text{Sens} \\cdot \\text{Prev}}",
           "{\\text{Sens} \\cdot \\text{Prev} + (1 - \\text{Spec})(1 - \\text{Prev})}"),
  "Spec*(1-Prev) / (Spec*(1-Prev) + (1-Sens)*Prev)" =
    paste0("\\frac{\\text{Spec}(1 - \\text{Prev})}",
           "{\\text{Spec}(1 - \\text{Prev}) + (1 - \\text{Sens}) \\cdot \\text{Prev}}"),
  "sqrt(p(1 - p) / n)" = "\\sqrt{\\frac{p(1 - p)}{n}}",
  "p +/- z * SE"       = "p \\pm z \\cdot SE"
)

#' @keywords internal
#' @noRd
latex_symbols <- c(
  R1 = "R_1", R0 = "R_0", Rt = "R_t",
  odds1 = "\\text{odds}_1", odds0 = "\\text{odds}_0",
  AFe = "AF_e", PAF = "PAF",
  OR_i = "OR_i", OR_MH = "OR_{MH}", OR_crude = "OR_{\\text{crude}}",
  RR_i = "RR_i", RR_MH = "RR_{MH}", RR_crude = "RR_{\\text{crude}}",
  A_i = "A_i", X2_i = "X^2_i", X2 = "X^2",
  lower = "\\text{lower}", upper = "\\text{upper}", CI = "\\text{CI}",
  Sens = "\\text{Sens}", Spec = "\\text{Spec}", Prev = "\\text{Prev}",
  PPV = "\\text{PPV}", NPV = "\\text{NPV}"
)
