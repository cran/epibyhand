## Renderers ------------------------------------------------------------------
##
## The console print method is one view of a derivation. These are two more:
## Markdown with embedded mathematics (what Quarto and R Markdown display) and
## standalone LaTeX (for problem sets and exams written in LaTeX directly).
##
## None of this runs unless asked for. The package never invokes LaTeX; it
## only builds strings. knitr is not imported: knit_print is registered lazily
## through the NAMESPACE, so it exists only when knitr is loaded.

## Escaping and conversion helpers ------------------------------------------

#' @keywords internal
#' @noRd
latex_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x
}

#' Typeset a symbol such as R1, OR_MH or a user-supplied one
#' @keywords internal
#' @noRd
latex_symbol <- function(sym) {
  if (is.null(sym) || !nzchar(sym)) return("")
  if (sym %in% names(latex_symbols)) return(latex_symbols[[sym]])
  if (grepl("^[A-Za-z]+$", sym)) {
    return(if (nchar(sym) <= 3L) sym else paste0("\\text{", sym, "}"))
  }
  m <- regmatches(sym, regexec("^([A-Za-z]+)_([A-Za-z0-9]+)$", sym))[[1L]]
  if (length(m) == 3L) return(paste0(m[2L], "_{", m[3L], "}"))
  paste0("\\text{", latex_escape(sym), "}")
}

#' Typeset a number
#' @keywords internal
#' @noRd
latex_number <- function(x, digits) {
  vapply(x, function(v) {
    if (is.na(v)) return("\\text{NA}")
    if (is.infinite(v)) return(if (v > 0) "\\infty" else "-\\infty")
    fmt(v, digits)
  }, character(1L))
}

#' Typeset the substituted line of a step
#'
#' These strings are built at run time and are almost entirely numbers and
#' operators. A handful of token substitutions is enough; this is not a
#' parser, and words it does not recognise are set as upright text.
#' @keywords internal
#' @noRd
latex_inline <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  x <- gsub("([%#&])", "\\\\\\1", x)

  # Named quantities (OR_MH, psi, ...) are swapped for placeholders first,
  # so the prose pass below cannot mistake them for words. Placeholders
  # contain no letters.
  known <- c(latex_symbols, psi = "\\psi")
  known <- known[order(-nchar(names(known)))]
  held <- character()
  for (nm in names(known)) {
    pat <- paste0("(?<![A-Za-z0-9_])", nm, "(?![A-Za-z0-9_])")
    if (grepl(pat, x, perl = TRUE)) {
      key <- paste0("\001", length(held), "\002")
      held[key] <- known[[nm]]
      x <- gsub(pat, key, x, perl = TRUE)
    }
  }

  # Runs of prose ("given as 0.2; the table itself has", "so df") are set as
  # upright text, a whole run at a time, so short words stay with their
  # neighbours and the spaces between them survive math mode. A run is prose
  # if it holds two or more words, or one word of three or more letters
  # that is not a function name. Function names are converted afterwards;
  # the other order wraps an already converted name a second time.
  runs <- gregexpr("[A-Za-z][A-Za-z ]*[A-Za-z]|[A-Za-z]", x, perl = TRUE)
  regmatches(x, runs) <- lapply(regmatches(x, runs), function(r) {
    vapply(r, function(w) {
      words <- strsplit(trimws(w), " +")[[1L]]
      fun <- words %in% c("sqrt", "exp", "log")
      prose <- (length(words) >= 2L && !all(fun)) ||
        any(nchar(words) >= 3L & !fun)
      if (prose) paste0("\\text{ ", w, " }") else w
    }, character(1L))
  })

  for (key in names(held)) x <- gsub(key, held[[key]], x, fixed = TRUE)
  x <- gsub("\\bsqrt\\b", "\\\\operatorname{sqrt}", x, perl = TRUE)
  x <- gsub("\\bexp\\b", "\\\\exp", x, perl = TRUE)
  x <- gsub("\\blog\\b", "\\\\log", x, perl = TRUE)
  x <- gsub("+/-", " \\pm ", x, fixed = TRUE)
  x <- gsub("*", " \\times ", x, fixed = TRUE)
  gsub("\\s+", " ", trimws(x))
}

#' The LaTeX right-hand side for a step's symbolic formula
#' @keywords internal
#' @noRd
step_latex <- function(s) {
  if (!is.null(s$latex)) return(s$latex)
  hit <- latex_formulas[s$formula]
  if (!is.na(hit)) return(unname(hit))
  paste0("\\text{", latex_escape(s$formula), "}")
}

## Shared pieces ------------------------------------------------------------

#' One step as a LaTeX aligned block (without the surrounding delimiters)
#' @keywords internal
#' @noRd
step_aligned <- function(s, verbose, digits) {
  lhs <- latex_symbol(s$symbol)
  # A step with no symbol (such as the two Mantel-Haenszel contribution
  # steps, whose formula is itself a pair of definitions) has nothing to
  # equate, so its first line carries no "=".
  lines <- if (nzchar(lhs)) {
    paste0(lhs, " &= ", step_latex(s))
  } else {
    paste0("& ", step_latex(s))
  }
  if (verbose >= 2L && !is.null(s$substituted) && nzchar(s$substituted)) {
    lines <- c(lines, paste0("&= ", latex_inline(s$substituted)))
  }
  if (!is.null(s$result) && !all(is.na(s$result))) {
    lines <- c(lines, paste0("&= ", latex_number(s$result, digits)))
  }
  paste0("\\begin{aligned}\n", paste(lines, collapse = " \\\\\n"),
         "\n\\end{aligned}")
}

#' Result line pieces: symbol, estimate, interval
#' @keywords internal
#' @noRd
result_latex <- function(x, digits) {
  out <- paste0(latex_symbol(x$symbol), " = ", latex_number(x$estimate, digits))
  ci <- NULL
  if (!is.null(x$ci)) {
    ci <- paste0(round(x$conf_level * 100), "% CI ",
                 latex_number(x$ci[1L], digits), " to ",
                 latex_number(x$ci[2L], digits))
  }
  list(math = out, ci = ci)
}

#' A data frame as a grid of strings, numbers formatted
#' @keywords internal
#' @noRd
format_grid <- function(df, digits) {
  out <- as.data.frame(lapply(df, function(col) {
    if (is.numeric(col)) fmt(col, digits) else as.character(col)
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- names(df)
  out
}

#' A 2 x 2 table with margins as a data frame
#' @keywords internal
#' @noRd
twobytwo_frame <- function(t) {
  df <- data.frame(
    c(t$exposure, "Total"),
    c(t$a, t$c, t$a + t$c),
    c(t$b, t$d, t$b + t$d),
    c(t$a + t$b, t$c + t$d, t$a + t$b + t$c + t$d),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  names(df) <- c(" ", t$outcome, "Total")
  df
}

#' Data tables carried by a derivation, as named data frames
#' @keywords internal
#' @noRd
data_frames <- function(data) {
  if (inherits(data, "epi2x2")) return(list(twobytwo_frame(data)))
  if (inherits(data, "epi_strata")) {
    out <- lapply(data$tables, twobytwo_frame)
    names(out) <- data$labels
    return(out)
  }
  list()
}

## Markdown -------------------------------------------------------------------

#' @keywords internal
#' @noRd
md_table <- function(df, digits) {
  g <- format_grid(df, digits)
  esc <- function(v) gsub("|", "\\|", v, fixed = TRUE)
  head <- paste0("| ", paste(esc(names(g)), collapse = " | "), " |")
  align <- paste0("|", paste(vapply(df, function(col)
    if (is.numeric(col)) "---:" else ":---", character(1L)), collapse = "|"), "|")
  body <- apply(g, 1L, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |"))
  paste(c(head, align, body), collapse = "\n")
}

#' Render a derivation as Markdown with embedded LaTeX mathematics
#'
#' Produces the same worked derivation the console shows, but as Markdown with
#' the formulas typeset as mathematics: fractions, square roots and subscripts
#' rather than slashes and underscores. Tables become Markdown tables. The
#' output renders in Quarto and R Markdown to HTML (through MathJax or KaTeX),
#' to PDF (through LaTeX), and to Word.
#'
#' You rarely need to call this directly. When a derivation is the result of
#' a code chunk in a Quarto or R Markdown document, it is rendered this way
#' automatically. Set `options(epibyhand.knit = "console")` to get the plain
#' console output in documents instead.
#'
#' @param x An `epibyhand_derivation`.
#' @param verbose Detail level, as for [print.epibyhand_derivation()].
#' @param digits Decimal places, as for [print.epibyhand_derivation()].
#'
#' @return A single character string of Markdown.
#'
#' @seealso [to_latex()] for standalone LaTeX.
#'
#' @examples
#' d <- odds_ratio(epi2x2(36, 14, 30, 25))
#' cat(to_markdown(d))
#' cat(to_markdown(d, verbose = 0))
#' @export
to_markdown <- function(x, verbose = NULL, digits = NULL) {
  stopifnot(inherits(x, "epibyhand_derivation"))
  verbose <- verbose %||% getOption("epibyhand.verbose", 2L)
  digits <- digits %||% getOption("epibyhand.digits", 4L)

  out <- paste0("**", x$method, "**")

  if (verbose >= 2L) {
    tabs <- data_frames(x$data)
    for (k in seq_along(tabs)) {
      if (!is.null(names(tabs)) && nzchar(names(tabs)[k])) {
        out <- c(out, paste0("*", names(tabs)[k], "*"))
      }
      out <- c(out, md_table(tabs[[k]], digits))
    }
  }

  if (verbose >= 1L) {
    for (i in seq_along(x$steps)) {
      s <- x$steps[[i]]
      out <- c(out, paste0("**Step ", i, ".** ", s$label))
      out <- c(out, paste0("$$\n", step_aligned(s, verbose, digits), "\n$$"))
      if (verbose >= 2L && !is.null(s$table)) {
        out <- c(out, md_table(s$table, digits))
      }
      if (!is.null(s$note)) out <- c(out, paste0("> ", s$note))
    }
  }

  r <- result_latex(x, digits)
  res <- paste0("**Result:** $", r$math, "$")
  if (!is.null(r$ci)) res <- paste0(res, " (", r$ci, ")")
  out <- c(out, res)

  if (length(x$notes)) {
    out <- c(out, paste0("- ", x$notes, collapse = "\n"))
  }

  paste(out, collapse = "\n\n")
}

## LaTeX ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
tex_table <- function(df, digits) {
  g <- format_grid(df, digits)
  spec <- paste(vapply(df, function(col) if (is.numeric(col)) "r" else "l",
                       character(1L)), collapse = "")
  row <- function(v) paste0(paste(latex_escape(v), collapse = " & "), " \\\\")
  c(paste0("\\begin{tabular}{", spec, "}"), "\\hline",
    row(names(g)), "\\hline",
    apply(g, 1L, row),
    "\\hline", "\\end{tabular}")
}

#' Render a derivation as LaTeX
#'
#' Produces a LaTeX fragment of the worked derivation, for dropping into a
#' problem set, an exam, or lecture notes written in LaTeX. Each step is an
#' aligned display with the formula in symbols, the same formula with the
#' observed numbers, and the result. Tables use `tabular`.
#'
#' The fragment needs `\usepackage{amsmath}` in the preamble. Nothing else.
#'
#' Set `standalone = TRUE` for a complete document that compiles on its own,
#' which is useful for checking the output or for a one-page answer sheet.
#'
#' @inheritParams to_markdown
#' @param standalone If `TRUE`, wrap the fragment in a minimal complete
#'   document.
#'
#' @return A single character string of LaTeX.
#'
#' @seealso [to_markdown()].
#'
#' @examples
#' d <- odds_ratio(epi2x2(36, 14, 30, 25))
#' cat(to_latex(d))
#'
#' # Write a compilable answer sheet
#' f <- tempfile(fileext = ".tex")
#' writeLines(to_latex(d, standalone = TRUE), f)
#' @export
to_latex <- function(x, verbose = NULL, digits = NULL, standalone = FALSE) {
  stopifnot(inherits(x, "epibyhand_derivation"))
  verbose <- verbose %||% getOption("epibyhand.verbose", 2L)
  digits <- digits %||% getOption("epibyhand.digits", 4L)

  out <- paste0("\\medskip\\noindent\\textbf{", latex_escape(x$method),
                "}\\par\\smallskip")

  if (verbose >= 2L) {
    tabs <- data_frames(x$data)
    for (k in seq_along(tabs)) {
      if (!is.null(names(tabs)) && nzchar(names(tabs)[k])) {
        out <- c(out, paste0("\\noindent\\emph{", latex_escape(names(tabs)[k]), "}\\par"))
      }
      out <- c(out, "\\begin{center}", tex_table(tabs[[k]], digits),
               "\\end{center}")
    }
  }

  if (verbose >= 1L) {
    for (i in seq_along(x$steps)) {
      s <- x$steps[[i]]
      out <- c(out, paste0("\\noindent\\textbf{Step ", i, ".} ",
                           latex_escape(s$label)))
      out <- c(out, "\\[", step_aligned(s, verbose, digits), "\\]")
      if (verbose >= 2L && !is.null(s$table)) {
        out <- c(out, "\\begin{center}", tex_table(s$table, digits),
                 "\\end{center}")
      }
      if (!is.null(s$note)) {
        out <- c(out, paste0("{\\small\\emph{", latex_escape(s$note), "}}\\par"))
      }
      out <- c(out, "\\medskip", "")
    }
  }

  r <- result_latex(x, digits)
  res <- paste0("\\noindent\\textbf{Result:} $", r$math, "$")
  if (!is.null(r$ci)) res <- paste0(res, " (", latex_escape(r$ci), ")")
  out <- c(out, paste0(res, "\\par"))

  if (length(x$notes)) {
    out <- c(out, "\\begin{itemize}",
             paste0("\\item ", latex_escape(x$notes)), "\\end{itemize}")
  }

  body <- paste(out, collapse = "\n")
  if (!standalone) return(body)
  paste(c("\\documentclass{article}", "\\usepackage{amsmath}",
          "\\usepackage[margin=1in]{geometry}", "\\begin{document}",
          body, "\\end{document}"), collapse = "\n")
}

## knitr ----------------------------------------------------------------------

#' Display a derivation in a Quarto or R Markdown document
#'
#' Called by knitr when a derivation is the value of a code chunk. Renders
#' the derivation with [to_markdown()], so formulas appear as typeset
#' mathematics. Set `options(epibyhand.knit = "console")` to show the plain
#' console output instead.
#'
#' @param x An `epibyhand_derivation`.
#' @param ... Unused.
#' @return A `knit_asis` object.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.epibyhand_derivation <- function(x, ...) {
  if (identical(getOption("epibyhand.knit", "markdown"), "console")) {
    lines <- character()
    con <- textConnection("lines", "w", local = TRUE)
    sink(con)
    on.exit({ sink(); close(con) }, add = TRUE)
    print(x)
    sink(); close(con); on.exit()
    return(knitr::asis_output(paste0(
      "```\n", paste(lines, collapse = "\n"), "\n```\n")))
  }
  knitr::asis_output(paste0("\n", to_markdown(x), "\n"))
}
