# ---------------------------------------------------------------------------
# Module-level micro-utility (internal)
# ---------------------------------------------------------------------------

#' @noRd
row_line <- function(vals) paste0(paste(vals, collapse = " & "), " \\\\")

# ---------------------------------------------------------------------------
# Exported row-builder helpers
# ---------------------------------------------------------------------------

#' Build a LaTeX coefficient estimate row
#'
#' Produces one coefficient estimate row: label in column 1, formatted
#' estimates with math-mode stars in the remaining columns.
#'
#' @param label Character. Row label (LaTeX-escaped internally).
#' @param b Numeric vector. Estimates, one per column; \code{NA} produces an
#'   empty cell.
#' @param p Numeric vector. p-values for star assignment, same length as
#'   \code{b}; \code{NA} produces no stars.
#' @param digits Integer. Decimal places. Default \code{3L}.
#'
#' @return \code{character(1)} — \code{&}-delimited row ending in \code{ \\}.
#'
#' @examples
#' tex_coef_row("Age", b = c(0.123, NA), p = c(0.03, NA))
#'
#' @export
tex_coef_row <- function(label, b, p, digits = 3L) {
  cells <- c(
    latex_escape(label),
    vapply(seq_along(b), function(j) {
      if (is.na(b[[j]])) return("")
      star_str <- apply_stars(p[[j]], c(0.1, 0.05, 0.01))
      fmt <- paste0(format_estimate(b[[j]], digits), star_str)
      if (fmt == "") "" else latex_stars(fmt)
    }, character(1L))
  )
  row_line(cells)
}

#' Build a LaTeX SE / t-statistic / p-value second row
#'
#' Produces the row that appears under each coefficient: standard errors in
#' parentheses, or t-statistics / p-values in brackets.
#'
#' @param se Numeric vector. Values, one per column; \code{NA} produces an
#'   empty cell.
#' @param digits Integer. Decimal places. Default \code{3L}.
#' @param format Character. \code{"se"} (default) wraps in \code{( )};
#'   \code{"tstat"} or \code{"pvalue"} wraps in \code{[ ]}.
#'
#' @return \code{character(1)} with a leading empty label cell.
#'
#' @examples
#' tex_se_row(se = c(0.045, NA))
#' tex_se_row(se = c(2.73, NA), format = "tstat")
#'
#' @export
tex_se_row <- function(se, digits = 3L, format = "se") {
  format <- match.arg(format, c("se", "tstat", "pvalue"))
  cells <- vapply(seq_along(se), function(j) {
    if (is.na(se[[j]])) return("")
    switch(format,
      "se"     = format_se(se[[j]], digits),
      "tstat"  = ,
      "pvalue" = format_bracket(se[[j]], digits)
    )
  }, character(1L))
  row_line(c("", cells))
}

#' Build a LaTeX fit-statistic row
#'
#' Produces a row for fit statistics such as N, R\eqn{^2}, RMSE, etc.
#'
#' @param label Character. Row label, e.g. \code{"$N$"} or \code{"$R^{2}$"}.
#' @param values Numeric vector. One value per column; \code{NA} produces an
#'   empty cell.
#' @param integer Logical. \code{TRUE} formats values as comma-separated
#'   integers (suitable for N or cluster counts). Default \code{FALSE}.
#' @param digits Integer. Decimal places when \code{integer = FALSE}.
#'   Default \code{3L}.
#'
#' @return \code{character(1)}.
#'
#' @examples
#' tex_stat_row("$N$", values = c(100, 200), integer = TRUE)
#' tex_stat_row("$R^{2}$", values = c(0.452, 0.631))
#'
#' @export
tex_stat_row <- function(label, values, integer = FALSE, digits = 3L) {
  stat_key <- if (integer) "N" else "R2"
  cells <- vapply(seq_along(values), function(j) {
    format_stat(values[[j]], stat_key, digits)
  }, character(1L))
  row_line(c(label, cells))
}

#' Build a LaTeX panel header row
#'
#' Produces a full-width panel-header row spanning all columns, with the
#' label in bold.
#'
#' @param label Character. Panel label, e.g. \code{"Panel A: Wages"}.
#' @param ncols Integer. Total column count (label column + data columns).
#'
#' @return \code{character(1)} — a \code{\\multicolumn} row.
#'
#' @examples
#' tex_panel("Panel A: Main results", ncols = 4L)
#'
#' @export
tex_panel <- function(label, ncols) {
  paste0("\\multicolumn{", ncols, "}{l}{\\textbf{", label, "}} \\\\")
}

#' Return a booktabs horizontal rule
#'
#' @return \code{character(1)} — \code{"\\midrule"}.
#'
#' @examples
#' tex_hline()
#'
#' @export
tex_hline <- function() "\\midrule"

#' Build a LaTeX blank spacer row
#'
#' @param ncols Integer. Total column count (label column + data columns).
#'
#' @return \code{character(1)} — \code{ncols} empty cells joined by \code{ & },
#'   ending in \code{ \\}.
#'
#' @examples
#' tex_blank_row(3L)
#'
#' @export
tex_blank_row <- function(ncols) {
  row_line(rep("", as.integer(ncols)))
}

# ---------------------------------------------------------------------------
# Internal helpers (@noRd)
# ---------------------------------------------------------------------------

#' Build the col_groups spanning header block
#' @noRd
tex_col_groups_block <- function(col_groups, mn) {
  if (is.null(col_groups)) return(character(0L))
  n_cols <- length(mn) + 1L
  groups <- unique(col_groups)
  cells  <- character(n_cols)
  cells[[1L]] <- ""
  cmidrule_parts <- character(0L)
  for (grp in groups) {
    grp_models <- names(col_groups)[col_groups == grp]
    grp_cols   <- which(mn %in% grp_models) + 1L
    if (length(grp_cols) == 0L) next
    lo <- min(grp_cols); hi <- max(grp_cols)
    cells[[lo]] <- paste0("\\multicolumn{", length(grp_cols), "}{c}{", grp, "}")
    if (hi > lo) for (k in seq(lo + 1L, hi)) cells[[k]] <- ""
    cmidrule_parts <- c(cmidrule_parts,
                        paste0("\\cmidrule(lr){", lo, "-", hi, "}"))
  }
  c(row_line(cells), paste(cmidrule_parts, collapse = " "))
}

#' Build a single FE indicator row
#' @noRd
tex_fe_row <- function(fl, fe_data, mn) {
  vals <- vapply(mn, function(mod) {
    r <- fe_data[fe_data$fe_label == fl & fe_data$model == mod, ]
    if (nrow(r) == 0L) "" else if (r$included[[1L]]) "Yes" else "No"
  }, character(1L))
  row_line(c(latex_escape(fl), vals))
}

#' Build the Mode A significance note line
#' @noRd
tex_note_line <- function(n_cols, se_type, se_format, fe_labels,
                           cluster_labels, note) {
  stars_note  <- "*** $p<0.01$, ** $p<0.05$, * $p<0.1$"
  se_note_str <- build_se_note(se_type, se_format,
                               fe_labels      = fe_labels,
                               cluster_labels = cluster_labels,
                               latex          = TRUE)
  note_text <- paste0(stars_note, ". ", se_note_str)
  if (!is.null(note) && nchar(note) > 0L) {
    note_text <- paste0(note_text, ". ", note)
  }
  paste0("\\multicolumn{", n_cols, "}{l}{\\textit{Note:} ", note_text, "} \\\\")
}

# ---------------------------------------------------------------------------
# tex_table() — dual-mode LaTeX table assembler
# ---------------------------------------------------------------------------

#' Assemble a LaTeX table from a regtab_table or raw rows
#'
#' Dual-mode assembler:
#' \itemize{
#'   \item \strong{Mode A} — supply \code{x} (a \code{regtab_table}); renders
#'         the full formatted table using all metadata stored in \code{x}.
#'   \item \strong{Mode B} — supply \code{rows} (a character vector of
#'         pre-built row lines, e.g. from \code{\link{tex_coef_row}},
#'         \code{\link{tex_se_row}}, etc.); wraps them in a tabular
#'         environment without requiring a \code{regtab_table} object.
#' }
#'
#' @param x A \code{regtab_table} object, or \code{NULL} (Mode A).
#' @param rows Character vector of pre-built LaTeX row lines, or \code{NULL}
#'   (Mode B).
#' @param file Character or \code{NULL}. File path to write output.
#'   Default \code{NULL}.
#' @param format \code{"fragment"} (default) returns only the \code{tabular}
#'   environment. \code{"full"} wraps in a minimal \code{article} document
#'   with \code{\\usepackage{booktabs}}.
#' @param note Character or \code{NULL}. Footnote text. In Mode A, appended
#'   after the standard significance note. In Mode B, used as the full note
#'   text (no automatic SE-type sentence).
#' @param digits Integer or \code{NULL}. Override digits for this render call
#'   only (Mode A only). \code{NULL} uses \code{x$digits}.
#' @param ncols Integer or \code{NULL}. Total column count (label col + data
#'   cols). Required in Mode B; ignored in Mode A.
#'
#' @return Character vector of LaTeX lines, invisibly.
#'
#' @examples
#' \dontrun{
#' # Mode A
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' tab <- regtab(list("(1)" = m1))
#' tex_table(tab)
#' tex_table(tab, file = "table1.tex", format = "full")
#' tex_table(tab, digits = 2L)
#'
#' # Mode B — build a table from scratch without a regtab_table object
#' rows <- c(
#'   tex_coef_row("Intercept", b = 1.23, p = 0.001),
#'   tex_se_row(se = 0.12),
#'   tex_hline(),
#'   tex_stat_row("$N$", values = 100L, integer = TRUE)
#' )
#' tex_table(rows = rows, ncols = 2L)
#' }
#'
#' @export
tex_table <- function(x       = NULL,
                      rows    = NULL,
                      file    = NULL,
                      format  = "fragment",
                      note    = NULL,
                      digits  = NULL,
                      ncols   = NULL) {
  format <- match.arg(format, c("fragment", "full"))

  if (!is.null(rows)) {
    # ------------------------------------------------------------------
    # Mode B — raw rows path
    # ------------------------------------------------------------------
    if (is.null(ncols)) {
      stop("exportreg: `ncols` is required when supplying `rows`.", call. = FALSE)
    }
    ncols    <- as.integer(ncols)
    col_spec <- paste0("l", paste(rep("c", ncols - 1L), collapse = ""))
    note_line <- if (!is.null(note) && nchar(note) > 0L) {
      paste0("\\multicolumn{", ncols, "}{l}{\\textit{Note:} ", note, "} \\\\")
    } else {
      character(0L)
    }
    tabular <- c(
      paste0("\\begin{tabular}{", col_spec, "}"),
      "\\toprule",
      rows,
      "\\bottomrule",
      note_line,
      "\\end{tabular}"
    )

  } else if (inherits(x, "regtab_table")) {
    # ------------------------------------------------------------------
    # Mode A — regtab_table path
    # ------------------------------------------------------------------
    d <- if (!is.null(digits)) as.integer(digits) else x$digits
    digits_override <- !is.null(digits) && as.integer(digits) != x$digits
    mn       <- x$model_names
    n_models <- length(mn)
    n_cols   <- n_models + 1L
    col_spec <- paste0("l", paste(rep("c", n_models), collapse = ""))

    # col_groups spanning header
    group_lines <- tex_col_groups_block(x$col_groups, mn)

    # Column header row
    header_line <- row_line(c("", mn))

    # Dependent variable row
    depvar_cells <- c(
      "Dep.\\ var.",
      vapply(mn, function(mod) {
        latex_escape(x$depvar_names[[mod]] %||% "")
      }, character(1L))
    )
    depvar_line <- row_line(depvar_cells)

    # Coefficient rows
    cd <- x$coef_data
    term_order <- unique(cd[order(cd$row_order),
                             c("term_display", "row_order", "is_factor_header")])
    term_order <- term_order[!duplicated(term_order$term_display), ]

    se_fmt_choice <- if (!is.null(x$se_format)) x$se_format else "se"
    coef_lines <- character(0L)

    for (i in seq_len(nrow(term_order))) {
      tdisp  <- term_order$term_display[[i]]
      is_hdr <- term_order$is_factor_header[[i]]

      if (is_hdr) {
        coef_lines <- c(coef_lines, tex_panel(tdisp, n_cols))
      } else {
        # Estimate row
        if (digits_override) {
          b_vals <- vapply(mn, function(mod) {
            r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
            if (nrow(r) == 0L) NA_real_ else r$estimate[[1L]]
          }, numeric(1L))
          p_vals <- vapply(mn, function(mod) {
            r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
            if (nrow(r) == 0L) NA_real_ else r$p.value[[1L]]
          }, numeric(1L))
          coef_lines <- c(coef_lines,
                          tex_coef_row(tdisp, b = b_vals, p = p_vals, digits = d))
        } else {
          est_cells <- c(
            latex_escape(tdisp),
            vapply(mn, function(mod) {
              r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
              if (nrow(r) == 0L) return("")
              fmt <- r$estimate_fmt[[1L]]
              if (fmt == "") "" else latex_stars(fmt)
            }, character(1L))
          )
          coef_lines <- c(coef_lines, row_line(est_cells))
        }

        # SE / tstat / pvalue row
        if (digits_override || se_fmt_choice != "se") {
          se_vals <- vapply(mn, function(mod) {
            r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
            if (nrow(r) == 0L) return(NA_real_)
            switch(se_fmt_choice,
              "se"     = r$std.error[[1L]],
              "tstat"  = {
                est <- r$estimate[[1L]]; se <- r$std.error[[1L]]
                if (is.na(est) || is.na(se) || se == 0) NA_real_
                else abs(est / se)
              },
              "pvalue" = r$p.value[[1L]],
              r$std.error[[1L]]
            )
          }, numeric(1L))
          coef_lines <- c(coef_lines,
                          tex_se_row(se_vals, digits = d, format = se_fmt_choice))
        } else {
          se_cells <- c(
            "",
            vapply(mn, function(mod) {
              r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
              if (nrow(r) == 0L) "" else r$se_fmt[[1L]]
            }, character(1L))
          )
          coef_lines <- c(coef_lines, row_line(se_cells))
        }
      }
    }

    # Fit statistic rows
    stat_lines <- character(0L)
    if (nrow(x$stat_data) > 0L) {
      stat_labels <- unique(x$stat_data$stat)
      for (sl in stat_labels) {
        stat_display <- latex_stat_label(sl)
        val_strs <- vapply(mn, function(mod) {
          r <- x$stat_data[x$stat_data$stat == sl & x$stat_data$model == mod, ]
          if (nrow(r) == 0L) return("")
          if (digits_override && "value_raw" %in% names(r) &&
              !is.na(r$value_raw[[1L]])) {
            format_stat(r$value_raw[[1L]], sl, d)
          } else {
            r$value_fmt[[1L]]
          }
        }, character(1L))
        stat_lines <- c(stat_lines, row_line(c(stat_display, val_strs)))
      }
    }

    # FE rows
    fe_lines <- character(0L)
    if (nrow(x$fe_data) > 0L) {
      fe_labels_uniq <- unique(x$fe_data$fe_label)
      for (fl in fe_labels_uniq) {
        fe_lines <- c(fe_lines, tex_fe_row(fl, x$fe_data, mn))
      }
    }

    # add_rows
    add_lines <- character(0L)
    if (!is.null(x$add_rows)) {
      for (i in seq_len(nrow(x$add_rows))) {
        ar <- x$add_rows[i, , drop = FALSE]
        vals <- vapply(mn, function(mod) {
          v <- ar[[mod]]
          if (is.null(v) || (length(v) == 1L && is.na(v))) "" else as.character(v)
        }, character(1L))
        add_lines <- c(add_lines,
                       row_line(c(latex_escape(ar[["label"]]), vals)))
      }
    }

    # Significance note line
    se_type_val <- x$se_type %||% stats::setNames(rep("IID", length(mn)), mn)
    note_line_str <- tex_note_line(
      n_cols         = n_cols,
      se_type        = se_type_val,
      se_format      = x$se_format %||% "se",
      fe_labels      = x$fe_labels,
      cluster_labels = x$cluster_labels,
      note           = note
    )

    tabular <- c(
      paste0("\\begin{tabular}{", col_spec, "}"),
      "\\toprule",
      group_lines,
      header_line,
      "\\midrule",
      depvar_line,
      coef_lines,
      if (length(stat_lines) > 0L) c("\\midrule", stat_lines),
      if (length(fe_lines) > 0L || length(add_lines) > 0L)
        c("\\midrule", fe_lines, add_lines),
      "\\bottomrule",
      note_line_str,
      "\\end{tabular}"
    )

  } else {
    stop(
      "exportreg: supply either `x` (regtab_table) or `rows` (character vector).",
      call. = FALSE
    )
  }

  if (format == "full") {
    tabular <- c(
      "\\documentclass{article}",
      "\\usepackage{booktabs}",
      "\\begin{document}",
      tabular,
      "\\end{document}"
    )
  }

  # Remove NULL elements from conditional blocks
  tabular <- tabular[!vapply(tabular, is.null, logical(1L))]

  cat(paste(tabular, collapse = "\n"), "\n")

  if (!is.null(file)) {
    writeLines(tabular, con = file)
  }

  invisible(tabular)
}

#' @rdname tex_table
#' @export
to_latex <- tex_table

# ---------------------------------------------------------------------------
# Internal LaTeX helpers
# ---------------------------------------------------------------------------

#' Escape special LaTeX characters in text strings
#' @noRd
latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

#' Map stat name to LaTeX display label
#' @noRd
latex_stat_label <- function(stat_name) {
  switch(stat_name,
    "N"          = "$N$",
    "R2"         = "$R^{2}$",
    "Within R2"  = "Within $R^{2}$",
    "RMSE"       = "RMSE",
    "Clusters"   = "Clusters",
    "KP F-stat"  = "KP $F$-stat",
    stat_name
  )
}
