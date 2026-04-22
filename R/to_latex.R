#' Export a regression table to LaTeX
#'
#' Renders a `regtab_table` as a LaTeX tabular environment using booktabs
#' style. Always prints to the console. Optionally writes to a file.
#'
#' Stars use LaTeX math-mode notation: `$^{*}$`, `$^{**}$`, `$^{***}$`.
#'
#' @param x A `regtab_table` object.
#' @param file Character or `NULL`. If a file path is supplied, the LaTeX
#'   string is written to that file in addition to being printed. Default
#'   `NULL`.
#' @param format `"fragment"` (default) returns only the `tabular`
#'   environment. `"full"` wraps it in a minimal `article` document with
#'   `\\usepackage{booktabs}`.
#' @param note Character or `NULL`. Custom text appended after the standard
#'   significance note.
#' @param digits Integer or `NULL`. Number of decimal places for numeric
#'   formatting. `NULL` (default) uses `x$digits` set at `regtab()` time.
#'   An integer value overrides `x$digits` for this render call only.
#'
#' @return Character vector of LaTeX lines, invisibly.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' tab <- regtab(list("(1)" = m1))
#' to_latex(tab)
#' to_latex(tab, file = "table1.tex", format = "full")
#' to_latex(tab, digits = 2L)
#' }
#'
#' @export
to_latex <- function(x, file = NULL, format = "fragment", note = NULL,
                     digits = NULL) {
  format <- match.arg(format, c("fragment", "full"))
  d <- if (!is.null(digits)) as.integer(digits) else x$digits
  digits_override <- !is.null(digits) && as.integer(digits) != x$digits
  mn       <- x$model_names
  n_models <- length(mn)
  n_cols   <- n_models + 1L

  col_spec <- paste0("l", paste(rep("c", n_models), collapse = ""))
  lines <- character(0L)

  amp <- function(vals) paste(vals, collapse = " & ")
  row_line <- function(vals) paste0(amp(vals), " \\\\")

  # --- col_groups spanning header ---------------------------------------------
  group_lines <- character(0L)
  if (!is.null(x$col_groups)) {
    groups <- unique(x$col_groups)
    cells  <- character(n_cols)
    cells[[1L]] <- ""
    cmidrule_parts <- character(0L)
    col_offset <- 2L
    for (grp in groups) {
      grp_models <- names(x$col_groups)[x$col_groups == grp]
      grp_cols   <- which(mn %in% grp_models) + 1L
      if (length(grp_cols) == 0L) next
      lo <- min(grp_cols); hi <- max(grp_cols)
      cells[[lo]] <- paste0("\\multicolumn{", length(grp_cols), "}{c}{", grp, "}")
      if (hi > lo) for (k in seq(lo + 1L, hi)) cells[[k]] <- ""
      cmidrule_parts <- c(cmidrule_parts,
                          paste0("\\cmidrule(lr){", lo, "-", hi, "}"))
    }
    group_lines <- c(row_line(cells), paste(cmidrule_parts, collapse = " "))
  }

  # --- Column header row ------------------------------------------------------
  header_cells <- c("", mn)
  header_line  <- row_line(header_cells)

  # --- Coefficient rows -------------------------------------------------------
  cd <- x$coef_data
  term_order <- unique(cd[order(cd$row_order), c("term_display", "row_order",
                                                   "is_factor_header")])
  term_order <- term_order[!duplicated(term_order$term_display), ]

  coef_lines <- character(0L)
  for (i in seq_len(nrow(term_order))) {
    tdisp  <- term_order$term_display[[i]]
    is_hdr <- term_order$is_factor_header[[i]]

    est_cells <- c(
      latex_escape(tdisp),
      vapply(mn, function(mod) {
        r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
        if (nrow(r) == 0L) return("")
        fmt <- if (digits_override && !is.na(r$estimate[[1L]])) {
          star_str <- apply_stars(r$p.value[[1L]], x$stars)
          paste0(format_estimate(r$estimate[[1L]], d), star_str)
        } else {
          r$estimate_fmt[[1L]]
        }
        if (fmt == "") "" else latex_stars(fmt)
      }, character(1L))
    )
    coef_lines <- c(coef_lines, row_line(est_cells))

    if (!is_hdr) {
      se_fmt_choice <- if (!is.null(x$se_format)) x$se_format else "se"
      se_cells <- c(
        "",
        vapply(mn, function(mod) {
          r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
          if (nrow(r) == 0L) return("")
          switch(se_fmt_choice,
            "se" = {
              if (digits_override && !is.na(r$std.error[[1L]]))
                format_se(r$std.error[[1L]], d)
              else r$se_fmt[[1L]]
            },
            "tstat" = {
              if (is.na(r$estimate[[1L]]) || is.na(r$std.error[[1L]]) ||
                  r$std.error[[1L]] == 0) ""
              else format_bracket(abs(r$estimate[[1L]] / r$std.error[[1L]]), d)
            },
            "pvalue" = {
              if (is.na(r$p.value[[1L]])) ""
              else format_bracket(r$p.value[[1L]], d)
            },
            r$se_fmt[[1L]]
          )
        }, character(1L))
      )
      coef_lines <- c(coef_lines, row_line(se_cells))
    }
  }

  # --- Fit statistics ---------------------------------------------------------
  stat_lines <- character(0L)
  if (nrow(x$stat_data) > 0L) {
    stat_labels <- unique(x$stat_data$stat)
    for (sl in stat_labels) {
      stat_display <- latex_stat_label(sl)
      vals <- vapply(mn, function(mod) {
        r <- x$stat_data[x$stat_data$stat == sl & x$stat_data$model == mod, ]
        if (nrow(r) == 0L) return("")
        if (digits_override && "value_raw" %in% names(r) &&
            !is.na(r$value_raw[[1L]])) {
          format_stat(r$value_raw[[1L]], sl, d)
        } else {
          r$value_fmt[[1L]]
        }
      }, character(1L))
      stat_lines <- c(stat_lines, row_line(c(stat_display, vals)))
    }
  }

  # --- FE block ---------------------------------------------------------------
  fe_lines <- character(0L)
  if (nrow(x$fe_data) > 0L) {
    fe_labels_uniq <- unique(x$fe_data$fe_label)
    for (fl in fe_labels_uniq) {
      vals <- vapply(mn, function(mod) {
        r <- x$fe_data[x$fe_data$fe_label == fl & x$fe_data$model == mod, ]
        if (nrow(r) == 0L) "" else if (r$included[[1L]]) "Yes" else "No"
      }, character(1L))
      fe_lines <- c(fe_lines, row_line(c(latex_escape(fl), vals)))
    }
  }

  # --- add_rows ---------------------------------------------------------------
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

  # --- Significance note -------------------------------------------------------
  stars_note  <- "*** $p<0.01$, ** $p<0.05$, * $p<0.1$"
  se_type_val <- x$se_type %||% stats::setNames(rep("IID", length(mn)), mn)
  se_note_str <- build_se_note(se_type_val, x$se_format %||% "se", latex = TRUE)
  note_text <- paste0(stars_note, ". ", se_note_str)
  if (!is.null(note) && nchar(note) > 0L) {
    note_text <- paste0(note_text, ". ", note)
  }
  note_line <- paste0(
    "\\multicolumn{", n_cols, "}{l}{\\textit{Note:} ", note_text, "} \\\\"
  )

  # --- Assemble ---------------------------------------------------------------
  tabular <- c(
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    group_lines,
    header_line,
    "\\midrule",
    coef_lines,
    if (length(stat_lines) > 0L) c("\\midrule", stat_lines),
    if (length(fe_lines) > 0L || length(add_lines) > 0L)
      c("\\midrule", fe_lines, add_lines),
    "\\bottomrule",
    note_line,
    "\\end{tabular}"
  )

  if (format == "full") {
    tabular <- c(
      "\\documentclass{article}",
      "\\usepackage{booktabs}",
      "\\begin{document}",
      tabular,
      "\\end{document}"
    )
  }

  # Remove NULL / empty elements that crept in from conditionals
  tabular <- tabular[!vapply(tabular, is.null, logical(1L))]

  cat(paste(tabular, collapse = "\n"), "\n")

  if (!is.null(file)) {
    writeLines(tabular, con = file)
  }

  invisible(tabular)
}

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
