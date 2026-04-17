#' Print a regtab_table to the console
#'
#' Renders an ASCII representation of the table. Useful for quick inspection.
#'
#' @param x A `regtab_table` object.
#' @param ... Ignored.
#' @return `x` invisibly.
#' @export
print.regtab_table <- function(x, ...) {
  mn <- x$model_names
  n_models <- length(mn)

  # --- Build wide coef block --------------------------------------------------
  # One row per unique (term_display, row_order) pair; interleave SE rows.
  cd <- x$coef_data

  # Get unique coefficient rows in display order (by min row_order per term)
  term_order <- unique(cd[order(cd$row_order), c("term_display", "row_order",
                                                   "is_factor_header")])
  term_order <- term_order[!duplicated(term_order$term_display), ]

  # Build display rows
  display_rows <- list()
  for (i in seq_len(nrow(term_order))) {
    tdisp  <- term_order$term_display[[i]]
    is_hdr <- term_order$is_factor_header[[i]]
    row_vals <- vapply(mn, function(mod) {
      r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
      if (nrow(r) == 0L) return("")
      r$estimate_fmt[[1L]]
    }, character(1L))
    display_rows <- c(display_rows, list(c(tdisp, row_vals)))

    if (!is_hdr) {
      se_vals <- vapply(mn, function(mod) {
        r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
        if (nrow(r) == 0L) return("")
        r$se_fmt[[1L]]
      }, character(1L))
      display_rows <- c(display_rows, list(c("", se_vals)))
    }
  }

  # --- Build wide stat block --------------------------------------------------
  stat_rows <- list()
  if (nrow(x$stat_data) > 0L) {
    stat_labels <- unique(x$stat_data$stat)
    for (sl in stat_labels) {
      vals <- vapply(mn, function(mod) {
        r <- x$stat_data[x$stat_data$stat == sl & x$stat_data$model == mod, ]
        if (nrow(r) == 0L) return("") else r$value_fmt[[1L]]
      }, character(1L))
      stat_rows <- c(stat_rows, list(c(sl, vals)))
    }
  }

  # --- Build wide FE block ----------------------------------------------------
  fe_rows <- list()
  if (nrow(x$fe_data) > 0L) {
    fe_labels_uniq <- unique(x$fe_data$fe_label)
    for (fl in fe_labels_uniq) {
      vals <- vapply(mn, function(mod) {
        r <- x$fe_data[x$fe_data$fe_label == fl & x$fe_data$model == mod, ]
        if (nrow(r) == 0L) return("") else if (r$included[[1L]]) "Yes" else "No"
      }, character(1L))
      fe_rows <- c(fe_rows, list(c(fl, vals)))
    }
  }

  # --- add_rows ---------------------------------------------------------------
  add_rows_list <- list()
  if (!is.null(x$add_rows)) {
    for (i in seq_len(nrow(x$add_rows))) {
      ar <- x$add_rows[i, , drop = FALSE]
      lbl  <- ar[["label"]]
      vals <- vapply(mn, function(mod) {
        v <- ar[[mod]]
        if (is.null(v) || is.na(v)) "" else as.character(v)
      }, character(1L))
      add_rows_list <- c(add_rows_list, list(c(lbl, vals)))
    }
  }

  # --- Assemble all rows ------------------------------------------------------
  header <- c("", mn)
  all_rows <- c(
    list(header),
    display_rows,
    if (length(stat_rows) > 0L) c(list(c("---", rep("---", n_models))), stat_rows),
    if (length(fe_rows) > 0L || length(add_rows_list) > 0L)
      c(list(c("---", rep("---", n_models))), fe_rows, add_rows_list)
  )

  # --- Compute column widths --------------------------------------------------
  col_widths <- vapply(seq_len(n_models + 1L), function(j) {
    max(nchar(vapply(all_rows, `[[`, character(1L), j)), na.rm = TRUE)
  }, integer(1L))

  # --- Print ------------------------------------------------------------------
  fmt_row <- function(row) {
    padded <- mapply(function(val, w) formatC(val, width = -w, flag = "-"),
                     row, col_widths, SIMPLIFY = TRUE)
    paste(padded, collapse = "  ")
  }

  cat(fmt_row(all_rows[[1L]]), "\n")
  cat(strrep("-", sum(col_widths) + 2L * n_models), "\n")
  for (row in all_rows[-1L]) {
    if (identical(row[[1L]], "---")) {
      cat(strrep("-", sum(col_widths) + 2L * n_models), "\n")
    } else {
      cat(fmt_row(row), "\n")
    }
  }

  # Significance note
  cat("\nNote: * p<0.1, ** p<0.05, *** p<0.01\n")

  invisible(x)
}
