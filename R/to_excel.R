#' Export a regression table to Excel
#'
#' Writes a formatted Excel workbook using the `openxlsx2` package.
#' No Java dependency.
#'
#' @param x A `regtab_table` object.
#' @param file Character. Output file path (required). Should end in `.xlsx`.
#' @param sheet Character. Worksheet name. Default `"Table 1"`.
#' @param open Logical. If `TRUE`, attempt to open the file after writing.
#'   Default `FALSE`.
#' @param raw Logical. If `TRUE`, estimate and SE cells are written as R
#'   numeric values (usable in Excel formulas) instead of pre-formatted
#'   character strings. All cell styling is still applied. Default `FALSE`.
#' @param digits Integer or `NULL`. Number of decimal places for numeric
#'   formatting. `NULL` (default) uses `x$digits` set at `regtab()` time.
#'   An integer value overrides `x$digits` for this render call only.
#'   Ignored when `raw = TRUE`.
#'
#' @return `x` invisibly.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' tab <- regtab(list("(1)" = m1))
#' to_excel(tab, file = "table1.xlsx")
#' to_excel(tab, file = "table1_raw.xlsx", raw = TRUE)
#' to_excel(tab, file = "table1_5dp.xlsx", digits = 5L)
#' }
#'
#' @export
to_excel <- function(x, file, sheet = "Table 1", open = FALSE,
                     raw = FALSE, digits = NULL) {
  if (missing(file)) stop("exportreg: `file` is required.", call. = FALSE)

  d <- if (!is.null(digits)) as.integer(digits) else x$digits
  digits_override <- !is.null(digits) && as.integer(digits) != x$digits

  mn       <- x$model_names
  n_models <- length(mn)
  n_cols   <- n_models + 1L

  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet)

  current_row <- 1L

  # Helper: write a vector of values as a single row (one cell per column)
  write_row <- function(values) {
    for (j in seq_along(values)) {
      v <- values[[j]]
      if (!is.null(v) && !identical(v, "")) {
        wb <<- openxlsx2::wb_add_data(
          wb, sheet = sheet,
          x = v,
          dims = openxlsx2::wb_dims(current_row, j),
          col_names = FALSE
        )
      }
    }
    current_row <<- current_row + 1L
  }

  # Helper: dims string for a full row
  row_dims <- function(r) {
    openxlsx2::wb_dims(rows = r, cols = seq_len(n_cols))
  }

  # Helper: dims string for a single cell
  cell_dims <- function(r, j) openxlsx2::wb_dims(r, j)

  # --- col_groups spanning header -------------------------------------------
  if (!is.null(x$col_groups)) {
    groups <- unique(x$col_groups)
    row_vals <- rep("", n_cols)
    for (grp in groups) {
      grp_models <- names(x$col_groups)[x$col_groups == grp]
      grp_cols   <- which(mn %in% grp_models) + 1L
      if (length(grp_cols) == 0L) next
      row_vals[[grp_cols[[1L]]]] <- grp
    }
    write_row(as.list(row_vals))
    grp_row <- current_row - 1L
    # Bold the group header row
    wb <- openxlsx2::wb_add_font(wb, sheet = sheet,
                                  dims = row_dims(grp_row), bold = TRUE)
    # Merge cells for each group that spans multiple columns
    for (grp in groups) {
      grp_models <- names(x$col_groups)[x$col_groups == grp]
      grp_cols   <- which(mn %in% grp_models) + 1L
      if (length(grp_cols) < 2L) next
      lo <- min(grp_cols); hi <- max(grp_cols)
      wb <- openxlsx2::wb_merge_cells(
        wb, sheet = sheet,
        dims = openxlsx2::wb_dims(rows = grp_row, cols = lo:hi)
      )
    }
  }

  # --- Column header row (model names) --------------------------------------
  header_row <- current_row
  write_row(c(list(""), as.list(mn)))
  # Bold headers
  wb <- openxlsx2::wb_add_font(wb, sheet = sheet,
                                dims = row_dims(header_row), bold = TRUE)
  # Bottom border on header row
  wb <- openxlsx2::wb_add_border(wb, sheet = sheet,
                                  dims = row_dims(header_row),
                                  bottom_border = "thin",
                                  top_border = NULL,
                                  left_border = NULL,
                                  right_border = NULL)

  # --- Dependent variable row ------------------------------------------------
  depvar_row_num <- current_row
  write_row(c(list("Dep. var."), lapply(mn, function(mod) {
    v <- x$depvar_names[[mod]] %||% ""
    if (identical(v, "")) NULL else v
  })))
  wb <- openxlsx2::wb_add_font(wb, sheet = sheet,
                                dims = row_dims(depvar_row_num), bold = TRUE)

  # --- Coefficient rows -------------------------------------------------------
  cd <- x$coef_data
  term_order <- unique(cd[order(cd$row_order),
                          c("term_display", "row_order",
                            "is_factor_header", "factor_group")])
  term_order <- term_order[!duplicated(term_order$term_display), ]

  for (i in seq_len(nrow(term_order))) {
    tdisp  <- term_order$term_display[[i]]
    is_hdr <- term_order$is_factor_header[[i]]
    fg     <- term_order$factor_group[[i]]

    est_vals <- c(list(tdisp), lapply(mn, function(mod) {
      r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
      if (nrow(r) == 0L) return(NULL)
      if (raw) {
        if (is.na(r$estimate[[1L]])) NULL else r$estimate[[1L]]
      } else if (digits_override && !is.na(r$estimate[[1L]])) {
        star_str <- apply_stars(r$p.value[[1L]], x$stars)
        paste0(format_estimate(r$estimate[[1L]], d), star_str)
      } else {
        if (r$estimate_fmt[[1L]] == "") NULL else r$estimate_fmt[[1L]]
      }
    }))
    est_row <- current_row
    write_row(est_vals)

    # Factor header: bold label cell
    if (is_hdr) {
      wb <- openxlsx2::wb_add_font(wb, sheet = sheet,
                                    dims = cell_dims(est_row, 1L), bold = TRUE)
    } else if (!is.na(fg)) {
      # Factor child: indent label cell
      wb <- openxlsx2::wb_add_cell_style(wb, sheet = sheet,
                                          dims = cell_dims(est_row, 1L),
                                          indent = 1L)
    }

    if (!is_hdr) {
      se_fmt_choice <- x$se_format %||% "se"
      se_vals <- c(list(NULL), lapply(mn, function(mod) {
        r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
        if (nrow(r) == 0L) return(NULL)
        val <- switch(se_fmt_choice,
          "se" = {
            if (raw) {
              if (is.na(r$std.error[[1L]])) NULL else r$std.error[[1L]]
            } else if (digits_override && !is.na(r$std.error[[1L]])) {
              format_se(r$std.error[[1L]], d)
            } else {
              if (r$se_fmt[[1L]] == "") NULL else r$se_fmt[[1L]]
            }
          },
          "tstat" = {
            if (is.na(r$estimate[[1L]]) || is.na(r$std.error[[1L]]) ||
                r$std.error[[1L]] == 0) NULL
            else if (raw) abs(r$estimate[[1L]] / r$std.error[[1L]])
            else format_bracket(abs(r$estimate[[1L]] / r$std.error[[1L]]), d)
          },
          "pvalue" = {
            if (is.na(r$p.value[[1L]])) NULL
            else if (raw) r$p.value[[1L]]
            else format_bracket(r$p.value[[1L]], d)
          },
          {
            if (r$se_fmt[[1L]] == "") NULL else r$se_fmt[[1L]]
          }
        )
        if (is.null(val) || identical(val, "")) NULL else val
      }))
      write_row(se_vals)
    }
  }

  # --- Top border + fit statistics -------------------------------------------
  if (nrow(x$stat_data) > 0L) {
    stat_start_row <- current_row
    stat_labels    <- unique(x$stat_data$stat)
    for (sl in stat_labels) {
      vals <- c(list(sl), lapply(mn, function(mod) {
        r <- x$stat_data[x$stat_data$stat == sl & x$stat_data$model == mod, ]
        if (nrow(r) == 0L) return(NULL)
        fmt <- if (digits_override && "value_raw" %in% names(r) &&
                   !is.na(r$value_raw[[1L]])) {
          format_stat(r$value_raw[[1L]], sl, d)
        } else {
          r$value_fmt[[1L]]
        }
        if (fmt == "") NULL else fmt
      }))
      write_row(vals)
    }
    wb <- openxlsx2::wb_add_border(wb, sheet = sheet,
                                    dims = row_dims(stat_start_row),
                                    top_border    = "thin",
                                    bottom_border = NULL,
                                    left_border   = NULL,
                                    right_border  = NULL)
  }

  # --- FE block ---------------------------------------------------------------
  fe_start_row <- current_row
  fe_written   <- FALSE
  if (nrow(x$fe_data) > 0L) {
    fe_labels_uniq <- unique(x$fe_data$fe_label)
    for (fl in fe_labels_uniq) {
      vals <- c(list(fl), lapply(mn, function(mod) {
        r <- x$fe_data[x$fe_data$fe_label == fl & x$fe_data$model == mod, ]
        if (nrow(r) == 0L) NULL else if (r$included[[1L]]) "Yes" else "No"
      }))
      write_row(vals)
      fe_written <- TRUE
    }
  }

  # --- add_rows ---------------------------------------------------------------
  if (!is.null(x$add_rows)) {
    for (i in seq_len(nrow(x$add_rows))) {
      ar <- x$add_rows[i, , drop = FALSE]
      vals <- c(list(ar[["label"]]), lapply(mn, function(mod) {
        v <- ar[[mod]]
        if (is.null(v) || (length(v) == 1L && is.na(v))) NULL else as.character(v)
      }))
      write_row(vals)
      fe_written <- TRUE
    }
  }

  if (fe_written) {
    wb <- openxlsx2::wb_add_border(wb, sheet = sheet,
                                    dims = row_dims(fe_start_row),
                                    top_border    = "thin",
                                    bottom_border = NULL,
                                    left_border   = NULL,
                                    right_border  = NULL)
  }

  # --- Significance note -------------------------------------------------------
  se_type_val <- x$se_type %||% stats::setNames(rep("IID", length(mn)), mn)
  se_note_str <- build_se_note(se_type_val, x$se_format %||% "se",
                               fe_labels      = x$fe_labels,
                               cluster_labels = x$cluster_labels,
                               latex          = FALSE)
  note_str    <- paste0("Note: *** p<0.01, ** p<0.05, * p<0.1. ", se_note_str)
  note_dims <- openxlsx2::wb_dims(rows = current_row, cols = seq_len(n_cols))
  wb <- openxlsx2::wb_add_data(
    wb, sheet = sheet,
    x = note_str,
    dims = cell_dims(current_row, 1L),
    col_names = FALSE
  )
  wb <- openxlsx2::wb_merge_cells(wb, sheet = sheet, dims = note_dims)
  wb <- openxlsx2::wb_add_font(wb, sheet = sheet,
                                dims = cell_dims(current_row, 1L), italic = TRUE)

  # --- Auto-fit column widths ------------------------------------------------
  wb <- openxlsx2::wb_set_col_widths(
    wb, sheet = sheet,
    cols = seq_len(n_cols), widths = "auto"
  )

  # --- Write file -------------------------------------------------------------
  openxlsx2::wb_save(wb, file = file, overwrite = TRUE)

  if (isTRUE(open)) {
    tryCatch(
      utils::browseURL(file),
      error = function(e) message("exportreg: could not open file: ", file)
    )
  }

  invisible(x)
}
