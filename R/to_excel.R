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
#'
#' @return `x` invisibly.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' tab <- regtab(list("(1)" = m1))
#' to_excel(tab, file = "table1.xlsx")
#' }
#'
#' @export
to_excel <- function(x, file, sheet = "Table 1", open = FALSE) {
  if (missing(file)) stop("exportreg: `file` is required.", call. = FALSE)

  mn       <- x$model_names
  n_models <- length(mn)
  n_cols   <- n_models + 1L   # label col + one per model

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
    # Bold the group header row
    wb <- openxlsx2::wb_add_font(wb, sheet = sheet,
                                  dims = row_dims(current_row - 1L), bold = TRUE)
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

  # --- Coefficient rows -------------------------------------------------------
  cd <- x$coef_data
  term_order <- unique(cd[order(cd$row_order), c("term_display", "row_order",
                                                   "is_factor_header")])
  term_order <- term_order[!duplicated(term_order$term_display), ]

  for (i in seq_len(nrow(term_order))) {
    tdisp  <- term_order$term_display[[i]]
    is_hdr <- term_order$is_factor_header[[i]]

    est_vals <- c(list(tdisp), lapply(mn, function(mod) {
      r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
      if (nrow(r) == 0L || r$estimate_fmt[[1L]] == "") NULL else r$estimate_fmt[[1L]]
    }))
    write_row(est_vals)

    if (!is_hdr) {
      se_vals <- c(list(NULL), lapply(mn, function(mod) {
        r <- cd[cd$term_display == tdisp & cd$model == mod, , drop = FALSE]
        if (nrow(r) == 0L || r$se_fmt[[1L]] == "") NULL else r$se_fmt[[1L]]
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
        if (nrow(r) == 0L || r$value_fmt[[1L]] == "") NULL else r$value_fmt[[1L]]
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
  note_str  <- "Note: * p<0.1, ** p<0.05, *** p<0.01"
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
