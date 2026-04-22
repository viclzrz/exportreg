#' Build a regression table
#'
#' Takes a named list of model objects and returns a `regtab_table` object
#' that can be exported to Excel or LaTeX via [to_excel()] and [to_latex()].
#'
#' @param models Named list of model objects. Names become column headers.
#'   Supported classes: `fixest`, `lm`, `glm`, `felm`, `ivreg`.
#' @param keep Character vector of regex patterns. Only coefficients whose
#'   raw names match at least one pattern are shown. Applied before `drop`.
#' @param drop Character vector of regex patterns. Coefficients matching any
#'   pattern are removed. Applied after `keep`.
#' @param coef_map Named character vector mapping raw coefficient names to
#'   display labels, e.g. `c("log_w" = "Log wage")`.
#' @param factor_labels Named character vector mapping a variable stem to a
#'   group header label, e.g. `c("region" = "Region (ref: Norte)")`.
#'   fixest `i()` variables are grouped automatically. Base R factors require
#'   explicit declaration.
#' @param add_rows `data.frame` of manually supplied rows to append below the
#'   FE block. Must have a `label` column plus one column per model name.
#' @param fe_labels Named character vector mapping raw FE variable names to
#'   display labels, e.g. `c("firm_id" = "Firm FE")`.
#' @param stars Numeric vector of p-value thresholds in ascending order.
#'   Default `c(0.1, 0.05, 0.01)` produces `*`, `**`, `***`.
#' @param digits Integer. Number of decimal places. Default `3L`.
#' @param col_groups Named character vector mapping each model name to a
#'   spanning group header label.
#' @param panels Named list of `regtab_table` objects for multi-panel tables.
#'   All panels must have identical `model_names`. When supplied, `models` is
#'   ignored.
#' @param se_format Character scalar controlling what appears in the second row
#'   under each coefficient. `"se"` (default) shows standard errors in
#'   parentheses; `"tstat"` shows absolute t-statistics in brackets; `"pvalue"`
#'   shows p-values in brackets.
#' @param depvar_labels Named character vector mapping raw extracted dependent
#'   variable strings to display labels,
#'   e.g. `c("log(wage)" = "Log Wage")`. `NULL` (default) uses the raw
#'   extracted string.
#'
#' @return An object of class `regtab_table`.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl + hp, data = mtcars)
#' tab <- regtab(list("(1)" = m1, "(2)" = m2))
#' print(tab)
#' }
#'
#' @export
regtab <- function(
    models,
    keep          = NULL,
    drop          = NULL,
    coef_map      = NULL,
    factor_labels = NULL,
    add_rows      = NULL,
    fe_labels     = NULL,
    stars         = c(0.1, 0.05, 0.01),
    digits        = 3L,
    col_groups    = NULL,
    panels        = NULL,
    se_format     = "se",
    depvar_labels = NULL
) {
  # --- panels mode: assemble pre-built regtab_table objects vertically ------
  if (!is.null(panels)) {
    return(assemble_panels(panels, col_groups = col_groups))
  }

  # --- Validate inputs -------------------------------------------------------
  se_format <- match.arg(se_format, c("se", "tstat", "pvalue"))

  if (!is.list(models) || is.null(names(models))) {
    stop("exportreg: `models` must be a named list.", call. = FALSE)
  }
  if (any(names(models) == "")) {
    stop("exportreg: all elements of `models` must be named.", call. = FALSE)
  }
  digits <- as.integer(digits)
  stars  <- sort(unique(stars))

  model_names <- names(models)

  # --- Extract tidy data from each model ------------------------------------
  tidy_list <- lapply(models, tidy_model)
  names(tidy_list) <- model_names

  # --- Coefficient alignment -------------------------------------------------
  all_terms     <- align_coefs(tidy_list)
  ordered_terms <- filter_coefs(all_terms, keep, drop)

  if (length(ordered_terms) == 0L) {
    stop(
      "exportreg: no coefficients remain after applying `keep`/`drop`.",
      call. = FALSE
    )
  }

  # --- Build data blocks -----------------------------------------------------
  coef_data <- build_coef_data(
    tidy_list, model_names, ordered_terms,
    coef_map, factor_labels, digits, stars
  )
  fe_data   <- build_fe_data(tidy_list, model_names, fe_labels)
  stat_data <- build_stat_data(tidy_list, model_names, digits)

  # --- Build se_type vector (one label per model) ----------------------------
  se_type <- vapply(model_names, function(mod) {
    st <- tidy_list[[mod]]$se_type
    if (is.null(st) || is.na(st)) "IID" else st
  }, character(1L))
  names(se_type) <- model_names

  # --- Build depvar_names vector (one resolved label per model) --------------
  depvar_names <- vapply(model_names, function(mod) {
    raw <- tidy_list[[mod]]$depvar
    if (is.null(raw) || is.na(raw)) raw <- ""
    if (!is.null(depvar_labels) && raw %in% names(depvar_labels))
      depvar_labels[[raw]]
    else
      raw
  }, character(1L))
  names(depvar_names) <- model_names

  # --- Return regtab_table ---------------------------------------------------
  structure(
    list(
      coef_data    = coef_data,
      fe_data      = fe_data,
      add_rows     = add_rows,
      stat_data    = stat_data,
      model_names  = model_names,
      col_groups   = col_groups,
      digits       = digits,
      stars        = stars,
      se_format    = se_format,
      se_type      = se_type,
      depvar_names = depvar_names,
      call         = match.call()
    ),
    class = "regtab_table"
  )
}

# ---------------------------------------------------------------------------
# Internal: assemble panels
# ---------------------------------------------------------------------------

assemble_panels <- function(panels, col_groups) {
  if (!is.list(panels) || is.null(names(panels))) {
    stop("exportreg: `panels` must be a named list of regtab_table objects.",
         call. = FALSE)
  }

  # Validate shared model_names
  all_mn <- lapply(panels, `[[`, "model_names")
  if (length(unique(lapply(all_mn, paste, collapse = "|"))) > 1L) {
    stop(
      "exportreg: all panels must have identical `model_names`.",
      call. = FALSE
    )
  }

  model_names <- panels[[1L]]$model_names
  panel_names <- names(panels)
  letters_uc  <- LETTERS[seq_along(panels)]

  # Tag each panel's coef_data with a panel identifier row and panel column
  coef_blocks <- vector("list", length(panels))
  fe_blocks   <- vector("list", length(panels))
  stat_blocks <- vector("list", length(panels))

  for (i in seq_along(panels)) {
    p    <- panels[[i]]
    plab <- paste0("Panel ", letters_uc[[i]], ": ", panel_names[[i]])

    # Insert a header row for each model (all empty values)
    header <- data.frame(
      term_display     = plab,
      term_raw         = paste0(".panel_header_", i),
      model            = model_names,
      estimate         = NA_real_,
      std.error        = NA_real_,
      p.value          = NA_real_,
      estimate_fmt     = "",
      se_fmt           = "",
      is_factor_header = TRUE,
      factor_group     = NA_character_,
      row_order        = -Inf,
      stringsAsFactors = FALSE
    )

    cd <- p$coef_data
    # Offset row_order so panels don't collide
    cd$row_order <- cd$row_order + (i - 1L) * 1e6

    coef_blocks[[i]] <- rbind(header, cd)
    fe_blocks[[i]]   <- p$fe_data
    stat_blocks[[i]] <- p$stat_data
  }

  structure(
    list(
      coef_data    = do.call(rbind, coef_blocks),
      fe_data      = do.call(rbind, fe_blocks),
      add_rows     = NULL,
      stat_data    = do.call(rbind, stat_blocks),
      model_names  = model_names,
      col_groups   = col_groups,
      digits       = panels[[1L]]$digits,
      stars        = panels[[1L]]$stars,
      se_format    = panels[[1L]]$se_format,
      se_type      = panels[[1L]]$se_type,
      depvar_names = panels[[1L]]$depvar_names,
      call         = match.call()
    ),
    class = "regtab_table"
  )
}
