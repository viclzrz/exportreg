# Internal utility helpers for exportreg
# None of these are exported.

# ---------------------------------------------------------------------------
# Stars
# ---------------------------------------------------------------------------

#' Apply significance stars to a p-value
#'
#' Returns a plain-text star string used in `estimate_fmt`.
#' `to_latex()` converts these to math-mode notation via `latex_stars()`.
#'
#' @param p_value numeric vector of p-values
#' @param thresholds numeric vector of thresholds in ascending order,
#'   e.g. `c(0.1, 0.05, 0.01)`
#' @return character vector, same length as `p_value`
#' @noRd
apply_stars <- function(p_value, thresholds) {
  thresholds <- sort(thresholds, decreasing = TRUE)
  stars <- vapply(p_value, function(p) {
    if (is.na(p)) return("")
    n <- sum(p < thresholds)
    strrep("*", n)
  }, character(1L))
  stars
}

#' Convert plain-text stars to LaTeX math-mode superscripts
#'
#' Replaces trailing `***`/`**`/`*` in a formatted estimate string with
#' `$^{***}$`, `$^{**}$`, `$^{*}$`.
#'
#' @param estimate_fmt character vector
#' @return character vector
#' @noRd
latex_stars <- function(estimate_fmt) {
  x <- estimate_fmt
  x <- gsub("[*]{3}$", "$^{***}$", x)
  x <- gsub("[*]{2}$", "$^{**}$",  x)
  x <- gsub("[*]{1}$", "$^{*}$",   x)
  x
}

# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

#' Format a numeric estimate to fixed decimal places
#'
#' @param x numeric vector
#' @param digits integer
#' @return character vector
#' @noRd
format_estimate <- function(x, digits) {
  formatC(x, digits = digits, format = "f", flag = "")
}

#' Format a standard error as "(0.xxx)"
#'
#' @param x numeric vector
#' @param digits integer
#' @return character vector
#' @noRd
format_se <- function(x, digits) {
  ifelse(is.na(x), "", paste0("(", format_estimate(x, digits), ")"))
}

#' Format a fit statistic value
#'
#' N gets comma-formatted integers; everything else gets `digits` decimal places.
#'
#' @param x numeric scalar
#' @param stat_name character scalar — one of "N", "Clusters", "R2",
#'   "Within R2", "RMSE", "KP F-stat"
#' @param digits integer
#' @return character scalar
#' @noRd
format_stat <- function(x, stat_name, digits) {
  if (is.na(x)) return("")
  if (stat_name %in% c("N", "Clusters")) {
    formatC(as.integer(x), format = "d", big.mark = ",")
  } else {
    format_estimate(x, digits)
  }
}

# ---------------------------------------------------------------------------
# Coefficient alignment
# ---------------------------------------------------------------------------

#' Union of term names across models, preserving first-appearance order
#'
#' Terms from `models[[1]]` come first, then any terms unique to later models
#' in the order they first appear.
#'
#' @param tidy_list list of tidy_model() outputs
#' @return character vector
#' @noRd
align_coefs <- function(tidy_list) {
  all_terms <- character(0L)
  for (tm in tidy_list) {
    new_terms <- setdiff(tm$coefs$term, all_terms)
    all_terms <- c(all_terms, new_terms)
  }
  all_terms
}

#' Filter a character vector of term names by keep/drop patterns
#'
#' `keep` and `drop` are treated as regex patterns. `keep` is applied first
#' (only terms matching any keep pattern are retained); then `drop` removes
#' any remaining terms matching any drop pattern.
#'
#' @param terms character vector of raw term names
#' @param keep character vector of regex patterns, or NULL
#' @param drop character vector of regex patterns, or NULL
#' @return character vector
#' @noRd
filter_coefs <- function(terms, keep, drop) {
  if (!is.null(keep) && length(keep) > 0L) {
    keep_re <- paste(keep, collapse = "|")
    terms <- terms[grepl(keep_re, terms)]
  }
  if (!is.null(drop) && length(drop) > 0L) {
    drop_re <- paste(drop, collapse = "|")
    terms <- terms[!grepl(drop_re, terms)]
  }
  terms
}

# ---------------------------------------------------------------------------
# Block builders
# ---------------------------------------------------------------------------

#' Build the coef_data long data.frame for a regtab_table
#'
#' @param tidy_list named list of tidy_model() outputs
#' @param model_names character vector of model labels (names of tidy_list)
#' @param ordered_terms character vector from align_coefs() / filter_coefs()
#' @param coef_map named character vector or NULL
#' @param factor_labels named character vector or NULL
#' @param digits integer
#' @param stars numeric vector of thresholds
#' @return data.frame
#' @noRd
build_coef_data <- function(tidy_list, model_names, ordered_terms,
                             coef_map, factor_labels, digits, stars) {
  rows <- vector("list", length(model_names) * length(ordered_terms))
  idx <- 1L

  for (mod in model_names) {
    coefs_df <- tidy_list[[mod]]$coefs
    for (i in seq_along(ordered_terms)) {
      trm <- ordered_terms[[i]]
      match_row <- coefs_df[coefs_df$term == trm, , drop = FALSE]

      if (nrow(match_row) == 0L) {
        est <- NA_real_; se <- NA_real_; pv <- NA_real_
        est_fmt <- ""; se_fmt <- ""
      } else {
        est <- match_row$estimate[[1L]]
        se  <- match_row$std.error[[1L]]
        pv  <- match_row$p.value[[1L]]
        star_str <- apply_stars(pv, stars)
        est_fmt  <- paste0(format_estimate(est, digits), star_str)
        se_fmt   <- format_se(se, digits)
      }

      # Display label: coef_map takes precedence over raw name
      disp <- if (!is.null(coef_map) && trm %in% names(coef_map)) {
        coef_map[[trm]]
      } else {
        trm
      }

      # Factor grouping: check if this term matches a factor_labels stem
      fg <- NA_character_
      if (!is.null(factor_labels)) {
        for (stem in names(factor_labels)) {
          if (startsWith(trm, stem)) {
            fg <- stem
            break
          }
        }
      }

      rows[[idx]] <- data.frame(
        term_display     = disp,
        term_raw         = trm,
        model            = mod,
        estimate         = est,
        std.error        = se,
        p.value          = pv,
        estimate_fmt     = est_fmt,
        se_fmt           = se_fmt,
        is_factor_header = FALSE,
        factor_group     = fg,
        row_order        = i,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  coef_data <- do.call(rbind, rows)

  # Insert factor header rows when factor_labels is provided
  if (!is.null(factor_labels)) {
    header_rows <- vector("list", length(factor_labels) * length(model_names))
    h_idx <- 1L
    for (stem in names(factor_labels)) {
      stem_rows <- coef_data[!is.na(coef_data$factor_group) &
                               coef_data$factor_group == stem, ]
      if (nrow(stem_rows) == 0L) next
      min_order <- min(stem_rows$row_order) - 0.5
      for (mod in model_names) {
        header_rows[[h_idx]] <- data.frame(
          term_display     = factor_labels[[stem]],
          term_raw         = stem,
          model            = mod,
          estimate         = NA_real_,
          std.error        = NA_real_,
          p.value          = NA_real_,
          estimate_fmt     = "",
          se_fmt           = "",
          is_factor_header = TRUE,
          factor_group     = stem,
          row_order        = min_order,
          stringsAsFactors = FALSE
        )
        h_idx <- h_idx + 1L
      }
    }
    header_rows <- header_rows[!vapply(header_rows, is.null, logical(1L))]
    if (length(header_rows) > 0L) {
      coef_data <- rbind(coef_data, do.call(rbind, header_rows))
    }
  }

  coef_data[order(coef_data$row_order), ]
}

#' Build the fe_data long data.frame for a regtab_table
#'
#' @param tidy_list named list of tidy_model() outputs
#' @param model_names character vector
#' @param fe_labels named character vector or NULL
#' @return data.frame
#' @noRd
build_fe_data <- function(tidy_list, model_names, fe_labels) {
  # Collect union of all FE variable names across fixest models
  all_fe <- character(0L)
  for (tm in tidy_list) {
    if (!is.null(tm$fe_vars)) {
      all_fe <- union(all_fe, tm$fe_vars)
    }
  }

  if (length(all_fe) == 0L) {
    return(data.frame(
      fe_label = character(0L),
      fe_raw   = character(0L),
      model    = character(0L),
      included = logical(0L),
      stringsAsFactors = FALSE
    ))
  }

  rows <- vector("list", length(all_fe) * length(model_names))
  idx <- 1L
  for (fe in all_fe) {
    lbl <- if (!is.null(fe_labels) && fe %in% names(fe_labels)) {
      fe_labels[[fe]]
    } else {
      fe
    }
    for (mod in model_names) {
      incl <- !is.null(tidy_list[[mod]]$fe_vars) &&
        fe %in% tidy_list[[mod]]$fe_vars
      rows[[idx]] <- data.frame(
        fe_label = lbl,
        fe_raw   = fe,
        model    = mod,
        included = incl,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, rows)
}

#' Build the stat_data long data.frame for a regtab_table
#'
#' Stat display order: N, Clusters, R2, Within R2, RMSE, KP F-stat.
#' A stat row is omitted entirely if it is NA for all models.
#'
#' @param tidy_list named list of tidy_model() outputs
#' @param model_names character vector
#' @param digits integer
#' @return data.frame
#' @noRd
build_stat_data <- function(tidy_list, model_names, digits) {
  stat_defs <- list(
    list(key = "nobs",          label = "N"),
    list(key = "nobs_clusters", label = "Clusters"),
    list(key = "r2",            label = "R2"),
    list(key = "r2_within",     label = "Within R2"),
    list(key = "rmse",          label = "RMSE"),
    list(key = "kp_fstat",      label = "KP F-stat")
  )

  rows <- list()
  for (sd in stat_defs) {
    vals <- vapply(model_names, function(mod) {
      tidy_list[[mod]]$glance[[sd$key]]
    }, numeric(1L))

    # Skip row if all NA
    if (all(is.na(vals))) next

    for (i in seq_along(model_names)) {
      rows <- c(rows, list(data.frame(
        stat      = sd$label,
        model     = model_names[[i]],
        value_fmt = format_stat(vals[[i]], sd$label, digits),
        value_raw = vals[[i]],
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      stat = character(0L), model = character(0L),
      value_fmt = character(0L), value_raw = numeric(0L),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Pivot helper
# ---------------------------------------------------------------------------

#' Pivot a long data.frame to wide format
#'
#' @param long_df data.frame with at least `id_col`, `"model"`, and `value_col`
#' @param id_col string: column whose unique values become row identifiers
#' @param value_col string: column whose values fill cells
#' @param model_names character vector: column order for the wide output
#' @return data.frame with `id_col` + one column per model
#' @noRd
pivot_wide <- function(long_df, id_col, value_col, model_names) {
  ids <- unique(long_df[[id_col]])
  out <- data.frame(label = ids, stringsAsFactors = FALSE)
  names(out) <- id_col
  for (mod in model_names) {
    sub <- long_df[long_df$model == mod, , drop = FALSE]
    vals <- sub[[value_col]][match(ids, sub[[id_col]])]
    out[[mod]] <- vals
  }
  out
}

#' Detect fixest i() variable stems from a model formula
#'
#' Looks for `i(var, ...)` patterns in the deparsed formula string.
#'
#' @param model a fixest model object
#' @return character vector of variable stems (may be length 0)
#' @noRd
detect_i_vars <- function(model) {
  fml_str <- tryCatch(
    deparse(stats::formula(model)),
    error = function(e) ""
  )
  m <- gregexpr("\\bi\\(([^,)]+)", fml_str, perl = TRUE)
  if (m[[1L]][[1L]] == -1L) return(character(0L))
  stems <- regmatches(fml_str, m)[[1L]]
  trimws(sub("^i\\(", "", stems))
}
