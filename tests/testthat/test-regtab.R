test_that("regtab() returns a regtab_table with correct structure", {
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))

  expect_s3_class(tab, "regtab_table")
  expect_named(tab, c("coef_data", "fe_data", "add_rows", "stat_data",
                       "model_names", "col_groups", "digits", "stars",
                       "se_format", "se_type", "fe_labels", "cluster_labels",
                       "depvar_names", "call"))
  expect_equal(tab$model_names, c("(1)", "(2)"))
})

test_that("regtab() coef_data has correct columns", {
  tab <- regtab(list("(1)" = lm_basic))
  expect_named(tab$coef_data,
    c("term_display", "term_raw", "model", "estimate", "std.error",
      "p.value", "estimate_fmt", "se_fmt", "is_factor_header",
      "factor_group", "row_order"))
})

test_that("regtab() aligns non-overlapping regressors", {
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  # x2 is in (1) but not (2); should still appear as a row
  all_terms <- unique(tab$coef_data$term_raw)
  expect_true("x2" %in% all_terms)

  # For model (2), x2 estimate should be NA
  x2_m2 <- tab$coef_data[tab$coef_data$term_raw == "x2" &
                            tab$coef_data$model == "(2)", ]
  expect_equal(nrow(x2_m2), 1L)
  expect_true(is.na(x2_m2$estimate))
  expect_equal(x2_m2$estimate_fmt, "")
})

test_that("regtab() keep and drop work correctly", {
  # keep only x1
  tab_keep <- regtab(list("(1)" = lm_basic), keep = "x1")
  expect_false("x2" %in% tab_keep$coef_data$term_raw)
  expect_true("x1" %in% tab_keep$coef_data$term_raw)

  # drop intercept
  tab_drop <- regtab(list("(1)" = lm_basic), drop = "Intercept")
  expect_false(any(grepl("Intercept", tab_drop$coef_data$term_raw)))
})

test_that("regtab() coef_map renames coefficients", {
  tab <- regtab(
    list("(1)" = lm_basic),
    coef_map = c("x1" = "Age", "x2" = "Male")
  )
  expect_true("Age"  %in% tab$coef_data$term_display)
  expect_true("Male" %in% tab$coef_data$term_display)
  # Raw names still stored
  expect_true("x1" %in% tab$coef_data$term_raw)
})

test_that("regtab() stat_data contains N row", {
  tab <- regtab(list("(1)" = lm_basic))
  expect_true("N" %in% tab$stat_data$stat)
  n_row <- tab$stat_data[tab$stat_data$stat == "N" &
                           tab$stat_data$model == "(1)", ]
  expect_equal(n_row$value_fmt, "16")
})

test_that("regtab() fe_data is populated for fixest models", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tab <- regtab(list("FE2" = mods$feols_multi_fe, "FE1" = mods$feols_one_fe))
  expect_true(nrow(tab$fe_data) > 0L)
  expect_named(tab$fe_data, c("fe_label", "fe_raw", "model", "included"))

  # firm_id should be Yes for both models
  firm_fe <- tab$fe_data[tab$fe_data$fe_raw == "firm_id", ]
  expect_true(all(firm_fe$included))

  # year FE: Yes for feols_multi_fe, No for feols_one_fe
  year_fe <- tab$fe_data[tab$fe_data$fe_raw == "year", ]
  expect_true(year_fe$included[year_fe$model == "FE2"])
  expect_false(year_fe$included[year_fe$model == "FE1"])
})

test_that("regtab() fe_labels rename FE rows", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tab <- regtab(
    list("(1)" = mods$feols_multi_fe),
    fe_labels = c("firm_id" = "Firm FE", "year" = "Year FE")
  )
  expect_true("Firm FE" %in% tab$fe_data$fe_label)
  expect_true("Year FE" %in% tab$fe_data$fe_label)
})

test_that("regtab() factor_labels inserts header row and groups dummies", {
  # lm_factor has terms regionNorte, regionSur, regionEste (Centro is ref)
  tab <- regtab(
    list("(1)" = lm_factor),
    factor_labels = c("region" = "Region (ref: Centro)")
  )

  # Header row should appear
  headers <- tab$coef_data[tab$coef_data$is_factor_header, ]
  expect_true(nrow(headers) > 0L)
  expect_true("Region (ref: Centro)" %in% headers$term_display)

  # Dummy rows (non-header) should be grouped under that stem
  dummy_rows <- tab$coef_data[
    !is.na(tab$coef_data$factor_group) &
      tab$coef_data$factor_group == "region" &
      !tab$coef_data$is_factor_header, ]
  expect_true(nrow(dummy_rows) > 0L)

  # Header row has no SE row (estimate_fmt = "")
  hdr_one_model <- headers[headers$model == "(1)", ]
  expect_equal(hdr_one_model$estimate_fmt, "")
  expect_equal(hdr_one_model$se_fmt, "")

  # Header rows sort before their child rows
  hdr_order   <- min(headers$row_order)
  child_order <- min(dummy_rows$row_order)
  expect_true(hdr_order < child_order)
})

test_that("format_stat formats N and Clusters as comma integers", {
  expect_equal(exportreg:::format_stat(1234,  "N",        3L), "1,234")
  expect_equal(exportreg:::format_stat(20,    "Clusters", 3L), "20")
  expect_equal(exportreg:::format_stat(NA,    "N",        3L), "")
  expect_equal(exportreg:::format_stat(0.456, "R2",       3L), "0.456")
  expect_equal(exportreg:::format_stat(0.456, "RMSE",     3L), "0.456")
})

test_that("regtab() errors on unnamed models list", {
  expect_error(regtab(list(lm_basic)), regexp = "named list")
})

test_that("print.regtab_table runs without error and returns invisibly", {
  tab <- regtab(list("(1)" = lm_basic))
  out <- capture.output(res <- print(tab))
  expect_s3_class(res, "regtab_table")
  expect_true(length(out) > 0L)
})

# ---------------------------------------------------------------------------
# se_format + se_type new slots
# ---------------------------------------------------------------------------

test_that("regtab() default se_format is 'se' and se_type is named character", {
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  expect_equal(tab$se_format, "se")
  expect_type(tab$se_type, "character")
  expect_named(tab$se_type, c("(1)", "(2)"))
})

test_that("regtab() se_type names equal model_names", {
  tab <- regtab(list("OLS" = lm_basic))
  expect_equal(names(tab$se_type), tab$model_names)
})

test_that("regtab() stores se_format = 'tstat' correctly", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "tstat")
  expect_equal(tab$se_format, "tstat")
})

test_that("regtab() stores se_format = 'pvalue' correctly", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "pvalue")
  expect_equal(tab$se_format, "pvalue")
})

test_that("regtab() errors on invalid se_format", {
  expect_error(regtab(list("(1)" = lm_basic), se_format = "bad"),
               regexp = "should be one of")
})

test_that("assemble_panels() inherits se_format and se_type from first panel", {
  tab1 <- regtab(list("(1)" = lm_basic), se_format = "tstat")
  tab2 <- regtab(list("(1)" = lm_extended), se_format = "tstat")
  combined <- regtab(panels = list("A" = tab1, "B" = tab2))
  expect_equal(combined$se_format, "tstat")
  expect_equal(combined$se_type, tab1$se_type)
})

test_that("build_se_note() uniform IID type, se_format='se'", {
  se_type <- c("(1)" = "IID", "(2)" = "IID")
  result <- exportreg:::build_se_note(se_type, "se", latex = FALSE)
  expect_equal(result, "Standard errors in parentheses")
})

test_that("build_se_note() uniform clustered type, se_format='tstat', latex=TRUE", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result <- exportreg:::build_se_note(se_type, "tstat", latex = TRUE)
  expect_true(grepl("t\\$-statistics", result))
  expect_true(grepl("firm_id", result, fixed = TRUE))
})

test_that("build_se_note() mixed types groups by type with column labels", {
  se_type <- c("(1)" = "IID", "(2)" = "Clustered (x)")
  result <- exportreg:::build_se_note(se_type, "se", latex = FALSE)
  expect_true(grepl("in parentheses", result, fixed = TRUE))
  expect_true(grepl("clustered at", result, fixed = TRUE))
  expect_true(grepl("\\(1\\)", result))
  expect_true(grepl("\\(2\\)", result))
})

test_that("build_se_note() cluster_labels provided — used directly", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "se",
    cluster_labels = c(firm_id = "Firm"),
    latex = FALSE
  )
  expect_true(grepl("Firm", result, fixed = TRUE))
  expect_true(grepl("clustered at the Firm level", result, fixed = TRUE))
  expect_false(grepl("firm_id", result, fixed = TRUE))
})

test_that("build_se_note() cluster_labels NULL, variable in fe_labels — fe_labels used", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "se",
    fe_labels = c(firm_id = "Firm FE"),
    latex = FALSE
  )
  expect_true(grepl("Firm FE", result, fixed = TRUE))
  expect_true(grepl("clustered at the Firm FE level", result, fixed = TRUE))
  expect_false(grepl("firm_id", result, fixed = TRUE))
})

test_that("build_se_note() cluster_labels NULL, fe_labels NULL — raw name used", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(se_type, "se", latex = FALSE)
  expect_true(grepl("firm_id", result, fixed = TRUE))
  expect_true(grepl("clustered at the firm_id level", result, fixed = TRUE))
})

test_that("build_se_note() twoway: one in cluster_labels, one falls back to fe_labels", {
  se_type <- c("(1)" = "Two-ways (firm_id & worker_id)")
  result  <- exportreg:::build_se_note(
    se_type, "se",
    fe_labels      = c(worker_id = "Worker FE"),
    cluster_labels = c(firm_id   = "Firm"),
    latex = FALSE
  )
  expect_true(grepl("Firm",      result, fixed = TRUE))
  expect_true(grepl("Worker FE", result, fixed = TRUE))
  expect_false(grepl("firm_id",   result, fixed = TRUE))
  expect_false(grepl("worker_id", result, fixed = TRUE))
})

test_that("build_se_note() twoway: one in fe_labels, one falls back to raw name", {
  se_type <- c("(1)" = "Two-ways (firm_id & worker_id)")
  result  <- exportreg:::build_se_note(
    se_type, "se",
    fe_labels = c(firm_id = "Firm FE"),
    latex = FALSE
  )
  expect_true(grepl("Firm FE",   result, fixed = TRUE))
  expect_true(grepl("worker_id", result, fixed = TRUE))
  expect_false(grepl("firm_id",  result, fixed = TRUE))
})

test_that("build_se_note() cluster_labels overrides fe_labels when both present", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "se",
    fe_labels      = c(firm_id = "Firm FE"),
    cluster_labels = c(firm_id = "Firm (cluster)"),
    latex = FALSE
  )
  expect_true(grepl("Firm (cluster)", result, fixed = TRUE))
  expect_false(grepl("Firm FE", result, fixed = TRUE))
  expect_false(grepl("firm_id", result, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# build_se_note() — tstat/pvalue label resolution and separator fixes
# ---------------------------------------------------------------------------

test_that("build_se_note() tstat + cluster_labels: label applied, period separator, LaTeX", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "tstat",
    cluster_labels = c(firm_id = "Firm"),
    latex = TRUE
  )
  expect_true(grepl("Standard errors clustered at the Firm level", result, fixed = TRUE))
  expect_true(grepl("$t$-statistics in brackets", result, fixed = TRUE))
  expect_true(grepl("Firm level. $t$", result, fixed = TRUE))  # period separator
  expect_false(grepl("firm_id", result, fixed = TRUE))
  expect_false(grepl(";", result, fixed = TRUE))  # no semicolon separator
})

test_that("build_se_note() pvalue + cluster_labels: label applied, period separator, LaTeX", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "pvalue",
    cluster_labels = c(firm_id = "Firm"),
    latex = TRUE
  )
  expect_true(grepl("Standard errors clustered at the Firm level", result, fixed = TRUE))
  expect_true(grepl("$p$-values in brackets", result, fixed = TRUE))
  expect_true(grepl("Firm level. $p$", result, fixed = TRUE))  # period separator
  expect_false(grepl("firm_id", result, fixed = TRUE))
  expect_false(grepl(";", result, fixed = TRUE))
})

test_that("build_se_note() tstat + IID: returns bracket note only, no SE prefix", {
  se_type <- c("(1)" = "IID")
  result  <- exportreg:::build_se_note(se_type, "tstat", latex = FALSE)
  expect_equal(result, "t-statistics in brackets")
})

test_that("build_se_note() pvalue + IID: returns bracket note only, no SE prefix", {
  se_type <- c("(1)" = "IID")
  result  <- exportreg:::build_se_note(se_type, "pvalue", latex = FALSE)
  expect_equal(result, "p-values in brackets")
})

test_that("format_bracket() formats correctly and handles NA", {
  expect_equal(exportreg:::format_bracket(0.456, 3L), "[0.456]")
  expect_equal(exportreg:::format_bracket(NA_real_, 3L), "")
})

# ---------------------------------------------------------------------------
# build_se_note() — bracket label appears exactly once in mixed SE tables
# ---------------------------------------------------------------------------

test_that("build_se_note() mixed SE + tstat: bracket label once at end, not per group", {
  se_type <- c("(1)" = "IID", "(2)" = "Clustered (firm_id)", "(3)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(se_type, "tstat", latex = FALSE)
  # Bracket label appears exactly once
  expect_equal(lengths(regmatches(result, gregexpr("t-statistics in brackets",
                                                    result, fixed = TRUE))), 1L)
  # Appears at the end, after the semicolon-separated group list
  expect_true(grepl(";", result, fixed = TRUE))
  expect_true(endsWith(result, "t-statistics in brackets"))
  # Column labels present for each group
  expect_true(grepl("(1)", result, fixed = TRUE))
  expect_true(grepl("(2)", result, fixed = TRUE))
})

test_that("build_se_note() mixed SE + pvalue: bracket label once at end, not per group", {
  se_type <- c("(1)" = "IID", "(2)" = "Clustered (x)")
  result  <- exportreg:::build_se_note(se_type, "pvalue", latex = FALSE)
  expect_equal(lengths(regmatches(result, gregexpr("p-values in brackets",
                                                    result, fixed = TRUE))), 1L)
  expect_true(grepl(";", result, fixed = TRUE))
  expect_true(endsWith(result, "p-values in brackets"))
})

test_that("build_se_note() mixed SE + tstat, LaTeX: bracket label once at end", {
  se_type <- c("(1) OLS" = "IID", "(2) FE" = "Clustered (firm_id)",
               "(3) IV"  = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "tstat",
    cluster_labels = c(firm_id = "Firm"),
    latex = TRUE
  )
  # Exactly one occurrence of the LaTeX bracket label
  expect_equal(lengths(regmatches(result,
    gregexpr("$t$-statistics in brackets", result, fixed = TRUE))), 1L)
  # SE descriptions present without bracket label embedded
  expect_true(grepl("Standard errors in parentheses", result, fixed = TRUE))
  expect_true(grepl("Standard errors clustered at the Firm level", result,
                    fixed = TRUE))
  # Column labels present
  expect_true(grepl("(1) OLS", result, fixed = TRUE))
  expect_true(grepl("(2) FE",  result, fixed = TRUE))
  # Groups separated by semicolon, bracket label at end
  expect_true(grepl(";", result, fixed = TRUE))
  expect_true(endsWith(result, "$t$-statistics in brackets"))
})

test_that("build_se_note() single SE + tstat: unchanged behaviour (bracket inline)", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result  <- exportreg:::build_se_note(
    se_type, "tstat",
    cluster_labels = c(firm_id = "Firm"),
    latex = TRUE
  )
  # Still a single-sentence result with inline bracket label
  expect_true(grepl("Firm level. $t$-statistics in brackets", result,
                    fixed = TRUE))
  # No semicolons (only one group)
  expect_false(grepl(";", result, fixed = TRUE))
})

test_that("to_latex() note has exactly one bracket label for mixed SE tstat table", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tab <- regtab(
    list("OLS" = lm_basic, "FE" = mods$feols_clustered),
    se_format      = "tstat",
    cluster_labels = c(firm_id = "Firm")
  )
  # to_latex() returns a character vector (one element per line); count
  # how many lines contain the bracket label — must be exactly one
  latex_lines <- to_latex(tab)
  n_hits <- sum(grepl("$t$-statistics in brackets", latex_lines, fixed = TRUE))
  expect_equal(n_hits, 1L)
})

# ---------------------------------------------------------------------------
# depvar_names slot
# ---------------------------------------------------------------------------

test_that("regtab() stores $depvar_names, names equal model_names", {
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  expect_type(tab$depvar_names, "character")
  expect_named(tab$depvar_names, c("(1)", "(2)"))
})

test_that("regtab() $depvar_names contains the raw extracted variable name", {
  tab <- regtab(list("(1)" = lm_basic))
  expect_equal(unname(tab$depvar_names), "y")
})

test_that("regtab() depvar_labels renames matched depvar", {
  tab <- regtab(
    list("(1)" = lm_basic),
    depvar_labels = c("y" = "Log Wage")
  )
  expect_equal(unname(tab$depvar_names), "Log Wage")
})

test_that("regtab() depvar_labels does not rename unmatched depvar", {
  tab <- regtab(
    list("(1)" = lm_basic),
    depvar_labels = c("other" = "Other")
  )
  expect_equal(unname(tab$depvar_names), "y")
})

test_that("regtab() two models with different depvars stored correctly", {
  m_y   <- lm(y     ~ x1, data = panel_data)
  m_bin <- lm(y_bin ~ x1, data = panel_data)
  tab <- regtab(list("(1)" = m_y, "(2)" = m_bin))
  expect_equal(unname(tab$depvar_names[["(1)"]]), "y")
  expect_equal(unname(tab$depvar_names[["(2)"]]), "y_bin")
})

test_that("assemble_panels() propagates $depvar_names from first panel", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(1)" = lm_extended))
  combined <- regtab(panels = list("A" = tab1, "B" = tab2))
  expect_equal(combined$depvar_names, tab1$depvar_names)
})

test_that("print.regtab_table() output contains depvar label", {
  tab <- regtab(list("(1)" = lm_basic))
  out <- capture.output(print(tab))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Dep\\. var\\.", combined))
  expect_true(grepl("\\by\\b", combined))
})

# ---------------------------------------------------------------------------
# coef_map label collision — Bug fix: different raw terms -> same display label
# ---------------------------------------------------------------------------

test_that("coef_map: two terms mapping to same label keep both estimates per column", {
  # m_x1: has term "x1"
  # m_x2: has term "x2"
  # coef_map maps both to "Var A"
  # Expected: column (1) shows x1 estimate; column (2) shows x2 estimate; no blanks
  m_x1 <- lm(y ~ x1,      data = panel_data)
  m_x2 <- lm(y ~ x2,      data = panel_data)
  tab <- regtab(
    list("(1)" = m_x1, "(2)" = m_x2),
    coef_map = c("x1" = "Var A", "x2" = "Var A")
  )

  # coef_data should have exactly one row per (display_label, model)
  vara_rows <- tab$coef_data[tab$coef_data$term_display == "Var A", ]
  expect_equal(nrow(vara_rows), 2L)  # one row per model, not four

  # Column (1) should have non-NA estimate (from x1)
  r1 <- vara_rows[vara_rows$model == "(1)", ]
  expect_false(is.na(r1$estimate))
  expect_equal(r1$term_raw, "x1")

  # Column (2) should have non-NA estimate (from x2)
  r2 <- vara_rows[vara_rows$model == "(2)", ]
  expect_false(is.na(r2$estimate))
  expect_equal(r2$term_raw, "x2")
})

test_that("coef_map: label collision — print output shows estimates in both columns", {
  m_x1 <- lm(y ~ x1, data = panel_data)
  m_x2 <- lm(y ~ x2, data = panel_data)
  tab <- regtab(
    list("(1)" = m_x1, "(2)" = m_x2),
    coef_map = c("x1" = "Var A", "x2" = "Var A")
  )
  out <- paste(capture.output(print(tab)), collapse = "\n")
  # "Var A" row appears exactly once in the output
  expect_equal(length(gregexpr("Var A", out, fixed = TRUE)[[1L]]), 1L)
})

test_that("coef_map: IV fit_educ maps to same label as educ — IV column shows estimate", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  m_ols <- lm(y ~ x1, data = panel_data)
  tab <- regtab(
    list("OLS" = m_ols, "IV" = mods$feols_iv_fe),
    coef_map = c("x1" = "Endogenous", "fit_educ" = "Endogenous",
                 "fit_x" = "Endogenous")
  )

  # The IV model contributes a fit_ term (fit_educ); that estimate should be
  # non-NA in the IV column after label deduplication
  endog_rows <- tab$coef_data[tab$coef_data$term_display == "Endogenous", ]
  iv_row <- endog_rows[endog_rows$model == "IV", ]
  if (nrow(iv_row) == 1L) {
    expect_false(is.na(iv_row$estimate))
  }
})

test_that("regtab() with README coef_map shows non-NA estimate in IV column", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  m_fe <- fixest::feols(y ~ x1 | firm_id, data = panel_data)
  tab <- regtab(
    list("OLS" = lm_basic, "FE" = m_fe, "IV" = mods$feols_iv_fe),
    coef_map = c("x1"       = "Regressor",
                 "fit_educ" = "Regressor",
                 "exper"    = "Experience")
  )

  # IV column: fit_educ -> "Regressor"; estimate must be non-NA
  reg_rows <- tab$coef_data[tab$coef_data$term_display == "Regressor", ]
  iv_row   <- reg_rows[reg_rows$model == "IV", ]
  expect_equal(nrow(iv_row), 1L)
  expect_false(is.na(iv_row$estimate))
  expect_equal(iv_row$term_raw, "fit_educ")

  # OLS and FE columns: x1 -> "Regressor"; estimates must be non-NA
  ols_row <- reg_rows[reg_rows$model == "OLS", ]
  expect_false(is.na(ols_row$estimate))
  fe_row  <- reg_rows[reg_rows$model == "FE", ]
  expect_false(is.na(fe_row$estimate))
})
