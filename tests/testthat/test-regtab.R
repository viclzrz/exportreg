test_that("regtab() returns a regtab_table with correct structure", {
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))

  expect_s3_class(tab, "regtab_table")
  expect_named(tab, c("coef_data", "fe_data", "add_rows", "stat_data",
                       "model_names", "col_groups", "digits", "stars",
                       "se_format", "se_type", "depvar_names", "call"))
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
  expect_equal(result, "SE in parentheses. SE: IID")
})

test_that("build_se_note() uniform clustered type, se_format='tstat', latex=TRUE", {
  se_type <- c("(1)" = "Clustered (firm_id)")
  result <- exportreg:::build_se_note(se_type, "tstat", latex = TRUE)
  expect_true(grepl("t\\$-statistics", result))
  expect_true(grepl("Clustered \\(firm_id\\)", result))
})

test_that("build_se_note() mixed types groups by type with column labels", {
  se_type <- c("(1)" = "IID", "(2)" = "Clustered (x)")
  result <- exportreg:::build_se_note(se_type, "se", latex = FALSE)
  expect_true(grepl("IID", result))
  expect_true(grepl("Clustered", result))
  expect_true(grepl("\\(1\\)", result))
  expect_true(grepl("\\(2\\)", result))
})

test_that("format_bracket() formats correctly and handles NA", {
  expect_equal(exportreg:::format_bracket(0.456, 3L), "[0.456]")
  expect_equal(exportreg:::format_bracket(NA_real_, 3L), "")
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
