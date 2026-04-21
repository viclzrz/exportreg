# Tests for assemble_panels, detect_i_vars, col_groups, and add_rows display

# ============================================================================
# assemble_panels
# ============================================================================

test_that("regtab(panels=) returns a regtab_table with correct model_names", {
  tab1 <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  tab2 <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  result <- regtab(panels = list("Outcome A" = tab1, "Outcome B" = tab2))

  expect_s3_class(result, "regtab_table")
  expect_equal(result$model_names, c("(1)", "(2)"))
})

test_that("assemble_panels inserts Panel A / Panel B header rows", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(1)" = lm_extended))
  result <- regtab(panels = list("Alpha" = tab1, "Beta" = tab2))

  headers <- result$coef_data[result$coef_data$is_factor_header, ]
  labels  <- unique(headers$term_display)
  expect_true(any(grepl("Panel A.*Alpha", labels)))
  expect_true(any(grepl("Panel B.*Beta",  labels)))
})

test_that("assemble_panels header rows have is_factor_header = TRUE and empty estimates", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(1)" = lm_extended))
  result <- regtab(panels = list("P1" = tab1, "P2" = tab2))

  headers <- result$coef_data[result$coef_data$is_factor_header &
                                grepl("^Panel", result$coef_data$term_display), ]
  expect_true(nrow(headers) > 0L)
  expect_true(all(is.na(headers$estimate)))
  expect_true(all(headers$estimate_fmt == ""))
  expect_true(all(headers$se_fmt == ""))
})

test_that("assemble_panels offsets row_order: panel 2 rows exceed panel 1 rows", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(1)" = lm_extended))
  result <- regtab(panels = list("P1" = tab1, "P2" = tab2))

  non_hdr <- result$coef_data[!result$coef_data$is_factor_header, ]
  # Panel 2 rows are offset by 1e6; panel 1 rows stay in [1, n]
  expect_true(max(non_hdr$row_order) >= 1e6)
  # Panel 1 rows are below 1e6
  expect_true(min(non_hdr$row_order[non_hdr$row_order < 1e6]) >= 1L)
})

test_that("assemble_panels concatenates stat_data from all panels", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(1)" = lm_extended))
  result <- regtab(panels = list("P1" = tab1, "P2" = tab2))

  n_stat <- result$stat_data[result$stat_data$stat == "N" &
                               result$stat_data$model == "(1)", ]
  # Two panels → two N rows for model "(1)"
  expect_equal(nrow(n_stat), 2L)
})

test_that("assemble_panels inherits digits and stars from first panel", {
  tab1 <- regtab(list("(1)" = lm_basic), digits = 4L,
                 stars = c(0.05, 0.01))
  tab2 <- regtab(list("(1)" = lm_extended))
  result <- regtab(panels = list("P1" = tab1, "P2" = tab2))

  expect_equal(result$digits, 4L)
  expect_equal(result$stars,  c(0.01, 0.05))
})

test_that("assemble_panels passes col_groups through", {
  tab1 <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  tab2 <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  cg <- c("(1)" = "Group A", "(2)" = "Group B")
  result <- regtab(panels = list("P1" = tab1, "P2" = tab2),
                   col_groups = cg)
  expect_equal(result$col_groups, cg)
})

test_that("assemble_panels errors when model_names differ across panels", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(X)" = lm_extended))
  expect_error(
    regtab(panels = list("P1" = tab1, "P2" = tab2)),
    regexp = "identical"
  )
})

test_that("assemble_panels errors on unnamed panels list", {
  tab1 <- regtab(list("(1)" = lm_basic))
  tab2 <- regtab(list("(1)" = lm_extended))
  expect_error(
    exportreg:::assemble_panels(list(tab1, tab2), col_groups = NULL),
    regexp = "named list"
  )
})

test_that("print.regtab_table runs without error on assembled panels", {
  tab1 <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  tab2 <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  result <- regtab(panels = list("Outcome A" = tab1, "Outcome B" = tab2))
  out <- capture.output(res <- print(result))
  expect_s3_class(res, "regtab_table")
  expect_true(any(grepl("Panel A", out)))
  expect_true(any(grepl("Panel B", out)))
})

# ============================================================================
# detect_i_vars
# ============================================================================

test_that("detect_i_vars returns character(0) for plain lm model", {
  result <- exportreg:::detect_i_vars(lm_basic)
  expect_type(result, "character")
  expect_length(result, 0L)
})

test_that("detect_i_vars returns character(0) for fixest model without i()", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  result <- exportreg:::detect_i_vars(mods$feols_multi_fe)
  expect_type(result, "character")
  expect_length(result, 0L)
})

test_that("detect_i_vars extracts stem from fixest model with i() syntax", {
  skip_if_not_installed("fixest")
  pd2 <- panel_data
  pd2$year_f <- factor(pd2$year)
  m_i <- tryCatch(
    fixest::feols(y ~ x1 + i(year_f) | firm_id, data = pd2),
    error = function(e) NULL
  )
  skip_if(is.null(m_i))

  result <- exportreg:::detect_i_vars(m_i)
  expect_type(result, "character")
  expect_true("year_f" %in% result)
})

test_that("detect_i_vars handles model with broken formula gracefully", {
  # Fake model whose formula() call throws
  bad_model <- structure(list(), class = "fixest")
  result <- exportreg:::detect_i_vars(bad_model)
  expect_type(result, "character")
  expect_length(result, 0L)
})

# ============================================================================
# col_groups display in to_latex()
# ============================================================================

test_that("to_latex() emits \\multicolumn and \\cmidrule for col_groups", {
  tab <- regtab(
    list("(1)" = lm_basic, "(2)" = lm_extended),
    col_groups = c("(1)" = "OLS", "(2)" = "OLS")
  )
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("\\\\multicolumn", combined))
  expect_true(grepl("\\\\cmidrule",   combined))
  expect_true(grepl("OLS",            combined))
})

test_that("to_latex() renders two distinct col_groups labels", {
  tab <- regtab(
    list("(1)" = lm_basic, "(2)" = lm_extended),
    col_groups = c("(1)" = "Group A", "(2)" = "Group B")
  )
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Group A", combined))
  expect_true(grepl("Group B", combined))
  # Both groups → two \cmidrule entries in one line
  cmidrule_line <- lines[grepl("cmidrule", lines)]
  expect_equal(
    length(gregexpr("cmidrule", cmidrule_line, fixed = TRUE)[[1L]]),
    2L
  )
})

# ============================================================================
# add_rows display in print.regtab_table() and to_latex()
# ============================================================================

test_that("print.regtab_table() renders add_rows labels and values", {
  ar <- data.frame(
    label = c("Control variables", "Robust SEs"),
    "(1)" = c("Yes", "No"),
    "(2)" = c("Yes", "Yes"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended),
                add_rows = ar)
  out <- capture.output(print(tab))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Control variables", combined))
  expect_true(grepl("Robust SEs",       combined))
  expect_true(grepl("Yes",              combined))
  expect_true(grepl("No",               combined))
})

test_that("to_latex() renders add_rows in the FE/add_rows block", {
  ar <- data.frame(
    label = "Bandwidth",
    "(1)" = "20km",
    "(2)" = "50km",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended),
                add_rows = ar)
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Bandwidth", combined))
  expect_true(grepl("20km",      combined))
  expect_true(grepl("50km",      combined))
})

test_that("add_rows with NA values renders as empty string in print", {
  ar <- data.frame(
    label = "Optional row",
    "(1)" = "Yes",
    "(2)" = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended),
                add_rows = ar)
  out <- capture.output(print(tab))
  expect_true(any(grepl("Optional row", out)))
})

test_that("add_rows with NA values renders as empty string in to_latex", {
  ar <- data.frame(
    label = "Optional row",
    "(1)" = "Yes",
    "(2)" = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended),
                add_rows = ar)
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Optional row", combined))
  # NA renders as empty — "Yes" should appear, no literal "NA"
  expect_true(grepl("Yes", combined))
  expect_false(grepl(" NA ", combined))
})
