test_that("to_excel() creates a valid xlsx file", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic, "(2)" = lm_extended))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  res <- to_excel(tab, file = tmp)
  expect_true(file.exists(tmp))
  expect_s3_class(res, "regtab_table")  # returns invisibly
})

test_that("to_excel() written file is a valid workbook with correct sheet", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic), )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  to_excel(tab, file = tmp, sheet = "Results")

  wb <- openxlsx2::wb_load(tmp)
  expect_true("Results" %in% openxlsx2::wb_get_sheet_names(wb))
})

test_that("to_excel() errors when file is missing", {
  tab <- regtab(list("(1)" = lm_basic))
  expect_error(to_excel(tab), regexp = "`file` is required")
})

test_that("to_excel() works with fixest model and FE block", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tab <- regtab(
    list("(1)" = mods$feols_multi_fe),
    fe_labels = c("firm_id" = "Firm FE", "year" = "Year FE")
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))

  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

# ---------------------------------------------------------------------------
# se_format tests
# ---------------------------------------------------------------------------

test_that("to_excel() se_format='tstat' writes without error", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic), se_format = "tstat")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

test_that("to_excel() se_format='pvalue' writes without error", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic), se_format = "pvalue")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

# ---------------------------------------------------------------------------
# Additional coverage: col_groups, raw+se_format, digits override,
# factor_labels, add_rows
# ---------------------------------------------------------------------------

test_that("to_excel() with col_groups writes without error", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(
    list("(1)" = lm_basic, "(2)" = lm_extended, "(3)" = lm_extended),
    col_groups = c("(1)" = "OLS", "(2)" = "OLS", "(3)" = "Extended")
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

test_that("to_excel() raw=TRUE + se_format='tstat' writes without error", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic), se_format = "tstat")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp, raw = TRUE))
  expect_true(file.exists(tmp))
})

test_that("to_excel() raw=TRUE + se_format='pvalue' writes without error", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic), se_format = "pvalue")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp, raw = TRUE))
  expect_true(file.exists(tmp))
})

test_that("to_excel() digits override reformats stat block", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp, digits = 5L))
  expect_true(file.exists(tmp))
})

test_that("to_excel() with factor_labels renders header and indented rows", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(
    list("(1)" = lm_factor),
    factor_labels = c("region" = "Region (ref: Centro)")
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

test_that("to_excel() with add_rows writes the extra rows block", {
  skip_if_not_installed("openxlsx2")
  ar <- data.frame(label = "Controls", "(1)" = "Yes", check.names = FALSE)
  tab <- regtab(list("(1)" = lm_basic), add_rows = ar)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

test_that("to_excel() raw=TRUE writes numeric SE to file", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp, raw = TRUE))
  expect_true(file.exists(tmp))
})

# ---------------------------------------------------------------------------
# depvar row in to_excel()
# ---------------------------------------------------------------------------

test_that("to_excel() with depvar_names writes without error", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic))
  expect_equal(unname(tab$depvar_names), "y")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  expect_true(file.exists(tmp))
})

test_that("to_excel() with depvar_labels writes correct label", {
  skip_if_not_installed("openxlsx2")
  tab <- regtab(list("(1)" = lm_basic), depvar_labels = c("y" = "Log Wage"))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  wb  <- openxlsx2::wb_load(tmp)
  dat <- openxlsx2::wb_read(wb, sheet = 1L, col_names = FALSE)
  # "Log Wage" must appear somewhere in the sheet
  expect_true(any(vapply(dat, function(col) any(col == "Log Wage", na.rm = TRUE),
                         logical(1L))))
})

test_that("to_excel() depvar row appears for two models with different depvars", {
  skip_if_not_installed("openxlsx2")
  m1 <- lm(y     ~ x1, data = panel_data)
  m2 <- lm(y_bin ~ x1, data = panel_data)
  tab <- regtab(list("(1)" = m1, "(2)" = m2))
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  expect_no_error(to_excel(tab, file = tmp))
  wb  <- openxlsx2::wb_load(tmp)
  dat <- openxlsx2::wb_read(wb, sheet = 1L, col_names = FALSE)
  expect_true(any(vapply(dat, function(col) any(col == "y_bin", na.rm = TRUE),
                         logical(1L))))
})
