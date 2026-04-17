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
