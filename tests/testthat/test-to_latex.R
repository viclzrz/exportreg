test_that("to_latex() returns character vector invisibly and prints", {
  tab <- regtab(list("(1)" = lm_basic))
  out_lines <- capture.output(res <- to_latex(tab))
  expect_type(res, "character")
  expect_true(length(res) > 0L)
  expect_true(length(out_lines) > 0L)
})

test_that("to_latex() fragment contains tabular environment", {
  tab <- regtab(list("(1)" = lm_basic))
  res <- capture.output(lines <- to_latex(tab))
  expect_true(any(grepl("\\\\begin\\{tabular\\}", lines)))
  expect_true(any(grepl("\\\\end\\{tabular\\}", lines)))
  expect_true(any(grepl("\\\\toprule",    lines)))
  expect_true(any(grepl("\\\\bottomrule", lines)))
  expect_true(any(grepl("\\\\midrule",    lines)))
})

test_that("to_latex() format=full wraps in documentclass", {
  tab <- regtab(list("(1)" = lm_basic))
  res <- capture.output(lines <- to_latex(tab, format = "full"))
  expect_true(any(grepl("\\\\documentclass", lines)))
  expect_true(any(grepl("\\\\usepackage\\{booktabs\\}", lines)))
  expect_true(any(grepl("\\\\begin\\{document\\}", lines)))
  expect_true(any(grepl("\\\\end\\{document\\}",   lines)))
})

test_that("to_latex() uses math-mode stars", {
  # Build a model with a clearly significant coefficient
  set.seed(42)
  df   <- data.frame(y = rnorm(100, mean = 10), x = rnorm(100))
  df$y <- df$y + 5 * df$x
  m    <- lm(y ~ x, data = df)
  tab  <- regtab(list("(1)" = m))

  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  # At least one math-mode star should appear
  expect_true(grepl("\\$\\^\\{\\*+\\}\\$", combined))
})

test_that("to_latex() significance note uses math-mode stars", {
  tab   <- regtab(list("(1)" = lm_basic))
  res   <- capture.output(lines <- to_latex(tab))
  note_line <- lines[grepl("Note", lines)]
  expect_true(length(note_line) > 0L)
  expect_true(any(grepl("\\$\\^\\{\\*\\}\\$", note_line)))
})

test_that("to_latex() writes file when file= is supplied", {
  tab  <- regtab(list("(1)" = lm_basic))
  tmp  <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp))
  capture.output(to_latex(tab, file = tmp))
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("tabular", content)))
})

test_that("to_latex() appends custom note", {
  tab <- regtab(list("(1)" = lm_basic))
  res <- capture.output(lines <- to_latex(tab, note = "Robust SEs in parentheses."))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Robust SEs in parentheses", combined))
})

test_that("to_latex() FE block appears when fixest model used", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tab <- regtab(
    list("(1)" = mods$feols_multi_fe),
    fe_labels = c("firm_id" = "Firm FE", "year" = "Year FE")
  )
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Firm FE", combined))
  expect_true(grepl("Year FE", combined))
  expect_true(grepl("Yes", combined))
})
