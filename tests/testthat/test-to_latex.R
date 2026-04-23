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

test_that("to_latex() significance note uses math-mode p-value notation", {
  tab   <- regtab(list("(1)" = lm_basic))
  res   <- capture.output(lines <- to_latex(tab))
  note_line <- lines[grepl("Note", lines)]
  expect_true(length(note_line) > 0L)
  # New format: "*** $p<0.01$, ** $p<0.05$, * $p<0.1$"
  expect_true(any(grepl("\\$p<0\\.01\\$", note_line)))
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

# ---------------------------------------------------------------------------
# se_format tests
# ---------------------------------------------------------------------------

test_that("to_latex() se_format='se' note contains 'in parentheses'", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "se")
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("in parentheses", combined, fixed = TRUE))
})

test_that("to_latex() se_format='se' second-row cells contain parentheses", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "se")
  res <- capture.output(lines <- to_latex(tab))
  # At least one SE row must contain "("
  expect_true(any(grepl("\\(", lines)))
})

test_that("to_latex() se_format='tstat' second-row cells contain brackets", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "tstat")
  res <- capture.output(lines <- to_latex(tab))
  expect_true(any(grepl("\\[", lines)))
})

test_that("to_latex() se_format='tstat' note contains 't-statistics'", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "tstat")
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("t-statistics|t\\$-statistics", combined))
})

test_that("to_latex() se_format='pvalue' second-row cells contain brackets", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "pvalue")
  res <- capture.output(lines <- to_latex(tab))
  expect_true(any(grepl("\\[", lines)))
})

test_that("to_latex() se_format='pvalue' note contains p-values mention", {
  tab <- regtab(list("(1)" = lm_basic), se_format = "pvalue")
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  # latex = TRUE → "$p$-values in brackets"
  expect_true(grepl("p.*values", combined))
})

test_that("to_latex() significance line contains math-mode p-values", {
  tab <- regtab(list("(1)" = lm_basic))
  res <- capture.output(lines <- to_latex(tab))
  note_line <- lines[grepl("Note", lines)]
  expect_true(length(note_line) > 0L)
  expect_true(any(grepl("\\$p<0\\.01\\$", note_line)))
})

# ---------------------------------------------------------------------------
# latex_stat_label branch coverage
# ---------------------------------------------------------------------------

test_that("latex_stat_label() returns 'Clusters' for Clusters branch", {
  expect_equal(exportreg:::latex_stat_label("Clusters"), "Clusters")
})

test_that("latex_stat_label() returns KP F-stat markup for KP branch", {
  expect_equal(exportreg:::latex_stat_label("KP F-stat"), "KP $F$-stat")
})

test_that("latex_stat_label() passes through unknown stat names", {
  expect_equal(exportreg:::latex_stat_label("custom stat"), "custom stat")
})

test_that("to_latex() Clusters row appears for clustered fixest model", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tab <- regtab(list("(1)" = mods$feols_clustered))
  # Clusters stat row must be in stat_data
  expect_true("Clusters" %in% tab$stat_data$stat)
  # to_latex() must include "Clusters" in output
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Clusters", combined))
})

test_that("to_latex() KP F-stat row appears for IV fixest model", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tab <- regtab(list("(1)" = mods$feols_iv))
  expect_false(is.na(tab$stat_data$value_raw[tab$stat_data$stat == "KP F-stat"]))
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("KP", combined))
})

# ---------------------------------------------------------------------------
# depvar row in to_latex()
# ---------------------------------------------------------------------------

test_that("to_latex() output contains 'Dep. var.' row", {
  tab <- regtab(list("(1)" = lm_basic))
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Dep", combined))
})

test_that("to_latex() depvar label appears in the tabular body", {
  tab <- regtab(list("(1)" = lm_basic))
  res <- capture.output(lines <- to_latex(tab))
  # The depvar line is between \midrule and the first coefficient row
  midrule_idx <- which(lines == "\\midrule")
  expect_true(length(midrule_idx) >= 1L)
  # Line immediately after first \midrule must contain the depvar
  depvar_line <- lines[[midrule_idx[[1L]] + 1L]]
  expect_true(grepl("y", depvar_line))
})

test_that("to_latex() depvar label is LaTeX-escaped", {
  # Use a model with a transformed depvar containing special chars
  df <- data.frame(x = 1:16, y = log(panel_data$y + 1))
  m  <- lm(log(y + 1) ~ x, data = df)
  tab <- regtab(list("(1)" = m))
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  # log(y + 1) contains ( and ) — latex_escape keeps those, but $ would be escaped
  expect_true(grepl("log", combined))
})

test_that("to_latex() depvar_labels renaming appears in output", {
  tab <- regtab(list("(1)" = lm_basic), depvar_labels = c("y" = "Log Wage"))
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  expect_true(grepl("Log Wage", combined))
})

test_that("to_latex() two models with different depvars both shown", {
  m1 <- lm(y     ~ x1, data = panel_data)
  m2 <- lm(y_bin ~ x1, data = panel_data)
  tab <- regtab(list("(1)" = m1, "(2)" = m2))
  res <- capture.output(lines <- to_latex(tab))
  combined <- paste(lines, collapse = "\n")
  # y_bin is LaTeX-escaped to y\_bin
  expect_true(grepl("y\\\\_bin|y_bin", combined))
})

# ---------------------------------------------------------------------------
# SE note construction — regression tests for Bug 1 and Bug 2 fixes
# ---------------------------------------------------------------------------

test_that("to_latex() note contains constructed cluster string with cluster_labels", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tab <- regtab(
    list("(1)" = mods$feols_clustered),
    cluster_labels = c("firm_id" = "Firm")
  )
  res <- capture.output(lines <- to_latex(tab))
  note_line <- paste(lines[grepl("Note", lines)], collapse = "\n")
  expect_true(
    grepl("Standard errors clustered at the Firm level in parentheses",
          note_line, fixed = TRUE)
  )
})

test_that("to_latex() note does not contain 'SE:' or raw var names when cluster_labels provided", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tab <- regtab(
    list("(1)" = mods$feols_clustered),
    cluster_labels = c("firm_id" = "Firm")
  )
  res <- capture.output(lines <- to_latex(tab))
  note_line <- paste(lines[grepl("Note", lines)], collapse = "\n")
  expect_false(grepl("SE:", note_line, fixed = TRUE))
  expect_false(grepl("firm_id", note_line, fixed = TRUE))
})
