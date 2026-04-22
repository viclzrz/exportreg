# Shared model fixtures for exportreg tests
# These are loaded automatically by testthat before any test file.

# Panel data: x2 varies within firm AND within year to avoid collinearity
panel_data <- data.frame(
  y       = c(1.2, 2.3, 3.1, 4.0, 2.5, 3.3, 1.8, 2.9,
              3.5, 1.9, 2.7, 4.1, 2.2, 3.8, 1.5, 3.0),
  x1      = c(0.5, 1.0, 1.5, 2.0, 0.8, 1.2, 0.3, 1.7,
              0.9, 1.4, 0.6, 1.8, 1.1, 0.7, 1.3, 2.1),
  x2      = c(0,   1,   0,   1,   1,   0,   1,   0,
              1,   0,   1,   0,   0,   1,   0,   1),
  firm_id = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2),
  year    = c(2000, 2001, 2002, 2003, 2000, 2001, 2002, 2003,
              2000, 2001, 2002, 2003, 2000, 2001, 2002, 2003),
  stringsAsFactors = FALSE
)

# lm model — no FEs
lm_basic <- lm(y ~ x1 + x2, data = panel_data)

# lm model — subset of regressors (non-overlapping: x2 absent)
lm_extended <- lm(y ~ x1, data = panel_data)

# glm model — logistic regression on binary outcome
panel_data$y_bin <- as.integer(panel_data$y > median(panel_data$y))
glm_basic <- glm(y_bin ~ x1 + x2, data = panel_data, family = binomial())

# lm model with factor variable — for testing factor_labels grouping
panel_data$region <- factor(
  rep(c("Norte", "Sur", "Centro", "Este"), 4),
  levels = c("Centro", "Norte", "Sur", "Este")
)
lm_factor <- lm(y ~ x1 + region, data = panel_data)

# fixest models — require fixest package (loaded conditionally in tests)
make_fixest_models <- function() {
  if (!requireNamespace("fixest", quietly = TRUE)) return(NULL)
  list(
    feols_multi_fe = fixest::feols(
      y ~ x1 + x2 | firm_id + year,
      data = panel_data
    ),
    feols_one_fe = fixest::feols(
      y ~ x1 | firm_id,
      data = panel_data
    ),
    feols_no_fe = fixest::feols(
      y ~ x1 + x2,
      data = panel_data
    ),
    feols_clustered = fixest::feols(
      y ~ x1 + x2 | firm_id,
      data = panel_data,
      cluster = ~firm_id
    ),
    feols_iv = {
      # Synthetic IV dataset: z is a strong instrument for x_endog
      set.seed(42)
      n <- 200
      z       <- rnorm(n)
      u       <- rnorm(n)
      x_endog <- 0.8 * z + 0.2 * u
      y_iv    <- 1 + 2 * x_endog + u
      iv_df   <- data.frame(y = y_iv, x = x_endog, z = z)
      fixest::feols(y ~ 1 | x ~ z, data = iv_df)
    }
  )
}
