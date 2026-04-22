test_that("tidy_model.lm returns correct schema", {
  tm <- exportreg:::tidy_model(lm_basic)

  # coefs
  expect_s3_class(tm$coefs, "data.frame")
  expect_named(tm$coefs, c("term", "estimate", "std.error", "p.value"))
  expect_true(all(c("x1", "x2") %in% tm$coefs$term))
  expect_type(tm$coefs$estimate,  "double")
  expect_type(tm$coefs$std.error, "double")
  expect_type(tm$coefs$p.value,   "double")

  # glance schema
  expect_s3_class(tm$glance, "data.frame")
  expect_named(tm$glance,
    c("nobs", "r2", "r2_within", "rmse", "nobs_clusters", "kp_fstat"))
  expect_equal(nrow(tm$glance), 1L)
  expect_type(tm$glance$nobs, "integer")
  expect_equal(tm$glance$nobs, 16L)

  # R² is available for lm
  expect_false(is.na(tm$glance$r2))
  expect_true(tm$glance$r2 >= 0 && tm$glance$r2 <= 1)

  # within R², clusters, KP stat should be NA for plain lm
  expect_true(is.na(tm$glance$r2_within))
  expect_true(is.na(tm$glance$nobs_clusters))
  expect_true(is.na(tm$glance$kp_fstat))

  # fe_vars is NULL for lm
  expect_null(tm$fe_vars)
})

test_that("tidy_model.lm with different regressors returns correct schema", {
  tm <- exportreg:::tidy_model(lm_extended)
  expect_equal(tm$coefs$term, c("(Intercept)", "x1"))
  expect_equal(tm$glance$nobs, 16L)
  expect_null(tm$fe_vars)
})

test_that("tidy_model.glm returns correct schema", {
  tm <- exportreg:::tidy_model(glm_basic)

  expect_s3_class(tm$coefs, "data.frame")
  expect_named(tm$coefs, c("term", "estimate", "std.error", "p.value"))
  expect_true(all(c("x1", "x2") %in% tm$coefs$term))

  expect_named(tm$glance,
    c("nobs", "r2", "r2_within", "rmse", "nobs_clusters", "kp_fstat"))
  expect_equal(nrow(tm$glance), 1L)
  expect_type(tm$glance$nobs, "integer")
  expect_equal(tm$glance$nobs, 16L)

  # glm: r2 left as NA (not meaningful across all families)
  expect_true(is.na(tm$glance$r2))
  expect_true(is.na(tm$glance$r2_within))
  expect_true(is.na(tm$glance$nobs_clusters))
  expect_true(is.na(tm$glance$kp_fstat))
  expect_null(tm$fe_vars)
})

test_that("tidy_model errors on unsupported class", {
  fake_model <- structure(list(), class = "unsupported_xyz")
  expect_error(
    exportreg:::tidy_model(fake_model),
    regexp = "unsupported model class 'unsupported_xyz'"
  )
})

test_that("tidy_model.fixest returns correct schema with multiple FEs", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tm <- exportreg:::tidy_model(mods$feols_multi_fe)

  expect_named(tm$coefs, c("term", "estimate", "std.error", "p.value"))
  expect_true(all(c("x1", "x2") %in% tm$coefs$term))

  expect_named(tm$glance,
    c("nobs", "r2", "r2_within", "rmse", "nobs_clusters", "kp_fstat"))
  expect_equal(nrow(tm$glance), 1L)
  expect_equal(tm$glance$nobs, 16L)

  # within R² should be available with FEs
  expect_false(is.na(tm$glance$r2_within))

  # fe_vars
  expect_type(tm$fe_vars, "character")
  expect_true("firm_id" %in% tm$fe_vars)
  expect_true("year"    %in% tm$fe_vars)
})

test_that("tidy_model.fixest one-FE model has correct fe_vars", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tm <- exportreg:::tidy_model(mods$feols_one_fe)
  expect_equal(tm$fe_vars, "firm_id")
})

test_that("tidy_model.fixest no-FE model has non-NULL fe_vars of length 0", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tm <- exportreg:::tidy_model(mods$feols_no_fe)
  # fixest$fixef_vars is NULL when there are no FEs
  expect_true(is.null(tm$fe_vars) || length(tm$fe_vars) == 0L)
})

test_that("mixed list of fixest and lm models both extract correctly", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))

  tm_fixest <- exportreg:::tidy_model(mods$feols_multi_fe)
  tm_lm     <- exportreg:::tidy_model(lm_basic)

  # Both return the same schema
  for (tm in list(tm_fixest, tm_lm)) {
    expect_named(tm$coefs, c("term", "estimate", "std.error", "p.value"))
    expect_named(tm$glance,
      c("nobs", "r2", "r2_within", "rmse", "nobs_clusters", "kp_fstat"))
    expect_equal(nrow(tm$glance), 1L)
  }
})

test_that("models with non-overlapping regressors are both extracted", {
  tm1 <- exportreg:::tidy_model(lm_basic)     # has x1, x2
  tm2 <- exportreg:::tidy_model(lm_extended)  # has x1 only

  expect_true("x2" %in% tm1$coefs$term)
  expect_false("x2" %in% tm2$coefs$term)
  expect_true("x1" %in% tm1$coefs$term)
  expect_true("x1" %in% tm2$coefs$term)
})

# ---------------------------------------------------------------------------
# se_type slot (4th slot)
# ---------------------------------------------------------------------------

test_that("tidy_model.lm returns se_type == 'IID'", {
  tm <- exportreg:::tidy_model(lm_basic)
  expect_type(tm$se_type, "character")
  expect_equal(tm$se_type, "IID")
})

test_that("tidy_model.glm returns se_type == 'IID'", {
  tm <- exportreg:::tidy_model(glm_basic)
  expect_type(tm$se_type, "character")
  expect_equal(tm$se_type, "IID")
})

test_that("tidy_model.fixest IID model returns se_type == 'IID'", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tm <- exportreg:::tidy_model(mods$feols_no_fe)
  expect_type(tm$se_type, "character")
  expect_equal(length(tm$se_type), 1L)
  # Without explicit clustering, fixest uses IID by default
  expect_equal(tm$se_type, "IID")
})

test_that("tidy_model.fixest clustered model returns se_type containing 'Clustered'", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tm <- exportreg:::tidy_model(mods$feols_clustered)
  expect_type(tm$se_type, "character")
  expect_true(grepl("Clustered", tm$se_type))
})

test_that("tidy_model.fixest IV model enters KP branch and populates kp_fstat", {
  skip_if_not_installed("fixest")
  mods <- make_fixest_models()
  skip_if(is.null(mods))
  tm <- exportreg:::tidy_model(mods$feols_iv)
  expect_type(tm$glance$kp_fstat, "double")
  expect_false(is.na(tm$glance$kp_fstat))
  expect_true(tm$glance$kp_fstat > 0)
})
