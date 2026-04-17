# Internal model extractors for exportreg
# None of these are exported.

# ---------------------------------------------------------------------------
# Dispatcher
# ---------------------------------------------------------------------------

#' Extract tidy coefficients and fit statistics from a model object
#'
#' Dispatches on the first element of `class(model)`. Supported classes:
#' `fixest`, `lm`, `glm`, `felm`, `ivreg`.
#'
#' @param model a supported model object
#' @return A named list with three slots:
#'   * `coefs` — data.frame with columns `term`, `estimate`, `std.error`,
#'     `p.value`
#'   * `glance` — single-row data.frame with columns `nobs`, `r2`,
#'     `r2_within`, `rmse`, `nobs_clusters`, `kp_fstat`
#'   * `fe_vars` — character vector of FE variable names (fixest only),
#'     or `NULL`
#' @noRd
tidy_model <- function(model) {
  cls <- class(model)[[1L]]
  switch(cls,
    fixest = tidy_model_fixest(model),
    lm     = tidy_model_lm(model),
    glm    = tidy_model_glm(model),
    felm   = tidy_model_felm(model),
    ivreg  = tidy_model_ivreg(model),
    stop(
      "exportreg: unsupported model class '", cls, "'. ",
      "Supported: fixest, lm, glm, felm, ivreg.",
      call. = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# Empty glance template — guarantees all six columns are always present
# ---------------------------------------------------------------------------

empty_glance <- function() {
  data.frame(
    nobs          = NA_integer_,
    r2            = NA_real_,
    r2_within     = NA_real_,
    rmse          = NA_real_,
    nobs_clusters = NA_integer_,
    kp_fstat      = NA_real_,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# fixest extractor (custom — does not use broom)
# ---------------------------------------------------------------------------

tidy_model_fixest <- function(model) {
  # Coefficients
  ests <- stats::coef(model)
  vcv  <- stats::vcov(model)
  ses  <- sqrt(diag(vcv))

  # p-values: use model's stored t-statistics when available, else compute
  # from the diagonal of vcov
  if (!is.null(model$se)) {
    # fixest stores SEs in model$se; t-stats = est / se
    tstat <- ests / model$se
  } else {
    tstat <- ests / ses
  }
  # degrees of freedom: use nobs - nparams as conservative fallback
  df_resid <- tryCatch(
    stats::df.residual(model),
    error = function(e) model$nobs - length(ests)
  )
  df_resid <- if (is.null(df_resid) || is.na(df_resid)) {
    model$nobs - length(ests)
  } else {
    df_resid
  }
  pvals <- 2 * stats::pt(abs(tstat), df = df_resid, lower.tail = FALSE)

  coefs <- data.frame(
    term      = names(ests),
    estimate  = unname(ests),
    std.error = unname(ses),
    p.value   = unname(pvals),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Glance
  gl <- empty_glance()
  gl$nobs <- as.integer(model$nobs)

  # R² via fixest::fitstat (model$r2 is NULL in current fixest versions)
  r2_val <- tryCatch(fixest::fitstat(model, "r2")$r2, error = function(e) NULL)
  if (!is.null(r2_val)) gl$r2 <- as.numeric(r2_val)

  wr2_val <- tryCatch(fixest::fitstat(model, "wr2")$wr2, error = function(e) NULL)
  if (!is.null(wr2_val)) gl$r2_within <- as.numeric(wr2_val)

  # RMSE = sqrt(SSR / N)
  ssr <- tryCatch(model$ssr, error = function(e) NULL)
  if (!is.null(ssr) && !is.na(ssr) && gl$nobs > 0L) {
    gl$rmse <- sqrt(ssr / gl$nobs)
  }

  # Clusters: fixest stores the number of clusters in attr(vcov, "G")
  cl_g <- tryCatch(attr(stats::vcov(model), "G"), error = function(e) NULL)
  if (!is.null(cl_g) && length(cl_g) > 0L) {
    gl$nobs_clusters <- as.integer(cl_g[[1L]])
  }

  # KP F-stat (IV models only)
  iv_first <- tryCatch(model$iv_first_stage, error = function(e) NULL)
  if (!is.null(iv_first)) {
    # iv_first_stage is a list of first-stage fixest objects; extract KP stat
    kp <- tryCatch(
      {
        # fixest stores the KP stat in the $irls_iter or as model stat
        # The Kleibergen-Paap F is accessible via summary(model)$iv_stat
        sm <- summary(model)
        sm$iv_stat[["KP F-stat"]]
      },
      error = function(e) NULL
    )
    if (!is.null(kp) && !is.na(kp)) gl$kp_fstat <- as.numeric(kp)
  }

  # FE variables
  fe_vars <- tryCatch(model$fixef_vars, error = function(e) NULL)

  list(coefs = coefs, glance = gl, fe_vars = fe_vars)
}

# ---------------------------------------------------------------------------
# lm extractor
# ---------------------------------------------------------------------------

tidy_model_lm <- function(model) {
  coefs <- broom::tidy(model)[, c("term", "estimate", "std.error", "p.value")]

  bg   <- broom::glance(model)
  gl   <- empty_glance()
  gl$nobs <- as.integer(stats::nobs(model))
  if ("r.squared" %in% names(bg))  gl$r2   <- bg$r.squared
  if ("sigma" %in% names(bg))      gl$rmse <- bg$sigma

  list(coefs = coefs, glance = gl, fe_vars = NULL)
}

# ---------------------------------------------------------------------------
# glm extractor
# ---------------------------------------------------------------------------

tidy_model_glm <- function(model) {
  coefs <- broom::tidy(model)[, c("term", "estimate", "std.error", "p.value")]

  bg   <- broom::glance(model)
  gl   <- empty_glance()
  gl$nobs <- as.integer(stats::nobs(model))
  # glm uses deviance-based pseudo-R²; leave r2 as NA (not meaningful for all families)

  list(coefs = coefs, glance = gl, fe_vars = NULL)
}

# ---------------------------------------------------------------------------
# felm extractor (lfe package)
# ---------------------------------------------------------------------------

tidy_model_felm <- function(model) {
  coefs <- broom::tidy(model)[, c("term", "estimate", "std.error", "p.value")]

  gl <- empty_glance()
  gl$nobs <- as.integer(model$N)

  r2_val <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)
  if (!is.null(r2_val) && !is.na(r2_val)) gl$r2 <- r2_val

  r2w <- tryCatch(summary(model)$r2within, error = function(e) NA_real_)
  if (!is.null(r2w) && !is.na(r2w)) gl$r2_within <- r2w

  # Clusters
  n_cl <- tryCatch(
    length(model$clustervar[[1L]]),
    error = function(e) NULL
  )
  if (!is.null(n_cl)) gl$nobs_clusters <- as.integer(n_cl)

  list(coefs = coefs, glance = gl, fe_vars = NULL)
}

# ---------------------------------------------------------------------------
# ivreg extractor (AER package)
# ---------------------------------------------------------------------------

tidy_model_ivreg <- function(model) {
  coefs <- broom::tidy(model)[, c("term", "estimate", "std.error", "p.value")]

  bg  <- broom::glance(model)
  gl  <- empty_glance()
  gl$nobs <- as.integer(stats::nobs(model))
  if ("r.squared" %in% names(bg)) gl$r2   <- bg$r.squared
  if ("sigma" %in% names(bg))     gl$rmse <- bg$sigma

  list(coefs = coefs, glance = gl, fe_vars = NULL)
}
