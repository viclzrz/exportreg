# exportreg — Project Context for Claude

## Package identity

| Item | Value |
|---|---|
| Package name | `exportreg` |
| Main function | `regtab()` |
| S3 class | `regtab_table` |
| Output functions | `to_excel()`, `to_latex()` |
| Print method | `print.regtab_table()` |
| Internal extractor | `tidy_model()` (unexported) |
| Primary model class | `fixest` (`feols`, `fepois`, `feiv`) |

---

## Step completion status

| Step | Status | Tests |
|---|---|---|
| 1 — Package infrastructure + all R source + tests | **Complete** | 118 passing, 0 failures |
| 2 — regtab() + print method + tests | **Complete** | 162 passing, 0 failures |
| 3 — Vignette `vignettes/getting-started.Rmd` | **Complete** | — |
| Layout plan — to_excel() + to_latex() rendering | **Complete** | 162 passing, 0 failures |

`devtools::check()` result after layout plan: **0 errors / 0 warnings / 0 notes.**
`covr::package_coverage()` overall: **86.70%** after step 3 (see coverage gaps section below).

### Layout plan changes
- `to_excel()`: added `raw` and `digits` parameters.
  - `raw = TRUE` writes numeric scalars to estimate/SE cells (formula-usable); all styling still applied.
  - `digits` overrides `x$digits` for this render call only; re-derives formatted strings from raw `coef_data` columns.
  - `col_groups` spanning header now calls `wb_merge_cells()` for groups covering ≥2 columns.
  - Factor header rows (`is_factor_header = TRUE`) now bold the label cell.
  - Factor child rows (`!is.na(factor_group) && !is_factor_header`) now indent the label cell via `wb_add_cell_style(..., indent = 1L)`.
- `to_latex()`: added `digits` parameter with same override semantics; re-derives coef and stat formatting from raw values when overriding.
- `utils.R`: `build_stat_data` now stores `value_raw` (numeric) in `stat_data` alongside `value_fmt`, enabling digits re-formatting in output functions.

### Step 2 changes
- Added `tests/testthat/test-panels-and-display.R` with 44 tests covering:
  `assemble_panels`, `detect_i_vars`, `col_groups` display in `to_latex()`,
  and `add_rows` display in `print.regtab_table()` and `to_latex()`.
- Fixed bug in `to_latex.R`: `seq(lo + 1L, hi)` when `lo == hi` produced a
  decreasing 2-element sequence in R, overwriting the multicolumn cell and
  extending `cells` spuriously. Fix: guard with `if (hi > lo)`.


---

## Locked design decisions — do not revisit

- **SEs always taken from the model object as-is.** No vcov swapping,
  no `sandwich`, no `lmtest`. Non-negotiable.
- **fixest is the primary class.** `tidy_model_fixest` is a custom extractor.
  Do not replace it with `broom::tidy.fixest`.
- **lmtest and sandwich are NOT in Imports or Suggests** and must not be
  added. coeftest support is v2 scope only.
- **No HTML output.** Excel and LaTeX are co-equal outputs. HTML is out of
  scope for all versions.
- **LaTeX stars use math-mode superscripts:** `$^{*}$`, `$^{**}$`,
  `$^{***}$`. Plain-text stars are stored in `coef_data$estimate_fmt`;
  `to_latex()` converts them via `latex_stars()` at render time.
- **Significance thresholds:** `stars = c(0.1, 0.05, 0.01)` means
  `*` p<0.1, `**` p<0.05, `***` p<0.01.
- **Excel is built on openxlsx2 v1.25** (no Java dependency). The correct
  API is `wb_add_font(wb, sheet, dims, bold = TRUE)` and
  `wb_add_border(wb, sheet, dims, ...)`. Do not use the non-exported
  `create_cell_style()`, `create_font()`, or `create_alignment()`.
- **FE rows auto-extracted from fixest** via `model$fixef_vars`. Non-fixest
  models provide FE rows manually via `add_rows`.
- **R ≥ 4.1.0** is the declared minimum. Do not use the `|>` pipe
  placeholder syntax (`\`_\``) introduced in R 4.2.0.
- **Explicit namespace calls throughout** (`pkg::fn()`, not bare `fn()`).
  No global variable bindings; use `.data$` in any dplyr chains.

---

## DESCRIPTION dependencies

```
Imports:   fixest, broom, openxlsx2
Suggests:  lfe, AER, testthat (>= 3.0.0), knitr, rmarkdown
R:         >= 4.1.0
```

---

## tidy_model() output schema

`tidy_model(model)` is unexported. Returns a named list with exactly three
slots. The schema is **fixed** — all slots and all columns are always present
regardless of model class. Missing values are `NA`, never absent columns.

```r
list(
  coefs = data.frame(
    term      = character,   # raw coefficient name as stored in model
    estimate  = numeric,
    std.error = numeric,     # from model's stored vcov — never swapped
    p.value   = numeric
  ),

  glance = data.frame(       # always exactly 1 row; always all 6 columns
    nobs          = integer, # observations used in estimation
    r2            = numeric, # overall R²; NA if not meaningful/available
    r2_within     = numeric, # within R² (demeaned); NA if no FEs or non-fixest
    rmse          = numeric, # sqrt(SSR/N) for fixest; sigma for lm/ivreg; NA otherwise
    nobs_clusters = integer, # number of clusters; NA if unclustered
    kp_fstat      = numeric  # Kleibergen-Paap F-stat; NA if not IV
  ),

  fe_vars = character        # from model$fixef_vars (fixest only); NULL otherwise
)
```

### Class dispatch table

| Class | Backend | Notes |
|---|---|---|
| `fixest` | Custom extractor | No broom; uses `coef()`, `vcov()`, `fitstat()`, `model$fixef_vars` |
| `lm` | `broom::tidy()` + `broom::glance()` | `r2_within`, `nobs_clusters`, `kp_fstat` always NA |
| `glm` | `broom::tidy()` + `broom::glance()` | `r2` left as NA (not meaningful across families) |
| `felm` | `broom::tidy()` + manual glance from `summary()` | `fe_vars` is NULL (lfe doesn't expose them cleanly) |
| `ivreg` | `broom::tidy()` + `broom::glance()` | `kp_fstat` is NA (AER ivreg uses different diagnostics) |

### fixest-specific extraction gotchas

- **R²**: use `fixest::fitstat(model, "r2")$r2` and `fixest::fitstat(model, "wr2")$wr2`.
  `model$r2` is `NULL` in current fixest — do not use it.
- **RMSE**: `sqrt(model$ssr / model$nobs)`.
- **Cluster count**: `attr(vcov(model), "G")[[1L]]`.
  The attribute is named `"G"`, not `"ncluster"`.
- **KP F-stat**: `summary(model)$iv_stat[["KP F-stat"]]` — only non-NULL
  for IV models.
- **p-values**: derived from `vcov(model)` diagonal so they always match
  the stored variance-covariance matrix (including clustered/robust vcov).

---

## regtab_table S3 class — slot reference

All data blocks are stored **long-form**. Output functions pivot to wide
using `model_names` as the column order.

```r
structure(
  list(
    coef_data = data.frame(
      term_display     = character,  # display label (after coef_map)
      term_raw         = character,  # original name from model
      model            = character,  # model label (column name)
      estimate         = numeric,
      std.error        = numeric,
      p.value          = numeric,
      estimate_fmt     = character,  # e.g. "0.123**"  (plain-text stars)
      se_fmt           = character,  # e.g. "(0.045)"
      is_factor_header = logical,    # TRUE → group label row, no SE row
      factor_group     = character,  # NA or factor stem name
      row_order        = numeric     # sort key; headers get stem_min - 0.5
    ),
    fe_data   = data.frame(fe_label, fe_raw, model, included),  # included = logical
    add_rows  = data.frame or NULL,   # columns: label + one per model name
    stat_data = data.frame(stat, model, value_fmt, value_raw),  # value_raw numeric; used for digits re-formatting in to_excel/to_latex
    model_names = character,
    col_groups  = named character or NULL,
    digits      = integer,
    stars       = numeric,
    call        = call
  ),
  class = "regtab_table"
)
```

---

## Coverage gaps and resolution plan

Function-level coverage after step 1 (`covr::package_coverage()`):

| Function | Coverage | Gap | Resolved in |
|---|---|---|---|
| `assemble_panels` | 0% | `panels` arg not exercised in tests | Step 2 vignette |
| `detect_i_vars` | 0% | fixest `i()` syntax not tested | Step 2 vignette |
| `pivot_wide` | 0% | Only called by output functions; path not reached | Step 2 vignette |
| `tidy_model_felm` | 0% | `lfe` not installed in this environment | Future: install lfe |
| `tidy_model_ivreg` | 0% | `AER` not installed in this environment | Future: install AER |
| `latex_stat_label` | 62.5% | "Clusters" and "KP F-stat" label branches not reached | Step 2 vignette (IV spec) |
| `print.regtab_table` | 76% | `col_groups` and `add_rows` display paths not triggered | Dedicated pass |
| `to_latex` | 76.9% | `col_groups`, `add_rows`, `format="full"` paths not tested | Dedicated pass |
| `tidy_model_fixest` | 77.1% | IV/KP branch and `model$se` path not reached | Step 2 vignette (IV spec) |

All four functions targeted in the last session now meet 80%:
`tidy_model_glm` 100%, `tidy_model` 83.3%, `build_coef_data` 98.6%,
`format_stat` 100%.

---

## Step 2 prompt

Use this exact prompt to start the next session:

```
We are building the exportreg R package. Step 1 is complete:
all R source files, infrastructure, and 118 tests are passing
with 0 errors / 0 warnings / 0 notes from R CMD check.

Read CLAUDE.md before doing anything else.

Step 2: write vignettes/labor-economics.Rmd.

Requirements:
- YAML front matter: title "Labor Economics Tables with exportreg",
  output rmarkdown::html_vignette, vignette index entry
- Generate synthetic wage panel data (at least 500 obs, 50 firms,
  10 years, 3 worker-level covariates, one instrument)
- Section 1 "OLS Specifications": baseline feols() with firm FE,
  extended spec adding year FE, side-by-side via regtab() with
  fe_labels, coef_map, and col_groups; call print() and to_latex()
- Section 2 "IV Specification": feols() with iv argument so KP
  F-stat appears in the stat block; show to_latex() output
- Section 3 "Multi-panel table": two regtab_table objects passed
  to panels argument; show print() output
- Section 4 "Excel export": to_excel() call writing to tempfile(),
  explain formatting choices, do not leave files on disk
- echo = TRUE throughout; use set.seed(42)
- to_excel() must use tempfile() so the vignette builds cleanly
- After writing the vignette, run devtools::build_vignettes() to
  confirm it compiles without errors, then run
  covr::package_coverage() and report any functions still below 80%
```
