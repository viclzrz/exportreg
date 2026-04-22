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

## Current package state (v1 complete)

| Metric | Value |
|---|---|
| Tests | **225 passing, 0 failures** |
| `devtools::check()` | **0 errors / 0 warnings / 0 notes** |
| Overall coverage | **87.82%** |

### Step completion status

| Step | Status | Tests |
|---|---|---|
| 1 — Package infrastructure + all R source + tests | **Complete** | 118 passing |
| 2 — regtab() + print method + tests | **Complete** | 162 passing |
| 3 — Vignette `vignettes/getting-started.Rmd` | **Complete** | — |
| Layout plan — to_excel() + to_latex() rendering | **Complete** | 162 passing |
| se_format + se_type + se_note plan | **Complete** | 201 passing |
| Coverage gap closure (se_format paths, IV/KP, latex_stat_label) | **Complete** | 225 passing |

---

## v1 feature set

- **fixest first-class support**: `feols`, `fepois`, `feiv` — custom extractor,
  no broom dependency for fixest
- **lm / glm support** via `broom::tidy()` + `broom::glance()`
- **FE auto-extraction** from fixest `model$fixef_vars`; human-readable names
  via `fe_labels`
- **Excel output** via openxlsx2 (no Java): `raw = TRUE/FALSE`, `digits`
  override, column auto-fit, bold headers, thin borders, factor indentation,
  `col_groups` merged spanning headers
- **LaTeX output** booktabs style: `fragment` / `full` format, `digits`
  override, math-mode stars, `col_groups` `\multicolumn`, significance note
  with SE-format and SE-type annotation
- **`se_format`**: `"se"` (parentheses) / `"tstat"` (brackets) / `"pvalue"`
  (brackets) — controls second row under each coefficient in all three output
  functions
- **SE note automation**: `se_type` auto-detected per model from the model
  object (e.g. `"IID"`, `"Clustered (firm_id)"`); `build_se_note()` assembles
  a human-readable note appended to the significance line at render time
- **`factor_labels`** grouped display: header row + indented child rows
- **`col_groups`** spanning headers in LaTeX and Excel
- **`add_rows`** for manually supplied rows (e.g. control indicators)
- **Multi-panel tables** via `panels` argument to `regtab()`
- **`coef_map`** for coefficient renaming
- **`keep` / `drop`** regex filters

---

## Known acceptable coverage gaps

| Function | Coverage | Reason | Resolution |
|---|---|---|---|
| `pivot_wide` | 0% | Only called by output functions at a code path not reachable without mock data; not worth a direct unit test | Acceptable — leave as-is |
| `tidy_model_felm` | 0% | `lfe` package not installed in CI | Acceptable — guarded by `skip_if_not_installed` |
| `tidy_model_ivreg` | 0% | `AER` package not installed in CI | Acceptable — guarded by `skip_if_not_installed` |

All other functions are ≥ 80% covered.

---

## v2 roadmap

- **coeftest / lmtest support**: wrap `coeftest` output with custom SE
  injection; requires adding `lmtest` to `Suggests`
- **`to_plot()`**: coefficient plot output using base R or ggplot2
- **Formatting profiles**: pre-built style sets (AER, journal-style, minimal)
  selectable via a `style` argument

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
- **LaTeX significance note format** (as of se_format plan): plain-text stars
  then math-mode p-values: `*** $p<0.01$, ** $p<0.05$, * $p<0.1$`, followed
  by the SE-format + SE-type note from `build_se_note()`.
- **Significance thresholds:** `stars = c(0.1, 0.05, 0.01)` means
  `*` p<0.1, `**` p<0.05, `***` p<0.01.
- **Excel is built on openxlsx2 v1.25** (no Java dependency). The correct
  API is `wb_add_font(wb, sheet, dims, bold = TRUE)` and
  `wb_add_border(wb, sheet, dims, ...)`. Do not use the non-exported
  `create_cell_style()`, `create_font()`, or `create_alignment()`.
- **FE rows auto-extracted from fixest** via `model$fixef_vars`. Non-fixest
  models provide FE rows manually via `add_rows`.
- **R ≥ 4.1.0** is the declared minimum. Do not use the `|>` pipe
  placeholder syntax (`` `_` ``) introduced in R 4.2.0.
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

`tidy_model(model)` is unexported. Returns a named list with exactly **four**
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

  fe_vars = character,       # from model$fixef_vars (fixest only); NULL otherwise

  se_type = character(1L)    # SE type label, e.g. "IID", "Clustered (firm_id)"
)
```

### Class dispatch table

| Class | Backend | Notes |
|---|---|---|
| `fixest` | Custom extractor | No broom; uses `coef()`, `vcov()`, `fitstat()`, `model$fixef_vars` |
| `lm` | `broom::tidy()` + `broom::glance()` | `r2_within`, `nobs_clusters`, `kp_fstat` always NA; `se_type = "IID"` |
| `glm` | `broom::tidy()` + `broom::glance()` | `r2` left as NA (not meaningful across families); `se_type = "IID"` |
| `felm` | `broom::tidy()` + manual glance from `summary()` | `fe_vars` is NULL; `se_type` from `model$clustervar` |
| `ivreg` | `broom::tidy()` + `broom::glance()` | `kp_fstat` is NA (AER uses different diagnostics); `se_type = "IID"` |

### fixest-specific extraction gotchas

- **R²**: use `fixest::fitstat(model, "r2")$r2` and `fixest::fitstat(model, "wr2")$wr2`.
  `model$r2` is `NULL` in current fixest — do not use it.
- **RMSE**: `sqrt(model$ssr / model$nobs)`.
- **Cluster count**: `attr(vcov(model), "G")[[1L]]`.
  The attribute is named `"G"`, not `"ncluster"`.
- **KP F-stat**: primary API is `fixest::fitstat(model, "kpr")$kpr$stat`
  (fixest ≥ 0.12). Legacy fallback: `summary(model)$iv_stat[["KP F-stat"]]`.
  The `$iv_stat` slot is NULL in fixest 0.14.0+.
- **SE type**: `attr(model$se, "vcov_type")`, fallback to
  `attr(vcov(model), "vcov_type")`, final fallback `"IID"`.
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
    stat_data = data.frame(stat, model, value_fmt, value_raw),  # value_raw numeric
    model_names = character,
    col_groups  = named character or NULL,
    digits      = integer,
    stars       = numeric,
    se_format   = character,          # "se" | "tstat" | "pvalue"
    se_type     = named character,    # model_name → SE type label
    call        = call
  ),
  class = "regtab_table"
)
```
