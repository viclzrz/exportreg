
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exportreg

exportreg builds publication-ready regression tables for economists,
with first-class fixest support and Excel and LaTeX as priority output
formats.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("viclzrz/exportreg")
```

## Minimal example

`regtab()` accepts a named list of model objects — mix `lm` and `feols`
freely. The names become the column headers.

### Data and models

``` r
library(exportreg)
library(fixest)

set.seed(42)
n <- 500
df <- data.frame(
  wage       = exp(rnorm(n)),
  educ       = sample(8:16, n, replace = TRUE),
  exper      = sample(1:30, n, replace = TRUE),
  firm_id    = sample(1:50, n, replace = TRUE),
  instrument = rnorm(n)
)

m1 <- lm(log(wage) ~ educ + exper,
         data = df)
m2 <- feols(log(wage) ~ educ + exper | firm_id,
            data = df, cluster = ~firm_id)
m3 <- feols(log(wage) ~ exper | firm_id |
            educ ~ instrument,
            data = df, cluster = ~firm_id)
```

### Example 1 — standard errors in parentheses

`m1` uses IID standard errors; `m2` and `m3` are clustered at the firm
level. `regtab()` detects both SE types and builds the note
automatically. `coef_map` aligns the Education row across all three
columns: OLS and FE report the coefficient as `educ`, the IV second
stage as `fit_educ` — both map to the same display label.

``` r
tab <- regtab(
  models         = list("(1) OLS" = m1,
                        "(2) FE"  = m2,
                        "(3) IV"  = m3),
  fe_labels      = c("firm_id" = "Firm"),
  cluster_labels = c("firm_id" = "Firm"),
  depvar_labels  = c("log(wage)" = "ln(wage)"),
  coef_map       = c("educ"     = "Education",
                     "fit_educ" = "Education",
                     "exper"    = "Experience")
)
```

``` r
to_latex(tab)
\begin{tabular}{lccc}
\toprule
 & (1) OLS & (2) FE & (3) IV \\
\midrule
Dep.\ var. & ln(wage) & ln(wage) & ln(wage) \\
(Intercept) & -0.180 &  &  \\
 & (0.212) &  &  \\
Education & 0.002 & 0.007 &  \\
 & (0.017) & (0.017) &  \\
Experience & 0.008 & 0.008 & 0.027 \\
 & (0.005) & (0.005) & (0.052) \\
\midrule
$N$ & 500 & 500 & 500 \\
Clusters &  & 50 & 50 \\
$R^{2}$ & 0.005 & 0.087 & 0.089 \\
Within $R^{2}$ &  & 0.006 & 0.008 \\
RMSE & 0.971 & 0.928 & 2.578 \\
\midrule
Firm & No & Yes & Yes \\
\bottomrule
\multicolumn{4}{l}{\textit{Note:} *** $p<0.01$, ** $p<0.05$, * $p<0.1$. Standard errors in parentheses ((1) OLS); Standard errors clustered at the Firm level in parentheses ((2) FE, (3) IV)} \\
\end{tabular} 
```

The significance note reflects the mixed SE setup: column (1) uses IID
standard errors, while columns (2) and (3) are clustered at the Firm
level. The Firm FE row shows No / Yes / Yes, and column (3) includes a
KP F-stat row from the first stage.

### Example 2 — t-statistics in brackets

Pass `se_format = "tstat"` to show absolute t-statistics in brackets
instead of standard errors in parentheses. The note updates
automatically.

``` r
tab_t <- regtab(
  models         = list("(1) OLS" = m1,
                        "(2) FE"  = m2,
                        "(3) IV"  = m3),
  fe_labels      = c("firm_id" = "Firm"),
  cluster_labels = c("firm_id" = "Firm"),
  depvar_labels  = c("log(wage)" = "ln(wage)"),
  coef_map       = c("educ"     = "Education",
                     "fit_educ" = "Education",
                     "exper"    = "Experience"),
  se_format      = "tstat"
)
```

``` r
to_latex(tab_t)
\begin{tabular}{lccc}
\toprule
 & (1) OLS & (2) FE & (3) IV \\
\midrule
Dep.\ var. & ln(wage) & ln(wage) & ln(wage) \\
(Intercept) & -0.180 &  &  \\
 & [0.849] &  &  \\
Education & 0.002 & 0.007 &  \\
 & [0.134] & [0.423] &  \\
Experience & 0.008 & 0.008 & 0.027 \\
 & [1.635] & [1.494] & [0.508] \\
\midrule
$N$ & 500 & 500 & 500 \\
Clusters &  & 50 & 50 \\
$R^{2}$ & 0.005 & 0.087 & 0.089 \\
Within $R^{2}$ &  & 0.006 & 0.008 \\
RMSE & 0.971 & 0.928 & 2.578 \\
\midrule
Firm & No & Yes & Yes \\
\bottomrule
\multicolumn{4}{l}{\textit{Note:} *** $p<0.01$, ** $p<0.05$, * $p<0.1$. $t$-statistics in brackets ((1) OLS); Standard errors clustered at the Firm level. $t$-statistics in brackets ((2) FE, (3) IV)} \\
\end{tabular} 
```

### Example 3 — Excel output

`to_excel()` writes a formatted `.xlsx` file using `openxlsx2` (no Java
required). Column widths are auto-fitted and headers are bolded.

``` r
to_excel(tab, file = "table1.xlsx")
# to_excel(tab, file = "table1.xlsx", raw = TRUE)
```

## Full documentation

For a full walkthrough including multi-panel tables and all available
arguments, see the vignette:

``` r
vignette("getting-started", package = "exportreg")
```
