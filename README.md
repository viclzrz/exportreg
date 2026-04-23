
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exportreg

`exportreg` turns a named list of regression models into
publication-ready tables for export to **Excel** and **LaTeX**, with
first-class support for `fixest` models (`feols`, `fepois`, `feiv`).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("vicentelopez/exportreg")
```

## Minimal example

### Simulate data and fit models

``` r
library(exportreg)
library(fixest)

set.seed(42)
n <- 200
dat <- data.frame(
  log_wage  = rnorm(n, mean = 2.8, sd = 0.5),
  educ      = sample(8:20, n, replace = TRUE),
  exper     = sample(1:30, n, replace = TRUE),
  female    = rbinom(n, 1, 0.45),
  firm_id   = sample(1:20, n, replace = TRUE)
)

m1 <- feols(log_wage ~ educ + exper          | firm_id, data = dat)
m2 <- feols(log_wage ~ educ + exper + female | firm_id, data = dat)
```

### Build the regression table

``` r
tab <- regtab(
  models    = list("(1)" = m1, "(2)" = m2),
  coef_map  = c(
    educ   = "Years of education",
    exper  = "Experience (years)",
    female = "Female"
  ),
  fe_labels = c(firm_id = "Firm FE")
)
```

### Console output

``` r
print(tab)
#>                     (1)       (2)      
#> -------------------------------------- 
#> Dep. var.           log_wage  log_wage 
#> Years of education  -0.014    -0.014   
#>                     (0.010)   (0.010)  
#> Experience (years)  -0.003    -0.004   
#>                     (0.004)   (0.004)  
#> Female                        -0.042   
#>                               (0.075)  
#> -------------------------------------- 
#> N                   200       200      
#> R2                  0.099     0.100    
#> Within R2           0.014     0.016    
#> RMSE                0.461     0.461    
#> -------------------------------------- 
#> Firm FE             Yes       Yes      
#> 
#> Note: * p<0.1, ** p<0.05, *** p<0.01
```

### LaTeX output

``` r
to_latex(tab)
#> \begin{tabular}{lcc}
#> \toprule
#>  & (1) & (2) \\
#> \midrule
#> Dep.\ var. & log\_wage & log\_wage \\
#> Years of education & -0.014 & -0.014 \\
#>  & (0.010) & (0.010) \\
#> Experience (years) & -0.003 & -0.004 \\
#>  & (0.004) & (0.004) \\
#> Female &  & -0.042 \\
#>  &  & (0.075) \\
#> \midrule
#> $N$ & 200 & 200 \\
#> $R^{2}$ & 0.099 & 0.100 \\
#> Within $R^{2}$ & 0.014 & 0.016 \\
#> RMSE & 0.461 & 0.461 \\
#> \midrule
#> Firm FE & Yes & Yes \\
#> \bottomrule
#> \multicolumn{3}{l}{\textit{Note:} *** $p<0.01$, ** $p<0.05$, * $p<0.1$. SE in parentheses. SE: IID} \\
#> \end{tabular}
```

### Excel output

``` r
to_excel(tab, path = "table1.xlsx")
```

## Full documentation

See the vignette for a complete walkthrough of all features including
`col_groups`, `add_rows`, `se_format`, factor variables, and IV models:

``` r
vignette("getting-started", package = "exportreg")
```
