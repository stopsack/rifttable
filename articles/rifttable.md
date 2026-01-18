# Get started with rifttable

## Loading the package

After installation (using `install.packages("rifttable")` or
`remotes::install_github("stopsack/rifttable")`), load the package with:

``` r
library(rifttable)
```

## Loading example data

The `cancer` dataset from survival package is used here:

``` r
data(cancer, package = "survival")

cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    # The exposure (here, 'sex') must be categorical
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    time = time / 365.25,
    status = status - 1
  )

cancer
#> # A tibble: 228 × 10
#>     inst  time status   age sex    ph.ecog ph.karno pat.karno meal.cal wt.loss
#>    <dbl> <dbl>  <dbl> <dbl> <fct>    <dbl>    <dbl>     <dbl>    <dbl>   <dbl>
#>  1     3 0.838      1    74 Male         1       90       100     1175      NA
#>  2     3 1.25       1    68 Male         0       90        90     1225      15
#>  3     3 2.77       0    56 Male         0       90        90       NA      15
#>  4     5 0.575      1    57 Male         1       90        60     1150      11
#>  5     1 2.42       1    60 Male         0      100        90       NA       0
#>  6    12 2.80       0    74 Male         1       50        80      513       0
#>  7     7 0.849      1    68 Female       2       70        60      384      10
#>  8    11 0.988      1    71 Female       2       60        80      538       1
#>  9     1 0.597      1    53 Male         1       70        80      825      16
#> 10     7 0.454      1    61 Male         2       70        70      271      34
#> # ℹ 218 more rows
```

## Example 1: Basic use with binary outcomes

Set the table design:

``` r
design1 <- tibble::tibble(
  label = c(
    "Outcomes",
    "Total",
    "Outcomes/Total",
    "Risk",
    "Risk (CI)",
    "Outcomes (Risk)",
    "Outcomes/Total (Risk)",
    "RR",
    "RD"
  )
) |>
  dplyr::mutate(
    type = label,
    exposure = "sex",
    outcome = "status"
  )
```

Generate rifttable:

``` r
rifttable(
  design = design1,
  data = cancer
)
#> # A tibble: 9 × 3
#>   sex                   Male              Female              
#>   <chr>                 <chr>             <chr>               
#> 1 Outcomes              112               53                  
#> 2 Total                 138               90                  
#> 3 Outcomes/Total        112/138           53/90               
#> 4 Risk                  0.81              0.59                
#> 5 Risk (CI)             0.81 (0.74, 0.87) 0.59 (0.49, 0.68)   
#> 6 Outcomes (Risk)       112 (0.81)        53 (0.59)           
#> 7 Outcomes/Total (Risk) 112/138 (0.81)    53/90 (0.59)        
#> 8 RR                    1 (reference)     0.73 (0.60, 0.88)   
#> 9 RD                    0 (reference)     -0.22 (-0.34, -0.10)
```

## Example 2: Formatted output

This example uses the `design` of Example 1 above. So far, the tables
produced when just running the code shown had an appearance like output
in the R console. To obtain formatted tables in HTML and other output
documents, pipe the output of
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
to a table formatting function such as

- [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html): works
  well with all output formats, but supports only limited formatting.
  Add to the YAML header a statement `df_print: kable` to print all
  tables using kable.
- [`gt::gt()`](https://gt.rstudio.com/reference/gt.html): supports more
  advanced formatting but output is no longer human-readable.

The rifttable package provides the
[`rt_gt()`](https://stopsack.github.io/rifttable/reference/rt_gt.md)
wrapper function. When knitting to HTML, PDF, or Word, it functions as a
wrapper for [`gt::gt()`](https://gt.rstudio.com/reference/gt.html),
passing along indentations from the `label` of the table `design`. When
knitting to a Markdown document (`.md`), such as `github_document` (in
RMarkdown) or `gfm` (in Quarto),
[`rt_gt()`](https://stopsack.github.io/rifttable/reference/rt_gt.md)
will automatically provide plain table output using the kable package.

``` r
rifttable(
  design = design1,
  data = cancer
) |>
  rt_gt()
```

| sex                   | Male              | Female               |
|:----------------------|:------------------|:---------------------|
| Outcomes              | 112               | 53                   |
| Total                 | 138               | 90                   |
| Outcomes/Total        | 112/138           | 53/90                |
| Risk                  | 0.81              | 0.59                 |
| Risk (CI)             | 0.81 (0.74, 0.87) | 0.59 (0.49, 0.68)    |
| Outcomes (Risk)       | 112 (0.81)        | 53 (0.59)            |
| Outcomes/Total (Risk) | 112/138 (0.81)    | 53/90 (0.59)         |
| RR                    | 1 (reference)     | 0.73 (0.60, 0.88)    |
| RD                    | 0 (reference)     | -0.22 (-0.34, -0.10) |

## Example 3: Swap rows and columns

To use the `design` as columns instead of rows, showing only three
`type`s:

``` r
rifttable(
  design = design1 |>
    dplyr::filter(label %in% c(
      "Outcomes/Total (Risk)",
      "RR",
      "RD"
    )),
  data = cancer,
  layout = "cols"
) |>
  rt_gt()
```

| sex    | Outcomes/Total (Risk) | RR                | RD                   |
|:-------|:----------------------|:------------------|:---------------------|
| Male   | 112/138 (0.81)        | 1 (reference)     | 0 (reference)        |
| Female | 53/90 (0.59)          | 0.73 (0.60, 0.88) | -0.22 (-0.34, -0.10) |

## Example 4: Survival outcomes, effect modifier, and confounder

Survival outcomes use the `time` and `event` variables in the `design`
(and `type2`, with late entry, as in
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)),
rather than the `outcome` variable used for binary, categorical, or
continuous outcomes.

Set table design:

``` r
design2 <- tibble::tribble(
  # Elements that vary by row:
  ~label,                       ~stratum, ~confounders, ~type,
  "**Overall**",                NULL,     "",           "blank",
  "  Events",                   NULL,     "",           "events",
  "  Person-years",             NULL,     "",           "time",
  "  Rate/1000 py (95% CI)",    NULL,     "",           "rate (ci)",
  "  Unadjusted HR (95% CI)",   NULL,     "",           "hr",
  "  Age-adjusted HR (95% CI)", NULL,     "+ age",      "hr",
  "",                           NULL,     "",           "blank",
  "**Stratified models**",      NULL,     "",           "",
  "*ECOG PS1* (events/N)",      1,        "",           "events/total",
  "  Unadjusted",               1,        "",           "hr",
  "  Age-adjusted",             1,        "+ age",      "hr",
  "*ECOG PS2* (events/N)",      2,        "",           "events/total",
  "  Unadjusted",               2,        "",           "hr",
  "  Age-adjusted",             2,        "+ age",      "hr",
  "",                           NULL,     "",           "",
  "**Joint model**, age-adj.",  NULL,     "",           "",
  "  ECOG PS1",                 1,        "+ age",      "hr_joint",
  "  ECOG PS2",                 2,        "+ age",      "hr_joint"
) |>
  # Elements that are the same for all rows:
  dplyr::mutate(
    exposure = "sex",
    event = "status",
    time = "time",
    effect_modifier = "ph.ecog"
  )
```

Generate rifttable:

``` r
rifttable(
  design = design2,
  data = cancer |>
    dplyr::filter(ph.ecog %in% 1:2)
) |>
  rt_gt()
```

| sex                       | Male                   | Female                |
|:--------------------------|:-----------------------|:----------------------|
| **Overall**               |                        |                       |
| Events                    | 82                     | 44                    |
| Person-years              | 70                     | 59                    |
| Rate/1000 py (95% CI)     | 1164.8 (938.1, 1446.3) | 746.7 (555.7, 1003.4) |
| Unadjusted HR (95% CI)    | 1 (reference)          | 0.60 (0.41, 0.86)     |
| Age-adjusted HR (95% CI)  | 1 (reference)          | 0.60 (0.41, 0.86)     |
|                           |                        |                       |
| **Stratified models**     |                        |                       |
| *ECOG PS1* (events/N)     | 54/71                  | 28/42                 |
| Unadjusted                | 1 (reference)          | 0.53 (0.33, 0.85)     |
| Age-adjusted              | 1 (reference)          | 0.53 (0.33, 0.85)     |
| *ECOG PS2* (events/N)     | 28/29                  | 16/21                 |
| Unadjusted                | 1 (reference)          | 0.70 (0.37, 1.30)     |
| Age-adjusted              | 1 (reference)          | 0.68 (0.34, 1.36)     |
|                           |                        |                       |
| **Joint model**, age-adj. |                        |                       |
| ECOG PS1                  | 1 (reference)          | 0.55 (0.35, 0.88)     |
| ECOG PS2                  | 1.54 (0.98, 2.44)      | 1.10 (0.62, 1.98)     |

## Example 5: Two estimates using `type` and `type2`

``` r
design3 <- tibble::tribble(
  ~label,     ~stratum, ~type,          ~type2,
  "ECOG PS1", 1,        "events/total", "hr",
  "ECOG PS2", 2,        "events/total", "hr"
) |>
  dplyr::mutate(
    exposure = "sex",
    event = "status",
    time = "time",
    confounders = "+ age",
    effect_modifier = "ph.ecog"
  )

rifttable(
  design = design3,
  data = cancer |>
    dplyr::filter(ph.ecog %in% 1:2)
) |>
  rt_gt()
```

| sex      | Male          | Female            |
|:---------|:--------------|:------------------|
| ECOG PS1 | 54/71         | 28/42             |
|          | 1 (reference) | 0.53 (0.33, 0.85) |
| ECOG PS2 | 28/29         | 16/21             |
|          | 1 (reference) | 0.68 (0.34, 1.36) |

With estimate `type` as columns:

``` r
rifttable(
  design = design3,
  data = cancer |>
    dplyr::filter(ph.ecog %in% 1:2),
  layout = "cols",
  type2_layout = "cols"
) |>
  rt_gt()
```

| sex    | ECOG PS1 | ECOG PS1          | ECOG PS2 | ECOG PS2          |
|:-------|:---------|:------------------|:---------|:------------------|
| Male   | 54/71    | 1 (reference)     | 28/29    | 1 (reference)     |
| Female | 28/42    | 0.53 (0.33, 0.85) | 16/21    | 0.68 (0.34, 1.36) |

## Example 6: Continuous outcomes, rounding, and trend (slope)

Request rounding to 1 decimal digit in some cases; add a continuous
trend, *i.e.*, the slope per one unit of the `trend` variable:

``` r
tibble::tribble(
  ~label,                   ~stratum, ~type,        ~digits,
  "Marginal mean (95% CI)", NULL,     "mean (ci)",  1,
  "  Male",                 "Male",   "mean",       NA,
  "  Female",               "Female", "mean",       NA,
  "",                       NULL,     "",           NA,
  "Stratified model",       NULL,     "",           NA,
  "  Male",                 "Male",   "diff",       1,
  "  Female",               "Female", "diff",       1,
  "",                       NULL,     "",           NA,
  "Joint model",            NULL,     "",           NA,
  "  Male",                 "Male",   "diff_joint", NA,
  "  Female",               "Female", "diff_joint", NA
) |>
  dplyr::mutate(
    exposure = "ph.ecog_factor",
    trend = "ph.ecog",
    outcome = "age",
    effect_modifier = "sex"
  ) |>
  rifttable(
    data = cancer |>
      dplyr::filter(ph.ecog < 3) |>
      dplyr::mutate(ph.ecog_factor = factor(ph.ecog))
  ) |>
  rt_gt()
```

| ph.ecog_factor         | 0                   | 1                   | 2                  | Trend              |
|:-----------------------|:--------------------|:--------------------|:-------------------|:-------------------|
| Marginal mean (95% CI) | 61.2 (58.8, 63.5)   | 61.5 (59.8, 63.1)   | 66.2 (64.0, 68.5)  |                    |
| Male                   | 63.00               | 62.79               | 65.00              |                    |
| Female                 | 58.70               | 59.19               | 67.90              |                    |
|                        |                     |                     |                    |                    |
| Stratified model       |                     |                     |                    |                    |
| Male                   | 0 (reference)       | -0.2 (-3.9, 3.5)    | 2.0 (-2.5, 6.5)    | 0.9 (-1.3, 3.2)    |
| Female                 | 0 (reference)       | 0.5 (-3.5, 4.5)     | 9.2 (4.5, 13.9)    | 4.4 (2.0, 6.7)     |
|                        |                     |                     |                    |                    |
| Joint model            |                     |                     |                    |                    |
| Male                   | 0 (reference)       | -0.21 (-3.75, 3.33) | 2.00 (-2.32, 6.32) | 0.93 (-1.33, 3.19) |
| Female                 | -4.30 (-8.70, 0.11) | -3.81 (-7.74, 0.12) | 4.90 (0.15, 9.66)  | 4.36 (1.97, 6.75)  |
