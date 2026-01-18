# Frequently Asked Questions

## Basic example

Most subsequent examples build on this simple rifttable:

``` r
library(rifttable)
library(dplyr) # for data management, e.g., mutate()
library(tibble) # for constructing a tibble, e.g. via tribble()
data(breastcancer, package = "risks")

design <- tribble(
  ~label,                       ~type,                   ~stratum,
  "**Overall**",                "",                      "",
  "  Deaths/N",                 "outcomes/total",        c("Low", "High"),
  "  Risk",                     "risk",                  c("Low", "High"),
  "  Risk ratio (95% CI)",      "rr",                    c("Low", "High"),
  "  Risk difference (95% CI)", "rd",                    c("Low", "High"),
  "",                           "",                      "",
  "**Low hormone receptor**",   "",                      "",
  "  Deaths/N (Risk)",          "outcomes/total (risk)", "Low",
  "  Risk difference (95% CI)", "rd",                    "Low",
  "**High hormone receptor**",  "",                      "",
  "  Deaths/N (Risk)",          "outcomes/total (risk)", "High",
  "  Risk difference (95% CI)", "rd",                    "High"
) |>
  mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor"
  )

rifttable(
  design = design,
  data = breastcancer
) |>
  rt_gt() # obtain formatted output
```

| Stage                     | Stage I       | Stage II           | Stage III         |
|:--------------------------|:--------------|:-------------------|:------------------|
| **Overall**               |               |                    |                   |
| Deaths/N                  | 7/67          | 26/96              | 21/29             |
| Risk                      | 0.10          | 0.27               | 0.72              |
| Risk ratio (95% CI)       | 1 (reference) | 2.59 (1.20, 5.6)   | 6.9 (3.3, 14)     |
| Risk difference (95% CI)  | 0 (reference) | 0.17 (0.05, 0.28)  | 0.62 (0.44, 0.80) |
|                           |               |                    |                   |
| **Low hormone receptor**  |               |                    |                   |
| Deaths/N (Risk)           | 2/12 (0.17)   | 9/22 (0.41)        | 12/14 (0.86)      |
| Risk difference (95% CI)  | 0 (reference) | 0.24 (-0.05, 0.54) | 0.69 (0.41, 0.97) |
| **High hormone receptor** |               |                    |                   |
| Deaths/N (Risk)           | 5/55 (0.09)   | 17/74 (0.23)       | 9/15 (0.60)       |
| Risk difference (95% CI)  | 0 (reference) | 0.14 (0.02, 0.26)  | 0.51 (0.25, 0.77) |

## Why do I get an error?

R’s error messages can be frustrating. When using rifttable, these are
the typical sources of errors:

- Clerical errors in variable names and arguments. There is no magic
  here except double-checking the code.
- Missing data. See below: [How do I handle missing
  data?](#how-do-i-handle-missing-data).
- Discrepancy between estimator (`type`) and the data. For example,
  `type = "mean"` will not work on a categorical (factor) variable.
- Models that fail to converge. For example, one may be trying to
  estimate a risk ratio with 0 outcomes in the reference category, or
  adjusting for 20 covariates in a Cox model with 5 events overall.
  (Sometimes such attempts will “just” return warning messages – it is
  still worth rethinking the modeling strategy.)

To identify where an error is coming from, start simple. Comment out all
but one line of the `design`, putting `#` at the beginning of the line.
Start with a line that gives basic descriptive data, such as
`type = "total"`, `type = "outcomes"` or `type = "events/time"`, and
re-run
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md).
Then add more lines with descriptive estimators, one by one. At the end,
add lines that fit models, such as `type = "hr"`.

## What is the `design`?

The `design` that the
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
function takes as input is simply a dataset that defines how the table
should look like when
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
has processed the `data`.

The `design` can be constructed in many different ways. All lead to the
same table:

1.  **A dataset (tibble) defined using
    [`tribble()`](https://tibble.tidyverse.org/reference/tribble.html)**

    ``` r
    design1 <- tribble(
      ~label,   ~exposure, ~outcome, ~type,
      "N",      "stage",   "death",  "total",
      "Deaths", "stage",   "death",  "outcomes"
    )
    design1
    #> # A tibble: 2 × 4
    #>   label  exposure outcome type    
    #>   <chr>  <chr>    <chr>   <chr>   
    #> 1 N      stage    death   total   
    #> 2 Deaths stage    death   outcomes
    rifttable(
      design = design1,
      data = breastcancer
    ) |>
      rt_gt()
    ```

    | Stage  | Stage I | Stage II | Stage III |
    |:-------|:--------|:---------|:----------|
    | N      | 67      | 96       | 29        |
    | Deaths | 7       | 26       | 21        |

2.  **A dataset (tibble) defined using
    [`tibble()`](https://tibble.tidyverse.org/reference/tibble.html)**

    ``` r
    design2 <- tibble(
      label = c("N", "Deaths"),
      exposure = "stage",
      outcome = "death",
      type = c("total", "outcomes")
    )
    design2
    #> # A tibble: 2 × 4
    #>   label  exposure outcome type    
    #>   <chr>  <chr>    <chr>   <chr>   
    #> 1 N      stage    death   total   
    #> 2 Deaths stage    death   outcomes
    rifttable(
      design = design2,
      data = breastcancer
    ) |>
      rt_gt()
    ```

    | Stage  | Stage I | Stage II | Stage III |
    |:-------|:--------|:---------|:----------|
    | N      | 67      | 96       | 29        |
    | Deaths | 7       | 26       | 21        |

3.  **Concatenating tibbles, then editing with
    [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)**

    ``` r
    design3 <- bind_rows(
      tibble( # row 1
    label = "N",
    type = "total"
      ),
      tibble( # row 2
    label = "Deaths",
    type = "outcomes"
      )
    ) |>
      mutate( # elements that are the same for all rows
    exposure = "stage",
    outcome = "death"
      )
    design3
    #> # A tibble: 2 × 4
    #>   label  type     exposure outcome
    #>   <chr>  <chr>    <chr>    <chr>  
    #> 1 N      total    stage    death  
    #> 2 Deaths outcomes stage    death
    rifttable(
      design = design3,
      data = breastcancer
    ) |>
      rt_gt()
    ```

    | Stage  | Stage I | Stage II | Stage III |
    |:-------|:--------|:---------|:----------|
    | N      | 67      | 96       | 29        |
    | Deaths | 7       | 26       | 21        |

4.  **For descriptive tables: Use
    [`table1_design()`](https://stopsack.github.io/rifttable/reference/table1_design.md)**

    ``` r
    design4 <- breastcancer |>
      table1_design(
    death, # the total count will automatically be included
    by = stage
      )
    design4
    #> # A tibble: 2 × 4
    #>   label outcome type            exposure
    #>   <chr> <chr>   <chr>           <chr>   
    #> 1 N     ""      total           stage   
    #> 2 Death "death" outcomes (risk) stage
    rifttable(
      design = design4,
      data = breastcancer
    ) |>
      rt_gt()
    ```

    | Stage | Stage I | Stage II | Stage III |
    |:------|:--------|:---------|:----------|
    | N     | 67      | 96       | 29        |
    | Death | 7 (10%) | 26 (27%) | 21 (72%)  |

    See a separate overview about [a descriptive Table
    1](https://stopsack.github.io/rifttable/articles/table1.md).

5.  **External datasets**

    The `design` could even be written out in an external dataset that
    can be loaded with `readr::read_csv()` (for CSV files) or
    `readxl::read_excel()` (for Excel sheets).

## How do I handle missing data?

rifttable tries to make as few assumptions as possible about how the
user wants to treat missing data.

- Missing values in **exposure**: By default, missing vales (`NA`) in
  the exposure are displayed as a separate exposure category for
  descriptive statistics (e.g., `type = "total"` or `type = "mean"`).
  They are omitted by comparative estimators (e.g., regression models).
  To change this behavior, call
  [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
  with the argument `exposure_levels = "nona"`.
- Missing values in **outcome**: By default, descriptive statistics will
  be missing (e.g., results will be `--` or `NA`), and regression models
  will use the non-missing observations. To exclude observations with
  missing outcome values altogether, add `na_rm = TRUE` to (specific
  rows of) the `design`.
- Missing values in **confounders**: Applies only to regression models,
  which typically exclude observations with missing values.
- Missing values in the **effect modifier**: Stratified and joint models
  are only shown for the specified `stratum` of the `effect_modifier`.
  To include observations with the missing effect modifier, add `NA` to
  the requested stratum in the `design`, e.g.,
  `effect_modifier = "bmi", stratum = c("<25", NA)`.

## How do I add overall statistics?

Use the `overall` argument to show descriptive data for the entire
`data` set. Inferential estimators showing comparisons between exposure
categories will be blank there.

``` r
rifttable(
  design = design,
  data = breastcancer,
  overall = TRUE
) |>
  rt_gt() # obtain formatted output
```

| Summary                   | Overall       | Stage I       | Stage II           | Stage III         |
|:--------------------------|:--------------|:--------------|:-------------------|:------------------|
| **Overall**               |               |               |                    |                   |
| Deaths/N                  | 54/192        | 7/67          | 26/96              | 21/29             |
| Risk                      | 0.28          | 0.10          | 0.27               | 0.72              |
| Risk ratio (95% CI)       |               | 1 (reference) | 2.59 (1.20, 5.6)   | 6.9 (3.3, 14)     |
| Risk difference (95% CI)  |               | 0 (reference) | 0.17 (0.05, 0.28)  | 0.62 (0.44, 0.80) |
|                           |               |               |                    |                   |
| **Low hormone receptor**  |               |               |                    |                   |
| Deaths/N (Risk)           | 23/48 (0.48)  | 2/12 (0.17)   | 9/22 (0.41)        | 12/14 (0.86)      |
| Risk difference (95% CI)  |               | 0 (reference) | 0.24 (-0.05, 0.54) | 0.69 (0.41, 0.97) |
| **High hormone receptor** |               |               |                    |                   |
| Deaths/N (Risk)           | 31/144 (0.22) | 5/55 (0.09)   | 17/74 (0.23)       | 9/15 (0.60)       |
| Risk difference (95% CI)  |               | 0 (reference) | 0.14 (0.02, 0.26)  | 0.51 (0.25, 0.77) |

## How do I test for trend?

Instead of *testing* a null hypothesis about a trend, rifttable proposes
*estimating* the difference in the outcome for a one-unit higher
exposure. This is also called a linear slope. Here, we estimate the risk
associated for stage that is one category higher.

``` r
rifttable(
  design = design |>
    mutate(trend = "stage_numeric"),
  data = breastcancer |>
    mutate(stage_numeric = as.numeric(stage))
) |>
  rt_gt() # obtain formatted output
```

| Stage                     | Stage I       | Stage II           | Stage III         | Trend             |
|:--------------------------|:--------------|:-------------------|:------------------|:------------------|
| **Overall**               |               |                    |                   |                   |
| Deaths/N                  | 7/67          | 26/96              | 21/29             |                   |
| Risk                      | 0.10          | 0.27               | 0.72              |                   |
| Risk ratio (95% CI)       | 1 (reference) | 2.59 (1.20, 5.6)   | 6.9 (3.3, 14)     | 2.50 (1.97, 3.2)  |
| Risk difference (95% CI)  | 0 (reference) | 0.17 (0.05, 0.28)  | 0.62 (0.44, 0.80) | 0.26 (0.19, 0.33) |
|                           |               |                    |                   |                   |
| **Low hormone receptor**  |               |                    |                   |                   |
| Deaths/N (Risk)           | 2/12 (0.17)   | 9/22 (0.41)        | 12/14 (0.86)      |                   |
| Risk difference (95% CI)  | 0 (reference) | 0.24 (-0.05, 0.54) | 0.69 (0.41, 0.97) | 0.32 (0.21, 0.43) |
| **High hormone receptor** |               |                    |                   |                   |
| Deaths/N (Risk)           | 5/55 (0.09)   | 17/74 (0.23)       | 9/15 (0.60)       |                   |
| Risk difference (95% CI)  | 0 (reference) | 0.14 (0.02, 0.26)  | 0.51 (0.25, 0.77) | 0.20 (0.11, 0.29) |

## How do I show multiple exposures in the same table?

Our simple toy dataset just has one exposure variable. For
demonstration, we just create a second variable, with two categories,
“Level 1” and “Level 2,” which is a simplified combination of the
`stage` and `receptor` variables.

We will flip the table `layout` from `"rows"` (the default) to `"cols"`
and concatenate two rifttables. We also need to give our new `exposure2`
variable the same label as `stage` to make sure results appear in the
same column.

``` r
breastcancer_2exposures <- breastcancer |>
  mutate(
    exposure2 = case_when(
      stage == "Stage I" |
        (stage == "Stage II" & receptor == "High") ~
        "Level 1",
      stage == "Stage III" |
        (stage == "Stage II" & receptor == "Low") ~
        "Level 2"
    )
  )

attr(breastcancer_2exposures$exposure2, which = "label") <- "Exposure"
attr(breastcancer_2exposures$stage, which = "label") <- "Exposure"

bind_rows(
  design |>
    mutate(exposure = "exposure2") |>
    slice(2:5) |>
    rifttable(
      data = breastcancer_2exposures,
      layout = "cols"
    ),
  design |>
    slice(2:5) |>
    rifttable(
      data = breastcancer_2exposures,
      layout = "cols"
    )
) |>
  rt_gt() # obtain formatted output
```

| Exposure  | Deaths/N | Risk | Risk ratio (95% CI) | Risk difference (95% CI) |
|:----------|:---------|:-----|:--------------------|:-------------------------|
| Level 1   | 24/141   | 0.17 | 1 (reference)       | 0 (reference)            |
| Level 2   | 30/51    | 0.59 | 3.5 (2.25, 5.3)     | 0.42 (0.27, 0.57)        |
| Stage I   | 7/67     | 0.10 | 1 (reference)       | 0 (reference)            |
| Stage II  | 26/96    | 0.27 | 2.59 (1.20, 5.6)    | 0.17 (0.05, 0.28)        |
| Stage III | 21/29    | 0.72 | 6.9 (3.3, 14)       | 0.62 (0.44, 0.80)        |

## How do I change how results are rounded?

By default, difference measures are being rounded to 2 decimal digits
(`0.01`), such as `type = "diff"`, the mean difference, or
`type = "quantreg"`, the median difference. The same goes for risk
measures, such as `type = "risk"`, unless shown as percentage points.
Ratio measures are also shown with 2 decimal digits, such as
`type = "hr"`, the hazard ratio, or `type = "fold"`, a ratio of
arithmetic means.

Rounding can be changed by setting the
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
arguments `diff_digits`, `risk_digits`, and `ratio_digits` globally for
the entire table.

``` r
design <- tribble(
  ~label,                     ~type,
  "Deaths/N",                 "outcomes/total",
  "Risk",                     "risk",
  "Risk ratio (95% CI)",      "rr",
  "Odds ratio (95% CI)",      "or",
  "Risk difference (95% CI)", "rd"
) |>
  mutate(
    exposure = "stage",
    outcome = "death"
  )

rifttable(
  design = design,
  data = breastcancer,
  ratio_digits = 3, # Many digits for ratios
  risk_digits = 1
) |> # Fewer digits for risks
  rt_gt() # obtain formatted output
```

| Stage                    | Stage I       | Stage II            | Stage III         |
|:-------------------------|:--------------|:--------------------|:------------------|
| Deaths/N                 | 7/67          | 26/96               | 21/29             |
| Risk                     | 0.1           | 0.3                 | 0.7               |
| Risk ratio (95% CI)      | 1 (reference) | 2.592 (1.195, 5.62) | 6.93 (3.32, 14.5) |
| Odds ratio (95% CI)      | 1 (reference) | 3.18 (1.351, 8.43)  | 22.5 (7.69, 75.0) |
| Risk difference (95% CI) | 0 (reference) | 0.2 (0.1, 0.3)      | 0.6 (0.4, 0.8)    |

As can be seen, ratios \> 3 are still shown with 1 fewer decimal, and
ratios \> 10 are shown with 2 fewer decimals ([Wilcox, *Epidemiology*
2004](https://doi.org/10.1097/01.ede.0000101026.08873.14) motivates
why). To disable such additional rounding of extremely high ratios:

``` r
rifttable(
  design = design,
  data = breastcancer,
  ratio_digits = 3,
  ratio_digits_decrease = NULL, # Do not round high ratios more
  risk_digits = 1
) |>
  rt_gt() # obtain formatted output
```

| Stage                    | Stage I       | Stage II             | Stage III              |
|:-------------------------|:--------------|:---------------------|:-----------------------|
| Deaths/N                 | 7/67          | 26/96                | 21/29                  |
| Risk                     | 0.1           | 0.3                  | 0.7                    |
| Risk ratio (95% CI)      | 1 (reference) | 2.592 (1.195, 5.621) | 6.931 (3.320, 14.471)  |
| Odds ratio (95% CI)      | 1 (reference) | 3.184 (1.351, 8.427) | 22.500 (7.691, 74.976) |
| Risk difference (95% CI) | 0 (reference) | 0.2 (0.1, 0.3)       | 0.6 (0.4, 0.8)         |

Additionally, rounding can be changed for each row, adding a column
`digits` to the rifttable `design`:

``` r
tribble(
  ~label,                     ~type,            ~digits,
  "Deaths/N",                 "outcomes/total", NA, # Uses rifttable default
  "Risk",                     "risk",           NA, # Uses risk_digits below
  "Risk ratio (95% CI)",      "",               NA,
  "  Rounded to 1 digit",     "rr",             1,
  "  Rounded to 2 digits",    "rr",             2,
  "Risk difference (95% CI)", "rd",             3
) |> # Overrides risk_digits
  mutate(
    exposure = "stage",
    outcome = "death"
  ) |>
  rifttable(
    data = breastcancer,
    risk_digits = 1
  ) |> # Fewer digits for risks, unless specified by "digits"
  rt_gt() # obtain formatted output
```

| Stage                    | Stage I       | Stage II             | Stage III            |
|:-------------------------|:--------------|:---------------------|:---------------------|
| Deaths/N                 | 7/67          | 26/96                | 21/29                |
| Risk                     | 0.1           | 0.3                  | 0.7                  |
| Risk ratio (95% CI)      |               |                      |                      |
| Rounded to 1 digit       | 1 (reference) | 2.6 (1.2, 6)         | 7 (3, 14)            |
| Rounded to 2 digits      | 1 (reference) | 2.59 (1.20, 5.6)     | 6.9 (3.3, 14)        |
| Risk difference (95% CI) | 0 (reference) | 0.166 (0.051, 0.282) | 0.620 (0.441, 0.798) |

## How can I create joint models?

By default, regression models will be fit separately for each stratum of
the `effect_modifier`.

Append `"_joint"` to `"hr"`, `"rr"`, `"rd"`, `"irr"`, `"irrrob"`,
`"diff"`, `"fold"`, `"foldlog"`, `"quantreg"`, or `"or"` to obtain
“joint” models for exposure and effect modifier that have a single
reference category.

Note that the joint model will be fit across all non-missing (`NA`)
strata of the effect modifier, even if the `design` table does not
request all strata be shown.

Compare stratified models to joint models for risk differences (for
simplicity of presentation, count data are omitted):

``` r
tribble(
  ~label,                       ~type,      ~stratum,
  "**Overall**",                "rd",       c("Low", "High"),
  "",                           "",         "",
  "**Stratified models**",      "",         "",
  "  Low hormone receptor",     "rd",       "Low",
  "  High hormone receptor",    "rd",       "High",
  "",                           "",         "",
  "**Joint models**",           "",         "",
  "  Low hormone receptor",     "rd_joint", "Low",
  "  High hormone receptor",    "rd_joint", "High"
) |>
  mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor"
  ) |>
  rifttable(data = breastcancer) |>
  rt_gt()
```

| Stage                 | Stage I            | Stage II           | Stage III         |
|:----------------------|:-------------------|:-------------------|:------------------|
| **Overall**           | 0 (reference)      | 0.17 (0.05, 0.28)  | 0.62 (0.44, 0.80) |
|                       |                    |                    |                   |
| **Stratified models** |                    |                    |                   |
| Low hormone receptor  | 0 (reference)      | 0.24 (-0.05, 0.54) | 0.69 (0.41, 0.97) |
| High hormone receptor | 0 (reference)      | 0.14 (0.02, 0.26)  | 0.51 (0.25, 0.77) |
|                       |                    |                    |                   |
| **Joint models**      |                    |                    |                   |
| Low hormone receptor  | 0.08 (-0.15, 0.30) | 0.32 (0.10, 0.54)  | 0.77 (0.57, 0.96) |
| High hormone receptor | 0 (reference)      | 0.14 (0.02, 0.26)  | 0.51 (0.25, 0.77) |

## How can I change the reference category?

The reference categories for exposure and effect modifier are always
their first factor levels. Compare the preceding example: `"High"` is,
alphabetically, before `"Low"`. To change the reference category, use
`forcats::fct_relevel()` or the base R alternative
[`relevel()`](https://rdrr.io/r/stats/relevel.html) on variables in the
`data` provided to
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md):

``` r
tribble(
  ~label,                       ~type,      ~stratum,
  "**Joint models**",           "",         "",
  "  Low hormone receptor",     "rd_joint", "Low",
  "  High hormone receptor",    "rd_joint", "High"
) |>
  mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor"
  ) |>
  rifttable(
    data = breastcancer |>
      mutate(
        receptor = relevel(
          factor(receptor), # Make "receptor" a factor in the first place
          ref = "Low"
        )
      )
  ) |> # Set new reference category
  rt_gt()
```

| Stage                 | Stage I             | Stage II           | Stage III         |
|:----------------------|:--------------------|:-------------------|:------------------|
| **Joint models**      |                     |                    |                   |
| Low hormone receptor  | 0 (reference)       | 0.24 (-0.05, 0.54) | 0.69 (0.41, 0.97) |
| High hormone receptor | -0.08 (-0.30, 0.15) | 0.06 (-0.17, 0.29) | 0.43 (0.11, 0.76) |

If a middle category of the exposure `stage` is desired as the
reference:

``` r
result_reordered <- tibble(
  label = "**RD (95% CI)**",
  type = "rd",
  exposure = "stage",
  outcome = "death"
) |>
  rifttable(
    data = breastcancer |>
      mutate(
        stage = relevel(
          stage,
          ref = "Stage II"
        )
      )
  )

result_reordered |>
  rt_gt()
```

| stage           | Stage II      | Stage I              | Stage III         |
|:----------------|:--------------|:---------------------|:------------------|
| **RD (95% CI)** | 0 (reference) | -0.17 (-0.28, -0.05) | 0.45 (0.27, 0.64) |

Using `forcats::fct_relevel()` may be preferable over
[`relevel()`](https://rdrr.io/r/stats/relevel.html), as it preserves the
variable label: here, the variable `stage` lost its label, `"Stage"`
starting with upper case S.

That results for “Stage II” are now listed first is probably
undesirable. Reorder the columns of the table that
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
produced to print results for “Stage I” first:

``` r
result_reordered |>
  select(stage, "Stage I", everything()) |>
  rt_gt()
```

| stage           | Stage I              | Stage II      | Stage III         |
|:----------------|:---------------------|:--------------|:------------------|
| **RD (95% CI)** | -0.17 (-0.28, -0.05) | 0 (reference) | 0.45 (0.27, 0.64) |

## How I do I change the level for confidence intervals?

Add a `ci` column to the `design`:

``` r
tribble(
  ~label,            ~type,                   ~ci,
  "Deaths/N (Risk)", "outcomes/total (risk)", NA,
  "Risk ratio",      "",                      NA,
  "  80% CI",        "rr",                    0.8,
  "  95% CI",        "rr",                    NA, # Defaults to 0.95
  "  99% CI",        "rr",                    0.99
) |>
  mutate(
    exposure = "stage",
    outcome = "death"
  ) |>
  rifttable(
    data = breastcancer,
    risk_percent = TRUE
  ) |>
  rt_gt() # obtain formatted output
```

| Stage           | Stage I       | Stage II         | Stage III      |
|:----------------|:--------------|:-----------------|:---------------|
| Deaths/N (Risk) | 7/67 (10%)    | 26/96 (27%)      | 21/29 (72%)    |
| Risk ratio      |               |                  |                |
| 80% CI          | 1 (reference) | 2.59 (1.56, 4.3) | 6.9 (4.3, 11)  |
| 95% CI          | 1 (reference) | 2.59 (1.20, 5.6) | 6.9 (3.3, 14)  |
| 99% CI          | 1 (reference) | 2.59 (0.94, 7.2) | 6.9 (2.63, 18) |

## How do I make rifttable calculate an estimand that is not built-in?

While the package provides a number of estimators commonly used in
epidemiology, it will never be able to include all possible estimators.
However, any custom estimate can be integrated into a rifttable by a
defining custom estimation function.

The subsequent example will reproduce the following basic rifttable,
which shows the mean age by sex, stratified by ECOG performance status,
in the `cancer` data set:

``` r
data(cancer, package = "survival")
cancer <- cancer |>
  tibble::as_tibble() |>
  mutate(
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    )
  )

design <- tibble::tibble(
  type = "mean",
  exposure = "sex",
  outcome = "age",
  effect_modifier = "ph.ecog",
  stratum = 1:2,
  label = paste0("ECOG PS ", stratum, ": mean age")
)

design |>
  rifttable(
    data = cancer,
    overall = TRUE
  ) |>
  rt_gt()
```

| Summary             | Overall | Male  | Female |
|:--------------------|:--------|:------|:-------|
| ECOG PS 1: mean age | 61.45   | 62.79 | 59.19  |
| ECOG PS 2: mean age | 66.22   | 65.00 | 67.90  |

Instead of relying on rifttable’s built-in estimator `type = "mean"`, we
will define a custom function that calculates the mean:

``` r
estimate_my_mean <- function(data, ...) {
  data |>
    group_by(.exposure) |>
    summarize(
      res = paste(
        round(
          mean(.outcome),
          digits = 3
        ),
        "yrs"
      )
    )
}
```

Use the custom function `my_mean` instead of the built-in `mean`:

``` r
design |> # Edit the previous design
  mutate(
    type = "my_mean", # Replace built-in "mean" by custom "my_mean"
    label = paste0(label, " (custom)")
  ) |>
  rifttable(
    data = cancer,
    overall = TRUE
  ) |>
  rt_gt()
```

| Summary                      | Overall    | Male       | Female     |
|:-----------------------------|:-----------|:-----------|:-----------|
| ECOG PS 1: mean age (custom) | 61.451 yrs | 62.789 yrs | 59.19 yrs  |
| ECOG PS 2: mean age (custom) | 66.22 yrs  | 65 yrs     | 67.905 yrs |

Specifications for custom functions:

- The function name must start with `estimate_`; this prefix is to be
  omitted when later calling the custom function within
  [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md).
- The `data` provided to
  [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
  will be available to the custom function as an argument named `data`.
- The `data` are already subsetted to the `stratum` of the
  `effect_modifier`, if applicable.
- Copies of key variables are accessible under the names, `.exposure`,
  `.outcome`, `.event`, `.time`, and `.time2`, as applicable.
- The function must accept all elements of a rifttable `design` (e.g.,
  `confounders`, `digits`, `na_rm`, etc.) and of the
  [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md)
  function (e.g., `reference`, `risk_percent`) as arguments. Many may
  not be relevant and can be captured in the argument list, `...` (see
  example).
- The function must return a tibble/data frame with one column for
  `.exposure`, with one row per exposure category, and one string column
  `res` for the estimate.
