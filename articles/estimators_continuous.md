# Continuous outcomes

## Example

The example uses the `cancer` data from the survival package. To compare
patient age by ECOG performance status, recode the variable `ph.ecog` to
be categorical (a `factor`) and exclude patients with rare elevated
`ecog.ps`:

``` r
library(dplyr)
library(rifttable)
data(cancer, package = "survival")
cancer <- cancer |>
  filter(ph.ecog < 3) |>
  mutate(ph.ecog = factor(ph.ecog))
attr(cancer$ph.ecog, which = "label") <- "ECOG performance status"

tribble(
  ~label,                                       ~type,
  "**Absolute estimates**",                     "",
  "Observations",                               "total",
  "Sum",                                        "sum",
  "Range",                                      "range",
  "Mean",                                       "",
  "  Mean (i.e., arithmetic mean)",             "mean",
  "  Mean (95% CI)",                            "mean (ci)",
  "  Mean (standard deviation)",                "mean (sd)",
  "  Geometric mean",                           "geomean",
  "Median",                                     "median",
  "Median (interquartile range)",               "median (iqr)",
  "",                                           "",
  "**Comparative estimates**",                  "",
  "Mean difference (95% CI)",                   "diff",
  "Median difference (95% CI)",                 "quantreg",
  "Mean ratio",                                 "",
  "  of arithmetic means",                      "fold",
  "  of arithmetic means, empirical SE",        "irrrob",
  "  of geometric means",                       "foldlog"
) |>
  mutate(
    exposure = "ph.ecog",
    outcome = "age"
  ) |>
  rifttable(
    data = cancer,
    diff_digits = 1, # Suppress unnecessary precision in most estimates
    # Show extraneous digits to highlight (minor) differences in ratio  models:
    ratio_digits = 3,
    overall = TRUE
  ) |>
  rt_gt() # obtain formatted output
```

| Summary                           | Overall           | 0                 | 1                    | 2                    |
|:----------------------------------|:------------------|:------------------|:---------------------|:---------------------|
| **Absolute estimates**            |                   |                   |                      |                      |
| Observations                      | 226               | 63                | 113                  | 50                   |
| Sum                               | 14108.0           | 3853.0            | 6944.0               | 3311.0               |
| Range                             | 39.0, 82.0        | 39.0, 82.0        | 40.0, 80.0           | 48.0, 77.0           |
| Mean                              |                   |                   |                      |                      |
| Mean (i.e., arithmetic mean)      | 62.4              | 61.2              | 61.5                 | 66.2                 |
| Mean (95% CI)                     | 62.4 (61.2, 63.6) | 61.2 (58.8, 63.5) | 61.5 (59.8, 63.1)    | 66.2 (64.0, 68.5)    |
| Mean (standard deviation)         | 62.4 (9.1)        | 61.2 (9.6)        | 61.5 (8.9)           | 66.2 (8.1)           |
| Geometric mean                    | 61.7              | 60.4              | 60.8                 | 65.7                 |
| Median                            | 63.0              | 61.0              | 63.0                 | 68.0                 |
| Median (interquartile range)      | 63.0 (56.0, 69.0) | 61.0 (56.5, 68.0) | 63.0 (55.0, 68.0)    | 68.0 (60.2, 73.0)    |
|                                   |                   |                   |                      |                      |
| **Comparative estimates**         |                   |                   |                      |                      |
| Mean difference (95% CI)          |                   | 0 (reference)     | 0.3 (-2.5, 3.1)      | 5.1 (1.7, 8.4)       |
| Median difference (95% CI)        |                   | 0 (reference)     | 2.0 (-4.8, 8.8)      | 7.0 (1.3, 12.2)      |
| Mean ratio                        |                   |                   |                      |                      |
| of arithmetic means               |                   | 1 (reference)     | 1.005 (0.961, 1.051) | 1.083 (1.028, 1.140) |
| of arithmetic means, empirical SE |                   | 1 (reference)     | 1.005 (0.959, 1.053) | 1.083 (1.029, 1.139) |
| of geometric means                |                   | 1 (reference)     | 1.007 (0.961, 1.055) | 1.088 (1.029, 1.151) |

## Absolute estimates per exposure category

| `type`           | Description                      | Options (`arguments =`) |
|------------------|----------------------------------|-------------------------|
| `"total"`        | Count of observations.           |                         |
| `"sum"`          | Sum.                             |                         |
| `"range"`        | Range: Minimum to maximum value. |                         |
| `"mean"`         | Mean (arithmetic mean).          |                         |
| `"mean (ci)"`    | Mean and CI (default: 95%).      |                         |
| `"mean (sd)"`    | Mean and standard deviation.     |                         |
| `"geomean"`      | Geometric mean.                  |                         |
| `"median"`       | Median.                          |                         |
| `"median (iqr)"` | Median and interquartile range.  |                         |

## Comparative estimates with confidence intervals

| `type`       | Description                                                                                                                                                                          | Options (`arguments =`)                                                  |
|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| `"diff"`     | Mean difference from linear model.                                                                                                                                                   |                                                                          |
| `"fold"`     | Fold change from generalized linear model with log link (i.e., ratio of arithmetic means).                                                                                           |                                                                          |
| `"foldlog"`  | Fold change from linear model after log transformation of the outcome (*i.e.*, ratio of geometric means).                                                                            |                                                                          |
| `"irrrob"`   | Fold change from generalized linear model with Poisson distribution and log link and robust (sandwich) standard errors                                                               |                                                                          |
| `"quantreg"` | Quantile difference from quantile regression using [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) with `method = "fn"`. By default, this is the difference in medians. | `list(tau = 0.75)` to change the quantile to, e.g., the 75th percentile. |

## More on ratios of continuous outcomes

Three types of ratios for continuous outcomes are implemented in
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md):

| `type =`    | Ratio of …       | Model                                                                                                                                     | Use                                                                                                                                                                           |
|-------------|------------------|-------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `"fold"`    | Arithmetic means | Generalized linear model with Gaussian distribution and log link: `glm(y ~ x, family = gaussian(link = "log"))`                           | Tolerates `0` in the `outcome` variable. May be informative if outcome is normally distributed without transformation.                                                        |
| `"irrrob"`  | Arithmetic means | Generalized linear model with Poisson distribution and log link: `glm(y ~ x, family = poisson())`, with robust (sandwich) standard errors | Tolerates `0` in the `outcome` variable. May be informative if outcome is normally distributed without transformation.                                                        |
| `"foldlog"` | Geometric means  | Linear model with log-transformed outcome: `lm(log(y) ~ x)`                                                                               | Does not tolerate `0` in the `outcome` variable, as `log(0)` is undefined (R returns `-Inf`). May be informative if outcome is normally distributed after log transformation. |

In all models, after exponentiation, beta coefficients can be
interpreted as ratios. rifttable automatically does all necessary
transformations.

In the `cancer` data, ratios of usual (arithmetic) means of age could be
considered informative, given that `hist(cancer$age)` etc. does not show
a major skew in this outcome.
