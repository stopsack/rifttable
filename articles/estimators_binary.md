# Binary outcomes

## Example

The example uses the `breastcancer` data set from the risks package and
compares the risk of death (a binary variable in this case) by
categories of cancer stage.

``` r
library(rifttable)
library(dplyr)
data(breastcancer, package = "risks")

tibble::tribble(
  ~label,                      ~type,
  "**Absolute estimates**",    "",
  "Observations",              "total",
  "Outcomes",                  "outcomes",
  "Outcomes/Total",            "outcomes/total",
  "Cases/Controls",            "cases/controls",
  "Risk",                      "risk",
  "Risk (95% CI)",             "risk (ci)",
  "Outcomes (Risk)",           "outcomes (risk)",
  "Outcomes/Total (Risk)",     "outcomes/total (risk)",
  "",                          "",
  "**Comparative estimates**", "",
  "Risk ratio (95% CI)",       "rr",
  "Risk difference (95% CI)",  "rd",
  "Odds ratio (95% CI)",       "or"
) |>
  mutate(
    exposure = "stage",
    outcome = "death"
  ) |>
  rifttable(
    data = breastcancer,
    overall = TRUE
  ) |>
  rt_gt() # Formatted output
```

| Summary                   | Overall           | Stage I           | Stage II          | Stage III         |
|:--------------------------|:------------------|:------------------|:------------------|:------------------|
| **Absolute estimates**    |                   |                   |                   |                   |
| Observations              | 192               | 67                | 96                | 29                |
| Outcomes                  | 54                | 7                 | 26                | 21                |
| Outcomes/Total            | 54/192            | 7/67              | 26/96             | 21/29             |
| Cases/Controls            | 54/138            | 7/60              | 26/70             | 21/8              |
| Risk                      | 0.28              | 0.10              | 0.27              | 0.72              |
| Risk (95% CI)             | 0.28 (0.22, 0.35) | 0.10 (0.05, 0.20) | 0.27 (0.19, 0.37) | 0.72 (0.54, 0.85) |
| Outcomes (Risk)           | 54 (0.28)         | 7 (0.10)          | 26 (0.27)         | 21 (0.72)         |
| Outcomes/Total (Risk)     | 54/192 (0.28)     | 7/67 (0.10)       | 26/96 (0.27)      | 21/29 (0.72)      |
|                           |                   |                   |                   |                   |
| **Comparative estimates** |                   |                   |                   |                   |
| Risk ratio (95% CI)       |                   | 1 (reference)     | 2.59 (1.20, 5.6)  | 6.9 (3.3, 14)     |
| Risk difference (95% CI)  |                   | 0 (reference)     | 0.17 (0.05, 0.28) | 0.62 (0.44, 0.80) |
| Odds ratio (95% CI)       |                   | 1 (reference)     | 3.2 (1.35, 8.4)   | 22 (7.7, 75)      |

## Absolute estimates per exposure category

| `type`                    | Description                                                                                                                                                                                                                                                               | Options (`arguments =`) |
|---------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------|
| `"cases/controls"`        | Cases and non-cases (events and non-events); useful for case-control studies.                                                                                                                                                                                             |                         |
| `"outcomes"`              | Outcome count.                                                                                                                                                                                                                                                            |                         |
| `"outcomes (risk)"`       | A combination: Outcomes followed by risk in parentheses.                                                                                                                                                                                                                  |                         |
| `"outcomes/total (risk)"` | A combination: Outcomes slash total followed by risk in parentheses.                                                                                                                                                                                                      |                         |
| `"risk"`                  | Risk (or prevalence), calculated as a proportion, *i.e.*, outcomes divided by number of observations. Change between display as proportion or percent using the parameter `risk_percent` of [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md). |                         |
| `"risk (ci)"`             | Risk with confidence interval (default: 95%): Wilson score interval for binomial proportions, see [`rifttable::scoreci()`](https://stopsack.github.io/rifttable/reference/scoreci.md).                                                                                    |                         |

## Comparative estimates with confidence intervals

| `type`  | Description                                                                                                                                                       | Options (`arguments =`)                                                                                                           |
|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `"irr"` | Incidence rate ratio for count outcomes from Poisson regression model, with confidence interval (default: 95%).                                                   |                                                                                                                                   |
| `"or"`  | Odds ratio from logistic regression, with confidence interval (default: 95%).                                                                                     |                                                                                                                                   |
| `"rd"`  | Risk difference (or prevalence difference) from risks::riskdiff(), with confidence interval (default: 95%).                                                       | `list(approach = "margstd_boot", bootrepeats = 2000)` to request model fitting via marginal standardization with 2000 bootstraps. |
| `"rr"`  | Risk ratio (or prevalence ratio) from [`risks::riskratio()`](https://stopsack.github.io/risks/reference/riskratio.html), with confidence interval (default: 95%). | See `"rd"`.                                                                                                                       |
