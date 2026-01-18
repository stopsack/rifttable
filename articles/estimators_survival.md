# Survival outcomes

## Example

The example uses the `cancer` data set from the survival package and
compares survival, defined by the time variable `time` (recoded to
years) and the event variable `status` (recoded to 1 = death, 0 =
censored), by sex.

``` r
library(rifttable)
data(cancer, package = "survival")

cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    # The exposure (here, 'sex') must be categorical (a factor)
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    time = time / 365.25, # transform to years
    status = status - 1
  )

tibble::tribble(
  ~label,                                  ~type,
  "**Absolute estimates**",                "",
  "*Counts and sums*",                     "",
  "  Observations, *N*",                   "total",
  "  Events, *n*",                         "events",
  "  Events/observations",                 "events/total",
  "  Events/person-years",                 "events/time",
  "*Follow-up*",                           "",
  "  Person-years",                        "time",
  "  Maximum follow-up, years",            "maxfu",
  "  Median follow-up, years",             "medfu",
  "  Median follow-up (IQR), years",       "medfu (iqr)",
  "*Rates*",                               "",
  "  Rate per 1000 person-years",          "rate",
  "  Rate per 1000 person-years (95% CI)", "rate (ci)",
  "  Events/py (rate per 1000 py)",        "events/time (rate)",
  "*Risks*",                               "",
  "  1-year survival",                     "surv",
  "  1-year survival (95% CI)",            "surv (ci)",
  "  1-year risk/cumulative incidence",    "cuminc",
  "  1-year risk (95% CI)",                "cuminc (ci)",
  "  Median survival, years",              "medsurv",
  "  Median survival (95 CI), years",      "medsurv (ci)",
  "",                                      "",
  "**Comparative estimates**",             "",
  "  1-year survival difference",          "survdiff",
  "  1-year risk difference",              "cumincdiff",
  "  1-year survival ratio",               "survratio",
  "  1-year risk ratio",                   "cumincratio",
  "  Hazard ratio (95% CI)",               "hr"
) |>
  dplyr::mutate(
    time = "time",
    event = "status",
    exposure = "sex",
    arguments = list(list(timepoint = 1))
  ) |>
  rifttable(
    data = cancer,
    overall = TRUE
  ) |>
  rt_gt()
```

| Summary                             | Overall               | Male                   | Female               |
|:------------------------------------|:----------------------|:-----------------------|:---------------------|
| **Absolute estimates**              |                       |                        |                      |
| *Counts and sums*                   |                       |                        |                      |
| Observations, *N*                   | 228                   | 138                    | 90                   |
| Events, *n*                         | 165                   | 112                    | 53                   |
| Events/observations                 | 165/228               | 112/138                | 53/90                |
| Events/person-years                 | 165/191               | 112/107                | 53/84                |
| *Follow-up*                         |                       |                        |                      |
| Person-years                        | 191                   | 107                    | 84                   |
| Maximum follow-up, years            | 2.80                  | 2.80                   | 2.64                 |
| Median follow-up, years             | 1.61                  | 2.30                   | 1.45                 |
| Median follow-up (IQR), years       | 1.61 (0.82, 2.64)     | 2.30 (1.11, 2.77)      | 1.45 (0.76, 2.25)    |
| *Rates*                             |                       |                        |                      |
| Rate per 1000 person-years          | 866.0                 | 1046.6                 | 634.6                |
| Rate per 1000 person-years (95% CI) | 866.0 (743.4, 1008.7) | 1046.6 (869.7, 1259.6) | 634.6 (484.8, 830.6) |
| Events/py (rate per 1000 py)        | 165/191 (866.0)       | 112/107 (1046.6)       | 53/84 (634.6)        |
| *Risks*                             |                       |                        |                      |
| 1-year survival                     | 0.41                  | 0.34                   | 0.53                 |
| 1-year survival (95% CI)            | 0.41 (0.34, 0.49)     | 0.34 (0.26, 0.43)      | 0.53 (0.42, 0.66)    |
| 1-year risk/cumulative incidence    | 0.59                  | 0.66                   | 0.47                 |
| 1-year risk (95% CI)                | 0.59 (0.51, 0.66)     | 0.66 (0.57, 0.74)      | 0.47 (0.34, 0.58)    |
| Median survival, years              | 0.85                  | 0.74                   | 1.17                 |
| Median survival (95 CI), years      | 0.85 (0.78, 0.99)     | 0.74 (0.58, 0.85)      | 1.17 (0.95, 1.51)    |
|                                     |                       |                        |                      |
| **Comparative estimates**           |                       |                        |                      |
| 1-year survival difference          |                       | 0 (reference)          | 0.19 (0.05, 0.34)    |
| 1-year risk difference              |                       | 0 (reference)          | -0.19 (-0.33, -0.04) |
| 1-year survival ratio               |                       | 1 (reference)          | 1.57 (1.12, 2.19)    |
| 1-year risk ratio                   |                       | 1 (reference)          | 0.71 (0.55, 1.00)    |
| Hazard ratio (95% CI)               |                       | 1 (reference)          | 0.59 (0.42, 0.82)    |

## Absolute estimates per exposure category

| `type`                 | Description                                                                                                                                                                                                                                                                                                                                                                                               | Options (`arguments =`)                                                    |
|------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------|
| `"cuminc"`             | Cumulative incidence (“risk”) from the Kaplan-Meier estimator or, if competing risks are present, its generalized form, the Aalen-Johansen estimator. If no time point is provided, returns cumulative incidence at end of follow-up. Change between display as proportion or percent using the parameter `risk_percent` of [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md). | `timepoint`, `id`. Example: `list(timepoint = 2.5)` to show 2.5-year risk. |
| `"cuminc (ci)"`        | Cumulative incidence (“risk”), as above, with confidence intervals (default: 95%; Greenwood standard errors with log transformation, the default of the survival package/[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)).                                                                                                                                                         | See `"cuminc"`.                                                            |
| `"events"`             | Event count.                                                                                                                                                                                                                                                                                                                                                                                              |                                                                            |
| `"events/time"`        | Events slash person-time.                                                                                                                                                                                                                                                                                                                                                                                 |                                                                            |
| `"events/time (rate)"` | A combination: Events slash time followed by rate in parentheses.                                                                                                                                                                                                                                                                                                                                         |                                                                            |
| `"events/total"`       | Events slash number of observations.                                                                                                                                                                                                                                                                                                                                                                      |                                                                            |
| `"rate"`               | Event rate: event count divided by person-time, multiplied by the [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md) parameter `factor`.                                                                                                                                                                                                                                        |                                                                            |
| `"rate (ci)"`          | Event rate with confidence interval (default: 95%; Poisson-type large-sample interval).                                                                                                                                                                                                                                                                                                                   |                                                                            |
| `"surv"`               | Survival from the Kaplan-Meier estimator. Not estimated if competing risks are present. If no time point is provided, returns survival at end of follow-up. Change between display as proportion or percent using the parameter `risk_percent` of [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md).                                                                           | See `"cuminc"`.                                                            |
| `"surv (ci)"`          | Survival from the Kaplan-Meier estimator, as above, with confidence interval (default: 95%; Greenwood standard errors with log transformation, the default of the survival package/[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)).                                                                                                                                               | See `"cuminc"`.                                                            |
| `"time"`               | Person-time.                                                                                                                                                                                                                                                                                                                                                                                              |                                                                            |
| `"maxfu"`              | Maximum follow-up time.                                                                                                                                                                                                                                                                                                                                                                                   |                                                                            |
| `"medfu"`              | Median follow-up (“reverse Kaplan-Meier”), equals median survival for censoring. If competing risks are present, events other than the event of interest are also considered censoring to estimate the median follow-up for the event of interest.                                                                                                                                                        |                                                                            |
| `"medfu (iqr)"`        | Median and interquartile range for follow-up, as above.                                                                                                                                                                                                                                                                                                                                                   |                                                                            |
| `"medsurv"`            | Median survival.                                                                                                                                                                                                                                                                                                                                                                                          |                                                                            |
| `"medsurv (ci)"`       | Median survival with confidence interval (default: 95%).                                                                                                                                                                                                                                                                                                                                                  |                                                                            |

## Comparative estimates with confidence intervals

| `type`          | Description                                                                                                                                                                                                                                                                                                         | Options (`arguments =`)                                                                                                                                                                                                                                                                |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `"cumincdiff"`  | Difference in cumulative incidence (risk difference) from Kaplan-Meier estimator or, if competing risks are present, its generalized form, the Aalen-Johansen estimator, with confidence interval (default: 95%). Uses [`rifttable::survdiff_ci()`](https://stopsack.github.io/rifttable/reference/survdiff_ci.md). | `timepoint`. Example: `list(timepoint = 2.5)` to calculate difference in 2.5-year risk.                                                                                                                                                                                                |
| `"cumincratio"` | Ratio of cumulative incidence (risk ratio), with confidence interval (default: 95%), similar to `"cumincdiff"`.                                                                                                                                                                                                     | `timepoint`                                                                                                                                                                                                                                                                            |
| `"hr"`          | Hazard ratio from Cox proportional hazards regression, with confidence interval (default: 95%). If competing events are present, hazard ratios are cause-specific for the event of interest.                                                                                                                        | `list(robust = TRUE)` for robust (sandwich) standard errors. Use `"+ cluster(id_variable)"` in `confounders` to obtain clustering specifically for Cox models, or use the `id` argument of the main [`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md) call. |
| `"survdiff"`    | Difference in survival from Kaplan-Meier estimator, with confidence interval (default: 95%). Not estimated if competing risks are present. Uses [`rifttable::survdiff_ci()`](https://stopsack.github.io/rifttable/reference/survdiff_ci.md).                                                                        | See `"cumincdiff"`.                                                                                                                                                                                                                                                                    |
| `"survratio"`   | Ratio of survival from Kaplan-Meier estimator, with confidence interval (default: 95%), similar to `"survdiff"`.                                                                                                                                                                                                    | See `"cumincdiff"`.                                                                                                                                                                                                                                                                    |

## Competing events

With only one event type, the `event` variable only has two levels:
censoring, typically encoded as `0`, and the event, typically encoded as
`1`.

With competing events, the `event` variable will have additional levels.
The [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
function used by rifttable assumes that the first-ordered level
represents censoring and the others are different non-censoring events.
For example, if the event variable is a factor, then `"Censoring"` needs
to be the first of the factor’s
[`levels()`](https://rdrr.io/r/base/levels.html).

It is necessary to specify the event of interest in the `design` if
competing events are present. For example, if the `event` variable is a
factor variable `status_competing`, with levels `"Censored"`,
`"Outcome of interest"`, and `"Other-cause death"`, then specify
`event = "status_competing@Outcome of interest"` in the table design.

See the tables above for how competing events are handled. When no
details are noted, the event of interest is recoded as the sole event
and other events are considered censoring.

## Weighted estimates

The name of a weights variable, for example inverse-probability weights,
can be provided via the column `weights` of the `design` table. Weights,
if present, are used by all comparative estimators of survival as well
as by `type = "cuminc"` and `type = "surv"`, and are ignored otherwise.

## Clustering and robust variance

An `id` variable identifying individuals in the `data` must be provided
when `time` and `time2` are used in the setting of competing events, or
when non-integer weights are present. If no `id` variable is provided,
each row is taken as one individual. Robust variance will then
automatically be calculated by estimators of survival/cumulative
incidence and Cox models.
