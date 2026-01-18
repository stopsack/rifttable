# Estimate Difference in Survival or Cumulative Incidence and Confidence Interval

This function estimates the unadjusted difference or ratio in survival
or cumulative incidence (risk) at a given time point based on the
difference between per-group Kaplan-Meier estimates or, if competing
events are present, Aalen-Johansen estimates of the cumulative
incidence.

For constructing confidence limits, the MOVER approach described by Zou
and Donner (2008) is used, with estimation on the log scale for ratios.

## Usage

``` r
survdiff_ci(
  formula,
  data,
  time,
  estimand = c("survival", "cuminc"),
  type = c("diff", "ratio"),
  approach = c("mover", "squareadd"),
  conf.level = 0.95,
  event_type = NULL,
  id_variable = NULL,
  weighted = FALSE
)
```

## Arguments

- formula:

  Formula of a survival object using
  [`Surv`](https://rdrr.io/pkg/survival/man/Surv.html) of the form,
  `Surv(time, event) ~ group`. The exposure variable (here, `group`)
  must be categorical with at least 2 categories.

- data:

  Data set.

- time:

  Time point to estimate survival difference at.

- estimand:

  Optional. Estimate difference in survival (`"survival"`) or cumulative
  incidence (`"cuminc"`)? This parameter affects the sign of the
  differences. Only `"cuminc"` is available if competing events are
  present, i.e., `event_type` is not `NULL`. Defaults to `"survival"`.

- type:

  Optional. Estimate differences (`"diff"`) or ratio (`"ratio"`) of
  survival or cumulative incidence? Defaults to `"diff"`.

- approach:

  Optional. For estimating confidence limits of differences, use the
  MOVER approach based on upper and lower confidence limits of each
  group (`"mover"`), or square-and-add standard errors (`"squareadd"`)?
  Defaults to `"mover"`. (For confidence limits of ratios, this argument
  is ignored and MOVER is used.)

- conf.level:

  Optional. Confidence level. Defaults to `0.95`.

- event_type:

  Optional. Event type (level) for event variable with competing events.
  Defaults to `NULL`.

- id_variable:

  Optional. Identifiers for individual oberversations, required if data
  are clustered, or if competing events and time/time2 notation are used
  concomitantly.

- weighted:

  Optional. Weigh survival curves, e.g. for inverse-probability
  weighting, before estimating differences or ratios? If `TRUE`, the
  `data` must contain a variable called `.weights`. Defaults to `FALSE`.

## Value

Tibble in [`tidy`](https://generics.r-lib.org/reference/tidy.html)
format:

- `term` Name of the exposure stratum.

- `estimate` Difference or ratio.

- `std.error` Large-sample standard error of the difference in survival
  functions (see References). For each survival function, Greenwood
  standard errors with log transformation are used, the default of the
  survival
  package/[`survfit`](https://rdrr.io/pkg/survival/man/survfit.html)).

- `statistic` z statistic.

- `p.value` From the z statistic.

- `conf.low` Lower confidence limit

- `conf.high` Upper confidence limit

## References

Com-Nougue C, Rodary C, Patte C. How to establish equivalence when data
are censored: a randomized trial of treatments for B non-Hodgkin
lymphoma. Stat Med 1993;12:1353–64.
[doi:10.1002/sim.4780121407](https://doi.org/10.1002/sim.4780121407)

Altman DG, Andersen PK. Calculating the number needed to treat for
trials where the outcome is time to an event. BMJ 1999;319:1492–5.
[doi:10.1136/bmj.319.7223.1492](https://doi.org/10.1136/bmj.319.7223.1492)

Zou GY, Donner A. Construction of confidence limits about effect
measures: A general approach. Statist Med 2008;27:1693–1702.
[doi:10.1002/sim.3095](https://doi.org/10.1002/sim.3095)

## Examples

``` r
# Load 'cancer' dataset from survival package (Used in all examples)
data(cancer, package = "survival")

cancer <- cancer |>
  dplyr::mutate(
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    status = status - 1
  )

survdiff_ci(
  formula = survival::Surv(time = time, event = status) ~ sex,
  data = cancer,
  time = 365.25
)
#> # A tibble: 1 × 7
#>   term   estimate std.error statistic p.value conf.low conf.high
#>   <chr>     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 Female    0.190    0.0750      2.54 0.00557   0.0475     0.342
# Females have 19 percentage points higher one-year survival than males
# (95% CI, 5 to 34 percentage points).
```
