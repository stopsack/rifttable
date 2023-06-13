
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated, Reproducible Generation of Results Tables: Bridging the Rift Between Epidemiologists and Their Data

<!-- badges: start -->

[![R-CMD-check](https://github.com/stopsack/rifttable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stopsack/rifttable/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Tables are the key format in which epidemiologists present their
results. Many results tables in applied studies merely show point
estimates and confidence intervals, or even p-values, from regression
models: a “growing rift between epidemiologists and their data”
([Rothman 2017](https://doi.org/10.1007/s10654-017-0314-3)). “Actual,”
descriptive data, such as counts stratified by exposure and a main
confounder or effect modifier, are often lacking.

**rifttable** creates presentation-ready results tables for
epidemiologists in an automated, reproducible fashion. The user provides
the final analytical dataset and specifies the design of the table, with
rows and/or columns defined by exposure(s), effect modifier(s), and
estimands as desired, allowing to show descriptors and inferential
estimates in one table – bridging the rift between epidemiologists and
their data, one table at a time.

## Installation

You can install the development version of rifttable directly from
GitHub:

``` r
remotes::install_github("stopsack/rifttable")
```

The installation procedure requires the {remotes} package, obtainable
via `install.packages("remotes")`.

## Example

``` r
library(rifttable)

example_design <- tibble::tribble(
  ~label,                       ~type,                   ~stratum,         
  "Overall",                    "",                      "",               
  "– Deaths/N",                 "outcomes/total",        c("Low", "High"), 
  "– Risk",                     "risk",                  c("Low", "High"), 
  "– Risk ratio (95% CI)",      "rr",                    c("Low", "High"), 
  "– Risk difference (95% CI)", "rd",                    c("Low", "High"), 
  "",                           "",                      "",               
  "Low hormone receptor",       "",                      "",               
  "– Deaths/N (Risk)",          "outcomes/total (risk)", "Low",           
  "– Risk difference (95% CI)", "rd",                    "Low",           
  "High hormone receptor",      "",                      "",               
  "– Deaths/N (Risk)",          "outcomes/total (risk)", "High",
  "– Risk difference (95% CI)", "rd",                    "High") %>%
  dplyr::mutate(
    exposure = "stage",
    outcome = "death",
    effect_modifier = "receptor")

rifttable(
  design = example_design,
  data = risks::breastcancer)
```

<div class="kable-table">

| Stage                      | Stage I       | Stage II           | Stage III         |
|:---------------------------|:--------------|:-------------------|:------------------|
| Overall                    |               |                    |                   |
| – Deaths/N                 | 7/67          | 26/96              | 21/29             |
| – Risk                     | 0.10          | 0.27               | 0.72              |
| – Risk ratio (95% CI)      | 1 (reference) | 2.59 (1.20, 5.6)   | 6.9 (3.3, 14)     |
| – Risk difference (95% CI) | 0 (reference) | 0.17 (0.05, 0.28)  | 0.62 (0.44, 0.80) |
|                            |               |                    |                   |
| Low hormone receptor       |               |                    |                   |
| – Deaths/N (Risk)          | 2/12 (0.17)   | 9/22 (0.41)        | 12/14 (0.86)      |
| – Risk difference (95% CI) | 0 (reference) | 0.24 (-0.05, 0.54) | 0.69 (0.41, 0.97) |
| High hormone receptor      |               |                    |                   |
| – Deaths/N (Risk)          | 5/55 (0.09)   | 17/74 (0.23)       | 9/15 (0.60)       |
| – Risk difference (95% CI) | 0 (reference) | 0.14 (0.02, 0.26)  | 0.51 (0.25, 0.77) |

</div>

For more examples, see the [Get Started
vignette](https://stopsack.github.io/rifttable/articles/rifttable.html).
