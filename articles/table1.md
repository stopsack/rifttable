# A Descriptive Table 1

## Loading the package

After installation, load the package with:

``` r
library(rifttable)
```

## Data handling

This example uses the `cancer` dataset from the survival package.

``` r
library(dplyr) # for data handling
data(cancer, package = "survival")
cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    sex = factor(
      sex,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    ph.ecog = factor(ph.ecog)
  )
```

## Generate a table design

rifttable’s
[`table1_design()`](https://stopsack.github.io/rifttable/reference/table1_design.md)
generates the `design` table that can then be passed on to
[`rifttable()`](https://stopsack.github.io/rifttable/reference/rifttable.md).

``` r
design <- cancer |>
  table1_design(
    age, ph.ecog, ph.karno, pat.karno, # leave empty to include all variables
    by = sex
  )

# Print the design for illustration purposes
design
#> # A tibble: 12 × 5
#>    label       outcome          type              na_rm exposure
#>    <chr>       <chr>            <chr>             <lgl> <chr>   
#>  1 "N"         ""               "total"           NA    sex     
#>  2 "age"       "age"            "median (iqr)"    FALSE sex     
#>  3 "ph.ecog"   ""               ""                FALSE sex     
#>  4 "  0"       "ph.ecog@0"      "outcomes (risk)" TRUE  sex     
#>  5 "  1"       "ph.ecog@1"      "outcomes (risk)" TRUE  sex     
#>  6 "  2"       "ph.ecog@2"      "outcomes (risk)" TRUE  sex     
#>  7 "  3"       "ph.ecog@3"      "outcomes (risk)" TRUE  sex     
#>  8 "  Unknown" "ph.ecog@_NA_"   "outcomes"        FALSE sex     
#>  9 "ph.karno"  "ph.karno"       "median (iqr)"    TRUE  sex     
#> 10 "  Unknown" "ph.karno@_NA_"  "outcomes"        FALSE sex     
#> 11 "pat.karno" "pat.karno"      "median (iqr)"    TRUE  sex     
#> 12 "  Unknown" "pat.karno@_NA_" "outcomes"        FALSE sex
```

## Obtain Table 1

``` r
design |>
  rifttable() |>
  rt_gt() # obtain formatted output
```

| sex       | Male                 | Female               |
|:----------|:---------------------|:---------------------|
| N         | 138                  | 90                   |
| age       | 64.00 (57.00, 70.00) | 61.00 (55.00, 68.00) |
| ph.ecog   |                      |                      |
| 0         | 36 (26%)             | 27 (30%)             |
| 1         | 71 (52%)             | 42 (47%)             |
| 2         | 29 (21%)             | 21 (23%)             |
| 3         | 1 (1%)               | 0 (0%)               |
| Unknown   | 1                    | 0                    |
| ph.karno  | 80.00 (70.00, 90.00) | 80.00 (80.00, 90.00) |
| Unknown   | 1                    | 0                    |
| pat.karno | 80.00 (70.00, 90.00) | 80.00 (70.00, 90.00) |
| Unknown   | 2                    | 1                    |

Of note, the dataset is passed along silently together with the
`design`. However, the same design can be applied to different data, if
it contains the same variable names, *e.g.*,
`rifttable(design = design, data = otherdata)`.

## Customize the table

Label variables, set custom rounding, change estimands, and add
“Overall” column.

``` r
# Alternative: set_variable_labels() from the {labelled} package
attr(cancer$age, "label") <- "Age, years" # At diagnosis? Unclear (Loprinzi 94)
attr(cancer$ph.ecog, "label") <- "Physician-rated ECOG score"
attr(cancer$ph.karno, "label") <- "Physician-rated Karnofsky score"
attr(cancer$pat.karno, "label") <- "Patient-rated Karnofsky score"
attr(cancer$sex, "label") <- "Sex"

design <- cancer |>
  table1_design(
    age, ph.ecog, ph.karno, pat.karno,
    by = sex,
    continuous_type = "mean (sd)"
  ) |> # default: "median (iqr)"
  mutate( # rounding specifically for the "age" variable
    digits = if_else(
      outcome == "age",
      true = 1,
      false = NA
    )
  )

design |>
  rifttable(
    diff_digits = 0, # rounding for continuous variables other than age
    overall = TRUE
  ) |> # add unstratified "overall" column
  rt_gt() |> # obtain formatted output
  gt::tab_footnote(
    footnote = "Data shown are count (percent) or mean (standard deviation)."
  )
```

| Summary                                                      | Overall    | Male       | Female     |
|:-------------------------------------------------------------|:-----------|:-----------|:-----------|
| N                                                            | 228        | 138        | 90         |
| Age, years                                                   | 62.4 (9.1) | 63.3 (9.1) | 61.1 (8.8) |
| Physician-rated ECOG score                                   |            |            |            |
| 0                                                            | 63 (28%)   | 36 (26%)   | 27 (30%)   |
| 1                                                            | 113 (50%)  | 71 (52%)   | 42 (47%)   |
| 2                                                            | 50 (22%)   | 29 (21%)   | 21 (23%)   |
| 3                                                            | 1 (0%)     | 1 (1%)     | 0 (0%)     |
| Unknown                                                      | 1          | 1          | 0          |
| Physician-rated Karnofsky score                              | 82 (12)    | 82 (12)    | 82 (12)    |
| Unknown                                                      | 1          | 1          | 0          |
| Patient-rated Karnofsky score                                | 80 (15)    | 79 (14)    | 81 (15)    |
| Unknown                                                      | 3          | 2          | 1          |
| Data shown are count (percent) or mean (standard deviation). |            |            |            |

## Add more statistics per variable

For the age variable, do not just display mean and standard deviation,
but also the range. To this end, edit the `design` just like a regular
dataset.

``` r
design_new <- design |>
  mutate( # create three rows, not one, for "age"
    copy = if_else(
      outcome == "age",
      true = 3,
      false = 1
    )
  ) |>
  tidyr::uncount(copy, .id = "copy") |>
  mutate( # set new labels and types for the three rows on age
    label = case_when(
      outcome == "age" & copy == 2 ~ "  Mean (SD)",
      outcome == "age" & copy == 3 ~ "  Range",
      TRUE ~ label
    ),
    type = case_when(
      outcome == "age" & copy == 1 ~ "",
      outcome == "age" & copy == 2 ~ "mean (sd)",
      outcome == "age" & copy == 3 ~ "range",
      TRUE ~ type
    )
  )
design_new
#> # A tibble: 14 × 7
#>    label                             outcome   type  na_rm exposure digits  copy
#>    <chr>                             <chr>     <chr> <lgl> <chr>     <dbl> <int>
#>  1 "N"                               ""        "tot… NA    sex          NA     1
#>  2 "Age, years"                      "age"     ""    FALSE sex           1     1
#>  3 "  Mean (SD)"                     "age"     "mea… FALSE sex           1     2
#>  4 "  Range"                         "age"     "ran… FALSE sex           1     3
#>  5 "Physician-rated ECOG score"      ""        ""    FALSE sex          NA     1
#>  6 "  0"                             "ph.ecog… "out… TRUE  sex          NA     1
#>  7 "  1"                             "ph.ecog… "out… TRUE  sex          NA     1
#>  8 "  2"                             "ph.ecog… "out… TRUE  sex          NA     1
#>  9 "  3"                             "ph.ecog… "out… TRUE  sex          NA     1
#> 10 "  Unknown"                       "ph.ecog… "out… FALSE sex          NA     1
#> 11 "Physician-rated Karnofsky score" "ph.karn… "mea… TRUE  sex          NA     1
#> 12 "  Unknown"                       "ph.karn… "out… FALSE sex          NA     1
#> 13 "Patient-rated Karnofsky score"   "pat.kar… "mea… TRUE  sex          NA     1
#> 14 "  Unknown"                       "pat.kar… "out… FALSE sex          NA     1
design_new |>
  rifttable(
    diff_digits = 0,
    overall = TRUE
  ) |>
  rt_gt() |>
  gt::tab_footnote(
    footnote = "Data shown are count (percent), unless indicated otherwise."
  )
```

| Summary                                                     | Overall    | Male       | Female     |
|:------------------------------------------------------------|:-----------|:-----------|:-----------|
| N                                                           | 228        | 138        | 90         |
| Age, years                                                  |            |            |            |
| Mean (SD)                                                   | 62.4 (9.1) | 63.3 (9.1) | 61.1 (8.8) |
| Range                                                       | 39.0, 82.0 | 39.0, 82.0 | 41.0, 77.0 |
| Physician-rated ECOG score                                  |            |            |            |
| 0                                                           | 63 (28%)   | 36 (26%)   | 27 (30%)   |
| 1                                                           | 113 (50%)  | 71 (52%)   | 42 (47%)   |
| 2                                                           | 50 (22%)   | 29 (21%)   | 21 (23%)   |
| 3                                                           | 1 (0%)     | 1 (1%)     | 0 (0%)     |
| Unknown                                                     | 1          | 1          | 0          |
| Physician-rated Karnofsky score                             | 82 (12)    | 82 (12)    | 82 (12)    |
| Unknown                                                     | 1          | 1          | 0          |
| Patient-rated Karnofsky score                               | 80 (15)    | 79 (14)    | 81 (15)    |
| Unknown                                                     | 3          | 2          | 1          |
| Data shown are count (percent), unless indicated otherwise. |            |            |            |
