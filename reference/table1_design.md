# Design A Descriptive Table

This function generates a `design` table from which
[`rifttable`](https://stopsack.github.io/rifttable/reference/rifttable.md)
can generate a descriptive table.

## Usage

``` r
table1_design(
  data,
  ...,
  by = NULL,
  total = TRUE,
  empty_levels = FALSE,
  na_always = FALSE,
  na_label = "Unknown",
  continuous_type = "median (iqr)",
  binary_type = "outcomes (risk)"
)
```

## Arguments

- data:

  Data set

- ...:

  Optional: Variables to include or exclude (using `-variable`)

- by:

  Optional: Stratification variable. Typically the exposure.

- total:

  Optional: Whether to add the total count at the beginning. Defaults to
  `TRUE`.

- empty_levels:

  Optional: Whether to include empty levels of factor variables.
  Defaults to `FALSE`.

- na_always:

  Optional: Whether to add the count of missing values for each
  variable, even if there are none. Defaults to `FALSE`, i.e., the count
  of missing values will only be shown if there are any.

- na_label:

  Label for count of missing values. Defaults to `"Unknown"`.

- continuous_type:

  Estimator (`type` in
  [`rifttable`](https://stopsack.github.io/rifttable/reference/rifttable.md)
  `design`) for continuous variables. Defaults to `"median (iqr)"`.

- binary_type:

  Estimator (`type` in
  [`rifttable`](https://stopsack.github.io/rifttable/reference/rifttable.md)
  `design`) for binary variables and strata of categorical variables.
  Defaults to `"outcomes (risk)"` (count and column proportion).

## Value

`design` tibble that can be passed on to
[`rifttable`](https://stopsack.github.io/rifttable/reference/rifttable.md).
Contains an attribute `rt_data` so that the dataset does not have to be
provided to
[`rifttable`](https://stopsack.github.io/rifttable/reference/rifttable.md)
another time.

## Examples

``` r
# Data preparation
cars <- tibble::as_tibble(mtcars) |>
  dplyr::mutate(
    gear = factor(
      gear,
      levels = 3:5,
      labels = c("Three", "Four", "Five")
    ),
    # Categorical version of "hp", shows each category
    hp_categorical = dplyr::if_else(
      hp >= 200,
      true = "200+ hp",
      false = "<200 hp"
    ),
    # Binary version of "hp", shows the TRUEs
    hp_binary = hp >= 200
  )
# Label some variables. Better alternative: labelled::set_variable_labels()
attr(cars$hp, "label") <- "Horsepower"
attr(cars$hp_categorical, "label") <- "Horsepower"
attr(cars$hp_binary, "label") <- "200+ hp"
attr(cars$am, "label") <- "Automatic transmission"
attr(cars$gear, "label") <- "Gears"

# Generate table "design"
design <- cars |>
  table1_design(
    hp, hp_categorical, hp_binary, mpg, am,
    by = gear
  )

# Use "design" to create a descriptive table.
design |>
  rifttable(diff_digits = 0)
#> # A tibble: 8 × 4
#>   Gears                    Three            Four           Five            
#>   <chr>                    <chr>            <chr>          <chr>           
#> 1 "N"                      "15"             "12"           "5"             
#> 2 "Horsepower"             "180 (150, 210)" "94 (66, 110)" "175 (113, 264)"
#> 3 "Horsepower"             ""               ""             ""              
#> 4 "  200+ hp"              "5 (33%)"        "0 (0%)"       "2 (40%)"       
#> 5 "  <200 hp"              "10 (67%)"       "12 (100%)"    "3 (60%)"       
#> 6 "200+ hp"                "5 (33%)"        "0 (0%)"       "2 (40%)"       
#> 7 "mpg"                    "16 (14, 18)"    "23 (21, 28)"  "20 (16, 26)"   
#> 8 "Automatic transmission" "0 (0%)"         "8 (67%)"      "5 (100%)"      

# Obtain a formatted table
design |>
  rifttable(diff_digits = 0) |>
  rt_gt()


  

Gears
```

Three

Four

Five

N

15

12

5

Horsepower

180 (150, 210)

94 (66, 110)

175 (113, 264)

Horsepower

200+ hp

5 (33%)

0 (0%)

2 (40%)

\<200 hp

10 (67%)

12 (100%)

3 (60%)

200+ hp

5 (33%)

0 (0%)

2 (40%)

mpg

16 (14, 18)

23 (21, 28)

20 (16, 26)

Automatic transmission

0 (0%)

8 (67%)

5 (100%)
