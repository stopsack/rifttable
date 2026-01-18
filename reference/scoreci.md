# Wilson Score Confidence Intervals

"This function computes a confidence interval for a proportion. It is
based on inverting the large-sample normal score test for the
proportion." (Alan Agresti, who wrote the original R code)

Inputs for `success`, `total`, and `level` are vectorized.

## Usage

``` r
scoreci(success, total, level = 0.95, return_midpoint = FALSE)
```

## Arguments

- success:

  Success count.

- total:

  Total count.

- level:

  Optional. Confidence level. Defaults to 0.95.

- return_midpoint:

  Optional. Return midpoint of confidence interval? Defaults to `FALSE`.

## Value

Data frame:

- `success` Success count

- `total` Total count

- `estimate` Proportion

- `conf.low` Lower bound of the confidence interval.

- `conf.high` Upper bound of the confidence interval.

- `midpoint` Mid-point of the confidence interval (for
  `return_midpoint = TRUE`).

- `level` Confidence level.

## See also

<https://users.stat.ufl.edu/~aa/cda/R/one-sample/R1/index.html>

Agresti A, Coull BA. Approximate is better than "exact" for interval
estimation of binomial proportions. Am Stat 1998;52:119-126.
[doi:10.2307/2685469](https://doi.org/10.2307/2685469)

Brown LD, Cai TT, DasGupta A. Interval estimation for a binomial
proportion (with discussion). Stat Sci 2001;16:101-133.
[doi:10.1214/ss/1009213286](https://doi.org/10.1214/ss/1009213286)

## Examples

``` r
scoreci(success = 5, total = 10)
#>   success total estimate  conf.low conf.high level
#> 1       5    10      0.5 0.2365931 0.7634069  0.95
scoreci(success = c(5:10), total = 10, level = 0.9)
#>   success total estimate  conf.low conf.high level
#> 1       5    10      0.5 0.2692718 0.7307282   0.9
#> 2       6    10      0.6 0.3516386 0.8057730   0.9
#> 3       7    10      0.7 0.4416998 0.8731234   0.9
#> 4       8    10      0.8 0.5407928 0.9314420   0.9
#> 5       9    10      0.9 0.6522813 0.9773651   0.9
#> 6      10    10      1.0 0.7870580 1.0000000   0.9
```
