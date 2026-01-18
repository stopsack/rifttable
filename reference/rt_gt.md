# Turn tibble into gt Table with Custom Formatting

Formatting includes:

- Text align to top/left

- Smaller row padding

- No top border

- Bold column labels

If this function is called within a document that is being knit to plain
markdown, such as `format: gfm` in a Quarto document or
`format: github_document` in an RMarkdown document, then a plain
markdown-formatted table (e.g., without footnotes) is returned via
[`kable`](https://rdrr.io/pkg/knitr/man/kable.html).

## Usage

``` r
rt_gt(df, md = 1, indent = 10, remove_border = TRUE)
```

## Arguments

- df:

  Data frame/tibble

- md:

  Optional. If not `NULL`, then the given columns will be printed with
  markdown formatting, e.g., `md = c(1, 3)` for columns 1 and 3.
  Defaults to `1`, i.e., the first column.

- indent:

  Optional. Detects cells in the first column of table, e.g., from
  [`rifttable`](https://stopsack.github.io/rifttable/reference/rifttable.md)
  where the first column contains the labels, that start with at least
  two spaces. This text is then indented via
  [`tab_style`](https://gt.rstudio.com/reference/tab_style.html).
  Defaults `10` for 10 pixels. Set to `NULL` to turn off.

- remove_border:

  Optional. For rows that are indented in the first column or have an
  empty first column, remove the upper horizontal border line? Defaults
  to `TRUE`.

## Value

Formatted gt table

## Examples

``` r
data(mtcars)
mtcars |>
  dplyr::slice(1:5) |>
  rt_gt()


  

mpg
```

cyl

disp

hp

drat

wt

qsec

vs

am

gear

carb

21

6

160

110

3.90

2.620

16.46

0

1

4

4

21

6

160

110

3.90

2.875

17.02

0

1

4

4

22.8

4

108

93

3.85

2.320

18.61

1

1

4

1

21.4

6

258

110

3.08

3.215

19.44

1

0

3

1

18.7

8

360

175

3.15

3.440

17.02

0

0

3

2
