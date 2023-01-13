#' Custom formatting for gt Tables
#'
#' @param mytab gt object
#' @noRd
rt_tabstyle <- function(mytab) {
  mytab %>%
    gt::tab_options(data_row.padding = gt::px(0),
                    column_labels.border.top.style = "none",
                    table.border.top.style = "none",
                    table_body.border.top.style = "none",
                    column_labels.font.weight = "bold") %>%
    gt::tab_style(style = gt::cell_text(align = "left",
                                        v_align = "top"),
                  locations = gt::cells_body()) %>%
    gt::tab_style(style = gt::cell_text(align = "left",
                                    v_align = "bottom"),
                  locations = gt::cells_column_labels())
}

#' Turn tibble into gt Table with Custom Formatting
#'
#' @description Formatting includes:
#'   * Text align to top/left
#'   * Smaller row padding
#'   * No top border
#'   * Bold column labels
#'
#' @param df Data frame/tibble
#' @param md Optional. If not \code{NULL}, then the given
#'   columns will be printed with markdown formatting, e.g., \code{md = c(1, 3)}
#'   for columns 1 and 3.
#' @param indent Optional. Detects labels (first
#'  columns in a \code{\link[rifttable]{rifttable}}) that start with two or four
#'   spaces and ensures indenting via \code{\link[gt]{tab_style}}. Defaults
#'   to 10 and 20 pixels for two or four spaces (\code{c(10, 20)}). Set to
#'   \code{NULL} to turn off. (Note that gt currently does not seem to support
#'   four-space indents in columns with markdown formatting, e.g.,
#'   \code{md = 1}.)
#' @param remove_border Optional. For indented lines, remove the upper
#'   horizontal border line? Defaults to \code{TRUE}.
#'
#' @return Formatted gt table
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars |>
#'   dplyr::slice(1:5) |>
#'   rt_gt()
#'
#' @section Example Output:
#' \if{html}{\figure{rt_gt.png}{options: width=50\%}}
rt_gt <- function(df, md = NULL, indent = c(10, 20), remove_border = TRUE) {
  # RMarkdown "output: github_document" cannot handle HTML styles
  if(any(stringr::str_detect(
    string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
    pattern = "gfm"))) {
    res <- knitr::kable(df)
    attr(x = res, which = "mydata") <- df
    return(res)
  } else {
    df_gt <- df %>%
      gt::gt() %>%
      rt_tabstyle()
    if(!is.null(indent[1])) {
      if(is.null(attr(df, "rt_gt.indent4")) |
         length(attr(df, "rt_gt.indent4")) == 0)
        indent4 <- FALSE
      else
        indent4 <- attr(df, "rt_gt.indent4")
      if(is.null(attr(df, "rt_gt.indent2")) |
         length(attr(df, "rt_gt.indent2")) == 0)
        indent2 <-FALSE
      else {
        indent2 <- attr(df, "rt_gt.indent2")
        indent2 <- indent2[!(indent2 %in% indent4)]
      }
      all_indents <- union(indent2, indent4)[union(indent2, indent4) != 0]
      df_gt <- df_gt %>%
        gt::tab_style(
          style = gt::cell_text(indent = gt::px(indent[1])),
          locations = gt::cells_body(columns = 1,
                                     rows = indent2)) %>%
        gt::tab_style(
          style = gt::cell_text(indent = gt::px(indent[2])),
          locations = gt::cells_body(columns = 1,
                                     rows = indent4))
      if(remove_border == TRUE & length(all_indents) > 0) {
        df_gt <- df_gt %>%
          gt::tab_style(
            style = gt::cell_borders(sides = "top", weight = NULL),
            locations = gt::cells_body(columns = gt::everything(),
                                       rows = all_indents))
      }
    }
    if(!is.null(md)) {
      df_gt <- df_gt %>%
        gt::fmt_markdown(columns = md)
    }
    df_gt
  }
}
