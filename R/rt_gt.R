#' Custom formatting for gt Tables
#'
#' @param mytab gt object
#' @noRd
rt_tabstyle <- function(mytab) {
  mytab %>%
    gt::tab_options(
      data_row.padding = gt::px(0),
      column_labels.border.top.style = "none",
      table.border.top.style = "none",
      table_body.border.top.style = "none",
      column_labels.font.weight = "bold") %>%
    gt::tab_style(
      style = gt::cell_text(
        align = "left",
        v_align = "top"),
      locations = gt::cells_body()) %>%
    gt::tab_style(
      style = gt::cell_text(
        align = "left",
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
#' If this function is called within a document that is being knit to plain
#' markdown, such as \code{format: gfm} in a Quarto document or
#' \code{format: github_document} in an RMarkdown document, then a plain
#' markdown-formatted table (e.g., without footnotes) is returned via
#' \code{\link[knitr]{kable}}.
#'
#' @param df Data frame/tibble
#' @param md Optional. If not \code{NULL}, then the given
#'   columns will be printed with markdown formatting, e.g., \code{md = c(1, 3)}
#'   for columns 1 and 3. Defaults to \code{1}, i.e., the first column.
#' @param indent Optional. Detects cells in the first column of table, e.g.,
#'   from \code{\link[rifttable]{rifttable}} where the first column contains the
#'   labels, that start with at least two spaces. This text is then indented via
#'   \code{\link[gt]{tab_style}}. Defaults \code{10} for 10 pixels. Set to
#'   \code{NULL} to turn off.
#' @param remove_border Optional. For rows that are indented in the first
#'   column or have an empty first column, remove the upper horizontal border
#'   line? Defaults to \code{TRUE}.
#'
#' @return Formatted gt table
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars %>%
#'   dplyr::slice(1:5) %>%
#'   rt_gt()
#' }
#'
#' @section Example Output:
#' \if{html}{\figure{rt_gt.png}{options: width=50\%}}
rt_gt <- function(
    df,
    md = 1,
    indent = 10,
    remove_border = TRUE) {
  # RMarkdown "output: github_document" cannot handle HTML styles
  # Likewise Quarto counterpart "output: gfm"
  if(
    any(
      stringr::str_detect(
        string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
        pattern = "gfm|commonmark")
    )
  ) {
    res <- knitr::kable(df)
    attr(x = res, which = "mydata") <- df
    return(res)
  } else {
    if (!is_package_installed("gt")) {
      stop(
        paste(
          "The package \"gt\" must be installed to create formatted tables",
          "via rifttable::rt_gt(). Use alternative packages for table",
          "formatting or install \"gt\":\n   install.packages(\"gt\")"),
        call. = FALSE)
    }
    df_gt <- df %>%
      gt::gt(id = "rifttable") %>%
      rt_tabstyle()
    if(!is.null(indent[1])) {
      indent2 <- union(
        stringr::str_which(
          string = df[[1]],
          pattern = "^[:blank:]{2,}"),
        which(df[[1]] == ""))
      df_gt <- df_gt %>%
        gt::tab_style(
          style = gt::cell_text(indent = gt::px(indent[1])),
          locations = gt::cells_body(columns = 1,
                                     rows = indent2))
      if(remove_border == TRUE & length(indent2) > 0) {
        df_gt <- df_gt %>%
          gt::tab_style(
            style = gt::cell_borders(sides = "top", weight = NULL),
            locations = gt::cells_body(columns = gt::everything(),
                                       rows = indent2))
      }
    }
    if(!is.null(md)) {
      df_gt <- df_gt %>%
        gt::fmt_markdown(columns = md)
    }
    return(df_gt)
  }
}
