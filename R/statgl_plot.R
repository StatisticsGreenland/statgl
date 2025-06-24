#' Create a highcharter plot from a data frame
#'
#' @param data A data frame
#' @param x, y, group Columns mapped to x, y, and group aesthetics
#' @param type A string specifying the plot type (e.g., "line", "column", "scatter")
#'
#' @return A highcharter plot object
#' @export
#'
#' @examples
#' statgl_plot(mtcars, x = wt, y = mpg, group = cyl, type = "scatter")
statgl_plot <- function(data, x, y, group = NULL, type = "line") {
  stopifnot(requireNamespace("highcharter", quietly = TRUE))

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  aes <- if (rlang::quo_is_null(group)) {
    highcharter::hcaes(x = !!x, y = !!y)
  } else {
    highcharter::hcaes(x = !!x, y = !!y, group = !!group)
  }

  highcharter::hchart(data = data, type = type, mapping = aes)
}
