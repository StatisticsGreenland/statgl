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
statgl_plot <- function(df, x, y = value, type = "line", name = NULL, group = NULL,
                        title = NULL, subtitle = NULL, caption = NULL,
                        show_last_value = TRUE,
                        xlab = "",
                        ylab = "",
                        tooltip = NULL,
                        locale = "en") {
  # Capture expressions
  x <- enexpr(x)
  y <- enexpr(y)
  group <- enexpr(group)

  # Build hcaes() mapping only including non-NULL aesthetics
  mapping <- expr(highcharter::hcaes(!!x, !!y))
  if (!is.null(group)) {
    mapping <- rlang::call_modify(mapping, group = group)
  }

  # Build hchart() args
  args <- rlang::dots_list(
    object = df,
    type = type,
    mapping = eval(mapping),
    .named = TRUE
  )
  if (!is.null(name)) {
    args$name <- name
  }

  # Create chart
  chart <- rlang::exec(highcharter::hchart, !!!args)

  # Add title if provided
  if (!is.null(title)) {
    chart <- highcharter::hc_title(chart, text = title, align = "left")
  }

  # Add subtitle if provided
  if (!is.null(subtitle)) {
    chart <- chart %>% highcharter::hc_subtitle(text = subtitle, align = "left")
  }

  # Add caption if provided
  if (!is.null(caption)) {
    chart <- chart %>% highcharter::hc_caption(text = caption, align = "right")
  }

  # Add axis labels if provided
  if (!is.null(xlab)) {
    chart <- highcharter::hc_xAxis(chart, title = list(text = xlab))
  }
  if (!is.null(ylab)) {
    chart <- highcharter::hc_yAxis(chart, title = list(text = ylab))
  }

  # Optional tooltip formatter (JS string)
  if (!is.null(tooltip)) {
    chart <- highcharter::hc_tooltip(chart, formatter = highcharter::JS(tooltip))
  }

  if (show_last_value) {
    decimal_mark <- if (locale %in% c("da", "kl")) "," else "."
    big_mark     <- if (locale %in% c("da", "kl")) "." else ","
    if (type %in% c("line", "spline")) {
      chart <- highcharter::hc_plotOptions(chart, series = list(
        dataLabels = list(
          enabled = TRUE,
          formatter = highcharter::JS(sprintf(
            "function() {
           return (this.point.index === this.series.data.length - 1)
             ? Highcharts.numberFormat(this.y, 0, '%s', '%s')
             : null;
         }",
            decimal_mark, big_mark
          ))
        )
      ))
    } else if (type %in% c("bar", "column")) {
    decimal_mark <- if (locale %in% c("da", "kl")) "," else "."
    big_mark     <- if (locale %in% c("da", "kl")) "." else ","
    chart <-
      highcharter::hc_plotOptions(chart, series = list(
        dataLabels = list(
          enabled = TRUE,
          formatter = highcharter::JS(sprintf("
  function() {
    return Highcharts.numberFormat(this.y, 0, '%s', '%s');
  }", decimal_mark, big_mark))
        )
      ))
  }
  }

  chart

}
