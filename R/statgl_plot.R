#' Create a highcharter plot with smart defaults
#'
#' A wrapper around `highcharter::hchart()` that makes it easy to create line, bar, and column plots with automatic titles, axis labels, tooltips, and localized number formatting.
#' If `show_last_value = TRUE`, it shows value labels on:
#' - only the last point in each line (`type = "line"` or `"spline"`)
#' - all bars for `type = "bar"` or `"column"`
#'
#' @param df A data frame.
#' @param x, y Bare column names for x and y aesthetics. `y` defaults to `value`.
#' @param type A string specifying the chart type (e.g., `"line"`, `"column"`, `"bar"`, `"scatter"`).
#' @param name An optional name for the series.
#' @param group A bare column name to split data into series (for grouped plots).
#' @param title, subtitle, caption Optional text elements to add to the chart.
#' @param show_last_value Logical; if `TRUE`, adds data labels to the last value of each line, or all bars in bar/column charts.
#' @param xlab, ylab Axis labels.
#' @param tooltip Optional JavaScript string for a custom tooltip formatter.
#' @param locale Locale code used to control number formatting (default is `"en"`; use `"da"` or `"kl"` for Danish/Greenlandic-style formatting).
#'
#' @return A `highchart` object.
#' @export
#'
#' @examples
#' sstatgl_plot(statgl_fetch("BEXSTA"), time)
#' statgl_plot(statgl_fetch("BEXSTA", time = px_top(), gender = c("M", "K")), x = gender, type = "bar")
statgl_plot <- function(
  df,
  x,
  y = value,
  type = NULL,
  name = NULL,
  group = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  show_last_value = TRUE,
  xlab = "",
  ylab = "",
  tooltip = NULL,
  locale = "en",
  height = 300
) {
  # Capture expressions
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  group <- rlang::enexpr(group)

  # Build hcaes() mapping only including non-NULL aesthetics
  mapping <- rlang::expr(highcharter::hcaes(!!x, !!y))
  if (!is.null(group)) {
    mapping <- rlang::call_modify(mapping, group = group)
  }

  # Infer type if not provided
  if (missing(type) || is.null(type)) {
    x_vals <- df[[rlang::as_name(x)]]

    if (
      inherits(x_vals, c("Date", "POSIXct")) ||
        is.numeric(x_vals) &&
          all(x_vals %% 1 == 0) &&
          length(unique(x_vals)) > 10
    ) {
      type <- "line"
    } else if (
      is.character(x_vals) ||
        is.factor(x_vals) ||
        (is.numeric(x_vals) && length(unique(x_vals)) <= 12)
    ) {
      type <- "column"
    } else {
      type <- "scatter"
    }
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
    chart <- highcharter::hc_subtitle(chart, text = subtitle, align = "left")
  }

  # Add caption if provided
  if (!is.null(caption)) {
    chart <- highcharter::hc_caption(chart, text = caption, align = "right")
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
    chart <- highcharter::hc_tooltip(
      chart,
      formatter = highcharter::JS(tooltip)
    )
  }

  if (show_last_value) {
    decimal_mark <- if (locale %in% c("da", "kl")) "," else "."
    big_mark <- if (locale %in% c("da", "kl")) "." else ","
    if (type %in% c("line", "spline")) {
      chart <- highcharter::hc_plotOptions(
        chart,
        series = list(
          dataLabels = list(
            enabled = TRUE,
            formatter = highcharter::JS(sprintf(
              'function() {
           return (this.point.index === this.series.data.length - 1)
             ? Highcharts.numberFormat(this.y, 0, "%s", "%s")
             : null;
         }',
              decimal_mark,
              big_mark
            ))
          )
        )
      )
    } else if (type %in% c("bar", "column")) {
      decimal_mark <- if (locale %in% c("da", "kl")) "," else "."
      big_mark <- if (locale %in% c("da", "kl")) "." else ","
      chart <-
        highcharter::hc_plotOptions(
          chart,
          series = list(
            dataLabels = list(
              enabled = TRUE,
              formatter = highcharter::JS(sprintf(
                '
  function() {
    return Highcharts.numberFormat(this.y, 0, "%s", "%s");
  }',
                decimal_mark,
                big_mark
              ))
            )
          )
        )
    }
  }

  chart <- highcharter::hc_chart(chart, height = height)

  chart
}
