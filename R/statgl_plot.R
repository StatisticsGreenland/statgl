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
statgl_plot <- function(df, x, y, type = "line", name = NULL, group = NULL,
                        color = NULL,
                        title = NULL, subtitle = NULL, caption = NULL) {
  # Capture expressions
  x <- enexpr(x)
  y <- enexpr(y)
  group <- enexpr(group)
  color <- enexpr(color)

  # Build hcaes() mapping only including non-NULL aesthetics
  mapping <- expr(highcharter::hcaes(!!x, !!y))
  if (!is.null(group)) {
    mapping <- rlang::call_modify(mapping, group = group)
  }
  if (!is.null(color)) {
    mapping <- rlang::call_modify(mapping, color = color)
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

  chart
}
