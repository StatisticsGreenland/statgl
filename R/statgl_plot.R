#' Create a highcharter plot with Statgl defaults
#'
#' `statgl_plot()` is a wrapper around [highcharter::hchart()] that provides
#' Statgl-friendly defaults for chart types, formatting, labels, colors, and
#' layout. It handles number formatting, suffixes, value labels, axis-title
#' suppression, grouped tooltips, stacking, colour palettes, and placing
#' last-value labels slightly to the right of the final point in line/area
#' charts.
#'
#' @export
#'
#' @param df A data frame.
#' @param x,y Bare column names for x and y aesthetics. `y` defaults to `value`.
#' @param type Optional string specifying the chart type. If `NULL`, the type is
#'   inferred from the structure of `x`:
#'   * `"line"` if `x` is `Date`/`POSIXct` or an integer-like numeric with many
#'     distinct values
#'   * `"column"` if `x` is a factor/character or numeric with few distinct
#'     values
#'   * `"scatter"` otherwise.
#' @param name Optional series name passed to [highcharter::hchart()].
#' @param group Optional bare column name used to split data into series
#'   (`highcharter::hcaes(group = ...)`).
#' @param title,subtitle,caption Optional text annotations added via
#'   [highcharter::hc_title()], [highcharter::hc_subtitle()] and
#'   [highcharter::hc_caption()]. Titles and subtitles are left-aligned;
#'   captions are right-aligned.
#' @param show_last_value Logical; if `TRUE` (default), adds data labels for the
#'   final point of each `"line"`, `"spline"` and `"area"` series, or for all
#'   bars in `"bar"` / `"column"` charts. For line/area/spline charts, the label
#'   is placed slightly to the right of the final point.
#' @param xlab,ylab Axis labels. If `NULL` or `""`, no axis title is shown and
#'   any automatic titles inferred by [highcharter::hchart()] are disabled.
#' @param tooltip Optional JavaScript string passed to
#'   [highcharter::hc_tooltip()] as a custom `formatter`. If `NULL`, a default
#'   tooltip is used that respects `group`, `digits`, `big.mark`,
#'   `decimal.mark` and `suffix`.
#' @param suffix Character suffix appended to formatted values in data labels
#'   and the default tooltip, e.g. `" %"` or `" personer"`. Defaults to `""`.
#' @param digits Integer number of decimal places used in the default tooltip
#'   and data labels. Defaults to `0`.
#' @param big.mark Character used as thousands separator for number formatting.
#'   Defaults to `"."`.
#' @param decimal.mark Character used as decimal separator for number
#'   formatting. Defaults to `","`.
#' @param locale Optional locale code (`"da"`, `"kl"`, `"en"`, etc.). If
#'   provided and `big.mark` and `decimal.mark` are not explicitly overridden,
#'   they are derived from `locale` (`"da"`/`"kl"` → decimal `","`, big mark
#'   `"."`; other values → decimal `"."`, big mark `","`).
#' @param stacking Optional stacking mode for `"area"`, `"column"` and `"bar"`
#'   charts. One of `"normal"` or `"percent"`, or `NULL` (no stacking).
#' @param palette Optional palette specification for the series colours. Either:
#'   * a single character name of an element in `statgl_palettes` (e.g.
#'     `"main"`, `"winter"`, `"autumn"`), or
#'   * a character vector of colour hex codes to pass directly to
#'     [highcharter::hc_colors()].
#'
#'   If the named palette is not found, a warning is issued and the default
#'   Highcharts colours are used.
#' @param height Numeric chart height in pixels passed to
#'   [highcharter::hc_chart()]. Defaults to `300`.
#'
#' @return A [highcharter::highchart] object.
#'
#' @examples
#' \dontrun{
#' # Grouped time series with suffix and Statgl palette
#' statgl_fetch("BEXSTNUK", citydistrict = "*") %>%
#'   statgl_plot(
#'     time,
#'     group   = citydistrict,
#'     suffix  = " personer",
#'     locale  = "da",
#'     palette = "spring"
#'   )
#' }
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
  xlab = NULL,
  ylab = NULL,
  tooltip = NULL,
  suffix = "",
  digits = 0,
  big.mark = ".",
  decimal.mark = ",",
  locale = NULL,
  stacking = NULL,
  palette = "main",
  height = 300
) {
  # --- number formatting setup -----------------------------------
  big_mark_missing <- missing(big.mark)
  decimal_mark_missing <- missing(decimal.mark)

  if (!is.null(locale) && big_mark_missing && decimal_mark_missing) {
    if (locale %in% c("da", "kl")) {
      decimal.mark <- ","
      big.mark <- "."
    } else {
      decimal.mark <- "."
      big.mark <- ","
    }
  }

  js_escape <- function(x) {
    x <- gsub("\\\\", "\\\\\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\\\"", x, fixed = TRUE)
    x
  }

  decimal_mark <- decimal.mark
  big_mark <- big.mark
  suffix_js <- js_escape(suffix)

  # --- mapping ---------------------------------------------------
  x_expr <- rlang::enexpr(x)
  y_expr <- rlang::enexpr(y)
  group_expr <- rlang::enexpr(group)

  has_group <- !rlang::is_missing(group_expr) &&
    !identical(group_expr, rlang::expr(NULL))

  mapping_expr <- if (has_group) {
    rlang::expr(
      highcharter::hcaes(!!x_expr, !!y_expr, group = !!group_expr)
    )
  } else {
    rlang::expr(
      highcharter::hcaes(!!x_expr, !!y_expr)
    )
  }

  mapping <- rlang::eval_tidy(mapping_expr)

  # --- type inference --------------------------------------------
  if (is.null(type)) {
    x_vals <- df[[rlang::as_name(x_expr)]]

    if (
      inherits(x_vals, c("Date", "POSIXct")) ||
        (is.numeric(x_vals) &&
          all(x_vals %% 1 == 0, na.rm = TRUE) &&
          length(unique(x_vals)) > 10)
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

  # --- build hchart() args ---------------------------------------
  args <- rlang::dots_list(
    object = df,
    type = type,
    mapping = mapping,
    .named = TRUE
  )
  if (!is.null(name)) {
    args$name <- name
  }

  chart <- rlang::exec(highcharter::hchart, !!!args)

  # --- titles / captions -----------------------------------------
  if (!is.null(title)) {
    chart <- highcharter::hc_title(chart, text = title, align = "left")
  }
  if (!is.null(subtitle)) {
    chart <- highcharter::hc_subtitle(chart, text = subtitle, align = "left")
  }
  if (!is.null(caption)) {
    chart <- highcharter::hc_caption(chart, text = caption, align = "right")
  }

  # --- axes ------------------------------------------------------
  # X axis
  x_axis_opts <- list()

  # only add right padding if we will draw end labels on line/spline/area
  if (isTRUE(show_last_value) && type %in% c("line", "spline", "area")) {
    x_axis_opts$maxPadding <- 0.12
  }

  if (!is.null(xlab) && nzchar(xlab)) {
    x_axis_opts$title <- list(text = xlab)
  } else {
    x_axis_opts$title <- list(text = NULL)
  }

  chart <- do.call(highcharter::hc_xAxis, c(list(chart), x_axis_opts))

  # Y axis
  y_axis_opts <- list()

  if (is.null(ylab) || !nzchar(ylab)) {
    y_axis_opts$title <- list(
      text = NULL,
      enabled = FALSE
    )
  } else {
    y_axis_opts$title <- list(
      text = ylab,
      enabled = TRUE
    )
  }

  chart <- do.call(highcharter::hc_yAxis, c(list(chart), y_axis_opts))

  # --- tooltip ---------------------------------------------------
  if (!is.null(tooltip)) {
    # user-supplied tooltip JS wins
    chart <- highcharter::hc_tooltip(
      chart,
      formatter = highcharter::JS(tooltip)
    )
  } else {
    # default tooltip that respects group / digits / marks / suffix
    if (has_group) {
      # grouped: show category, then "group: value + suffix"
      chart <- highcharter::hc_tooltip(
        chart,
        shared = FALSE,
        useHTML = FALSE,
        formatter = highcharter::JS(sprintf(
          'function() {
             var cat = (this.point.category !== undefined)
               ? this.point.category
               : this.x;
             var name = (this.series && this.series.name)
               ? this.series.name + ": "
               : "";
             var value = Highcharts.numberFormat(this.y, %d, "%s", "%s") + "%s";
             if (cat !== undefined) {
               return cat + "<br/>" + name + value;
             }
             return name + value;
           }',
          digits,
          decimal_mark,
          big_mark,
          suffix_js
        ))
      )
    } else {
      # ungrouped: "category: value + suffix"
      chart <- highcharter::hc_tooltip(
        chart,
        shared = FALSE,
        useHTML = FALSE,
        formatter = highcharter::JS(sprintf(
          'function() {
             var cat = (this.point.category !== undefined)
               ? this.point.category
               : this.x;
             var value = Highcharts.numberFormat(this.y, %d, "%s", "%s") + "%s";
             if (cat !== undefined) {
               return cat + ": " + value;
             }
             return value;
           }',
          digits,
          decimal_mark,
          big_mark,
          suffix_js
        ))
      )
    }
  }

  # --- dataLabels + stacking -------------------------------------
  series_opts <- list()

  if (isTRUE(show_last_value)) {
    if (type %in% c("line", "spline", "area")) {
      series_opts$dataLabels <- list(
        enabled = TRUE,
        align = "left", # text grows to the right
        x = 6, # horizontal offset
        verticalAlign = "middle",
        crop = FALSE, # prevent clipping
        overflow = "allow",
        formatter = highcharter::JS(sprintf(
          'function() {
             if (this.point.index !== this.series.data.length - 1) return null;
             return Highcharts.numberFormat(this.y, %d, "%s", "%s") + "%s";
           }',
          digits,
          decimal_mark,
          big_mark,
          suffix_js
        ))
      )
    } else if (type %in% c("bar", "column")) {
      series_opts$dataLabels <- list(
        enabled = TRUE,
        formatter = highcharter::JS(sprintf(
          'function() {
             return Highcharts.numberFormat(this.y, %d, "%s", "%s") + "%s";
           }',
          digits,
          decimal_mark,
          big_mark,
          suffix_js
        ))
      )
    }
  }

  if (!is.null(stacking) && type %in% c("area", "column", "bar")) {
    stacking <- match.arg(stacking, c("normal", "percent"))
    series_opts$stacking <- stacking
  }

  # default: no markers on line/spline/area
  if (type %in% c("line", "spline", "area")) {
    series_opts$marker <- list(enabled = FALSE)
  }

  if (length(series_opts) > 0) {
    chart <- highcharter::hc_plotOptions(chart, series = series_opts)
  }

  # --- palette ---------------------------------------------------
  if (!is.null(palette)) {
    pal_vals <- NULL

    if (is.character(palette) && length(palette) == 1L) {
      if (exists("statgl_palettes", inherits = TRUE)) {
        pal_list <- get("statgl_palettes", inherits = TRUE)
        if (!is.null(pal_list[[palette]])) {
          pal_vals <- unname(pal_list[[palette]])
        } else {
          warning(
            "Palette '",
            palette,
            "' not found in statgl_palettes; ignoring `palette`."
          )
        }
      } else {
        warning("statgl_palettes not found; ignoring `palette` name.")
      }
    } else {
      pal_vals <- palette
    }

    if (!is.null(pal_vals)) {
      chart <- highcharter::hc_colors(chart, pal_vals)
    }
  }

  # --- height ----------------------------------------------------
  chart <- highcharter::hc_chart(chart, height = height)

  chart
}
