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
  palette = NULL,
  height = 300
) {
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

  # --- axis labels -----------------------------------------------
  if (!is.null(xlab) && nzchar(xlab)) {
    chart <- highcharter::hc_xAxis(chart, title = list(text = xlab))
  }
  if (!is.null(ylab) && nzchar(ylab)) {
    chart <- highcharter::hc_yAxis(chart, title = list(text = ylab))
  }

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
        formatter = highcharter::JS(sprintf(
          'function() {
             return (this.point.index === this.series.data.length - 1)
               ? Highcharts.numberFormat(this.y, %d, "%s", "%s") + "%s"
               : null;
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

  # --- default: no markers on line/spline/area ---------------------
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
