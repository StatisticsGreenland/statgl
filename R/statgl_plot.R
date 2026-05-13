#' Create a highcharter plot with Statgl defaults
#'
#' `statgl_plot()` is a wrapper around [highcharter::hchart()] that provides
#' Statgl-friendly defaults for chart types, formatting, labels, colors, and
#' layout. It handles number formatting, suffixes, value labels, axis-title
#' suppression, grouped tooltips, stacking, colour palettes, and optionally
#' highlighting the last values in line/area charts.
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
#' @param show_last_value Logical; defaults `FALSE`. If `TRUE`, adds data labels
#'   for the final point of each `"line"`, `"spline"` and `"area"` series, or
#'   for all
#'   bars in `"bar"` / `"column"` charts.
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
#'   they are derived from `locale` (`"da"`/`"kl"` -> decimal `","`, big mark
#'   `"."`; other values -> decimal `"."`, big mark `","`).
#' @param stacking Optional stacking mode for `"area"`, `"column"` and `"bar"`
#'   charts. One of `"normal"` or `"percent"`, or `NULL` (no stacking).
#' @param palette Optional palette specification for the series colours. Either:
#'   * a single character name of an element in `statgl_palettes` (e.g.
#'     `"main"`, `"winter"`, `"autumn"`), or
#'   * a character vector of colour hex codes to pass directly to
#'     [highcharter::hc_colors()].
#' @param palette_reverse Logical; if `TRUE`, reverse the palette when a named
#'   Statgl palette is used. Ignored when `palette` is a vector of hex colours.
#'   If the named palette is not found, a warning is issued and the default
#'   Highcharts colours are used.
#' @param pyramid Pyramid layout for two-group charts (e.g. population
#'   pyramids). Default `NULL` (or `FALSE`) draws a normal chart. Other forms:
#'   * `TRUE` -- enable pyramid mode using the column passed to `group =` as
#'     the splitting variable. Convention: men go on the right. Side ordering
#'     is chosen by (a) a male-label heuristic
#'     (`M`/`M\u00e6nd`/`Men`/`Angutit`/...) putting men on the right,
#'     (b) a female-label heuristic putting women on the left, (c) factor
#'     levels if the group column is a factor, or (d) first appearance
#'     otherwise. When `TRUE`, the group column must resolve to exactly two
#'     distinct values.
#'   * A single string such as `"M"` -- treated as the men/right value; the
#'     other side is inferred from the data (the single remaining value, or
#'     the one matching the female-label heuristic if more than one remains).
#'   * A length-2 character vector `c(left, right)` such as `c("K", "M")`
#'     setting the sides explicitly.
#'
#'   When `pyramid` is a string or a length-2 vector, rows whose `group`
#'   value isn't one of the named levels are silently dropped (useful for
#'   PXWeb tables that include `"I alt"` / `"T"` totals alongside the two
#'   sex codes). Series and legend order are then locked to pyramid order so
#'   the legend reads left -> right with men on the right.
#'
#'   It composes with any `type`; if `type` is not supplied it defaults to
#'   `"bar"` (rather than the usual `"line"` inference for integer ages).
#'   For `"bar"` and `"column"`, `stacking` defaults to `"normal"` so the
#'   two sides share the zero baseline; for `"area"`, `"line"` and friends
#'   it is left unset, since each series is already drawn from baseline 0
#'   and forcing stacking on a mixed-sign area chart causes Highcharts to
#'   clip the negated side.
#'
#'   Pyramid always renders horizontally (categorical axis vertical, value
#'   axis horizontal extending left/right of zero -- the conventional
#'   orientation, equivalent to applying `ggplot2::coord_flip()`). For
#'   `type = "bar"` Highcharts handles this automatically; for `"area"`,
#'   `"line"`, `"column"`, `"spline"` and `"areaspline"` it is achieved with
#'   `chart.inverted = TRUE`. The x-axis is set to `reversed = FALSE` so
#'   age 0 sits at the bottom. When `x` has more than ~30 distinct values
#'   and `height` is not passed explicitly, height is scaled up to as tall as
#'   is allowed.
#' @param highlight Optional character vector of labels to visually
#'   emphasise. Matching elements are drawn in the Statgl accent orange
#'   (`#faa41a`); everything else is drawn in neutral grey (`#d3d3d3`).
#'   Overrides `palette` when set. Dispatch depends on chart shape:
#'   * **Grouped chart** (`group =` supplied): `highlight` matches against
#'     series names (the `group` values). Line/area types additionally get
#'     a thicker stroke and a higher `zIndex` so the highlighted series
#'     sits in the foreground.
#'   * **Ungrouped bar / column chart**: `highlight` matches against the
#'     `x` values, re-colouring individual bars. Useful for emphasising
#'     one district, commodity, etc. on a per-bar chart.
#'   * **Anything else ungrouped** (line, scatter, ...): no-op with a
#'     warning, since there's nothing series- or bar-shaped to single out.
#'
#'   Examples: `statgl_plot(df, bydel, highlight = "Nuuk")` highlights the
#'   Nuuk bar; `statgl_plot(df, time, value, group = commodity,
#'   highlight = "I alt")` highlights the totals line.
#' @param height Numeric chart height in pixels passed to
#'   [highcharter::hc_chart()]. Defaults to `300`.
#' @param legend_position Where to place the legend. One of `"top"`,
#'   `"bottom"` (default), `"left"`, `"right"`. Any other value (e.g.
#'   `"none"`, `NULL`, `FALSE`) hides the legend.
#' @param ... Additional arguments forwarded to [highcharter::hchart()].
#'   Names that collide with arguments `statgl_plot()` already sets
#'   (`object`, `type`, `mapping`, `name`) are silently ignored so the
#'   wrapper's own values win.
#'
#' @return A [highcharter::highchart] object.
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
  show_last_value = FALSE,
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
  palette_reverse = FALSE,
  pyramid = NULL,
  highlight = NULL,
  height = 300,
  legend_position = "bottom",
  ...
) {
  # --- number formatting setup -----------------------------------
  big_mark_missing <- missing(big.mark)
  decimal_mark_missing <- missing(decimal.mark)
  height_user_set      <- !missing(height)

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

  # --- y validation ---------------------------------------------
  # Warn (don't error) if the input y has negative values. This runs on
  # the *original* y, before any pyramid mirroring, so the check is about
  # what the user actually passed in. Only fires when y resolves to a bare
  # column name -- expression y's (e.g. `y = log(value)`) are left alone.
  if (rlang::is_symbol(y_expr)) {
    y_name_check <- rlang::as_name(y_expr)
    if (y_name_check %in% names(df)) {
      y_vals_check <- df[[y_name_check]]
      if (is.numeric(y_vals_check) && any(y_vals_check < 0, na.rm = TRUE)) {
        warning(
          "`y` column \"", y_name_check, "\" contains negative values. ",
          "`statgl_plot()` expects non-negative y; the chart may render ",
          "unexpectedly.",
          call. = FALSE
        )
      }
    }
  }

  # --- pyramid setup --------------------------------------------
  # Resolve pyramid early so we can mutate `df` *before* hchart() builds
  # series. Uses the same `group =` column as the rest of the function -- it is
  # a modifier on a grouped chart, not its own grouping mechanism.
  pyramid_on     <- !is.null(pyramid) && !identical(pyramid, FALSE)
  pyramid_levels <- NULL

  if (pyramid_on) {
    if (!has_group) {
      stop(
        "`pyramid` requires `group = <column>`. ",
        "Did you mean: statgl_plot(", deparse(rlang::enexpr(df)),
        ", x = ", deparse(x_expr),
        ", group = <column>, pyramid = ",
        if (isTRUE(pyramid)) "TRUE"
        else paste0("c(\"", paste(pyramid, collapse = "\", \""), "\")"),
        ")?",
        call. = FALSE
      )
    }
    if (!rlang::is_symbol(group_expr)) {
      stop(
        "`pyramid` requires `group =` to be a bare column name.",
        call. = FALSE
      )
    }
    if (!rlang::is_symbol(y_expr)) {
      stop(
        "`pyramid` requires `y =` to be a bare column name (got an ",
        "expression). Pre-compute the column or rename it.",
        call. = FALSE
      )
    }
    if (!isTRUE(pyramid) && !is.character(pyramid)) {
      stop(
        "`pyramid` must be TRUE/FALSE/NULL, a single string naming the ",
        "men/right value (e.g. \"M\"), or a length-2 character vector ",
        "like c(\"K\", \"M\").",
        call. = FALSE
      )
    }

    group_name <- rlang::as_name(group_expr)
    y_name     <- rlang::as_name(y_expr)

    if (!group_name %in% names(df)) {
      stop("`group` column \"", group_name, "\" not found in `df`.",
           call. = FALSE)
    }
    if (!y_name %in% names(df)) {
      stop("`y` column \"", y_name, "\" not found in `df`.", call. = FALSE)
    }

    pyramid_levels <- .resolve_pyramid_levels(
      pyramid     = pyramid,
      group_vals  = df[[group_name]],
      group_name  = group_name
    )

    # Drop rows whose group value isn't one of the two pyramid levels.
    # This silently removes totals like "I alt" / "T" when the user passed
    # `pyramid = c("K","M")` or `pyramid = "M"`. We work on a local copy
    # of df so the caller's data is untouched.
    df <- df[as.character(df[[group_name]]) %in% pyramid_levels, ,
             drop = FALSE]

    # Lock the group column's order to c(left, right) so hchart() builds
    # series in pyramid order; this also makes the legend read left -> right.
    df[[group_name]] <- factor(
      as.character(df[[group_name]]),
      levels = pyramid_levels
    )

    # Mirror the left side across zero.
    is_left <- as.character(df[[group_name]]) == pyramid_levels[1]
    df[[y_name]][is_left] <- -df[[y_name]][is_left]

    # Default to bars when the user didn't set a type. Without this, an integer
    # age column would be inferred as "line", which is rarely what you want for
    # a pyramid. Other types ("area", "column", ...) still compose if asked.
    if (is.null(type)) {
      type <- "bar"
    }

    # For bar/column pyramids the two sides need a shared baseline, otherwise
    # they render dodged side-by-side. Force `stacking = "normal"` only for
    # those types -- area/line/spline don't need it (each series is already
    # drawn from the zero baseline) and forcing it on area in particular
    # collapses the auto-bounds of the negated side, clipping it visually.
    if (is.null(stacking) && type %in% c("bar", "column")) {
      stacking <- "normal"
    }

    # Auto-scale height when there are many bars and the user didn't set
    # `height` explicitly.
    if (!height_user_set) {
      x_name <- rlang::as_name(x_expr)
      if (x_name %in% names(df)) {
        n_x <- length(unique(df[[x_name]]))
        if (n_x > 30L) {
          height <- NULL
        }
      }
    }
  }

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

  # Forward any user-supplied `...` to hchart(), but never let it overwrite
  # the args we already set on purpose above.
  dots <- rlang::list2(...)
  if (length(dots) > 0L) {
    keep <- setdiff(names(dots), c("object", "type", "mapping", "name"))
    if (length(keep) > 0L) {
      args[keep] <- dots[keep]
    }
  }

  chart <- rlang::exec(highcharter::hchart, !!!args)

  # Pyramid implies a coord-flip-style horizontal layout (categorical axis
  # vertical, value axis horizontal extending left/right of zero). `type =
  # "bar"` is auto-inverted by Highcharts already; any other pyramid type
  # needs `chart.inverted = TRUE` to get the conventional orientation.
  if (pyramid_on && !identical(type, "bar")) {
    chart <- highcharter::hc_chart(chart, inverted = TRUE)
  }

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
  neutral_ink <- "#7d7d7d"

  # X axis
  x_axis_opts <- list(
    labels = list(style = list(color = neutral_ink))
  )

  if (!is.null(xlab) && nzchar(xlab)) {
    x_axis_opts$title <- list(
      text = xlab,
      style = list(color = neutral_ink)
    )
  } else {
    x_axis_opts$title <- list(
      text = NULL,
      style = list(color = neutral_ink)
    )
  }

  # Pyramid charts are always inverted (bar auto-inverts, others get
  # chart.inverted = TRUE above), and Highcharts defaults xAxis.reversed to
  # TRUE on inverted charts -- which puts the largest value at the bottom.
  # Override so age 0 sits at the bottom and age 100 at the top, regardless
  # of pyramid type.
  if (pyramid_on) {
    x_axis_opts$reversed <- FALSE
  }

  chart <- do.call(highcharter::hc_xAxis, c(list(chart), x_axis_opts))

  # Y axis
  y_axis_labels <- list(style = list(color = neutral_ink))
  if (pyramid_on) {
    # Show absolute values on the axis since the left side is negated.
    y_axis_labels$formatter <- highcharter::JS(sprintf(
      'function() {
         return Highcharts.numberFormat(Math.abs(this.value), %d, "%s", "%s") + "%s";
       }',
      digits,
      decimal_mark,
      big_mark,
      suffix_js
    ))
  }
  y_axis_opts <- list(
    labels = y_axis_labels,
    title = list(
      style = list(color = "#7d7d7d")
    ),
    gridLineColor = "#7d7d7d",
    gridLineWidth = 0.25
  )

  if (is.null(ylab) || !nzchar(ylab)) {
    y_axis_opts$title <- list(
      text = NULL,
      enabled = FALSE,
      style = list(color = neutral_ink)
    )
  } else {
    y_axis_opts$title <- list(
      text = ylab,
      enabled = TRUE,
      style = list(color = neutral_ink)
    )
  }

  chart <- do.call(highcharter::hc_yAxis, c(list(chart), y_axis_opts))

  # --- tooltip ---------------------------------------------------
  # When pyramid is on, the underlying y is negated for the left side; the
  # tooltip should always show the magnitude.
  y_value_js <- if (pyramid_on) "Math.abs(this.y)" else "this.y"

  if (!is.null(tooltip)) {
    # user-supplied tooltip JS wins
    chart <- highcharter::hc_tooltip(
      chart,
      formatter = highcharter::JS(tooltip)
    )
  } else {
    # Build a pointFormatter that optionally prepends the series name
    if (has_group) {
      pf_js <- highcharter::JS(sprintf(
        'function() {
           var name = (this.series && this.series.name)
             ? this.series.name + ": "
             : "";
           var value = Highcharts.numberFormat(%s, %d, "%s", "%s") + "%s";
           return name + value;
         }',
        y_value_js,
        digits,
        decimal_mark,
        big_mark,
        suffix_js
      ))
    } else {
      pf_js <- highcharter::JS(sprintf(
        'function() {
           var value = Highcharts.numberFormat(%s, %d, "%s", "%s") + "%s";
           return value;
         }',
        y_value_js,
        digits,
        decimal_mark,
        big_mark,
        suffix_js
      ))
    }

    chart <- highcharter::hc_tooltip(
      chart,
      shared = FALSE, # can change to has_group later if you want shared tooltips
      valueDecimals = digits,
      valueSuffix = suffix,
      pointFormatter = pf_js
    )
  }

  # consistent tooltip text color
  chart <- highcharter::hc_tooltip(
    chart,
    style = list(color = neutral_ink)
  )

  # --- dataLabels + stacking -------------------------------------
  series_opts <- list()

  if (isTRUE(show_last_value)) {
    if (type %in% c("line", "spline", "area")) {
      series_opts$dataLabels <- list(
        enabled = TRUE,
        style = list(
          color = neutral_ink,
          textOutline = "white" #
        ),
        formatter = highcharter::JS(sprintf(
          'function() {
           if (this.point.index !== this.series.data.length - 1) return null;
           return Highcharts.numberFormat(%s, %d, "%s", "%s") + "%s";
         }',
          y_value_js,
          digits,
          decimal_mark,
          big_mark,
          suffix_js
        ))
      )
    } else if (type %in% c("bar", "column")) {
      series_opts$dataLabels <- list(
        enabled = TRUE,
        style = list(
          color = neutral_ink
        ),
        formatter = highcharter::JS(sprintf(
          'function() {
           return Highcharts.numberFormat(%s, %d, "%s", "%s") + "%s";
         }',
          y_value_js,
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
  # When `highlight` is set we colour the chart ourselves below, so skip
  # the palette pass entirely. That way users can pass the default
  # `palette = "main"` together with `highlight` without conflict.
  if (!is.null(palette) && is.null(highlight)) {
    # Get existing series from chart
    series_list <- chart$x$hc_opts$series
    if (is.null(series_list)) {
      series_list <- list()
    }

    # Named Statgl palette?
    if (
      is.character(palette) &&
        length(palette) == 1L &&
        exists("statgl_palettes", inherits = TRUE)
    ) {
      pal_list <- get("statgl_palettes", inherits = TRUE)
      base_pal <- pal_list[[palette]]

      if (is.null(base_pal)) {
        warning(
          "Palette '",
          palette,
          "' not found in statgl_palettes; using Highcharts defaults."
        )
      } else {
        if (isTRUE(palette_reverse)) {
          base_pal <- rev(base_pal)
        }

        # Special case: ungrouped column/bar chart -> colour *each bar*
        if (
          !has_group &&
            type %in% c("column", "bar") &&
            length(series_list) == 1L
        ) {
          n_points <- length(series_list[[1]]$data)
          if (n_points < 1L) {
            n_points <- 1L
          }

          ramp <- grDevices::colorRampPalette(base_pal)
          cols <- ramp(n_points)

          for (i in seq_len(n_points)) {
            d <- series_list[[1]]$data[[i]]
            if (is.list(d)) {
              d$color <- cols[i]
            } else {
              # data might be a bare y value
              d <- list(y = d, color = cols[i])
            }
            series_list[[1]]$data[[i]] <- d
          }

          chart$x$hc_opts$series <- series_list
        } else {
          # Default: colour per series (lines, grouped columns, etc.)
          n_series <- length(series_list)
          if (n_series < 1L) {
            n_series <- 1L
          }

          ramp <- grDevices::colorRampPalette(base_pal)
          pal_vals <- ramp(n_series)

          chart <- highcharter::hc_colors(chart, pal_vals)
        }
      }
    } else if (!is.null(palette) && !is.character(palette)) {
      # Direct vector of hex colours supplied
      chart <- highcharter::hc_colors(chart, palette)
    }
  }

  # --- highlight -------------------------------------------------
  # Re-colour the chart so values in `highlight` are the Statgl accent
  # orange and everything else is neutral grey. Dispatch by shape:
  #   * grouped chart        -> match against series names (per-series colour)
  #   * ungrouped bar/column -> match against x values (per-point colour)
  #   * anything else        -> warn (nothing meaningful to single out)
  if (!is.null(highlight)) {
    highlight_set   <- as.character(highlight)
    highlight_color <- "#faa41a"
    neutral_color   <- "#d3d3d3"

    if (has_group) {
      # --- per-series highlight ----------------------------------
      series_list <- chart$x$hc_opts$series
      if (is.null(series_list)) {
        series_list <- list()
      }

      series_names <- vapply(
        series_list,
        function(s) if (is.null(s$name)) NA_character_ else as.character(s$name),
        character(1)
      )

      matched <- series_names %in% highlight_set

      if (length(series_list) > 0L && !any(matched)) {
        warning(
          "`highlight` matched no series. Looked for ",
          paste0("\"", highlight_set, "\"", collapse = ", "),
          "; series are ",
          paste0("\"", series_names, "\"", collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      cols <- ifelse(matched, highlight_color, neutral_color)
      chart <- highcharter::hc_colors(chart, cols)

      # Emphasise highlighted lines / areas: thicker stroke + foreground.
      if (type %in% c("line", "spline", "area", "areaspline")) {
        for (i in seq_along(series_list)) {
          if (isTRUE(matched[i])) {
            series_list[[i]]$lineWidth <- 3
            series_list[[i]]$zIndex    <- 5
          } else {
            series_list[[i]]$zIndex    <- 1
          }
        }
        chart$x$hc_opts$series <- series_list
      }

    } else if (type %in% c("bar", "column")) {
      # --- per-point highlight on an ungrouped bar/column chart ---
      # Match against the x values; recolour individual points in the
      # single series. Falls back to df row order when point names aren't
      # exposed by hchart (they usually are for categorical x).
      x_name <- rlang::as_name(x_expr)
      series_list <- chart$x$hc_opts$series
      if (is.null(series_list)) series_list <- list()

      if (length(series_list) >= 1L &&
          length(series_list[[1]]$data) > 0L) {

        pts <- series_list[[1]]$data
        n_pts <- length(pts)

        # Prefer matching on point name (hchart sets this for categorical x);
        # fall back to df[[x_name]] in row order when names are absent.
        pt_names <- vapply(
          pts,
          function(d) if (is.list(d) && !is.null(d$name)) {
            as.character(d$name)
          } else {
            NA_character_
          },
          character(1)
        )

        if (all(is.na(pt_names)) && x_name %in% names(df) &&
            length(df[[x_name]]) == n_pts) {
          pt_names <- as.character(df[[x_name]])
        }

        matched <- pt_names %in% highlight_set

        if (!any(matched)) {
          warning(
            "`highlight` matched no bars. Looked for ",
            paste0("\"", highlight_set, "\"", collapse = ", "),
            "; x values are ",
            paste0("\"", unique(pt_names[!is.na(pt_names)]), "\"",
                   collapse = ", "),
            ".",
            call. = FALSE
          )
        }

        cols <- ifelse(matched, highlight_color, neutral_color)

        for (i in seq_len(n_pts)) {
          d <- pts[[i]]
          if (is.list(d)) {
            d$color <- cols[i]
          } else {
            d <- list(y = d, color = cols[i])
          }
          series_list[[1]]$data[[i]] <- d
        }
        chart$x$hc_opts$series <- series_list
      }

    } else {
      warning(
        "`highlight` has no effect on ungrouped ", type, " charts. ",
        "Pass `group =` to highlight a series, or use a bar/column ",
        "chart to highlight a category.",
        call. = FALSE
      )
    }
  }

  # --- height ----------------------------------------------------
  chart <- highcharter::hc_chart(chart, height = height)

  # --- legend ----------------------------------------------------
  # `legend_position`: one of top/bottom/left/right places the legend;
  # anything else (including NULL/FALSE/"none") hides it.
  legend_args <- list(itemStyle = list(color = "#7d7d7d"))

  legend_position_lc <- if (
    is.character(legend_position) && length(legend_position) == 1L
  ) tolower(legend_position) else ""

  if (legend_position_lc == "top") {
    legend_args$align <- "center"
    legend_args$verticalAlign <- "top"
    legend_args$layout <- "horizontal"
  } else if (legend_position_lc == "bottom") {
    legend_args$align <- "center"
    legend_args$verticalAlign <- "bottom"
    legend_args$layout <- "horizontal"
  } else if (legend_position_lc == "left") {
    legend_args$align <- "left"
    legend_args$verticalAlign <- "middle"
    legend_args$layout <- "vertical"
  } else if (legend_position_lc == "right") {
    legend_args$align <- "right"
    legend_args$verticalAlign <- "middle"
    legend_args$layout <- "vertical"
  } else {
    legend_args$enabled <- FALSE
  }

  # Highcharts reverses the legend on inverted/bar charts so it reads top to
  # bottom with the bars. For pyramids we already locked the series order to
  # c(left, right) via factor levels, so force `reversed = FALSE` to keep
  # the legend reading left -> right in pyramid order (men on the right).
  if (pyramid_on) {
    legend_args$reversed <- FALSE
  }

  chart <- do.call(highcharter::hc_legend, c(list(chart), legend_args))

  chart <- htmlwidgets::onRender(
    chart,
    '
  function(el, x) {

    function findChart() {
      // Highcharts keeps all charts in Highcharts.charts
      for (var i = 0; i < Highcharts.charts.length; i++) {
        var c = Highcharts.charts[i];
        if (c && c.renderTo && c.renderTo === el) return c;
      }
      // fallback: sometimes renderTo is a child of el
      for (var i = 0; i < Highcharts.charts.length; i++) {
        var c = Highcharts.charts[i];
        if (c && c.renderTo && el.contains(c.renderTo)) return c;
      }
      return null;
    }

    function applyOutline() {
      var dark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;

      // pick what you actually want
      var outline = dark
        ? ".5px rgba(0,0,0,0.70)"          // dark mode: dark halo (prevents glow)
        : ".5px rgba(255,255,255,0.35)"; // light mode: subtle off-white halo

      var chart = findChart();
      if (!chart) return;

      chart.update({
        plotOptions: {
          series: {
            dataLabels: {
              style: { textOutline: outline }
            }
          }
        }
      }, false);

      chart.redraw();
    }

    applyOutline();

    // Update live if the OS/browser theme toggles
    if (window.matchMedia) {
      var mql = window.matchMedia("(prefers-color-scheme: dark)");
      if (mql.addEventListener) mql.addEventListener("change", applyOutline);
      else if (mql.addListener) mql.addListener(applyOutline); // Safari fallback
    }
  }
  '
  )

  chart
}

# Internal: sex-coded labels across the languages Statistics Greenland uses.
# Used by `pyramid = TRUE` to put males on the right and females on the left
# when the group column doesn't otherwise dictate order. Factor levels are
# still respected if no sex-coded label is found.
.statgl_male_labels <- c(
  "M", "M\u00e6nd", "Maend", "Mand", "Men", "Male",
  "Angutit", "Angut"
)
.statgl_female_labels <- c(
  "K", "Kvinder", "Kvinde", "Women", "Woman", "Female", "F",
  "Arnat", "Arnaq"
)

# Internal: resolve `pyramid` argument + the group column's values into a
# length-2 character vector c(left, right).
#
# Convention: men go on the right when we can detect them. Explicit length-2
# input is trusted as-is (user picked the order). length-1 is the
# "men"/right value and the other side is inferred from the data.
#
# Errors with friendly messages on:
#   - pyramid not TRUE/FALSE/NULL/length-1/length-2 character
#   - group column resolving to != 2 distinct present values (pyramid = TRUE)
#   - explicit pyramid levels not all present in the data
#   - length-1 pyramid ambiguous (more than 2 distinct group values present
#     and no female label among the others)
.resolve_pyramid_levels <- function(pyramid, group_vals, group_name) {
  present <- unique(as.character(group_vals[!is.na(group_vals)]))

  if (is.character(pyramid)) {
    # Explicit length-2: trust the user, verify both levels exist.
    if (length(pyramid) == 2L) {
      missing <- setdiff(pyramid, present)
      if (length(missing) > 0L) {
        stop(
          "`pyramid` levels not found in `", group_name, "`: ",
          paste0("\"", missing, "\"", collapse = ", "),
          ". Present values: ",
          paste0("\"", present, "\"", collapse = ", "), ".",
          call. = FALSE
        )
      }
      return(pyramid)
    }

    # Length-1: this is the men/right value. Find the other side.
    if (length(pyramid) == 1L) {
      if (!pyramid %in% present) {
        stop(
          "`pyramid` value \"", pyramid, "\" not found in `", group_name,
          "`. Present values: ",
          paste0("\"", present, "\"", collapse = ", "), ".",
          call. = FALSE
        )
      }
      others <- setdiff(present, pyramid)
      if (length(others) == 0L) {
        stop(
          "`pyramid = \"", pyramid, "\"` needs at least one other value ",
          "in `", group_name, "`; only this value is present.",
          call. = FALSE
        )
      }
      if (length(others) == 1L) {
        return(c(others, pyramid))
      }
      # Multiple "others" -- disambiguate via female-label heuristic.
      female_match <- intersect(.statgl_female_labels, others)
      if (length(female_match) == 1L) {
        return(c(female_match, pyramid))
      }
      stop(
        "`pyramid = \"", pyramid, "\"` is ambiguous: multiple other ",
        "values in `", group_name, "` (",
        paste0("\"", others, "\"", collapse = ", "),
        "). Pass `pyramid = c(left, right)` to disambiguate.",
        call. = FALSE
      )
    }

    stop(
      "`pyramid` must be TRUE, a single string naming the men/right value, ",
      "or a length-2 character vector. Got length ", length(pyramid), ".",
      call. = FALSE
    )
  }

  # pyramid == TRUE: derive ordering from the data.
  if (length(present) != 2L) {
    stop(
      "`pyramid` requires `group = ", group_name, "` to have exactly 2 ",
      "distinct values; found ", length(present),
      if (length(present) > 0L)
        paste0(" (", paste0("\"", present, "\"", collapse = ", "), ")")
      else "",
      ". Pass `pyramid = c(\"left\", \"right\")` (or a single \"men\" ",
      "value) to set them explicitly.",
      call. = FALSE
    )
  }

  # Men on the right when we can detect them, regardless of factor/character.
  male_match <- intersect(.statgl_male_labels, present)
  if (length(male_match) == 1L) {
    return(c(setdiff(present, male_match), male_match))
  }

  # Women on the left as a secondary heuristic.
  female_match <- intersect(.statgl_female_labels, present)
  if (length(female_match) == 1L) {
    return(c(female_match, setdiff(present, female_match)))
  }

  # No sex detection -- fall back to factor levels or first-appearance order.
  if (is.factor(group_vals)) {
    lv <- levels(group_vals)
    lv <- lv[lv %in% present]
    return(lv)
  }

  present
}
