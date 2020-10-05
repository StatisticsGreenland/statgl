#' Statistics Greenland color scale
#'
#' Generates a color scale compatible with ggplot2. Uses colors defined by the GS design manual.
#'
#' @param palette Character string with name of palette. Can be changed to \code{"dawn"}, etc. Defaults to \code{"main"}
#' @param discrete Generate discrete palette if \code{TRUE}
#' @param reverse Generate reverse palette if \code{TRUE}
#' @param ... Parameters to \code{discrete_scale} or \code{scale_color_gradientn}
#'
#' @return
#' @export
#'
#' @examples
#' gg_example <- ggplot2::qplot(displ, hwy, data = ggplot2::mpg, color = class)
#'
#' gg_example + scale_color_statgl()
#' gg_example + scale_color_statgl(palette = "aurora", reverse = TRUE)
scale_color_statgl <-
  function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

    if(!palette %in% names(statgl_palettes)){
      stop(
        paste(c("Palette must be one of: ", paste(names(statgl_palettes), collapse = ", "))),
        call. = FALSE)
    }

    pal <- statgl_pal(palette = palette, reverse = reverse)

    if (discrete) {
      ggplot2::discrete_scale("colour", paste0("statgl_", palette), palette = pal, ...)
    } else {
      ggplot2::scale_color_gradientn(colours = pal(256), ...)
    }
  }

#' Statistics Greenland fill scale
#'
#' Generates a fill scale compatible with ggplot2. Uses colors defined by the GS design manual.
#'
#' @param palette Character string with name of palette. Can be changed to \code{"dawn"}, etc. Defaults to \code{"main"}
#' @param discrete Generate discrete palette if \code{TRUE}
#' @param reverse Generate reverse palette if \code{TRUE}
#' @param ... Parameters to \code{discrete_scale} or \code{scale_color_gradientn}
#'
#' @return
#' @export
#'
#' @examples
#' gg_example <- ggplot2::qplot(manufacturer, fill = manufacturer, data = ggplot2::mpg)
#'
#' gg_example + scale_fill_statgl()
#' gg_example + scale_fill_statgl(palette = "aurora")
scale_fill_statgl <-
  function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

    if(!palette %in% names(statgl_palettes)){
      stop(
        paste(c("Palette must be one of: ", paste(names(statgl_palettes), collapse = ", "))),
        call. = FALSE)
    }

    pal <- statgl_pal(palette = palette, reverse = reverse)

    if (discrete) {
      ggplot2::discrete_scale("fill", paste0("statgl_", palette), palette = pal, ...)
    } else {
      ggplot2::scale_fill_gradientn(colours = pal(256), ...)
    }
  }

# Official colors of Statistics Greenland --------------------------------------

statgl_colors <- c(
  # From design manual
  "darkblue"     = "#004459",
  "darkgreen"    = "#939905",
  "green"        = "#94BB1F",
  "blue"         = "#007F99",
  "lightgreen"   = "#CEE007",
  "orange"       = "#faa41a",
  "peach"        = "#F97242",
  "darkorange"   = "#F95602",

  # Greys
  "darkgrey"     = "#848c8c",
  "grey"         = "#b8bab8",
  "lightgrey"    = "#f1f2f2",

  # Not from design manual, strictly for logo
  "logo_blue"    = "#002a3a",
  "logo_orange"  = "#f16728"
)

# Function to get hex keys from string/index -----------------------------------

statgl_cols <- function(...){
  cols <- c(...)

  if(is.null(cols)){
    return(statgl_colors)
  }

  statgl_colors[cols]
}

# List of palettes -------------------------------------------------------------

statgl_palettes <- list(
  `main`   = statgl_cols("darkblue", "blue", "green", "lightgreen",
                         "orange", "peach", "darkorange"),

  `logo`   = statgl_cols("logo_blue", "logo_orange"),

  `dawn`   = statgl_cols("darkblue", "peach", "orange"),
  `aurora` = statgl_cols("darkblue", "lightgreen"),

  `blue`   = statgl_cols("darkblue", "blue"),
  `green`  = statgl_cols("darkgreen", "green", "lightgreen"),
  `orange` = statgl_cols("darkorange", "peach", "orange"),

  `grey`   = statgl_cols("darkgrey", "grey", "lightgrey"),

  `winter` = statgl_cols("blue", "lightgrey", "orange"),
  `spring` = statgl_cols("blue", "lightgreen", "orange"),
  `summer` = statgl_cols("darkblue", "lightgreen", "darkorange"),
  `autumn` = statgl_cols("blue", "peach", "orange")
)

# Function to access and interpolate palette -----------------------------------

statgl_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- statgl_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
