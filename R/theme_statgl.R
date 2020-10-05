#' Statistics Greenland ggplot2 theme
#'
#' A ggplot2 theme following design manual of Statistics Greenland.
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @param ...  Parameters to \code{ggplot2::theme}
#'
#' @return
#' @export
#'
#' @examples
#' ggplot2::qplot(mpg, data = mtcars) + theme_statgl()
theme_statgl <- function(base_size = 12, base_family = NULL, ...){

  ggplot2::theme_minimal(base_family = base_family, base_size = base_size, ...) +
    ggplot2::theme(

      # Plot title
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.9),
      plot.title.position = "plot",
      # Margins
      plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, unit = "cm"),

      # Legend
      legend.position = "bottom",
      legend.justification = 1,
      legend.key.size = ggplot2::unit(.5, "cm"),

      # Minimal axis text and grid
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = statgl_cols("grey"), fill = NA),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),

      # Strips for faceting
      strip.text = ggplot2::element_text(color = "black", hjust = 0),
      strip.background = ggplot2::element_rect(
        fill = "white", color = statgl_cols("grey")
      ),

    )
}

