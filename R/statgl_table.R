#' Automatic statgl table
#'
#' Concvenience function to quickly write html tables. Returns a kable with automatic alignments and formats.
#'
#' @param df A data frame
#' @param year_col Name of year column. Disregards formatiing at this num column.
#' @param ... Parameters to \code{format}
#'
#' @return
#' @export
#'
#' @examples
#' statgl_table(ggplot2::mpg, year_col = year)
statgl_table <- function(df, year_col, replace_0s = FALSE, ...) {
  aligns <- paste0(c("l", rep("r", ncol(df))), collapse = "")

  if(!missing(year_col)){
    df <- dplyr::mutate(df, dplyr::across({{year_col}}, as.character))
  }

  if(replace_0s){
    df <- dplyr::mutate_all(df, as.character)
    df <- dplyr::mutate_all(df, trimws)
    df <- dplyr::mutate_all(df, plyr::mapvalues, from = "0", to = "[-]{}", warn_missing = FALSE)
  }

  df <- dplyr::mutate_if(df, is.numeric, format, big.mark = ".", decimal.mark = ",", justify = "right", digits = 3, ...)

  kableExtra::kable_styling(
    kableExtra::kable(df, align = aligns),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

}
