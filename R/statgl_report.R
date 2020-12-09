#' Render statgl report
#'
#' Wrapper function for rmarkdown::render(), to render html pages with statgl
#' design.
#'
#' This is a development version, and design of the output will change!
#'
#' @param output Format of report. Must be \code{"html"} in current release.
#' @param ... Additional arguments passed to rmarkdown::html_document
#'
#' @return
#' @export
#'
#' @examples
#' statgl_report()
statgl_report <- function(output = "html", ...){

  warning("This is a development version of statgl_report(), output is certain to change")
  if(output == "html"){
    stylesheet <- system.file("statgl.css", package = "statgl")
    rmarkdown::html_document(
      css = stylesheet,
      anchor_sections = FALSE,
      ...
    )
  }
}
