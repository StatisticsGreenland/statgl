#' Render statgl report
#'
#' Wrapper function for rmarkdown::render(), to render html pages with statgl
#' design.
#'
#' This is a development version, and design of the output will change!
#'
#' @param output Format of report.
#' @param ... Additional arguments passed to rmarkdown::html_document
#'
#' @return
#' @export
#'
#' @examples
#' statgl_report()
statgl_report <- function(output = "html", ...){

  if(output == "html"){
    stylesheet <- system.file("statgl.css", package = "statgl")
    rmarkdown::html_document(
      css = stylesheet,
      ...
    )

  } else if(output == "docx") {
    word_template <- system.file("statgl.docx", package = "statgl")
    rmarkdown::word_document(
      reference_docx = word_template,
      fig_width = 5.11811024,
      fig_height = 2.75590551,
      ...
    )
  } else if(output == "pretty_html") {
    prettydoc::html_pretty(theme = "hpstr", highlight = "github", ...)
  }
}
