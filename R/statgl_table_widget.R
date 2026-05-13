# Internal htmlwidget wrapper for pre-rendered HTML tables.
#
# Both statgl_table() and statgl_crosstable() build their tables as
# kableExtra HTML and then pass the rendered string through this
# wrapper. The widget itself is intentionally trivial — its JS shim
# (inst/htmlwidgets/statgl_table.js) does `el.innerHTML = x.html` and
# nothing else. The reason for routing through htmlwidgets at all is
# integration parity with `statgl_plot()` (which is a highchart
# widget): a single return value that opens in the RStudio Viewer
# interactively, renders in knitr / Quarto chunks, and survives
# Quarto's shortcode-parameter interpolation the way other widgets
# do.

statgl_table_widget <- function(html, width = NULL, height = NULL) {
  htmlwidgets::createWidget(
    name    = "statgl_table",
    x       = list(html = html),
    width   = width,
    height  = height,
    package = "statgl",
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth  = "100%",
      defaultHeight = "auto",
      # Don't treat the table as a figure / iframe in knitr — render
      # inline so it inherits the surrounding document's bootstrap CSS.
      knitr.figure  = FALSE,
      knitr.defaultWidth  = "100%",
      knitr.defaultHeight = "auto",
      viewer.fill   = FALSE,
      browser.fill  = FALSE
    )
  )
}
