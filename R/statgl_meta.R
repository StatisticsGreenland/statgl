#' Retrieve metadata from Statbank
#'
#' Retrieves metadata from pxweb API url.
#'
#' @param url API url of Statbank table
#'
#' @return
#' @export
#'
#' @examples
#' statgl_meta(statgl_url("BEXST1"))
statgl_meta <- function(url) {
  api_get <- httr::content(httr::GET(url))
  structure(api_get, class = "statgl_meta")
}

# Print function ---------------------------------------------------------------

#' @export
#' @importFrom utils head
print.statgl_meta <- function(x, n_values = 6, ...) {

  px_title <- x[["title"]]
  px_vars <- x[["variables"]]

  cat(paste("\nTitle:", px_title, "\n\n"))

  for(i in px_vars) {

    cat(paste0(
      "  code: ", i[["code"]], "\n",
      "  text: ", i[["text"]], "\n")
    )

    ellipses <- ifelse(length(i[["values"]]) > n_values, "...", "")
    cat(
      "  vals:",
      paste0("{", head(i[["values"]], n_values), ":",
             head(i[["valueTexts"]], n_values), "}"),
      ellipses,
      "\n\n")
  }
}
