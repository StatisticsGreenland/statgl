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

  if (!is_valid_url(url)) {
    url <- toupper(url)
    lang <- get_language_code(substr(url, 3, 3))
    if (is.na(lang)) {
      lang <- "en"
    }
    url <- paste0(substr(url, 1, 2), "X", substr(url, 4, nchar(url)))
    url <- statgl_url(url, lang = lang)
  }
  api_get <- httr::content(httr::GET(url))

  validate_px(url)

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
