#' Search Statistics Greenland API
#'
#' Do a search of Statistics Greenland API. Will work for other PXWEB APIs, when
#' custom URL is provided in \code{api_url}
#'
#' @param query Search query
#' @param lang Language. Defaults to \code{"en"}, and must be one of
#' \code{c("en", "kl", "da")} for Statistics Greenland PX API
#' @param path Subfolder of API to search, e.g. \code{"BE"} for population
#' @param api_url Base API URL.
#'
#' @return
#' @export
#'
#' @examples
#' statgl_search("population")
#' statgl_search("inuit", lang = "kl")
statgl_search <- function(
  query, lang = "en", path = "",
  api_url = paste0("https://bank.stat.gl:443/api/v1/", lang, "/Greenland/", path)
  ) {
  query <- urltools::url_encode(query)
  search_result <- httr::content(httr::GET(
    paste0(api_url, "?query=", query)
    ))
  structure(search_result, class = "px_search")
}

# Print function ---------------------------------------------------------------

#' @export
#' @importFrom utils str
print.px_search <- function(x, ...) {
  str(x)
}
