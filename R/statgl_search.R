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
#' @param returnclass Return class from search query. Must be one of
#' \code{"tibble"} or \code{"list"}. Defaults to tibble. Returns to default list
#' from API when list.
#'
#' @return
#' @export
#'
#' @importFrom utils URLencode
#'
#' @examples
#' statgl_search("population")
#' statgl_search("inuit", lang = "kl")
statgl_search <- function(
    query = "", lang = "en", path = "",
    api_url = paste0("https://bank.stat.gl:443/api/v1/", lang, "/Greenland/", path),
    returnclass = "tibble"
) {
  query <- URLencode(query)
  search_result <- httr::content(httr::GET(
    paste0(api_url,  "?query=", query)
  ))

  if(tolower(returnclass) == "tibble") {
    df_list <- lapply(search_result, as.data.frame)
    df <- dplyr::bind_rows(df_list)

    if(query != "") {
      df$path <- paste0(df$path, "/", df$id)
      df$title <- trimws(gsub("<em>.*", "", df$title))
    }
    df$id <- sub("\\.(px|PX)$", "", df$id)

    return(tibble::as_tibble(df))

  }

  if(tolower(returnclass) == "list") {
    return(structure(search_result, class = "px_search"))
  }

  stop("returnclass must be one of: 'tibble', 'list'")

}
