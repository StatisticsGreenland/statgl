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
    query = "", path = "", lang = "en",
    api_url = paste0("https://bank.stat.gl:443/api/v1/", lang, "/Greenland/", path),
    returnclass = "tibble"
) {

  query <- URLencode(query)

  path <- toupper(path)
  if(nchar(path) > 2 & !grepl("/", path)) {
    path <- generate_subfolders(path)
    message(paste0("Trying path: ", path))
  }

  search_result <- httr::content(httr::GET(
    paste0(api_url,  "?query=", query)
  ))

  if(length(search_result) == 0) {
    stop("0 hits")
  }

  if(tolower(returnclass) == "tibble") {
    df_list <- lapply(search_result, tibble::as_tibble)
    df <- dplyr::bind_rows(df_list)

    if(query == "") {
      df$text <- trimws(gsub("<em>.*", "", df$text))
    } else {
      df$title <- trimws(gsub("<em>.*", "", df$title))
      df$type <- "t"
    }

    df$path <- gsub("/+", "/", paste0("/", toupper(path), "/", df$id))
    df$id <- sub("\\.(px|PX)$", "", df$id)

    df <- dplyr::select(
      df, "id", dplyr::any_of(c("text", "title")), "type", "path",
      dplyr::everything()
    )

    return(df)
  }

  else if(tolower(returnclass) == "list") {
    return(search_result)
  }

  stop("returnclass must be one of: 'tibble', 'list'")

}
