#' Retrieve metadata from Statbank
#'
#' Retrieves metadata from pxweb API url.
#'
#' @param url API url of Statbank table
#' @param returnclass Returns list with tibble if \code{"tibble"} (default).
#'
#' @return
#' @export
#'
#' @examples
#' statgl_meta(statgl_url("BEXSTA"))
statgl_meta <- function(url, returnclass = "tibble") {

  if(returnclass == "tibble") {
  if (!is_valid_url(url)) {
    url <- statgl_url(url)
  }
  api_get <- httr::content(httr::GET(url))

  meta_list <- lapply(api_get$variables, tibble::as_tibble)

  tbl_list <- lapply(
    meta_list, dplyr::mutate, values = unlist(values),
    valueTexts = unlist(valueTexts)
  )

  tbl <- dplyr::summarise(
    dplyr::group_by(
      dplyr::bind_rows(tbl_list),
      code, text, elimination, time),
    values = list(values), valueTexts = list(valueTexts),
    .groups = "drop")

  return(list(title = api_get$title, url = url, variables = tbl))

  } else {

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

    return(api_get)
  }

}
