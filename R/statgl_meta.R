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
#' statgl_meta(statgl_url("BEXSTA"))
statgl_meta <- function(url) {

  if (!is_valid_url(url)) {
    url <- statgl_url(url)
  }
  api_get <- httr::content(httr::GET(url))

  cat(paste0(unique(api_get$title), "\n\n"))
  meta_list <- lapply(api_get$variables, tibble::as_tibble)
  lapply(meta_list, dplyr::mutate, values = unlist("values"), valueTexts = unlist("valueTexts"))
}
