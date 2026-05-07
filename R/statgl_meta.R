#' Retrieve metadata from Statbank
#'
#' Retrieves metadata from pxweb API url.
#'
#' @param url API url of Statbank table, or a table ID.
#' @param returnclass One of `"tibble"` (default), `"px_list"`, or any other
#'   value to return the raw parsed API response.
#'
#' @return When `returnclass = "tibble"`, a list with elements `title`,
#'   `url`, and `variables` (a tibble of variable metadata). When
#'   `returnclass = "px_list"`, a named list of PX file metadata fields.
#'   Otherwise, the raw API response as parsed by [httr::content()].
#' @export
#'
#' @examples
#' \donttest{
#' statgl_meta(statgl_url("BEXSTA"))
#' }
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
      dplyr::across(dplyr::any_of(c("code", "text", "elimination", "time")))),
    values = list(values), valueTexts = list(valueTexts),
    .groups = "drop")

  return(list(title = api_get$title, url = url, variables = tbl))

  } else if(returnclass == "px_list") {
    if (!is_valid_url(url)) {
      url <- statgl_url(url)
    }
    px_content <- httr::content(
      httr::POST(
        url,
        body = build_query(list(), .format = "px")),
      as = "text")
    response_lines <- strsplit(px_content, ";\r\n")[[1]]
    data_structure <- list()

    for (line in response_lines) {
      parts <- strsplit(line, "=")[[1]]
      key <- parts[1]
      value <- parts[2]

      value <- gsub("\"", "", value)

      data_structure[[key]] <- value
    }

    data_structure[["DATA"]] <- NULL

    return(data_structure)
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
