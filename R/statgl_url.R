#' Retrieve URL of px table
#'
#' Searches the statbank for a table of a given ID, and returns the URL if
#' found. Only works on PX tables from Statistics Greenland.
#'
#' @param table_id ID of table
#' @param lang API language of query. Defaults to \code{"en"}
#' @param api_url URL of Statbank API. Defaults to Statistics Greenland API URL
#'
#' @return
#' @export
#'
#' @examples
#' statgl_url("BEXST1")
#' statgl_url("BEXST1", lang = "kl")
statgl_url <-
  function(
    table_id, lang,
    api_url = "https://bank.stat.gl:443/api/v1/en/Greenland/"
  ) {

    # Should extend the search with .px
    if(!base::endsWith(table_id, ".px")) {
      searchtable <- paste0(table_id, ".px")
    } else {
      searchtable <- table_id
    }

    # There is a language trick in the Greenlandic (and only Greenlandic) API
    if(grepl("^https?://bank\\.stat\\.gl.*?/api/v", api_url)) {

      if(missing(lang) & toupper(substr(table_id, 3, 3)) %in% c("D", "E", "N")) {
        lang <- get_language(substr(table_id, 3, 3))
      } else if(missing(lang)) {
        lang <- extract_substring(api_url, pattern = "/v\\d+/(.*?)/")
      }

      searchtable <- paste0(
        substr(searchtable, 1, 2), "X",
        substr(searchtable, 4, nchar(searchtable))
      )

    }

    # Else if lang is not provided, it should guess from the API link
    else if(missing(lang)) {
      lang <- extract_substring(api_url, pattern = "/v\\d+/(.*?)/")
    }

    api_url <-
      gsub("(?<=/v\\d/)[^/]+", lang, api_url, perl = TRUE)

    search_results <- statgl_search(
      searchtable, lang = lang, api_url = api_url, returnclass = "list"
    )


    for(i in search_results) {
      if(toupper(i[["id"]]) == toupper(searchtable)) {
        return(
          paste0(gsub("/$", "", api_url), i[["path"]], "/", i[["id"]])
        )
      }
    }

    stop(paste0("Table '", table_id, "' not found in pxweb API"))
  }
