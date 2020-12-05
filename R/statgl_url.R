#' Retrieve URL of px table
#'
#' Searches the statbank for a table of a given ID, and returns the URL if found.
#' Only works on PX tables from Statistics Greenland
#'
#' @param table_id ID of table
#' @param lang API language of query. Defaults to \code{"en"}
#' @param api_url URL of Statbank API. Default \code{glue::glue("https://bank.stat.gl:443/api/v1/{lang}/Greenland")}
#'
#' @return
#' @export
#'
#' @examples
#' statgl_url("BEXST1")
#' statgl_url("BEXST1", lang = "kl")
statgl_url <-
  function(
    table_id, lang = "en",
    api_url = paste0("https://bank.stat.gl:443/api/v1/", lang, "/Greenland/")
  ) {
    search_results <-
      statgl_search(table_id, lang = lang, api_url = api_url)

    for(i in search_results) {
      if(toupper(i[["id"]]) == toupper(paste0(table_id, ".PX"))) {
        return(paste0(api_url, i[["path"]], "/", i[["id"]]))
      }
    }

    stop(paste0("Table '", table_id, "' not found in pxweb API"))
  }
