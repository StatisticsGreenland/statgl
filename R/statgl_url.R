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
    table_id, lang = "en",
    api_url = paste0("https://bank.stat.gl:443/api/v1/", lang, "/Greenland/")
  ) {

    if(!lang %in% c("en", "da", "kl")) {
      stop("lang must be one of: 'en', 'da', 'kl'")
    }

    searchtable <- table_id
    substring(searchtable, 3, 3) <- list(en = "E", da = "D", kl = "N")[[lang]]
    search_results <- statgl_search(searchtable, lang = lang, api_url = api_url,
                                    returnclass = "list")

    for(i in search_results) {
      if(toupper(i[["id"]]) == toupper(paste0(table_id, ".PX"))) {
        return(paste0(api_url, i[["path"]], "/", i[["id"]]))
      }
    }

    stop(paste0("Table '", table_id, "' not found in pxweb API"))
  }
