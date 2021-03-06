#' Retrieve statbank data via URL
#'
#' @param url API url of statbank matrix
#' @param ... Selection queries for variables
#' @param .col_code \code{TRUE}/\code{FALSE}. Display column names as code.
#' @param .val_code \code{TRUE}/\code{FALSE}. Display cell values as code.
#' @param .eliminate_rest \code{TRUE}/\code{FALSE}. If \code{FALSE}, retrive all selections for remaining variables in table (experimental).
#'
#' @return
#' @export
#'
#' @importFrom utils URLencode
#'
#' @examples
#' statgl_fetch(statgl_url("BEXST1"))
#' statgl_fetch(statgl_url("BEXST1"), gender = c("M", "K"), time = 2010:2020)
#' statgl_fetch(statgl_url("BEXST1"), time = px_top(1), age = px_all("*0"))
statgl_fetch <- function(url, ..., .col_code = FALSE, .val_code = FALSE,
                         .eliminate_rest = TRUE){

  url < URLencode(url)

  recognize_px(url)

  # Gather query list
  vls <- list(...)

  # Check if other variables should be eliminated
  if(!.eliminate_rest) {

    sgl_meta <- statgl_meta(url)[["variables"]]
    el_list <- vector("list", length = length(sgl_meta))

    for(i in seq_along(sgl_meta)) {
      if("elimination" %in% names(sgl_meta[[i]])) {
        el_list[[i]] <- sgl_meta[[i]][["code"]]
      }
    }

    the_rest <- setdiff(unlist(el_list), names(vls))
    rest_lst <- stats::setNames(
      replicate(length(the_rest), expr = px_all(), simplify = FALSE),
      the_rest)

    vls <- c(vls, rest_lst)
  }

  # Build query
  body <- build_query(vls)

  # Post to server
  api_post <- suppressMessages(
    httr::content(httr::POST(url, body = body), as = "text")
  )

  # Get data
  text_df <- rjstat::fromJSONstat(api_post, naming = "label")[[1]]
  code_df <- rjstat::fromJSONstat(api_post, naming = "id")[[1]]

  # Switch between code and text
  if(is.logical(.val_code)) {
    if(.val_code) {
      rtn <- code_df
    } else {
      rtn <- text_df
    }
  } else if(is.character(.val_code)){
    rtn <- text_df
    for(i in .val_code) {
      idx <- which(names(code_df) == i)
      rtn[,idx] <- code_df[,idx]
    }
  }

  if(is.logical(.col_code)){
    if(.col_code) {
      names(rtn) <- names(code_df)
    } else {
      names(rtn) <- names(text_df)
    }
  } else if(is.character(.col_code)){
    nms <- names(text_df)
    for(i in .col_code) {
      idx <- which(names(code_df) == i)
      nms[idx] <- names(code_df)[idx]
    }
    names(rtn) <- nms
  }


  # Return
  rtn
}

# Query builder ----------------------------------------------------------------

build_query <- function(vls, .format = "json-stat") {

  nms <- names(vls)
  returner <- vector(mode = "list", length = length(vls))

  if (length(vls) != 0){
    for(i in seq_along(vls)) {

      attr_list <- names(attributes(vls[[i]]))

      if(is.null(attr_list)) {
        is_px_filter <- FALSE
      } else if(attr_list == ".px_filter") {
        is_px_filter <- TRUE
      } else {
        is_px_filter <- FALSE
      }

      if(is_px_filter) {
        fff <- attr(vls[[i]], ".px_filter")
      } else {
        fff <- "item"
      }

      returner[[i]] <- list(
        code = jsonlite::unbox(nms[[i]]),
        selection = list(filter = jsonlite::unbox(fff),
                         values = as.character(vls[[i]])
        )
      )
    }
  }

  returner <-
    list(query = returner, response = list(format = jsonlite::unbox(.format)))

  jsonlite::toJSON(returner)

}

# Fetch helpers ----------------------------------------------------------------

#' Fetch helpers
#'
#' Helper functions to make custom filters in pxweb API queries:
#'
#' * [px_top()]: API returns the the top n values provided
#' * [px_all()]: API returns all values, matching a wildcard pattern
#' * [px_agg()]; Instructs API to use aggregations, and which values to select.
#'
#' @param top_n Numeric. Requests the first n values of a variable, or the last
#'   n values if variable is time.
#' @param pattern Character. Requests variables matching a wildcard string. E.g.
#'   \code{"\*0"} returns all variables ending in 0, \code{"*"} returns all
#'   values
#' @param agg_file Aggregation file used to aggregate values in pxweb API
#' @param ... Values selected from aggregation
#'
#' @return
#' @export
#'
#' @examples
#' statgl_fetch(statgl_url("BEXST1"), time = px_top(5))
#' statgl_fetch(statgl_url("BEXST1"), time = px_all("*5))
#' statgl_fetch(statgl_url("BEXST1"), age = px_agg("5-year.agg", "-4", "5-9", "10-14"))
px_top <- function(top_n = 1) {
  structure(top_n, .px_filter = "Top")
}

#' @rdname px_top
#' @export
px_all <- function(pattern = "*") {
  structure(pattern, .px_filter = "All")
}

#' @rdname px_top
#' @export
px_agg <- function(agg_file, ...) {
  structure(c(...), .px_filter = paste0("agg:", agg_file))
}

