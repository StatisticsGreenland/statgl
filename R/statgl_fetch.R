#' Retrieve statbank data via URL
#'
#' @description
#' This funtion retrive data from statistics banks via URL and hidden quiestions in a qury
#'
#' The variables is displayed by their text_code by default. This can be changed
#' by setting .col_code = TRUE.
#'
#' The selection of variables should be given using their code values and not text values.
#'
#' These can be found by using the statgl_meta() function prior to using statgl_fetch()
#'
#' @details
#' The variables is displayed by their text_code by default. This can be changed
#' by setting .col_code = TRUE.
#'
#' The selection of variables should be given using their code values and not text values.
#'
#' These can be found by using the statgl_meta() function prior to using statgl_fetch()
#'
#' @param x API url of statbank matrix
#' @param ... Selection queries for variables
#' @param .col_code \code{TRUE}/\code{FALSE}. Display column names as code.
#' @param .val_code \code{TRUE}/\code{FALSE}. Display cell values as code.
#' @param .eliminate_rest \code{TRUE}/\code{FALSE}. If \code{FALSE}, retrive all selections for remaining variables in table (experimental).
#' @param url deprecated
#'
#' @return return a dataframe / tibble with the data from the registry
#' @export
#'
#' @importFrom utils URLencode
#'
#'
#' @examples
#' statgl_fetch("BEXSTA")
#' statgl_fetch("BEXSTA")
#' statgl_fetch("BEXSTA", gender = c("M", "K"), time = 2010:2020)
#' statgl_fetch(statgl_url("BEXSTA"), time = px_top(1), age = px_all("*0"))
statgl_fetch <- function(x, ..., .col_code = FALSE, .val_code = FALSE,
                         .eliminate_rest = TRUE, url = NULL){

  #browser()

  if (!missing(url)) {
    message("`url` as parameter is deprecated. Please use `x`.\n")
  }

  # Looks up Greenland API tables if not URL
  if (!is_valid_url(x)) {
    x <- toupper(x)
    lang <- get_language(substr(x, 3, 3))
    if (is.na(lang)) {
      lang <- "en"
    }
    x <- paste0(substr(x, 1, 2), "X", substr(x, 4, nchar(x)))
    x <- statgl_url(x, lang = lang)
  }

  x <- URLencode(x)

  #validate_px(x)

  # Gather query list
  vls <- rlang::dots_list(...)

  # Check if other variables should be eliminated
  if(!.eliminate_rest) {

    sgl_meta <- statgl_meta(x, "list")[["variables"]]
    el_list <- vector("list", length = length(sgl_meta))

    for(i in seq_along(sgl_meta)) {
      if("elimination" %in% names(sgl_meta[[i]])) {
        el_list[[i]] <- sgl_meta[[i]][["code"]]
      }
    }

    the_rest <- setdiff(unlist(el_list), names(vls))
    rest_lst <- stats::setNames(
      replicate(length(the_rest), expr = px_all(), simplify = FALSE),
      the_rest
    )

    vls <- c(vls, rest_lst)
  }

  # Build query
  body <- build_query(vls)
  # Post to server
  api_response <- httr::POST(x, body = body)
  # Validate return status
  #httr::stop_for_status(api_response)

  # Get content
    api_content <- suppressMessages(
      httr::content(api_response, as = "text")
    )

    # Get data
  text_df <- tryCatch(rjstat::fromJSONstat(api_content, naming = "label")[[1]],
             error = function(e) {
             e
             print("The recieved content:")
             print(api_content)
             print("is not json compatible. This is likely due to a bad query.")
             print("Investigate correct column codes with statgl_meta()")
             print(e)
             stop("Stopped execution due to the error above")}
           )


  code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]


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

  # Ensure input is a tibble
  rtn <- tibble::as_tibble(rtn)

  # Identify columns where all non-missing values are "F" or "T"
  ft_cols <- character()
  for (col in names(rtn)) {
    values <- rtn[[col]]
    non_na_values <- values[!is.na(values)]
    if (length(non_na_values) > 0 && all(non_na_values %in% c("F", "T"))) {
      ft_cols <- c(ft_cols, col)
    }
  }

  # Create col_types object
  col_types <- readr::cols(.default = readr::col_guess())
  for (col in ft_cols) {
    col_types$cols[[col]] <- readr::col_character()
  }

  # Apply type_convert with custom col_types
  rtn <- readr::type_convert(rtn, col_types = col_types, na = c("", "NA"))

  rtn
}

# Query builder ----------------------------------------------------------------

build_query <- function(vls, .format = "json-stat") {
  nms <- names(vls)
  returner <- vector(mode = "list", length = length(vls))

  if (length(vls) != 0) {
    for (i in seq_along(vls)) {
      attr_list <- names(attributes(vls[[i]]))
      is_px_filter <- ifelse(is.null(attr_list), FALSE, attr_list == ".px_filter")

      fff <- if (is_px_filter) {
        attr(vls[[i]], ".px_filter")
      } else {
        "item"
      }

      returner[[i]] <- list(
        code = jsonlite::unbox(nms[[i]]),
        selection = list(
          filter = jsonlite::unbox(fff),
          values = as.character(vls[[i]])
        )
      )
    }
  }

  returner <- list(query = returner, response = list(format = jsonlite::unbox(.format)))
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
#' @return grep like search result
#' @export
#'
#' @examples
#' statgl_fetch(statgl_url("BEXST1"), time = px_top(5))
#' statgl_fetch(statgl_url("BEXST1"), time = px_all("*5"))
#' statgl_fetch(statgl_url("BEXST1"), age = px_agg("5-year.agg", "-4", "5-9", "10-14"))
px_top <- function(top_n = 1) {
  structure(top_n, .px_filter = "Top")
}

#' @rdname px_top
#' @export
px_all <- function(pattern = "*") {
  structure(pattern, .px_filter = "all")
}

#' @rdname px_top
#' @export
px_agg <- function(agg_file, ...) {
  structure(c(...), .px_filter = paste0("agg:", agg_file))
}

