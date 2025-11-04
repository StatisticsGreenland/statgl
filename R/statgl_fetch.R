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
statgl_fetch <- function(
  x,
  ...,
  .col_code = FALSE,
  .val_code = FALSE,
  .eliminate_rest = TRUE,
  .chunk_size = NULL,
  url = NULL
) {
  # Restore the original URL handling logic
  if (!is.null(url)) {
    lifecycle::deprecate_warn("0.2.0", "statgl_fetch(url)", "statgl_fetch(x)")
    x <- url
  }

  # Convert table code to URL if needed
  if (!grepl("^https?://", x)) {
    x <- statgl_url(x)
  }

  vls <- rlang::dots_list(...)

  # Handle .eliminate_rest = FALSE - build query for all variables
  if (!.eliminate_rest) {
    # Get metadata and add px_all("*") for eliminable variables not already specified
    meta <- statgl_meta(x, "list")
    for (var in meta$variables) {
      # Check if variable is eliminable (default selected subset)
      is_eliminable <- if (is.null(var$elimination)) {
        FALSE
      } else if (is.logical(var$elimination)) {
        var$elimination # TRUE means eliminable
      } else if (is.character(var$elimination)) {
        tolower(var$elimination) %in% c("true", "yes", "1")
      } else {
        FALSE
      }

      # For eliminable variables not already specified by user, request all values
      if (is_eliminable && !var$code %in% names(vls)) {
        # Added the second condition
        vls[[var$code]] <- px_all("*")
      }
    }
  }

  # Try the query first - if it fails with 403, chunk it
  body <- build_query(vls, .format = "json-stat")
  api_response <- httr::POST(x, body = body)

  # If successful, proceed normally
  if (!httr::http_error(api_response)) {
    api_content <- httr::content(api_response, as = "text")
    text_df <- rjstat::fromJSONstat(api_content, naming = "label")[[1]]
    code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]
    return(process_dataframe(text_df, code_df, .val_code, .col_code))
  }

  # If 403 error, try chunking
  if (httr::status_code(api_response) == 403) {
    message("Query too large, attempting to chunk...")
    return(chunk_large_query(x, vls, .col_code, .val_code))
  }

  # For other HTTP errors
  if (httr::http_error(api_response)) {
    api_content <- httr::content(api_response, as = "text")
    cat("The received content:\n")
    cat(api_content, "\n")
    cat("is not json compatible. This is likely due to a bad query.\n")
    cat("Investigate correct column codes with statgl_meta()\n")
    stop("API request failed with status ", httr::status_code(api_response))
  }

  # Normal processing fallback
  api_content <- httr::content(api_response, as = "text")
  text_df <- rjstat::fromJSONstat(api_content, naming = "label")[[1]]
  code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]
  process_dataframe(text_df, code_df, .val_code, .col_code)
}

# Query builder ----------------------------------------------------------------

build_query <- function(vls, .format = "json-stat") {
  nms <- names(vls)
  returner <- vector(mode = "list", length = length(vls))

  if (length(vls) != 0) {
    for (i in seq_along(vls)) {
      # Check if the value is "*" and convert to px_all()
      if (identical(vls[[i]], "*")) {
        vls[[i]] <- px_all("*")
      }

      attr_list <- names(attributes(vls[[i]]))
      is_px_filter <- ifelse(
        is.null(attr_list),
        FALSE,
        attr_list == ".px_filter"
      )

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

  returner <- list(
    query = returner,
    response = list(format = jsonlite::unbox(.format))
  )
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

# Helper functions for chunking ------------------------------------------------

get_api_limits <- function(x) {
  base_url <- sub("/api/v1/.*", "", x)
  config_url <- paste0(base_url, "/api/v1/?config")

  response <- httr::GET(config_url)
  if (httr::status_code(response) == 200) {
    config <- httr::content(response, as = "parsed")
    return(list(
      max_calls = config$maxCalls %||% 50000,
      max_cells = config$maxDataCells %||% 1000000
    ))
  }

  # Return defaults if config unavailable
  list(max_calls = 50000, max_cells = 1000000)
}

estimate_query_size <- function(x, vls) {
  meta <- statgl_meta(x, "list")
  total_size <- 1

  for (var in meta$variables) {
    var_code <- var$code

    if (var_code %in% names(vls)) {
      # Get the actual cardinality based on px_ function type
      var_selection <- vls[[var_code]]
      selected_count <- get_selection_cardinality(var_selection, var)
    } else {
      # Variable not specified - use default behavior
      is_eliminable <- if (is.null(var$elimination)) {
        FALSE
      } else if (is.logical(var$elimination)) {
        var$elimination
      } else if (is.character(var$elimination)) {
        tolower(var$elimination) %in% c("true", "yes", "1")
      } else {
        FALSE
      }

      # If eliminable, API returns subset; if not, returns all
      if (is_eliminable) {
        # Estimate default elimination subset (typically small)
        selected_count <- min(length(var$values), 10) # Conservative estimate
      } else {
        selected_count <- length(var$values)
      }
    }

    total_size <- total_size * selected_count
  }

  total_size
}

get_selection_cardinality <- function(selection, var_metadata) {
  # Handle different px_ function types
  px_filter <- attr(selection, ".px_filter")

  if (is.null(px_filter)) {
    # Regular vector selection (numeric range or character vector)
    if (is.numeric(selection)) {
      return(length(selection))
    } else {
      return(length(as.character(selection)))
    }
  }

  switch(
    px_filter,
    "all" = {
      # px_all() - need to count matching values
      pattern <- as.character(selection)[1]
      if (pattern == "*") {
        # All values
        return(length(var_metadata$values))
      } else {
        # Wildcard pattern - count matches
        matching_values <- grep(
          glob2rx(pattern),
          var_metadata$values,
          value = TRUE
        )
        return(length(matching_values))
      }
    },
    "Top" = {
      # px_top() - returns specified number
      top_n <- as.numeric(selection)[1]
      return(min(top_n, length(var_metadata$values)))
    },
    {
      # px_agg() or other filters
      if (startsWith(px_filter, "agg:")) {
        # Aggregation - return number of specified aggregation values
        return(length(as.character(selection)))
      } else {
        # Unknown filter type - conservative estimate
        return(length(as.character(selection)))
      }
    }
  )
}

# Helper function to convert glob patterns to regex
glob2rx <- function(pattern) {
  # Simple glob to regex conversion
  pattern <- gsub("\\.", "\\\\.", pattern)
  pattern <- gsub("\\*", ".*", pattern)
  pattern <- gsub("\\?", ".", pattern)
  paste0("^", pattern, "$")
}

calculate_chunk_size <- function(x, vls, limits) {
  # Find the time variable (usually largest dimension)
  meta <- statgl_meta(x, "list")
  time_var <- find_time_variable(meta$variables)

  if (!is.null(time_var) && !time_var %in% names(vls)) {
    # Chunk by time periods
    time_values <- get_variable_values(meta$variables, time_var)
    max_chunk <- floor(
      limits$max_cells / (estimate_query_size(x, vls) / length(time_values))
    )
    return(max(1, min(max_chunk, 10))) # Reasonable chunk size
  }

  # Default chunking strategy
  return(10)
}

fetch_with_chunking <- function(x, vls, chunk_size, col_code, val_code) {
  meta <- statgl_meta(x, "list")
  time_var <- find_time_variable(meta$variables)

  if (is.null(time_var) || time_var %in% names(vls)) {
    stop("Cannot determine chunking strategy for this query")
  }

  # Get all time values
  time_values <- get_variable_values(meta$variables, time_var)
  time_chunks <- split(
    time_values,
    ceiling(seq_along(time_values) / chunk_size)
  )

  # Fetch each chunk
  results <- purrr::map_dfr(time_chunks, function(chunk) {
    chunk_vls <- vls
    chunk_vls[[time_var]] <- chunk

    # Build and execute query for this chunk
    body <- build_query(chunk_vls)
    api_response <- httr::POST(x, body = body)

    if (httr::http_error(api_response)) {
      stop("Chunk query failed with status ", httr::status_code(api_response))
    }

    api_content <- httr::content(api_response, as = "text")

    # Process chunk data (similar to existing code)
    text_df <- rjstat::fromJSONstat(api_content, naming = "label")[[1]]
    code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]

    # Apply value/column code logic
    process_dataframe(text_df, code_df, val_code, col_code)
  })

  tibble::as_tibble(results)
}

find_time_variable <- function(variables) {
  # Look for common time variable names
  time_patterns <- c("time", "år", "year", "periode", "period")

  for (var in variables) {
    if (
      tolower(var$code) %in%
        time_patterns ||
        any(grepl(paste(time_patterns, collapse = "|"), tolower(var$text)))
    ) {
      return(var$code)
    }
  }

  # Fall back to variable with most values (often time)
  max_values <- max(sapply(variables, function(v) length(v$values)))
  time_var <- variables[sapply(variables, function(v) {
    length(v$values) == max_values
  })][[1]]
  return(time_var$code)
}

fetch_large_dataset <- function(
  x,
  meta,
  chunk_var,
  chunk_size,
  col_code,
  val_code
) {
  # Get all values for the chunking variable
  chunk_values <- NULL
  for (var in meta$variables) {
    if (var$code == chunk_var) {
      chunk_values <- var$values
      break
    }
  }

  if (is.null(chunk_values)) {
    stop("Could not find chunking variable in metadata")
  }

  # Create chunks
  chunks <- split(chunk_values, ceiling(seq_along(chunk_values) / chunk_size))

  # Build base query for all other non-eliminable variables
  base_vls <- list()
  for (var in meta$variables) {
    is_eliminable <- if (is.null(var$elimination)) {
      FALSE
    } else if (is.logical(var$elimination)) {
      var$elimination
    } else if (is.character(var$elimination)) {
      tolower(var$elimination) %in% c("true", "yes", "1")
    } else {
      FALSE
    }

    if (!is_eliminable && var$code != chunk_var) {
      base_vls[[var$code]] <- px_all("*")
    }
  }

  # Fetch each chunk
  results <- list()
  for (i in seq_along(chunks)) {
    message("Fetching chunk ", i, " of ", length(chunks))

    chunk_vls <- base_vls
    chunk_vls[[chunk_var]] <- chunks[[i]]

    # Build and execute query
    body <- build_query(chunk_vls)
    api_response <- httr::POST(x, body = body)

    if (httr::http_error(api_response)) {
      stop("Chunk ", i, " failed with status ", httr::status_code(api_response))
    }

    api_content <- httr::content(api_response, as = "text")
    text_df <- rjstat::fromJSONstat(api_content, naming = "label")[[1]]
    code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]

    results[[i]] <- process_dataframe(text_df, code_df, val_code, col_code)
  }

  # Combine all chunks
  dplyr::bind_rows(results)
}

# Add these helper functions to your file:

get_variable_values <- function(variables, var_code) {
  for (var in variables) {
    if (var$code == var_code) {
      return(var$values)
    }
  }
  return(NULL)
}

process_dataframe <- function(text_df, code_df, val_code, col_code) {
  # Switch between code and text for values
  if (is.logical(val_code)) {
    if (val_code) {
      rtn <- code_df
    } else {
      rtn <- text_df
    }
  } else if (is.character(val_code)) {
    rtn <- text_df
    for (i in val_code) {
      idx <- which(names(code_df) == i)
      rtn[, idx] <- code_df[, idx]
    }
  } else {
    rtn <- text_df
  }

  # Switch between code and text for column names
  if (is.logical(col_code)) {
    if (col_code) {
      names(rtn) <- names(code_df)
    } else {
      names(rtn) <- names(text_df)
    }
  } else if (is.character(col_code)) {
    nms <- names(text_df)
    for (i in col_code) {
      idx <- which(names(code_df) == i)
      nms[idx] <- names(code_df)[idx]
    }
    names(rtn) <- nms
  }

  # Convert to tibble and handle F/T columns
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

chunk_large_query <- function(x, vls, col_code, val_code) {
  # Get metadata to find the best variable to chunk by
  meta <- statgl_meta(x, "list")

  # Find variables with px_all("*") - these are candidates for chunking
  px_all_vars <- names(vls)[sapply(vls, function(v) {
    identical(attr(v, ".px_filter"), "all") && identical(as.character(v), "*")
  })]

  # If we have px_all variables, prefer the one with most values
  if (length(px_all_vars) > 0) {
    chunk_var <- NULL
    max_values <- 0

    for (var_name in px_all_vars) {
      # Find this variable in metadata
      var_meta <- meta$variables[sapply(meta$variables, function(v) {
        v$code == var_name
      })][[1]]
      var_count <- length(var_meta$values)

      if (var_count > max_values) {
        max_values <- var_count
        chunk_var <- var_name
      }
    }

    # Get the actual values from metadata
    var_meta <- meta$variables[sapply(meta$variables, function(v) {
      v$code == chunk_var
    })][[1]]
    all_values <- var_meta$values
  } else {
    # Fall back to original logic for user-specified ranges
    chunk_var <- NULL
    max_length <- 0

    for (var_name in names(vls)) {
      var_values <- vls[[var_name]]
      if (is.numeric(var_values)) {
        var_length <- length(var_values)
      } else {
        var_length <- length(as.character(var_values))
      }

      if (var_length > max_length) {
        max_length <- var_length
        chunk_var <- var_name
      }
    }

    if (is.null(chunk_var)) {
      stop("Cannot determine chunking strategy - no suitable variable found")
    }

    chunk_values <- vls[[chunk_var]]
    if (is.numeric(chunk_values)) {
      all_values <- chunk_values
    } else {
      all_values <- as.character(chunk_values)
    }
  }

  # Start with smaller initial chunk size for very large datasets
  initial_chunk_size <- min(10, length(all_values) %/% 4) # More conservative
  chunk_size <- max(1, initial_chunk_size)

  while (chunk_size > 0) {
    chunks <- split(all_values, ceiling(seq_along(all_values) / chunk_size))

    message("Trying chunk size: ", chunk_size, " (", length(chunks), " chunks)")

    # Try first chunk to see if it works
    test_vls <- vls
    test_vls[[chunk_var]] <- chunks[[1]]

    test_body <- build_query(test_vls)
    test_response <- httr::POST(x, body = test_body)

    if (!httr::http_error(test_response)) {
      # Success! Process all chunks
      results <- list()
      for (i in seq_along(chunks)) {
        message("Fetching chunk ", i, " of ", length(chunks))

        chunk_vls <- vls
        chunk_vls[[chunk_var]] <- chunks[[i]]

        body <- build_query(chunk_vls)
        api_response <- httr::POST(x, body = body)

        if (httr::http_error(api_response)) {
          stop("Chunk ", i, " failed")
        }

        api_content <- httr::content(api_response, as = "text")
        text_df <- rjstat::fromJSONstat(api_content, naming = "label")[[1]]
        code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]

        results[[i]] <- process_dataframe(text_df, code_df, val_code, col_code)
      }

      return(dplyr::bind_rows(results))
    }

    # If still too large, try smaller chunks
    chunk_size <- max(1, chunk_size %/% 2)
  }

  stop("Unable to chunk query small enough to avoid API limits")
}
