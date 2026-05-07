#' Retrieve statbank data
#'
#' @description
#' Retrieves data from a PXWeb statbank by table ID or URL, with selection
#' filters passed through `...`.
#'
#' Variable values and column names are displayed by their text labels
#' by default. The `.val_code` and `.col_code` arguments switch to the
#' underlying codes - either everywhere (`TRUE`) or on a per-column
#' basis (a character vector of column names).
#'
#' Selections in `...` should be supplied using the variable's *code*
#' values (not text labels). Use [statgl_meta()] to inspect available
#' codes before calling [statgl_fetch()].
#'
#' @details
#' Eliminable variables not specified in `...` are collapsed to the
#' API's default selection. Pass `.eliminate_rest = FALSE` to retrieve
#' all values for unspecified eliminable variables.
#'
#' If the API rejects a query as too large (HTTP 403),
#' [statgl_fetch()] falls back to chunked retrieval automatically.
#'
#' Use `.dry_run = TRUE` to inspect the resolved URL and JSON body
#' [statgl_fetch()] would send, without actually issuing the request.
#'
#' @param x Table ID (e.g. `"BEXSTA"`) or full PXWeb API URL. Table IDs
#'   are passed through [statgl_url()] (so `.lang` and `.api_url` apply);
#'   full URLs are taken as-is, except that `.lang` will rewrite the
#'   `/v<N>/<lang>/` segment in place when supplied.
#' @param ... Selection queries for variables, named by variable code
#'   (e.g. `gender = c("M", "K")`, `time = 2010:2020`). Helper
#'   constructors [px_top()], [px_all()], and [px_agg()] are available
#'   for non-trivial filters.
#' @param .col_code Either a logical, or a character vector of variable
#'   codes. If `TRUE`, all column names are shown as their underlying
#'   codes; if `FALSE` (default), all column names are the
#'   human-readable text. If a character vector, only those columns are
#'   shown as codes; the rest keep their text labels.
#' @param .val_code Same as `.col_code` but applied to cell *values*
#'   instead of column names.
#' @param .eliminate_rest \code{TRUE}/\code{FALSE}. If \code{FALSE},
#'   retrieve all selections for remaining variables in the table
#'   (experimental).
#' @param .lang Optional API language override (one of `"en"`, `"kl"`,
#'   `"da"` for the Statistics Greenland API). When supplied, takes
#'   precedence over the language implied by `x`. If `x` is a URL, the
#'   `/v<N>/<lang>/` segment is rewritten in place; if `x` is a table
#'   ID, the value is forwarded to [statgl_url()].
#' @param .api_url Base URL of the PXWeb API. Used only when `x` is a
#'   table ID (full URLs in `x` are taken at face value). Defaults to
#'   [statgl_api_url()].
#' @param .dry_run Logical. If `TRUE`, do not send the request; instead
#'   return a list with the resolved `url`, the parsed `selections`
#'   list, and the JSON `body` that would be POSTed. Useful for
#'   debugging selection issues without burning an API call.
#' @param url `r lifecycle::badge("deprecated")` Use `x` instead.
#'
#' @return Normally a [tibble][tibble::tibble-package] with one column
#'   per variable plus a `value` column. When `.dry_run = TRUE`, a
#'   length-3 list (`url`, `selections`, `body`) describing the request
#'   that would have been sent.
#' @export
#'
#' @importFrom utils URLencode
#'
#'
#' @examples
#' \donttest{
#' statgl_fetch("BEXSTA")
#' statgl_fetch("BEXSTA", gender = c("M", "K"), time = 2010:2020)
#' statgl_fetch("BEXSTA", .lang = "da")
#' statgl_fetch(statgl_url("BEXSTA"), time = px_top(1), age = px_all("*0"))
#'
#' # Inspect the query without sending it.
#' statgl_fetch(
#'   "BEXSTA",
#'   gender = c("M", "K"),
#'   time = 2010:2020,
#'   .dry_run = TRUE
#' )
#' }
statgl_fetch <- function(
  x,
  ...,
  .col_code = FALSE,
  .val_code = FALSE,
  .eliminate_rest = TRUE,
  .lang = NULL,
  .api_url = statgl_api_url(),
  .dry_run = FALSE,
  url = NULL
) {
  # Deprecated `url` argument: error rather than silently substitute.
  if (!is.null(url)) {
    lifecycle::deprecate_stop(
      "0.5.2",
      "statgl_fetch(url)",
      "statgl_fetch(x)"
    )
  }

  # Resolve `x` to a full URL.
  if (!grepl("^https?://", x)) {
    # Table ID -> let statgl_url() handle .lang and .api_url.
    x <- statgl_url(x, lang = .lang, api_url = .api_url)
  } else if (!is.null(.lang)) {
    # Full URL with explicit .lang: rewrite the language segment.
    x <- sub("(/v\\d+/)[^/]+", paste0("\\1", .lang), x)
  }

  vls <- rlang::dots_list(...)

  # Handle .eliminate_rest = FALSE - build query for all variables
  if (!.eliminate_rest) {
    meta <- statgl_meta(x, "list")
    for (var in meta$variables) {
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
        vls[[var$code]] <- px_all("*")
      }
    }
  }

  # Build the query body.
  body <- build_query(vls, .format = "json-stat")

  # Dry run: return the resolved request without sending it.
  if (isTRUE(.dry_run)) {
    return(list(
      url = x,
      selections = vls,
      body = body
    ))
  }

  # Send the request.
  api_response <- httr::POST(x, body = body)

  # Success path.
  if (!httr::http_error(api_response)) {
    api_content <- httr::content(api_response, as = "text")
    text_df <- rjstat::fromJSONstat(api_content, naming = "label")[[1]]
    code_df <- rjstat::fromJSONstat(api_content, naming = "id")[[1]]
    return(process_dataframe(text_df, code_df, .val_code, .col_code))
  }

  # 403: try chunking.
  if (httr::status_code(api_response) == 403) {
    message("Query too large, attempting to chunk...")
    return(chunk_large_query(x, vls, .col_code, .val_code))
  }

  # Other HTTP errors: surface status, body, and a hint in one stop().
  api_content <- httr::content(api_response, as = "text")
  stop(
    "API request failed with status ",
    httr::status_code(api_response), ".\n",
    "Response body: ", api_content, "\n",
    "Hint: check column codes with statgl_meta(\"", x, "\").",
    call. = FALSE
  )
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
#' \donttest{
#' statgl_fetch(statgl_url("BEXST1"), time = px_top(5))
#' statgl_fetch(statgl_url("BEXST1"), time = px_all("*5"))
#' statgl_fetch(statgl_url("BEXST1"), age = px_agg("5-year.agg", "-4", "5-9", "10-14"))
#' }
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
  time_patterns <- c("time", "\u00e5r", "year", "periode", "period")

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
