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
  # Deprecated `url` argument
  if (!is.null(url)) {
    lifecycle::deprecate_stop(
      "0.5.2",
      "statgl_fetch(url)",
      "statgl_fetch(x)"
    )
  }

  # ---- Pipeline: resolve -> expand -> build -> (dry-run | send) ---------
  x <- resolve_url(x, .lang, .api_url)

  vls <- rlang::dots_list(...)

  # Cache meta so we don't fetch it twice if both .eliminate_rest and the
  # 403 chunking path need it.
  meta <- NULL
  if (!.eliminate_rest) {
    meta <- statgl_meta(x, "list")
    vls <- expand_eliminables(vls, meta)
  }

  body <- build_query(vls, .format = "json-stat")

  if (isTRUE(.dry_run)) {
    return(list(url = x, selections = vls, body = body))
  }

  resp <- httr::POST(x, body = body)

  # 403: fall back to chunked retrieval.
  if (httr::status_code(resp) == 403L) {
    message("Query too large, attempting to chunk...")
    return(chunk_large_query(x, vls, .col_code, .val_code, meta = meta))
  }

  if (httr::http_error(resp)) {
    stop(http_error_message(resp, x), call. = FALSE)
  }

  fetch_jsonstat(resp, .val_code, .col_code)
}

# Internal helpers -------------------------------------------------------------

# Resolve `x` to a full PXWeb URL. Table IDs go through statgl_url() (so
# both .lang and .api_url take effect); full URLs are taken as-is, except
# that .lang rewrites the /v<N>/<lang>/ segment.
resolve_url <- function(x, lang, api_url) {
  if (!grepl("^https?://", x)) {
    return(statgl_url(x, lang = lang, api_url = api_url))
  }
  if (!is.null(lang)) {
    return(sub("(/v\\d+/)[^/]+", paste0("\\1", lang), x))
  }
  x
}

# Is a variable from statgl_meta(x, "list")$variables eliminable? The API
# is inconsistent about whether it returns a logical or one of the strings
# "true" / "yes" / "1", so we accept both.
is_eliminable <- function(var) {
  e <- var$elimination
  if (is.null(e))         FALSE
  else if (is.logical(e)) isTRUE(e)
  else if (is.character(e)) tolower(e) %in% c("true", "yes", "1")
  else FALSE
}

# For each eliminable variable not already mentioned by the user, request
# all of its values via px_all("*"). Used when .eliminate_rest = FALSE.
expand_eliminables <- function(vls, meta) {
  for (var in meta$variables) {
    if (is_eliminable(var) && !var$code %in% names(vls)) {
      vls[[var$code]] <- px_all("*")
    }
  }
  vls
}

# Standard error message for a non-403 HTTP failure: status, response
# body, and a hint pointing at statgl_meta().
http_error_message <- function(resp, x) {
  paste0(
    "API request failed with status ", httr::status_code(resp), ".\n",
    "Response body: ", httr::content(resp, as = "text"), "\n",
    "Hint: check column codes with statgl_meta(\"", x, "\")."
  )
}

# Parse a successful PXWeb JSON-stat response into a tidy tibble.
# Both the label-naming and id-naming variants are needed so
# process_dataframe() can pick between text and code on a per-column
# basis (see .col_code / .val_code in statgl_fetch()).
fetch_jsonstat <- function(resp, val_code, col_code) {
  txt <- httr::content(resp, as = "text")
  process_dataframe(
    rjstat::fromJSONstat(txt, naming = "label")[[1]],
    rjstat::fromJSONstat(txt, naming = "id")[[1]],
    val_code,
    col_code
  )
}

# Query builder ----------------------------------------------------------------

build_query <- function(vls, .format = "json-stat") {
  nms <- names(vls)
  out <- vector(mode = "list", length = length(vls))

  for (i in seq_along(vls)) {
    # Allow `var = "*"` as a shorthand for `var = px_all("*")`.
    if (identical(vls[[i]], "*")) {
      vls[[i]] <- px_all("*")
    }

    # px_top()/px_all()/px_agg() attach a .px_filter attribute. Anything
    # without one is treated as a plain "item" filter.
    filter <- attr(vls[[i]], ".px_filter") %||% "item"

    out[[i]] <- list(
      code = jsonlite::unbox(nms[[i]]),
      selection = list(
        filter = jsonlite::unbox(filter),
        values = as.character(vls[[i]])
      )
    )
  }

  jsonlite::toJSON(list(
    query = out,
    response = list(format = jsonlite::unbox(.format))
  ))
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

# Output processing ------------------------------------------------------------

process_dataframe <- function(text_df, code_df, val_code, col_code) {
  # Switch between code and text for values
  if (is.logical(val_code)) {
    rtn <- if (val_code) code_df else text_df
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
    names(rtn) <- if (col_code) names(code_df) else names(text_df)
  } else if (is.character(col_code)) {
    nms <- names(text_df)
    for (i in col_code) {
      idx <- which(names(code_df) == i)
      nms[idx] <- names(code_df)[idx]
    }
    names(rtn) <- nms
  }

  rtn <- tibble::as_tibble(rtn)

  # Force columns whose only values are "F" / "T" to character: readr's
  # type_convert would otherwise read them as logical FALSE / TRUE, which
  # is wrong for PXWeb codes (e.g. F = Female, T = Total).
  ft_cols <- character()
  for (col in names(rtn)) {
    values <- rtn[[col]]
    non_na_values <- values[!is.na(values)]
    if (length(non_na_values) > 0L && all(non_na_values %in% c("F", "T"))) {
      ft_cols <- c(ft_cols, col)
    }
  }

  col_types <- readr::cols(.default = readr::col_guess())
  for (col in ft_cols) {
    col_types$cols[[col]] <- readr::col_character()
  }

  readr::type_convert(rtn, col_types = col_types, na = c("", "NA"))
}

# Chunking ---------------------------------------------------------------------

# Pick the variable to chunk on. Prefer `px_all("*")` selections (the
# user explicitly asked for "everything", so the cardinality is large
# and known via metadata). Otherwise fall back to the user's longest
# explicit selection.
pick_chunk_variable <- function(vls, meta) {
  px_all_vars <- names(vls)[vapply(vls, is_px_all_star, logical(1))]

  if (length(px_all_vars) > 0L) {
    counts <- vapply(px_all_vars, function(name) {
      vm <- Find(function(v) v$code == name, meta$variables)
      length(vm$values)
    }, integer(1))
    return(px_all_vars[which.max(counts)])
  }

  if (length(vls) == 0L) {
    stop(
      "Cannot determine chunking strategy - no suitable variable found.",
      call. = FALSE
    )
  }

  lengths <- vapply(vls, function(v) length(as.character(v)), integer(1))
  names(vls)[which.max(lengths)]
}

# All values to chunk over for the given chunk variable.
chunk_variable_values <- function(chunk_var, vls, meta) {
  v <- vls[[chunk_var]]
  if (is_px_all_star(v)) {
    vm <- Find(function(m) m$code == chunk_var, meta$variables)
    return(vm$values)
  }
  if (is.numeric(v)) v else as.character(v)
}

# Helper predicate: is this selection a `px_all("*")`?
is_px_all_star <- function(v) {
  identical(attr(v, ".px_filter"), "all") && identical(as.character(v), "*")
}

# Try fetching one chunk. Returns the parsed tibble on success, NULL on
# HTTP 403 (signalling "still too big, halve and retry"), or stops with
# a clear error on other HTTP failures (which halving wouldn't fix).
try_chunk <- function(x, vls, chunk_var, chunk_values, val_code, col_code) {
  vls[[chunk_var]] <- chunk_values
  body <- build_query(vls)
  resp <- httr::POST(x, body = body)

  if (!httr::http_error(resp)) {
    return(fetch_jsonstat(resp, val_code, col_code))
  }
  if (httr::status_code(resp) == 403L) {
    return(NULL)
  }
  stop(http_error_message(resp, x), call. = FALSE)
}

# Chunked retrieval for queries that exceed the API's cell limit.
# Probes by trying the first chunk; on success, keeps that result and
# fetches the remaining chunks. On 403, halves chunk_size and retries
# from the top.
chunk_large_query <- function(x, vls, col_code, val_code, meta = NULL) {
  if (is.null(meta)) {
    meta <- statgl_meta(x, "list")
  }

  chunk_var <- pick_chunk_variable(vls, meta)
  all_values <- chunk_variable_values(chunk_var, vls, meta)

  chunk_size <- max(1L, min(10L, length(all_values) %/% 4L))

  while (chunk_size > 0L) {
    chunks <- split(
      all_values,
      ceiling(seq_along(all_values) / chunk_size)
    )
    message(
      "Trying chunk size: ", chunk_size, " (", length(chunks), " chunks)"
    )

    # Probe with the first chunk.
    first <- try_chunk(
      x, vls, chunk_var, chunks[[1L]], val_code, col_code
    )

    if (is.null(first)) {
      # Still too large -> halve and retry from the top.
      chunk_size <- chunk_size %/% 2L
      next
    }

    # First chunk succeeded; keep its result and fetch the remaining
    # chunks at the same size.
    results <- vector("list", length(chunks))
    results[[1L]] <- first

    for (i in seq_along(chunks)[-1L]) {
      message("Fetching chunk ", i, " of ", length(chunks))
      r <- try_chunk(
        x, vls, chunk_var, chunks[[i]], val_code, col_code
      )
      if (is.null(r)) {
        stop(
          "Chunk ", i, " failed at chunk size ", chunk_size,
          " (first chunk had succeeded).",
          call. = FALSE
        )
      }
      results[[i]] <- r
    }

    return(dplyr::bind_rows(results))
  }

  stop(
    "Unable to chunk query small enough to avoid API limits.",
    call. = FALSE
  )
}
