validate_px <- function(url) {
  api_get <- httr::content(httr::GET(url))
  if(!("title" %in% names(api_get) & "title" %in% names(api_get))) {
    stop("URL not recognized as pxweb API")
  }
}

is_valid_url <- function(string) {
  pattern <- "https?://[^ /$.?#].[^\\s]*"
  grepl(pattern, string)
}

get_language <- function(char) {
  char <- toupper(char)
  lang_mapping <- c("N" = "kl", "D" = "da", "E" = "en", "X" = "en")
  return(lang_mapping[char])
}

get_language_code <- function(char) {
  char <- tolower(char)
  lang_mapping <- c("kl" = "N", "da" = "D", "en" = "E")
  return(lang_mapping[char])
}

generate_subfolders <- function(pattern) {
  num_levels <- nchar(pattern) %/% 2
  returner <- character(num_levels)

  for (i in seq_len(num_levels)) {
    returner[i] <- substr(pattern, 1, i * 2)
  }

  return(paste(returner, collapse = "/"))
}

extract_substring <- function(input_string, pattern) {

  # Use regexec to find the position of the first match
  match_result <- regexec(pattern, input_string)

  # Check if a match was found
  if (length(match_result[[1]]) >= 0) {
    # Extract the captured substring
    captured_text <- regmatches(input_string, match_result)[[1]][2]
    return(captured_text)
  } else {
    return(NULL)
  }
}


utils::globalVariables(c(
  "values", "valueTexts", "code", "text", "elimination", "time",
  # `col_key` is the column tidyr::unite() creates inside statgl_crosstable()
  "col_key",
  # `value` is the default `y` argument in statgl_plot() (NSE)
  "value"
))

# Default base URL for Statistics Greenland's PXWeb statbank API.
# Centralised so it lives in one place; functions that accept an `api_url`
# argument should use `statgl_api_url()` as their default.
#
# Override globally with `options(statgl.api_url = "...")`, or per-call via
# the `api_url` argument.
.statgl_default_api_url <- "https://bank.stat.gl/api/v1/en/Greenland/"

#' Default PXWeb base URL
#'
#' Returns the base URL used when other functions in the package fall back
#' to a default `api_url`. The value is read from
#' `getOption("statgl.api_url")`, so users can override it globally with
#' `options(statgl.api_url = "...")`. If no option is set, the official
#' Statistics Greenland endpoint is used.
#'
#' @return A length-1 character vector with the active API base URL.
#'
#' @examples
#' statgl_api_url()
#'
#' withr::with_options(
#'   list(statgl.api_url = "https://example.test/api/v1/en/Test/"),
#'   statgl_api_url()
#' )
#'
#' @export
statgl_api_url <- function() {
  getOption("statgl.api_url", default = .statgl_default_api_url)
}

# Compatibility helper used in older R: rlang and base R 4.4+ both provide
# `%||%`, but defining it locally avoids depending on either.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Does an api_url point at Statistics Greenland's PXWeb instance?
# Used to decide whether the language-character trick in table IDs applies.
is_greenland_api <- function(api_url) {
  grepl("^https?://bank\\.stat\\.gl.*?/api/v", api_url)
}

# Pull the language segment out of a PXWeb api_url (the bit between
# `/v<digits>/` and the next `/`). Errors out clearly if the URL doesn't
# match the expected `/v\d+/<lang>/...` shape.
infer_lang_from_url <- function(api_url) {
  m <- regmatches(api_url, regexec("/v\\d+/([^/]+)/", api_url))[[1]]
  if (length(m) < 2L) {
    stop(
      "Could not infer 'lang' from api_url: ", api_url,
      call. = FALSE
    )
  }
  m[2]
}




