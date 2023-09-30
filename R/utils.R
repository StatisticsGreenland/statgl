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


utils::globalVariables(c("values", "valueTexts"))




