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

get_language_code <- function(char) {
  lang_mapping <- c("N" = "kl", "D" = "da")
  return(lang_mapping[char])
}
