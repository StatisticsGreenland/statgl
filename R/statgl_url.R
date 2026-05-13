#' Retrieve URL of px table
#'
#' Searches the statbank for a table by ID and returns its full PXWeb API
#' URL.
#'
#' @details
#' For Statistics Greenland's API, the third character of `table_id`
#' encodes the language (`N` = Greenlandic / `kl`, `D` = Danish / `da`,
#' `E` = English / `en`). When `lang` is not supplied, [statgl_url()]
#' uses this character to infer the API language. The character itself
#' is rewritten to `X` for the search request, since the API stores
#' tables under that canonical form.
#'
#' For other PXWeb APIs the language is inferred from `api_url` instead,
#' and `table_id` is used as-is.
#'
#' `table_id` is case-insensitive; a `.px` suffix is appended if missing.
#'
#' @param table_id Table ID (case-insensitive). A `.px` suffix is added
#'   automatically if missing.
#' @param lang API language. If `NULL` (default), inferred from
#'   `table_id` for the Statistics Greenland API or from `api_url`
#'   otherwise. For the Statistics Greenland API must be one of `"en"`,
#'   `"kl"`, or `"da"`.
#' @param api_url Base URL of the PXWeb API. Defaults to
#'   [statgl_api_url()].
#'
#' @return A character string with the full PXWeb API URL for the table.
#' @export
#'
#' @examples
#' \donttest{
#' statgl_url("BEXST1")
#' statgl_url("bexst1")            # same as above; case-insensitive
#' statgl_url("BEXST1", lang = "kl")
#' }
statgl_url <- function(
  table_id,
  lang = NULL,
  api_url = statgl_api_url()
) {
  # ---- Input validation --------------------------------------------------
  if (!is.character(table_id) ||
      length(table_id) != 1L ||
      is.na(table_id) ||
      !nzchar(table_id)) {
    stop("'table_id' must be a non-empty character string.", call. = FALSE)
  }
  if (!is.null(lang) &&
      (!is.character(lang) || length(lang) != 1L || is.na(lang))) {
    stop("'lang' must be NULL or a single character string.", call. = FALSE)
  }

  # ---- Normalise: uppercase + .PX suffix --------------------------------
  table_id <- toupper(table_id)
  if (!endsWith(table_id, ".PX")) {
    table_id <- paste0(table_id, ".PX")
  }

  greenland <- is_greenland_api(api_url)

  # ---- Infer lang if the caller didn't supply one ------------------------
  if (is.null(lang)) {
    lang_char <- substr(table_id, 3L, 3L)
    lang <- if (greenland && lang_char %in% c("D", "E", "N")) {
      unname(get_language(lang_char))
    } else {
      infer_lang_from_url(api_url)
    }
  }

  # ---- Build the search ID. Greenland API canonicalises char 3 to "X". --
  searchtable <- if (greenland) {
    paste0(
      substr(table_id, 1L, 2L),
      "X",
      substr(table_id, 4L, nchar(table_id))
    )
  } else {
    table_id
  }

  # ---- Swap the language segment of api_url ------------------------------
  # Capture-and-replace instead of a lookbehind, since PCRE lookbehinds
  # must be fixed-length and `\\d+` would break that constraint.
  api_url <- sub("(/v\\d+/)[^/]+", paste0("\\1", lang), api_url)

  # ---- Look up the table -------------------------------------------------
  search_results <- statgl_search(
    searchtable,
    lang = lang,
    api_url = api_url,
    returnclass = "list"
  )

  match <- Find(
    function(i) toupper(i[["id"]]) == searchtable,
    search_results
  )

  if (is.null(match)) {
    stop(
      "Table '", table_id, "' not found at ", api_url,
      call. = FALSE
    )
  }

  paste0(
    sub("/$", "", api_url),
    match[["path"]],
    "/",
    match[["id"]]
  )
}
