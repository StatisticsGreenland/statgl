#' Mark a string for translation
#'
#' `i18n()` returns its argument unchanged. It exists so that string literals
#' inside R code chunks (group labels in `case_when()`, series names, axis
#' titles) can be found by a site's translation tooling and rewritten in
#' generated language twins, without the page itself knowing which language
#' it is rendered in.
#'
#' In the source page `i18n("Folkeskole")` is `"Folkeskole"`. The twin
#' generator (statwww: `R/generate_twins.R`) collects the literal into the
#' page's translation file and emits `i18n("Meeqqat atuarfiat")` in the
#' Greenlandic twin. Only a single string literal directly inside the call is
#' recognised; `i18n(x)` with a variable is a plain identity.
#'
#' Use it for labels that are *displayed*. Do not use it for values that are
#' *matched* against Statbank labels (`filter(sex == i18n("Kvinder"))`): a
#' translation is not guaranteed to equal the Statbank's own label. Match on
#' codes instead (`.val_code`).
#'
#' @param x A character vector, normally one string literal.
#' @return `x`, unchanged.
#' @export
#' @examples
#' i18n("Folkeskole")
#' dplyr::case_when(1:3 == 2 ~ i18n("Ja"), TRUE ~ i18n("Nej"))
i18n <- function(x) x
