statgl_table <- function(
  df,
  year_col,
  replace_0s = FALSE,
  ...,
  digits = 3,
  .as_html = FALSE
) {
  aligns <- paste0(c("l", rep("r", ncol(df) - 1L)), collapse = "")
  if (!missing(year_col)) {
    df <- dplyr::mutate(df, dplyr::across({{ year_col }}, as.character))
  }
  if (replace_0s) {
    df[] <- lapply(df, function(x) {
      x <- trimws(as.character(x))
      x[x == "0"] <- "[-]{}"
      x
    })
  }
  df <- dplyr::mutate_if(
    df,
    is.numeric,
    format,
    big.mark = ".",
    decimal.mark = ",",
    digits = digits,
    justify = "right",
    ...
  )

  k <- df |>
    kableExtra::kable(format = "html", align = aligns, escape = FALSE) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )

  paste0("`", k, "`{=html}")
}
