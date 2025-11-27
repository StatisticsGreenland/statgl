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

  if (.as_html) {
    return(as.character(k))
  }
  k
}

#' Create a formatted HTML table for Statgl
#'
#' `statgl_table2()` is a helper for turning a data frame into a
#' Bootstrap-styled HTML table via **kableExtra**, with optional
#' responsive hiding of “secondary” columns on small screens.
#'
#' @param df A data frame.
#' @param ... Additional arguments passed on to [base::format()] when
#'   formatting numeric columns (for example `big.mark`, `scientific`,
#'   etc.). Defaults are suitable for Danish/Greenlandic number
#'   formatting.
#' @param .digits Number of digits used when formatting numeric columns.
#' @param .secondary Tidyselect specification of columns that should be
#'   considered “secondary”. These columns will be hidden on viewports
#'   narrower than 768px (via CSS) when `.as_html = TRUE`.
#' @param .year_col Tidyselect specification of the column(s) containing
#'   years (e.g. `year` or `c(year, quarter)`). These column(s) are
#'   converted to character to avoid formatting like `2,024`. Optional.
#' @param .replace_0s Logical; if `TRUE`, any cell equal to the literal
#'   string `"0"` is replaced by `"[-]{}"` (intended as a special
#'   missing/zero placeholder).
#' @param .as_html Logical; if `FALSE` (default), the function returns
#'   the `knitr_kable` object created by **kableExtra**. If `TRUE`, the
#'   function returns a single character string with inline CSS and the
#'   `<table>` HTML.
#'
#' @return
#' If `.as_html = FALSE`, a `knitr_kable` / `kableExtra` object.
#' If `.as_html = TRUE`, a length-1 character vector containing HTML.
#'
#' @importFrom dplyr mutate across where
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo
#' @importFrom kableExtra kable kable_styling
#' @export
statgl_table2 <- function(
  df,
  ...,
  .digits = 3,
  .secondary = NULL, # tidyselect
  .year_col,
  .replace_0s = FALSE,
  .as_html = FALSE
) {
  # Basic check so error messages are clearer in user code
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  # 1) Alignments: first column left, rest right
  aligns <- paste0(c("l", rep("r", max(ncol(df) - 1L, 0L))), collapse = "")

  # 2) Convert year column(s) to character (avoid 2,024 style formatting)
  if (!missing(.year_col)) {
    df <- dplyr::mutate(
      df,
      dplyr::across({{ .year_col }}, as.character)
    )
  }

  # 3) Optional: replace literal "0" with placeholder
  if (isTRUE(.replace_0s)) {
    df[] <- lapply(df, function(x) {
      x_chr <- trimws(as.character(x))
      x_chr[x_chr == "0"] <- "[-]{}"
      x_chr
    })
  }

  # 4) Format numeric columns (locale style: "." thousands, "," decimal)
  df <- dplyr::mutate(
    df,
    dplyr::across(
      dplyr::where(is.numeric),
      ~ format(
        .x,
        big.mark = ".",
        decimal.mark = ",",
        digits = .digits,
        justify = "right",
        ...
      )
    )
  )

  # 5) Tidyselect: which columns are "secondary"?
  sec_idx <- integer(0)
  if (!is.null(.secondary)) {
    sec_idx <- unname(
      tidyselect::eval_select(rlang::enquo(.secondary), df)
    )
  }

  # 6) Build kable
  k <- kableExtra::kable(
    df,
    format = "html",
    align = aligns,
    escape = FALSE
  )

  k <- kableExtra::kable_styling(
    k,
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

  # If caller wants a kable object, just return it as-is
  if (!isTRUE(.as_html)) {
    return(k)
  }

  # 7) Turn into HTML string
  html <- as.character(k)

  # 8) If no secondary columns defined, we’re done
  if (!length(sec_idx)) {
    return(html)
  }

  # 9) Give the table a unique id
  table_id <- paste0(
    "statgltbl-",
    paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  )

  # add id to the first <table ...> occurrence
  html <- sub(
    "<table",
    paste0("<table id=\"", table_id, "\""),
    html,
    fixed = TRUE
  )

  # 10) Build CSS that hides those columns on small screens
  selectors_td <- paste(
    sprintf("#%s td:nth-child(%d)", table_id, sec_idx),
    collapse = ",\n  "
  )
  selectors_th <- paste(
    sprintf("#%s th:nth-child(%d)", table_id, sec_idx),
    collapse = ",\n  "
  )

  style <- sprintf(
    "<style>
@media (max-width: 768px) {
  %s,
  %s {
    display: none;
  }
}
</style>",
    selectors_td,
    selectors_th
  )

  # 11) Prepend style block so it travels with the table
  html <- paste(style, html, sep = "\n")

  html
}


#' Crosstable helper for Statgl
#'
#' Build a wide crosstable with grouped column headers and formatted
#' numeric values, suitable for HTML display via **kableExtra**.
#'
#' @param df A data frame.
#' @param ... Grouping columns for the column headers (e.g. `køn`,
#'   `hændelsestype`). The first group becomes the top-level header;
#'   remaining groups form sub-headers.
#' @param .value Column used for the cell values (default `value`).
#' @param .row Optional row variable. If `NULL`, the function will try
#'   to auto-detect a row variable as the one remaining column not used
#'   in `...` or `.value`.
#' @param .big.mark Thousands separator used for numeric formatting.
#' @param .decimal.mark Decimal separator used for numeric formatting.
#' @param .caption Optional table caption passed to **kableExtra**.
#' @param .year_col Optional tidyselect specification of year column(s)
#'   that should be converted to character to avoid `2,024` formatting.
#' @param .as_html Logical; if `FALSE` (default) returns a kable object;
#'   if `TRUE`, returns an HTML string.
#'
#' @return
#' If `.as_html = FALSE`, a `knitr_kable` / `kableExtra` object.
#' If `.as_html = TRUE`, a length-1 character vector containing HTML.
#'
#' @importFrom dplyr arrange distinct mutate select across where all_of count
#' @importFrom tidyr unite pivot_wider
#' @importFrom rlang enquos enquo quo_is_null quo_is_missing quo_name sym
#' @importFrom stringr str_to_title
#' @importFrom kableExtra kable kable_styling add_header_above
#' @export
statgl_crosstable <- function(
  df,
  ...,
  .value = value,
  .row = NULL,
  .big.mark = ".",
  .decimal.mark = ",",
  .caption = NULL,
  .year_col = NULL,
  .as_html = FALSE
) {
  # ---- 0) Basic checks & quos --------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  groups <- rlang::enquos(...)
  value <- rlang::enquo(.value)
  row_quo <- rlang::enquo(.row)

  if (length(groups) == 0) {
    stop(
      "You must supply at least one grouping column in `...`.",
      call. = FALSE
    )
  }

  # Convert year column(s) to character if specified
  if (!missing(.year_col)) {
    df <- dplyr::mutate(
      df,
      dplyr::across({{ .year_col }}, as.character)
    )
  }

  # ---- 1) Auto-detect row variable if not supplied -----------------
  if (rlang::quo_is_null(row_quo) || rlang::quo_is_missing(row_quo)) {
    group_names <- vapply(groups, rlang::quo_name, character(1))
    value_name <- rlang::quo_name(value)

    candidates <- setdiff(names(df), c(group_names, value_name))

    if (length(candidates) == 0) {
      stop(
        "Could not auto-detect a row variable: all columns are used as groups or value.\n",
        "Specify `.row = <column>` explicitly.",
        call. = FALSE
      )
    }
    if (length(candidates) > 1) {
      stop(
        "Ambiguous row variable: candidates are ",
        paste(candidates, collapse = ", "),
        ".\nSpecify `.row = <column>` explicitly.",
        call. = FALSE
      )
    }

    row_quo <- rlang::sym(candidates[1])
  }

  # primary group (top header) + rest (sub-headers)
  primary_group <- groups[[1]]
  rest_groups <- if (length(groups) > 1) groups[-1] else list()

  # ---- 2) Wide table: row + one column per distinct combo of groups ----
  # col_key like "Kvinder – Levendefødte"
  df_wide <- df %>%
    dplyr::arrange(!!primary_group, !!!rest_groups) %>%
    tidyr::unite("col_key", !!!groups, sep = " – ", remove = FALSE) %>%
    dplyr::select(!!row_quo, col_key, !!value) %>%
    tidyr::pivot_wider(
      names_from = col_key,
      values_from = !!value,
      names_sort = FALSE # keep order of appearance
    ) %>%
    dplyr::arrange(!!row_quo)

  # ---- 3) Numeric formatting (all numeric columns) -----------------
  num_cols <- names(df_wide)[vapply(df_wide, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    df_wide <- df_wide %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(num_cols),
          ~ format(
            .x,
            big.mark = .big.mark,
            decimal.mark = .decimal.mark
          )
        )
      )
  }

  row_name <- rlang::quo_name(row_quo)
  orig_cols <- names(df_wide)
  wide_cols <- orig_cols[orig_cols != row_name]

  # What we show in the second header row:
  # "1981 – I alt" → "I alt"
  display_labels <- sub("^[^–]+\\s*–\\s*", "", wide_cols)

  # ---- 4) Build header_above from the long data --------------------
  if (length(rest_groups) == 0) {
    # only one grouping var; each level is one column under top header
    group_counts <- df %>%
      dplyr::distinct(!!primary_group) %>%
      dplyr::count(!!primary_group, name = "n")
  } else {
    group_counts <- df %>%
      dplyr::distinct(!!primary_group, !!!rest_groups) %>%
      dplyr::count(!!primary_group, name = "n")
  }

  primary_name <- rlang::quo_name(primary_group)

  header <- c(
    stats::setNames(1, stringr::str_to_title(row_name)),
    stats::setNames(
      group_counts$n,
      as.character(group_counts[[primary_name]])
    )
  )

  # sanity check so kableExtra doesn't explode cryptically
  if (sum(header) != ncol(df_wide)) {
    stop(
      sprintf(
        "Header span (%d) does not match number of columns in table (%d).",
        sum(header),
        ncol(df_wide)
      ),
      call. = FALSE
    )
  }

  # ---- 5) Build kableExtra table -----------------------------------
  kb <- df_wide %>%
    kableExtra::kable(
      format = "html",
      align = c("l", rep("r", ncol(df_wide) - 1L)),
      escape = FALSE,
      caption = .caption,
      col.names = c(" ", display_labels)
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ) %>%
    kableExtra::add_header_above(header = header)

  if (.as_html) {
    return(as.character(kb))
  }

  kb
}
