#' Create a formatted HTML table for Statgl
#'
#' `statgl_table()` is a helper for turning a data frame into a
#' Bootstrap-styled HTML table via **kableExtra**, with optional
#' responsive hiding of “secondary” columns on small screens.
#'
#' By default it uses Danish/Greenlandic-style number formatting
#' (thousands separator `"."`, decimal separator `","`), but you can
#' override this via `.big.mark` and `.decimal.mark`.
#'
#' @param df A data frame.
#' @param ... Additional arguments passed on to [base::format()] when
#'   formatting numeric columns (for example `scientific`, etc.).
#' @param .digits Number of digits used when formatting numeric columns.
#' @param .big.mark Thousands separator used for numeric formatting.
#'   Defaults to `"."`.
#' @param .decimal.mark Decimal separator used for numeric formatting.
#'   Defaults to `","`.
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
statgl_table <- function(
  df,
  ...,
  .digits = 3,
  .big.mark = ".",
  .decimal.mark = ",",
  .secondary = NULL, # tidyselect
  .year_col,
  .replace_0s = FALSE,
  .as_html = FALSE
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  neutral_ink <- "#7d7d7d"

  # 1) Alignments: first column left, rest right
  aligns <- paste0(c("l", rep("r", max(ncol(df) - 1L, 0L))), collapse = "")

  # 2) Convert year column(s) to character
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
      x_chr[x_chr == "0"] <- "-"
      x_chr
    })
  }

  # 4) Format numeric columns
  df <- dplyr::mutate(
    df,
    dplyr::across(
      dplyr::where(is.numeric),
      ~ format(
        .x,
        big.mark = .big.mark,
        decimal.mark = .decimal.mark,
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
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  )

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

  html <- sub(
    "<table",
    paste0("<table id=\"", table_id, "\""),
    html,
    fixed = TRUE
  )

  # 10) CSS: hide secondary cols on small **portrait** only,
  #     and draw a red indicator border after the last primary column
  selectors_td <- paste(
    sprintf("#%s td:nth-child(%d)", table_id, sec_idx),
    collapse = ",\n  "
  )
  selectors_th <- paste(
    sprintf("#%s th:nth-child(%d)", table_id, sec_idx),
    collapse = ",\n  "
  )

  last_primary <- min(sec_idx) - 1L
  if (last_primary < 1L) {
    last_primary <- 1L
  }

  indicator_css <- sprintf(
    "#%s th:nth-child(%d), #%s td:nth-child(%d) { border-right: 2px solid #e94141; }",
    table_id,
    last_primary,
    table_id,
    last_primary
  )

  style <- sprintf(
    "<style>
/* hide secondary cols only on small portrait screens */
@media (max-width: 768px) and (orientation: portrait) {
  %s,
  %s {
    display: none;
  }
}

/* visual indicator that more columns exist */
%s
</style>",
    selectors_td,
    selectors_th,
    indicator_css
  )

  html <- paste(style, html, sep = "\n")
  html
}


#' Crosstable helper for Statgl
#'
#' Build a wide crosstable with grouped column headers and formatted numeric
#' values, suitable for HTML display via **kableExtra**.
#'
#' The columns in `...` define the column layout: the first becomes the
#' top-level header, and any additional columns form sub-headers. One or more
#' remaining columns are used as row “stub” variables and shown on the left. A
#' row-group variable can be promoted to group headers (using
#' [kableExtra::group_rows()]) so several rows share a common label.
#'
#' @param df A data frame.
#' @param ... Grouping columns for the **column headers** (e.g. `køn`,
#'   `hændelsestype`). The first group becomes the top-level header; remaining
#'   groups form sub-headers.
#' @param .value Column used for the cell values (default `value`).
#' @param .row Tidyselect specification of one or more **row stub** columns.
#'   These are shown on the left of the table. If `NULL`, all columns not used
#'   in `...` or `.value` are used as row variables.
#' @param .row_group Optional tidyselect specification of one or more stub
#'   columns to use for **row grouping**. Currently the first selected column is
#'   used to create group headers via [kableExtra::group_rows()], and the
#'   repeated values in that column are hidden inside the table body.
#' @param .row_label Optional label spanning all stub columns in the top header
#'   row. If `NULL` or `""`, a blank label is used; when blank, the stub column
#'   names are also hidden.
#' @param .big.mark Thousands separator used for numeric formatting.
#' @param .decimal.mark Decimal separator used for numeric formatting.
#' @param .caption Optional table caption passed to **kableExtra**.
#' @param .year_col Optional tidyselect specification of year column(s) that
#'   should be converted to character to avoid formatting like `2,024`.
#' @param .bold_rows Optional specification of rows to render in bold. Can be:
#'   * an integer vector of row positions (slice-style),
#'   * a character vector matching values in the primary stub column, or
#'   * a one-sided formula (e.g. `~ tid == max(tid)`) evaluated on the wide
#'     table; it must return a logical vector.
#' @param .replace_0s Either `FALSE` (no special handling), `TRUE` (replace
#'   literal `"0"` with `"–"`), or a single character string used as a custom
#'   replacement for `"0"`.
#' @param .replace_nas Either `FALSE` (leave `NA` as is), `TRUE` (replace `NA`
#'   with `"."`), or a single character string used as a custom replacement for
#'   `NA`.
#' @param .first_col_width Optional CSS width passed to
#'   [kableExtra::column_spec()] for the first column (e.g. `"8em"`).
#' @param .bottom_rule Logical; if `TRUE` (default), a thicker bottom border is
#'   added to the last data row.
#' @param .as_html Logical; if `FALSE` (default) returns a `kableExtra` table
#'   object; if `TRUE`, returns a single HTML string.
#'
#' @return
#' If `.as_html = FALSE`, a `knitr_kable` / `kableExtra` object.
#'
#' If `.as_html = TRUE`, a length-1 character vector containing HTML.
#'
#' @importFrom dplyr arrange distinct mutate select across all_of count
#' @importFrom tidyr unite pivot_wider
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquos enquo sym f_rhs eval_tidy is_formula quo_name
#' @importFrom kableExtra kable kable_styling add_header_above group_rows
#'   column_spec row_spec
#' @export
statgl_crosstable <- function(
  df,
  ...,
  .value = value,
  .row = NULL, # tidyselect spec; can be 1+ columns
  .row_group = NULL, # tidyselect spec; one or more stub cols to group
  .row_label = NULL, # label spanning all stub cols
  .big.mark = ".",
  .decimal.mark = ",",
  .caption = NULL,
  .year_col = NULL, # tidyselect spec
  .bold_rows = NULL, # numeric / character / formula
  .replace_0s = FALSE, # FALSE / TRUE / "custom"
  .replace_nas = FALSE, # FALSE / TRUE / "custom"
  .first_col_width = NULL,
  .bottom_rule = TRUE,
  .as_html = FALSE
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  groups <- rlang::enquos(...)
  if (length(groups) == 0) {
    stop(
      "You must supply at least one grouping column in `...`.",
      call. = FALSE
    )
  }
  value <- rlang::enquo(.value)

  # ---- 1) Determine row columns (can be 1 or many) -----------------
  if (missing(.row) || is.null(.row)) {
    group_names <- vapply(groups, rlang::quo_name, character(1))
    value_name <- rlang::quo_name(value)
    row_names <- setdiff(names(df), c(group_names, value_name))
  } else {
    row_idx <- tidyselect::eval_select(rlang::enquo(.row), df)
    row_names <- names(row_idx)
  }

  if (!length(row_names)) {
    stop(
      "Could not determine row variables; specify `.row =` explicitly.",
      call. = FALSE
    )
  }

  # ---- 1a) Row groups: ensure they are part of row_names and ordered first
  row_group_names <- character(0)
  if (!is.null(.row_group)) {
    rg_idx <- tidyselect::eval_select(rlang::enquo(.row_group), df)
    row_group_names <- names(rg_idx)
    row_names <- c(row_group_names, setdiff(row_names, row_group_names))
  }

  row_quos <- lapply(row_names, rlang::sym)
  primary_row_name <- row_names[1]

  # ---- 2) Replace zeros and NAs in df (before pivot/formatting) ----
  zero_repl <- NULL
  if (isTRUE(.replace_0s)) {
    zero_repl <- "–"
  } else if (is.character(.replace_0s) && length(.replace_0s) == 1L) {
    zero_repl <- .replace_0s
  }
  if (!is.null(zero_repl)) {
    df[] <- lapply(df, function(x) {
      x_chr <- trimws(as.character(x))
      x_chr[x_chr == "0"] <- zero_repl
      x_chr
    })
  }

  na_repl <- NULL
  if (isTRUE(.replace_nas)) {
    na_repl <- "."
  } else if (is.character(.replace_nas) && length(.replace_nas) == 1L) {
    na_repl <- .replace_nas
  }
  if (!is.null(na_repl)) {
    df[] <- lapply(df, function(x) {
      x_chr <- as.character(x)
      x_chr[is.na(x)] <- na_repl
      x_chr
    })
  }

  # ---- 3) Convert year column(s) to character if specified ----------
  if (!is.null(.year_col)) {
    df <- dplyr::mutate(
      df,
      dplyr::across({{ .year_col }}, as.character)
    )
  }

  # ---- 4) Build wide table -----------------------------------------
  primary_group <- groups[[1]]
  rest_groups <- if (length(groups) > 1) groups[-1] else list()

  df_wide <- df %>%
    dplyr::arrange(!!!row_quos, !!primary_group, !!!rest_groups) %>%
    tidyr::unite("col_key", !!!groups, sep = " – ", remove = FALSE) %>%
    dplyr::select(!!!row_quos, col_key, !!value) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(row_names),
      names_from = col_key,
      values_from = !!value,
      names_sort = FALSE
    ) %>%
    dplyr::arrange(!!!row_quos)

  # ---- 5) Numeric formatting ---------------------------------------
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

  # keep an unmodified copy for logic (bold_rows, grouping)
  df_logic <- df_wide

  # ---- 6) Header mapping -------------------------------------------
  orig_cols <- names(df_wide)
  wide_cols <- setdiff(orig_cols, row_names)

  display_labels <- sub("^[^–]+\\s*–\\s*", "", wide_cols)

  if (length(rest_groups) == 0) {
    group_counts <- df %>%
      dplyr::distinct(!!primary_group) %>%
      dplyr::count(!!primary_group, name = "n")
  } else {
    group_counts <- df %>%
      dplyr::distinct(!!primary_group, !!!rest_groups) %>%
      dplyr::count(!!primary_group, name = "n")
  }

  primary_name <- rlang::quo_name(primary_group)

  stub_span <- length(row_names)
  row_label_header <- if (is.null(.row_label) || .row_label == "") {
    " "
  } else {
    .row_label
  }

  header <- c(
    stats::setNames(stub_span, row_label_header),
    stats::setNames(
      group_counts$n,
      as.character(group_counts[[primary_name]])
    )
  )

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

  # ---- 6a) Precompute group_rows info & blank group column in display ----
  grp_info <- NULL
  if (length(row_group_names)) {
    grp_col <- row_group_names[1L]
    grp_vals <- df_logic[[grp_col]]

    r <- rle(as.character(grp_vals))
    starts <- cumsum(c(1L, head(r$lengths, -1L)))
    ends <- cumsum(r$lengths)
    labels <- r$values

    grp_info <- list(
      col_name = grp_col,
      starts = starts,
      ends = ends,
      labels = labels
    )

    # Blank the group column in the *display* table so the group
    # name only appears in the group_rows header.
    df_wide[[grp_col]] <- ""
  }

  # ---- 7) Build kable ----------------------------------------------
  # ---- Build stub column names based on .row_label -------------------
  # If .row_label is blank (" "), hide the stub column headers too
  if (is.null(.row_label) || .row_label == "") {
    stub_colnames <- rep("", length(row_names))
  } else {
    stub_colnames <- row_names
  }

  kb <- df_wide %>%
    kableExtra::kable(
      format = "html",
      align = c(rep("l", stub_span), rep("r", ncol(df_wide) - stub_span)),
      escape = FALSE,
      caption = .caption,
      col.names = c(stub_colnames, display_labels)
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE
    ) %>%
    kableExtra::add_header_above(header = header)
  # ---- 7b) Apply group_rows headers -------------------------------
  if (!is.null(grp_info)) {
    for (i in seq_along(grp_info$labels)) {
      kb <- kableExtra::group_rows(
        kb,
        group_label = grp_info$labels[i],
        start_row = grp_info$starts[i],
        end_row = grp_info$ends[i]
      )
    }
  }

  # first column width, if requested
  if (!is.null(.first_col_width)) {
    kb <- kableExtra::column_spec(kb, 1, width = .first_col_width)
  }

  # ---- 8) Bold rows (using df_logic, not the blanked display copy) --
  bold_idx <- integer(0)

  if (!is.null(.bold_rows)) {
    if (is.numeric(.bold_rows)) {
      bold_idx <- .bold_rows
    } else if (is.character(.bold_rows)) {
      bold_idx <- which(df_logic[[primary_row_name]] %in% .bold_rows)
    } else if (rlang::is_formula(.bold_rows)) {
      expr <- rlang::f_rhs(.bold_rows)
      mask <- rlang::eval_tidy(expr, data = df_logic)
      if (!is.logical(mask)) {
        stop(
          "`.bold_rows` formula must evaluate to a logical vector.",
          call. = FALSE
        )
      }
      bold_idx <- which(mask %in% TRUE)
    }
  }

  if (length(bold_idx)) {
    kb <- kableExtra::row_spec(kb, bold_idx, bold = TRUE)
  }

  # ---- 9) Bottom rule ----------------------------------------------
  if (isTRUE(.bottom_rule)) {
    kb <- kableExtra::row_spec(
      kb,
      nrow(df_wide),
      extra_css = "border-bottom: 2px solid #004459;"
    )
  }

  if (.as_html) {
    return(as.character(kb))
  }

  kb
}
