#' Statgl Table Helper
#'
#' Build a clean, styled HTML table suitable for Statistics Greenland pages.
#' The function formats numeric columns, applies optional mobile-responsive
#' column hiding, inserts group row banners (similar to
#' [statgl_crosstable()]), and offers additional layout helpers such as
#' bottom rules and first-column width control.
#'
#' @param df A data frame to render as a table.
#' @param ... Passed to [format()] for numeric formatting.
#' @param .digits Number of digits used by [format()] for numeric columns.
#' @param .big.mark Thousands separator.
#' @param .decimal.mark Decimal separator.
#' @param .hide_mobile Optional tidyselect expression identifying columns
#'   that should be hidden on small screens (mobile). Implemented via a CSS
#'   media query (`max-width: 768px`) emitted inline alongside the table; the
#'   columns are still present in the HTML, just visually hidden on narrow
#'   viewports.
#' @param .secondary `r lifecycle::badge("deprecated")` Use `.hide_mobile`
#'   instead. Same shape and semantics.
#' @param .row_group Optional column (bare or quoted) used to create
#'   grouped row banners. The column is removed from the printed table.
#' @param .row_label Optional label for the first column (similar to
#'   `.row_label` in [statgl_crosstable()]).
#' @param .year_col Optional column that should be converted to character
#'   before formatting (e.g., time variables).
#' @param .replace_0s `FALSE` (default; no replacement), `TRUE` (replace
#'   `0` with an en-dash), or a single character string used as a custom
#'   replacement (e.g. `"[-]{}"` for the Statistics Greenland Quarto
#'   shortcode).
#' @param .replace_nas `NULL` (default; no replacement) or a single
#'   character string used as the replacement for `NA` (e.g. `".."`).
#' @param .first_col_width Optional CSS width (e.g. `"12em"`) applied to the
#'   first column via [kableExtra::column_spec()].
#' @param .bottom_rule Logical; if `TRUE`, draws a thick bottom border on the
#'   last data row.
#' @param .as_html Logical; if `FALSE` (default), returns an
#'   `htmlwidget` that renders correctly in every context — the
#'   RStudio Viewer when called interactively, knitr / Quarto chunks,
#'   and Quarto shortcode parameters — matching the behavior of
#'   [statgl_plot()]. If `TRUE`, returns a plain length-1 character
#'   vector instead; kept as an escape hatch for environments that
#'   can't handle widgets.
#'
#' @return An `htmlwidget` object by default (wrapping the rendered
#'   table HTML), or a length-1 character vector when
#'   `.as_html = TRUE`. In both cases the HTML includes a `<style>`
#'   block when `.hide_mobile` is in play.
#'
#' @examples
#' \dontrun{
#' df |>
#'   statgl_table(
#'     .row_group       = region,
#'     .row_label       = "",
#'     .replace_nas     = "..",
#'     .first_col_width = "12em",
#'     .bottom_rule     = TRUE
#'   )
#' }
#'
#' @export
statgl_table <- function(
  df,
  ...,
  .digits = 3,
  .big.mark = ".",
  .decimal.mark = ",",
  .hide_mobile = NULL,
  .secondary = NULL, # deprecated: use .hide_mobile
  .row_group = NULL,
  .row_label = NULL,
  .year_col,
  .replace_0s = FALSE,
  .replace_nas = NULL,
  .first_col_width = NULL,
  .bottom_rule = TRUE,
  .as_html = FALSE
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  # --- year column as character --------------------------------------------
  if (!missing(.year_col)) {
    df <- dplyr::mutate(df, dplyr::across({{ .year_col }}, as.character))
  }

  # --- capture and remove row-group column (like statgl_crosstable) --------
  row_group_vec <- NULL

  if (!missing(.row_group) && !rlang::quo_is_null(rlang::enquo(.row_group))) {
    if (is.character(.row_group)) {
      if (length(.row_group) != 1L || !.row_group %in% names(df)) {
        stop(
          "`.row_group` must be a single existing column name.",
          call. = FALSE
        )
      }
      row_group_vec <- df[[.row_group]]
      df <- df[, setdiff(names(df), .row_group), drop = FALSE]
    } else {
      rg_idx <- tidyselect::eval_select(rlang::enquo(.row_group), df)
      if (length(rg_idx) != 1L) {
        stop("`.row_group` must select exactly one column.", call. = FALSE)
      }
      row_group_vec <- df[[rg_idx]]
      df <- df[, -rg_idx, drop = FALSE]
    }
  }

  # --- replace 0s -----------------------------------------------------------
  zero_repl <- resolve_replace_0s(.replace_0s)
  if (!is.null(zero_repl)) {
    df[] <- lapply(df, function(x) {
      x_chr <- trimws(as.character(x))
      x_chr[x_chr == "0"] <- zero_repl
      x_chr
    })
  }

  # --- format numeric columns ----------------------------------------------
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

  # --- replace NAs in table + group labels ---------------------------------
  na_repl <- resolve_replace_nas(.replace_nas)
  if (!is.null(na_repl)) {
    df[] <- lapply(df, function(x) {
      x_chr <- as.character(x)
      x_chr[is.na(x)] <- na_repl
      x_chr
    })
    if (!is.null(row_group_vec)) {
      rg_chr <- as.character(row_group_vec)
      rg_chr[is.na(row_group_vec)] <- na_repl
      row_group_vec <- rg_chr
    }
  }

  # --- first column label (like statgl_crosstable) -------------------------
  if (!is.null(.row_label) && ncol(df) >= 1L) {
    colnames(df)[1] <- .row_label
  }

  # --- alignment: first col left, rest right -------------------------------
  aligns <- paste0(c("l", rep("r", max(ncol(df) - 1L, 0L))), collapse = "")

  # --- mobile-hidden columns (on the printed df) ---------------------------
  hide_q <- resolve_hide_mobile_q(
    rlang::enquo(.hide_mobile),
    rlang::enquo(.secondary)
  )
  hide_idx <- integer(0)
  if (!rlang::quo_is_null(hide_q)) {
    hide_idx <- unname(tidyselect::eval_select(hide_q, df))
  }

  # --- build base kable (after dropping group column) ----------------------
  k <- kableExtra::kable(
    df,
    format = "html",
    align = aligns,
    escape = FALSE
  )

  # --- group rows: exact same pattern as statgl_crosstable -----------------
  if (!is.null(row_group_vec)) {
    grp_chr <- as.character(row_group_vec)
    grp_rle <- rle(grp_chr)

    index <- grp_rle$lengths
    names(index) <- grp_rle$values

    # drop empty / NA labels
    ok <- !is.na(names(index)) & trimws(names(index)) != ""
    index <- index[ok]

    if (length(index)) {
      k <- kableExtra::group_rows(k, index = index)
    }
  }

  # --- styling --------------------------------------------------------------
  k <- kableExtra::kable_styling(
    k,
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

  if (!is.null(.first_col_width)) {
    k <- kableExtra::column_spec(k, 1, width = .first_col_width)
  }

  if (isTRUE(.bottom_rule) && nrow(df) > 0) {
    k <- kableExtra::row_spec(
      k,
      nrow(df),
      extra_css = "border-bottom: 2px solid #000;"
    )
  }

  # --- Escape hatch: plain character HTML (legacy shortcode path) --------
  if (isTRUE(.as_html)) {
    return(hide_mobile_css(as.character(k), hide_idx))
  }

  # --- Default: wrap the rendered table in an htmlwidget -----------------
  # The widget framework gives us the same Quarto / knitr / RStudio
  # integration that statgl_plot() gets from highcharter: a single
  # return value that opens in the Viewer, renders in chunks, and
  # interpolates cleanly into Quarto shortcode parameters.
  k_with_css <- attach_hide_mobile_css(k, hide_idx)
  statgl_table_widget(as.character(k_with_css))
}

#' Crosstable helper for Statgl
#'
#' Build a wide crosstable with grouped column headers and formatted numeric
#' values, suitable for HTML display via **kableExtra**.
#'
#' The columns in `...` define the column layout: the first becomes the
#' top-level header, and any additional columns form sub-headers. One or more
#' remaining columns are used as row "stub" variables and shown on the left. A
#' row-group variable can be promoted to group headers (using
#' [kableExtra::group_rows()]) so several rows share a common label.
#'
#' @param df A data frame.
#' @param ... Grouping columns for the **column headers** (e.g. `koen`,
#'   `haendelsestype`). The first group becomes the top-level header; remaining
#'   groups form sub-headers.
#' @param .value Column used for the cell values, given as a bare
#'   symbol, a string, or any tidyselect expression that resolves to
#'   exactly one column. The default `NULL` means **auto**: if `df` has
#'   a `value` column (the convention produced by [statgl_fetch()]) it
#'   is used; otherwise the function counts rows via [dplyr::n()] per
#'   `groups × row` combination and emits a message. To compute other
#'   per-cell statistics (means, sums, etc.), pre-aggregate with
#'   [dplyr::summarise()] before piping into `statgl_crosstable()`.
#'   Passing `.value` to a column that doesn't exist, or to an
#'   expression that selects more than one column, is an error.
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
#' @param .drop Optional named list specifying which **column groups**
#'   should be dropped from the table. Each element should be a vector of
#'   values for a grouping variable used in `...`, e.g.
#'   `list(gender = "Total", citydistrict = "Total")`. Any column whose
#'   column-header combination contains one of these values is removed from
#'   the output. A warning is issued if a name doesn't match a known
#'   column-group dimension or if values match no rows, so typos and
#'   value-mismatches surface clearly.
#' @param .secondary `r lifecycle::badge("deprecated")` Use `.drop`
#'   instead. Same shape and semantics.
#' @param .hide_mobile Optional named list of `column_group = values_to_hide`
#'   (same shape as `.drop`) identifying column groups to **visually hide
#'   on small screens** via a CSS media query (`max-width: 768px`). Unlike
#'   `.drop`, the columns remain in the rendered HTML — they're just hidden
#'   on narrow viewports. Validated the same way as `.drop`, so typos in
#'   dimension names or values surface as warnings.
#' @param .replace_0s `FALSE` (default; no replacement), `TRUE` (replace
#'   `0` with an en-dash), or a single character string used as a custom
#'   replacement.
#' @param .replace_nas `NULL` (default; no replacement) or a single
#'   character string used as the replacement for `NA` (e.g. `"."`).
#'   `TRUE` is accepted as a deprecated alias for `"."` and emits a
#'   warning.
#' @param .first_col_width Optional CSS width passed to
#'   [kableExtra::column_spec()] for the first column (e.g. `"8em"`).
#' @param .as_html Logical; if `FALSE` (default), returns an
#'   `htmlwidget` that renders correctly in every context — the
#'   RStudio Viewer when called interactively, knitr / Quarto chunks,
#'   and Quarto shortcode parameters — matching the behavior of
#'   [statgl_plot()]. If `TRUE`, returns a plain length-1 character
#'   vector instead; kept as an escape hatch for environments that
#'   can't handle widgets.
#'
#' @return An `htmlwidget` object by default (wrapping the rendered
#'   table HTML), or a length-1 character vector when
#'   `.as_html = TRUE`. In both cases the HTML includes a `<style>`
#'   block when `.hide_mobile` is in play.
#'
#' @importFrom dplyr arrange distinct mutate select across all_of count %>%
#' @importFrom tidyr unite pivot_wider
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquos enquo sym f_rhs eval_tidy is_formula quo_name
#' @importFrom kableExtra kable kable_styling add_header_above group_rows
#'   column_spec row_spec
#' @importFrom utils head
#'
#' @export
statgl_crosstable <- function(
  df,
  ...,
  .value = NULL, # NULL = auto: use `value` column if present, else dplyr::n()
  .row = NULL, # tidyselect spec; can be 1+ columns
  .row_group = NULL, # tidyselect spec; one or more stub cols to group
  .row_label = NULL, # label spanning all stub cols
  .big.mark = ".",
  .decimal.mark = ",",
  .caption = NULL,
  .year_col = NULL, # tidyselect spec
  .bold_rows = NULL, # numeric / character / formula
  .drop = NULL, # list(dim_name = values_to_drop)
  .secondary = NULL, # deprecated: use .drop
  .hide_mobile = NULL, # list(dim_name = values_to_hide_on_mobile)
  .replace_0s = FALSE, # FALSE / TRUE (en-dash) / custom string
  .replace_nas = NULL, # NULL / custom string (TRUE deprecated)
  .first_col_width = NULL,
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
  # ---- Resolve .value ---------------------------------------------------
  # NULL (default) = auto: use a `value` column if it exists, otherwise
  # fall back to row counts via dplyr::n() and emit a message. Anything
  # else is treated as a tidyselect spec that must resolve to exactly
  # one existing column (so typos and multi-column expressions surface
  # as clear errors).
  .value_q   <- rlang::enquo(.value)
  auto_value <- rlang::quo_is_null(.value_q)

  if (auto_value) {
    if ("value" %in% names(df)) {
      value_name <- "value"
    } else {
      value_name <- "n" # the count column we'll add after row_names is known
    }
  } else {
    sel <- tryCatch(
      tidyselect::eval_select(.value_q, df),
      error = function(e) {
        val_label <- rlang::as_label(.value_q)
        stop(
          "Column `", val_label, "` not found in `df`. ",
          "Pass `.value = <column>` to choose the cell-value column. ",
          "Available columns: ",
          paste(names(df), collapse = ", "), ".",
          call. = FALSE
        )
      }
    )
    if (length(sel) != 1L) {
      stop(
        "`.value` must select exactly one column; got: ",
        paste(names(sel), collapse = ", "), ".",
        call. = FALSE
      )
    }
    value_name <- names(sel)
  }

  value <- rlang::sym(value_name)

  # ---- 1) Determine row columns (can be 1 or many) -----------------
  if (missing(.row) || is.null(.row)) {
    group_names <- vapply(groups, rlang::quo_name, character(1))
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

  # ---- 1b) n() fallback: aggregate to one row per (group x row) combo --
  # When `.value` is auto and the data frame has no `value` column, we
  # count rows per (groups + row_names) combination and use that as the
  # cell value. This is what makes `statgl_crosstable(mpg, manufacturer)`
  # do something useful out of the box. For other statistics, the user
  # should pre-aggregate with summarise() before piping in.
  if (auto_value && value_name == "n") {
    message(
      "No `value` column found; counting rows via dplyr::n(). ",
      "Pre-aggregate with summarise() for other statistics."
    )
    group_names_for_count <- vapply(groups, rlang::quo_name, character(1))
    count_syms <- rlang::syms(c(group_names_for_count, row_names))
    df <- dplyr::count(df, !!!count_syms, name = "n")
  }

  # ---- 2) Replace zeros and NAs in df (before pivot/formatting) ----
  zero_repl <- resolve_replace_0s(.replace_0s)
  if (!is.null(zero_repl)) {
    df[] <- lapply(df, function(x) {
      x_chr <- trimws(as.character(x))
      x_chr[x_chr == "0"] <- zero_repl
      x_chr
    })
  }

  na_repl <- resolve_replace_nas(.replace_nas)
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
    tidyr::unite("col_key", !!!groups, sep = " \u2013 ", remove = FALSE) %>%
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

  # labels shown in the second header row (e.g. "I alt", "Kvinder", ...)
  display_labels <- sub("^[^\u2013]+\\s*\u2013\\s*", "", wide_cols)

  # build a combo data frame describing each column group combination,
  # aligned to wide_cols
  combo_df <- df %>%
    dplyr::distinct(!!!groups) %>%
    dplyr::arrange(!!primary_group, !!!rest_groups) %>%
    tidyr::unite("col_key", !!!groups, sep = " \u2013 ", remove = FALSE)

  combo_df <- combo_df[match(wide_cols, combo_df$col_key), ]

  # ---- 6a) Apply .drop: drop column groups by dimension value ------
  .drop <- resolve_drop(.drop, .secondary)
  if (!is.null(.drop) && length(wide_cols)) {
    validate_drop(.drop, combo_df)

    sec_mask <- rep(FALSE, nrow(combo_df))
    for (nm in names(.drop)) {
      if (!nm %in% names(combo_df)) next
      sec_mask <- sec_mask | combo_df[[nm]] %in% .drop[[nm]]
    }

    keep_mask <- !sec_mask

    wide_cols <- wide_cols[keep_mask]
    display_labels <- display_labels[keep_mask]
    combo_df <- combo_df[keep_mask, , drop = FALSE]

    df_wide <- df_wide[, c(row_names, wide_cols), drop = FALSE]
    df_logic <- df_logic[, c(row_names, wide_cols), drop = FALSE]
  }

  # after dropping secondary columns, recompute group_counts to match
  group_counts <- combo_df %>%
    dplyr::count(!!primary_group, name = "n")

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

  # ---- 9) Mobile hiding (.hide_mobile, CSS) --------------------------------
  # Indices reference positions in the final wide table: row stubs occupy
  # columns 1..stub_span, then the (post-.drop) wide_cols follow. We hide via
  # CSS so the columns are still in the HTML — just not rendered on narrow
  # viewports — which is consistent with statgl_table's .hide_mobile.
  hide_idx <- integer(0)
  if (!is.null(.hide_mobile)) {
    validate_hide_mobile(.hide_mobile, combo_df)
    hide_mask <- rep(FALSE, nrow(combo_df))
    for (nm in names(.hide_mobile)) {
      if (!nm %in% names(combo_df)) next
      hide_mask <- hide_mask | combo_df[[nm]] %in% .hide_mobile[[nm]]
    }
    hide_idx <- which(hide_mask) + stub_span
  }

  # --- Escape hatch: plain character HTML (legacy shortcode path) -------
  if (isTRUE(.as_html)) {
    return(hide_mobile_css(as.character(kb), hide_idx))
  }

  # --- Default: wrap the rendered table in an htmlwidget --------------
  kb_with_css <- attach_hide_mobile_css(kb, hide_idx)
  statgl_table_widget(as.character(kb_with_css))
}

# Internal helpers -------------------------------------------------------------

# Choose between the new `.drop` argument and the deprecated `.secondary`
# alias. If `.secondary` is supplied, emit a deprecation warning and fall
# back to it only when `.drop` is unset; if both are supplied, `.drop`
# wins (and the user still gets the deprecation message about
# `.secondary`).
resolve_drop <- function(drop, secondary) {
  if (!is.null(secondary)) {
    lifecycle::deprecate_warn(
      "0.5.2",
      "statgl_crosstable(.secondary = )",
      "statgl_crosstable(.drop = )"
    )
    if (is.null(drop)) drop <- secondary
  }
  drop
}

# Surface the three silent-failure modes of value-based column filtering:
# wrong shape (not a named list), wrong dimension name (typo / not in
# `...`), and wrong values (no match against the actual column values).
# Each produces a warning that names what was wrong and what's available
# instead, so the user can self-correct without reading the docs again.
validate_drop <- function(drop, combo_df) {
  if (!is.list(drop) ||
      is.null(names(drop)) ||
      any(!nzchar(names(drop)))) {
    warning(
      "`.drop` must be a named list of `column_group = values_to_drop`. ",
      "Got: ", paste(deparse(drop), collapse = " "),
      call. = FALSE
    )
    return(invisible())
  }

  dim_names <- setdiff(names(combo_df), "col_key")

  for (nm in names(drop)) {
    if (!nm %in% dim_names) {
      warning(
        "`.drop$", nm, "` is not a column-group dimension. Available: ",
        paste(dim_names, collapse = ", "), ".",
        call. = FALSE
      )
      next
    }
    vals <- drop[[nm]]
    column_values <- combo_df[[nm]]
    if (!any(vals %in% column_values)) {
      warning(
        "`.drop$", nm, "` values c(",
        paste0('"', vals, '"', collapse = ", "),
        ") matched no columns. Available: ",
        paste0('"', unique(as.character(column_values)), '"', collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  invisible()
}

# Resolve `.replace_0s` to either NULL (no replacement) or a single
# character string. `FALSE`/`NULL` -> no replacement, `TRUE` -> en-dash
# (U+2013), single-string passthrough. Warns on bad shapes.
resolve_replace_0s <- function(x) {
  if (isTRUE(x)) return("\u2013")
  if (isFALSE(x) || is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && !is.na(x)) return(x)
  warning(
    "`.replace_0s` should be FALSE/TRUE or a single character string. ",
    "Got: ", paste(deparse(x), collapse = " "),
    call. = FALSE
  )
  NULL
}

# Pick between the new `.hide_mobile` and the deprecated `.secondary`
# (CSS-hide alias) in statgl_table. Both arrive as already-captured
# quosures (tidyselect specifications). If `.secondary` is in play we
# emit a deprecation warning; if both are supplied, `.hide_mobile` wins
# but the warning still fires.
resolve_hide_mobile_q <- function(hide_q, sec_q) {
  if (!rlang::quo_is_null(sec_q)) {
    lifecycle::deprecate_warn(
      "0.5.2",
      "statgl_table(.secondary = )",
      "statgl_table(.hide_mobile = )"
    )
    if (rlang::quo_is_null(hide_q)) return(sec_q)
  }
  hide_q
}

# Validate the `.hide_mobile` shape used by statgl_crosstable (a named
# list of `column_group = values_to_hide`, mirroring `.drop`). Same
# three failure modes are surfaced as warnings: bad shape, unknown
# dimension, zero-match values.
validate_hide_mobile <- function(hide_mobile, combo_df) {
  if (!is.list(hide_mobile) ||
      is.null(names(hide_mobile)) ||
      any(!nzchar(names(hide_mobile)))) {
    warning(
      "`.hide_mobile` must be a named list of `column_group = values_to_hide`. ",
      "Got: ", paste(deparse(hide_mobile), collapse = " "),
      call. = FALSE
    )
    return(invisible())
  }

  dim_names <- setdiff(names(combo_df), "col_key")

  for (nm in names(hide_mobile)) {
    if (!nm %in% dim_names) {
      warning(
        "`.hide_mobile$", nm, "` is not a column-group dimension. Available: ",
        paste(dim_names, collapse = ", "), ".",
        call. = FALSE
      )
      next
    }
    vals <- hide_mobile[[nm]]
    column_values <- combo_df[[nm]]
    if (!any(vals %in% column_values)) {
      warning(
        "`.hide_mobile$", nm, "` values c(",
        paste0('"', vals, '"', collapse = ", "),
        ") matched no columns. Available: ",
        paste0('"', unique(as.character(column_values)), '"', collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  invisible()
}

# Attach the .hide_mobile <style> block to a kable object, preserving
# the kable's class and attributes so its print / knit_print methods
# keep doing the right thing (Viewer rendering, chunk rendering,
# inline R, shortcode stringification). If `hide_idx` is empty, the
# kable is returned untouched.
attach_hide_mobile_css <- function(kable_obj, hide_idx) {
  if (!length(hide_idx)) return(kable_obj)
  new_html <- hide_mobile_css(as.character(kable_obj), hide_idx)
  attributes(new_html) <- attributes(kable_obj)
  new_html
}

# Wrap a rendered HTML kable string with a media-query <style> block
# that hides columns at the given positions on narrow viewports
# (<= 768px). Used by both statgl_table and statgl_crosstable so the
# CSS shape stays identical. `hide_idx` is a vector of 1-based column
# positions; if empty, returns `html` unchanged.
hide_mobile_css <- function(html, hide_idx) {
  if (!length(hide_idx)) return(html)

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

  selectors_td <- paste(
    sprintf("#%s td:nth-child(%d)", table_id, hide_idx),
    collapse = ",\n  "
  )
  selectors_th <- paste(
    sprintf("#%s th:nth-child(%d)", table_id, hide_idx),
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

  paste(style, html, sep = "\n")
}

# Resolve `.replace_nas` to either NULL (no replacement) or a single
# character string. `NULL`/`FALSE` -> no replacement, single-string
# passthrough. `TRUE` is a deprecated alias for `"."`. Warns on bad
# shapes and on the deprecated TRUE value.
resolve_replace_nas <- function(x) {
  if (isTRUE(x)) {
    warning(
      "`.replace_nas = TRUE` is deprecated; pass `.replace_nas = \".\"` ",
      "explicitly to keep the same behavior. (TRUE was treated as \".\".)",
      call. = FALSE
    )
    return(".")
  }
  if (isFALSE(x) || is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && !is.na(x)) return(x)
  warning(
    "`.replace_nas` should be NULL or a single character string. ",
    "Got: ", paste(deparse(x), collapse = " "),
    call. = FALSE
  )
  NULL
}
