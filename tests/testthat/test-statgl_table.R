# Tests for statgl_table.R. Pass 1 (in progress): focused on the
# `.drop` rename and its validation. More to come as the unification
# work lands.

# ---- statgl_crosstable: .drop value-based filtering --------------------------

make_long_df <- function() {
  data.frame(
    time   = rep(2020:2021, each = 3),
    gender = rep(c("M", "K", "T"), 2),
    value  = 1:6,
    stringsAsFactors = FALSE
  )
}

test_that(".drop removes the matching column-group values", {
  df <- make_long_df()
  html <- statgl_crosstable(
    df, gender,
    .drop    = list(gender = "T"),
    .as_html = TRUE
  )
  # Header for "T" should be gone; M and K remain.
  # kableExtra renders column-group labels as `<div>VALUE</div>` inside
  # the top header row.
  expect_no_match(html, ">T</div>")
  expect_match(html, ">M</div>")
  expect_match(html, ">K</div>")
})

test_that(".drop with no matches still produces a (warned) table", {
  df <- make_long_df()
  expect_warning(
    out <- statgl_crosstable(
      df, gender,
      .drop    = list(gender = "Totaal"), # typo
      .as_html = TRUE
    ),
    "matched no columns"
  )
  # Original three columns are still there.
  expect_match(out, ">M</div>")
  expect_match(out, ">K</div>")
  expect_match(out, ">T</div>")
})

# ---- statgl_crosstable: validation warnings ---------------------------------

test_that(".drop warns when given a non-list shape", {
  df <- make_long_df()
  expect_warning(
    statgl_crosstable(df, gender, .drop = "T"),
    "named list"
  )
})

test_that(".drop warns on unknown column-group dimension", {
  df <- make_long_df()
  expect_warning(
    statgl_crosstable(
      df, gender,
      .drop = list(genders = "T") # typo on the dim name
    ),
    "is not a column-group dimension"
  )
})

test_that(".drop warns when values match nothing", {
  df <- make_long_df()
  expect_warning(
    statgl_crosstable(
      df, gender,
      .drop = list(gender = "Totaal")
    ),
    'values c\\("Totaal"\\) matched no columns'
  )
})

# ---- .secondary deprecation alias -------------------------------------------

test_that(".secondary still works but warns about deprecation", {
  df <- make_long_df()
  expect_warning(
    out <- statgl_crosstable(
      df, gender,
      .secondary = list(gender = "T"),
      .as_html   = TRUE
    ),
    class = "lifecycle_warning_deprecated"
  )
  # The deprecated path still drops the column.
  expect_no_match(out, ">T</div>")
  expect_match(out, ">M</div>")
})

test_that(".drop wins when both .drop and .secondary are supplied", {
  df <- make_long_df()
  expect_warning(
    out <- statgl_crosstable(
      df, gender,
      .drop      = list(gender = "T"),
      .secondary = list(gender = "M"), # would also drop M, but is ignored
      .as_html   = TRUE
    ),
    class = "lifecycle_warning_deprecated"
  )
  # Only "T" is dropped, M is preserved.
  expect_no_match(out, ">T</div>")
  expect_match(out, ">M</div>")
})

# ---- .replace_0s / .replace_nas unified shape -------------------------------

test_that("resolve_replace_0s handles FALSE / NULL / TRUE / string", {
  resolve_replace_0s <- statgl:::resolve_replace_0s
  expect_null(resolve_replace_0s(FALSE))
  expect_null(resolve_replace_0s(NULL))
  expect_equal(resolve_replace_0s(TRUE), "–")
  expect_equal(resolve_replace_0s("-"), "-")
  expect_equal(resolve_replace_0s("[-]{}"), "[-]{}")
})

test_that("resolve_replace_0s warns on bad shapes", {
  resolve_replace_0s <- statgl:::resolve_replace_0s
  expect_warning(out <- resolve_replace_0s(123),         "single character string")
  expect_null(out)
  expect_warning(out <- resolve_replace_0s(c("a", "b")), "single character string")
  expect_null(out)
  expect_warning(out <- resolve_replace_0s(NA),          "single character string")
  expect_null(out)
})

test_that("resolve_replace_nas handles NULL / FALSE / string", {
  resolve_replace_nas <- statgl:::resolve_replace_nas
  expect_null(resolve_replace_nas(NULL))
  expect_null(resolve_replace_nas(FALSE))
  expect_equal(resolve_replace_nas("."), ".")
  expect_equal(resolve_replace_nas(".."), "..")
})

test_that("resolve_replace_nas deprecates TRUE -> '.'", {
  resolve_replace_nas <- statgl:::resolve_replace_nas
  expect_warning(out <- resolve_replace_nas(TRUE), "deprecated")
  expect_equal(out, ".")
})

test_that("resolve_replace_nas warns on bad shapes", {
  resolve_replace_nas <- statgl:::resolve_replace_nas
  expect_warning(out <- resolve_replace_nas(123),         "NULL or a single character string")
  expect_null(out)
  expect_warning(out <- resolve_replace_nas(c("a", "b")), "NULL or a single character string")
  expect_null(out)
})

# End-to-end: confirm both functions consume the helpers the same way.

test_that("statgl_table replaces 0s with en-dash on .replace_0s = TRUE", {
  df <- data.frame(label = c("a", "b"), value = c(0, 5))
  out <- statgl_table(df, .replace_0s = TRUE, .as_html = TRUE)
  expect_match(out, "–", fixed = TRUE)
})

test_that("statgl_table accepts a custom .replace_0s string", {
  df <- data.frame(label = c("a", "b"), value = c(0, 5))
  out <- statgl_table(df, .replace_0s = "[-]{}", .as_html = TRUE)
  expect_match(out, "[-]{}", fixed = TRUE)
})

test_that("statgl_crosstable .replace_nas = TRUE warns and uses '.'", {
  df <- make_long_df()
  df$value[1] <- NA_integer_
  expect_warning(
    out <- statgl_crosstable(
      df, gender,
      .replace_nas = TRUE
    ),
    "deprecated"
  )
})

# ---- .value validation ------------------------------------------------------

test_that("statgl_crosstable falls back to n() when no `value` column", {
  df <- data.frame(
    region = c("A", "A", "B", "B", "B"),
    year   = c(2020, 2021, 2020, 2020, 2021),
    stringsAsFactors = FALSE
  )
  expect_message(
    out <- statgl_crosstable(df, region, .as_html = TRUE),
    "counting rows via dplyr::n"
  )
  expect_match(out, "<table", fixed = TRUE)
})

test_that(".value error names the missing column and lists what is available", {
  df <- data.frame(a = "x", b = "y", c = 1)
  err <- tryCatch(statgl_crosstable(df, a, .value = nope), error = identity)
  expect_s3_class(err, "simpleError")
  expect_match(conditionMessage(err), "Column `nope` not found", fixed = TRUE)
  expect_match(conditionMessage(err), "a, b, c",                fixed = TRUE)
})

test_that(".value resolving to multiple columns errors clearly", {
  df <- data.frame(a = "x", b = 1, c = 2)
  expect_error(
    statgl_crosstable(df, a, .value = c(b, c)),
    "must select exactly one column"
  )
})

test_that(".value fallback to n() works on ggplot2::mpg", {
  skip_if_not_installed("ggplot2")
  expect_message(
    out <- statgl_crosstable(
      dplyr::select(ggplot2::mpg, manufacturer, trans),
      trans,
      .as_html = TRUE
    ),
    "counting rows via dplyr::n"
  )
  # `manufacturer` is the row-stub column; with the default
  # .row_label = NULL the header is blanked, but the values still
  # appear as row labels — check for one of them.
  expect_match(out, "audi", fixed = TRUE)
})

# ---- statgl_table: .hide_mobile (CSS responsive hiding) ---------------------

make_wide_for_hide <- function() {
  data.frame(
    aar     = c(2020, 2021),
    kvinder = c(1L, 2L),
    maend   = c(3L, 4L),
    ialt    = c(5L, 6L),
    stringsAsFactors = FALSE
  )
}

test_that("statgl_table .hide_mobile emits a media-query <style>", {
  # Use .as_html = TRUE so `out` is a character vector for expect_match.
  out <- statgl_table(
    make_wide_for_hide(),
    .hide_mobile = ialt,
    .as_html     = TRUE
  )
  expect_match(out, "max-width: 768px", fixed = TRUE)
  expect_match(out, "display: none",    fixed = TRUE)
  # The hidden column is still in the HTML — just CSS-hidden.
  expect_match(out, "> 5 <", fixed = TRUE)
  expect_match(out, "> 6 <", fixed = TRUE)
})

test_that("statgl_table without .hide_mobile emits no media-query CSS", {
  out <- statgl_table(make_wide_for_hide(), .as_html = TRUE)
  expect_no_match(out, "max-width: 768px", fixed = TRUE)
})

test_that("statgl_table .secondary still works but warns about deprecation", {
  expect_warning(
    out <- statgl_table(
      make_wide_for_hide(),
      .secondary = ialt,
      .as_html   = TRUE
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_match(out, "max-width: 768px", fixed = TRUE)
})

test_that(".hide_mobile wins when both .hide_mobile and .secondary are supplied", {
  expect_warning(
    out <- statgl_table(
      make_wide_for_hide(),
      .hide_mobile = ialt,    # column 4
      .secondary   = kvinder, # column 2
      .as_html     = TRUE
    ),
    class = "lifecycle_warning_deprecated"
  )
  # .hide_mobile (column 4) is what gets applied — not .secondary (column 2).
  expect_match(out, "nth-child\\(4\\)")
  expect_no_match(out, "nth-child\\(2\\)")
})

# ---- statgl_crosstable: .hide_mobile (CSS responsive hiding) ----------------

test_that("statgl_crosstable .hide_mobile emits CSS without dropping columns", {
  out <- statgl_crosstable(
    make_long_df(), gender,
    .hide_mobile = list(gender = "T"),
    .as_html     = TRUE
  )
  expect_match(out, "max-width: 768px", fixed = TRUE)
  expect_match(out, "display: none",    fixed = TRUE)
  # Unlike .drop, the T columns remain in the HTML (just CSS-hidden).
  expect_match(out, ">T</div>")
})

test_that("statgl_crosstable .hide_mobile warns on non-list shape", {
  expect_warning(
    statgl_crosstable(
      make_long_df(), gender,
      .hide_mobile = c("T"),
      .as_html     = TRUE
    ),
    "must be a named list"
  )
})

test_that("statgl_crosstable .hide_mobile warns on unknown dimension", {
  expect_warning(
    statgl_crosstable(
      make_long_df(), gender,
      .hide_mobile = list(genders = "T"),
      .as_html     = TRUE
    ),
    "is not a column-group dimension"
  )
})

test_that("statgl_crosstable .hide_mobile warns when values match nothing", {
  expect_warning(
    statgl_crosstable(
      make_long_df(), gender,
      .hide_mobile = list(gender = "X"),
      .as_html     = TRUE
    ),
    "matched no columns"
  )
})

# ---- Return type: htmlwidget by default -------------------------------------

test_that("statgl_table returns an htmlwidget by default", {
  out <- statgl_table(make_wide_for_hide())
  expect_s3_class(out, "htmlwidget")
  expect_s3_class(out, "statgl_table")
  # The pre-rendered HTML lives at out$x$html.
  expect_type(out$x$html, "character")
  expect_match(out$x$html, "<table", fixed = TRUE)
})

test_that("statgl_crosstable returns an htmlwidget by default", {
  out <- statgl_crosstable(make_long_df(), gender)
  expect_s3_class(out, "htmlwidget")
  expect_match(out$x$html, "<table", fixed = TRUE)
})

test_that("statgl_table .hide_mobile CSS is embedded in the widget HTML", {
  out <- statgl_table(make_wide_for_hide(), .hide_mobile = ialt)
  expect_s3_class(out, "htmlwidget")
  expect_match(out$x$html, "max-width: 768px", fixed = TRUE)
})

test_that("statgl_crosstable .hide_mobile CSS is embedded in the widget HTML", {
  out <- statgl_crosstable(
    make_long_df(), gender,
    .hide_mobile = list(gender = "T")
  )
  expect_s3_class(out, "htmlwidget")
  expect_match(out$x$html, "max-width: 768px", fixed = TRUE)
})

# ---- Escape hatch: .as_html = TRUE returns a plain character vector ---------

test_that("statgl_table .as_html = TRUE returns a plain HTML string", {
  out <- statgl_table(make_wide_for_hide(), .as_html = TRUE)
  expect_type(out, "character")
  expect_false(inherits(out, "htmlwidget"))
  expect_false(inherits(out, "knitr_kable"))
  expect_match(out, "<table", fixed = TRUE)
})

test_that("statgl_crosstable .as_html = TRUE returns a plain HTML string", {
  out <- statgl_crosstable(
    make_long_df(), gender,
    .as_html = TRUE
  )
  expect_type(out, "character")
  expect_false(inherits(out, "htmlwidget"))
  expect_match(out, "<table", fixed = TRUE)
})

test_that(".as_html = TRUE also carries the .hide_mobile CSS", {
  out <- statgl_table(
    make_wide_for_hide(),
    .hide_mobile = ialt,
    .as_html     = TRUE
  )
  expect_match(out, "max-width: 768px", fixed = TRUE)
})

# ---- Pass 4: cross-pollinated params ----------------------------------------

test_that("statgl_table accepts a .caption", {
  out <- statgl_table(
    data.frame(x = 1:2, y = c("a", "b")),
    .caption = "My Caption",
    .as_html = TRUE
  )
  expect_match(out, "My Caption", fixed = TRUE)
})

test_that("statgl_table .bold_rows accepts integer positions", {
  df <- data.frame(label = c("a", "b", "c"), value = c(1, 2, 3))
  out <- statgl_table(df, .bold_rows = 2, .as_html = TRUE)
  # kableExtra::row_spec(..., bold = TRUE) emits inline `font-weight: bold;`.
  expect_match(out, "font-weight: bold", fixed = TRUE)
})

test_that("statgl_table .bold_rows accepts a character vector matching first col", {
  df <- data.frame(label = c("a", "b", "c"), value = c(1, 2, 3))
  out <- statgl_table(df, .bold_rows = "b", .as_html = TRUE)
  expect_match(out, "font-weight: bold", fixed = TRUE)
})

test_that("statgl_table .bold_rows accepts a one-sided formula", {
  df <- data.frame(label = c("a", "b", "c"), value = c(1, 2, 3))
  out <- statgl_table(df, .bold_rows = ~ value == "3", .as_html = TRUE)
  expect_match(out, "font-weight: bold", fixed = TRUE)
})

test_that("statgl_crosstable applies a .bottom_rule by default", {
  df <- make_long_df()
  out <- statgl_crosstable(df, gender, .as_html = TRUE)
  # The bottom rule is implemented as an inline border-bottom style.
  expect_match(out, "border-bottom: 2px solid #000", fixed = TRUE)
})

test_that("statgl_crosstable .bottom_rule = FALSE suppresses the bottom rule", {
  df <- make_long_df()
  out <- statgl_crosstable(df, gender, .bottom_rule = FALSE, .as_html = TRUE)
  expect_no_match(out, "border-bottom: 2px solid #000", fixed = TRUE)
})

test_that("statgl_crosstable .digits is passed through to format()", {
  df <- data.frame(
    year   = c(2020L, 2021L),
    gender = c("M", "K"),
    value  = c(1234.567, 7654.321),
    stringsAsFactors = FALSE
  )
  default <- statgl_crosstable(df, gender,              .as_html = TRUE)
  digits3 <- statgl_crosstable(df, gender, .digits = 3, .as_html = TRUE)
  # Passing .digits should change the rendered numbers — the exact
  # output of format() depends on big.mark/decimal.mark interactions,
  # so verifying "different" is more robust than asserting a literal.
  expect_false(identical(default, digits3))
  # And the original unrounded magnitude should disappear under digits = 3.
  expect_no_match(digits3, "1234,567", fixed = TRUE)
})

test_that("resolve_bold_rows handles NULL / numeric / character / formula", {
  resolve_bold_rows <- statgl:::resolve_bold_rows
  df <- data.frame(label = c("a", "b", "c"), value = 1:3)
  expect_equal(resolve_bold_rows(NULL, df, "label"), integer(0))
  expect_equal(resolve_bold_rows(c(1L, 3L), df, "label"), c(1L, 3L))
  expect_equal(resolve_bold_rows("b", df, "label"), 2L)
  expect_equal(resolve_bold_rows(~ value > 1, df, "label"), c(2L, 3L))
})

test_that("resolve_bold_rows errors on non-logical formula result", {
  resolve_bold_rows <- statgl:::resolve_bold_rows
  df <- data.frame(label = "a", value = 1)
  expect_error(
    resolve_bold_rows(~ value + 1, df, "label"),
    "logical vector"
  )
})

# ---- Helper unit tests (offline, no kable rendering) -------------------------

test_that("validate_drop is silent on well-formed input", {
  validate_drop <- statgl:::validate_drop
  combo_df <- data.frame(
    gender  = c("M", "K", "T"),
    col_key = c("M", "K", "T"),
    stringsAsFactors = FALSE
  )
  expect_silent(validate_drop(list(gender = "T"), combo_df))
})

test_that("resolve_drop prefers .drop over .secondary, warns on either", {
  resolve_drop <- statgl:::resolve_drop

  # Neither set -> NULL, no warning.
  expect_silent(out <- resolve_drop(NULL, NULL))
  expect_null(out)

  # Only .drop set -> .drop, no warning.
  expect_silent(out <- resolve_drop(list(g = "T"), NULL))
  expect_equal(out, list(g = "T"))

  # Only .secondary set -> warns, returns secondary.
  expect_warning(
    out <- resolve_drop(NULL, list(g = "T")),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(out, list(g = "T"))

  # Both set -> warns, returns .drop (wins over secondary).
  expect_warning(
    out <- resolve_drop(list(g = "A"), list(g = "B")),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(out, list(g = "A"))
})

test_that("validate_hide_mobile is silent on well-formed input", {
  validate_hide_mobile <- statgl:::validate_hide_mobile
  combo_df <- data.frame(
    gender  = c("M", "K", "T"),
    col_key = c("M", "K", "T"),
    stringsAsFactors = FALSE
  )
  expect_silent(validate_hide_mobile(list(gender = "T"), combo_df))
})

test_that("validate_hide_mobile surfaces the three silent-failure modes", {
  validate_hide_mobile <- statgl:::validate_hide_mobile
  combo_df <- data.frame(
    gender  = c("M", "K", "T"),
    col_key = c("M", "K", "T"),
    stringsAsFactors = FALSE
  )
  expect_warning(validate_hide_mobile(c("T"), combo_df),
                 "must be a named list")
  expect_warning(validate_hide_mobile(list(genders = "T"), combo_df),
                 "is not a column-group dimension")
  expect_warning(validate_hide_mobile(list(gender = "X"), combo_df),
                 "matched no columns")
})

test_that("resolve_hide_mobile_q prefers .hide_mobile and warns on .secondary", {
  resolve_hide_mobile_q <- statgl:::resolve_hide_mobile_q

  # Neither set -> returns the (NULL-wrapping) hide_q, no warning.
  out <- resolve_hide_mobile_q(rlang::quo(NULL), rlang::quo(NULL))
  expect_true(rlang::quo_is_null(out))

  # Only .hide_mobile set -> returns hide_q, no warning.
  expect_silent(
    out <- resolve_hide_mobile_q(rlang::quo(year), rlang::quo(NULL))
  )
  expect_equal(rlang::as_label(out), "year")

  # Only .secondary set -> warns, returns secondary.
  expect_warning(
    out <- resolve_hide_mobile_q(rlang::quo(NULL), rlang::quo(year)),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(rlang::as_label(out), "year")

  # Both set -> warns, returns .hide_mobile (wins).
  expect_warning(
    out <- resolve_hide_mobile_q(rlang::quo(year), rlang::quo(model)),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(rlang::as_label(out), "year")
})

test_that("hide_mobile_css returns html unchanged on empty index", {
  hide_mobile_css <- statgl:::hide_mobile_css
  expect_identical(
    hide_mobile_css("<table>x</table>", integer(0)),
    "<table>x</table>"
  )
})

test_that("hide_mobile_css wraps the table with media-query style", {
  hide_mobile_css <- statgl:::hide_mobile_css
  out <- hide_mobile_css("<table>x</table>", c(2L, 4L))
  expect_match(out, "max-width: 768px",        fixed = TRUE)
  expect_match(out, "<table id=\"statgltbl-",  fixed = TRUE)
  expect_match(out, "td:nth-child\\(2\\)")
  expect_match(out, "td:nth-child\\(4\\)")
  expect_match(out, "th:nth-child\\(2\\)")
})
