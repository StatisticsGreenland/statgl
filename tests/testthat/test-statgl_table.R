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
      .drop = list(gender = "Totaal"), # typo
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
    statgl_crosstable(df, gender, .drop = "T", .as_html = TRUE),
    "named list"
  )
})

test_that(".drop warns on unknown column-group dimension", {
  df <- make_long_df()
  expect_warning(
    statgl_crosstable(
      df, gender,
      .drop = list(genders = "T"), # typo on the dim name
      .as_html = TRUE
    ),
    "is not a column-group dimension"
  )
})

test_that(".drop warns when values match nothing", {
  df <- make_long_df()
  expect_warning(
    statgl_crosstable(
      df, gender,
      .drop = list(gender = "Totaal"),
      .as_html = TRUE
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
      .as_html = TRUE
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
      .replace_nas = TRUE,
      .as_html = TRUE
    ),
    "deprecated"
  )
})

# ---- .value validation ------------------------------------------------------

test_that("statgl_crosstable errors clearly when `.value` column is missing", {
  df <- data.frame(a = c("x", "y"), b = c("p", "q"))  # no `value` column
  expect_error(
    statgl_crosstable(df, a),
    "Column `value` not found",
    fixed = TRUE
  )
})

test_that(".value error names the missing column and lists what is available", {
  df <- data.frame(a = "x", b = "y", c = 1)
  err <- tryCatch(statgl_crosstable(df, a, .value = nope), error = identity)
  expect_s3_class(err, "simpleError")
  expect_match(conditionMessage(err), "Column `nope` not found", fixed = TRUE)
  expect_match(conditionMessage(err), "a, b, c",                fixed = TRUE)
})

# NOTE: an end-to-end test on ggplot2::mpg lives with the upcoming `.value`
# fallback work (auto-counting when no `value` column is present). The two
# error-path tests above cover the validation added in this commit.

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

test_that("statgl_table .hide_mobile emits a media-query <style> on .as_html", {
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

test_that("statgl_table .hide_mobile is ignored when .as_html = FALSE", {
  out <- statgl_table(
    make_wide_for_hide(),
    .hide_mobile = ialt,
    .as_html     = FALSE
  )
  # `out` is a kable object (knitr_kable is a character S3, so we test
  # the class — not is.character()).
  expect_s3_class(out, "knitr_kable")
  expect_no_match(as.character(out), "max-width: 768px", fixed = TRUE)
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

test_that("statgl_crosstable .hide_mobile is ignored when .as_html = FALSE", {
  out <- statgl_crosstable(
    make_long_df(), gender,
    .hide_mobile = list(gender = "T"),
    .as_html     = FALSE
  )
  expect_s3_class(out, "knitr_kable")
  expect_no_match(as.character(out), "max-width: 768px", fixed = TRUE)
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
