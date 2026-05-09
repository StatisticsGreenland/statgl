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
