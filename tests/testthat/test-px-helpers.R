# Pure-logic tests for the px_* selection helpers used in statgl_fetch().
# These functions are thin wrappers that attach a .px_filter attribute to
# their input — no network or API access is needed to verify them.

test_that("px_top tags input with .px_filter = 'Top'", {
  x <- px_top(5)
  expect_equal(as.numeric(x), 5)
  expect_identical(attr(x, ".px_filter"), "Top")
})

test_that("px_top defaults to top_n = 1", {
  expect_equal(as.numeric(px_top()), 1)
})

test_that("px_all tags input with .px_filter = 'all'", {
  x <- px_all("*0")
  expect_equal(as.character(x), "*0")
  expect_identical(attr(x, ".px_filter"), "all")
})

test_that("px_all defaults to '*' (match-all)", {
  expect_equal(as.character(px_all()), "*")
})

test_that("px_agg tags values with the supplied aggregation file", {
  x <- px_agg("5-year.agg", "-4", "5-9", "10-14")
  expect_equal(as.character(x), c("-4", "5-9", "10-14"))
  expect_identical(attr(x, ".px_filter"), "agg:5-year.agg")
})
