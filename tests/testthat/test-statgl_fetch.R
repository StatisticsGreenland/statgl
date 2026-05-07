# Tests for statgl_fetch() that don't require network access. They
# exercise the request-building / URL-rewriting paths via .dry_run = TRUE,
# plus the hard error from the deprecated `url` argument.

test_that(".dry_run returns the resolved URL, selections, and JSON body", {
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BEXSTA.px"
  out <- statgl_fetch(
    url,
    gender = c("M", "K"),
    time = 2010:2012,
    .dry_run = TRUE
  )
  expect_named(out, c("url", "selections", "body"))
  expect_equal(out$url, url)
  expect_named(out$selections, c("gender", "time"))
  expect_equal(out$selections$gender, c("M", "K"))
  # The JSON body should mention the variable codes and the values.
  expect_match(out$body, '"code":"gender"', fixed = TRUE)
  expect_match(out$body, '"values":\\["M","K"\\]')
  expect_match(out$body, '"code":"time"', fixed = TRUE)
})

test_that(".lang on a URL input rewrites the language segment", {
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BEXSTA.px"
  out <- statgl_fetch(url, .lang = "kl", .dry_run = TRUE)
  expect_match(out$url, "/v1/kl/", fixed = TRUE)
  expect_no_match(out$url, "/v1/en/", fixed = TRUE)
})

test_that(".lang preserves the rest of the URL", {
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BEXSTA.px"
  out <- statgl_fetch(url, .lang = "da", .dry_run = TRUE)
  expect_equal(
    out$url,
    "https://bank.stat.gl/api/v1/da/Greenland/BE/BE01/BEXSTA.px"
  )
})

test_that(".dry_run on an empty selection produces an empty query body", {
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BEXSTA.px"
  out <- statgl_fetch(url, .dry_run = TRUE)
  # build_query produces {"query":[],"response":{"format":"json-stat"}}
  expect_match(out$body, '"query":\\[\\]')
  expect_match(out$body, '"format":"json-stat"', fixed = TRUE)
})

test_that("statgl_fetch(url=) errors via lifecycle::deprecate_stop", {
  expect_error(
    statgl_fetch("BEXSTA", url = "https://example.com/whatever"),
    class = "lifecycle_error_deprecated"
  )
})
