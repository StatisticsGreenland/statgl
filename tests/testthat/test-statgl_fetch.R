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

# ---- Internal helpers --------------------------------------------------------
# These exercise the small pieces statgl_fetch() composes, without network.

test_that("is_eliminable handles the API's logical/character variants", {
  is_eliminable <- statgl:::is_eliminable
  expect_true(is_eliminable(list(elimination = TRUE)))
  expect_false(is_eliminable(list(elimination = FALSE)))
  expect_true(is_eliminable(list(elimination = "true")))
  expect_true(is_eliminable(list(elimination = "TRUE")))
  expect_true(is_eliminable(list(elimination = "Yes")))
  expect_true(is_eliminable(list(elimination = "1")))
  expect_false(is_eliminable(list(elimination = "no")))
  expect_false(is_eliminable(list(elimination = "0")))
  # Missing / NULL / unsupported types -> not eliminable
  expect_false(is_eliminable(list()))
  expect_false(is_eliminable(list(elimination = NULL)))
  expect_false(is_eliminable(list(elimination = NA)))
  expect_false(is_eliminable(list(elimination = 1)))
})

test_that("expand_eliminables fills in unspecified eliminable variables", {
  expand_eliminables <- statgl:::expand_eliminables
  meta <- list(variables = list(
    list(code = "gender", elimination = TRUE,  values = c("M", "K", "T")),
    list(code = "age",    elimination = FALSE, values = c("0", "1", "2")),
    list(code = "time",   elimination = NULL,  values = c("2020", "2021"))
  ))

  out <- expand_eliminables(list(), meta)
  expect_named(out, "gender")
  expect_identical(attr(out$gender, ".px_filter"), "all")

  # Already-specified variables are left alone.
  out <- expand_eliminables(list(gender = c("M", "K")), meta)
  expect_equal(out$gender, c("M", "K"))
  expect_null(attr(out$gender, ".px_filter"))

  # Non-eliminable variables are not added.
  out <- expand_eliminables(list(), meta)
  expect_false("age"  %in% names(out))
  expect_false("time" %in% names(out))
})

test_that("resolve_url leaves URLs alone when .lang is NULL", {
  resolve_url <- statgl:::resolve_url
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BEXSTA.px"
  expect_equal(resolve_url(url, NULL, statgl_api_url()), url)
})

test_that("resolve_url rewrites the language segment when .lang is set", {
  resolve_url <- statgl:::resolve_url
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BEXSTA.px"
  expect_equal(
    resolve_url(url, "kl", statgl_api_url()),
    "https://bank.stat.gl/api/v1/kl/Greenland/BE/BE01/BEXSTA.px"
  )
})

test_that("pick_chunk_variable prefers the longest px_all('*') variable", {
  pick_chunk_variable <- statgl:::pick_chunk_variable
  meta <- list(variables = list(
    list(code = "age",    values = as.character(0:99)), # 100 values
    list(code = "gender", values = c("M", "K", "T"))    #   3 values
  ))
  vls <- list(age = px_all("*"), gender = px_all("*"))
  expect_equal(pick_chunk_variable(vls, meta), "age")
})

test_that("pick_chunk_variable falls back to longest user selection", {
  pick_chunk_variable <- statgl:::pick_chunk_variable
  meta <- list(variables = list(
    list(code = "age",  values = as.character(0:99)),
    list(code = "time", values = c("2020", "2021"))
  ))
  vls <- list(age = c("0", "1"), time = 2010:2025)
  expect_equal(pick_chunk_variable(vls, meta), "time")
})

test_that("pick_chunk_variable errors when there's nothing to chunk on", {
  pick_chunk_variable <- statgl:::pick_chunk_variable
  expect_error(
    pick_chunk_variable(list(), list(variables = list())),
    "no suitable variable"
  )
})
