# Pure-logic tests for internal helpers in R/utils.R and R/statgl_fetch.R.

test_that("get_language maps the Greenlandic language characters", {
  get_language <- statgl:::get_language
  expect_equal(unname(get_language("N")), "kl")
  expect_equal(unname(get_language("D")), "da")
  expect_equal(unname(get_language("E")), "en")
  expect_equal(unname(get_language("X")), "en")
  # case-insensitive
  expect_equal(unname(get_language("n")), "kl")
})

test_that("get_language_code is the inverse mapping", {
  get_language_code <- statgl:::get_language_code
  expect_equal(unname(get_language_code("kl")), "N")
  expect_equal(unname(get_language_code("da")), "D")
  expect_equal(unname(get_language_code("en")), "E")
  # case-insensitive
  expect_equal(unname(get_language_code("EN")), "E")
})

test_that("is_valid_url accepts http(s) URLs and rejects bare strings", {
  is_valid_url <- statgl:::is_valid_url
  expect_true(is_valid_url("https://bank.stat.gl/api/v1/en/Greenland/"))
  expect_true(is_valid_url("http://example.com"))
  expect_false(is_valid_url("BEXSTA"))
  expect_false(is_valid_url(""))
})

test_that("generate_subfolders builds nested 2-char path segments", {
  generate_subfolders <- statgl:::generate_subfolders
  # Empty / short strings
  expect_equal(generate_subfolders(""), "")
  expect_equal(generate_subfolders("BE"), "BE")
  # Two-level path
  expect_equal(generate_subfolders("BE01"), "BE/BE01")
  # Three-level path
  expect_equal(generate_subfolders("BE0120"), "BE/BE01/BE0120")
})

test_that("is_greenland_api recognises Statistics Greenland URLs", {
  is_greenland_api <- statgl:::is_greenland_api
  expect_true(is_greenland_api("https://bank.stat.gl/api/v1/en/Greenland/"))
  expect_true(is_greenland_api("http://bank.stat.gl:443/api/v1/en/Greenland/"))
  expect_true(is_greenland_api("https://bank.stat.gl/api/v2/da/Greenland/"))
  expect_false(is_greenland_api("https://api.scb.se/OV0104/v1/doris/en/ssd/"))
  expect_false(is_greenland_api("https://example.com/api/v1/en/"))
})

test_that("infer_lang_from_url extracts the language segment", {
  infer_lang_from_url <- statgl:::infer_lang_from_url
  expect_equal(
    infer_lang_from_url("https://bank.stat.gl/api/v1/en/Greenland/"),
    "en"
  )
  expect_equal(
    infer_lang_from_url("https://bank.stat.gl/api/v1/kl/Greenland/"),
    "kl"
  )
  expect_equal(
    infer_lang_from_url("https://bank.stat.gl/api/v2/da/Greenland/"),
    "da"
  )
  expect_error(
    infer_lang_from_url("https://example.com/no-version/"),
    "Could not infer"
  )
})

test_that("statgl_api_url returns the default and respects options()", {
  expect_equal(
    statgl_api_url(),
    "https://bank.stat.gl/api/v1/en/Greenland/"
  )

  withr::with_options(
    list(statgl.api_url = "https://example.test/api/v1/en/Test/"),
    expect_equal(
      statgl_api_url(),
      "https://example.test/api/v1/en/Test/"
    )
  )
})
