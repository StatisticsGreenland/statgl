# Input-validation tests for statgl_url(). These all fail before any
# network call, so they're safe to run on offline build hosts.

test_that("statgl_url rejects bad table_id", {
  expect_error(statgl_url(NULL),                "non-empty character string")
  expect_error(statgl_url(NA_character_),       "non-empty character string")
  expect_error(statgl_url(""),                  "non-empty character string")
  expect_error(statgl_url(123),                 "non-empty character string")
  expect_error(
    statgl_url(c("BEXST1", "BEXST2")),
    "non-empty character string"
  )
})

test_that("statgl_url rejects bad lang", {
  expect_error(
    statgl_url("BEXST1", lang = c("en", "kl")),
    "single character string"
  )
  expect_error(
    statgl_url("BEXST1", lang = 1),
    "single character string"
  )
  expect_error(
    statgl_url("BEXST1", lang = NA_character_),
    "single character string"
  )
})
