df_line <- data.frame(
  time  = 2018:2022,
  value = c(10, 20, 15, 25, 30)
)

df_group <- data.frame(
  time     = rep(2018:2022, 2),
  value    = c(10, 20, 15, 25, 30, 5, 8, 12, 18, 22),
  locality = rep(c("Nuuk", "Sisimiut"), each = 5)
)

# ---------------------------------------------------------------------------
# Basic output type
# ---------------------------------------------------------------------------

test_that("statgl_plot() returns a highchart object", {
  p <- statgl_plot(df_line, x = time, y = value)
  expect_s3_class(p, "highchart")
})

# ---------------------------------------------------------------------------
# onRender reflow
# ---------------------------------------------------------------------------

test_that("onRender callback contains a requestAnimationFrame reflow", {
  p <- statgl_plot(df_line, x = time, y = value)
  js <- p$jsHooks$render[[1]]$code
  expect_true(grepl("requestAnimationFrame", js, fixed = TRUE))
  expect_true(grepl("reflow", js, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# series_tags
# ---------------------------------------------------------------------------

test_that("series_tags writes tags onto each series", {
  p <- statgl_plot(
    df_group,
    x = time, y = value, group = locality,
    series_tags = list(station = "locality")
  )
  series <- p$x$hc_opts$series
  tags   <- vapply(series, function(s) s$tags$station %||% NA_character_, character(1))
  expect_setequal(tags, c("Nuuk", "Sisimiut"))
})

test_that("series_tags errors when column is missing", {
  expect_error(
    statgl_plot(df_group, x = time, y = value, group = locality,
                series_tags = list(station = "nonexistent")),
    "not found"
  )
})

# ---------------------------------------------------------------------------
# legend_position
# ---------------------------------------------------------------------------

test_that("legend_position = 'none' disables legend", {
  p <- statgl_plot(df_line, x = time, y = value, legend_position = "none")
  expect_false(isTRUE(p$x$hc_opts$legend$enabled))
})

test_that("legend_position = 'right' sets align and layout", {
  p <- statgl_plot(df_line, x = time, y = value, legend_position = "right")
  leg <- p$x$hc_opts$legend
  expect_equal(leg$align, "right")
  expect_equal(leg$layout, "vertical")
})

# ---------------------------------------------------------------------------
# height
# ---------------------------------------------------------------------------

test_that("height is forwarded to chart options", {
  p <- statgl_plot(df_line, x = time, y = value, height = 500)
  expect_equal(p$x$hc_opts$chart$height, 500)
})

# ---------------------------------------------------------------------------
# type inference
# ---------------------------------------------------------------------------

test_that("Date x infers 'line'", {
  df_dates <- data.frame(
    time  = as.Date(paste0(2010:2022, "-01-01")),
    value = seq_len(13)
  )
  p <- statgl_plot(df_dates, x = time, y = value)
  expect_equal(p$x$hc_opts$series[[1]]$type, "line")
})

test_that("character x infers 'column'", {
  df_cat <- data.frame(cat = c("A", "B", "C"), value = c(1, 2, 3))
  p <- statgl_plot(df_cat, x = cat, y = value)
  expect_equal(p$x$hc_opts$series[[1]]$type, "column")
})
