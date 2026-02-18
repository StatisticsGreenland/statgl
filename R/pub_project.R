pub_project <- function(path, ...) {
  # Ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  dots <- list(...)
  `%||%` <- function(x, y) if (is.null(x) || x == "") y else x

  pub_title <- dots$pub_title %||% "Untitled"
  pub_subtitle <- dots$pub_subtitle %||% ""

  # Create the template inside the new project folder
  withr::with_dir(
    path,
    use_statgl_template(title = pub_title, subtitle = pub_subtitle)
  )

  withr::with_dir(
    path,
    file.copy(
      "_extensions/StatisticsGreenland/statglshortcodes/_quarto",
      "_quarto.yml"
    )
  )

  invisible(path)
}
