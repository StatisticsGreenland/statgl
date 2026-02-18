#' Create a statgl Quarto document from template
#'
#' Generates a new `.qmd` file based on the `statglshortcodes` template.
#' The extension is installed if missing. The filename is derived from
#' the provided title using [janitor::make_clean_names()]. The YAML
#' `title` and optional `subtitle` fields are populated automatically,
#' and the file is opened in the editor.
#'
#' @param title Character. Document title. Used for both the YAML header
#'   and filename generation. Must not be empty.
#' @param subtitle Character. Optional subtitle for the YAML header.
#'
#' @return Invisibly returns the path to the created `.qmd` file.
#'
#' @export
use_statgl_template <- function(title, subtitle) {
  if (missing(title)) {
    stop("You must provide a title")
  }
  if (title == "") {
    stop("You must provide a title")
  }

  slug <- janitor::make_clean_names(trimws(title))

  ext_path <- "_extensions/StatisticsGreenland/statglshortcodes"

  if (!dir.exists(ext_path)) {
    quarto::quarto_add_extension(
      "StatisticsGreenland/statgl-shortcodes",
      no_prompt = TRUE,
      quiet = TRUE
    )
    message("Installed statgl shortcodes in '_extensions' folder")
  }

  source_file <- file.path(
    ext_path,
    "statgl_template"
  )

  if (!file.exists(source_file)) {
    stop("Template file not found inside extension.")
  }

  lines <- readLines(source_file)
  target_file <- paste0(slug, ".qmd")

  if (file.exists(target_file)) {
    stop("File already exists.")
  }

  title_line_index <- grep("^title:", lines)
  if (length(title_line_index) > 0) {
    lines[title_line_index[1]] <- paste0('title: "', title, '"')
  }

  if (!missing(subtitle)) {
    subtitle_line_index <- grep("^subtitle:", lines)
    if (length(subtitle_line_index) > 0) {
      lines[subtitle_line_index[1]] <- paste0('subtitle: "', subtitle, '"')
    }
  }

  writeLines(lines, target_file)

  file.edit(target_file)

  invisible(target_file)
}
