pub_project <- function(path, ...) {
  `%||%` <- function(x, y) if (is.null(x) || x == "") y else x

  # Ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Install extension inside project
  withr::with_dir(
    path,
    quarto::quarto_add_extension(
      "StatisticsGreenland/statgl-shortcodes",
      no_prompt = TRUE,
      quiet = TRUE
    )
  )

  dots <- list(...)

  pub_title <- dots$pub_title %||% "Untitled"
  pub_subtitle <- dots$pub_subtitle %||% ""

  # Slugged filename
  slug <- janitor::make_clean_names(pub_title)
  qmd_file <- file.path(path, paste0(slug, ".qmd"))

  # QMD template
  qmd_template <- c(
    "---",
    paste0('title: "', pub_title, '"'),
    paste0('subtitle: "', pub_subtitle, '"'),
    "format: statglshortcodes-html",
    "execute:",
    "  echo: false",
    "  message: false",
    "  warning: false",
    "---",
    "",
    "```{r}",
    "library(tidyverse)",
    "library(statgl)",
    "```",
    "",
    "```{r}",
    'df1 <- statgl_fetch("BEDSTALL",',
    '  district = px_agg("BEXSTALLstørrelse.agg",',
    '    "Hovedstad",',
    '    "Hovedbosteder",',
    '    "Større bosteder",',
    '    "Bosteder",',
    '    "Mindre bosteder",',
    '    "Mindste bosteder"',
    '  ),',
    '  content = "fb",',
    '  .col_code = TRUE',
    ')',
    "",
    "p1 <- statgl_plot(df1, time, group = fct_inorder(district))",
    "```",
    "",
    '{{< shorty',
    '  title = "Demografisk forsørgerbyrde"',
    '  subtitle = "1977 - 2025"',
    '  description = "Den demografiske forsørgerbyrde beregnes som det matematiske forhold mellem den del af befolkningen, der ikke er tilknyttet arbejdsmarkedet og dermed skal forsørges, og forsørgerne, altså den arbejdsdygtige befolkning."',
    "  plot = '`r p1`'",
    '  link = "[Se tallene i Statistikbanken](https://bank.stat.gl:443/sq/285d1569-0136-49a8-9347-98158fa57300)"',
    '  doc1_title = "Her er en titel"',
    '  doc1_text = "Her kan der stå noget tekst"',
    '  doc2_title = "Her er en anden titel"',
    '  doc2_text = "Her kan der stå noget andet tekst"',
    '>}}',
    ""
  )

  writeLines(qmd_template, con = qmd_file)
}
