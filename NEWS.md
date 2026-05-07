# statgl 0.5.1

## Maintenance

* `DESCRIPTION` now wraps multiple authors in `c()` and lists Emil Malta as
  the sole maintainer (`cre`); Alexander Krabbe is listed as `aut`.
* Added `lifecycle`, `purrr`, `htmlwidgets`, and `janitor` to `Imports`
  (previously used but undeclared).
* Moved `prettydoc`, `magick`, and `rstudioapi` to `Suggests` and added
  `requireNamespace()` guards where they are used.
* Centralised the default PXWeb base URL in `statgl_api_url()`. The value
  can be overridden globally with `options(statgl.api_url = "...")` or
  per-call via the existing `api_url` argument.
* Added a `testthat` scaffold and a basic GitHub Actions `R-CMD-check`
  workflow.
* Documentation: filled in empty `@return` tags for exported functions,
  fixed typos in `statgl_fetch()`'s docs, and wrapped live-API examples
  in `\donttest{}` so `R CMD check` passes on offline build hosts.
* `man/` is stale relative to the current set of exports — run
  `devtools::document()` to regenerate `.Rd` files for `statgl_fetch()`,
  `statgl_table()`, `statgl_crosstable()`, `statgl_plot()`, `px_agg()`,
  `px_all()`, `use_statgl_template()`, `word_to_qmd()`, and
  `word_to_qmd_addin()`.

# statgl 0.5.0.9002

* Word-to-Quarto converter now handles æøå in filenames correctly.

# statgl 0.5.0.9001

* New `word_to_qmd()` (and RStudio addin `word_to_qmd_addin()`) for
  converting `.docx` to `.qmd` via Pandoc, with media extraction, EMF →
  PNG conversion, and YAML front-matter generation.

# statgl 0.5.0.9000

* New `use_statgl_template()` for scaffolding Quarto documents from the
  Statistics Greenland shortcodes extension.
* New `pub_project()` helper that creates a publication folder with a
  Quarto template and `_quarto.yml`.

# statgl 0.5.0

* Added Quarto template for new sites.

# statgl 0.4.2

Iterative improvements over the 0.4.2.9000–0.4.2.9018 development series:

* `statgl_plot()`: enhanced colour handling, last-value labels, refined
  tooltip and legend styling, configurable label outline that adapts to
  light/dark mode, better y-axis defaults.
* `statgl_table()`: row-group support, top and bottom rule helpers, class
  fixes, mobile-responsive column hiding via `.secondary`.
* `statgl_crosstable()`: option to hide column values, multiple stub
  columns and row grouping.
* `statgl_fetch()`: chunking support for queries that exceed the API's
  cell limit; better handling of `.eliminate_rest = FALSE`.
