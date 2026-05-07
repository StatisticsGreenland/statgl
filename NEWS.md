# statgl (development version)

## Bug fixes

* `statgl_url()` no longer constructs malformed search IDs from
  mixed-case input — `statgl_url("bexst1")` now behaves the same as
  `statgl_url("BEXST1")`.

## New behavior

* `statgl_url()` validates its inputs explicitly and reports the URL
  searched when a table is not found.
* The `lang` argument of `statgl_url()` now defaults to `NULL` (was
  previously detected via `missing()`); existing callers that omit
  `lang` are unaffected.

## New features

* `statgl_api_url()` is now exported. It returns the active API base URL
  (which can be overridden globally with
  `options(statgl.api_url = "...")`) and is the default for the
  `api_url` argument of `statgl_search()`, `statgl_url()`, and friends.

## Internal

* New internal helpers `is_greenland_api()` and `infer_lang_from_url()`
  in `R/utils.R` — used by `statgl_url()` and ready for reuse by other
  callers that need to branch on whether they're talking to the
  Statistics Greenland API.

# statgl 0.5.1

## Maintenance

* Added `lifecycle`, `purrr`, `htmlwidgets`, and `janitor` to `Imports`
  (previously used but undeclared).
* Moved `prettydoc`, `magick`, and `rstudioapi` to `Suggests` and added
  `requireNamespace()` guards where they are used.
* Centralised the default PXWeb base URL in `statgl_api_url()`. The value
  can be overridden globally with `options(statgl.api_url = "...")` or
  per-call via the existing `api_url` argument.
* Added a `testthat` scaffold and a basic GitHub Actions `R-CMD-check`
  workflow.

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
