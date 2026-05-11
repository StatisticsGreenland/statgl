# statgl (development version)

## Bug fixes

* `statgl_url()` no longer constructs malformed search IDs from
  mixed-case input — `statgl_url("bexst1")` now behaves the same as
  `statgl_url("BEXST1")`.

## New features

* `statgl_api_url()` is now exported. It returns the active API base URL
  (which can be overridden globally with
  `options(statgl.api_url = "...")`) and is the default for the
  `api_url` argument of `statgl_search()`, `statgl_url()`, and friends.
* `statgl_fetch()` now auto-wraps a length-1 character scalar containing
  `*` or `?` as `px_all()`. Both `gender = "*"` and `time = "*0"` now
  work without explicit `px_all()` calls — multi-element character
  vectors are still treated as item filters. Explicit `px_all()` /
  `px_top()` / `px_agg()` calls are unaffected.
* New `top()` exported as a short alias for `px_top()`. Lets you write
  `statgl_fetch("BEXSTA", time = top(3))` instead of
  `statgl_fetch("BEXSTA", time = px_top(3))`. `px_top()` is unchanged
  and remains the canonical name; both are documented on the same Rd
  page.
* `statgl_fetch()` gains three new arguments:
  - `.lang` — override the API language directly. When `x` is a table
    ID, forwarded to `statgl_url()`; when `x` is a URL, the
    `/v<N>/<lang>/` segment is rewritten in place. Takes precedence
    over the language implied by the table ID.
  - `.api_url` — override the PXWeb base URL when `x` is a table ID.
    Defaults to `statgl_api_url()`.
  - `.dry_run` — return the resolved URL, parsed selections, and JSON
    body that would be POSTed, without sending the request. Useful for
    debugging selection issues.

## Behavior changes

* `statgl_url()` validates its inputs explicitly and reports the URL
  searched when a table is not found.
* The `lang` argument of `statgl_url()` now defaults to `NULL` (was
  previously detected via `missing()`); existing callers that omit
  `lang` are unaffected.
* `statgl_fetch()` now produces a single, cleaner error on non-403 HTTP
  failures (status, response body, and a `statgl_meta()` hint folded
  into one `stop()`); previously it printed several lines via `cat()`
  before erroring.

## Deprecations and removals

* The `url` argument of `statgl_fetch()` (deprecated since 0.2.0) now
  errors via `lifecycle::deprecate_stop()` instead of warning. Use `x`
  instead. The argument will be removed entirely in a future release.
* The `.chunk_size` argument of `statgl_fetch()` was a non-functional
  placeholder and has been removed. Chunking continues to happen
  automatically when a query exceeds the API's cell limit.
* `statgl_crosstable()`'s `.secondary` argument is renamed to `.drop`
  to clarify that it filters data (not styling). `.secondary` still
  works for now but emits a `lifecycle::deprecate_warn`. If both are
  supplied, `.drop` wins.

## Behavior changes (continued)

* `statgl_crosstable()`'s value-based column filter (`.drop`) now
  warns clearly when the input is malformed (not a named list), names
  a dimension that isn't a column group, or supplies values that match
  no rows. Previously these silently produced an unfiltered table —
  the source of recurring "`.secondary` doesn't work" reports.
* `statgl_crosstable()` now errors up-front if the column referenced by
  `.value` doesn't exist in `df`, listing the available columns. The
  default `.value = value` matches the convention from
  `statgl_fetch()`, but data frames without a `value` column
  (e.g. `ggplot2::mpg`) previously produced a confusing dplyr-internal
  error about quosures.
* `statgl_table()` and `statgl_crosstable()` now share the same shape
  for `.replace_0s` and `.replace_nas`:
  - `.replace_0s`: `FALSE` (default; no replacement), `TRUE` (replace
    with en-dash), or a single string used as a custom replacement.
  - `.replace_nas`: `NULL` (default; no replacement) or a single
    string used as a custom replacement (e.g. `".."`).
* `.replace_0s = TRUE` in `statgl_table()` now produces an en-dash
  (was `"[-]{}"`). To keep the Statistics Greenland Quarto-shortcode
  behavior, pass `.replace_0s = "[-]{}"` explicitly.
* `.replace_nas` default in `statgl_crosstable()` changed from
  `FALSE` to `NULL`. `FALSE` is still accepted as a no-op alias
  (silent), and `.replace_nas = TRUE` is now deprecated with a
  warning — pass `.replace_nas = "."` to keep the previous behavior.

## Documentation

* `statgl_fetch()`'s docs now describe `x` as accepting either a table
  ID or a URL, and document the previously-undocumented behavior of
  `.col_code` and `.val_code` accepting a character vector of column
  names (not just a logical).

## Internal

* New internal helpers `is_greenland_api()` and `infer_lang_from_url()`
  in `R/utils.R` — used by `statgl_url()` and ready for reuse by other
  callers that need to branch on whether they're talking to the
  Statistics Greenland API.
* `R/statgl_fetch.R` cleaned up: removed ~350 lines of unreachable
  chunking helpers (`get_api_limits`, `estimate_query_size`,
  `get_selection_cardinality`, `glob2rx`, `calculate_chunk_size`,
  `fetch_with_chunking`, `fetch_large_dataset`, `find_time_variable`,
  `get_variable_values`); extracted `is_eliminable()`,
  `expand_eliminables()`, `resolve_url()`, `fetch_jsonstat()`,
  `pick_chunk_variable()`, and `try_chunk()` so behavior lives in one
  testable place each; collapsed `build_query()`'s filter detection to
  a single `attr(...) %||% "item"`; threaded the `meta` list through
  to `chunk_large_query()` so we no longer make two `statgl_meta()`
  round-trips when both `.eliminate_rest = FALSE` and chunking
  trigger.
* `chunk_large_query()` no longer POSTs the first chunk twice. It now
  probes with one request and reuses that result on success; on 403 it
  halves the chunk size and retries from the top, surfacing other HTTP
  errors directly instead of treating them as "still too large."
* Dropped `purrr` from `Imports` — its only use (`purrr::map_dfr` in
  the unreachable `fetch_with_chunking()`) went away with the dead-code
  cleanup.

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
