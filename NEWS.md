# statgl 0.5.2

This release is a substantial refactor of `statgl_table()` and
`statgl_crosstable()`: their argument surfaces are now largely shared
(`.hide_mobile`, `.caption`, `.bottom_rule`, `.bold_rows`, `.digits`
are available on both), they return an `htmlwidget` by default for
first-class RStudio Viewer / knitr / Quarto-shortcode integration,
`statgl_crosstable()` emits nested column headers when 3+ grouping
variables are passed (instead of dash-uniting them into a single
label row), and `.value` defaults to an auto-detect that falls back
to `dplyr::n()` row counts when the data frame has no `value` column.
The old behaviors are preserved on the deprecation-warning path or as
explicit opt-ins.

## Bug fixes

* `statgl_url()` no longer constructs malformed search IDs from
  mixed-case input — `statgl_url("bexst1")` now behaves the same as
  `statgl_url("BEXST1")`.

## New features

* `statgl_crosstable()` now produces **nested column headers** when
  3+ grouping variables are passed. Previously the second-and-later
  group values were dash-united into a single label row (e.g.
  `"Men – Born outside Greenland"`); now each non-leaf grouping
  level becomes its own spanning header row via
  `kableExtra::add_header_above()`, with the leaf grouping variable's
  values as the `col.names`. The two-variable case is unchanged.
* `statgl_table()` and `statgl_crosstable()` now share a common set of
  styling arguments — features that previously lived on only one of
  the two functions are available on both:
  - `.caption` (was crosstable-only) now adds a table caption to
    `statgl_table()` as well, passed through to `kableExtra::kable()`.
  - `.bottom_rule` (was table-only) now draws the thick bottom border
    on the last data row of a `statgl_crosstable()` too. Default
    `TRUE` in both functions, matching the Statistics Greenland design
    convention.
  - `.bold_rows` (was crosstable-only) now works in `statgl_table()`.
    Same shape: integer positions, a character vector matching values
    in the first column, or a one-sided formula evaluated on the
    formatted data frame (e.g. `~ year == max(year)`).
  - `.digits` (was table-only) now exists on `statgl_crosstable()`.
    Default is `NULL` (no rounding — preserves existing PXWeb output);
    set explicitly to control precision via `format()`. The asymmetric
    default with `statgl_table()`'s `.digits = 3` is deliberate.
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
* `statgl_table()` and `statgl_crosstable()` now return an
  `htmlwidget` by default, mirroring the return type of
  `statgl_plot()`. The widget wraps the pre-rendered table HTML
  (kable + any `.hide_mobile` `<style>` block) in a trivial JS shim
  that just sets `innerHTML` on its container `<div>`. The benefit is
  integration parity with `statgl_plot()`: the same return value
  opens in the RStudio Viewer interactively, renders in knitr /
  Quarto chunks, and substitutes cleanly into Quarto **shortcode
  parameters** (e.g. `` `r t` `` inside `{{< … >}}`) the way other
  widgets do. The widget bundles Bootstrap 5.1.3 as an HTML
  dependency so the kableExtra bootstrap classes (`table`,
  `table-striped`, …) style correctly when it renders standalone
  (e.g. the RStudio Viewer); Quarto / R Markdown documents that
  already bundle Bootstrap will dedupe by name + version.
  `.as_html = TRUE` is retained as an escape hatch and still returns
  a plain character vector with no class — for callers that need to
  bypass the widget entirely (e.g. older Quarto extensions that
  can't parse widget output).
* `statgl_table()` and `statgl_crosstable()` gain a `.hide_mobile`
  argument for CSS-based responsive hiding of columns at small
  viewports (`max-width: 768px`). The columns remain in the rendered
  HTML; they're just visually hidden on narrow screens.
  - In `statgl_table()`, `.hide_mobile` is a tidyselect expression
    (e.g. `.hide_mobile = c(ialt, model)`) and replaces the old,
    same-shape `.secondary` argument.
  - In `statgl_crosstable()`, `.hide_mobile` is a named list of
    `column_group = values_to_hide` (same shape as `.drop`), so
    `.hide_mobile = list(gender = "Total")` hides every `Total` column
    on mobile while still rendering them on desktop. Validation
    mirrors `.drop` (warns on bad shape, unknown dimension, zero-match
    values).
  - The CSS is embedded into the returned table's underlying HTML, so
    it travels with the table whether you take the default kable
    return or pass `.as_html = TRUE` for shortcode parameters.
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
* `statgl_plot()` gains a `pyramid` argument for population-pyramid
  layouts on two-group charts. Accepts:
  - `TRUE` — auto-detect the two sides via a male-label heuristic
    (`M` / `Mænd` / `Men` / `Angutit` / ...) and a female-label
    fallback, putting men on the left (the dominant international
    convention used by the US Census, UN, and most demographic
    textbooks).
  - A single string like `"M"` — treated as the men/left value; the
    other side is inferred from the data (the single remaining value,
    or the one matching the female-label heuristic when more than one
    remains).
  - A length-2 character vector `c(left, right)` like `c("M", "K")` —
    sets both sides explicitly.

  When `pyramid` is a string or length-2 vector, rows whose `group`
  value isn't one of the named levels are silently dropped — useful
  for PXWeb tables that include an `"I alt"` / `"T"` total alongside
  the two sex codes. Series and legend order are then locked to
  pyramid order so the legend reads left → right with men on the
  left. Pyramid composes with `type`: defaults to `"bar"` when
  `type` is not supplied (so an integer age column doesn't trigger
  line inference); for `"bar"` and `"column"`, `stacking` defaults to
  `"normal"` so the two sides share the zero baseline. The chart
  always renders horizontally — Highcharts auto-inverts `"bar"`;
  other types get `chart.inverted = TRUE`.
* `statgl_plot()` gains a `highlight` argument for visually
  emphasising specific labels. Matching elements are drawn in the
  Statgl accent orange (`#faa41a`); everything else is neutral grey
  (`#d3d3d3`). Dispatch follows chart shape:
  - **Grouped chart** (`group =` supplied): matches against series
    names. Line/area types additionally get a thicker stroke
    (`lineWidth = 3`) and a higher `zIndex` so the highlighted
    series sits in the foreground.
  - **Ungrouped bar / column**: matches against the `x` values,
    re-colouring individual bars. Useful for emphasising one
    district, commodity, etc. on a per-bar chart without faking a
    `group =`.
  - **Anything else ungrouped** (line, scatter, ...): no-op with a
    warning, since there's nothing series- or bar-shaped to single
    out.

  Overrides `palette` when set, so the default `palette = "main"`
  doesn't fight with it. Example uses:
  `statgl_plot(df, bydel, highlight = "Nuuk")` highlights the Nuuk
  bar; `statgl_plot(df, time, value, group = commodity,
  highlight = "I alt")` highlights the totals line.
* `statgl_plot()` gains a `legend_position` argument. One of
  `"top"`, `"bottom"` (default, matching the prior Highcharts
  layout), `"left"`, `"right"`. Any other value — `"none"`, `NULL`,
  `FALSE`, etc. — hides the legend entirely.
* `statgl_plot()` now forwards `...` to
  `highcharter::hchart()`. Names that `statgl_plot()` already sets
  itself (`object`, `type`, `mapping`, `name`) are dropped so the
  wrapper's own values win; everything else reaches `hchart()`.
  Useful as an escape hatch for one-off `hchart()`-level options
  without giving up the Statgl defaults.
* `statgl_plot()` now warns when the input `y` column contains
  negative values. The check runs on the original column before
  any `pyramid` mirroring, so pyramid charts don't trip it. Only
  fires when `y` resolves to a bare column name — expression
  `y`'s like `y = log(value)` are left alone.

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
* `statgl_plot()`'s `show_last_value` default changed from `TRUE` to
  `FALSE`. The end-of-line value labels are still useful on a single
  line / area series, but were visually noisy on grouped charts and
  fired on every bar of bar/column charts. Pass `show_last_value =
  TRUE` on a per-call basis to restore the old behavior.
* `statgl_crosstable()` with a **single** grouping variable no longer
  emits a redundant spanning header row above the column names — the
  spanning row used to duplicate the `col.names`. The 2-variable
  layout is unchanged.
* `statgl_crosstable()`'s value-based column filter (`.drop`) now
  warns clearly when the input is malformed (not a named list), names
  a dimension that isn't a column group, or supplies values that match
  no rows. Previously these silently produced an unfiltered table —
  the source of recurring "`.secondary` doesn't work" reports.
* `statgl_crosstable()`'s `.value` argument is now resolved more
  carefully and its default changes from `value` to `NULL`:
  - `NULL` (the new default) means **auto**: use a `value` column if
    `df` has one (the convention produced by `statgl_fetch()`),
    otherwise fall back to row counts via `dplyr::n()` and emit a
    message nudging users toward `summarise()` for other statistics.
    This means `statgl_crosstable(ggplot2::mpg, manufacturer)` now
    produces a count crosstab out of the box.
  - Explicit `.value` is resolved through `tidyselect::eval_select()`
    and must yield exactly one existing column. Typos and stray
    multi-column expressions surface as clear errors (`Column \`X\` not
    found …` / `\`.value\` must select exactly one column; got: …`)
    rather than the confusing dplyr-internal quosure error users hit
    on `ggplot2::mpg` before.
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
* `statgl_table()`'s `.secondary` argument is renamed to `.hide_mobile`
  to clarify that it controls CSS responsive hiding (not styling or
  filtering). `.secondary` still works for now but emits a
  `lifecycle::deprecate_warn`. If both are supplied, `.hide_mobile`
  wins.

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
