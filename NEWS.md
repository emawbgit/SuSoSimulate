# SuSoSimulate 0.2.0

## New
* **Shiny app** (`suso_app()`), modelled on the DIME iehfc workflow:
  1. Questionnaire. 2. Sample design. 3. Settings. 4. Generate & review (coverage, data browser, paradata, system files). 5. Export.
  * Export downloads: the full output as a zip, settings as JSON (and re-upload), an R script that reproduces the run, and a Stata do-file that loads and merges the output.
  * `suso_app_object()` and `inst/app/app.R` for deployment on Posit Connect or shinyapps.io.
* `suso_mock(progress = )` callback, used by the app's progress bar.
* `suso_mock_config(special_value_rate = )`: numeric special values (e.g. -98 "Don't know") are now generated.
* Clear, early errors for invalid arguments in `suso_design()`, `suso_mock_config()` and `suso_mock()`.

## Bug fixes
* `DateTime.Now` / `DateTime.Today` (and other static members) were never recognised, so they fell back to random outcomes.
* Numeric roster triggers never produced 0, so `n > 0`-type branches were never FALSE. They now honour branch constants. User-set `roster_sizes` always take precedence.
* `sample(x, 1)` on a length-1 numeric `x` draws from `1:x`. Replaced everywhere by a safe helper. This affected tight roster ranges, single in-bound targets and two-option answer corrections in paradata.
* Shortened Stata names could collide (`var__1` …) and fail `haven::write_dta()`. Names are now shortened with collision-safe suffixes that still fit in 32 characters.
* `roster_sizes = list(x = 5)` (a single fixed size) is now honoured.
* Factor columns in a user `frame` were preloaded as level indices instead of values.
* `zip = TRUE` needed an external `zip` program, which is often missing on Windows. Zipping now uses the `zip` package.
* `paradata.tab` is now written exactly as HQ does it: UTF-8 with BOM and CRLF line endings, encoding-safe on Windows.
* Non-ASCII characters removed from the package sources (they broke the app under non-UTF-8 locales).

## Tests
* New edge-case fixture questionnaire covering:
  * DateTime arithmetic, `??`, string methods;
  * a yes/no-triggered roster, a fixed roster with per-row conditions, 3-level nesting;
  * shared-trigger rosters, an option filter, a linked multi-select to a question in another roster;
  * circular conditions, a lookup table, broken syntax;
  * missing reusable categories, special values, over-long names, and the barcode, picture, audio and area question types.
* Referential-integrity checker across all roster files and paradata. Argument-validation tests. Shiny server tests. A test that runs the generated R template code. 111 expectations in total.

# SuSoSimulate 0.1.0
* First release.
