# SuSoSimulate 0.3.1

## Bug fixes
* PDF input: `NUMERIC: INTEGER` questions were read as decimal (e.g. a region code of 8.88). Options printed at the end of a wrapped line ("... 01 Yes"), page footers, "/" in question text, wrapped conditions and validations, and a categories-only Appendix A are now handled.
* Text answers follow the variable: codes and ids give digit strings, barcodes alphanumeric strings, region/district/EA/site names place names.
* `ConsistsOf()` is supported.

## Speed
* Coverage repair skips outcomes ruled out by validation bounds, stops after `max(20, repair_budget/10)` candidates without progress, and honours the new `suso_mock_config(repair_time = 120)` limit (seconds; also in the app).
* Repair candidates steer the whole chain of questions that enables an uncovered item (long "== 1" gate sequences are now reached in a few candidates instead of never).
* Enabling conditions are re-evaluated only when a variable they reference changes; disabled sections are cleared once. About 25% faster on heavy questionnaires.

## App
* Several files can be uploaded together (document.json + category files).

# SuSoSimulate 0.3.0

## Export layout (breaking)
* Every run writes `<out_dir>/<Q>_SuSoMock_<YYYYMMDD_HHMMSS>/`; `out_dir` (default `"."`) is now the parent folder.
* Archives and folders are named as HQ names them: `<Q>_<version>_STATA_All.zip` and `<Q>_<version>_Paradata_All.zip`, with files at the archive root. `zip = TRUE` is now the default.
* `suso_mock(version = )` and `write_suso_mock(version = )` set the questionnaire version. Default: detected from an HQ export input (`export__info.json` or the file name), otherwise 1.
* The STATA export carries `Questionnaire/content.zip` (document.json + categories) and an HQ-style `export__readme.txt`; the paradata export carries `export__info.json`.

## Paradata
* Aligned with the real HQ 24.x paradata: progressive yes/no `AnswerSet`s, `AnswerRemoved`, `VariableDisabled`, system block (`VariableDisabled` > `QuestionDeclaredValid` > `QuestionDeclaredInvalid` > `VariableSet`) only at the final completion, `Restarted` and supervisor-rejection cycles with HQ roles and time-zone offsets. Pauses no longer precede the interview creation.
* New `suso_mock_config(restart_rate, rejection_rate)`.
* New `suso_read_paradata()`: reads real or mock paradata and parses `parameters`.

## App and wizard
* Questionnaire version input (pre-filled from the uploaded export), "save to folder", and downloads named `<Q>_SuSoMock_<timestamp>.zip`.

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
