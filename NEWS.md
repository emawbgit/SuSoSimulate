# SuSoSimulate 0.6.0

Learned from a real HQ 26.07 export with lookup tables (614 questions, 14 roster levels).

## Questionnaire logic
* Lookup tables: read from `Lookup Tables/*.txt`, used as `table[rowcode].column`, `ContainsKey`, `Count`, `Keys`, LINQ over the rows; re-exported in `Questionnaire/content.zip`.
* HQ 26.x `document.json` (`"$type"` entries in the macro and lookup-table dictionaries) no longer stops the reader.
* Comma-list macros (`$codes = 1,2,6,9` in `InList($codes)`) are substituted as text, as SuSo does.
* Numeric ids used by an option filter to select a slice of a list (`household_id` picking its members from a preloaded name list) are drawn from the list's parent codes.
* Gatekeeper single-selects (household found, consent) take the answer that opens the questionnaire 85% of the time.

## Coverage
* Conditions on calculated variables set to a constant, items inside a module switched off that way, and `x == c` on a question never enabled are marked `unreachable` and skipped by the repair; `print()` counts them separately.

## Export (as in the 26.07 export)
* Multi-select combo boxes: `var__0..var__{n-1}` holding the selected codes (n = MaxAllowedAnswers, else the options up to 200), value-labelled.
* Text-list columns labelled `label:k` / `label:kc`; multi-select columns always `label:Option`.
* `& < >` in Stata variable labels written as HTML entities; duplicate value-label texts kept (no `.1` suffixes).
* Assignment action codes 10-13 (`TargetAreaChanged`, `Completed`, `Closed`, `Reopened`).

# SuSoSimulate 0.5.1

* Paradata checked against a real 26.07 export: `InterviewCreated` now carries the assignment id; geography (`lon,lat;…`), linked single/multi and yes/no values already matched.
* README: limitations trimmed.

# SuSoSimulate 0.5.0

## Export
* Export folder named after the format instead of a timestamp: `<Q>_<version>_SuSoMock_STATA` or `<Q>_<version>_SuSoMock_Tabular` (also the app download). An existing folder is never overwritten (`_2`, `_3`, ...). `write_suso_mock()` loses its `timestamp` argument.
* Readme, `.do` and template files are written as UTF-8 with LF, as HQ does, whatever the session encoding; CSV reports as UTF-8.

## Checked against the Survey Solutions documentation
* Geography answers: `suso_mock_config(geo_format = "geometry_string" | "wkt" | "geojson")`; WKT/GeoJSON polygons are closed; `.Area` / `.Length` are null for geometry types they do not apply to.
* GPS `__Timestamp` values are UTC (timestamp questions stay in device time).
* DateTime calculated variables appear in paradata as `M/D/YYYY h:mm:ss AM|PM`.
* `assignments.tab` has no `assignment__id` column, so it can be uploaded to HQ as is.

## Languages and special characters
* New sanitising layer (`R/text.R`): every input string (questionnaire JSON, category files, frames, settings, GeoJSON, wizard answers, user generators) is valid UTF-8; Latin-1 / Windows-1252 files (e.g. CSV saved by Excel) are converted, control characters dropped. Tables, labels and paradata are checked again before writing.
* Keyword heuristics (phone, code, place, name, age, year, money, counts, interviewer question) work on accent-folded text in English, French, Spanish and Portuguese.
* Text answers use names, places and words in the questionnaire's language (English, French or Spanish, detected).
* ASCII-safe file names and interviewer logins (`José Peña` -> `JosePena`); numbers always use `.` whatever `options(OutDec)`; readme dates in English whatever `LC_TIME`.
* An output folder the session's locale cannot represent is reported before generation, with the cause.
* A failing user generator leaves the answer unanswered instead of stopping the run.
* New French/Spanish test fixture; the test suite passes in UTF-8 and C locales.

## Progress
* Console and app show elapsed time and an estimate of the time left during generation, and the most time left during coverage repair.

## Other
* No bslib deprecation warning: `page_navbar()` uses `navbar_options()` on bslib >= 0.9.
* License changed to MIT. `citation("SuSoSimulate")`.

# SuSoSimulate 0.4.0

## Export
* **Tabular format**: `suso_mock(format = "tabular")` (and in the app/wizard) writes `<Q>_<v>_Tabular_All` with a `.tab` and a `.do` per file, as HQ does: UTF-8 BOM, CRLF, `-999999999` / `##N/A##` for enabled-but-unanswered, `.do` labels with the full question text and `text:option` for multi-select/list columns, reusable categories under their name. Matches a real HQ 24.06 Tabular export line by line.
* Export folder named with the version: `<Q>_<version>_SuSoMock_<timestamp>` (also the app download).
* Stata: enabled-but-unanswered numerics are `.a`, as in HQ (were `.`). Value labels keep their text verbatim (HTML-escaped, whitespace kept, also from category `.xlsx`).
* Geography (area) questions: polygon/polyline/point answers near the dwelling with `__area`, `__len`, `__num`, `__racc`, `__rfrq`; `self.Area`, `.Length`, `.NumberOfPoints` in expressions.
* `interview__errors` / `interview__comments` carry one `idN` column per roster nesting level; warnings are no longer listed as errors (`has__errors` now equals the errors in `interview__errors`).
* `assignments.tab` (HQ preload format) next to the export.

## Consistency
* New `suso_validate_mock()`: cross-checks ids, statuses, error and comment counts, assignments, interviewers, preload and paradata across all files.
* Interviewer question (e.g. a single-select of enumerator names) detected or set with `suso_design(interviewer_var = )`; its option labels are the interviewer logins in the data, paradata, actions and assignments.
* Cover (identifying) questions and the interviewer question come first in the main file (`suso_mock_config(cover_first = TRUE)`; `FALSE` keeps HQ's order).

## Sampling
* `suso_design(boundary = )`: survey area as GeoJSON (file/string) or lon-lat matrix; PSU centroids are drawn inside it. In the app: draw it on OpenStreetMap or upload a GeoJSON.

## Expressions
* `new decimal?[]{...}` (nullable typed arrays), `Quest.IRnd()` (= `sssys_irnd`), `.RosterVector`.

## App
* Stata/Tabular choice; questionnaire upload limited to HQ/Designer zips and JSON (PDF removed from the app; still readable in R).
* The total number of interviews always reflects the current inputs (empty fields included), and results generated with older settings are flagged.

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
