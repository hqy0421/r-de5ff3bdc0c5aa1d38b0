# Build a resumable batch of web entries

`web_entry_batch()` binds a data frame of scalar observations to the
configuration used to enter them into a web application. The batch is
passed to
[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md),
which fills and saves the records, reads each one back to confirm it,
and analyses the confirmed observations with
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md) and
[`fit()`](https://infer.tidymodels.org/dev/reference/fit.infer.md) using
the original formula.

A batch is identified by a fingerprint of its data and configuration.
The fingerprint is stored with any progress, so a batch built from
different data or a different configuration never resumes the progress
of another batch.

## Usage

``` r
web_entry_batch(
  data,
  key,
  fields,
  formula,
  browser = NULL,
  form,
  query,
  success = NULL,
  normalize = NULL,
  state_dir = NULL
)
```

## Arguments

- data:

  A data frame of scalar observations. Every column must be a scalar
  atomic vector, and no value of a column referenced by `fields` may be
  missing.

- key:

  A single column name in `data` holding the business key. Keys must be
  unique after normalization: one key describes one remote record.

- fields:

  A named character vector mapping form field names (names) to columns
  of `data` (values).

- formula:

  The analysis formula, in terms of the column names of `data` that the
  batch carries (the `key` column and the columns mapped from `fields`).
  It is passed to
  [`specify()`](https://infer.tidymodels.org/dev/reference/specify.md)
  once the whole batch is confirmed.

- browser:

  A logged-in browser handle supplied by the caller. It is passed
  verbatim to the adapters and is never fingerprinted.

- form:

  The form adapter: either a function that performs the real form entry
  and save, or a list containing such a function under `submit` (or
  separate `fill` and `save` steps) plus optional location
  configuration. It is called as `submit(browser, values, config)`,
  where `values` maps field names to the scalar values to enter and
  `config` contains the business `key`, the location configuration (both
  as `config$locate` and spread into the config), and the field mapping.
  Its return value is a hint only, never confirmation: even an explicit
  success signal is ignored.

- query:

  The query adapter: either a function that reads back the current
  record for a business key, or a list containing such a function under
  `get` (or `read`/`query`) plus optional location configuration. It is
  called as `get(browser, key, config)` and must return a data frame of
  the current record(s) for that key with one column per field name. The
  readback must be strongly consistent with earlier saves, and must
  return zero rows when the key does not exist.

- success:

  A string giving the response level treated as a success by
  [`specify()`](https://infer.tidymodels.org/dev/reference/specify.md),
  if any.

- normalize:

  A function applied to every field value and to the business key before
  they are compared, and to the values of a readback before they are
  compared with the batch. It must return a single non-missing string.
  Defaults to trimming surrounding whitespace and coercing to character.

- state_dir:

  A directory for persisted progress (receipts) and the last usable
  analysis. Defaults to a per-session directory under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). Supply a stable
  directory to carry progress across sessions; because a save is always
  verified by a readback, a fresh session still never enters an
  observation twice even if the directory is gone.

## Value

An object of class `infer_web_entry_batch` for
[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md),
[`web_entry_verify()`](https://infer.tidymodels.org/dev/reference/web_entry_verify.md),
and
[`web_entry_receipts()`](https://infer.tidymodels.org/dev/reference/web_entry_receipts.md).

## See also

[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md),
[`web_entry_verify()`](https://infer.tidymodels.org/dev/reference/web_entry_verify.md),
[`web_entry_receipts()`](https://infer.tidymodels.org/dev/reference/web_entry_receipts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
batch <- web_entry_batch(
  data = trials,
  key = "trial_id",
  fields = c(subject = "subject_id", arm = "treatment", outcome = "score"),
  formula = outcome ~ arm,
  browser = browser,
  form = list(
    open = "/trials/new",
    submit = function(browser, values, config) {
      fill_form(browser, config$open, values)
      click(browser, "#save")
    }
  ),
  query = list(
    open = "/trials/search",
    get = function(browser, key, config) {
      read_trial_record(browser, config$open, key)
    }
  ),
  state_dir = "~/.cache/web-entry"
)

web_entry_run(batch)
} # }
```
