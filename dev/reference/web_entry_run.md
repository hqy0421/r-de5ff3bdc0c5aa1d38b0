# Run a resumable batch of web entries and analyse confirmed observations

`web_entry_run()` enters the observations of a batch built with
[`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md)
into the remote web application and confirms each one by reading it
back. It is safe to call again after an interruption:

- every key is read back *before* anything is saved, so a record that
  was saved by an earlier run (whose local receipt was lost) is adopted
  instead of being entered again;

- a record already holding different values under the same key is
  reported as a conflict and is never overwritten;

- when the outcome of an earlier save is unknown, the record is verified
  rather than resubmitted;

- a save is only treated as confirmed when a readback shows the fields
  agree with the batch; the adapter's return value, including an
  apparent success message, is never treated as confirmation.

Only when *every* row is confirmed by a readback made during this run
are the confirmed observations passed to
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md) and
[`fit()`](https://infer.tidymodels.org/dev/reference/fit.infer.md) with
the batch's original formula. Otherwise, or if the analysis or writing
the analysis fails, the previous usable analysis is kept and reported
instead.

## Usage

``` r
web_entry_run(
  batch,
  browser = NULL,
  max_attempts = 3L,
  state_dir = NULL,
  analyze = TRUE
)

# S3 method for class 'infer_web_entry'
print(x, ...)
```

## Arguments

- batch:

  A batch built with
  [`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md).

- browser:

  A logged-in browser handle. Defaults to the handle supplied to
  [`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md).

- max_attempts:

  The maximum cumulative number of save attempts per row.

- state_dir:

  Directory holding persisted progress. Defaults to the batch's
  `state_dir`.

- analyze:

  Whether to analyse and fit the batch when it is fully confirmed.

- x:

  An object of class `infer_web_entry`, as returned by `web_entry_run()`
  or
  [`web_entry_verify()`](https://infer.tidymodels.org/dev/reference/web_entry_verify.md).

- ...:

  Unused.

## Value

An object of class `infer_web_entry`, a list with components:

- `fingerprint`: the batch fingerprint.

- `receipts`: a tibble with one receipt per row: `key`, `status`,
  `confirmed`, `remote_saved`, `attempts`, `confirmed_at`, `values`, and
  `detail`.

- `complete`: whether every row is confirmed by a readback from this
  run.

- `included`: the observations actually included in the analysis (those
  read back from the remote application), or those of the previous
  usable analysis when no current one is available.

- `coefficients`: the native
  [`fit()`](https://infer.tidymodels.org/dev/reference/fit.infer.md)
  coefficients for the batch formula, for the analysis reported in
  `analysis$status`.

- `analysis`: a list with `status` (`"current"`, `"stale"`, `"failed"`,
  or `"none"`), `reason`, `completed_at`, `persisted`, `previous`, and,
  when a current fit could not be kept, `computed`.

`x`, invisibly.

## See also

[`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md),
[`web_entry_verify()`](https://infer.tidymodels.org/dev/reference/web_entry_verify.md),
[`web_entry_receipts()`](https://infer.tidymodels.org/dev/reference/web_entry_receipts.md)
