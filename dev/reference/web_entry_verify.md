# Read a batch back without saving anything

`web_entry_verify()` reads every row of a batch back from the remote
application and refreshes the persisted receipts. It never fills a form
or saves a record, so it is safe for investigating an interrupted run. A
row is confirmed only when the record currently read back agrees with
the batch.

## Usage

``` r
web_entry_verify(batch, browser = NULL, state_dir = NULL)
```

## Arguments

- batch:

  A batch built with
  [`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md).

- browser:

  A logged-in browser handle. Defaults to the handle supplied to
  [`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md).

- state_dir:

  Directory holding persisted progress. Defaults to the batch's
  `state_dir`.

## Value

An object of class `infer_web_entry` without an analysis; see
[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md)
for its components.

## See also

[`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md),
[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md),
[`web_entry_receipts()`](https://infer.tidymodels.org/dev/reference/web_entry_receipts.md)
