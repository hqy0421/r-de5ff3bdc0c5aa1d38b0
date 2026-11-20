# Inspect the receipts of a batch

`web_entry_receipts()` returns the persisted per-row receipts of a batch
without touching the browser, so progress can be inspected before
deciding whether to resume.

## Usage

``` r
web_entry_receipts(batch, state_dir = NULL)
```

## Arguments

- batch:

  A batch built with
  [`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md).

- state_dir:

  Directory holding persisted progress. Defaults to the batch's
  `state_dir`.

## Value

A tibble with one receipt per row; see
[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md).

## See also

[`web_entry_batch()`](https://infer.tidymodels.org/dev/reference/web_entry_batch.md),
[`web_entry_run()`](https://infer.tidymodels.org/dev/reference/web_entry_run.md)
