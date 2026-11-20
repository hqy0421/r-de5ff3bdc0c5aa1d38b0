# Resumable batch web entry ---------------------------------------------------
#
# `web_entry_batch()` binds a table of scalar observations to the configuration
# used to enter them into a web application. `web_entry_run()` enters the
# records, confirms every one of them by reading it back, and---only when the
# whole batch is confirmed by a readback made in the same run---analyses the
# confirmed observations with `specify()` and `fit()` using the original
# formula.

web_entry_or <- function(x, y) {
  if (is.null(x)) y else x
}

# Canonical representation of configuration used for fingerprinting. Functions
# are rendered as their formals and body: this is stable across sessions and
# does not depend on the environment a closure happens to capture (which may
# contain runtime handles that change between runs).
web_entry_canon <- function(x) {
  if (is.function(x)) {
    return(list(
      kind = "function",
      formals = deparse(formals(x), width.cutoff = 500L),
      body = deparse(body(x), width.cutoff = 500L)
    ))
  }

  if (is.environment(x)) {
    web_entry_call_error(
      c(
        "Configuration cannot contain environments.",
        i = "Pass the browser handle to `web_entry_run()`, not as part of `form` or `query`."
      ),
      class = "infer_web_entry_bad_argument"
    )
  }

  if (is.list(x)) {
    return(list(
      kind = "list",
      names = names(x),
      items = lapply(seq_along(x), function(i) web_entry_canon(x[[i]]))
    ))
  }

  if (is.atomic(x) || is.null(x)) {
    return(list(kind = "atomic", value = x))
  }

  list(
    kind = "other",
    text = paste(deparse(x, width.cutoff = 500L), collapse = "\n")
  )
}

# Normalize a form/query specification into `list(locate, fn, action)`. A bare
# function is accepted as the action itself; a list must contain an action
# function. A form may supply separate `fill` and `save` functions, which are
# called in that order.
web_entry_adapter <- function(
  x,
  actions,
  name,
  call = rlang::caller_env()
) {
  if (is.function(x)) {
    return(list(locate = NULL, fn = x, action = NULL))
  }

  if (!is.list(x)) {
    web_entry_call_error(
      "`{name}` must be a function or a list containing one of {.field {actions}}.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  present <- intersect(actions, names(x))
  present <- present[vapply(x[present], rlang::is_function, logical(1))]

  if (length(present) == 0L) {
    web_entry_call_error(
      "`{name}` must contain one of {.field {actions}} as a function.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  fn <- web_entry_compose_actions(x[present], present)
  action <- paste(present, collapse = "+")

  list(
    locate = x[setdiff(names(x), present)],
    fn = fn,
    action = action
  )
}

# Combine a form's `fill` and `save` steps into one action when no single
# `submit` action is supplied.
web_entry_compose_actions <- function(fns, present) {
  steps <- c("fill", "save")
  steps <- steps[steps %in% present]

  if ("submit" %in% present) {
    return(fns[["submit"]])
  }

  if (length(steps) == 0L) {
    steps <- present[1]
  }

  if (length(steps) == 1L) {
    return(fns[[steps]])
  }

  if (length(steps) == 2L) {
    fill <- fns[["fill"]]
    save <- fns[["save"]]

    return(function(browser, values, config) {
      fill(browser, values, config)
      save(browser, values, config)
    })
  }

  # A query adapter only has one known action.
  fns[[present[[1]]]]
}

# Location configuration of an adapter, both as a nested list (`config$locate`)
# and spread into the config (`config$open`, `config$save`, ...) so adapters can
# use whichever is convenient.
web_entry_adapter_config <- function(
  adapter,
  key,
  fields,
  field_names,
  values = NULL
) {
  locate <- adapter$locate
  if (is.null(locate)) {
    locate <- list()
  }

  config <- c(
    list(
      key = key,
      locate = locate,
      fields = fields,
      field_names = field_names
    ),
    locate
  )

  if (!is.null(values)) {
    config$values <- values
  }

  config
}

# Call an adapter, supplying only as many documented arguments as it declares.
# Adapters are documented as `(browser, values, config)` / `(browser, key,
# config)`, but shorter ones are convenient for fakes and are supported.
web_entry_call_adapter <- function(fn, args) {
  fmls <- formals(fn)

  if (!is.null(fmls) && !any(vapply(fmls, identical, logical(1), quote(...)))) {
    n <- length(fmls)
    if (n < length(args)) {
      args <- args[seq_len(n)]
    }
  }

  do.call(fn, args)
}

#' Build a resumable batch of web entries
#'
#' @description
#'
#' `web_entry_batch()` binds a data frame of scalar observations to the
#' configuration used to enter them into a web application. The batch is passed
#' to [web_entry_run()], which fills and saves the records, reads each one back
#' to confirm it, and analyses the confirmed observations with [specify()] and
#' [`fit()`][fit.infer()] using the original formula.
#'
#' A batch is identified by a fingerprint of its data and configuration. The
#' fingerprint is stored with any progress, so a batch built from different
#' data or a different configuration never resumes the progress of another
#' batch.
#'
#' @param data A data frame of scalar observations. Every column must be a
#'   scalar atomic vector, and no value of a column referenced by `fields` may
#'   be missing.
#' @param key A single column name in `data` holding the business key. Keys must
#'   be unique after normalization: one key describes one remote record.
#' @param fields A named character vector mapping form field names (names) to
#'   columns of `data` (values).
#' @param formula The analysis formula, in terms of the column names of `data`
#'   that the batch carries (the `key` column and the columns mapped from
#'   `fields`). It is passed to [specify()] once the whole batch is confirmed.
#' @param browser A logged-in browser handle supplied by the caller. It is
#'   passed verbatim to the adapters and is never fingerprinted.
#' @param form The form adapter: either a function that performs the real form
#'   entry and save, or a list containing such a function under `submit` (or
#'   separate `fill` and `save` steps) plus optional location configuration. It
#'   is called as `submit(browser, values, config)`, where `values` maps field
#'   names to the scalar values to enter and `config` contains the business
#'   `key`, the location configuration (both as `config$locate` and spread into
#'   the config), and the field mapping. Its return value is a hint only, never
#'   confirmation: even an explicit success signal is ignored.
#' @param query The query adapter: either a function that reads back the current
#'   record for a business key, or a list containing such a function under `get`
#'   (or `read`/`query`) plus optional location configuration. It is called as
#'   `get(browser, key, config)` and must return a data frame of the current
#'   record(s) for that key with one column per field name. The readback must be
#'   strongly consistent with earlier saves, and must return zero rows when the
#'   key does not exist.
#' @param success A string giving the response level treated as a success by
#'   [specify()], if any.
#' @param normalize A function applied to every field value and to the business
#'   key before they are compared, and to the values of a readback before they
#'   are compared with the batch. It must return a single non-missing string.
#'   Defaults to trimming surrounding whitespace and coercing to character.
#' @param state_dir A directory for persisted progress (receipts) and the last
#'   usable analysis. Defaults to a per-session directory under [tempdir()].
#'   Supply a stable directory to carry progress across sessions; because a
#'   save is always verified by a readback, a fresh session still never enters
#'   an observation twice even if the directory is gone.
#'
#' @return An object of class `infer_web_entry_batch` for [web_entry_run()],
#'   [web_entry_verify()], and [web_entry_receipts()].
#'
#' @seealso [web_entry_run()], [web_entry_verify()], [web_entry_receipts()]
#'
#' @examples
#' \dontrun{
#' batch <- web_entry_batch(
#'   data = trials,
#'   key = "trial_id",
#'   fields = c(subject = "subject_id", arm = "treatment", outcome = "score"),
#'   formula = outcome ~ arm,
#'   browser = browser,
#'   form = list(
#'     open = "/trials/new",
#'     submit = function(browser, values, config) {
#'       fill_form(browser, config$open, values)
#'       click(browser, "#save")
#'     }
#'   ),
#'   query = list(
#'     open = "/trials/search",
#'     get = function(browser, key, config) {
#'       read_trial_record(browser, config$open, key)
#'     }
#'   ),
#'   state_dir = "~/.cache/web-entry"
#' )
#'
#' web_entry_run(batch)
#' }
#' @export
web_entry_batch <- function(
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
) {
  data <- web_entry_check_data(data)
  fields <- web_entry_check_fields(fields, data)
  success <- web_entry_check_success(success)
  normalize <- web_entry_check_normalize(normalize, data, fields)
  key_norm <- web_entry_check_key(key, data, normalize)

  analysis_columns <- unique(c(key, unname(fields)))
  formula <- web_entry_check_formula(formula, analysis_columns)

  form_adapter <- web_entry_adapter(
    form,
    c("submit", "fill", "save"),
    "form"
  )
  query_adapter <- web_entry_adapter(
    query,
    c("get", "read", "query"),
    "query"
  )

  state_dir <- web_entry_or(
    state_dir,
    file.path(tempdir(), "infer-web-entry")
  )

  if (!is.character(state_dir) || length(state_dir) != 1L || is.na(state_dir) ||
      !nzchar(state_dir)) {
    web_entry_call_error(
      "`state_dir` must be a single non-missing path.",
      class = "infer_web_entry_bad_argument"
    )
  }

  state_dir <- path.expand(state_dir)

  fingerprint <- web_entry_hash(list(
    version = web_entry_state_version(),
    data = web_entry_fingerprint_data(data),
    key = key,
    key_values = key_norm,
    fields = unname(fields),
    field_names = names(fields),
    formula = paste(deparse(formula, width.cutoff = 500L), collapse = "\n"),
    success = success,
    normalize = web_entry_canon(normalize),
    form = web_entry_canon(form_adapter$locate),
    form_action = form_adapter$action,
    form_fn = web_entry_canon(form_adapter$fn),
    query = web_entry_canon(query_adapter$locate),
    query_action = query_adapter$action,
    query_fn = web_entry_canon(query_adapter$fn)
  ))

  batch <- list(
    data = data,
    key = key,
    key_values = key_norm,
    fields = fields,
    field_names = names(fields),
    formula = formula,
    success = success,
    normalize = normalize,
    browser = browser,
    form = form_adapter,
    query = query_adapter,
    state_dir = state_dir,
    fingerprint = fingerprint
  )

  structure(batch, class = c("infer_web_entry_batch", "list"))
}

# Read one business key back from the remote application and interpret the
# answer against the batch's expected values.
web_entry_query_row <- function(batch, browser, key) {
  expected <- web_entry_expected_for_key(batch, key)

  config <- web_entry_adapter_config(
    batch$query,
    key = key,
    fields = batch$fields,
    field_names = batch$field_names
  )

  value <- tryCatch(
    web_entry_call_adapter(
      batch$query$fn,
      list(browser = browser, key = key, config = config)
    ),
    error = function(e) e
  )

  if (inherits(value, "error")) {
    return(list(
      state = "unknown",
      detail = paste0(
        "The readback for this business key failed: ",
        conditionMessage(value)
      )
    ))
  }

  web_entry_interpret_query(value, expected, batch$normalize)
}

web_entry_expected_for_key <- function(batch, key) {
  i <- web_entry_row_index(batch, key)
  web_entry_row_expected(batch$data, batch$fields, i, batch$normalize)
}

# Perform the real form entry and save for one business key. The result is
# deliberately reduced to success/error; it is never treated as confirmation.
web_entry_submit_row <- function(batch, browser, key) {
  i <- web_entry_row_index(batch, key)
  values <- web_entry_row_values(batch$data, batch$fields, i)

  config <- web_entry_adapter_config(
    batch$form,
    key = key,
    fields = batch$fields,
    field_names = batch$field_names,
    values = values
  )

  tryCatch(
    {
      web_entry_call_adapter(
        batch$form$fn,
        list(browser = browser, values = values, config = config)
      )
      list(error = NULL)
    },
    error = function(e) list(error = e)
  )
}

# Persist the state, warning (rather than failing) if it cannot be written. A
# lost local receipt is recoverable: the next run reads the remote record
# before saving anything.
web_entry_persist <- function(batch, state, state_dir) {
  tryCatch(
    {
      web_entry_state_save(batch, state, state_dir)
      TRUE
    },
    error = function(e) {
      cli::cli_warn(
        c(
          "Could not persist web entry progress.",
          x = conditionMessage(e),
          i = "The remote application may already hold the record. A later run reads the current record back before saving, so nothing will be entered twice."
        ),
        class = "infer_web_entry_persist_warning"
      )
      FALSE
    }
  )
}

web_entry_row_index <- function(batch, key) {
  which(batch$key_values == key)
}

web_entry_run_state_dir <- function(state_dir, default) {
  state_dir <- web_entry_or(state_dir, default)

  if (!is.character(state_dir) || length(state_dir) != 1L || is.na(state_dir) ||
      !nzchar(state_dir)) {
    web_entry_call_error(
      "`state_dir` must be a single non-missing path.",
      class = "infer_web_entry_bad_argument"
    )
  }

  path.expand(state_dir)
}

# Advance one row. Order matters:
#   1. Read back first, so a save whose receipt was lost is adopted instead of
#      being repeated.
#   2. Never save over an existing record with different values.
#   3. Only save after the readback shows there is no record for the key.
web_entry_advance_row <- function(
  batch,
  state,
  browser,
  key,
  attempts_left,
  max_attempts,
  state_dir
) {
  row <- state$rows[[key]]

  # Return the updated row together with the updated state so the caller can
  # thread it through without relying on closures (whose mutable capture is easy
  # to get subtly wrong).
  finish <- function(
    row,
    status,
    action,
    detail,
    values = NULL,
    at = NULL,
    remote_present = FALSE,
    submitted = FALSE
  ) {
    row$status <- status
    row$last_action <- action
    row$last_detail <- detail
    row$confirmed_values <- values
    row$confirmed_at <- if (is.null(at)) as.POSIXct(NA) else at
    row$remote_present <- remote_present
    row$updated_at <- web_entry_now()
    state$rows[[key]] <- row
    web_entry_persist(batch, state, state_dir)
    list(state = state, receipt = row, submitted = submitted)
  }

  probe <- web_entry_query_row(batch, browser, key)

  if (identical(probe$state, "match")) {
    note <- if (identical(row$status, "confirmed")) {
      "Readback matches the batch; the receipt was refreshed without resubmitting."
    } else {
      "Readback found an existing matching record; it was adopted without resubmitting."
    }

    return(finish(
      row,
      "confirmed",
      "confirmed",
      note,
      values = probe$observed,
      at = web_entry_now(),
      remote_present = TRUE
    ))
  }

  if (identical(probe$state, "conflict")) {
    return(finish(
      row,
      "conflict",
      "conflict",
      paste0(
        "A different value is stored under this business key; it was not overwritten. ",
        probe$detail
      ),
      values = probe$observed,
      remote_present = TRUE
    ))
  }

  if (identical(probe$state, "ambiguous")) {
    return(finish(
      row,
      "unknown",
      "unresolved",
      paste0(
        "The business key does not resolve to a single record, so nothing was saved. ",
        probe$detail
      ),
      remote_present = TRUE
    ))
  }

  if (!identical(probe$state, "missing")) {
    detail <- probe$detail
    if (is.null(detail) || !nzchar(detail)) {
      detail <- "The readback could not be interpreted."
    }

    return(finish(
      row,
      "unknown",
      "unresolved",
      paste0(
        "The current state of this key is unknown, so nothing was saved. ",
        detail
      )
    ))
  }

  # The readback is strongly consistent and shows no record for this key, so a
  # save is safe. The attempt limit applies to this run only: an earlier run's
  # failed attempts do not prevent a later resume, because every attempt is
  # preceded by a readback proving that nothing was saved.
  if (attempts_left <= 0L) {
    return(finish(
      row,
      "failed",
      "exhausted",
      glue::glue(
        "No record was found for this key, but the {max_attempts}-attempt limit for this run was reached."
      )
    ))
  }

  # Record that a save is in flight before making it, so an interrupted run is
  # known to need verification rather than a blind resubmission.
  row$status <- "submitting"
  row$attempts <- row$attempts + 1L
  row$last_action <- "submitting"
  row$last_detail <- ""
  row$updated_at <- web_entry_now()
  state$rows[[key]] <- row
  web_entry_persist(batch, state, state_dir)

  submit_outcome <- web_entry_submit_row(batch, browser, key)
  probe_after <- web_entry_query_row(batch, browser, key)

  if (identical(probe_after$state, "match")) {
    note <- if (is.null(submit_outcome$error)) {
      "Saved and confirmed by readback."
    } else {
      paste0(
        "The save reported an error (",
        conditionMessage(submit_outcome$error),
        "), but the record was confirmed by readback."
      )
    }

    return(finish(
      row,
      "confirmed",
      "confirmed",
      note,
      values = probe_after$observed,
      at = web_entry_now(),
      remote_present = TRUE,
      submitted = TRUE
    ))
  }

  if (identical(probe_after$state, "conflict")) {
    return(finish(
      row,
      "conflict",
      "conflict",
      paste0(
        "A different value is stored under this business key after saving; it was not overwritten. ",
        probe_after$detail
      ),
      values = probe_after$observed,
      remote_present = TRUE,
      submitted = TRUE
    ))
  }

  retry_safe <- identical(probe_after$state, "missing")
  detail <- if (retry_safe) {
    "The readback found no record for this key."
  } else {
    paste0("The readback did not confirm a record. ", probe_after$detail)
  }

  if (is.null(submit_outcome$error)) {
    # The save appeared to succeed, yet no record was found. With a strongly
    # consistent readback this is contradictory, so the outcome is unknown and
    # is not retried now: a later run verifies it first.
    return(finish(
      row,
      "unknown",
      "unresolved",
      paste0(
        "The save appeared to succeed but ",
        detail,
        " The outcome is unknown and must be verified before another submission."
      ),
      submitted = TRUE
    ))
  }

  detail <- paste0(
    "The save failed (",
    conditionMessage(submit_outcome$error),
    "). ",
    detail
  )

  if (retry_safe) {
    # The save failed and the readback proves no record exists. This run stops
    # at that verification; a later run may try again from a known state.
    return(finish(
      row,
      "failed",
      "save_failed",
      detail,
      submitted = TRUE
    ))
  }

  finish(
    row,
    "unknown",
    "unresolved",
    paste0(
      detail,
      " The outcome is unknown and must be verified before another submission."
    ),
    submitted = TRUE
  )
}

# Receipts and analysis ------------------------------------------------------

web_entry_receipts_tibble <- function(state) {
  rows <- lapply(state$rows, function(row) {
    tibble::tibble(
      key = row$key,
      status = row$status,
      confirmed = identical(row$status, "confirmed"),
      remote_saved = isTRUE(row$remote_present),
      attempts = as.integer(row$attempts),
      confirmed_at = as.POSIXct(row$confirmed_at),
      values = list(row$confirmed_values),
      detail = row$last_detail
    )
  })

  dplyr::bind_rows(rows)
}

web_entry_all_confirmed <- function(state) {
  length(state$rows) > 0L &&
    all(vapply(
      state$rows,
      function(row) identical(row$status, "confirmed"),
      logical(1)
    ))
}

# Build the observations that were actually read back from the remote
# application, in batch row order, with each field converted back to the type of
# its original column.
web_entry_included_from_state <- function(batch, state) {
  row_keys <- vapply(
    seq_len(nrow(batch$data)),
    function(i) batch$key_values[[i]],
    character(1)
  )

  field_values <- lapply(row_keys, function(key) {
    state$rows[[key]]$confirmed_values
  })
  field_names <- names(field_values[[1]])

  columns <- list()
  columns[[batch$key]] <- batch$data[[batch$key]]

  for (j in seq_along(batch$fields)) {
    field <- names(batch$fields)[[j]]
    col <- unname(batch$fields)[[j]]

    if (!(field %in% field_names)) {
      web_entry_call_error(
        c(
          "A batch field was not found in the confirmed readback.",
          x = "Field: {.field {field}}."
        ),
        class = "infer_web_entry_analysis_error"
      )
    }

    raw <- vapply(field_values, function(x) x[[field]], character(1))
    cast <- tryCatch(
      web_entry_cast(raw, web_entry_type_spec(batch$data[[col]])),
      error = function(e) NULL
    )

    if (is.null(cast) || anyNA(cast)) {
      web_entry_call_error(
        c(
          "A confirmed value could not be converted back to its column type.",
          x = "Field {.field {field}} does not hold valid values for column {.field {col}}."
        ),
        class = "infer_web_entry_analysis_error"
      )
    }

    if (is.null(columns[[col]])) {
      columns[[col]] <- cast
    }
  }

  tibble::as_tibble(columns[!duplicated(names(columns))])
}

web_entry_previous_summary <- function(previous) {
  if (is.null(previous)) {
    return(NULL)
  }

  list(
    completed_at = previous$completed_at,
    formula = previous$formula,
    success = previous$success,
    included = previous$included,
    coefficients = previous$coefficients
  )
}

# Compute the analysis for this run. The result always carries the coefficients
# of the most recent *usable* analysis: a fresh fit when this run produced one,
# otherwise the persisted one. Failures never destroy the last usable analysis.
web_entry_analyze <- function(batch, state, state_dir) {
  previous <- web_entry_analysis_load(batch, state_dir)

  fallback <- function(status, reason, computed = NULL, persisted = NA) {
    list(
      status = status,
      reason = reason,
      completed_at = if (is.null(previous)) {
        as.POSIXct(NA)
      } else {
        previous$completed_at
      },
      fingerprint = batch$fingerprint,
      persisted = persisted,
      coefficients = if (is.null(previous)) NULL else previous$coefficients,
      included = if (is.null(previous)) NULL else previous$included,
      computed = computed,
      previous = web_entry_previous_summary(previous)
    )
  }

  if (!web_entry_all_confirmed(state)) {
    return(fallback(
      "stale",
      "Not every observation in the batch is confirmed by a readback from this run; no analysis was produced and the previous usable analysis was kept."
    ))
  }

  included <- tryCatch(
    web_entry_included_from_state(batch, state),
    error = function(e) e
  )

  if (inherits(included, "error")) {
    return(fallback(
      "failed",
      paste0("The observations could not be assembled: ", conditionMessage(included))
    ))
  }

  computed <- tryCatch(
    {
      specified <- specify(
        included,
        formula = batch$formula,
        success = batch$success
      )
      coefficients <- fit(specified)

      list(
        status = "current",
        reason = "",
        completed_at = web_entry_now(),
        fingerprint = batch$fingerprint,
        persisted = TRUE,
        coefficients = coefficients,
        included = included,
        specified = specified,
        previous = web_entry_previous_summary(previous)
      )
    },
    error = function(e) {
      fallback(
        "failed",
        paste0("The analysis failed: ", conditionMessage(e))
      )
    }
  )

  if (identical(computed$status, "current")) {
    saved <- tryCatch(
      {
        web_entry_analysis_save(
          list(
            status = "current",
            completed_at = computed$completed_at,
            fingerprint = batch$fingerprint,
            formula = batch$formula,
            success = batch$success,
            included = computed$included,
            coefficients = computed$coefficients
          ),
          batch,
          state_dir
        )
        TRUE
      },
      error = function(e) {
        cli::cli_warn(
          c(
            "The analysis could not be saved; the previous usable analysis was kept.",
            x = conditionMessage(e)
          ),
          class = "infer_web_entry_persist_warning"
        )
        FALSE
      }
    )

    if (!saved) {
      return(fallback(
        "failed",
        "The analysis could not be saved to disk; the previous usable analysis was kept.",
        computed = list(
          completed_at = computed$completed_at,
          coefficients = computed$coefficients,
          included = computed$included
        ),
        persisted = FALSE
      ))
    }
  }

  computed
}

# Read every key back once more and keep only rows that still agree with the
# batch. Used right before analysis so that the fit is based on the current
# state of the remote application.
web_entry_revalidate <- function(batch, state, browser, state_dir) {
  for (key in names(state$rows)) {
    row <- state$rows[[key]]
    probe <- web_entry_query_row(batch, browser, key)

    if (identical(probe$state, "match")) {
      was_confirmed <- identical(row$status, "confirmed")
      row$status <- "confirmed"
      row$confirmed_at <- web_entry_now()
      row$confirmed_values <- probe$observed
      row$remote_present <- TRUE
      row$last_action <- "reconfirmed"
      if (!was_confirmed) {
        # Keep what happened during entry; the refreshed confirmation time
        # already records the analysis-time readback.
        row$last_detail <- "Confirmed by a readback made after entry."
      }
    } else if (identical(probe$state, "conflict")) {
      row$status <- "conflict"
      row$confirmed_at <- as.POSIXct(NA)
      row$confirmed_values <- probe$observed
      row$remote_present <- TRUE
      row$last_action <- "conflict"
      row$last_detail <- paste0(
        "The readback made for the analysis found different values. ",
        probe$detail
      )
    } else if (identical(probe$state, "missing")) {
      row$status <- "pending"
      row$confirmed_at <- as.POSIXct(NA)
      row$confirmed_values <- NULL
      row$remote_present <- FALSE
      row$last_action <- "vanished"
      row$last_detail <- paste0(
        "The readback made for the analysis found no record for this key."
      )
    } else {
      row$status <- "unknown"
      row$confirmed_at <- as.POSIXct(NA)
      row$confirmed_values <- NULL
      row$remote_present <- identical(probe$state, "ambiguous")
      row$last_action <- "unresolved"
      row$last_detail <- paste0(
        "The readback made for the analysis was inconclusive. ",
        probe$detail
      )
    }


    row$updated_at <- web_entry_now()
    state$rows[[key]] <- row
  }

  web_entry_persist(batch, state, state_dir)
  state
}

#' Run a resumable batch of web entries and analyse confirmed observations
#'
#' @description
#'
#' `web_entry_run()` enters the observations of a batch built with
#' [web_entry_batch()] into the remote web application and confirms each one by
#' reading it back. It is safe to call again after an interruption:
#'
#' \itemize{
#'   \item every key is read back *before* anything is saved, so a record that
#'     was saved by an earlier run (whose local receipt was lost) is adopted
#'     instead of being entered again;
#'   \item a record already holding different values under the same key is
#'     reported as a conflict and is never overwritten;
#'   \item when the outcome of an earlier save is unknown, the record is
#'     verified rather than resubmitted;
#'   \item a save is only treated as confirmed when a readback shows the fields
#'     agree with the batch; the adapter's return value, including an apparent
#'     success message, is never treated as confirmation.
#' }
#'
#' Only when *every* row is confirmed by a readback made during this run are the
#' confirmed observations passed to [specify()] and [`fit()`][fit.infer()] with
#' the batch's original formula. Otherwise, or if the analysis or writing the
#' analysis fails, the previous usable analysis is kept and reported instead.
#'
#' @param batch A batch built with [web_entry_batch()].
#' @param browser A logged-in browser handle. Defaults to the handle supplied to
#'   [web_entry_batch()].
#' @param max_attempts The maximum number of save attempts per row in one run.
#'   Every attempt is preceded by a readback showing that no record exists, so
#'   the limit only stops repeated failures; it never causes a record to be
#'   saved over an existing one, and a later run starts with a fresh budget.
#' @param state_dir Directory holding persisted progress. Defaults to the
#'   batch's `state_dir`.
#' @param analyze Whether to analyse and fit the batch when it is fully
#'   confirmed.
#'
#' @return An object of class `infer_web_entry`, a list with components:
#' \itemize{
#'   \item `fingerprint`: the batch fingerprint.
#'   \item `receipts`: a tibble with one receipt per row: `key`, `status`,
#'     `confirmed`, `remote_saved`, `attempts`, `confirmed_at`, `values`, and
#'     `detail`.
#'   \item `complete`: whether every row is confirmed by a readback from this
#'     run.
#'   \item `included`: the observations actually included in the analysis
#'     (those read back from the remote application), or those of the previous
#'     usable analysis when no current one is available.
#'   \item `coefficients`: the native [`fit()`][fit.infer()] coefficients for
#'     the batch formula, for the analysis reported in `analysis$status`.
#'   \item `analysis`: a list with `status` (`"current"`, `"stale"`,
#'     `"failed"`, or `"none"`), `reason`, `completed_at`, `persisted`,
#'     `previous`, and, when a current fit could not be kept, `computed`.
#' }
#'
#' @seealso [web_entry_batch()], [web_entry_verify()],
#'   [web_entry_receipts()]
#' @export
web_entry_run <- function(
  batch,
  browser = NULL,
  max_attempts = 3L,
  state_dir = NULL,
  analyze = TRUE
) {
  web_entry_check_batch(batch)
  browser <- web_entry_or(browser, batch$browser)

  if (is.null(browser)) {
    web_entry_call_error(
      c(
        "A logged-in `browser` handle is required.",
        i = "Supply it to `web_entry_batch()` or to `web_entry_run()`."
      ),
      class = "infer_web_entry_bad_argument"
    )
  }

  web_entry_check_count(max_attempts, "max_attempts", min_val = 1L)
  web_entry_check_flag(analyze, "analyze")

  state_dir <- web_entry_run_state_dir(state_dir, batch$state_dir)
  state <- web_entry_state_load(batch, state_dir)

  for (key in names(state$rows)) {
    attempts_left <- max_attempts
    repeat {
      advanced <- web_entry_advance_row(
        batch,
        state,
        browser,
        key,
        attempts_left = attempts_left,
        max_attempts = max_attempts,
        state_dir = state_dir
      )
      state <- advanced$state

      if (isTRUE(advanced$submitted)) {
        attempts_left <- attempts_left - 1L
      }

      status <- advanced$receipt$status

      # A row only ends as `failed` when a readback has proved that no record
      # exists, so trying again is not a blind resubmission. Rows in any other
      # state (including `unknown`) are left alone for a later verification.
      if (!identical(status, "failed") || attempts_left <= 0L) {
        break
      }
    }
  }

  # The analysis may only use observations confirmed by a current readback. The
  # per-row loop already confirmed each row, but a record could have changed
  # while the rest of the batch was being processed, so read the whole batch
  # back once more immediately before analysing.
  if (analyze && web_entry_all_confirmed(state)) {
    state <- web_entry_revalidate(batch, state, browser, state_dir)
  }

  analysis <- if (analyze) {
    web_entry_analyze(batch, state, state_dir)
  } else {
    list(
      status = "none",
      reason = "Analysis was not requested.",
      completed_at = as.POSIXct(NA),
      fingerprint = batch$fingerprint,
      persisted = NA,
      coefficients = NULL,
      included = NULL,
      computed = NULL,
      previous = web_entry_previous_summary(
        web_entry_analysis_load(batch, state_dir)
      )
    )
  }

  structure(
    list(
      fingerprint = batch$fingerprint,
      key = batch$key,
      formula = batch$formula,
      n = nrow(batch$data),
      receipts = web_entry_receipts_tibble(state),
      complete = web_entry_all_confirmed(state),
      included = analysis$included,
      coefficients = analysis$coefficients,
      analysis = analysis
    ),
    class = c("infer_web_entry", "list")
  )
}

#' Read a batch back without saving anything
#'
#' @description
#'
#' `web_entry_verify()` reads every row of a batch back from the remote
#' application and refreshes the persisted receipts. It never fills a form or
#' saves a record, so it is safe for investigating an interrupted run. A row is
#' confirmed only when the record currently read back agrees with the batch.
#'
#' @inheritParams web_entry_run
#'
#' @return An object of class `infer_web_entry` without an analysis; see
#'   [web_entry_run()] for its components.
#'
#' @seealso [web_entry_batch()], [web_entry_run()],
#'   [web_entry_receipts()]
#' @export
web_entry_verify <- function(batch, browser = NULL, state_dir = NULL) {
  web_entry_check_batch(batch)
  browser <- web_entry_or(browser, batch$browser)

  if (is.null(browser)) {
    web_entry_call_error(
      c(
        "A logged-in `browser` handle is required.",
        i = "Supply it to `web_entry_batch()` or to `web_entry_verify()`."
      ),
      class = "infer_web_entry_bad_argument"
    )
  }

  state_dir <- web_entry_run_state_dir(state_dir, batch$state_dir)
  state <- web_entry_state_load(batch, state_dir)

  for (key in names(state$rows)) {
    row <- state$rows[[key]]
    probe <- web_entry_query_row(batch, browser, key)

    if (identical(probe$state, "match")) {
      row$status <- "confirmed"
      row$confirmed_at <- web_entry_now()
      row$confirmed_values <- probe$observed
      row$remote_present <- TRUE
      row$last_action <- "verified"
      row$last_detail <- "Confirmed by a readback."
    } else if (identical(probe$state, "missing")) {
      row$status <- if (identical(row$status, "confirmed")) "pending" else row$status
      row$confirmed_at <- as.POSIXct(NA)
      row$confirmed_values <- NULL
      row$remote_present <- FALSE
      row$last_action <- "not_found"
      row$last_detail <- "No record was found in the readback."
    } else if (identical(probe$state, "conflict")) {
      row$status <- "conflict"
      row$confirmed_at <- as.POSIXct(NA)
      row$confirmed_values <- probe$observed
      row$remote_present <- TRUE
      row$last_action <- "conflict"
      row$last_detail <- paste0("The readback found different values. ", probe$detail)
    } else {
      row$status <- "unknown"
      row$confirmed_at <- as.POSIXct(NA)
      row$confirmed_values <- NULL
      row$remote_present <- identical(probe$state, "ambiguous")
      row$last_action <- "unresolved"
      row$last_detail <- paste0("The readback was inconclusive. ", probe$detail)
    }

    row$updated_at <- web_entry_now()
    state$rows[[key]] <- row
  }

  web_entry_persist(batch, state, state_dir)

  structure(
    list(
      fingerprint = batch$fingerprint,
      key = batch$key,
      formula = batch$formula,
      n = nrow(batch$data),
      receipts = web_entry_receipts_tibble(state),
      complete = web_entry_all_confirmed(state),
      included = NULL,
      coefficients = NULL,
      analysis = list(
        status = "none",
        reason = "Verification only; no analysis was run.",
        completed_at = as.POSIXct(NA),
        fingerprint = batch$fingerprint,
        persisted = NA,
        coefficients = NULL,
        included = NULL,
        computed = NULL,
        previous = web_entry_previous_summary(
          web_entry_analysis_load(batch, state_dir)
        )
      )
    ),
    class = c("infer_web_entry", "list")
  )
}

#' Inspect the receipts of a batch
#'
#' @description
#'
#' `web_entry_receipts()` returns the persisted per-row receipts of a batch
#' without touching the browser, so progress can be inspected before deciding
#' whether to resume.
#'
#' @inheritParams web_entry_run
#'
#' @return A tibble with one receipt per row; see [web_entry_run()].
#'
#' @seealso [web_entry_batch()], [web_entry_run()]
#' @export
web_entry_receipts <- function(batch, state_dir = NULL) {
  web_entry_check_batch(batch)
  state_dir <- web_entry_run_state_dir(state_dir, batch$state_dir)
  state <- web_entry_state_load(batch, state_dir)
  web_entry_receipts_tibble(state)
}

web_entry_check_batch <- function(batch, call = rlang::caller_env()) {
  if (!inherits(batch, "infer_web_entry_batch")) {
    web_entry_call_error(
      "`batch` must be built with `web_entry_batch()`.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  invisible(batch)
}

web_entry_check_count <- function(
  x,
  name,
  min_val = 0L,
  call = rlang::caller_env()
) {
  ok <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    x >= min_val &&
    x == round(x)

  if (!ok) {
    web_entry_call_error(
      "`{name}` must be a single whole number of at least {min_val}.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  as.integer(x)
}

web_entry_check_flag <- function(x, name, call = rlang::caller_env()) {
  if (!isTRUE(x) && !isFALSE(x)) {
    web_entry_call_error(
      "`{name}` must be `TRUE` or `FALSE`.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  x
}

#' Print a web entry result
#'
#' @param x An object of class `infer_web_entry`, as returned by
#'   [web_entry_run()] or [web_entry_verify()].
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @rdname web_entry_run
#' @export
print.infer_web_entry <- function(x, ...) {
  cat(glue::glue(
    "<infer web entry: {x$n} record{if (x$n == 1) '' else 's'}, fingerprint {substr(x$fingerprint, 1, 8)}>"
  ))
  cat("\n")

  if (!is.null(x$receipts) && nrow(x$receipts) > 0L) {
    levels <- c(
      "confirmed",
      "pending",
      "submitting",
      "unknown",
      "conflict",
      "failed"
    )
    counts <- table(factor(x$receipts$status, levels = levels))
    counts <- counts[counts > 0L]

    cat(glue::glue(
      "Receipts: {paste(names(counts), as.integer(counts), sep = ' = ', collapse = ', ')}"
    ))
    cat("\n")
  }

  cat(glue::glue("Analysis: {x$analysis$status}"))
  cat("\n")

  if (!is.null(x$analysis$reason) && nzchar(x$analysis$reason)) {
    cat(x$analysis$reason)
    cat("\n")
  }

  if (!is.null(x$coefficients)) {
    if (!isTRUE(x$analysis$status == "current")) {
      cat("Coefficients of the previous usable analysis:\n")
    }
    print(x$coefficients)
  }

  invisible(x)
}
