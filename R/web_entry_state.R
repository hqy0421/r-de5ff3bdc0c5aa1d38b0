# Durable state for resumable web entry ---------------------------------------
#
# A batch is bound to the supplied data and configuration by a fingerprint.
# State is persisted under a fingerprint-derived file name, so a batch created
# from different inputs (or a different configuration) can never pick up the
# progress of another batch.

web_entry_state_version <- function() {
  1L
}

web_entry_new_state <- function(fingerprint, keys) {
  rows <- lapply(keys, function(key) {
    list(
      key = key,
      status = "pending",
      attempts = 0L,
      confirmed_at = as.POSIXct(NA),
      confirmed_values = NULL,
      remote_present = FALSE,
      last_action = "created",
      last_detail = "",
      updated_at = web_entry_now()
    )
  })
  names(rows) <- keys

  list(
    version = web_entry_state_version(),
    fingerprint = fingerprint,
    rows = rows
  )
}

web_entry_state_file <- function(state_dir, fingerprint) {
  file.path(state_dir, paste0("web-entry-", fingerprint, ".rds"))
}

web_entry_analysis_file <- function(state_dir, fingerprint) {
  file.path(state_dir, paste0("web-entry-", fingerprint, "-analysis.rds"))
}

web_entry_atomic_save <- function(object, path, call = rlang::caller_env()) {
  dir <- dirname(path)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(dir)) {
    web_entry_call_error(
      "Could not create the state directory {.path {dir}}.",
      class = "infer_web_entry_state_error",
      call = call
    )
  }

  tmp <- tempfile(tmpdir = dir, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)

  ok <- tryCatch(
    {
      saveRDS(object, tmp)
      TRUE
    },
    error = function(e) {
      web_entry_call_error(
        c(
          "Could not write web entry state to {.path {path}}.",
          x = conditionMessage(e)
        ),
        class = "infer_web_entry_state_error",
        call = call
      )
    }
  )

  if (!ok || !file.rename(tmp, path)) {
    web_entry_call_error(
      "Could not atomically replace web entry state at {.path {path}}.",
      class = "infer_web_entry_state_error",
      call = call
    )
  }

  invisible(path)
}

# Load the persisted state of one batch. An existing but unreadable state file
# is an error rather than an empty batch: silently starting over could enter
# already saved observations twice.
web_entry_state_load <- function(batch, state_dir, call = rlang::caller_env()) {
  if (is.null(state_dir)) {
    return(web_entry_new_state(
      batch$fingerprint,
      web_entry_key_values(batch)
    ))
  }

  path <- web_entry_state_file(state_dir, batch$fingerprint)

  if (!file.exists(path)) {
    return(web_entry_new_state(
      batch$fingerprint,
      web_entry_key_values(batch)
    ))
  }

  state <- tryCatch(
    readRDS(path),
    error = function(e) {
      web_entry_call_error(
        c(
          "The persisted state for this batch exists but could not be read.",
          x = "{.path {path}}: {conditionMessage(e)}",
          i = "Refusing to start over; already saved observations could be entered twice."
        ),
        class = "infer_web_entry_state_error",
        call = call
      )
    }
  )

  web_entry_check_state(state, batch, path, call = call)
}

web_entry_check_state <- function(state, batch, path, call) {
  if (!is.list(state) || is.null(state$fingerprint) || !is.list(state$rows)) {
    web_entry_call_error(
      c(
        "The persisted state for this batch is malformed.",
        x = "{.path {path}}"
      ),
      class = "infer_web_entry_state_error",
      call = call
    )
  }

  if (!identical(state$version, web_entry_state_version())) {
    web_entry_call_error(
      c(
        "The persisted state was written by an incompatible version.",
        x = "Stored version: {.val {state$version}}.",
        x = "Current version: {.val {web_entry_state_version()}}.",
        i = "Refusing to interpret it; start a new batch with a different `state_dir`."
      ),
      class = "infer_web_entry_state_error",
      call = call
    )
  }

  if (!identical(state$fingerprint, batch$fingerprint)) {
    web_entry_call_error(
      c(
        "The persisted state belongs to a different batch.",
        x = "Stored fingerprint: {.val {state$fingerprint}}.",
        x = "Current fingerprint: {.val {batch$fingerprint}}.",
        i = "Progress is bound to the original data and configuration and cannot be reused with different inputs."
      ),
      class = "infer_web_entry_stale_state",
      call = call
    )
  }

  keys <- web_entry_key_values(batch)
  unknown <- setdiff(keys, names(state$rows))
  extra <- setdiff(names(state$rows), keys)

  if (length(unknown) > 0L || length(extra) > 0L) {
    web_entry_call_error(
      c(
        "The persisted state does not match the batch's business keys.",
        x = "Keys without state: {.val {unknown}}.",
        x = "State without keys: {.val {extra}}."
      ),
      class = "infer_web_entry_stale_state",
      call = call
    )
  }

  # Any row that was mid-submission when the process stopped must be treated as
  # unknown: the remote side may or may not hold the observation.
  for (key in names(state$rows)) {
    if (identical(state$rows[[key]]$status, "submitting")) {
      state$rows[[key]]$status <- "unknown"
      state$rows[[key]]$last_action <- "interrupted"
      state$rows[[key]]$last_detail <- paste0(
        "A save was in flight when the batch was interrupted; ",
        "its outcome must be verified before any further submission."
      )
    }
  }

  state
}

web_entry_state_save <- function(batch, state, state_dir) {
  if (is.null(state_dir)) {
    return(invisible(state))
  }

  web_entry_atomic_save(
    state,
    web_entry_state_file(state_dir, batch$fingerprint)
  )
}

# Analysis persistence -------------------------------------------------------

web_entry_analysis_load <- function(batch, state_dir) {
  if (is.null(state_dir)) {
    return(NULL)
  }

  path <- web_entry_analysis_file(state_dir, batch$fingerprint)

  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    readRDS(path),
    error = function(e) NULL
  )
}

web_entry_analysis_save <- function(analysis, batch, state_dir) {
  if (is.null(state_dir)) {
    return(FALSE)
  }

  web_entry_atomic_save(
    analysis,
    web_entry_analysis_file(state_dir, batch$fingerprint)
  )

  TRUE
}
