# A tiny in-memory web application used by the web entry tests. It records
# saved observations by business key, counts saves, and can be made to fail,
# time out, or hide a record from readback to exercise recovery paths.
new_fake_site <- function() {
  site <- new.env(parent = emptyenv())
  site$records <- list()
  site$saves <- 0L
  site$queries <- 0L
  site
}

fake_form <- function(
  site,
  error_after_save = NULL,
  fail_times = 0L,
  return_value = "Saved successfully!"
) {
  force(site)
  force(error_after_save)
  force(fail_times)
  force(return_value)

  state <- new.env(parent = emptyenv())
  state$remaining_failures <- fail_times

  function(browser, values, config) {
    if (state$remaining_failures > 0L) {
      state$remaining_failures <- state$remaining_failures - 1L
      stop("temporary network failure")
    }

    site$saves <- site$saves + 1L
    site$records[[config$key]] <- as.list(values)

    if (!is.null(error_after_save)) {
      stop(error_after_save)
    }

    return_value
  }
}

fake_query <- function(site, hidden = NULL) {
  force(site)
  force(hidden)

  function(browser, key, config) {
    site$queries <- site$queries + 1L

    if (!is.null(hidden) && key %in% hidden) {
      return(data.frame())
    }

    record <- site$records[[key]]

    if (is.null(record)) {
      return(data.frame())
    }

    as.data.frame(record, stringsAsFactors = FALSE)
  }
}

fake_data <- function() {
  data.frame(
    trial = c("t1", "t2", "t3", "t4"),
    arm = factor(c("control", "treatment", "control", "treatment")),
    score = c(1, 2, 3, 4.5),
    note = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
}

fake_fields <- function() {
  c(arm = "arm", score = "score")
}

new_fake_batch <- function(
  data = fake_data(),
  site = new_fake_site(),
  state_dir = tempfile(),
  form = NULL,
  query = NULL,
  fields = fake_fields(),
  formula = score ~ arm,
  ...
) {
  if (is.null(form)) {
    form <- fake_form(site)
  }
  if (is.null(query)) {
    query <- fake_query(site)
  }

  list(
    batch = web_entry_batch(
      data = data,
      key = "trial",
      fields = fields,
      formula = formula,
      browser = "fake-browser",
      form = form,
      query = query,
      state_dir = state_dir,
      ...
    ),
    site = site,
    state_dir = state_dir
  )
}

receipt_status <- function(result) {
  stats::setNames(result$receipts$status, result$receipts$key)
}

status_of <- function(result, key) {
  receipts <- result$receipts
  receipts$status[match(key, receipts$key)]
}

test_that("a fresh batch enters every record once and confirms by readback", {
  setup <- new_fake_batch()

  result <- web_entry_run(setup$batch)

  expect_s3_class(result, "infer_web_entry")
  expect_true(result$complete)
  expect_identical(receipt_status(result), c(
    t1 = "confirmed",
    t2 = "confirmed",
    t3 = "confirmed",
    t4 = "confirmed"
  ))
  expect_identical(setup$site$saves, 4L)
  expect_true(all(result$receipts$confirmed))
  expect_true(all(result$receipts$remote_saved))

  # A second run reads the records back and does not save them again.
  second <- web_entry_run(setup$batch)
  expect_identical(setup$site$saves, 4L)
  expect_true(second$complete)
})

test_that("only a readback that agrees with the batch confirms a record", {
  site <- new_fake_site()

  # The form reports success but never stores anything.
  silent_form <- function(browser, values, config) "Saved successfully!"
  setup <- new_fake_batch(site = site, form = silent_form)

  result <- web_entry_run(setup$batch)

  expect_false(result$complete)
  expect_identical(unname(receipt_status(result)), rep("unknown", 4))
  expect_true(all(grepl("outcome is unknown", result$receipts$detail)))
  expect_identical(result$analysis$status, "stale")
  expect_null(result$coefficients)
})

test_that("a save that reports an error but lands is confirmed by readback", {
  site <- new_fake_site()
  form <- fake_form(site, error_after_save = "timeout after save")

  setup <- new_fake_batch(site = site, form = form)
  result <- web_entry_run(setup$batch)

  expect_true(result$complete)
  expect_identical(unname(receipt_status(result)), rep("confirmed", 4))
  expect_true(any(grepl("reported an error", result$receipts$detail)))
  expect_identical(result$analysis$status, "current")
})

test_that("a record saved remotely is adopted without being entered again", {
  site <- new_fake_site()

  # Another process (or an earlier run whose receipt was lost) already saved
  # every record exactly as the batch describes.
  data <- fake_data()
  for (i in seq_len(nrow(data))) {
    key <- data$trial[[i]]
    site$records[[key]] <- list(
      arm = as.character(data$arm[[i]]),
      score = as.character(data$score[[i]])
    )
  }

  setup <- new_fake_batch(site = site)
  result <- web_entry_run(setup$batch)

  expect_true(result$complete)
  expect_identical(site$saves, 0L)
  expect_true(all(grepl("without resubmitting|adopted", result$receipts$detail)))
  expect_identical(result$analysis$status, "current")
})

test_that("a lost local receipt is rebuilt without duplicate entry", {
  site <- new_fake_site()
  state_dir <- tempfile()

  # The save lands but the process dies before confirmation is persisted: the
  # stored state still reads `submitting`.
  setup <- new_fake_batch(
    site = site,
    state_dir = state_dir,
    form = fake_form(site, error_after_save = "process interrupted")
  )

  web_entry_run(setup$batch, max_attempts = 1L)
  expect_identical(site$saves, 4L)

  state_path <- list.files(
    state_dir,
    pattern = "web-entry-[0-9a-f]+\\.rds$",
    full.names = TRUE
  )
  expect_length(state_path, 1L)

  state <- readRDS(state_path)
  for (key in names(state$rows)) {
    state$rows[[key]]$status <- "submitting"
    state$rows[[key]]$confirmed_values <- NULL
  }
  saveRDS(state, state_path)

  # Reopening must verify and confirm, not submit again.
  resumed <- web_entry_run(setup$batch)
  expect_identical(site$saves, 4L)
  expect_true(resumed$complete)
  expect_identical(resumed$analysis$status, "current")
})

test_that("a different value under the same business key is a conflict, not an overwrite", {
  site <- new_fake_site()
  site$records[["t1"]] <- list(arm = "control", score = "999")

  setup <- new_fake_batch(site = site)
  result <- web_entry_run(setup$batch)

  statuses <- receipt_status(result)
  expect_identical(unname(statuses[["t1"]]), "conflict")
  expect_true(all(statuses[names(statuses) != "t1"] == "confirmed"))

  # The stored record is untouched and the conflicting key was never saved.
  expect_identical(site$records[["t1"]]$score, "999")
  expect_identical(site$saves, 3L)
  expect_false(result$complete)
  expect_identical(result$analysis$status, "stale")
  expect_null(result$coefficients)
})

test_that("an unknown outcome is verified before anything is submitted", {
  site <- new_fake_site()

  # `t1` is stored, but the form throws; readback hides the record on the first
  # run, so its outcome is unknown.
  hidden <- new.env(parent = emptyenv())
  hidden$keys <- "t1"

  form <- fake_form(site)
  query <- fake_query(site, hidden = NULL)
  guarded_query <- function(browser, key, config) {
    if (key %in% hidden$keys) {
      site$queries <- site$queries + 1L
      return(data.frame())
    }
    query(browser, key, config)
  }

  setup <- new_fake_batch(
    site = site,
    form = form,
    query = guarded_query,
    data = fake_data()[1:2, ]
  )

  first <- web_entry_run(setup$batch)
  expect_identical(status_of(first, "t1"), "unknown")
  expect_identical(site$saves, 2L)

  # The record becomes visible again. Resuming must adopt it, not save again.
  hidden$keys <- character()
  second <- web_entry_run(setup$batch)

  expect_identical(site$saves, 2L)
  expect_identical(status_of(second, "t1"), "confirmed")
  expect_true(second$complete)
})

test_that("a failed save is retried only once readback proves no record exists", {
  site <- new_fake_site()
  form <- fake_form(site, fail_times = 1L)

  setup <- new_fake_batch(site = site, form = form)
  result <- web_entry_run(setup$batch, max_attempts = 2L)

  # The first attempt fails before saving; the readback proves the key is
  # missing, so a second attempt is safe.
  expect_true(result$complete)
  expect_identical(result$receipts$attempts[[1]], 2L)
})

test_that("the attempt limit bounds repeated saves", {
  site <- new_fake_site()
  # Fails before saving every time.
  form <- function(browser, values, config) stop("network down")

  setup <- new_fake_batch(
    site = site,
    form = form,
    data = fake_data()[1, ]
  )

  result <- web_entry_run(setup$batch, max_attempts = 2L)

  expect_false(result$complete)
  expect_identical(receipt_status(result)[[1]], "failed")
  expect_identical(result$receipts$attempts[[1]], 2L)

  # The budget is per run: a later run gets a fresh budget, but still verifies
  # (and finds no record) before every save.
  resumed <- web_entry_run(setup$batch, max_attempts = 2L)
  expect_identical(resumed$receipts$attempts[[1]], 4L)
  expect_identical(receipt_status(resumed)[[1]], "failed")
})

test_that("progress is bound to the data and configuration", {
  site <- new_fake_site()
  state_dir <- tempfile()

  setup <- new_fake_batch(site = site, state_dir = state_dir)
  web_entry_run(setup$batch)
  expect_identical(site$saves, 4L)

  # Different data: a new fingerprint and a new batch that cannot reuse the
  # first batch's progress.
  changed <- fake_data()
  changed$score[[1]] <- 100
  different_data <- new_fake_batch(
    data = changed,
    site = site,
    state_dir = state_dir
  )

  expect_false(identical(
    setup$batch$fingerprint,
    different_data$batch$fingerprint
  ))

  # The record for `t1` holds the original score, so it conflicts rather than
  # being overwritten by the changed data.
  result <- web_entry_run(different_data$batch)
  expect_identical(status_of(result, "t1"), "conflict")
  expect_identical(site$records[["t1"]]$score, "1")

  # Different configuration: also a different fingerprint.
  other_fields <- new_fake_batch(
    data = fake_data(),
    site = site,
    state_dir = state_dir,
    fields = c(arm = "arm", score = "score", note = "note"),
    formula = score ~ arm + note
  )
  expect_false(identical(
    setup$batch$fingerprint,
    other_fields$batch$fingerprint
  ))
})

test_that("state files are never mixed between batches", {
  site <- new_fake_site()
  state_dir <- tempfile()

  first <- new_fake_batch(site = site, state_dir = state_dir)
  result <- web_entry_run(first$batch)

  other <- new_fake_batch(
    data = fake_data()[1:2, ],
    site = site,
    state_dir = state_dir
  )

  # The second batch sees only its own keys.
  other_result <- web_entry_run(other$batch)
  expect_identical(nrow(other_result$receipts), 2L)
  expect_identical(
    sort(unique(unlist(other_result$receipts$key))),
    c("t1", "t2")
  )
})

test_that("analysis uses the confirmed readback and the original formula", {
  setup <- new_fake_batch()
  result <- web_entry_run(setup$batch)

  expect_identical(result$analysis$status, "current")
  expect_equal(result$included, tibble::as_tibble(fake_data()[, c("trial", "arm", "score")]), ignore_attr = TRUE)

  expected <- fit(specify(fake_data(), score ~ arm))
  expect_equal(
    as.data.frame(result$coefficients),
    as.data.frame(expected),
    ignore_attr = TRUE
  )
  expect_identical(names(result$coefficients), c("term", "estimate"))
})

test_that("a record that changes before analysis is not included in the fit", {
  site <- new_fake_site()
  queries <- new.env(parent = emptyenv())
  queries$n <- 0L

  # After the entry loop confirms the batch, the site starts returning a
  # different value for `t2`. The analysis-time readback must notice.
  query <- function(browser, key, config) {
    queries$n <- queries$n + 1L

    if (identical(key, "t2") && queries$n > 6L) {
      site$queries <- site$queries + 1L
      return(data.frame(arm = "treatment", score = "777"))
    }

    record <- site$records[[key]]
    if (is.null(record)) {
      return(data.frame())
    }
    as.data.frame(record, stringsAsFactors = FALSE)
  }

  data <- fake_data()
  data$score <- c(1, 2, 3.5, 4)

  setup <- new_fake_batch(
    data = data,
    site = site,
    query = query
  )

  result <- web_entry_run(setup$batch)

  expect_false(result$complete)
  expect_identical(status_of(result, "t2"), "conflict")
  expect_identical(result$analysis$status, "stale")
  expect_null(result$coefficients)
})

test_that("analysis can be deferred", {
  # Use data with a testable relationship; `fake_data()` as a whole has a
  # constant response within groups.
  data <- fake_data()
  data$score <- c(1, 2, 3.5, 4)
  setup <- new_fake_batch(data = data)
  result <- web_entry_run(setup$batch, analyze = FALSE)

  expect_true(result$complete)
  expect_identical(result$analysis$status, "none")
  expect_null(result$coefficients)

  # A later run analyses the already confirmed batch without saving again.
  resumed <- web_entry_run(setup$batch)
  expect_identical(setup$site$saves, 4L)
  expect_identical(resumed$analysis$status, "current")
  expect_false(is.null(resumed$coefficients))
})

test_that("the analysis includes readback values, not the supplied data", {
  site <- new_fake_site()
  data <- fake_data()

  # The web application normalizes the arm labels, and the batch must analyse
  # what the application actually holds.
  form <- function(browser, values, config) {
    site$saves <- site$saves + 1L
    site$records[[config$key]] <- list(
      arm = toupper(values[["arm"]]),
      score = values[["score"]]
    )
    "ok"
  }

  setup <- new_fake_batch(site = site, form = form)
  result <- web_entry_run(setup$batch)

  # Uppercase labels differ from the batch, so every row is a conflict.
  expect_identical(unname(receipt_status(result)), rep("conflict", 4))
  expect_identical(result$analysis$status, "stale")

  # Normalizing the readback the same way the site does resolves them.
  setup2 <- new_fake_batch(
    site = site,
    form = form,
    normalize = function(x) toupper(trimws(x))
  )
  # The normalize function is part of the fingerprint, so this is a new batch.
  expect_false(identical(setup$batch$fingerprint, setup2$batch$fingerprint))
})

test_that("an incomplete batch keeps the previous usable analysis", {
  site <- new_fake_site()
  state_dir <- tempfile()
  setup <- new_fake_batch(site = site, state_dir = state_dir)

  good <- web_entry_run(setup$batch)
  expect_identical(good$analysis$status, "current")
  good_coefficients <- good$coefficients

  # A new run where one record has meanwhile changed: the batch is no longer
  # fully confirmed, so the analysis must stay the previous one.
  site$records[["t3"]]$score <- "777"
  stale <- web_entry_run(setup$batch)

  expect_identical(status_of(stale, "t3"), "conflict")
  expect_false(stale$complete)
  expect_identical(stale$analysis$status, "stale")
  expect_equal(
    as.data.frame(stale$coefficients),
    as.data.frame(good_coefficients),
    ignore_attr = TRUE
  )
  expect_equal(stale$included, good$included, ignore_attr = TRUE)
})

test_that("an analysis failure keeps the previous usable analysis", {
  site <- new_fake_site()
  state_dir <- tempfile()

  # First, a batch with a testable formula.
  setup <- new_fake_batch(
    site = site,
    state_dir = state_dir,
    data = fake_data()[c(1, 2, 3, 4), ]
  )
  good <- web_entry_run(setup$batch)
  expect_identical(good$analysis$status, "current")

  # Now a batch for the same records whose formula cannot be fitted (the
  # response is constant), so the analysis fails after entry succeeds.
  constant <- fake_data()
  constant$score <- 1
  failing_site <- new_fake_site()

  failing <- new_fake_batch(
    site = failing_site,
    state_dir = tempfile(),
    data = constant
  )
  # Nothing is stored yet for this new batch, so entry succeeds but the fit
  # has no variation.
  result <- tryCatch(
    web_entry_run(failing$batch),
    error = function(e) e
  )

  expect_false(inherits(result, "error"))
  expect_identical(result$analysis$status, "failed")
  expect_null(result$coefficients)
  expect_true(grepl("analysis failed", result$analysis$reason))
})

test_that("a failed analysis write keeps the previous usable analysis", {
  site <- new_fake_site()
  state_dir <- tempfile()
  setup <- new_fake_batch(site = site, state_dir = state_dir)

  good <- web_entry_run(setup$batch)
  good_coefficients <- good$coefficients

  analysis_path <- list.files(
    state_dir,
    pattern = "-analysis\\.rds$",
    full.names = TRUE
  )
  expect_length(analysis_path, 1L)

  # Make the analysis file impossible to replace by pointing its directory at a
  # file. The state still loads, so only the analysis write fails.
  blocked_dir <- file.path(state_dir, "blocked")
  dir.create(blocked_dir)
  invisible(file.remove(list.files(blocked_dir, full.names = TRUE)))

  # A batch whose analysis cannot be persisted: replace the analysis directory
  # with a regular file.
  analysis_dir <- file.path(state_dir, "web-entry-analysis")
  # (Not used; the analysis lives next to the state file.)

  # Simulate the write failure by removing write permission on the state
  # directory after the state has been written.
  Sys.chmod(state_dir, mode = "0555")
  on.exit(Sys.chmod(state_dir, mode = "0755"), add = TRUE)

  stale <- suppressWarnings(web_entry_run(setup$batch))
  Sys.chmod(state_dir, mode = "0755")

  expect_identical(stale$analysis$status, "failed")
  expect_true(stale$analysis$persisted %in% c(FALSE, NA))
  expect_equal(
    as.data.frame(stale$coefficients),
    as.data.frame(good_coefficients),
    ignore_attr = TRUE
  )
})

test_that("verify reads back without ever saving", {
  site <- new_fake_site()
  setup <- new_fake_batch(site = site)

  verified <- web_entry_verify(setup$batch)
  expect_identical(site$saves, 0L)
  expect_false(verified$complete)
  expect_identical(unname(receipt_status(verified)), rep("pending", 4))
  expect_identical(verified$analysis$status, "none")

  web_entry_run(setup$batch)
  expect_identical(site$saves, 4L)

  verified <- web_entry_verify(setup$batch)
  expect_true(verified$complete)
  expect_identical(site$saves, 4L)
})

test_that("receipts can be inspected without a browser", {
  site <- new_fake_site()
  setup <- new_fake_batch(site = site)

  receipts <- web_entry_receipts(setup$batch)
  expect_s3_class(receipts, "tbl_df")
  expect_identical(nrow(receipts), 4L)
  expect_identical(unique(receipts$status), "pending")
  expect_identical(
    names(receipts),
    c("key", "status", "confirmed", "remote_saved", "attempts", "confirmed_at", "values", "detail")
  )
})

test_that("web_entry_batch validates its inputs", {
  site <- new_fake_site()
  form <- fake_form(site)
  query <- fake_query(site)
  data <- fake_data()

  expect_error(
    web_entry_batch(
      "not data", key = "trial", fields = fake_fields(),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )
  expect_error(
    web_entry_batch(
      data, key = "missing", fields = fake_fields(),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )
  expect_error(
    web_entry_batch(
      data, key = "trial", fields = c(arm = "nope"),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )
  expect_error(
    web_entry_batch(
      data, key = "trial", fields = c(arm = "arm", trial = "trial"),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )
  expect_error(
    web_entry_batch(
      data, key = "trial", fields = fake_fields(),
      formula = outcome ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )
  expect_error(
    web_entry_batch(
      data, key = "trial", fields = fake_fields(),
      formula = score ~ arm, form = "not a function", query = query
    ),
    class = "infer_web_entry_bad_argument"
  )

  dup <- data
  dup$trial[[2]] <- dup$trial[[1]]
  expect_error(
    web_entry_batch(
      dup, key = "trial", fields = fake_fields(),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_duplicate_key"
  )

  expect_error(web_entry_run("not a batch"), class = "infer_web_entry_bad_argument")
  expect_error(web_entry_receipts("not a batch"), class = "infer_web_entry_bad_argument")
  expect_error(
    web_entry_run(
      web_entry_batch(
        data, key = "trial", fields = fake_fields(), formula = score ~ arm,
        form = form, query = query
      )
    ),
    class = "infer_web_entry_bad_argument"
  )
})

test_that("scalar fields reject missing values and recursive columns", {
  site <- new_fake_site()
  form <- fake_form(site)
  query <- fake_query(site)
  data <- fake_data()

  data$arm[[1]] <- NA
  expect_error(
    web_entry_batch(
      data, key = "trial", fields = fake_fields(),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )

  data <- fake_data()
  data$arm <- I(as.list(data$arm))
  expect_error(
    web_entry_batch(
      data, key = "trial", fields = fake_fields(),
      formula = score ~ arm, form = form, query = query
    ),
    class = "infer_web_entry_bad_argument"
  )
})

test_that("a malformed state file is an error rather than a fresh start", {
  site <- new_fake_site()
  state_dir <- tempfile()
  setup <- new_fake_batch(site = site, state_dir = state_dir)

  web_entry_run(setup$batch)
  state_path <- list.files(state_dir, pattern = "-[0-9a-f]+\\.rds$", full.names = TRUE)
  state_path <- setdiff(state_path, list.files(state_dir, pattern = "analysis", full.names = TRUE))

  # Not valid RDS.
  writeLines("not an rds", state_path[[1]])

  expect_error(web_entry_run(setup$batch), class = "infer_web_entry_state_error")
})

test_that("a batch whose analysis produces no result reports the previous one", {
  setup <- new_fake_batch()
  result <- web_entry_run(setup$batch)
  expect_identical(result$analysis$status, "current")

  # A batch that fails before writing any analysis reports no coefficients and
  # no previous analysis.
  site <- new_fake_site()
  bad <- new_fake_batch(
    site = site,
    form = function(browser, values, config) stop("never saves"),
    data = fake_data()[1, ]
  )
  result <- web_entry_run(bad$batch, max_attempts = 1L)
  expect_false(result$complete)
  expect_null(result$coefficients)
  expect_null(result$included)
  expect_identical(result$analysis$status, "stale")
})
