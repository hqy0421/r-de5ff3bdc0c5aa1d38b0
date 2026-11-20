# Internal helpers for resumable batch web entry -----------------------------
#
# The web entry subsystem stores *observations* in a remote web application and
# then uses only observations that were read back from that application to
# power `specify()`/`fit()`. All helpers in this file are internal.

web_entry_normalize_value <- function(x) {
  trimws(as.character(x))
}

web_entry_call_error <- function(
  message,
  class,
  call = rlang::caller_env(),
  envir = rlang::caller_env()
) {
  # `envir` is where cli evaluates `{}` expressions in `message`; by default it
  # must be the caller of this helper, not this helper's own frame.
  cli_abort(
    message,
    class = c(class, "infer_web_entry_error"),
    call = call,
    .envir = envir
  )
}

web_entry_check_chr_scalar <- function(x, name, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    web_entry_call_error(
      "`{name}` must be a single non-missing, non-empty string.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  x
}

web_entry_check_named_chr <- function(x, name, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) == 0L || anyNA(x) || any(!nzchar(x))) {
    web_entry_call_error(
      "`{name}` must be a non-empty character vector of non-missing values.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  if (is.null(names(x)) || all(!nzchar(names(x)))) {
    names(x) <- unname(x)
  } else if (any(!nzchar(names(x)))) {
    web_entry_call_error(
      "All elements of `{name}` must be named.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  if (anyDuplicated(names(x))) {
    web_entry_call_error(
      "Names of `{name}` must be unique.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  x
}

# Deterministic digest used to bind a batch to its data and configuration.
# Only plain lists/atomic vectors are passed here, so both `rlang::hash()` and
# serialization are stable across sessions of the same R build.
web_entry_hash <- function(x) {
  rlang::hash(x)
}

web_entry_now <- function() {
  Sys.time()
}

# Column type handling -------------------------------------------------------

web_entry_column_kind <- function(x) {
  if (is.factor(x)) {
    "factor"
  } else if (inherits(x, "Date")) {
    "Date"
  } else if (inherits(x, "POSIXct")) {
    "POSIXct"
  } else if (is.logical(x)) {
    "logical"
  } else if (is.integer(x)) {
    "integer"
  } else if (is.double(x)) {
    "double"
  } else if (is.character(x)) {
    "character"
  } else {
    "other"
  }
}

web_entry_type_spec <- function(x) {
  kind <- web_entry_column_kind(x)

  list(
    kind = kind,
    class = paste(class(x), collapse = "/"),
    levels = if (kind == "factor") levels(x) else NULL,
    ordered = if (kind == "factor") is.ordered(x) else NULL,
    tz = if (kind == "POSIXct") attr(x, "tzone") else NULL
  )
}

web_entry_cast <- function(x, spec) {
  switch(
    spec$kind,
    factor = factor(
      x,
      levels = spec$levels,
      ordered = isTRUE(spec$ordered)
    ),
    Date = as.Date(x),
    POSIXct = as.POSIXct(
      x,
      tz = if (is.null(spec$tz)) "UTC" else spec$tz[[1]]
    ),
    logical = as.logical(x),
    integer = as.integer(x),
    double = as.numeric(x),
    character = as.character(x),
    as.character(x)
  )
}

# Values used for fingerprints. These are lossless enough to notice any change
# of the supplied input while remaining deterministic across sessions.
web_entry_fingerprint_values <- function(x) {
  kind <- web_entry_column_kind(x)

  switch(
    kind,
    factor = as.character(x),
    Date = format(x, "%Y-%m-%d"),
    POSIXct = format(
      x,
      "%Y-%m-%dT%H:%M:%OS6",
      tz = {
        tz <- attr(x, "tzone")
        if (is.null(tz) || !nzchar(tz[[1]])) "UTC" else tz[[1]]
      }
    ),
    logical = as.character(x),
    integer = as.character(x),
    double = sprintf("%.17g", x),
    character = x,
    as.character(x)
  )
}

web_entry_fingerprint_column <- function(x) {
  list(
    spec = web_entry_type_spec(x),
    values = web_entry_fingerprint_values(x)
  )
}

web_entry_fingerprint_data <- function(data) {
  list(
    names = names(data),
    nrow = nrow(data),
    columns = lapply(data, web_entry_fingerprint_column)
  )
}

# Validation helpers ---------------------------------------------------------

web_entry_check_data <- function(data, call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    web_entry_call_error(
      "`data` must be a data frame of scalar observations.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  data <- tibble::as_tibble(data)

  if (nrow(data) == 0L) {
    web_entry_call_error(
      "`data` must contain at least one row.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  if (ncol(data) == 0L) {
    web_entry_call_error(
      "`data` must contain at least one column.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  bad <- names(data)[
    !vapply(
      data,
      function(x) is.atomic(x) && is.null(dim(x)),
      logical(1)
    )
  ]

  if (length(bad) > 0L) {
    web_entry_call_error(
      c(
        "Every column of `data` must be a scalar atomic vector.",
        x = "Unsupported column{?s}: {.field {bad}}.",
        i = "List columns, matrix columns, and other recursive columns cannot be entered as scalar fields."
      ),
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  data
}

web_entry_check_fields <- function(
  fields,
  data,
  call = rlang::caller_env()
) {
  fields <- web_entry_check_named_chr(fields, "fields", call = call)

  missing_cols <- setdiff(unname(fields), names(data))

  if (length(missing_cols) > 0L) {
    web_entry_call_error(
      c(
        "Every column referenced by `fields` must exist in `data`.",
        x = "Missing column{?s}: {.field {missing_cols}}."
      ),
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  if ("key" %in% names(fields)) {
    web_entry_call_error(
      'The field name `"key"` is reserved for the business key column.',
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  has_na <- vapply(
    unname(fields),
    function(col) anyNA(data[[col]]),
    logical(1)
  )

  if (any(has_na)) {
    web_entry_call_error(
      c(
        "Scalar field values may not be missing.",
        x = "Column{?s} with missing values: {.field {unname(fields)[has_na]}}.",
        i = "Filter or repair `data` before creating a batch."
      ),
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  fields
}

web_entry_check_key <- function(
  key,
  data,
  normalize,
  call = rlang::caller_env()
) {
  web_entry_check_chr_scalar(key, "key", call = call)

  if (!(key %in% names(data))) {
    web_entry_call_error(
      "`key` must name a column in `data`.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  key_values <- data[[key]]

  if (anyNA(key_values)) {
    web_entry_call_error(
      "The business key column may not contain missing values.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  key_norm <- vapply(
    seq_along(key_values),
    function(i) web_entry_normalize_with(normalize, key_values[[i]]),
    character(1)
  )

  if (any(!nzchar(key_norm))) {
    web_entry_call_error(
      "The business key column may not contain empty values.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  duplicated_keys <- unique(key_norm[duplicated(key_norm)])

  if (length(duplicated_keys) > 0L) {
    web_entry_call_error(
      c(
        "Targets must be unique by business key.",
        x = "Duplicate key{?s}: {.val {duplicated_keys}}.",
        i = "One key may describe only one observation."
      ),
      class = "infer_web_entry_duplicate_key",
      call = call
    )
  }

  key_norm
}

web_entry_check_formula <- function(
  formula,
  field_names,
  call = rlang::caller_env()
) {
  if (is.character(formula)) {
    if (length(formula) != 1L || is.na(formula)) {
      web_entry_call_error(
        "`formula` must be a single formula or a single string.",
        class = "infer_web_entry_bad_argument",
        call = call
      )
    }

    formula <- stats::as.formula(formula)
  }

  if (!rlang::is_formula(formula)) {
    web_entry_call_error(
      "`formula` must be a formula (or a string that can be parsed as one).",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  vars <- all.vars(formula)

  if ("." %in% vars) {
    web_entry_call_error(
      c(
        "`.` is not supported in the batch `formula`.",
        i = "Name the response and explanatory fields explicitly."
      ),
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  unknown <- setdiff(vars, field_names)

  if (length(unknown) > 0L) {
    web_entry_call_error(
      c(
        "Every variable in `formula` must be a batch field.",
        x = "Not a field: {.field {unknown}}.",
        i = "Available fields: {.field {field_names}}."
      ),
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  formula
}

web_entry_check_success <- function(success, call = rlang::caller_env()) {
  if (!is.null(success)) {
    web_entry_check_chr_scalar(success, "success", call = call)
  }

  success
}

web_entry_normalize_with <- function(normalize, x) {
  out <- normalize(x)

  if (!is.character(out) || length(out) != 1L || is.na(out)) {
    web_entry_call_error(
      c(
        "`normalize` must return a single non-missing character value.",
        x = "Got {.obj_type_friendly {out}} of length {length(out)}."
      ),
      class = "infer_web_entry_bad_argument",
      call = rlang::caller_env()
    )
  }

  out
}

web_entry_check_normalize <- function(
  normalize,
  data,
  fields,
  call = rlang::caller_env()
) {
  if (is.null(normalize)) {
    normalize <- web_entry_normalize_value
    return(normalize)
  }

  if (!rlang::is_function(normalize)) {
    web_entry_call_error(
      "`normalize` must be `NULL` or a function.",
      class = "infer_web_entry_bad_argument",
      call = call
    )
  }

  sample_value <- data[[unname(fields)[[1]]]][[1]]

  tryCatch(
    web_entry_normalize_with(normalize, sample_value),
    error = function(e) {
      web_entry_call_error(
        c(
          "`normalize` could not normalize a field value.",
          x = conditionMessage(e)
        ),
        class = "infer_web_entry_bad_argument",
        call = call
      )
    }
  )

  normalize
}

web_entry_normalize_description <- function(normalize) {
  paste(
    deparse(formals(normalize), width.cutoff = 500L),
    deparse(body(normalize), width.cutoff = 500L),
    collapse = "\n"
  )
}

# Extract the scalar values of one input row in field order.
web_entry_row_values <- function(data, fields, i) {
  values <- vapply(
    unname(fields),
    function(col) as.character(data[[col]][[i]]),
    character(1)
  )
  names(values) <- names(fields)
  values
}

web_entry_row_expected <- function(data, fields, i, normalize) {
  values <- web_entry_row_values(data, fields, i)
  out <- vapply(
    values,
    function(x) web_entry_normalize_with(normalize, x),
    character(1)
  )
  names(out) <- names(values)
  out
}

web_entry_key_values <- function(batch) {
  key_values <- batch$data[[batch$key]]
  vapply(
    seq_along(key_values),
    function(i) web_entry_normalize_with(batch$normalize, key_values[[i]]),
    character(1)
  )
}

# Query interpretation -------------------------------------------------------

# Interpret the strongly consistent answer of a query for one business key.
# Returns one of:
# - `match`: exactly one record, every field agrees with the expected values
# - `missing`: no record for the key
# - `conflict`: exactly one record, at least one field disagrees
# - `ambiguous`: more than one record for the key
# - `invalid`: the adapter returned something that cannot be interpreted
web_entry_interpret_query <- function(value, expected, normalize) {
  if (!is.data.frame(value)) {
    return(list(
      state = "invalid",
      detail = "The query adapter must return a data frame of current records."
    ))
  }

  value <- tibble::as_tibble(value)

  if (nrow(value) == 0L) {
    return(list(state = "missing", detail = "No record exists for the business key."))
  }

  if (nrow(value) > 1L) {
    return(list(
      state = "ambiguous",
      detail = glue::glue("The business key matched {nrow(value)} records.")
    ))
  }

  missing_fields <- setdiff(names(expected), names(value))

  if (length(missing_fields) > 0L) {
    return(list(
      state = "invalid",
      detail = paste0(
        "The current record lacks field(s): ",
        paste(missing_fields, collapse = ", ")
      )
    ))
  }

  observed <- vapply(
    names(expected),
    function(field) {
      tryCatch(
        web_entry_normalize_with(normalize, value[[field]][[1]]),
        error = function(e) NA_character_
      )
    },
    character(1)
  )
  names(observed) <- names(expected)

  if (anyNA(observed)) {
    return(list(
      state = "invalid",
      detail = "The current record contains a value that could not be normalized."
    ))
  }

  same <- vapply(
    names(expected),
    function(field) identical(expected[[field]], observed[[field]]),
    logical(1)
  )

  if (all(same)) {
    return(list(
      state = "match",
      observed = as.list(observed),
      detail = ""
    ))
  }

  list(
    state = "conflict",
    observed = as.list(observed),
    mismatched = names(expected)[!same],
    detail = paste0(
      "The current record differs in field(s): ",
      paste(names(expected)[!same], collapse = ", "),
      "."
    )
  )
}
