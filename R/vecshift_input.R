#' Internal helpers for the vecshift v2 input contract
#'
#' These helpers centralize the validation that every public entry point
#' consuming `vecshift` output performs. They check that the input is a
#' `data.table`, that it carries the columns the caller actually needs, and
#' (when `vecshift` >= 2.0.0 is installed) that the object is a
#' `vecshift_result` produced with day-level granularity. Class and metadata
#' are reattached to consolidation outputs so downstream code keeps seeing
#' the same S3 type.
#'
#' @name vecshift_input_helpers
#' @keywords internal
NULL

# 1. Input assertion -----

#' Validate a vecshift input object
#'
#' @param x Object to validate. Expected to be a `data.table`, possibly of
#'   class `vecshift_result` when produced by vecshift >= 2.0.0.
#' @param required_cols Character vector of columns that the caller will
#'   read. The default is the minimal set common to every vecshift output.
#'
#' @return `invisible(TRUE)` on success. Errors on hard failures
#'   (non-data.table, missing columns, unsupported granularity); warns when
#'   the object is not a `vecshift_result` but is otherwise usable.
#'
#' @keywords internal
#' @noRd
.assert_vecshift_input <- function(
  x,
  required_cols = c("cf", "inizio", "fine", "durata", "arco")
) {
  if (!data.table::is.data.table(x)) {
    stop(
      "Input must be a data.table (vecshift output).",
      call. = FALSE
    )
  }

  has_vecshift <- requireNamespace("vecshift", quietly = TRUE)
  if (has_vecshift) {
    vs_version <- tryCatch(
      utils::packageVersion("vecshift"),
      error = function(e) NULL
    )
    if (
      !is.null(vs_version) &&
        vs_version >= "2.0.0" &&
        exists("is_vecshift_result", envir = asNamespace("vecshift"))
    ) {
      is_result <- vecshift::is_vecshift_result(x)
      if (!is_result) {
        warning(
          "Input is not a vecshift_result object; results assume vecshift v2 ",
          "day-granularity semantics.",
          call. = FALSE
        )
      } else {
        meta <- attr(x, "vecshift_metadata")
        if (
          !is.null(meta) &&
            !is.null(meta$granularity) &&
            !identical(meta$granularity, "day")
        ) {
          stop(
            "longworkR supports only day-granularity vecshift output (got '",
            meta$granularity,
            "').",
            call. = FALSE
          )
        }
      }
    }
  }

  miss <- setdiff(required_cols, names(x))
  if (length(miss)) {
    stop(
      "Missing required columns: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# 2. Class preservation -----

#' Reattach the vecshift_result class and metadata to a derived data.table
#'
#' @param out A `data.table` produced by a longworkR transformation.
#' @param original The input object passed by the caller. If it carries the
#'   `vecshift_result` class, the same class and `vecshift_metadata`
#'   attribute are propagated to `out`.
#'
#' @return `out`, possibly with the `vecshift_result` class prepended and
#'   the metadata attribute reattached. A no-op for plain data.tables.
#'
#' @keywords internal
#' @noRd
.preserve_vecshift_class <- function(out, original) {
  if (
    inherits(original, "vecshift_result") &&
      data.table::is.data.table(out) &&
      !inherits(out, "vecshift_result")
  ) {
    class(out) <- unique(c("vecshift_result", class(out)))
    attr(out, "vecshift_metadata") <- attr(original, "vecshift_metadata")
  }
  out
}
