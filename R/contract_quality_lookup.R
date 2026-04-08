#' Shared survival-based contract-quality lookup
#'
#' Internal helper that builds a single named numeric vector of quality
#' scores indexed by contract type, derived from Kaplan-Meier median
#' survival durations. Consumed by `career_profile()` and by
#' `compute_temporal_employment_indicators()` to guarantee one source of
#' truth for contract quality. Fixed weights are explicitly forbidden by
#' project policy (`CLAUDE.md` CRITICAL CORRECTION).
#'
#' @name contract_quality_lookup
#' @keywords internal
NULL

# 1. Build survival-based quality lookup -----

#' Build a contract-type quality lookup from survival medians
#'
#' Runs `estimate_contract_survival_optimized()` on the full input and
#' converts median survival times to quality scores on the `[0.3, 1.0]`
#' scale (longer median survival -> higher quality). The same linear
#' normalization is used by the legacy `.compute_contract_quality()` helper
#' in `temporal_indicators.R`, so outputs remain comparable.
#'
#' @param data A `data.table` with at least `COD_TIPOLOGIA_CONTRATTUALE`,
#'   `durata`, and `fine` columns. A `censored` column is added on the fly
#'   when missing: contracts ending at `max(fine)` are treated as
#'   right-censored.
#' @param contract_type_var Character. Name of the contract-type column.
#'   Default `"COD_TIPOLOGIA_CONTRATTUALE"`.
#' @param duration_var Character. Name of the duration column. Default
#'   `"durata"`.
#' @param end_date_var Character. Name of the contract end-date column used
#'   to derive censoring. Default `"fine"`.
#' @param min_spell_duration Numeric. Minimum duration (days) required to
#'   include a record in the survival estimation. Default `1`.
#'
#' @return A named numeric vector of quality scores in `[0.3, 1.0]`,
#'   indexed by contract type. Returns an empty named numeric vector (with
#'   a `message()`) when the required column is missing, the data is
#'   empty, or survival estimation fails.
#'
#' @keywords internal
#' @noRd
.build_contract_quality_lookup <- function(
  data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  duration_var = "durata",
  end_date_var = "fine",
  min_spell_duration = 1L
) {
  empty <- stats::setNames(numeric(0), character(0))

  if (!data.table::is.data.table(data) || nrow(data) == 0L) {
    return(empty)
  }
  if (!contract_type_var %in% names(data)) {
    message(
      "Contract-type column '",
      contract_type_var,
      "' not found; contract_quality will be NA."
    )
    return(empty)
  }
  if (!duration_var %in% names(data) || !end_date_var %in% names(data)) {
    return(empty)
  }

  # 1.1 Slice to usable rows -----
  keep <- !is.na(data[[contract_type_var]]) &
    !is.na(data[[duration_var]]) &
    data[[duration_var]] >= min_spell_duration
  if (!any(keep)) {
    return(empty)
  }
  work <- data[
    keep,
    c(contract_type_var, duration_var, end_date_var),
    with = FALSE
  ]

  # 1.2 Attach censoring indicator (contracts ending at max(fine) censored) -----
  max_end <- max(work[[end_date_var]], na.rm = TRUE)
  work[, censored := as.integer(get(end_date_var) == max_end)]

  # 1.3 Run survival estimation -----
  med <- tryCatch(
    {
      res <- estimate_contract_survival_optimized(
        data = work,
        contract_type_var = contract_type_var,
        duration_var = duration_var,
        censored_var = "censored"
      )
      res$median_survival
    },
    error = function(e) {
      message(
        "Survival-based quality lookup failed (",
        conditionMessage(e),
        "); contract_quality will be NA."
      )
      NULL
    },
    warning = function(w) {
      # Re-run without the warning handler so we still capture the result,
      # but only if the underlying call succeeds.
      tryCatch(
        {
          res <- suppressWarnings(
            estimate_contract_survival_optimized(
              data = work,
              contract_type_var = contract_type_var,
              duration_var = duration_var,
              censored_var = "censored"
            )
          )
          res$median_survival
        },
        error = function(e) NULL
      )
    }
  )

  if (is.null(med) || length(med) == 0L) {
    return(empty)
  }

  # 1.4 Linear rescale median survival to [0.3, 1.0] -----
  valid <- med[!is.na(med) & is.finite(med)]
  if (length(valid) == 0L) {
    return(empty)
  }

  max_m <- max(valid)
  min_m <- min(valid)

  scores <- if (max_m > min_m) {
    out <- 0.3 + 0.7 * (med - min_m) / (max_m - min_m)
    out[is.na(out) | !is.finite(out)] <- 0.3
    out
  } else {
    stats::setNames(rep(0.7, length(med)), names(med))
  }

  # Preserve original ordering and names
  stats::setNames(as.numeric(scores), names(med))
}
