# employment_summaries.R
# Summary utilities for consolidated employment data
# Migrated from the COB data pipelines (data_pipeline/R/helpers.R)
# Author: Giampaolo Montaletti (giampaolo.montaletti@gmail.com)

# 1. Consolidation stage comparison -----

#' Compare consolidation stages
#'
#' Builds a compact comparison report across consolidation stages of an
#' employment data pipeline. For each stage it reports the number of records
#' and the number of unique persons, making it easy to verify how much each
#' consolidation step reduces the dataset.
#'
#' @param stages A named list of data.tables (or data.frames), one per
#'   consolidation stage, each containing a `cf` person identifier column.
#'   List names are used as stage labels.
#'
#' @return A data.table with one row per stage and columns:
#'   \itemize{
#'     \item `stage`: stage label (list name)
#'     \item `records`: number of rows in the stage dataset
#'     \item `unique_persons`: number of distinct `cf` values
#'   }
#'
#' @examples
#' raw <- data.table::data.table(cf = c("A", "A", "B", "C"))
#' consolidated <- data.table::data.table(cf = c("A", "B", "C"))
#' compare_consolidation_stages(list(raw = raw, consolidated = consolidated))
#'
#' @export
compare_consolidation_stages <- function(stages) {
  # Input validation
  if (!is.list(stages) || is.data.frame(stages)) {
    stop("'stages' must be a list of data.tables, one per consolidation stage")
  }
  if (length(stages) == 0) {
    stop("'stages' must contain at least one consolidation stage")
  }
  if (is.null(names(stages)) || any(!nzchar(names(stages)))) {
    stop("All elements of 'stages' must be named (stage labels)")
  }
  is_valid <- vapply(
    stages,
    function(x) is.data.frame(x) && "cf" %in% names(x),
    logical(1)
  )
  if (any(!is_valid)) {
    stop(
      "All stages must be data.tables (or data.frames) with a 'cf' column. ",
      "Invalid stages: ",
      paste(names(stages)[!is_valid], collapse = ", ")
    )
  }

  data.table(
    stage = names(stages),
    records = unname(vapply(stages, nrow, integer(1))),
    unique_persons = unname(vapply(
      stages,
      function(x) uniqueN(x[["cf"]]),
      integer(1)
    ))
  )
}

# 2. Employment statistics -----

#' Extract employment statistics
#'
#' Computes summary statistics from consolidated employment data produced by
#' the vecshift/longworkR consolidation chain. Employment and unemployment
#' spells are distinguished via the `arco` indicator (`arco == 1` employment,
#' `arco == 0` unemployment); durations are taken from the `durata` column.
#' On consolidated data overlaps are already resolved, so `arco` takes only
#' the values 0 and 1.
#'
#' @param data A data.table (or data.frame) of consolidated employment spells
#'   containing at least the columns `cf` (person identifier), `durata`
#'   (spell duration in days), and `arco` (concurrent employment indicator,
#'   0 for unemployment spells).
#'
#' @return A named list with elements:
#'   \itemize{
#'     \item `total_records`: total number of spells
#'     \item `unique_persons`: number of distinct `cf` values
#'     \item `total_employment_days`: sum of `durata` for employment spells
#'     \item `total_unemployment_days`: sum of `durata` for unemployment spells
#'     \item `avg_employment_duration`: mean `durata` of employment spells
#'     \item `avg_unemployment_duration`: mean `durata` of unemployment spells
#'   }
#'   `NA` values in `durata` are ignored (`na.rm = TRUE`).
#'
#' @examples
#' dt <- data.table::data.table(
#'   cf = c("A", "A", "B"),
#'   durata = c(100, 30, 200),
#'   arco = c(1, 0, 1)
#' )
#' employment_statistics(dt)
#'
#' @export
employment_statistics <- function(data) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.table or data.frame")
  }
  required_cols <- c("cf", "durata", "arco")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "'data' is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  durata <- data[["durata"]]
  arco <- data[["arco"]]
  employed <- !is.na(arco) & arco == 1
  unemployed <- !is.na(arco) & arco == 0

  list(
    total_records = nrow(data),
    unique_persons = uniqueN(data[["cf"]]),
    total_employment_days = sum(durata[employed], na.rm = TRUE),
    total_unemployment_days = sum(durata[unemployed], na.rm = TRUE),
    avg_employment_duration = mean(durata[employed], na.rm = TRUE),
    avg_unemployment_duration = mean(durata[unemployed], na.rm = TRUE)
  )
}
