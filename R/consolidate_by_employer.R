#' Consolidate Employment Periods by Employer
#'
#' @description
#' Consolidates consecutive employment periods with the same employer, merging
#' contracts separated by short gaps into single employment spells. Two periods
#' are consolidated if they belong to the same person, the same employer, and
#' the gap between them does not exceed \code{max_gap_days}. Unemployment
#' periods act as barriers that prevent consolidation.
#'
#' @param data data.table with employment records. Must contain columns:
#'   \code{cf}, \code{inizio}, \code{fine}, \code{durata}. The \code{arco} column
#'   is used if present to identify employment vs unemployment periods.
#' @param employer_var Character string specifying the column name containing
#'   employer identifiers (e.g., \code{"datore"}, \code{"employer_id"}).
#' @param max_gap_days Numeric value specifying the maximum gap in days between
#'   consecutive contracts from the same employer to be consolidated (default: 8).
#'   Must be non-negative.
#' @param variable_handling Character string specifying aggregation strategy for variables:
#'   \code{"weight"} uses weighted mean/mode (default), \code{"first"} takes first non-NA value
#'
#' @return data.table with employer-consolidated employment periods. Includes all
#'   original columns plus \code{n_periods_consolidated} indicating how many
#'   periods were merged (1 means no consolidation occurred for that record).
#'
#' @details
#' **What makes periods consolidatable:**
#'
#' Two employment periods are consolidated if:
#' - They belong to the same person (\code{cf})
#' - They share the same employer (\code{employer_var})
#' - The gap between them is at most \code{max_gap_days} days
#' - Both are employment periods (\code{arco > 0} or missing)
#' - There is no unemployment period between them
#'
#' **How unemployment acts as a barrier:**
#'
#' Unemployment periods (\code{arco == 0}) prevent consolidation. For example,
#' if a worker has Employment(Employer A)-Unemployment-Employment(Employer A),
#' these will NOT be consolidated even if the employer is the same and the gap
#' is within threshold.
#'
#' **Difference from other consolidation functions:**
#'
#' - \code{\link{consolidate_overlapping}}: Merges concurrent employment (same \code{over_id})
#' - \code{\link{consolidate_adjacent}}: Merges sequential employment with no gap, regardless of employer
#' - \code{consolidate_by_employer}: Merges sequential employment by the same employer within a gap threshold
#' - \code{\link{consolidate_short_gaps}}: Bridges short unemployment gaps regardless of employer
#'
#' **Aggregation rules:**
#'
#' When consolidating periods, the function delegates aggregation to the shared
#' consolidation engine (\code{.consolidate_groups}):
#' - Uses \code{min(inizio)} and \code{max(fine)} for date range
#' - Recalculates \code{durata} as the full span
#' - Uses weighted mode for qualitative variables (e.g., contract type)
#' - Uses weighted mean for quantitative variables (e.g., salary)
#' - Weights are based on the \code{durata} of each period
#'
#' **Performance:**
#'
#' Fully vectorized implementation with Phase 4 single-period worker bypass
#' optimization. Workers with only one period are excluded from consolidation
#' logic since no same-employer merging is possible.
#'
#' **Composability:**
#'
#' This function is designed to be chained with other consolidation functions:
#' \preformatted{
#' data |>
#'   consolidate_overlapping() |>       # First merge concurrent
#'   consolidate_by_employer("datore") |> # Then merge same-employer
#'   consolidate_adjacent() |>           # Then merge adjacent
#'   consolidate_short_gaps(30)          # Finally bridge short gaps
#' }
#'
#' The recommended position for employer consolidation is after overlapping
#' consolidation and before adjacent/gap consolidation.
#'
#' @examples
#' \dontrun{
#' # Basic: Consolidate contracts with the same employer
#' data <- data.table::data.table(
#'   cf = rep(1, 4),
#'   inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-04-01")),
#'   fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30", "2023-06-30")),
#'   durata = c(90, 91, 92, 91),
#'   arco = c(1, 1, 1, 1),
#'   datore = c("A", "A", "A", "B")
#' )
#'
#' result <- consolidate_by_employer(data, employer_var = "datore")
#' nrow(result)  # 2 (employer A periods merged, employer B separate)
#'
#' # With gap threshold: only merge if gap <= max_gap_days
#' data2 <- data.table::data.table(
#'   cf = rep(1, 3),
#'   inizio = as.Date(c("2023-01-01", "2023-04-05", "2023-07-01")),
#'   fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
#'   durata = c(90, 87, 92),
#'   arco = c(1, 1, 1),
#'   datore = c("A", "A", "A")
#' )
#'
#' # Gap between first two: 5 days (merged with default max_gap_days=8)
#' result1 <- consolidate_by_employer(data2, "datore", max_gap_days = 8)
#' nrow(result1)  # All merged into 1 record
#'
#' # Gap between first two: 5 days (NOT merged with max_gap_days=3)
#' result2 <- consolidate_by_employer(data2, "datore", max_gap_days = 3)
#' nrow(result2)  # 2 records (first separate, last two merged)
#'
#' # Chaining in a pipeline
#' data <- readRDS("data/sample.rds")
#' result <- data |>
#'   consolidate_overlapping() |>
#'   consolidate_by_employer("datore") |>
#'   consolidate_adjacent() |>
#'   consolidate_short_gaps(30)
#'
#' cat("Original records:", nrow(data), "\n")
#' cat("After consolidation:", nrow(result), "\n")
#' }
#'
#' @seealso
#' \code{\link{consolidate_overlapping}} for concurrent employment consolidation
#'
#' \code{\link{consolidate_adjacent}} for contiguous period consolidation
#'
#' \code{\link{consolidate_short_gaps}} for gap-bridging consolidation
#'
#' \code{\link{consolidation_helpers}} for internal aggregation functions
#'
#' @export
consolidate_by_employer <- function(
  data,
  employer_var,
  max_gap_days = 8,
  variable_handling = "weight"
) {
  # 1. Input validation -----
  if (!inherits(data, "data.table")) {
    stop("data must be a data.table")
  }

  if (
    missing(employer_var) ||
      !is.character(employer_var) ||
      length(employer_var) != 1
  ) {
    stop(
      "employer_var must be a single character string specifying the employer column name"
    )
  }

  if (!employer_var %in% names(data)) {
    stop("employer_var '", employer_var, "' not found in data columns")
  }

  if (
    !is.numeric(max_gap_days) || length(max_gap_days) != 1 || max_gap_days < 0
  ) {
    stop("max_gap_days must be a single non-negative numeric value")
  }

  if (!variable_handling %in% c("weight", "first")) {
    stop("variable_handling must be 'weight' or 'first'")
  }

  # Check required columns
  required <- c("cf", "inizio", "fine", "durata")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # 2. Handle empty dataset -----
  if (nrow(data) == 0) {
    result <- data.table::copy(data)
    result[, n_periods_consolidated := integer()]
    return(result)
  }

  # 3. Work on copy to avoid modifying original -----
  dt <- data.table::copy(data)

  # Ensure date columns are Date type
  if (!inherits(dt$inizio, "Date")) {
    dt[, inizio := as.Date(inizio)]
  }
  if (!inherits(dt$fine, "Date")) {
    dt[, fine := as.Date(fine)]
  }

  # Add arco column if missing (assume employment)
  if (!"arco" %in% names(dt)) {
    dt[, arco := 1L]
  }

  # Sort by person, employer, and start date
  data.table::setkeyv(dt, c("cf", employer_var, "inizio"))

  # 4. Phase 4 optimization: single-period worker bypass -----
  dt[, .n_periods_temp := .N, by = cf]

  skip_mask <- dt$.n_periods_temp == 1L
  skip_records <- if (any(skip_mask)) {
    dt[skip_mask]
  } else {
    data.table::data.table()
  }
  process_records <- if (any(!skip_mask)) {
    dt[!skip_mask]
  } else {
    data.table::data.table()
  }

  if (nrow(skip_records) > 0) {
    skip_records[, .n_periods_temp := NULL]
  }
  if (nrow(process_records) > 0) {
    process_records[, .n_periods_temp := NULL]
  }

  # 5. Core grouping logic -----
  if (nrow(process_records) > 0) {
    # Create shift columns by cf to detect employer changes and gaps
    process_records[,
      `:=`(
        prev_employer = data.table::shift(get(employer_var), 1L, type = "lag"),
        prev_fine = data.table::shift(fine, 1L, type = "lag"),
        prev_arco = data.table::shift(arco, 1L, type = "lag")
      ),
      by = cf
    ]

    # Calculate gap in days between current start and previous end
    process_records[,
      gap_days := data.table::fifelse(
        is.na(prev_fine),
        NA_integer_,
        as.integer(inizio - prev_fine - 1L)
      )
    ]

    # Detect new group starts (vectorized)
    # Start new group when:
    # - First record for person (prev_fine is NA)
    # - Employer changes
    # - Gap > max_gap_days
    # - Previous was unemployment
    # - Current is unemployment
    process_records[,
      new_group := is.na(prev_fine) |
        get(employer_var) != prev_employer |
        gap_days > max_gap_days |
        prev_arco == 0L |
        arco == 0L
    ]

    # Handle NA in employer comparison (treat NA employer as always different)
    process_records[is.na(new_group), new_group := TRUE]

    # Create consolidation group IDs
    process_records[,
      consolidation_group := paste(cf, cumsum(new_group), sep = "_"),
      by = cf
    ]

    # Clean up temporary columns
    process_records[,
      c(
        "prev_employer",
        "prev_fine",
        "prev_arco",
        "gap_days",
        "new_group"
      ) := NULL
    ]

    # Delegate aggregation to shared consolidation helper
    consolidated <- .consolidate_groups(
      process_records,
      remove_group_col = TRUE,
      variable_handling = variable_handling
    )
  } else {
    consolidated <- data.table::data.table()
  }

  # 6. Prepare skip records -----
  if (nrow(skip_records) > 0) {
    skip_records[, n_periods_consolidated := 1L]
  }

  # 7. Combine results -----
  result <- data.table::rbindlist(
    list(skip_records, consolidated),
    use.names = TRUE,
    fill = TRUE
  )

  # Restore temporal order
  data.table::setkey(result, cf, inizio, fine)

  return(result)
}
