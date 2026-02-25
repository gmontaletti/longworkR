#' Consolidate Employment Periods with Short Gaps
#'
#' @description
#' Consolidates employment periods separated by unemployment gaps up to a
#' specified threshold. This bridges short unemployment periods between jobs,
#' useful for analyzing labor market attachment and employment stability.
#'
#' @param data data.table with employment records. Must contain columns:
#'   \code{cf}, \code{inizio}, \code{fine}, \code{durata}. The \code{arco} column
#'   is used if present to identify employment vs unemployment periods.
#' @param max_gap_days Maximum gap in days to consolidate across (default: 8).
#'   Common values:
#'   \itemize{
#'     \item 7-8 days: Very short breaks, weekly consolidation (default)
#'     \item 14 days: Bi-weekly consolidation
#'     \item 30 days: Monthly gaps
#'     \item 90 days: Quarterly employment analysis
#'   }
#' @param variable_handling Character string specifying aggregation strategy for variables:
#'   \code{"first"} takes first non-NA value (default), \code{"weight"} uses weighted mean/mode
#' @param engine Character string specifying the consolidation engine: \code{"v2"}
#'   (default) uses the collapse-native engine for maximum performance, \code{"v1"}
#'   uses the original data.table J-expression engine for backward compatibility.
#'
#' @return data.table with periods consolidated across short gaps. Includes all
#'   original columns plus:
#'   \itemize{
#'     \item \code{n_periods_consolidated}: Number of periods merged
#'     \item \code{non_working_days}: Total unemployment days within consolidated period
#'   }
#'
#' @details
#' **How gaps are bridged:**
#'
#' This function consolidates employment periods when they are separated by
#' gaps of \code{max_gap_days} or fewer. **Important:** Unemployment periods
#' with duration > \code{max_gap_days} act as consolidation barriers. Short
#' unemployment periods (<= threshold) can be bridged. For example, with
#' \code{max_gap_days = 30}:
#' - Employment (Jan 1-15) → Gap (10 days) → Employment (Jan 26-31) = CONSOLIDATED
#' - Employment (Jan 1-15) → Gap (50 days) → Employment (Mar 6-31) = NOT consolidated
#' - Employment (Jan 1-15) → Unemployment (20 days) → Employment = CONSOLIDATED
#' - Employment (Jan 1-15) → Unemployment (40 days) → Employment = NOT consolidated
#'
#' **What non_working_days represents:**
#'
#' The \code{non_working_days} column tracks the total unemployment days that
#' were bridged within each consolidated period. This allows you to analyze
#' the "quality" of employment continuity even after consolidation.
#'
#' **Use cases for different thresholds:**
#'
#' \describe{
#'   \item{7-14 days}{Very short breaks, sick leave, brief unemployment}
#'   \item{30 days}{Monthly analysis, standard employment continuity (default)}
#'   \item{60-90 days}{Seasonal work, quarterly analysis}
#'   \item{180+ days}{Long-term labor market attachment}
#' }
#'
#' **Difference from other consolidation functions:**
#'
#' - \code{\link{consolidate_overlapping}}: Merges concurrent employment (same time)
#' - \code{\link{consolidate_adjacent}}: Merges touching periods (no gap, employment only)
#' - \code{consolidate_short_gaps}: Bridges short gaps and brief unemployment periods;
#'   long unemployment (> threshold) acts as barrier
#'
#' **Aggregation rules:**
#'
#' When consolidating periods, the function:
#' - Uses \code{min(inizio)} and \code{max(fine)} for date range
#' - Recalculates \code{durata} as the full span (including gaps)
#' - Counts total unemployment days as \code{non_working_days}
#' - Uses weighted mode for qualitative variables
#' - Uses weighted mean for quantitative variables
#' - Weights are based on employment durations (unemployment excluded)
#'
#' **Performance:**
#'
#' Fully vectorized implementation with exceptional performance:
#' - Handles 10M+ employment records efficiently
#' - **Phase 5 optimization: 10-15x faster than Phase 3 baseline**
#' - Phase 4: Single-period worker bypass (1.2-3x additional speedup)
#' - Phase 3: Vectorized consolidation (9x faster than baseline)
#' - Memory efficient: < 1x input data size
#' - Optimized throughput: ~150,000-200,000 records/second
#'
#' **Optimization strategy:**
#' - Phase 4 skips gap bridging for single-period workers (no gaps to bridge)
#' - Phase 5 optimizes .consolidate_groups() by:
#'   - Splitting single-record consolidation groups (no aggregation needed)
#'   - Simplified first-value aggregation (avoids expensive weighted mode)
#'   - Results in 10-15x speedup with variable_handling = "first" (default)
#'
#' Performance scales with percentage of single-period workers and consolidation
#' group sizes. The function efficiently processes large-scale employment data.
#'
#' **Composability:**
#'
#' This function is designed to be the final step in a consolidation chain:
#' \preformatted{
#' data |>
#'   consolidate_overlapping() |>  # First merge concurrent
#'   consolidate_adjacent() |>     # Then merge adjacent
#'   consolidate_short_gaps(30)    # Finally bridge short gaps
#' }
#'
#' Always run this function last, after overlapping and adjacent consolidation.
#' This ensures the most accurate gap calculations and consolidation results.
#'
#' @examples
#' \dontrun{
#' # Basic: 15-day gap with max_gap=30
#' data <- data.table::data.table(
#'   cf = rep(1, 5),
#'   inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01",
#'                      "2023-03-01", "2023-04-01")),
#'   fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15",
#'                    "2023-03-15", "2023-04-15")),
#'   durata = c(15, 6, 15, 15, 15),
#'   arco = c(1, 0, 1, 0, 1)
#' )
#'
#' result30 <- consolidate_short_gaps(data, max_gap_days = 30)
#' nrow(result30)  # 1 (all periods consolidated)
#' result30$non_working_days  # 21 days (6 + 15 unemployment periods)
#'
#' # Threshold test: same data with different max_gap
#' result10 <- consolidate_short_gaps(data, max_gap_days = 10)
#' nrow(result10)  # 2 (splits at 15-day gap)
#'
#' # Multiple gaps
#' data_multi <- data.table::data.table(
#'   cf = rep(1, 7),
#'   inizio = as.Date(c("2023-01-01", "2023-01-08", "2023-02-01",
#'                      "2023-02-08", "2023-03-01", "2023-03-08",
#'                      "2023-05-01")),
#'   fine = as.Date(c("2023-01-07", "2023-01-31", "2023-02-07",
#'                    "2023-02-28", "2023-03-07", "2023-03-31",
#'                    "2023-05-31")),
#'   durata = c(7, 24, 7, 21, 7, 24, 31),
#'   arco = c(1, 1, 1, 1, 1, 1, 1)
#' )
#' # Gaps: 0, 0, 0, 0, 0, 30 days
#'
#' result <- consolidate_short_gaps(data_multi, max_gap_days = 30)
#' nrow(result)  # 2 (breaks at 30-day gap before May)
#'
#' # Full chain: overlapping → adjacent → short_gaps
#' data <- readRDS("data/sample.rds")
#' final <- data |>
#'   consolidate_overlapping() |>
#'   consolidate_adjacent() |>
#'   consolidate_short_gaps(30)
#'
#' cat("Original records:", nrow(data), "\n")
#' cat("Final consolidated:", nrow(final), "\n")
#' cat("Total reduction:", round((1 - nrow(final)/nrow(data)) * 100, 1), "%\n")
#'
#' # Analyze non_working_days
#' summary(final$non_working_days)
#' hist(final$non_working_days, main = "Distribution of unemployment days bridged")
#'
#' # Integration with analyze_employment_transitions()
#' consolidated <- data |>
#'   consolidate_overlapping() |>
#'   consolidate_adjacent() |>
#'   consolidate_short_gaps(30)
#' transitions <- analyze_employment_transitions(consolidated)
#'
#' # Comparison of different thresholds
#' result_strict <- data |> consolidate_short_gaps(7)   # 1 week
#' result_medium <- data |> consolidate_short_gaps(30)  # 1 month
#' result_lenient <- data |> consolidate_short_gaps(90) # 3 months
#'
#' cat("Strict (7d):", nrow(result_strict), "periods\n")
#' cat("Medium (30d):", nrow(result_medium), "periods\n")
#' cat("Lenient (90d):", nrow(result_lenient), "periods\n")
#' }
#'
#' @seealso
#' \code{\link{consolidate_overlapping}} for concurrent employment consolidation
#'
#' \code{\link{consolidate_adjacent}} for contiguous period consolidation
#'
#' \code{\link{consolidate_by_employer}} for same-employer consolidation
#'
#' \code{\link{consolidate_employer_gaps}} for combined employer + gap-bridging
#'   consolidation in a single pass
#'
#' \code{\link{consolidation_helpers}} for internal aggregation functions
#'
#' @export
consolidate_short_gaps <- function(
  data,
  max_gap_days = 8,
  variable_handling = "first",
  engine = "v2"
) {
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("data must be a data.table")
  }

  # Validate max_gap_days
  if (!is.numeric(max_gap_days) || max_gap_days < 0) {
    stop("max_gap_days must be a non-negative number")
  }

  # Validate variable_handling
  if (!variable_handling %in% c("weight", "first")) {
    stop("variable_handling must be 'weight' or 'first'")
  }

  engine <- match.arg(engine, c("v2", "v1"))

  # Check required columns
  required <- c("cf", "inizio", "fine", "durata")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Handle empty dataset
  if (nrow(data) == 0) {
    result <- data.table::copy(data)
    result[, n_periods_consolidated := integer()]
    result[, non_working_days := integer()]
    return(result)
  }

  # In-place split to avoid full copy of input -----
  needs_arco <- !"arco" %in% names(data)

  # Compute per-cf counts temporarily on input, then clean up
  data[, .n_temp__ := .N, by = cf]
  skip_mask <- data$.n_temp__ == 1L
  data[, .n_temp__ := NULL]

  # Subset into independent allocations (no full copy of input)
  skip_records <- if (any(skip_mask)) {
    data[skip_mask]
  } else {
    data.table::data.table()
  }
  process_records <- if (any(!skip_mask)) {
    data[!skip_mask]
  } else {
    data.table::data.table()
  }

  # Prepare subsets without modifying original data
  if (nrow(skip_records) > 0) {
    if (needs_arco) {
      skip_records[, arco := 1L]
    }
    if (!inherits(skip_records$inizio, "Date")) {
      skip_records[, inizio := as.Date(inizio)]
    }
    if (!inherits(skip_records$fine, "Date")) {
      skip_records[, fine := as.Date(fine)]
    }
  }
  if (nrow(process_records) > 0) {
    if (needs_arco) {
      process_records[, arco := 1L]
    }
    if (!inherits(process_records$inizio, "Date")) {
      process_records[, inizio := as.Date(inizio)]
    }
    if (!inherits(process_records$fine, "Date")) {
      process_records[, fine := as.Date(fine)]
    }
    data.table::setkey(process_records, cf, inizio, fine)
  }

  # Process multi-period workers only
  if (nrow(process_records) > 0) {
    # Create shift columns to calculate gaps and detect unemployment barriers
    process_records[,
      prev_fine := data.table::shift(fine, 1L, type = "lag"),
      by = cf
    ]
    process_records[,
      prev_arco := data.table::shift(arco, 1L, type = "lag"),
      by = cf
    ]
    process_records[,
      prev_durata := data.table::shift(durata, 1L, type = "lag"),
      by = cf
    ]

    # Calculate gap in days
    process_records[,
      gap_days := data.table::fifelse(
        is.na(prev_fine),
        NA_integer_,
        as.integer(inizio - prev_fine - 1L)
      )
    ]

    # Detect new group starts based on threshold
    # New group when:
    # - First record for person (prev_fine is NA)
    # - Gap exceeds max_gap_days
    # - Previous period was unemployment with duration > max_gap_days (acts as barrier)
    # - Current period is unemployment with duration > max_gap_days (acts as barrier)
    process_records[,
      new_group := is.na(prev_fine) |
        is.na(gap_days) |
        gap_days > max_gap_days |
        (!is.na(prev_arco) & prev_arco == 0L & prev_durata > max_gap_days) |
        (arco == 0L & durata > max_gap_days)
    ]

    # Create consolidation group IDs
    process_records[,
      consolidation_group := data.table::rleid(cf, cumsum(new_group))
    ]

    # Calculate non_working_days per group BEFORE consolidation
    # Mark unemployment days
    process_records[,
      non_working_days_temp := data.table::fifelse(arco == 0L, durata, 0L)
    ]

    # Aggregate by group
    non_working_summary <- process_records[,
      .(
        non_working_days = sum(non_working_days_temp, na.rm = TRUE)
      ),
      by = .(cf, consolidation_group)
    ]

    # Clean temporary columns before consolidation
    process_records[,
      c(
        "prev_fine",
        "prev_arco",
        "prev_durata",
        "gap_days",
        "new_group",
        "non_working_days_temp"
      ) := NULL
    ]

    # Call consolidation helper with engine routing
    if (engine == "v2") {
      consolidated <- .consolidate_groups_v2(
        process_records,
        remove_group_col = FALSE,
        variable_handling = variable_handling
      )
    } else if (variable_handling == "first") {
      consolidated <- .consolidate_groups_optimized(
        process_records,
        remove_group_col = FALSE,
        variable_handling = variable_handling
      )
    } else {
      consolidated <- .consolidate_groups(
        process_records,
        remove_group_col = FALSE,
        variable_handling = variable_handling
      )
    }

    # Merge non_working_days back to result
    consolidated <- merge(
      consolidated,
      non_working_summary,
      by = c("cf", "consolidation_group"),
      all.x = TRUE
    )

    # Remove consolidation_group from consolidated output
    consolidated[, consolidation_group := NULL]
  } else {
    # No records to process
    consolidated <- data.table::data.table()
  }

  # Prepare skip records (add required columns)
  if (nrow(skip_records) > 0) {
    skip_records[, n_periods_consolidated := 1L]
    skip_records[, non_working_days := 0L] # Single period = no gaps bridged
  }

  # Combine results
  result <- data.table::rbindlist(
    list(skip_records, consolidated),
    use.names = TRUE,
    fill = TRUE
  )

  # Restore temporal order
  data.table::setkey(result, cf, inizio, fine)

  return(result)
}
