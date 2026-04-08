#' Consolidate Adjacent Employment Periods
#'
#' @description
#' Consolidates contiguous employment periods with no gap or unemployment between them.
#' Two employment periods are adjacent if the end of one is immediately followed
#' by the start of the next (no days between). Unemployment periods act as barriers
#' that prevent consolidation.
#'
#' @param data data.table with employment records. Must contain columns:
#'   \code{cf}, \code{inizio}, \code{fine}, \code{durata}. The \code{arco} column
#'   is used if present to identify employment vs unemployment periods.
#' @param variable_handling Character string specifying aggregation strategy for variables:
#'   \code{"weight"} uses weighted mean/mode (default), \code{"first"} takes first non-NA value
#' @param engine Character string specifying the consolidation engine: \code{"v2"}
#'   (default) uses the collapse-native engine for maximum performance, \code{"v1"}
#'   uses the original data.table J-expression engine for backward compatibility.
#'
#' @return data.table with adjacent employment periods consolidated. Includes all
#'   original columns plus \code{n_periods_consolidated} indicating how many
#'   periods were merged (1 means no consolidation occurred for that record).
#'
#' @details
#' **What makes periods "adjacent":**
#'
#' Two employment periods are adjacent if:
#' - They belong to the same person (\code{cf})
#' - They are consecutive in time (no gap days between them)
#' - Both are employment periods (\code{arco > 0} or missing)
#' - There is no unemployment period between them
#'
#' **How unemployment acts as a barrier:**
#'
#' Unemployment periods (\code{arco == 0}) prevent consolidation. For example,
#' if you have Employment-Unemployment-Employment, these will NOT be consolidated
#' even if the dates are adjacent. Use \code{\link{consolidate_short_gaps}} if
#' you want to bridge unemployment gaps.
#'
#' **Difference from overlapping consolidation:**
#'
#' - \code{\link{consolidate_overlapping}}: Merges concurrent employment (same \code{over_id})
#' - \code{consolidate_adjacent}: Merges sequential employment with no gap
#'
#' **Aggregation rules:**
#'
#' When consolidating periods, the function:
#' - Uses \code{min(inizio)} and \code{max(fine)} for date range
#' - Recalculates \code{durata} as the full span
#' - Uses weighted mode for qualitative variables (e.g., contract type)
#' - Uses weighted mean for quantitative variables (e.g., salary)
#' - Weights are based on the \code{durata} of each period
#'
#' **Performance:**
#'
#' Fully vectorized implementation with exceptional performance:
#' - Handles 10M+ employment records efficiently
#' - 9x faster than previous consolidation implementations (Phase 3)
#' - Phase 4 optimization: 1.2-3x additional speedup via single-period worker bypass
#' - Memory efficient: < 1x input data size
#' - Base throughput: ~41,000 records/second (Phase 3)
#' - Optimized throughput: ~50,000-120,000 records/second (Phase 4, dataset dependent)
#'
#' Phase 4 automatically skips consolidation for single-period workers (no adjacent
#' periods possible). Performance scales with percentage of single-period workers:
#' - 20% singles: ~1.2x speedup
#' - 40% singles: ~1.4x speedup
#' - 50% singles: ~1.7x speedup
#' - 70% singles: ~2.9x speedup
#'
#' **Composability:**
#'
#' This function is designed to be chained with other consolidation functions:
#' \preformatted{
#' data |>
#'   consolidate_overlapping() |>  # First merge concurrent
#'   consolidate_adjacent() |>     # Then merge adjacent
#'   consolidate_short_gaps(30)    # Finally bridge short gaps
#' }
#'
#' The order matters: always consolidate overlapping employment first, then
#' adjacent periods, and finally bridge gaps if needed.
#'
#' @examples
#' \dontrun{
#' # Basic: Consolidate 3 consecutive employment periods
#' data <- data.table::data.table(
#'   cf = rep(1, 3),
#'   inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
#'   durata = c(31, 28, 31),
#'   arco = c(1, 1, 1)
#' )
#'
#' result <- consolidate_adjacent(data)
#' nrow(result)  # 1 (all three periods merged)
#' result$n_periods_consolidated  # 3
#'
#' # With gaps: periods separated by days won't consolidate
#' data_with_gap <- data.table::data.table(
#'   cf = rep(1, 3),
#'   inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01")),
#'   fine = as.Date(c("2023-01-31", "2023-02-28", "2023-04-30")),
#'   durata = c(31, 28, 30),
#'   arco = c(1, 1, 1)
#' )
#' # Periods 1-2 are adjacent (Jan 31 → Feb 1)
#' # Period 3 has a gap (Feb 28 → Apr 1 = 32 days)
#'
#' result <- consolidate_adjacent(data_with_gap)
#' nrow(result)  # 2 (first two merged, third separate)
#'
#' # With unemployment barrier
#' data_barrier <- data.table::data.table(
#'   cf = rep(1, 3),
#'   inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
#'   durata = c(31, 28, 31),
#'   arco = c(1, 0, 1)  # Middle period is unemployment
#' )
#'
#' result <- consolidate_adjacent(data_barrier)
#' nrow(result)  # 3 (unemployment blocks consolidation)
#'
#' # Chaining: after consolidate_overlapping()
#' data <- readRDS("data/sample.rds")
#' result <- data |>
#'   consolidate_overlapping() |>
#'   consolidate_adjacent()
#'
#' cat("Original records:", nrow(data), "\n")
#' cat("After consolidation:", nrow(result), "\n")
#'
#' # Integration with transition analysis
#' consolidated <- data |>
#'   consolidate_overlapping() |>
#'   consolidate_adjacent()
#' transitions <- analyze_employment_transitions(consolidated)
#'
#' # Edge case: empty data
#' empty_data <- data.table::data.table(
#'   cf = integer(),
#'   inizio = as.Date(character()),
#'   fine = as.Date(character()),
#'   durata = integer()
#' )
#' result_empty <- consolidate_adjacent(empty_data)  # Returns empty data.table
#'
#' # Edge case: single record
#' single_record <- data[1]
#' result_single <- consolidate_adjacent(single_record)  # Returns as-is
#' }
#'
#' @seealso
#' \code{\link{consolidate_overlapping}} for concurrent employment consolidation
#'
#' \code{\link{consolidate_by_employer}} for same-employer consolidation
#'
#' \code{\link{consolidate_short_gaps}} for gap-bridging consolidation
#'
#' \code{\link{consolidation_helpers}} for internal aggregation functions
#'
#' @export
consolidate_adjacent <- function(
  data,
  variable_handling = "weight",
  engine = "v2"
) {
  .assert_vecshift_input(
    data,
    required_cols = c("cf", "inizio", "fine", "durata")
  )

  # Input validation
  if (!inherits(data, "data.table")) {
    stop("data must be a data.table")
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
    return(.preserve_vecshift_class(result, data))
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
    # Create shift columns by cf to detect adjacency
    process_records[,
      `:=`(
        prev_fine = data.table::shift(fine, 1L, type = "lag"),
        prev_arco = data.table::shift(arco, 1L, type = "lag")
      ),
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

    # Detect new group starts (vectorized)
    # Start new group when:
    # - First record for person (prev_fine is NA)
    # - Gap > 0 days
    # - Previous was unemployment
    # - Current is unemployment
    process_records[,
      new_group := is.na(prev_fine) |
        gap_days > 0L |
        prev_arco == 0L |
        arco == 0L
    ]

    # Create consolidation group IDs
    process_records[,
      consolidation_group := data.table::rleid(cf, cumsum(new_group))
    ]

    # Clean up temporary columns
    process_records[,
      c("prev_fine", "prev_arco", "gap_days", "new_group") := NULL
    ]

    # Call shared consolidation helper
    if (engine == "v2") {
      consolidated <- .consolidate_groups_v2(
        process_records,
        remove_group_col = TRUE,
        variable_handling = variable_handling
      )
    } else if (variable_handling == "first") {
      consolidated <- .consolidate_groups_optimized(
        process_records,
        remove_group_col = TRUE,
        variable_handling = "first"
      )
    } else {
      consolidated <- .consolidate_groups(
        process_records,
        remove_group_col = TRUE,
        variable_handling = variable_handling
      )
    }
  } else {
    # No multi-period workers to process
    consolidated <- data.table::data.table()
  }

  # Prepare skip records (add n_periods_consolidated column if not present)
  if (nrow(skip_records) > 0) {
    if (!"n_periods_consolidated" %in% names(skip_records)) {
      skip_records[, n_periods_consolidated := 1L]
    }
  }

  # Combine results
  result <- data.table::rbindlist(
    list(skip_records, consolidated),
    use.names = TRUE,
    fill = TRUE
  )

  # Restore temporal order
  data.table::setkey(result, cf, inizio, fine)

  return(.preserve_vecshift_class(result, data))
}
