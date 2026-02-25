#' Consolidation Engine v2 — collapse-native
#'
#' @description
#' Internal function implementing a high-performance consolidation engine
#' using the collapse package's C-level grouped operations. Replaces the
#' per-group data.table J-expression approach with vectorized column-wise
#' aggregation using a single pre-built GRP object.
#'
#' @param data A data.table with employment periods and a `consolidation_group` column
#' @param remove_group_col Logical, whether to remove the `consolidation_group` column
#' @param variable_handling Character string: "weight" or "first"
#'
#' @return A data.table with consolidated employment periods
#'
#' @importFrom collapse ffirst fmax fmean fmode fmin fsum GRP
#'
#' @keywords internal
#' @noRd
.consolidate_groups_v2 <- function(
  data,
  remove_group_col = TRUE,
  variable_handling = "weight"
) {
  # 1. Input validation -----
  if (!data.table::is.data.table(data)) {
    stop("Input must be a data.table")
  }

  if (!variable_handling %in% c("weight", "first")) {
    stop("variable_handling must be 'weight' or 'first'")
  }

  required_cols <- c("cf", "inizio", "fine", "durata", "consolidation_group")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove duplicate column names (v1 tolerates them, GRP/setkey does not)
  col_names <- names(data)
  if (anyDuplicated(col_names)) {
    keep_idx <- !duplicated(col_names)
    data <- data[, which(keep_idx), with = FALSE]
  }

  # 2. Column classification — ONCE -----
  all_cols <- names(data)
  durata_is_integer <- is.integer(data$durata)
  exclude_cols <- c(
    "cf",
    "consolidation_group",
    "inizio",
    "fine",
    "durata",
    "n_periods_consolidated",
    "non_working_days"
  )
  special_cols <- c("arco", "over_id", "stato")
  standard_cols <- setdiff(all_cols, c(exclude_cols, special_cols))

  # Classify column types using vapply
  col_is_numeric <- vapply(
    standard_cols,
    function(col) is.numeric(data[[col]]),
    logical(1L)
  )
  col_is_character <- vapply(
    standard_cols,
    function(col) is.character(data[[col]]),
    logical(1L)
  )
  col_is_factor <- vapply(
    standard_cols,
    function(col) is.factor(data[[col]]),
    logical(1L)
  )
  col_is_logical <- vapply(
    standard_cols,
    function(col) is.logical(data[[col]]),
    logical(1L)
  )

  numeric_cols <- standard_cols[col_is_numeric]
  character_cols <- standard_cols[col_is_character | col_is_factor]
  logical_cols <- standard_cols[col_is_logical]

  # Record integer columns (subset of numeric)
  integer_cols <- vapply(
    numeric_cols,
    function(col) is.integer(data[[col]]),
    logical(1L)
  )
  integer_col_names <- numeric_cols[integer_cols]

  # Record IDate columns for inizio/fine
  inizio_is_idate <- inherits(data$inizio, "IDate")
  fine_is_idate <- inherits(data$fine, "IDate")

  # Record original classes for factor columns (to restore levels)
  factor_col_names <- standard_cols[col_is_factor]
  factor_levels <- lapply(factor_col_names, function(col) levels(data[[col]]))
  names(factor_levels) <- factor_col_names

  # Filter special cols to those actually present
  has_arco <- "arco" %in% all_cols
  has_over_id <- "over_id" %in% all_cols
  has_stato <- "stato" %in% all_cols
  over_id_is_integer <- has_over_id && is.integer(data$over_id)

  # 3. Build GRP object ONCE -----
  data.table::setkey(data, cf, consolidation_group)
  grp <- collapse::GRP(
    data,
    by = c("cf", "consolidation_group"),
    sort = TRUE,
    return.groups = TRUE,
    call = FALSE
  )
  group_sizes <- grp[[3L]]

  # 4. All-single-record fast path (works in-place; callers pass -----
  #    disposable subsets so no copy needed)
  if (max(group_sizes) == 1L) {
    data[, n_periods_consolidated := 1L]
    if (durata_is_integer) {
      data[, durata := as.integer(fine - inizio + 1L)]
    } else {
      data[, durata := as.numeric(fine - inizio + 1)]
    }

    if (remove_group_col) {
      data[, consolidation_group := NULL]
    }

    original_data_cols <- intersect(all_cols, names(data))
    original_data_cols <- setdiff(original_data_cols, "consolidation_group")
    new_metric_cols <- setdiff(names(data), all_cols)
    data.table::setcolorder(data, c(original_data_cols, new_metric_cols))
    data.table::setindex(data, cf)
    return(data)
  }

  # 5. Core aggregation -----
  # Start with group keys
  group_keys <- grp[["groups"]]
  result <- data.table::copy(group_keys)

  # Temporal columns
  inizio_agg <- collapse::fmin(as.numeric(data$inizio), g = grp, na.rm = TRUE)
  fine_agg <- collapse::fmax(as.numeric(data$fine), g = grp, na.rm = TRUE)

  # Restore IDate class
  if (inizio_is_idate) {
    result[,
      inizio := structure(as.integer(inizio_agg), class = c("IDate", "Date"))
    ]
  } else {
    result[, inizio := structure(as.integer(inizio_agg), class = "Date")]
  }
  if (fine_is_idate) {
    result[,
      fine := structure(as.integer(fine_agg), class = c("IDate", "Date"))
    ]
  } else {
    result[, fine := structure(as.integer(fine_agg), class = "Date")]
  }

  if (durata_is_integer) {
    result[, durata := as.integer(fine - inizio + 1L)]
  } else {
    result[, durata := as.numeric(fine - inizio + 1)]
  }

  if (variable_handling == "first") {
    # 5a. "first" mode -----

    # Standard columns: ffirst for all
    for (col in c(numeric_cols, character_cols, logical_cols)) {
      result[, (col) := collapse::ffirst(data[[col]], g = grp, na.rm = TRUE)]
    }

    # Special: arco
    if (has_arco) {
      result[, arco := collapse::fmax(data$arco, g = grp, na.rm = TRUE)]
    }

    # Special: over_id
    if (has_over_id) {
      result[, over_id := .aggregate_over_id_v2(data, grp, over_id_is_integer)]
    }

    # Special: stato
    if (has_stato) {
      result[, stato := collapse::ffirst(data$stato, g = grp, na.rm = TRUE)]
    }
  } else {
    # 5b. "weight" mode -----
    weights <- data$durata

    # Total weight per group (denominator for ALL weighted means)
    # v1 uses sum(durata) as denominator regardless of NA values in target
    # column, so NA values contribute 0 to numerator but their durata still
    # counts in denominator. Replicate this with fsum(w)/fsum(x*w).
    total_weight <- collapse::fsum(weights, g = grp, na.rm = TRUE)

    # Numeric columns: weighted mean with v1-compatible NA handling
    for (col in numeric_cols) {
      xw <- data[[col]] * weights
      numerator <- collapse::fsum(xw, g = grp, na.rm = TRUE)
      wmean <- numerator / total_weight

      # Single-record groups or zero total weight: take first non-NA
      single_or_zero <- which(group_sizes == 1L | total_weight == 0)
      if (length(single_or_zero) > 0) {
        first_vals <- collapse::ffirst(data[[col]], g = grp, na.rm = TRUE)
        wmean[single_or_zero] <- first_vals[single_or_zero]
      }

      # All-NA groups should return NA
      all_na_groups <- which(
        is.na(collapse::ffirst(data[[col]], g = grp, na.rm = TRUE))
      )
      if (length(all_na_groups) > 0) {
        wmean[all_na_groups] <- NA_real_
      }

      result[, (col) := wmean]
    }

    # Character/factor columns: weighted mode
    for (col in character_cols) {
      result[,
        (col) := collapse::fmode(
          data[[col]],
          g = grp,
          w = weights,
          ties = "first",
          na.rm = TRUE
        )
      ]
    }

    # Logical columns: weighted majority rule (same v1-compatible denominator)
    for (col in logical_cols) {
      xw <- as.numeric(data[[col]]) * weights
      numerator <- collapse::fsum(xw, g = grp, na.rm = TRUE)
      wmean <- numerator / total_weight

      single_or_zero <- which(group_sizes == 1L | total_weight == 0)
      if (length(single_or_zero) > 0) {
        first_vals <- collapse::ffirst(
          as.numeric(data[[col]]),
          g = grp,
          na.rm = TRUE
        )
        wmean[single_or_zero] <- first_vals[single_or_zero]
      }

      result[, (col) := as.logical(wmean >= 0.5)]
    }

    # Special: arco
    if (has_arco) {
      result[, arco := collapse::fmax(data$arco, g = grp, na.rm = TRUE)]
    }

    # Special: over_id
    if (has_over_id) {
      result[, over_id := .aggregate_over_id_v2(data, grp, over_id_is_integer)]
    }

    # Special: stato
    if (has_stato) {
      result[, stato := .aggregate_stato_v2(data, grp)]
    }
  }

  # n_periods_consolidated
  result[, n_periods_consolidated := as.integer(group_sizes)]

  # 6. Type restoration -----

  # Integer columns: round and cast
  for (col in integer_col_names) {
    if (col %in% names(result) && is.numeric(result[[col]])) {
      data.table::set(result, j = col, value = as.integer(round(result[[col]])))
    }
  }

  # Factor columns: restore factor class with original levels
  for (col in factor_col_names) {
    if (col %in% names(result)) {
      data.table::set(
        result,
        j = col,
        value = factor(result[[col]], levels = factor_levels[[col]])
      )
    }
  }

  # 7. Column ordering + cleanup -----
  if (remove_group_col) {
    result[, consolidation_group := NULL]
  }

  original_data_cols <- intersect(all_cols, names(result))
  original_data_cols <- setdiff(original_data_cols, "consolidation_group")
  new_metric_cols <- setdiff(names(result), all_cols)
  final_col_order <- c(original_data_cols, new_metric_cols)
  data.table::setcolorder(result, final_col_order)

  data.table::setindex(result, cf)

  return(result)
}


#' Aggregate stato Column with Employment-Aware Weighted Mode
#'
#' @description
#' Internal helper for v2 consolidation engine. Applies employment-aware
#' aggregation for the `stato` column: employment groups use weighted mode
#' of employment states only; unemployment groups are set to "disoccupato".
#'
#' @param data A data.table with `stato`, `arco`, and `durata` columns.
#' @param grp A collapse GRP object built on `c("cf", "consolidation_group")`.
#'
#' @return Character vector with one aggregated stato value per group.
#'
#' @keywords internal
#' @noRd
.aggregate_stato_v2 <- function(data, grp) {
  # Max arco per group determines employment vs unemployment
  arco_max <- collapse::fmax(data$arco, g = grp, na.rm = TRUE)

  # Group index for each row (1-based)
  gi <- grp[[2L]]
  n_groups <- length(arco_max)

  # Employment groups: mask stato to NA where arco == 0, then weighted fmode
  masked_stato <- data.table::copy(data$stato)
  is_unemployment_row <- data$arco == 0 | is.na(data$arco)
  is_employment_group <- arco_max[gi] > 0

  # For employment groups, mask out unemployment rows
  masked_stato[
    is_unemployment_row & is_employment_group[seq_along(gi)]
  ] <- NA_character_

  # Weighted mode across all groups
  stato_agg <- collapse::fmode(
    masked_stato,
    g = grp,
    w = data$durata,
    ties = "first",
    na.rm = TRUE
  )

  # For unemployment groups (arco_max == 0) with 2+ records: set to "disoccupato"
  # v1 compatibility: single-record groups keep their original stato value because
  # the special stato handling in v1 only triggers when length(non_na_vals) > 1
  group_sizes <- grp[[3L]]
  unemployment_groups <- which(
    (arco_max == 0 | is.na(arco_max)) & group_sizes > 1L
  )
  if (length(unemployment_groups) > 0) {
    # Check if original data has a matching "disoccupato" variant
    disoccupato_vals <- grep(
      "disoccupato",
      data$stato,
      ignore.case = TRUE,
      value = TRUE
    )
    disoccupato_label <- if (length(disoccupato_vals) > 0) {
      disoccupato_vals[1L]
    } else {
      "disoccupato"
    }
    stato_agg[unemployment_groups] <- disoccupato_label
  }

  # Fallback for employment groups where fmode returned NA (all employment records had NA stato)
  na_employment <- which(is.na(stato_agg) & arco_max > 0)
  if (length(na_employment) > 0) {
    fallback <- collapse::ffirst(data$stato, g = grp, na.rm = TRUE)
    stato_agg[na_employment] <- fallback[na_employment]
  }

  return(stato_agg)
}


#' Aggregate over_id Column with Two-Pass Logic
#'
#' @description
#' Internal helper for v2 consolidation engine. Uses a two-pass approach:
#' first attempts to get the first positive (non-zero, non-NA) over_id value
#' per group, then falls back to any first value for groups where all values
#' are zero or NA.
#'
#' @param data A data.table with an `over_id` column.
#' @param grp A collapse GRP object built on `c("cf", "consolidation_group")`.
#' @param is_integer Logical, whether the original over_id column is integer.
#'
#' @return Numeric or integer vector with one aggregated over_id per group.
#'
#' @keywords internal
#' @noRd
.aggregate_over_id_v2 <- function(data, grp, is_integer) {
  # Masked version: set over_id to NA where <= 0 or NA
  over_id_vals <- data$over_id
  masked <- data.table::copy(over_id_vals)
  masked[is.na(masked) | masked <= 0] <- NA

  # First pass: first non-zero value per group
  agg <- collapse::ffirst(masked, g = grp, na.rm = TRUE)

  # Second pass: for groups where first pass returned NA, use any first value
  na_groups <- is.na(agg)
  if (any(na_groups)) {
    fallback <- collapse::ffirst(over_id_vals, g = grp, na.rm = TRUE)
    agg[na_groups] <- fallback[na_groups]
  }

  # Type preservation
  if (is_integer) {
    return(as.integer(agg))
  } else {
    return(as.numeric(agg))
  }
}
