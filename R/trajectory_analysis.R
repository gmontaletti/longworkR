#' @title Contract Trajectory Analysis
#' @description
#' Functions for tracking employment trajectories after specific contract events.
#' This module provides tools to analyze employment patterns following contract
#' transitions, focusing on quarterly employment status tracking.
#'
#' @name trajectory-analysis
#' @importFrom data.table data.table setkey setnames rbindlist copy is.data.table setkeyv fcase
#' @importFrom ggplot2 ggplot aes geom_text labs theme_void scale_fill_manual theme element_blank after_stat
#' @importFrom ggalluvial geom_flow geom_stratum StatStratum
#' @importFrom stats setNames
#' @importFrom rlang sym
NULL

#' Vectorized trajectory calculation helper function
#'
#' @param dt Employment data
#' @param quarters_dt Quarter data for all persons
#' @param start_col Start date column name
#' @param end_col End date column name
#' @param person_col Person ID column name
#' @param arco_col Arco column name for employment status
#' @param employment_thresholds Numeric vector with employment classification thresholds
#' @param quarter_days Number of days per quarter
#' @param reference_dates Optional data.table with reference dates and person_vars to merge
#' @return data.table with employment trajectories
#' @keywords internal
calculate_trajectories_vectorized <- function(dt, quarters_dt, start_col, end_col, person_col, arco_col, employment_thresholds, quarter_days, reference_dates = NULL) {

  # Create cartesian join between quarters and employment data
  overlaps <- merge(quarters_dt, dt, by = person_col, allow.cartesian = TRUE)

  # Filter to overlapping periods only (include all arco values)
  overlaps <- overlaps[
    get(start_col) <= quarter_end & get(end_col) >= quarter_start
  ]

  # Calculate employment period within each quarter
  overlaps[, `:=`(
    emp_start = pmax(get(start_col), quarter_start),
    emp_end = pmin(get(end_col), quarter_end)
  )]

  # Remove invalid periods
  overlaps <- overlaps[emp_start <= emp_end]

  # Calculate days for each overlap period
  overlaps[, days_in_period := as.numeric(emp_end - emp_start) + 1]

  # Handle overlapping periods within quarters - separate employment vs. non-employment
  # Sort by person, quarter, and employment period
  setkeyv(overlaps, c(person_col, "quarter_num", "emp_start", "emp_end"))

  # Merge overlapping periods within each person-quarter combination
  group_cols <- c(person_col, "quarter", "quarter_num", "quarter_start", "quarter_end", "quarter_days")

  merged_employment <- overlaps[, {
    if (.N == 0) {
      data.table(days_employed = 0)
    } else {
      # Separate employment periods (arco > 0) from non-employment periods (arco = 0)
      employment_periods <- .SD[get(arco_col) > 0, .(start = emp_start, end = emp_end)]

      if (nrow(employment_periods) == 0) {
        # No employment periods, only arco = 0 periods
        .(days_employed = 0)
      } else if (nrow(employment_periods) == 1) {
        # Single employment period
        .(days_employed = as.numeric(employment_periods$end - employment_periods$start) + 1)
      } else {
        # Multiple employment periods - merge overlapping ones
        setkey(employment_periods, start, end)

        # Use non-overlapping interval union algorithm for employment periods only
        merged <- employment_periods[1]

        if (nrow(employment_periods) > 1) {
          for (i in 2:nrow(employment_periods)) {
            curr_start <- employment_periods$start[i]
            curr_end <- employment_periods$end[i]
            last_end <- merged$end[nrow(merged)]

            if (curr_start <= last_end + 1) {
              # Overlapping or adjacent - extend last period
              merged$end[nrow(merged)] <- pmax(last_end, curr_end)
            } else {
              # Non-overlapping - add new period
              merged <- rbind(merged, employment_periods[i])
            }
          }
        }

        .(days_employed = sum(as.numeric(merged$end - merged$start) + 1))
      }
    }
  }, by = group_cols]

  # Get all person-quarter combinations (including those with no employment)
  all_combinations <- copy(quarters_dt)

  # Left join to include quarters with no employment
  result <- merge(all_combinations, merged_employment,
                 by = group_cols,
                 all.x = TRUE)

  # Keep days_employed as NA for quarters with no contract data
  # (this will be classified as "No Information")

  # Calculate employment percentage
  result[, employment_percentage := (days_employed / quarter_days) * 100]

  # Vectorized employment status classification using configurable thresholds
  result[, employment_status := fcase(
    is.na(days_employed), "No Information",
    employment_percentage < employment_thresholds[1], "Not Working",
    employment_percentage <= employment_thresholds[2], "Partially Working",
    employment_percentage <= employment_thresholds[3], "Mostly Working",
    employment_percentage > employment_thresholds[3], "Fully Working"
  )]

  # Merge with reference_dates to include person_vars if provided
  if (!is.null(reference_dates)) {
    result <- merge(result, reference_dates, by = person_col, all.x = TRUE)
  }

  # Order by person and quarter
  setkeyv(result, c(person_col, "quarter_num"))

  return(result)
}

#' Track Contract Trajectories
#'
#' Tracks employment trajectories for individuals who experienced a specific
#' contract type, following their employment status for 4 quarters (91 days each)
#' after the end date of their chronologically first occurrence of that contract type.
#'
#' This function identifies individuals who had a specific contract type, uses
#' the end date of their chronologically FIRST occurrence (earliest start date)
#' as a reference point, then tracks employment status in the subsequent quarters.
#' Employment status is calculated based on the percentage of days employed within
#' each quarter, with overlapping contracts counting as employment time.
#'
#' @param data A data.table containing vecshift-processed employment data
#' @param contract_type_var Character. Column name containing contract type codes
#'   (default: "COD_TIPOLOGIA_CONTRATTUALE")
#' @param contract_type_value Character. Specific contract type value to track
#' @param n_quarters Integer. Number of quarters to track after reference date
#'   (default: 4)
#' @param person_id Character. Column name for person identifier (default: "cf")
#' @param start_date_col Character. Column name for contract start dates
#'   (default: "inizio")
#' @param end_date_col Character. Column name for contract end dates
#'   (default: "fine")
#' @param arco_col Character. Column name for employment status indicator where
#'   0 = unemployment and > 0 = employment (default: "arco")
#' @param employment_thresholds Numeric vector. Three percentage values defining
#'   employment status classification thresholds (default: c(1, 40, 99))
#' @param return_plot Logical. Whether to return the alluvial plot (default: TRUE)
#' @param palette Character. Color palette to use: "employment", "main",
#'   "desaturated", "bw" (default: "employment")
#' @param use_bw Logical. Force black and white palette (default: FALSE)
#' @param plot_title Character. Custom plot title (default: auto-generated)
#' @param plot_subtitle Character. Custom plot subtitle (default: auto-generated)
#' @param person_vars Character vector of person-level variable names to include
#'   in the output data (e.g., c("eta", "sesso", "istruzione")). Variables are
#'   extracted from the first target contract for each person. Default is NULL
#'   (no additional variables).
#' @param chunk_size Integer. Number of persons to process per chunk for memory
#'   management (default: 10000)
#'
#' @details
#' Employment is determined by the `arco` variable where:
#' \itemize{
#'   \item `arco = 0`: Indicates unemployment (not working)
#'   \item `arco > 0`: Indicates employment (working)
#' }
#'
#' All contract periods (both `arco = 0` and `arco > 0`) are included in the analysis,
#' but only periods where `arco > 0` are counted as employment days. Employment status
#' is then classified based on the percentage of days employed within each 91-day quarter
#' using configurable thresholds (default: 1%, 40%, 99%):
#' \itemize{
#'   \item "Not Working": < employment_thresholds[1]% of days employed
#'   \item "Partially Working": >= employment_thresholds[1]% and <= employment_thresholds[2]% of days employed
#'   \item "Mostly Working": > employment_thresholds[2]% and <= employment_thresholds[3]% of days employed
#'   \item "Fully Working": > employment_thresholds[3]% of days employed
#'   \item "No Information": No contract data available for the quarter (days_employed = NA)
#' }
#'
#' The function only tracks forward in time from the reference date. Any
#' employment is counted as working time, regardless of contract intensity
#' or prior employment status. Overlapping contracts are handled by counting
#' the union of employment periods using vectorized operations for maximum performance.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: data.table with individual-level quarterly trajectories
#'   \item \code{summary}: data.table with aggregated trajectory patterns
#'   \item \code{transitions}: data.table with aggregated transition counts between quarters,
#'     including columns: from_quarter, to_quarter, from_status, to_status, person_vars (if provided), count
#'   \item \code{plot}: ggplot2 alluvial plot (if return_plot = TRUE)
#'   \item \code{parameters}: List of function parameters used
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track trajectories after temporary contracts
#' result <- track_contract_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   n_quarters = 4
#' )
#'
#' # View the summary of trajectory patterns
#' print(result$summary)
#'
#' # Display the alluvial plot
#' print(result$plot)
#'
#' # Custom visualization with black and white
#' result_bw <- track_contract_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   use_bw = TRUE,
#'   plot_title = "Employment Trajectories After Temporary Contracts"
#' )
#'
#' # Large dataset with chunked processing
#' result_large <- track_contract_trajectories(
#'   data = large_employment_data,
#'   contract_type_value = "B.01.00",
#'   chunk_size = 5000
#' )
#' }
#'
#' @seealso \code{\link{theme_vecshift}}, \code{\link{vecshift_colors}}
track_contract_trajectories <- function(data,
                                      contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
                                      contract_type_value,
                                      n_quarters = 4,
                                      person_id = "cf",
                                      start_date_col = "inizio",
                                      end_date_col = "fine",
                                      arco_col = "arco",
                                      employment_thresholds = c(1, 40, 99),
                                      return_plot = TRUE,
                                      palette = "employment",
                                      use_bw = FALSE,
                                      plot_title = NULL,
                                      plot_subtitle = NULL,
                                      person_vars = NULL,
                                      chunk_size = 10000) {

  # Input validation
  if (!is.data.table(data)) {
    stop("Input data must be a data.table object")
  }

  required_cols <- c(person_id, contract_type_var, start_date_col, end_date_col, arco_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (missing(contract_type_value)) {
    stop("contract_type_value must be specified")
  }

  if (n_quarters < 1 || n_quarters > 20) {
    stop("n_quarters must be between 1 and 20")
  }

  # Validate employment_thresholds
  if (!is.numeric(employment_thresholds)) {
    stop("employment_thresholds must be a numeric vector")
  }

  if (length(employment_thresholds) != 3) {
    stop("employment_thresholds must contain exactly 3 values")
  }

  if (any(employment_thresholds < 0) || any(employment_thresholds > 100)) {
    stop("employment_thresholds values must be between 0 and 100")
  }

  if (!identical(employment_thresholds, sort(employment_thresholds))) {
    stop("employment_thresholds must be in ascending order")
  }

  # Create working copy
  dt <- copy(data)
  setkeyv(dt, c(person_id, start_date_col))

  # Convert date columns to Date class if needed
  date_cols <- c(start_date_col, end_date_col)
  for (col in date_cols) {
    if (!inherits(dt[[col]], "Date")) {
      dt[[col]] <- as.Date(dt[[col]])
    }
  }

  # Step 1: Identify individuals who had the specific contract type
  target_contracts <- dt[get(contract_type_var) == contract_type_value]
  if (nrow(target_contracts) == 0) {
    stop("No records found for contract type: ", contract_type_value)
  }

  # Step 2: FIXED - Find the first occurrence per person and get reference date (end date of chronologically first contract)
  if (!is.null(person_vars)) {
    # Validate person_vars exist in data
    missing_vars <- setdiff(person_vars, names(dt))
    if (length(missing_vars) > 0) {
      warning("Variables not found in data: ", paste(missing_vars, collapse = ", "))
      person_vars <- intersect(person_vars, names(dt))
    }

    if (length(person_vars) > 0) {
      reference_dates <- target_contracts[
        order(get(person_id), get(start_date_col), get(end_date_col))
      ][, .SD[1], by = person_id][, c(person_id, end_date_col, person_vars), with = FALSE]
    } else {
      reference_dates <- target_contracts[
        order(get(person_id), get(start_date_col), get(end_date_col))
      ][, .SD[1], by = person_id][, c(person_id, end_date_col), with = FALSE]
    }
  } else {
    reference_dates <- target_contracts[
      order(get(person_id), get(start_date_col), get(end_date_col))
    ][, .SD[1], by = person_id][, c(person_id, end_date_col), with = FALSE]
  }

  setnames(reference_dates, c(person_id, end_date_col), c("person_id", "reference_date"))

  # Rename person_id column in dt to standardize on "person_id"
  setnames(dt, person_id, "person_id")

  if (nrow(reference_dates) == 0) {
    stop("No reference dates could be established")
  }

  # Step 3: Vectorized quarter generation for all persons at once
  quarter_days <- 91  # Updated to 91 days per quarter (4 quarters = 364 days ≈ 12 months)

  all_quarters <- reference_dates[, {
    q_nums <- 1:n_quarters
    .(
      quarter = paste0("Q", q_nums),
      quarter_num = q_nums,
      quarter_start = reference_date + (q_nums - 1) * quarter_days + 1,
      quarter_end = reference_date + q_nums * quarter_days,
      quarter_days = quarter_days
    )
  }, by = person_id]

  setkeyv(all_quarters, c("person_id", "quarter_num"))

  # Step 4: Vectorized employment calculation using merge with allow.cartesian
  # Create cartesian product of quarters and employment data
  setkeyv(dt, "person_id")

  # Process in chunks to prevent memory issues
  n_persons <- nrow(reference_dates)

  if (n_persons <= chunk_size) {
    # Process all at once
    all_trajectories <- calculate_trajectories_vectorized(dt, all_quarters,
                                                         start_date_col, end_date_col,
                                                         "person_id", arco_col,
                                                         employment_thresholds, quarter_days,
                                                         reference_dates)
  } else {
    # Process in chunks
    person_list <- reference_dates[["person_id"]]
    chunks <- split(person_list, ceiling(seq_along(person_list) / chunk_size))

    trajectory_list <- vector("list", length(chunks))

    for (i in seq_along(chunks)) {
      chunk_persons <- chunks[[i]]
      chunk_quarters <- all_quarters[person_id %in% chunk_persons]
      chunk_data <- dt[person_id %in% chunk_persons]
      chunk_reference <- reference_dates[person_id %in% chunk_persons]

      trajectory_list[[i]] <- calculate_trajectories_vectorized(chunk_data, chunk_quarters,
                                                               start_date_col, end_date_col,
                                                               "person_id", arco_col,
                                                               employment_thresholds, quarter_days,
                                                               chunk_reference)
    }

    all_trajectories <- rbindlist(trajectory_list)
  }

  # Create summary of trajectory patterns
  # Create trajectory sequence for each person using vectorized operations
  trajectory_sequences <- all_trajectories[
    order(person_id, quarter_num)
  ][, .(trajectory = paste(employment_status, collapse = " -> ")),
    by = person_id]

  # Count trajectory patterns
  trajectory_summary <- trajectory_sequences[,
    .(count = .N,
      percentage = round(.N / nrow(reference_dates) * 100, 1)),
    by = trajectory][order(-count)]

  # Prepare data for alluvial plot
  plot_data <- copy(all_trajectories)

  # Create the plot if requested
  plot_obj <- NULL
  if (return_plot) {
    # Load required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting")
    }
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
      stop("Package 'ggalluvial' is required for alluvial plots")
    }

    # Determine colors
    if (use_bw) {
      palette <- "bw"
    }

    # Get colors using theme_vecshift function
    employment_statuses <- c("Not Working", "Partially Working", "Mostly Working",
                           "Fully Working", "No Information")

    if (palette == "employment" && !use_bw) {
      colors <- c(
        "Not Working" = "#E74C3C",        # Red
        "Partially Working" = "#F39C12",   # Orange
        "Mostly Working" = "#3498DB",      # Blue
        "Fully Working" = "#27AE60",       # Green
        "No Information" = "#95A5A6"       # Grey
      )
    } else {
      # Use vecshift_colors function (assuming it exists)
      base_colors <- vecshift_colors(palette = if (use_bw) "bw" else "main",
                                   n = length(employment_statuses))
      colors <- setNames(base_colors[1:length(employment_statuses)], employment_statuses)
    }

    # Create alluvial plot
    plot_obj <- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x = quarter, stratum = employment_status,
                                          alluvium = person_id, fill = employment_status)) +
      ggalluvial::geom_flow(alpha = 0.6, curve_type = "cubic", width = 0.3) +
      ggalluvial::geom_stratum(alpha = 0.8, width = 0.3) +
      ggplot2::geom_text(stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3, color = "white", fontface = "bold") +
      ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
      ggplot2::labs(
        title = if (is.null(plot_title)) {
          paste("Employment Trajectories After", contract_type_value, "Contract")
        } else {
          plot_title
        },
        subtitle = if (is.null(plot_subtitle)) {
          paste("Tracking", nrow(reference_dates), "individuals over", n_quarters, "quarters")
        } else {
          plot_subtitle
        },
        x = "Quarter After Contract End",
        caption = paste("Reference contract type:", contract_type_value)
      ) +
      theme_vecshift() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  # Create transitions aggregation
  # Sort data by person and quarter to get consecutive quarters
  all_trajectories_sorted <- all_trajectories[order(person_id, quarter_num)]

  # Create transitions by comparing consecutive quarters for each person
  transitions_raw <- all_trajectories_sorted[, {
    if (.N > 1) {
      data.table(
        from_quarter = quarter_num[-.N],
        to_quarter = quarter_num[-1],
        from_status = employment_status[-.N],
        to_status = employment_status[-1]
      )
    } else {
      # If only one quarter, no transitions
      data.table()
    }
  }, by = person_id]

  # Aggregate transitions based on whether person_vars exist
  if (!is.null(person_vars) && length(person_vars) > 0) {
    # Merge person_vars from reference_dates into transitions
    transitions_with_vars <- merge(transitions_raw,
                                   reference_dates[, c("person_id", person_vars), with = FALSE],
                                   by = "person_id", all.x = TRUE)

    # Group by transition characteristics and person_vars, then count
    group_cols <- c("from_quarter", "to_quarter", "from_status", "to_status", person_vars)
    transition_counts <- transitions_with_vars[, .(count = .N), by = group_cols]
  } else {
    # Group by transition characteristics only
    transition_counts <- transitions_raw[, .(count = .N),
                                        by = .(from_quarter, to_quarter, from_status, to_status)]
  }

  # Order by from_quarter, to_quarter, and count
  setkeyv(transition_counts, c("from_quarter", "to_quarter", "count"))
  setorderv(transition_counts, c("from_quarter", "to_quarter", "count"), order = c(1, 1, -1))

  # Prepare return object
  result <- list(
    data = all_trajectories,
    summary = trajectory_summary,
    transitions = transition_counts,
    plot = plot_obj,
    parameters = list(
      contract_type_var = contract_type_var,
      contract_type_value = contract_type_value,
      n_quarters = n_quarters,
      person_id = person_id,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      arco_col = arco_col,
      employment_thresholds = employment_thresholds,
      palette = palette,
      use_bw = use_bw,
      person_vars = person_vars,
      quarter_days = quarter_days,
      chunk_size = chunk_size,
      n_individuals = nrow(reference_dates)
    )
  )

  return(result)
}

#' Vectorized professional trajectory calculation helper function
#'
#' @param dt Employment data
#' @param quarters_dt Quarter data for all persons
#' @param start_col Start date column name
#' @param end_col End date column name
#' @param person_col Person ID column name
#' @param arco_col Arco column name for employment status
#' @param qualifica_col Professional code column name
#' @param reference_codes data.table with reference professional codes per person
#' @param quarter_days Number of days per quarter
#' @return data.table with professional trajectories
#' @keywords internal
calculate_professional_trajectories_vectorized <- function(dt, quarters_dt, start_col, end_col, person_col, arco_col, qualifica_col, reference_codes, quarter_days) {

  # Create cartesian join between quarters and employment data
  overlaps <- merge(quarters_dt, dt, by = person_col, allow.cartesian = TRUE)

  # Filter to overlapping periods only (include all arco values)
  overlaps <- overlaps[
    get(start_col) <= quarter_end & get(end_col) >= quarter_start
  ]

  # Calculate employment period within each quarter
  overlaps[, `:=`(
    emp_start = pmax(get(start_col), quarter_start),
    emp_end = pmin(get(end_col), quarter_end)
  )]

  # Remove invalid periods
  overlaps <- overlaps[emp_start <= emp_end]

  # Handle professional code selection within quarters
  # Sort by person, quarter, and employment period
  setkeyv(overlaps, c(person_col, "quarter_num", "emp_end"))

  # Group by person-quarter and select professional code
  group_cols <- c(person_col, "quarter", "quarter_num", "quarter_start", "quarter_end", "quarter_days")

  professional_codes <- overlaps[, {
    if (.N == 0) {
      data.table(quarter_code = NA_character_, all_arco_zero = TRUE)
    } else {
      # Check if there are any employment contracts (arco > 0)
      employment_contracts <- .SD[get(arco_col) > 0]

      if (nrow(employment_contracts) > 0) {
        # Prioritize employment contracts - get code from latest end date
        latest_employment <- employment_contracts[.N]  # Already sorted by emp_end
        .(quarter_code = latest_employment[[qualifica_col]],
          all_arco_zero = FALSE)
      } else {
        # All contracts have arco = 0
        # Still get code from latest contract for potential future use
        latest_contract <- .SD[.N]  # Already sorted by emp_end
        .(quarter_code = latest_contract[[qualifica_col]],
          all_arco_zero = TRUE)
      }
    }
  }, by = group_cols]

  # Get all person-quarter combinations (including those with no contracts)
  all_combinations <- copy(quarters_dt)

  # Left join to include quarters with no contracts
  result <- merge(all_combinations, professional_codes,
                 by = group_cols,
                 all.x = TRUE)

  # Keep quarter_code as NA for quarters with no contract data
  # all_arco_zero defaults to FALSE for NA cases

  # Merge with reference codes
  result <- merge(result, reference_codes, by = person_col, all.x = TRUE)

  # Vectorized professional status classification
  result[, professional_status := fcase(
    all_arco_zero == TRUE, "Not Working",
    is.na(quarter_code), "No Information",
    quarter_code == reference_code, "Same Code",
    quarter_code != reference_code, "Different Code"
  )]

  # Order by person and quarter
  setkeyv(result, c(person_col, "quarter_num"))

  return(result)
}

#' Vectorized employer trajectory calculation helper function
#'
#' @param dt Employment data
#' @param quarters_dt Quarter data for all persons
#' @param start_col Start date column name
#' @param end_col End date column name
#' @param person_col Person ID column name
#' @param arco_col Arco column name for employment status
#' @param employer_col Employer column name
#' @param reference_employers data.table with reference employers per person
#' @param quarter_days Number of days per quarter
#' @return data.table with employer trajectories
#' @keywords internal
calculate_employer_trajectories_vectorized <- function(dt, quarters_dt, start_col, end_col, person_col, arco_col, employer_col, reference_employers, quarter_days) {

  # Create cartesian join between quarters and employment data
  overlaps <- merge(quarters_dt, dt, by = person_col, allow.cartesian = TRUE)

  # Filter to overlapping periods only (include all arco values)
  overlaps <- overlaps[
    get(start_col) <= quarter_end & get(end_col) >= quarter_start
  ]

  # Calculate employment period within each quarter
  overlaps[, `:=`(
    emp_start = pmax(get(start_col), quarter_start),
    emp_end = pmin(get(end_col), quarter_end)
  )]

  # Remove invalid periods
  overlaps <- overlaps[emp_start <= emp_end]

  # Handle employer selection within quarters
  # Sort by person, quarter, and employment period
  setkeyv(overlaps, c(person_col, "quarter_num", "emp_end"))

  # Group by person-quarter and select employer
  group_cols <- c(person_col, "quarter", "quarter_num", "quarter_start", "quarter_end", "quarter_days")

  employer_codes <- overlaps[, {
    if (.N == 0) {
      data.table(quarter_employer = NA_character_, all_arco_zero = TRUE)
    } else {
      # Check if there are any employment contracts (arco > 0)
      employment_contracts <- .SD[get(arco_col) > 0]

      if (nrow(employment_contracts) > 0) {
        # Prioritize employment contracts - get employer from latest end date
        latest_employment <- employment_contracts[.N]  # Already sorted by emp_end
        .(quarter_employer = latest_employment[[employer_col]],
          all_arco_zero = FALSE)
      } else {
        # All contracts have arco = 0
        # Still get employer from latest contract for potential future use
        latest_contract <- .SD[.N]  # Already sorted by emp_end
        .(quarter_employer = latest_contract[[employer_col]],
          all_arco_zero = TRUE)
      }
    }
  }, by = group_cols]

  # Get all person-quarter combinations (including those with no contracts)
  all_combinations <- copy(quarters_dt)

  # Left join to include quarters with no contracts
  result <- merge(all_combinations, employer_codes,
                 by = group_cols,
                 all.x = TRUE)

  # Keep quarter_employer as NA for quarters with no contract data
  # all_arco_zero defaults to FALSE for NA cases

  # Merge with reference employers
  result <- merge(result, reference_employers, by = person_col, all.x = TRUE)

  # Vectorized employer status classification
  result[, employer_status := fcase(
    all_arco_zero == TRUE, "Not Working",
    is.na(quarter_employer), "No Information",
    quarter_employer == reference_employer, "Same Employer",
    quarter_employer != reference_employer, "Different Employer"
  )]

  # Order by person and quarter
  setkeyv(result, c(person_col, "quarter_num"))

  return(result)
}

#' Track Professional Trajectories
#'
#' Tracks professional code trajectories for individuals who experienced a specific
#' contract type, following their professional code changes for 4 quarters (91 days each)
#' after the end date of their chronologically first occurrence of that contract type.
#'
#' This function identifies individuals who had a specific contract type, uses
#' the end date of their chronologically FIRST occurrence (earliest start date)
#' as a reference point, then tracks professional code changes in the subsequent quarters.
#' Professional status is determined by comparing the quarter's professional code
#' with the reference contract's professional code.
#'
#' @param data A data.table containing vecshift-processed employment data
#' @param contract_type_var Character. Column name containing contract type codes
#'   (default: "COD_TIPOLOGIA_CONTRATTUALE")
#' @param contract_type_value Character. Specific contract type value to track
#' @param n_quarters Integer. Number of quarters to track after reference date
#'   (default: 4)
#' @param person_id Character. Column name for person identifier (default: "cf")
#' @param start_date_col Character. Column name for contract start dates
#'   (default: "inizio")
#' @param end_date_col Character. Column name for contract end dates
#'   (default: "fine")
#' @param arco_col Character. Column name for employment status indicator where
#'   0 = unemployment and > 0 = employment (default: "arco")
#' @param qualifica_col Character. Column name for professional codes
#'   (default: "qualifica")
#' @param return_plot Logical. Whether to return the alluvial plot (default: TRUE)
#' @param palette Character. Color palette to use: "professional", "main",
#'   "desaturated", "bw" (default: "professional")
#' @param use_bw Logical. Force black and white palette (default: FALSE)
#' @param plot_title Character. Custom plot title (default: auto-generated)
#' @param plot_subtitle Character. Custom plot subtitle (default: auto-generated)
#' @param person_vars Character vector of person-level variable names to include
#'   in the output data (e.g., c("eta", "sesso", "istruzione")). Variables are
#'   extracted from the first target contract for each person. Default is NULL
#'   (no additional variables).
#' @param chunk_size Integer. Number of persons to process per chunk for memory
#'   management (default: 10000)
#'
#' @details
#' Professional code tracking is determined by the `qualifica` variable where:
#' \itemize{
#'   \item Employment contracts (`arco > 0`) are prioritized over unemployment contracts (`arco = 0`)
#'   \item Within each quarter, the professional code is taken from the contract with the latest end date
#'   \item Professional status is classified as:
#'     \itemize{
#'       \item "Same Code": Professional code matches the reference contract
#'       \item "Different Code": Professional code differs from the reference contract
#'       \item "Not Working": All contracts in the quarter have arco = 0 (unemployment)
#'       \item "No Information": No contract data available for the quarter
#'     }
#' }
#'
#' The function only tracks forward in time from the reference date. When multiple
#' contracts exist within a quarter, employment contracts (`arco > 0`) take priority,
#' and among those, the contract with the latest end date provides the professional code.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: data.table with individual-level quarterly professional trajectories
#'   \item \code{summary}: data.table with aggregated trajectory patterns
#'   \item \code{transitions}: data.table with aggregated transition counts between quarters,
#'     including columns: from_quarter, to_quarter, from_status, to_status, person_vars (if provided), count
#'   \item \code{plot}: ggplot2 alluvial plot (if return_plot = TRUE)
#'   \item \code{parameters}: List of function parameters used
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track professional trajectories after temporary contracts
#' result <- track_professional_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   n_quarters = 4
#' )
#'
#' # View the summary of professional trajectory patterns
#' print(result$summary)
#'
#' # Display the alluvial plot
#' print(result$plot)
#'
#' # Custom visualization with black and white
#' result_bw <- track_professional_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   use_bw = TRUE,
#'   plot_title = "Professional Trajectories After Temporary Contracts"
#' )
#' }
#'
#' @seealso \code{\link{track_contract_trajectories}}, \code{\link{theme_vecshift}}
track_professional_trajectories <- function(data,
                                           contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
                                           contract_type_value,
                                           n_quarters = 4,
                                           person_id = "cf",
                                           start_date_col = "inizio",
                                           end_date_col = "fine",
                                           arco_col = "arco",
                                           qualifica_col = "qualifica",
                                           return_plot = TRUE,
                                           palette = "professional",
                                           use_bw = FALSE,
                                           plot_title = NULL,
                                           plot_subtitle = NULL,
                                           person_vars = NULL,
                                           chunk_size = 10000) {

  # Input validation
  if (!is.data.table(data)) {
    stop("Input data must be a data.table object")
  }

  required_cols <- c(person_id, contract_type_var, start_date_col, end_date_col, arco_col, qualifica_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (missing(contract_type_value)) {
    stop("contract_type_value must be specified")
  }

  if (n_quarters < 1 || n_quarters > 20) {
    stop("n_quarters must be between 1 and 20")
  }

  # Create working copy
  dt <- copy(data)
  setkeyv(dt, c(person_id, start_date_col))

  # Convert date columns to Date class if needed
  date_cols <- c(start_date_col, end_date_col)
  for (col in date_cols) {
    if (!inherits(dt[[col]], "Date")) {
      dt[[col]] <- as.Date(dt[[col]])
    }
  }

  # Step 1: Identify individuals who had the specific contract type
  target_contracts <- dt[get(contract_type_var) == contract_type_value]
  if (nrow(target_contracts) == 0) {
    stop("No records found for contract type: ", contract_type_value)
  }

  # Step 2: Find the first occurrence per person and get reference data
  reference_data <- target_contracts[
    order(get(person_id), get(start_date_col), get(end_date_col))
  ][, .SD[1], by = person_id]

  # Extract reference dates and professional codes
  # If multiple codes in reference contract, get the latest available
  # Build the list of columns to extract
  if (!is.null(person_vars)) {
    # Validate person_vars exist in data
    missing_vars <- setdiff(person_vars, names(reference_data))
    if (length(missing_vars) > 0) {
      stop("person_vars not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }

  # Create the reference_codes data.table with conditional inclusion of person_vars
  if (is.null(person_vars)) {
    reference_codes <- reference_data[, .(
      person_id = get(person_id),
      reference_date = get(end_date_col),
      reference_code = get(qualifica_col)
    )]
  } else {
    # Build the list expression with person_vars
    j_expr <- paste0(".(person_id = get(person_id), reference_date = get(end_date_col), reference_code = get(qualifica_col)",
                    paste0(", ", person_vars, " = ", person_vars, collapse = ""),
                    ")")
    reference_codes <- reference_data[, eval(parse(text = j_expr))]
  }

  setnames(reference_codes, "person_id", person_id)

  if (nrow(reference_codes) == 0) {
    stop("No reference dates could be established")
  }

  # Step 3: Vectorized quarter generation for all persons at once
  quarter_days <- 91  # 91 days per quarter (4 quarters = 364 days ≈ 12 months)

  all_quarters <- reference_codes[, {
    q_nums <- 1:n_quarters
    .(
      quarter = paste0("Q", q_nums),
      quarter_num = q_nums,
      quarter_start = reference_date + (q_nums - 1) * quarter_days + 1,
      quarter_end = reference_date + q_nums * quarter_days,
      quarter_days = quarter_days
    )
  }, by = person_id]

  setkeyv(all_quarters, c(person_id, "quarter_num"))

  # Step 4: Vectorized professional code calculation
  setkeyv(dt, person_id)

  # Process in chunks to prevent memory issues
  n_persons <- nrow(reference_codes)

  if (n_persons <= chunk_size) {
    # Process all at once
    all_trajectories <- calculate_professional_trajectories_vectorized(dt, all_quarters,
                                                                      start_date_col, end_date_col,
                                                                      person_id, arco_col, qualifica_col,
                                                                      reference_codes, quarter_days)
  } else {
    # Process in chunks
    person_list <- reference_codes[[person_id]]
    chunks <- split(person_list, ceiling(seq_along(person_list) / chunk_size))

    trajectory_list <- vector("list", length(chunks))

    for (i in seq_along(chunks)) {
      chunk_persons <- chunks[[i]]
      chunk_quarters <- all_quarters[get(person_id) %in% chunk_persons]
      chunk_data <- dt[get(person_id) %in% chunk_persons]
      chunk_reference <- reference_codes[get(person_id) %in% chunk_persons]

      trajectory_list[[i]] <- calculate_professional_trajectories_vectorized(chunk_data, chunk_quarters,
                                                                            start_date_col, end_date_col,
                                                                            person_id, arco_col, qualifica_col,
                                                                            chunk_reference, quarter_days)
    }

    all_trajectories <- rbindlist(trajectory_list)
  }

  # Create summary of trajectory patterns
  # Create trajectory sequence for each person using vectorized operations
  trajectory_sequences <- all_trajectories[
    order(get(person_id), quarter_num)
  ][, .(trajectory = paste(professional_status, collapse = " -> ")),
    by = person_id]

  # Count trajectory patterns
  trajectory_summary <- trajectory_sequences[,
    .(count = .N,
      percentage = round(.N / nrow(reference_codes) * 100, 1)),
    by = trajectory][order(-count)]

  # Prepare data for alluvial plot
  plot_data <- copy(all_trajectories)

  # Create the plot if requested
  plot_obj <- NULL
  if (return_plot) {
    # Load required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting")
    }
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
      stop("Package 'ggalluvial' is required for alluvial plots")
    }

    # Determine colors
    if (use_bw) {
      palette <- "bw"
    }

    # Get colors for professional status
    professional_statuses <- c("Same Code", "Different Code", "Not Working", "No Information")

    if (palette == "professional" && !use_bw) {
      colors <- c(
        "Same Code" = "#27AE60",        # Green
        "Different Code" = "#3498DB",    # Blue
        "Not Working" = "#E74C3C",       # Red
        "No Information" = "#95A5A6"     # Grey
      )
    } else {
      # Use vecshift_colors function
      base_colors <- vecshift_colors(palette = if (use_bw) "bw" else "main",
                                   n = length(professional_statuses))
      colors <- setNames(base_colors[1:length(professional_statuses)], professional_statuses)
    }

    # Create alluvial plot
    plot_obj <- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x = quarter, stratum = professional_status,
                                          alluvium = !!sym(person_id), fill = professional_status)) +
      ggalluvial::geom_flow(alpha = 0.6, curve_type = "cubic", width = 0.3) +
      ggalluvial::geom_stratum(alpha = 0.8, width = 0.3) +
      ggplot2::geom_text(stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3, color = "white", fontface = "bold") +
      ggplot2::scale_fill_manual(values = colors, name = "Professional\nStatus") +
      ggplot2::labs(
        title = if (is.null(plot_title)) {
          paste("Professional Trajectories After", contract_type_value, "Contract")
        } else {
          plot_title
        },
        subtitle = if (is.null(plot_subtitle)) {
          paste("Tracking", nrow(reference_codes), "individuals over", n_quarters, "quarters")
        } else {
          plot_subtitle
        },
        x = "Quarter After Contract End",
        caption = paste("Reference contract type:", contract_type_value)
      ) +
      theme_vecshift() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  # Create transitions aggregation
  # Sort data by person and quarter to get consecutive quarters
  all_trajectories_sorted <- all_trajectories[order(get(person_id), quarter_num)]

  # Create transitions by comparing consecutive quarters for each person
  transitions_raw <- all_trajectories_sorted[, {
    if (.N > 1) {
      data.table(
        from_quarter = quarter_num[-.N],
        to_quarter = quarter_num[-1],
        from_status = professional_status[-.N],
        to_status = professional_status[-1]
      )
    } else {
      # If only one quarter, no transitions
      data.table()
    }
  }, by = person_id]

  # Aggregate transitions based on whether person_vars exist
  if (!is.null(person_vars) && length(person_vars) > 0) {
    # Merge person_vars from reference_codes into transitions
    transitions_with_vars <- merge(transitions_raw,
                                   reference_codes[, c(person_id, person_vars), with = FALSE],
                                   by = person_id, all.x = TRUE)

    # Group by transition characteristics and person_vars, then count
    group_cols <- c("from_quarter", "to_quarter", "from_status", "to_status", person_vars)
    transition_counts <- transitions_with_vars[, .(count = .N), by = group_cols]
  } else {
    # Group by transition characteristics only
    transition_counts <- transitions_raw[, .(count = .N),
                                        by = .(from_quarter, to_quarter, from_status, to_status)]
  }

  # Order by from_quarter, to_quarter, and count
  setkeyv(transition_counts, c("from_quarter", "to_quarter", "count"))
  setorderv(transition_counts, c("from_quarter", "to_quarter", "count"), order = c(1, 1, -1))

  # Prepare return object
  result <- list(
    data = all_trajectories,
    summary = trajectory_summary,
    transitions = transition_counts,
    plot = plot_obj,
    parameters = list(
      contract_type_var = contract_type_var,
      contract_type_value = contract_type_value,
      n_quarters = n_quarters,
      person_id = person_id,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      arco_col = arco_col,
      qualifica_col = qualifica_col,
      palette = palette,
      use_bw = use_bw,
      person_vars = person_vars,
      quarter_days = quarter_days,
      chunk_size = chunk_size,
      n_individuals = nrow(reference_codes)
    )
  )

  return(result)
}

#' Track Employer Trajectories
#'
#' Tracks employer transitions for individuals who experienced a specific
#' contract type, following their employer changes for 4 quarters (91 days each)
#' after the end date of their chronologically first occurrence of that contract type.
#'
#' This function identifies individuals who had a specific contract type, uses
#' the end date of their chronologically FIRST occurrence (earliest start date)
#' as a reference point, then tracks employer transitions in the subsequent quarters.
#' Employer status is determined by comparing the quarter's employer with
#' the reference contract's employer.
#'
#' @param data A data.table containing vecshift-processed employment data
#' @param contract_type_var Character. Column name containing contract type codes
#'   (default: "COD_TIPOLOGIA_CONTRATTUALE")
#' @param contract_type_value Character. Specific contract type value to track
#' @param employer_col Character. Column name containing employer codes
#'   (required parameter, no default)
#' @param n_quarters Integer. Number of quarters to track after reference date
#'   (default: 4)
#' @param person_id Character. Column name for person identifier (default: "cf")
#' @param start_date_col Character. Column name for contract start dates
#'   (default: "inizio")
#' @param end_date_col Character. Column name for contract end dates
#'   (default: "fine")
#' @param arco_col Character. Column name for employment status indicator where
#'   0 = unemployment and > 0 = employment (default: "arco")
#' @param return_plot Logical. Whether to return the alluvial plot (default: TRUE)
#' @param palette Character. Color palette to use: "employer", "main",
#'   "desaturated", "bw" (default: "employer")
#' @param use_bw Logical. Force black and white palette (default: FALSE)
#' @param plot_title Character. Custom plot title (default: auto-generated)
#' @param plot_subtitle Character. Custom plot subtitle (default: auto-generated)
#' @param person_vars Character vector of person-level variable names to include
#'   in the output data (e.g., c("eta", "sesso", "istruzione")). Variables are
#'   extracted from the first target contract for each person. Default is NULL
#'   (no additional variables).
#' @param chunk_size Integer. Number of persons to process per chunk for memory
#'   management (default: 10000)
#'
#' @details
#' Employment is determined by the `arco` variable where:
#' \itemize{
#'   \item `arco = 0`: Indicates unemployment (not working)
#'   \item `arco > 0`: Indicates employment (working)
#' }
#'
#' When multiple contracts exist within a quarter:
#' \itemize{
#'   \item Employment contracts (`arco > 0`) take priority over unemployment periods
#'   \item Within each quarter, the employer is taken from the contract with the latest end date
#'   \item Employer status is classified as:
#'     \itemize{
#'       \item "Same Employer": Employer matches the reference contract employer
#'       \item "Different Employer": Employer differs from the reference contract
#'       \item "Not Working": All contracts in the quarter have arco = 0 (unemployment)
#'       \item "No Information": No contract data available for the quarter
#'     }
#' }
#'
#' The function only tracks forward in time from the reference date. When multiple
#' contracts exist within a quarter, employment contracts (`arco > 0`) take priority,
#' and among those, the contract with the latest end date provides the employer code.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: data.table with individual-level quarterly employer trajectories
#'   \item \code{summary}: data.table with aggregated trajectory patterns
#'   \item \code{transitions}: data.table with aggregated transition counts between quarters,
#'     including columns: from_quarter, to_quarter, from_status, to_status, person_vars (if provided), count
#'   \item \code{plot}: ggplot2 alluvial plot (if return_plot = TRUE)
#'   \item \code{parameters}: List of function parameters used
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track employer trajectories after temporary contracts
#' result <- track_employer_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   employer_col = "COD_DATORE_LAVORO",
#'   n_quarters = 4
#' )
#'
#' # View the summary of employer trajectory patterns
#' print(result$summary)
#'
#' # Display the alluvial plot
#' print(result$plot)
#'
#' # Custom visualization with black and white
#' result_bw <- track_employer_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   employer_col = "COD_DATORE_LAVORO",
#'   use_bw = TRUE,
#'   plot_title = "Employer Trajectories After Temporary Contracts"
#' )
#' }
#'
#' @seealso \code{\link{track_contract_trajectories}}, \code{\link{track_professional_trajectories}}, \code{\link{theme_vecshift}}
track_employer_trajectories <- function(data,
                                       contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
                                       contract_type_value,
                                       employer_col,
                                       n_quarters = 4,
                                       person_id = "cf",
                                       start_date_col = "inizio",
                                       end_date_col = "fine",
                                       arco_col = "arco",
                                       return_plot = TRUE,
                                       palette = "employer",
                                       use_bw = FALSE,
                                       plot_title = NULL,
                                       plot_subtitle = NULL,
                                       person_vars = NULL,
                                       chunk_size = 10000) {

  # Input validation
  if (!is.data.table(data)) {
    stop("Input data must be a data.table object")
  }

  if (missing(employer_col)) {
    stop("employer_col parameter must be specified")
  }

  required_cols <- c(person_id, contract_type_var, start_date_col, end_date_col, arco_col, employer_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (missing(contract_type_value)) {
    stop("contract_type_value must be specified")
  }

  if (n_quarters < 1 || n_quarters > 20) {
    stop("n_quarters must be between 1 and 20")
  }

  # Create working copy
  dt <- copy(data)
  setkeyv(dt, c(person_id, start_date_col))

  # Convert date columns to Date class if needed
  date_cols <- c(start_date_col, end_date_col)
  for (col in date_cols) {
    if (!inherits(dt[[col]], "Date")) {
      dt[[col]] <- as.Date(dt[[col]])
    }
  }

  # Step 1: Identify individuals who had the specific contract type
  target_contracts <- dt[get(contract_type_var) == contract_type_value]
  if (nrow(target_contracts) == 0) {
    stop("No records found for contract type: ", contract_type_value)
  }

  # Step 2: Find the first occurrence per person and get reference data
  reference_data <- target_contracts[
    order(get(person_id), get(start_date_col), get(end_date_col))
  ][, .SD[1], by = person_id]

  # Extract reference dates and employer codes
  # Build the list of columns to extract
  if (!is.null(person_vars)) {
    # Validate person_vars exist in data
    missing_vars <- setdiff(person_vars, names(reference_data))
    if (length(missing_vars) > 0) {
      stop("person_vars not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }

  # Create the reference_employers data.table with conditional inclusion of person_vars
  if (is.null(person_vars)) {
    reference_employers <- reference_data[, .(
      person_id = get(person_id),
      reference_date = get(end_date_col),
      reference_employer = get(employer_col)
    )]
  } else {
    # Build the list expression with person_vars
    j_expr <- paste0(".(person_id = get(person_id), reference_date = get(end_date_col), reference_employer = get(employer_col)",
                    paste0(", ", person_vars, " = ", person_vars, collapse = ""),
                    ")")
    reference_employers <- reference_data[, eval(parse(text = j_expr))]
  }

  setnames(reference_employers, "person_id", person_id)

  if (nrow(reference_employers) == 0) {
    stop("No reference dates could be established")
  }

  # Step 3: Vectorized quarter generation for all persons at once
  quarter_days <- 91  # 91 days per quarter (4 quarters = 364 days ≈ 12 months)

  all_quarters <- reference_employers[, {
    q_nums <- 1:n_quarters
    .(
      quarter = paste0("Q", q_nums),
      quarter_num = q_nums,
      quarter_start = reference_date + (q_nums - 1) * quarter_days + 1,
      quarter_end = reference_date + q_nums * quarter_days,
      quarter_days = quarter_days
    )
  }, by = person_id]

  setkeyv(all_quarters, c(person_id, "quarter_num"))

  # Step 4: Vectorized employer calculation
  setkeyv(dt, person_id)

  # Process in chunks to prevent memory issues
  n_persons <- nrow(reference_employers)

  if (n_persons <= chunk_size) {
    # Process all at once
    all_trajectories <- calculate_employer_trajectories_vectorized(dt, all_quarters,
                                                                  start_date_col, end_date_col,
                                                                  person_id, arco_col, employer_col,
                                                                  reference_employers, quarter_days)
  } else {
    # Process in chunks
    person_list <- reference_employers[[person_id]]
    chunks <- split(person_list, ceiling(seq_along(person_list) / chunk_size))

    trajectory_list <- vector("list", length(chunks))

    for (i in seq_along(chunks)) {
      chunk_persons <- chunks[[i]]
      chunk_quarters <- all_quarters[get(person_id) %in% chunk_persons]
      chunk_data <- dt[get(person_id) %in% chunk_persons]
      chunk_reference <- reference_employers[get(person_id) %in% chunk_persons]

      trajectory_list[[i]] <- calculate_employer_trajectories_vectorized(chunk_data, chunk_quarters,
                                                                        start_date_col, end_date_col,
                                                                        person_id, arco_col, employer_col,
                                                                        chunk_reference, quarter_days)
    }

    all_trajectories <- rbindlist(trajectory_list)
  }

  # Create summary of trajectory patterns
  # Create trajectory sequence for each person using vectorized operations
  trajectory_sequences <- all_trajectories[
    order(get(person_id), quarter_num)
  ][, .(trajectory = paste(employer_status, collapse = " -> ")),
    by = person_id]

  # Count trajectory patterns
  trajectory_summary <- trajectory_sequences[,
    .(count = .N,
      percentage = round(.N / nrow(reference_employers) * 100, 1)),
    by = trajectory][order(-count)]

  # Prepare data for alluvial plot
  plot_data <- copy(all_trajectories)

  # Create the plot if requested
  plot_obj <- NULL
  if (return_plot) {
    # Load required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting")
    }
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
      stop("Package 'ggalluvial' is required for alluvial plots")
    }

    # Determine colors
    if (use_bw) {
      palette <- "bw"
    }

    # Get colors for employer status
    employer_statuses <- c("Same Employer", "Different Employer", "Not Working", "No Information")

    if (palette == "employer" && !use_bw) {
      colors <- c(
        "Same Employer" = "#2ECC71",         # Bright green
        "Different Employer" = "#E67E22",    # Orange
        "Not Working" = "#E74C3C",           # Red
        "No Information" = "#95A5A6"         # Grey
      )
    } else {
      # Use vecshift_colors function
      base_colors <- vecshift_colors(palette = if (use_bw) "bw" else "main",
                                   n = length(employer_statuses))
      colors <- setNames(base_colors[1:length(employer_statuses)], employer_statuses)
    }

    # Create alluvial plot
    plot_obj <- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x = quarter, stratum = employer_status,
                                          alluvium = !!sym(person_id), fill = employer_status)) +
      ggalluvial::geom_flow(alpha = 0.6, curve_type = "cubic", width = 0.3) +
      ggalluvial::geom_stratum(alpha = 0.8, width = 0.3) +
      ggplot2::geom_text(stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3, color = "white", fontface = "bold") +
      ggplot2::scale_fill_manual(values = colors, name = "Employer\nStatus") +
      ggplot2::labs(
        title = if (is.null(plot_title)) {
          paste("Employer Trajectories After", contract_type_value, "Contract")
        } else {
          plot_title
        },
        subtitle = if (is.null(plot_subtitle)) {
          paste("Tracking", nrow(reference_employers), "individuals over", n_quarters, "quarters")
        } else {
          plot_subtitle
        },
        x = "Quarter After Contract End",
        caption = paste("Reference contract type:", contract_type_value)
      ) +
      theme_vecshift() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  # Create transitions aggregation
  # Sort data by person and quarter to get consecutive quarters
  all_trajectories_sorted <- all_trajectories[order(get(person_id), quarter_num)]

  # Create transitions by comparing consecutive quarters for each person
  transitions_raw <- all_trajectories_sorted[, {
    if (.N > 1) {
      data.table(
        from_quarter = quarter_num[-.N],
        to_quarter = quarter_num[-1],
        from_status = employer_status[-.N],
        to_status = employer_status[-1]
      )
    } else {
      # If only one quarter, no transitions
      data.table()
    }
  }, by = person_id]

  # Aggregate transitions based on whether person_vars exist
  if (!is.null(person_vars) && length(person_vars) > 0) {
    # Merge person_vars from reference_employers into transitions
    transitions_with_vars <- merge(transitions_raw,
                                   reference_employers[, c(person_id, person_vars), with = FALSE],
                                   by = person_id, all.x = TRUE)

    # Group by transition characteristics and person_vars, then count
    group_cols <- c("from_quarter", "to_quarter", "from_status", "to_status", person_vars)
    transition_counts <- transitions_with_vars[, .(count = .N), by = group_cols]
  } else {
    # Group by transition characteristics only
    transition_counts <- transitions_raw[, .(count = .N),
                                        by = .(from_quarter, to_quarter, from_status, to_status)]
  }

  # Order by from_quarter, to_quarter, and count
  setkeyv(transition_counts, c("from_quarter", "to_quarter", "count"))
  setorderv(transition_counts, c("from_quarter", "to_quarter", "count"), order = c(1, 1, -1))

  # Prepare return object
  result <- list(
    data = all_trajectories,
    summary = trajectory_summary,
    transitions = transition_counts,
    plot = plot_obj,
    parameters = list(
      contract_type_var = contract_type_var,
      contract_type_value = contract_type_value,
      employer_col = employer_col,
      n_quarters = n_quarters,
      person_id = person_id,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      arco_col = arco_col,
      palette = palette,
      use_bw = use_bw,
      quarter_days = quarter_days,
      chunk_size = chunk_size,
      n_individuals = nrow(reference_employers)
    )
  )

  return(result)
}

#' Vectorized sector trajectory calculation helper function
#'
#' @param dt Employment data
#' @param quarters_dt Quarter data for all persons
#' @param start_col Start date column name
#' @param end_col End date column name
#' @param person_col Person ID column name
#' @param arco_col Arco column name for employment status
#' @param sector_col Sector column name
#' @param reference_sectors data.table with reference sectors per person
#' @param quarter_days Number of days per quarter
#' @return data.table with sector trajectories
#' @keywords internal
calculate_sector_trajectories_vectorized <- function(dt, quarters_dt, start_col, end_col, person_col, arco_col, sector_col, reference_sectors, quarter_days) {

  # Create cartesian join between quarters and employment data
  overlaps <- merge(quarters_dt, dt, by = person_col, allow.cartesian = TRUE)

  # Filter to overlapping periods only (include all arco values)
  overlaps <- overlaps[
    get(start_col) <= quarter_end & get(end_col) >= quarter_start
  ]

  # Calculate employment period within each quarter
  overlaps[, `:=`(
    emp_start = pmax(get(start_col), quarter_start),
    emp_end = pmin(get(end_col), quarter_end)
  )]

  # Remove invalid periods
  overlaps <- overlaps[emp_start <= emp_end]

  # Handle sector selection within quarters
  # Sort by person, quarter, and employment period
  setkeyv(overlaps, c(person_col, "quarter_num", "emp_end"))

  # Group by person-quarter and select sector
  group_cols <- c(person_col, "quarter", "quarter_num", "quarter_start", "quarter_end", "quarter_days")

  sector_codes <- overlaps[, {
    if (.N == 0) {
      data.table(quarter_sector = NA_character_, all_arco_zero = TRUE)
    } else {
      # Check if there are any employment contracts (arco > 0)
      employment_contracts <- .SD[get(arco_col) > 0]

      if (nrow(employment_contracts) > 0) {
        # Prioritize employment contracts - get sector from latest end date
        latest_employment <- employment_contracts[.N]  # Already sorted by emp_end
        .(quarter_sector = latest_employment[[sector_col]],
          all_arco_zero = FALSE)
      } else {
        # All contracts have arco = 0
        # Still get sector from latest contract for potential future use
        latest_contract <- .SD[.N]  # Already sorted by emp_end
        .(quarter_sector = latest_contract[[sector_col]],
          all_arco_zero = TRUE)
      }
    }
  }, by = group_cols]

  # Get all person-quarter combinations (including those with no contracts)
  all_combinations <- copy(quarters_dt)

  # Left join to include quarters with no contracts
  result <- merge(all_combinations, sector_codes,
                 by = group_cols,
                 all.x = TRUE)

  # Keep quarter_sector as NA for quarters with no contract data
  # all_arco_zero defaults to FALSE for NA cases

  # Merge with reference sectors
  result <- merge(result, reference_sectors, by = person_col, all.x = TRUE)

  # Vectorized sector status classification
  result[, sector_status := fcase(
    all_arco_zero == TRUE, "Not Working",
    is.na(quarter_sector), "No Information",
    quarter_sector == reference_sector, "Same Sector",
    quarter_sector != reference_sector, "Different Sector"
  )]

  # Order by person and quarter
  setkeyv(result, c(person_col, "quarter_num"))

  return(result)
}

#' Track Sector Trajectories
#'
#' Tracks economic sector transitions for individuals who experienced a specific
#' contract type, following their sector changes for 4 quarters (91 days each)
#' after the end date of their chronologically first occurrence of that contract type.
#'
#' This function identifies individuals who had a specific contract type, uses
#' the end date of their chronologically FIRST occurrence (earliest start date)
#' as a reference point, then tracks sector transitions in the subsequent quarters.
#' Sector status is determined by comparing the quarter's sector with
#' the reference contract's sector.
#'
#' @param data A data.table containing vecshift-processed employment data
#' @param contract_type_var Character. Column name containing contract type codes
#'   (default: "COD_TIPOLOGIA_CONTRATTUALE")
#' @param contract_type_value Character. Specific contract type value to track
#' @param sector_col Character. Column name containing sector codes
#'   (required parameter, no default)
#' @param n_quarters Integer. Number of quarters to track after reference date
#'   (default: 4)
#' @param person_id Character. Column name for person identifier (default: "cf")
#' @param start_date_col Character. Column name for contract start dates
#'   (default: "inizio")
#' @param end_date_col Character. Column name for contract end dates
#'   (default: "fine")
#' @param arco_col Character. Column name for employment status indicator where
#'   0 = unemployment and > 0 = employment (default: "arco")
#' @param return_plot Logical. Whether to return the alluvial plot (default: TRUE)
#' @param palette Character. Color palette to use: "sector", "main",
#'   "desaturated", "bw" (default: "sector")
#' @param use_bw Logical. Force black and white palette (default: FALSE)
#' @param plot_title Character. Custom plot title (default: auto-generated)
#' @param plot_subtitle Character. Custom plot subtitle (default: auto-generated)
#' @param chunk_size Integer. Number of persons to process per chunk for memory
#'   management (default: 10000)
#' @param person_vars Character vector of person-level variable names to include
#'   in the output data (e.g., c("eta", "sesso", "istruzione")). Variables are
#'   extracted from the first target contract for each person. Default is NULL
#'   (no additional variables).
#'
#' @details
#' Employment is determined by the `arco` variable where:
#' \itemize{
#'   \item `arco = 0`: Indicates unemployment (not working)
#'   \item `arco > 0`: Indicates employment (working)
#' }
#'
#' When multiple contracts exist within a quarter:
#' \itemize{
#'   \item Employment contracts (`arco > 0`) take priority over unemployment periods
#'   \item Within each quarter, the sector is taken from the contract with the latest end date
#'   \item Sector status is classified as:
#'     \itemize{
#'       \item "Same Sector": Sector matches the reference contract sector
#'       \item "Different Sector": Sector differs from the reference contract
#'       \item "Not Working": All contracts in the quarter have arco = 0 (unemployment)
#'       \item "No Information": No contract data available for the quarter
#'     }
#' }
#'
#' The function only tracks forward in time from the reference date. When multiple
#' contracts exist within a quarter, employment contracts (`arco > 0`) take priority,
#' and among those, the contract with the latest end date provides the sector code.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: data.table with individual-level quarterly sector trajectories
#'   \item \code{summary}: data.table with aggregated trajectory patterns
#'   \item \code{transitions}: data.table with aggregated transition counts between quarters,
#'     including columns: from_quarter, to_quarter, from_status, to_status, person_vars (if provided), count
#'   \item \code{plot}: ggplot2 alluvial plot (if return_plot = TRUE)
#'   \item \code{parameters}: List of function parameters used
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track sector trajectories after temporary contracts using ATECO codes
#' result <- track_sector_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   sector_col = "ATECO_2007_SEZIONE",
#'   n_quarters = 4
#' )
#'
#' # View the summary of sector trajectory patterns
#' print(result$summary)
#'
#' # Display the alluvial plot
#' print(result$plot)
#'
#' # Track trajectories with industry division codes
#' result_industry <- track_sector_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   sector_col = "ATECO_2007_DIVISIONE",
#'   n_quarters = 6
#' )
#'
#' # Custom visualization with black and white
#' result_bw <- track_sector_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   sector_col = "ATECO_2007_SEZIONE",
#'   use_bw = TRUE,
#'   plot_title = "Sector Trajectories After Temporary Contracts"
#' )
#' }
#'
#' @seealso \code{\link{track_contract_trajectories}}, \code{\link{track_professional_trajectories}},
#' \code{\link{track_employer_trajectories}}, \code{\link{theme_vecshift}}
track_sector_trajectories <- function(data,
                                     contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
                                     contract_type_value,
                                     sector_col,
                                     n_quarters = 4,
                                     person_id = "cf",
                                     start_date_col = "inizio",
                                     end_date_col = "fine",
                                     arco_col = "arco",
                                     return_plot = TRUE,
                                     palette = "sector",
                                     use_bw = FALSE,
                                     plot_title = NULL,
                                     plot_subtitle = NULL,
                                     chunk_size = 10000,
                                     person_vars = NULL) {

  # Input validation
  if (!is.data.table(data)) {
    stop("Input data must be a data.table object")
  }

  if (missing(sector_col)) {
    stop("sector_col parameter must be specified")
  }

  required_cols <- c(person_id, contract_type_var, start_date_col, end_date_col, arco_col, sector_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (missing(contract_type_value)) {
    stop("contract_type_value must be specified")
  }

  if (n_quarters < 1 || n_quarters > 20) {
    stop("n_quarters must be between 1 and 20")
  }

  # Create working copy
  dt <- copy(data)
  setkeyv(dt, c(person_id, start_date_col))

  # Convert date columns to Date class if needed
  date_cols <- c(start_date_col, end_date_col)
  for (col in date_cols) {
    if (!inherits(dt[[col]], "Date")) {
      dt[[col]] <- as.Date(dt[[col]])
    }
  }

  # Step 1: Identify individuals who had the specific contract type
  target_contracts <- dt[get(contract_type_var) == contract_type_value]
  if (nrow(target_contracts) == 0) {
    stop("No records found for contract type: ", contract_type_value)
  }

  # Step 2: Find the first occurrence per person and get reference data
  reference_data <- target_contracts[
    order(get(person_id), get(start_date_col), get(end_date_col))
  ][, .SD[1], by = person_id]

  # Extract reference dates and sector codes
  # Build the list of columns to extract
  if (!is.null(person_vars)) {
    # Validate person_vars exist in data
    missing_vars <- setdiff(person_vars, names(reference_data))
    if (length(missing_vars) > 0) {
      stop("person_vars not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }

  # Create the reference_sectors data.table with conditional inclusion of person_vars
  if (is.null(person_vars)) {
    reference_sectors <- reference_data[, .(
      person_id = get(person_id),
      reference_date = get(end_date_col),
      reference_sector = get(sector_col)
    )]
  } else {
    # Build the list expression with person_vars
    j_expr <- paste0(".(person_id = get(person_id), reference_date = get(end_date_col), reference_sector = get(sector_col)",
                    paste0(", ", person_vars, " = ", person_vars, collapse = ""),
                    ")")
    reference_sectors <- reference_data[, eval(parse(text = j_expr))]
  }

  setnames(reference_sectors, "person_id", person_id)

  if (nrow(reference_sectors) == 0) {
    stop("No reference dates could be established")
  }

  # Step 3: Vectorized quarter generation for all persons at once
  quarter_days <- 91  # 91 days per quarter (4 quarters = 364 days ≈ 12 months)

  all_quarters <- reference_sectors[, {
    q_nums <- 1:n_quarters
    .(
      quarter = paste0("Q", q_nums),
      quarter_num = q_nums,
      quarter_start = reference_date + (q_nums - 1) * quarter_days + 1,
      quarter_end = reference_date + q_nums * quarter_days,
      quarter_days = quarter_days
    )
  }, by = person_id]

  setkeyv(all_quarters, c(person_id, "quarter_num"))

  # Step 4: Vectorized sector calculation
  setkeyv(dt, person_id)

  # Process in chunks to prevent memory issues
  n_persons <- nrow(reference_sectors)

  if (n_persons <= chunk_size) {
    # Process all at once
    all_trajectories <- calculate_sector_trajectories_vectorized(dt, all_quarters,
                                                                start_date_col, end_date_col,
                                                                person_id, arco_col, sector_col,
                                                                reference_sectors, quarter_days)
  } else {
    # Process in chunks
    person_list <- reference_sectors[[person_id]]
    chunks <- split(person_list, ceiling(seq_along(person_list) / chunk_size))

    trajectory_list <- vector("list", length(chunks))

    for (i in seq_along(chunks)) {
      chunk_persons <- chunks[[i]]
      chunk_quarters <- all_quarters[get(person_id) %in% chunk_persons]
      chunk_data <- dt[get(person_id) %in% chunk_persons]
      chunk_reference <- reference_sectors[get(person_id) %in% chunk_persons]

      trajectory_list[[i]] <- calculate_sector_trajectories_vectorized(chunk_data, chunk_quarters,
                                                                      start_date_col, end_date_col,
                                                                      person_id, arco_col, sector_col,
                                                                      chunk_reference, quarter_days)
    }

    all_trajectories <- rbindlist(trajectory_list)
  }

  # Create summary of trajectory patterns
  # Create trajectory sequence for each person using vectorized operations
  trajectory_sequences <- all_trajectories[
    order(get(person_id), quarter_num)
  ][, .(trajectory = paste(sector_status, collapse = " -> ")),
    by = person_id]

  # Count trajectory patterns
  trajectory_summary <- trajectory_sequences[,
    .(count = .N,
      percentage = round(.N / nrow(reference_sectors) * 100, 1)),
    by = trajectory][order(-count)]

  # Prepare data for alluvial plot
  plot_data <- copy(all_trajectories)

  # Create the plot if requested
  plot_obj <- NULL
  if (return_plot) {
    # Load required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting")
    }
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
      stop("Package 'ggalluvial' is required for alluvial plots")
    }

    # Determine colors
    if (use_bw) {
      palette <- "bw"
    }

    # Get colors for sector status
    sector_statuses <- c("Same Sector", "Different Sector", "Not Working", "No Information")

    if (palette == "sector" && !use_bw) {
      colors <- c(
        "Same Sector" = "#3498DB",        # Blue
        "Different Sector" = "#9B59B6",   # Purple
        "Not Working" = "#E74C3C",        # Red
        "No Information" = "#95A5A6"      # Grey
      )
    } else {
      # Use vecshift_colors function
      base_colors <- vecshift_colors(palette = if (use_bw) "bw" else "main",
                                   n = length(sector_statuses))
      colors <- setNames(base_colors[1:length(sector_statuses)], sector_statuses)
    }

    # Create alluvial plot
    plot_obj <- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x = quarter, stratum = sector_status,
                                          alluvium = !!sym(person_id), fill = sector_status)) +
      ggalluvial::geom_flow(alpha = 0.6, curve_type = "cubic", width = 0.3) +
      ggalluvial::geom_stratum(alpha = 0.8, width = 0.3) +
      ggplot2::geom_text(stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3, color = "white", fontface = "bold") +
      ggplot2::scale_fill_manual(values = colors, name = "Sector\nStatus") +
      ggplot2::labs(
        title = if (is.null(plot_title)) {
          paste("Sector Trajectories After", contract_type_value, "Contract")
        } else {
          plot_title
        },
        subtitle = if (is.null(plot_subtitle)) {
          paste("Tracking", nrow(reference_sectors), "individuals over", n_quarters, "quarters")
        } else {
          plot_subtitle
        },
        x = "Quarter After Contract End",
        caption = paste("Reference contract type:", contract_type_value)
      ) +
      theme_vecshift() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  # Create transitions aggregation
  # Sort data by person and quarter to get consecutive quarters
  all_trajectories_sorted <- all_trajectories[order(get(person_id), quarter_num)]

  # Create transitions by comparing consecutive quarters for each person
  transitions_raw <- all_trajectories_sorted[, {
    if (.N > 1) {
      data.table(
        from_quarter = quarter_num[-.N],
        to_quarter = quarter_num[-1],
        from_status = sector_status[-.N],
        to_status = sector_status[-1]
      )
    } else {
      # If only one quarter, no transitions
      data.table()
    }
  }, by = person_id]

  # Aggregate transitions based on whether person_vars exist
  if (!is.null(person_vars) && length(person_vars) > 0) {
    # Merge person_vars from reference_sectors into transitions
    transitions_with_vars <- merge(transitions_raw,
                                   reference_sectors[, c(person_id, person_vars), with = FALSE],
                                   by = person_id, all.x = TRUE)

    # Group by transition characteristics and person_vars, then count
    group_cols <- c("from_quarter", "to_quarter", "from_status", "to_status", person_vars)
    transition_counts <- transitions_with_vars[, .(count = .N), by = group_cols]
  } else {
    # Group by transition characteristics only
    transition_counts <- transitions_raw[, .(count = .N),
                                        by = .(from_quarter, to_quarter, from_status, to_status)]
  }

  # Order by from_quarter, to_quarter, and count
  setkeyv(transition_counts, c("from_quarter", "to_quarter", "count"))
  setorderv(transition_counts, c("from_quarter", "to_quarter", "count"), order = c(1, 1, -1))

  # Prepare return object
  result <- list(
    data = all_trajectories,
    summary = trajectory_summary,
    transitions = transition_counts,
    plot = plot_obj,
    parameters = list(
      contract_type_var = contract_type_var,
      contract_type_value = contract_type_value,
      sector_col = sector_col,
      n_quarters = n_quarters,
      person_id = person_id,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      arco_col = arco_col,
      palette = palette,
      use_bw = use_bw,
      quarter_days = quarter_days,
      chunk_size = chunk_size,
      person_vars = person_vars,
      n_individuals = nrow(reference_sectors)
    )
  )

  return(result)
}

#' Calculate professional group trajectories vectorized (internal helper function)
#'
#' @param dt data.table with employment contracts
#' @param quarters_dt data.table with quarters per person
#' @param start_col Start date column name
#' @param end_col End date column name
#' @param person_col Person ID column name
#' @param arco_col Employment intensity column name
#' @param group_col Professional group column name
#' @param reference_groups data.table with reference groups per person
#' @param quarter_days Number of days per quarter
#' @param group_mapping Named vector mapping groups (keeps original group names)
#' @return data.table with professional group trajectories
#' @keywords internal
calculate_professional_group_trajectories_vectorized <- function(dt, quarters_dt, start_col, end_col, person_col, arco_col, group_col, reference_groups, quarter_days, group_mapping) {
  # Create cartesian join between quarters and employment data
  overlaps <- merge(quarters_dt, dt, by = person_col, allow.cartesian = TRUE)

  # Filter to overlapping periods only (include all arco values)
  overlaps <- overlaps[
    get(start_col) <= quarter_end & get(end_col) >= quarter_start
  ]

  # Calculate employment period within each quarter
  overlaps[, `:=`(
    emp_start = pmax(get(start_col), quarter_start),
    emp_end = pmin(get(end_col), quarter_end)
  )]

  # Remove invalid periods
  overlaps <- overlaps[emp_start <= emp_end]

  # Handle group selection within quarters
  # Sort by person, quarter, and employment period
  setkeyv(overlaps, c(person_col, "quarter_num", "emp_end"))

  # Group by person-quarter and select group
  group_cols <- c(person_col, "quarter", "quarter_num", "quarter_start", "quarter_end", "quarter_days")
  group_codes <- overlaps[, {
    if (.N == 0) {
      data.table(quarter_group = NA_character_, all_arco_zero = TRUE)
    } else {
      # Check if there are any employment contracts (arco > 0)
      employment_contracts <- .SD[get(arco_col) > 0]

      if (nrow(employment_contracts) > 0) {
        # Prioritize employment contracts - get group from latest end date
        latest_employment <- employment_contracts[.N]  # Already sorted by emp_end
        .(quarter_group = latest_employment[[group_col]],
          all_arco_zero = FALSE)
      } else {
        # All contracts have arco = 0
        # Still get group from latest contract for potential future use
        latest_contract <- .SD[.N]  # Already sorted by emp_end
        .(quarter_group = latest_contract[[group_col]],
          all_arco_zero = TRUE)
      }
    }
  }, by = group_cols]

  # Get all person-quarter combinations (including those with no contracts)
  all_combinations <- copy(quarters_dt)

  # Merge with group codes
  result <- merge(all_combinations, group_codes,
                 by = c(person_col, "quarter", "quarter_num", "quarter_start", "quarter_end", "quarter_days"),
                 all.x = TRUE)

  # Add reference group data
  result <- merge(result, reference_groups, by = person_col)

  # Apply group mapping to quarter_group
  result[!is.na(quarter_group), quarter_group := group_mapping[quarter_group]]
  result[!is.na(reference_group), reference_group := group_mapping[reference_group]]

  # Determine professional group status - use actual group names
  # Handle three cases: unemployment (arco=0), no data, or actual group
  result[, group_status := fcase(
    all_arco_zero == TRUE, "Not Working",  # Has contracts but all with arco = 0
    is.na(quarter_group), "No Information",  # No contracts at all in quarter
    default = quarter_group  # Has employment - use actual group name
  )]

  # For plotting, use the same logic - just actual group names
  result[, group_status_simple := group_status]

  # Clean up temporary columns (only remove if they exist)
  cols_to_remove <- intersect(c("emp_start", "emp_end", "all_arco_zero"), names(result))
  if (length(cols_to_remove) > 0) {
    result[, (cols_to_remove) := NULL]
  }

  return(result)
}

#' Track professional group trajectories over time
#'
#' This function analyzes how individuals move between professional groups after completing
#' a specific contract type. It tracks whether individuals stay in the same professional
#' group, move to different groups, become unemployed, or have no information available
#' over a specified number of quarters.
#'
#' The function requires exactly 4 unique professional groups in the data and uses
#' the actual group names found in the data (no relabeling).
#'
#' @param data data.table containing employment contract data processed by vecshift
#' @param contract_type_var Character string specifying the contract type variable name
#' @param contract_type_value Character string specifying the contract type to analyze
#' @param group_col Character string specifying the professional group column name (required)
#' @param n_quarters Integer specifying number of quarters to track (1-20)
#' @param person_id Character string specifying the person identifier column
#' @param start_date_col Character string specifying the contract start date column
#' @param end_date_col Character string specifying the contract end date column
#' @param arco_col Character string specifying the employment intensity column
#' @param return_plot Logical indicating whether to create an alluvial plot
#' @param palette Character string specifying the color palette to use
#' @param use_bw Logical indicating whether to use black and white colors
#' @param plot_title Character string for plot title (auto-generated if NULL)
#' @param plot_subtitle Character string for plot subtitle (auto-generated if NULL)
#' @param person_vars Character vector of person-level variable names to include
#'   in the output data (e.g., c("eta", "sesso", "istruzione")). Variables are
#'   extracted from the first target contract for each person. Default is NULL
#'   (no additional variables).
#' @param chunk_size Integer specifying the chunk size for processing large datasets
#'
#' @details
#' The function identifies individuals who completed the specified contract type and tracks
#' their professional group transitions over the following quarters. For each person-quarter,
#' the function shows which of the 4 professional groups the person is working in, or:
#'
#' \itemize{
#'   \item \strong{[Group Name]}: Person works in one of the 4 professional groups (actual group names from data)
#'   \item \strong{Not Working}: Person has contracts but with arco = 0 (unemployment/inactivity)
#'   \item \strong{No Information}: Person has no contracts in the quarter
#' }
#'
#' The function validates that exactly 4 unique professional groups exist in the target
#' contracts and uses the actual group names found in the data:
#' \itemize{
#'   \item Groups preserve their original names from the data
#'   \item Colors assigned automatically: first group (Red), second group (Blue), third group (Green), fourth group (Orange)
#'   \item Additional categories: "Not Working" (Grey), "No Information" (Dark Grey)
#' }
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: data.table with individual-level quarterly group trajectories
#'   \item \code{summary}: data.table with aggregated trajectory patterns
#'   \item \code{transitions}: data.table with aggregated transition counts between quarters,
#'     including columns: from_quarter, to_quarter, from_status, to_status, person_vars (if provided), count
#'   \item \code{plot}: ggplot2 alluvial plot (if return_plot = TRUE)
#'   \item \code{parameters}: List of function parameters used
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track professional group trajectories after temporary contracts
#' result <- track_professional_group_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   group_col = "professional_category",
#'   n_quarters = 4
#' )
#'
#' # View the summary of group trajectory patterns
#' print(result$summary)
#'
#' # Display the alluvial plot
#' print(result$plot)
#'
#' # Track trajectories with custom visualization
#' result_custom <- track_professional_group_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   group_col = "job_level",
#'   n_quarters = 6,
#'   plot_title = "Professional Group Mobility After Temporary Contracts"
#' )
#'
#' # Black and white visualization
#' result_bw <- track_professional_group_trajectories(
#'   data = employment_data,
#'   contract_type_value = "B.01.00",
#'   group_col = "professional_category",
#'   use_bw = TRUE
#' )
#' }
#'
#' @seealso \code{\link{track_contract_trajectories}}, \code{\link{track_sector_trajectories}},
#' \code{\link{track_employer_trajectories}}, \code{\link{theme_vecshift}}
track_professional_group_trajectories <- function(data,
                                                  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
                                                  contract_type_value,
                                                  group_col,
                                                  n_quarters = 4,
                                                  person_id = "cf",
                                                  start_date_col = "inizio",
                                                  end_date_col = "fine",
                                                  arco_col = "arco",
                                                  return_plot = TRUE,
                                                  palette = "professional_group",
                                                  use_bw = FALSE,
                                                  plot_title = NULL,
                                                  plot_subtitle = NULL,
                                                  person_vars = NULL,
                                                  chunk_size = 10000) {

  # Input validation
  if (!is.data.table(data)) {
    stop("Input data must be a data.table object")
  }

  if (missing(group_col)) {
    stop("group_col parameter must be specified")
  }

  required_cols <- c(person_id, contract_type_var, start_date_col, end_date_col, arco_col, group_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (missing(contract_type_value)) {
    stop("contract_type_value must be specified")
  }

  if (n_quarters < 1 || n_quarters > 20) {
    stop("n_quarters must be between 1 and 20")
  }

  # Create working copy
  dt <- copy(data)
  setkeyv(dt, c(person_id, start_date_col))

  # Convert date columns to Date class if needed
  date_cols <- c(start_date_col, end_date_col)
  for (col in date_cols) {
    if (!inherits(dt[[col]], "Date")) {
      dt[[col]] <- as.Date(dt[[col]])
    }
  }

  # Step 1: Identify individuals who had the specific contract type
  target_contracts <- dt[get(contract_type_var) == contract_type_value]

  if (nrow(target_contracts) == 0) {
    stop("No records found for contract type: ", contract_type_value)
  }

  # Step 2: Validate professional groups and create mapping
  # Get unique groups from target contracts (excluding NAs)
  unique_groups <- sort(unique(target_contracts[!is.na(get(group_col)), get(group_col)]))

  if (length(unique_groups) != 4) {
    stop("Professional group column must contain exactly 4 unique groups. Found ",
         length(unique_groups), " groups: ", paste(unique_groups, collapse = ", "))
  }

  # Create group mapping - use actual group names, no relabeling
  group_mapping <- setNames(unique_groups, unique_groups)

  # Step 3: Find the first occurrence per person and get reference data
  reference_data <- target_contracts[
    order(get(person_id), get(start_date_col), get(end_date_col))
  ][, .SD[1], by = person_id]

  # Extract reference dates and group codes
  # Build the list of columns to extract
  if (!is.null(person_vars)) {
    # Validate person_vars exist in data
    missing_vars <- setdiff(person_vars, names(reference_data))
    if (length(missing_vars) > 0) {
      stop("person_vars not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }

  # Create the reference_groups data.table with conditional inclusion of person_vars
  if (is.null(person_vars)) {
    reference_groups <- reference_data[, .(
      person_id = get(person_id),
      reference_date = get(end_date_col),
      reference_group = get(group_col)
    )]
  } else {
    # Build the list expression with person_vars
    j_expr <- paste0(".(person_id = get(person_id), reference_date = get(end_date_col), reference_group = get(group_col)",
                    paste0(", ", person_vars, " = ", person_vars, collapse = ""),
                    ")")
    reference_groups <- reference_data[, eval(parse(text = j_expr))]
  }

  setnames(reference_groups, "person_id", person_id)

  if (nrow(reference_groups) == 0) {
    stop("No reference dates could be established")
  }

  # Step 4: Vectorized quarter generation for all persons at once
  quarter_days <- 91  # 91 days per quarter (4 quarters = 364 days ≈ 12 months)

  all_quarters <- reference_groups[, {
    q_nums <- 1:n_quarters
    .(
      quarter = paste0("Q", q_nums),
      quarter_num = q_nums,
      quarter_start = reference_date + (q_nums - 1) * quarter_days + 1,
      quarter_end = reference_date + q_nums * quarter_days,
      quarter_days = quarter_days
    )
  }, by = person_id]

  setkeyv(all_quarters, c(person_id, "quarter_num"))

  # Step 5: Vectorized group calculation
  setkeyv(dt, person_id)

  # Process in chunks to prevent memory issues
  n_persons <- nrow(reference_groups)
  if (n_persons <= chunk_size) {
    # Process all at once
    all_trajectories <- calculate_professional_group_trajectories_vectorized(dt, all_quarters,
                                                                             start_date_col, end_date_col,
                                                                             person_id, arco_col, group_col,
                                                                             reference_groups, quarter_days, group_mapping)
  } else {
    # Process in chunks
    person_list <- reference_groups[[person_id]]
    chunks <- split(person_list, ceiling(seq_along(person_list) / chunk_size))
    trajectory_list <- vector("list", length(chunks))

    for (i in seq_along(chunks)) {
      chunk_persons <- chunks[[i]]
      chunk_quarters <- all_quarters[get(person_id) %in% chunk_persons]
      chunk_data <- dt[get(person_id) %in% chunk_persons]
      chunk_reference <- reference_groups[get(person_id) %in% chunk_persons]

      trajectory_list[[i]] <- calculate_professional_group_trajectories_vectorized(chunk_data, chunk_quarters,
                                                                                   start_date_col, end_date_col,
                                                                                   person_id, arco_col, group_col,
                                                                                   chunk_reference, quarter_days, group_mapping)
    }

    all_trajectories <- rbindlist(trajectory_list)
  }

  # Create summary of trajectory patterns
  # Create trajectory sequence for each person using vectorized operations
  trajectory_sequences <- all_trajectories[
    order(get(person_id), quarter_num)
  ][, .(trajectory = paste(group_status_simple, collapse = " -> ")),
    by = person_id]

  # Count trajectory patterns
  trajectory_summary <- trajectory_sequences[,
    .(count = .N,
      percentage = round(.N / nrow(reference_groups) * 100, 1)),
    by = trajectory][order(-count)]

  # Prepare data for alluvial plot
  plot_data <- copy(all_trajectories)

  # Create the plot if requested
  plot_obj <- NULL
  if (return_plot) {
    # Load required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting")
    }
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
      stop("Package 'ggalluvial' is required for alluvial plots")
    }

    # Determine colors
    if (use_bw) {
      palette <- "bw"
    }

    # Get all possible group statuses for coloring
    group_statuses <- unique(plot_data$group_status_simple)

    if (palette == "professional_group" && !use_bw) {
      # Define colors for professional groups
      # Assign colors to actual group names found in data
      group_colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12")  # Red, Blue, Green, Orange

      # Create color mapping for actual group names
      actual_groups <- unique_groups
      colors <- c(
        setNames(group_colors[1:length(actual_groups)], actual_groups),
        "Not Working" = "#95A5A6",       # Grey
        "No Information" = "#34495E"     # Dark Grey
      )

      # Filter to only the statuses present in data
      colors <- colors[names(colors) %in% group_statuses]
    } else {
      # Use vecshift_colors function
      base_colors <- vecshift_colors(palette = if (use_bw) "bw" else "main",
                                   n = length(group_statuses))
      colors <- setNames(base_colors[1:length(group_statuses)], group_statuses)
    }

    # Create alluvial plot
    plot_obj <- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x = quarter, stratum = group_status_simple,
                                          alluvium = !!sym(person_id), fill = group_status_simple)) +
      ggalluvial::geom_flow(alpha = 0.6, curve_type = "cubic", width = 0.3) +
      ggalluvial::geom_stratum(alpha = 0.8, width = 0.3) +
      ggplot2::geom_text(stat = ggalluvial::StatStratum,
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3, color = "white", fontface = "bold") +
      ggplot2::scale_fill_manual(values = colors, name = "Professional\nGroup Status") +
      ggplot2::labs(
        title = if (is.null(plot_title)) {
          paste("Professional Group Trajectories After", contract_type_value, "Contract")
        } else {
          plot_title
        },
        subtitle = if (is.null(plot_subtitle)) {
          paste("Tracking", nrow(reference_groups), "individuals over", n_quarters, "quarters")
        } else {
          plot_subtitle
        },
        x = "Quarter After Contract End",
        caption = paste("Reference contract type:", contract_type_value, "| Professional groups:", paste(unique_groups, collapse = ", "))
      ) +
      theme_vecshift() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  # Create transitions aggregation
  # Sort data by person and quarter to get consecutive quarters
  all_trajectories_sorted <- all_trajectories[order(get(person_id), quarter_num)]

  # Create transitions by comparing consecutive quarters for each person
  transitions_raw <- all_trajectories_sorted[, {
    if (.N > 1) {
      data.table(
        from_quarter = quarter_num[-.N],
        to_quarter = quarter_num[-1],
        from_status = group_status[-.N],
        to_status = group_status[-1]
      )
    } else {
      # If only one quarter, no transitions
      data.table()
    }
  }, by = person_id]

  # Aggregate transitions based on whether person_vars exist
  if (!is.null(person_vars) && length(person_vars) > 0) {
    # Merge person_vars from reference_groups into transitions
    transitions_with_vars <- merge(transitions_raw,
                                   reference_groups[, c(person_id, person_vars), with = FALSE],
                                   by = person_id, all.x = TRUE)

    # Group by transition characteristics and person_vars, then count
    group_cols <- c("from_quarter", "to_quarter", "from_status", "to_status", person_vars)
    transition_counts <- transitions_with_vars[, .(count = .N), by = group_cols]
  } else {
    # Group by transition characteristics only
    transition_counts <- transitions_raw[, .(count = .N),
                                        by = .(from_quarter, to_quarter, from_status, to_status)]
  }

  # Order by from_quarter, to_quarter, and count
  setkeyv(transition_counts, c("from_quarter", "to_quarter", "count"))
  setorderv(transition_counts, c("from_quarter", "to_quarter", "count"), order = c(1, 1, -1))

  # Prepare return object
  result <- list(
    data = all_trajectories,
    summary = trajectory_summary,
    transitions = transition_counts,
    plot = plot_obj,
    parameters = list(
      contract_type_var = contract_type_var,
      contract_type_value = contract_type_value,
      group_col = group_col,
      n_quarters = n_quarters,
      person_id = person_id,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      arco_col = arco_col,
      chunk_size = chunk_size,
      group_mapping = group_mapping
    )
  )

  return(result)
}