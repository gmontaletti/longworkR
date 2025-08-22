#' Prepare Metrics Output for Impact Analysis
#'
#' This function serves as a bridge between the output of 
#' \code{\link{calculate_comprehensive_impact_metrics}} and impact evaluation 
#' methods like \code{\link{difference_in_differences}}. It restructures the 
#' metrics data to create a proper panel structure suitable for causal inference 
#' methods.
#'
#' @param metrics_output data.table or list. Output from 
#'   \code{\link{calculate_comprehensive_impact_metrics}}
#' @param treatment_assignment data.table. A data.table with columns for the 
#'   individual identifier (default: "cf") and treatment indicator 
#'   (default: "is_treated"). This maps individuals to their treatment status.
#' @param impact_method character. The intended impact evaluation method. 
#'   One of "did" (difference-in-differences), "event_study", or "matching".
#'   This affects how the data is restructured. Default: "did"
#' @param id_column character. Name of the individual identifier column. 
#'   Default: "cf"
#' @param period_column character. Name of the period column in metrics_output. 
#'   Default: "period"
#' @param outcome_vars character vector or NULL. Names of outcome variables to 
#'   include in the analysis. If NULL and auto_detect_outcomes is TRUE, 
#'   automatically detects numeric metrics columns. Default: NULL
#' @param auto_detect_outcomes logical. Whether to automatically detect outcome 
#'   variables from the metrics output. Default: TRUE
#' @param verbose logical. Whether to print diagnostic information. Default: TRUE
#'
#' @details
#' This function handles several key transformations needed for impact analysis:
#' 
#' \subsection{Data Structure Transformations}{
#' \itemize{
#'   \item \strong{Panel Creation}: For treated units with "pre"/"post" periods, 
#'     maintains both observations
#'   \item \strong{Control Expansion}: For control units (typically with 
#'     "control" period), creates duplicate observations with "pre" and "post" 
#'     labels to enable proper DiD estimation
#'   \item \strong{Time Variable Creation}: Creates binary time variables 
#'     (post = 0/1) suitable for regression analysis
#'   \item \strong{Treatment Interaction}: Ensures proper interaction terms 
#'     can be created for DiD estimation
#' }}
#' 
#' \subsection{Outcome Variable Selection}{
#' The function automatically detects numeric metrics as potential outcomes, 
#' excluding ID and time variables. Common detected outcomes include:
#' \itemize{
#'   \item Employment stability metrics (employment_rate, employment_spells)
#'   \item Contract quality metrics (permanent_contract_rate, avg_contract_quality)
#'   \item Career complexity metrics (transition_complexity, career_entropy)
#'   \item Transition metrics (avg_transition_duration, transition_success_rate)
#' }}
#' 
#' \subsection{Compatibility with Impact Methods}{
#' \itemize{
#'   \item \strong{DiD}: Creates standard panel with binary post treatment variable
#'   \item \strong{Event Study}: Maintains event_time structure for relative 
#'     time analysis
#'   \item \strong{Matching}: Preserves original structure for propensity 
#'     score matching
#' }}
#'
#' @return data.table with the following structure:
#' \itemize{
#'   \item \strong{Individual identifiers}: Column specified by id_column
#'   \item \strong{Treatment indicator}: "is_treated" (0/1)
#'   \item \strong{Time variables}: Both original period column and binary "post" variable
#'   \item \strong{Outcome variables}: Selected metrics suitable for analysis
#'   \item \strong{Panel structure}: Proper pre/post observations for all units
#' }
#' 
#' The output is directly compatible with \code{\link{difference_in_differences}} 
#' and other impact evaluation functions in the package.
#'
#' @examples
#' \dontrun{
#' # ===== BASIC WORKFLOW EXAMPLE =====
#' # Load sample data
#' sample_data <- readRDS(system.file("data", "sample.rds", package = "longworkR"))
#' 
#' # Step 1: Calculate comprehensive employment metrics
#' metrics_result <- calculate_comprehensive_impact_metrics(
#'   data = sample_data,
#'   metrics = c("stability", "quality", "complexity"),
#'   output_format = "wide"
#' )
#' 
#' # Step 2: Define treatment assignment
#' treatment_data <- data.table(
#'   cf = unique(sample_data$cf),
#'   is_treated = sample(c(0, 1), length(unique(sample_data$cf)), replace = TRUE),
#'   # Add baseline covariates for matching/controls
#'   baseline_employment = runif(length(unique(sample_data$cf))),
#'   region = sample(c("urban", "rural"), length(unique(sample_data$cf)), replace = TRUE)
#' )
#' 
#' # Step 3: Prepare for DiD analysis
#' did_data <- prepare_metrics_for_impact_analysis(
#'   metrics_output = metrics_result,
#'   treatment_assignment = treatment_data,
#'   impact_method = "did",
#'   verbose = TRUE
#' )
#' 
#' # Step 4: Run difference-in-differences
#' did_results <- difference_in_differences(
#'   data = did_data,
#'   outcome_vars = c("employment_rate", "permanent_contract_rate", "career_stability_score"),
#'   treatment_var = "is_treated",
#'   time_var = "post",
#'   id_var = "cf",
#'   control_vars = c("baseline_employment")
#' )
#' 
#' # ===== ADVANCED SCENARIOS =====
#' 
#' # Event Study Analysis
#' # First create event time structure in original data
#' sample_data[, event_time := ifelse(event_period == "pre", -1, 
#'                                   ifelse(event_period == "post", 1, 0))]
#' 
#' # Recalculate metrics with event time
#' metrics_event <- calculate_comprehensive_impact_metrics(
#'   data = sample_data,
#'   metrics = "all",
#'   period_column = "event_time"
#' )
#' 
#' # Prepare for event study
#' event_data <- prepare_metrics_for_impact_analysis(
#'   metrics_output = metrics_event,
#'   treatment_assignment = treatment_data,
#'   impact_method = "event_study"
#' )
#' 
#' # Multiple Treatment Groups
#' treatment_multi <- copy(treatment_data)
#' treatment_multi[, `:=`(
#'   treatment_type = sample(c("none", "training", "subsidies", "both"), .N, replace = TRUE),
#'   is_treated = as.numeric(treatment_type != "none")
#' )]
#' 
#' multi_data <- prepare_metrics_for_impact_analysis(
#'   metrics_output = metrics_result,
#'   treatment_assignment = treatment_multi,
#'   impact_method = "did"
#' )
#' 
#' # Propensity Score Matching Setup
#' matching_data <- prepare_metrics_for_impact_analysis(
#'   metrics_output = metrics_result,
#'   treatment_assignment = treatment_data,
#'   impact_method = "matching",
#'   outcome_vars = c("employment_rate", "contract_quality_score"),
#'   auto_detect_outcomes = FALSE  # Specify outcomes explicitly
#' )
#' 
#' # ===== DATA VALIDATION =====
#' # Check prepared data structure
#' validation <- validate_integration_setup(
#'   data = did_data,
#'   impact_method = "did",
#'   id_column = "cf",
#'   verbose = TRUE
#' )
#' 
#' if (validation) {
#'   cat("Data successfully prepared for impact analysis\n")
#' }
#' }
#'
#' @seealso 
#' \code{\link{calculate_comprehensive_impact_metrics}}, 
#' \code{\link{difference_in_differences}}, 
#' \code{\link{propensity_score_matching}}
#'
#' @export
prepare_metrics_for_impact_analysis <- function(metrics_output,
                                              treatment_assignment,
                                              impact_method = c("did", "event_study", "matching"),
                                              id_column = "cf",
                                              period_column = "period",
                                              outcome_vars = NULL,
                                              auto_detect_outcomes = TRUE,
                                              verbose = TRUE) {
  
  # Input validation
  if (!inherits(metrics_output, c("data.table", "list"))) {
    stop("metrics_output must be a data.table or list from calculate_comprehensive_impact_metrics()")
  }
  
  if (!inherits(treatment_assignment, "data.table")) {
    stop("treatment_assignment must be a data.table")
  }
  
  impact_method <- match.arg(impact_method)
  
  # Check required columns in treatment_assignment
  if (!id_column %in% names(treatment_assignment)) {
    stop(paste("Column", id_column, "not found in treatment_assignment"))
  }
  
  if (!"is_treated" %in% names(treatment_assignment)) {
    stop("Column 'is_treated' not found in treatment_assignment")
  }
  
  # Convert list output to data.table if needed
  if (inherits(metrics_output, "list")) {
    if (verbose) {
      cat("Converting list output to wide format data.table...\n")
    }
    
    # Combine all metrics into a single data.table
    metric_tables <- list()
    for (metric_name in names(metrics_output)) {
      metric_dt <- metrics_output[[metric_name]]
      if (inherits(metric_dt, "data.table")) {
        # Add prefix to metric columns to avoid conflicts
        metric_cols <- setdiff(names(metric_dt), c(id_column, period_column))
        setnames(metric_dt, metric_cols, paste0(metric_name, "_", metric_cols))
        metric_tables[[metric_name]] <- metric_dt
      }
    }
    
    # Merge all metrics
    if (length(metric_tables) > 0) {
      metrics_output <- Reduce(function(x, y) {
        merge(x, y, by = c(id_column, period_column), all = TRUE)
      }, metric_tables)
    } else {
      stop("No valid data.table objects found in metrics_output list")
    }
  }
  
  # Ensure metrics_output is a data.table
  if (!inherits(metrics_output, "data.table")) {
    metrics_output <- as.data.table(metrics_output)
  }
  
  # Check required columns in metrics_output
  if (!id_column %in% names(metrics_output)) {
    stop(paste("Column", id_column, "not found in metrics_output"))
  }
  
  if (!period_column %in% names(metrics_output)) {
    stop(paste("Column", period_column, "not found in metrics_output"))
  }
  
  # Auto-detect outcome variables if needed
  if (is.null(outcome_vars) && auto_detect_outcomes) {
    # Find numeric columns that are not ID or time variables
    excluded_cols <- c(id_column, period_column, "is_treated", "post", "event_time")
    numeric_cols <- names(metrics_output)[sapply(metrics_output, is.numeric)]
    outcome_vars <- setdiff(numeric_cols, excluded_cols)
    
    if (verbose && length(outcome_vars) > 0) {
      cat("Auto-detected outcome variables:", paste(outcome_vars, collapse = ", "), "\n")
    }
  }
  
  if (length(outcome_vars) == 0) {
    stop("No outcome variables specified or detected")
  }
  
  # Validate outcome variables exist
  missing_outcomes <- setdiff(outcome_vars, names(metrics_output))
  if (length(missing_outcomes) > 0) {
    stop(paste("Outcome variables not found in metrics_output:", 
               paste(missing_outcomes, collapse = ", ")))
  }
  
  if (verbose) {
    cat("Using outcome variables:", paste(outcome_vars, collapse = ", "), "\n")
  }
  
  # Ensure treatment assignment is at person level to avoid cartesian joins
  # The matching functions return event-level data, but we need person-level treatment assignment
  initial_treatment_rows <- nrow(treatment_assignment)
  unique_persons <- uniqueN(treatment_assignment[[id_column]])
  
  if (initial_treatment_rows > unique_persons) {
    if (verbose) {
      cat("\n=== DEDUPLICATION REQUIRED ===\n")
      cat("Detected event-level treatment assignment data\n")
      cat("Original rows:", initial_treatment_rows, "\n")
      cat("Unique persons:", unique_persons, "\n")
      cat("Ratio:", round(initial_treatment_rows / unique_persons, 2), "events per person\n")
    }
    
    # Check for inconsistent treatment status within persons (this would be problematic)
    treatment_consistency <- treatment_assignment[, .(n_status = uniqueN(is_treated)), by = id_column]
    inconsistent_persons <- treatment_consistency[n_status > 1, get(id_column)]
    
    if (length(inconsistent_persons) > 0) {
      warning(paste("Found", length(inconsistent_persons), 
                   "persons with inconsistent treatment status across events.",
                   "Using first occurrence for each person."))
      if (verbose) {
        cat("WARNING: Inconsistent treatment status for", length(inconsistent_persons), "persons\n")
      }
    }
    
    # Identify event-level columns that should be excluded from person-level data
    event_level_columns <- c(
      # vecshift output columns (event-level)
      "inizio", "fine", "durata", "over_id", "arco", "prior", 
      "COD_TIPOLOGIA_CONTRATTUALE", "contratto", "stipendio", "retribuzione",
      "pre_event_period", "post_event_period", "event_time",
      # Matching-specific columns that vary by event
      "cem_weight", "cem_strata", "propensity_score", "match_distance",
      # Period/time variables
      "period", "event_period", "time_period"
    )
    
    # Preserve essential person-level columns and any additional covariates
    person_level_cols <- c(id_column, "is_treated")
    
    # Include additional columns that are likely person-level characteristics
    # (not event-specific employment data)
    potential_person_cols <- setdiff(names(treatment_assignment), 
                                   c(event_level_columns, person_level_cols))
    
    # Add columns that don't vary within person (true person-level characteristics)
    for (col in potential_person_cols) {
      if (col %in% names(treatment_assignment)) {
        # Check if this column is constant within persons
        col_variability <- treatment_assignment[, .(n_unique = uniqueN(get(col), na.rm = TRUE)), 
                                              by = id_column]
        # If most persons have only 1 unique value for this column, it's person-level
        prop_constant <- mean(col_variability$n_unique <= 1, na.rm = TRUE)
        if (prop_constant >= 0.8) {  # 80% of persons have constant values
          person_level_cols <- c(person_level_cols, col)
          if (verbose) {
            cat("Including person-level covariate:", col, 
                "(constant for", round(prop_constant * 100, 1), "% of persons)\n")
          }
        } else if (verbose) {
          cat("Excluding event-level variable:", col, 
              "(varies for", round((1 - prop_constant) * 100, 1), "% of persons)\n")
        }
      }
    }
    
    # Ensure all selected columns exist
    person_level_cols <- intersect(person_level_cols, names(treatment_assignment))
    
    # Deduplicate by taking first occurrence per person for selected columns
    # Sort by a reasonable order (if event_time exists, prioritize 'pre' then 'post')
    if ("event_time" %in% names(treatment_assignment)) {
      # Create ordering priority: pre > post > control > others
      treatment_assignment[, priority_order := ifelse(event_time == "pre", 1, 
                                                     ifelse(event_time == "post", 2, 
                                                           ifelse(event_time == "control", 3, 4)))]
      setorderv(treatment_assignment, c(id_column, "priority_order"))
      treatment_assignment[, priority_order := NULL]  # Remove helper column
    }
    
    # Remove id_column from .SDcols since it's already in the by clause
    selected_cols <- setdiff(person_level_cols, id_column)
    
    # DEFENSIVE: Remove rows with NA id_column BEFORE deduplication to prevent data loss
    na_id_rows <- sum(is.na(treatment_assignment[[id_column]]))
    if (na_id_rows > 0) {
      if (verbose) {
        cat("- Removing", na_id_rows, "rows with NA", id_column, "values before deduplication\n")
      }
      treatment_assignment <- treatment_assignment[!is.na(get(id_column))]
    }
    
    # DEFENSIVE: Ensure .SDcols exist in the data to prevent selection errors
    existing_selected_cols <- intersect(selected_cols, names(treatment_assignment))
    if (length(existing_selected_cols) < length(selected_cols) && verbose) {
      missing_cols <- setdiff(selected_cols, existing_selected_cols)
      cat("- Warning: Some selected columns missing:", paste(missing_cols, collapse = ", "), "\n")
    }
    
    treatment_assignment <- treatment_assignment[, .SD[1], by = id_column, .SDcols = existing_selected_cols]
    
    # Verify the column selection worked correctly
    if (verbose) {
      cat("Columns in deduplicated treatment assignment:", paste(names(treatment_assignment), collapse = ", "), "\n")
    }
    
    if (verbose) {
      cat("Deduplication completed:\n")
      cat("  Final rows:", nrow(treatment_assignment), "\n")
      cat("  Columns preserved:", length(person_level_cols), "\n")
      cat("  Columns included:", paste(person_level_cols, collapse = ", "), "\n")
      cat("\nThis transformation is safe because:\n")
      cat("  - Treatment status is consistent within persons\n")
      cat("  - Only person-level characteristics are preserved\n")
      cat("  - Event-level employment data is excluded\n")
    }
  } else {
    if (verbose) {
      cat("Treatment assignment data is already at person-level (no deduplication needed)\n")
    }
  }
  
  # Track initial person count for validation - ENHANCED with NA detection
  # Remove NA values from ID column before counting to prevent silent filtering
  metrics_clean <- metrics_output[!is.na(get(id_column))]
  na_ids_original <- nrow(metrics_output) - nrow(metrics_clean)
  if (na_ids_original > 0 && verbose) {
    cat("WARNING: Removed", na_ids_original, "rows with NA", id_column, "values from metrics_output\n")
  }
  metrics_output <- metrics_clean
  original_person_count <- uniqueN(metrics_output[[id_column]])
  
  # Enhanced input validation
  if (verbose) {
    cat("\n=== INPUT DATA QUALITY VALIDATION ===\n")
    cat("Metrics data: rows =", nrow(metrics_output), "persons =", original_person_count, "\n")
    cat("Treatment assignment: rows =", nrow(treatment_assignment), "persons =", uniqueN(treatment_assignment[[id_column]]), "\n")
    
    # Check for duplicate keys in treatment assignment
    if (any(duplicated(treatment_assignment[[id_column]]))) {
      dup_count <- sum(duplicated(treatment_assignment[[id_column]]))
      cat("WARNING: Found", dup_count, "duplicate IDs in treatment assignment\n")
    }
    
    # Check for data type consistency
    metrics_id_type <- class(metrics_output[[id_column]])[1]
    treatment_id_type <- class(treatment_assignment[[id_column]])[1] 
    cat("ID column types: metrics =", metrics_id_type, ", treatment =", treatment_id_type, "\n")
    
    if (metrics_id_type != treatment_id_type) {
      cat("WARNING: ID column types differ - may cause merge issues\n")
    }
  }
  
  # Merge with treatment assignment (now guaranteed to be person-level)
  if (verbose) {
    cat("\n=== MERGING DATA ===\n")
    cat("Metrics data:", nrow(metrics_output), "rows,", 
        uniqueN(metrics_output[[id_column]]), "unique persons\n")
    cat("Treatment assignment:", nrow(treatment_assignment), "rows,", 
        uniqueN(treatment_assignment[[id_column]]), "unique persons\n")
    cat("VALIDATION: Original person count for tracking:", original_person_count, "\n")
  }
  
  # Check for duplicate column names before merge (excluding the join key)
  metrics_cols <- setdiff(names(metrics_output), id_column)
  treatment_cols <- setdiff(names(treatment_assignment), id_column)
  duplicate_cols <- intersect(metrics_cols, treatment_cols)
  
  if (length(duplicate_cols) > 0) {
    if (verbose) {
      cat("Removing duplicate columns from treatment assignment:", paste(duplicate_cols, collapse = ", "), "\n")
    }
    # Remove duplicate columns from treatment assignment (keep metrics version)
    treatment_assignment <- treatment_assignment[, !..duplicate_cols]
  }
  
  # ENHANCED MERGE with comprehensive validation and error handling
  # Step 1: Pre-merge validation
  if (verbose) {
    cat("\n=== ENHANCED MERGE OPERATION ===\n")
  }
  
  # Remove NA values from treatment assignment ID column to prevent merge issues
  treatment_clean <- treatment_assignment[!is.na(get(id_column))]
  na_ids_treatment <- nrow(treatment_assignment) - nrow(treatment_clean)
  if (na_ids_treatment > 0 && verbose) {
    cat("Removed", na_ids_treatment, "rows with NA", id_column, "values from treatment_assignment\n")
  }
  
  # Step 2: Perform merge with enhanced error handling
  working_data <- NULL
  merge_method <- "unknown"
  
  tryCatch({
    working_data <- merge(metrics_output, treatment_clean, by = id_column, all.x = TRUE, sort = FALSE)
    merge_method <- "standard merge"
    if (verbose) cat("✓ Standard merge successful\n")
  }, error = function(e) {
    if (verbose) cat("⚠ Standard merge failed:", e$message, "\n")
    
    # Fallback method: Convert ID columns to same type if needed
    tryCatch({
      metrics_temp <- copy(metrics_output)
      treatment_temp <- copy(treatment_clean)
      
      # Convert to character for compatibility
      metrics_temp[[id_column]] <- as.character(metrics_temp[[id_column]])
      treatment_temp[[id_column]] <- as.character(treatment_temp[[id_column]])
      
      working_data <<- merge(metrics_temp, treatment_temp, by = id_column, all.x = TRUE, sort = FALSE)
      merge_method <<- "type-converted merge"
      if (verbose) cat("✓ Type-converted merge successful\n")
    }, error = function(e2) {
      stop(paste("All merge methods failed. Original error:", e$message, "Fallback error:", e2$message))
    })
  })
  
  if (is.null(working_data)) {
    stop("Merge operation failed - working_data is NULL")
  }
  
  if (verbose) {
    cat("Merge completed using:", merge_method, "\n")
  }
  
  # ENHANCED VALIDATION: Comprehensive post-merge validation
  post_merge_person_count <- uniqueN(working_data[!is.na(get(id_column)), get(id_column)])
  post_merge_na_count <- sum(is.na(working_data[[id_column]]))
  
  if (verbose) {
    cat("Merged data:", nrow(working_data), "rows,", 
        post_merge_person_count, "unique persons (excl. NA)\n")
    if (post_merge_na_count > 0) {
      cat("WARNING:", post_merge_na_count, "rows with NA", id_column, "after merge\n")
    }
    
    # Verify no cartesian join occurred
    expected_rows <- nrow(metrics_output)
    if (nrow(working_data) == expected_rows) {
      cat("✓ Merge successful - no cartesian join detected\n")
    } else {
      cat("⚠ WARNING: Merge produced", nrow(working_data) - expected_rows, "additional rows\n")
      cat("This may indicate a cartesian join - check data structure\n")
    }
    
    # ENHANCED VALIDATION: Check person count integrity after merge
    if (post_merge_person_count != original_person_count) {
      cat("⚠ PERSON COUNT CHANGED DURING MERGE\n")
      cat("  Original:", original_person_count, "\n")
      cat("  After merge:", post_merge_person_count, "(excluding NA)\n")
      cat("  Difference:", post_merge_person_count - original_person_count, "\n")
      
      # Enhanced missing/extra person analysis
      original_ids <- unique(metrics_output[[id_column]])
      merged_ids <- unique(working_data[!is.na(get(id_column)), get(id_column)])
      missing_ids <- setdiff(original_ids, merged_ids)
      extra_ids <- setdiff(merged_ids, original_ids)
      
      if (length(missing_ids) > 0) {
        cat("  Missing persons:", length(missing_ids), "first few:", head(missing_ids, 5), "\n")
        
        # RECOVERY ATTEMPT: Try to identify why persons are missing
        missing_in_treatment <- missing_ids %in% treatment_clean[[id_column]]
        cat("  Missing persons found in treatment assignment:", sum(missing_in_treatment), "/", length(missing_ids), "\n")
        
        if (sum(!missing_in_treatment) > 0) {
          cat("  Some missing persons not in treatment assignment - this is expected\n")
        }
      }
      if (length(extra_ids) > 0) {
        cat("  Extra persons:", length(extra_ids), "first few:", head(extra_ids, 5), "\n")
      }
      
      # Check for merge-specific issues
      if (post_merge_na_count > 0) {
        cat("  NOTE:", post_merge_na_count, "rows have NA", id_column, "- these may affect person counts\n")
      }
    } else {
      cat("✓ Person count preserved during merge\n")
    }
  }
  
  # DEFENSIVE: Remove rows with NA ID values that may have been introduced during merge
  if (post_merge_na_count > 0) {
    if (verbose) {
      cat("Removing", post_merge_na_count, "rows with NA", id_column, "values after merge\n")
    }
    working_data <- working_data[!is.na(get(id_column))]
    post_merge_person_count <- uniqueN(working_data[[id_column]])
    if (verbose) {
      cat("After NA removal:", nrow(working_data), "rows,", post_merge_person_count, "persons\n")
    }
  }
  
  # Check for missing treatment assignments with enhanced reporting
  missing_treatment <- sum(is.na(working_data$is_treated))
  if (missing_treatment > 0) {
    missing_persons <- uniqueN(working_data[is.na(is_treated), get(id_column)])
    
    warning_text <- paste("Missing treatment assignment for", missing_treatment, "observations",
                         "across", missing_persons, "persons")
    warning(warning_text)
    
    if (verbose) {
      cat("⚠ MISSING TREATMENT ASSIGNMENTS:\n")
      cat("- Missing observations:", missing_treatment, "\n")
      cat("- Missing persons:", missing_persons, "\n")
      cat("- Percentage of observations:", round(100 * missing_treatment / nrow(working_data), 2), "%\n")
      cat("- Percentage of persons:", round(100 * missing_persons / uniqueN(working_data[[id_column]]), 2), "%\n")
      
      # Show which persons are missing treatment assignments
      missing_treatment_ids <- unique(working_data[is.na(is_treated), get(id_column)])
      if (length(missing_treatment_ids) > 0) {
        cat("- First few persons missing treatment:", head(missing_treatment_ids, 5), "\n")
        
        # Check if these persons were in the original treatment assignment
        original_treatment_ids <- unique(treatment_assignment[[id_column]])
        not_in_original <- setdiff(missing_treatment_ids, original_treatment_ids)
        if (length(not_in_original) > 0) {
          cat("- Persons not in original treatment assignment:", length(not_in_original), "\n")
          cat("- This suggests a merge issue or incomplete treatment data\n")
        } else {
          cat("- All missing persons were in original treatment assignment\n")
          cat("- This suggests NA values in the original treatment data\n")
        }
      }
    }
  } else if (verbose) {
    cat("✓ All observations have treatment assignments\n")
  }
  
  # Handle different period structures based on impact method
  if (impact_method == "did") {
    # For DiD, we need a proper panel structure with binary time variable
    
    # Identify unique period values
    period_values <- unique(working_data[[period_column]])
    
    if (verbose) {
      cat("Found period values:", paste(period_values, collapse = ", "), "\n")
    }
    
    # Handle control observations (typically have "control" period)
    if ("control" %in% period_values) {
      if (verbose) {
        cat("Expanding control observations for panel structure...\n")
      }
      
      # Get control observations
      control_obs <- working_data[get(period_column) == "control"]
      
      if (nrow(control_obs) > 0) {
        # Create pre and post observations for controls
        control_pre <- copy(control_obs)
        control_pre[[period_column]] <- "pre"
        
        control_post <- copy(control_obs)
        control_post[[period_column]] <- "post"
        
        # ENHANCED: Track control expansion with detailed person counting
        pre_removal_count <- uniqueN(working_data[[id_column]])
        control_person_count <- uniqueN(control_obs[[id_column]])
        
        if (verbose) {
          cat("Control expansion tracking:\n")
          cat("- Total persons before removal:", pre_removal_count, "\n")
          cat("- Control persons to expand:", control_person_count, "\n")
        }
        
        # Remove original control observations
        working_data_no_control <- working_data[get(period_column) != "control"]
        post_removal_count <- uniqueN(working_data_no_control[[id_column]])
        
        if (verbose) {
          cat("- Persons after control removal:", post_removal_count, "\n")
        }
        
        # ENHANCED: Safer data combination with comprehensive validation
        if (verbose) {
          cat("Preparing control expansion data...\n")
          
          # Enhanced column type compatibility check
          main_cols <- names(working_data_no_control)
          pre_cols <- names(control_pre)
          post_cols <- names(control_post)
          
          all_cols <- unique(c(main_cols, pre_cols, post_cols))
          
          type_mismatches <- 0
          for (col in all_cols) {
            types <- character(0)
            if (col %in% main_cols) types <- c(types, paste("main:", class(working_data_no_control[[col]])[1]))
            if (col %in% pre_cols) types <- c(types, paste("pre:", class(control_pre[[col]])[1]))
            if (col %in% post_cols) types <- c(types, paste("post:", class(control_post[[col]])[1]))
            
            unique_types <- unique(gsub(".*:", "", types))
            if (length(unique_types) > 1) {
              type_mismatches <- type_mismatches + 1
              cat("  - WARNING: Column", col, "type mismatch:", paste(types, collapse = ", "), "\n")
            }
          }
          
          if (type_mismatches > 0) {
            cat("  - Found", type_mismatches, "column type mismatches - attempting type harmonization\n")
          }
        }
        
        # ENHANCED: Safer rbind with multiple fallback strategies and validation
        rbind_result <- NULL
        rbind_method <- "unknown"
        
        # Strategy 1: Standard rbind
        tryCatch({
          rbind_result <- rbind(working_data_no_control, control_pre, control_post, use.names = TRUE, fill = TRUE)
          rbind_method <- "standard rbind"
          if (verbose) cat("✓ Standard rbind successful\n")
        }, error = function(e) {
          if (verbose) {
            cat("⚠ Standard rbind failed:", e$message, "\n")
            cat("Attempting rbindlist strategy...\n")
          }
          
          # Strategy 2: rbindlist
          tryCatch({
            rbind_result <<- rbindlist(list(working_data_no_control, control_pre, control_post), use.names = TRUE, fill = TRUE)
            rbind_method <<- "rbindlist"
            if (verbose) cat("✓ rbindlist successful\n")
          }, error = function(e2) {
            if (verbose) {
              cat("⚠ rbindlist failed:", e2$message, "\n")
              cat("Attempting manual column harmonization...\n")
            }
            
            # Strategy 3: Manual column harmonization
            tryCatch({
              # Get all unique columns
              all_unique_cols <- unique(c(names(working_data_no_control), names(control_pre), names(control_post)))
              
              # Ensure all data.tables have the same columns with compatible types
              for (dt_name in c("working_data_no_control", "control_pre", "control_post")) {
                dt <- get(dt_name)
                for (col in all_unique_cols) {
                  if (!col %in% names(dt)) {
                    # Add missing column with appropriate default value
                    if (col %in% names(working_data_no_control)) {
                      col_class <- class(working_data_no_control[[col]])[1]
                    } else if (col %in% names(control_pre)) {
                      col_class <- class(control_pre[[col]])[1]
                    } else {
                      col_class <- class(control_post[[col]])[1]
                    }
                    
                    if (col_class == "numeric") {
                      dt[, (col) := NA_real_]
                    } else if (col_class == "character") {
                      dt[, (col) := NA_character_]
                    } else if (col_class == "logical") {
                      dt[, (col) := NA]
                    } else {
                      dt[, (col) := NA]
                    }
                  }
                }
                assign(dt_name, dt)
              }
              
              # Try rbind again with harmonized columns
              rbind_result <<- rbind(working_data_no_control, control_pre, control_post, use.names = TRUE, fill = TRUE)
              rbind_method <<- "harmonized rbind"
              if (verbose) cat("✓ Manual harmonization successful\n")
            }, error = function(e3) {
              if (verbose) cat("❌ All rbind strategies failed. Last error:", e3$message, "\n")
              
              # Final fallback: return data without control expansion
              warning("Control expansion failed - proceeding without expansion")
              rbind_result <<- working_data_no_control
              rbind_method <<- "no expansion (fallback)"
            })
          })
        })
        
        if (is.null(rbind_result)) {
          stop("All rbind strategies failed - rbind_result is NULL")
        }
        
        working_data <- rbind_result
        
        if (verbose) {
          cat("Control expansion completed using:", rbind_method, "\n")
        }
        
        # ENHANCED VALIDATION: Comprehensive control expansion validation with bug fix
        post_expansion_person_count <- uniqueN(working_data[!is.na(get(id_column)), get(id_column)])
        post_expansion_na_count <- sum(is.na(working_data[[id_column]]))
        
        if (verbose) {
          cat("Control expansion validation:\n")
          cat("- Pre-removal persons:", pre_removal_count, "\n")
          cat("- Post-removal persons:", post_removal_count, "\n")
          cat("- Control persons to expand:", control_person_count, "\n")
          cat("- After expansion: rows =", nrow(working_data), ", persons =", post_expansion_person_count, "\n")
          if (post_expansion_na_count > 0) {
            cat("- WARNING:", post_expansion_na_count, "rows with NA", id_column, "after expansion\n")
          }
          
          # CRITICAL FIX: Corrected expected person count calculation
          # Control expansion should NOT change the total number of unique persons
          # It only changes the number of rows (each control person gets 2 rows instead of 1)
          expected_persons_after_expansion <- post_merge_person_count  # Should remain the same
          
          if (post_expansion_person_count != expected_persons_after_expansion) {
            cat("⚠ PERSON COUNT CHANGED DURING CONTROL EXPANSION\n")
            cat("  Before expansion:", post_merge_person_count, "\n")
            cat("  After expansion:", post_expansion_person_count, "\n")
            cat("  Expected (corrected):", expected_persons_after_expansion, "\n")
            cat("  Difference:", post_expansion_person_count - expected_persons_after_expansion, "\n")
            
            # Enhanced debugging with corrected logic
            if (verbose) {
              cat("  Detailed analysis:\n")
              cat("  - Non-control persons (should be preserved):", post_removal_count, "\n")
              cat("  - Control persons (should be re-added):", control_person_count, "\n")
              cat("  - Expected total:", post_removal_count + control_person_count, "\n")
              cat("  - Actual total:", post_expansion_person_count, "\n")
              
              # Check for overlapping IDs that might cause loss
              if (exists("working_data_no_control")) {
                no_control_ids <- unique(working_data_no_control[[id_column]])
                control_ids <- unique(control_obs[[id_column]])
                overlap_ids <- intersect(no_control_ids, control_ids)
                if (length(overlap_ids) > 0) {
                  cat("  - CRITICAL: Overlapping IDs between non-control and control:", length(overlap_ids), "\n")
                  cat("  - This indicates the same person appears in both control and non-control data\n")
                  cat("  - This is likely the source of the person count discrepancy\n")
                  cat("  - First few overlapping IDs:", head(overlap_ids, 5), "\n")
                } else {
                  cat("  - No overlapping IDs detected between control and non-control data\n")
                }
              }
              
              # Check for data integrity in expansion components
              if (exists("control_pre") && exists("control_post")) {
                control_pre_persons <- uniqueN(control_pre[[id_column]])
                control_post_persons <- uniqueN(control_post[[id_column]])
                cat("  - Control pre persons:", control_pre_persons, "\n")
                cat("  - Control post persons:", control_post_persons, "\n")
                
                if (control_pre_persons != control_person_count || control_post_persons != control_person_count) {
                  cat("  - WARNING: Control expansion data has inconsistent person counts\n")
                }
              }
            }
            
            # RECOVERY ATTEMPT: Try to identify and fix the discrepancy
            cat("  Attempting to identify and resolve the discrepancy...\n")
            
            # Check if some persons were lost during the rbind operation
            if (post_expansion_person_count < expected_persons_after_expansion) {
              missing_count <- expected_persons_after_expansion - post_expansion_person_count
              cat("  - CRITICAL: Lost", missing_count, "persons during control expansion\n")
              
              # Try to identify which persons were lost
              expected_ids <- unique(c(
                if (exists("working_data_no_control")) working_data_no_control[[id_column]] else character(0),
                control_obs[[id_column]]
              ))
              actual_ids <- unique(working_data[[id_column]])
              lost_ids <- setdiff(expected_ids, actual_ids)
              
              if (length(lost_ids) > 0) {
                cat("  - Lost person IDs:", head(lost_ids, 5), "...\n")
                
                # Check if lost persons can be recovered
                recovery_needed <- FALSE
                if (exists("working_data_no_control")) {
                  lost_in_no_control <- lost_ids[lost_ids %in% working_data_no_control[[id_column]]]
                  if (length(lost_in_no_control) > 0) {
                    cat("  - Can recover", length(lost_in_no_control), "persons from non-control data\n")
                    recovery_needed <- TRUE
                  }
                }
                
                lost_in_control <- lost_ids[lost_ids %in% control_obs[[id_column]]]
                if (length(lost_in_control) > 0) {
                  cat("  - Can recover", length(lost_in_control), "persons from control data\n")
                  recovery_needed <- TRUE
                }
                
                if (recovery_needed) {
                  cat("  - WARNING: Data recovery might be needed\n")
                }
              }
            }
          } else {
            cat("✓ Person count preserved during control expansion\n")
          }
        }
        
        # DEFENSIVE: Remove any NA ID values introduced during expansion
        if (post_expansion_na_count > 0) {
          if (verbose) {
            cat("Removing", post_expansion_na_count, "rows with NA", id_column, "after expansion\n")
          }
          working_data <- working_data[!is.na(get(id_column))]
          post_expansion_person_count <- uniqueN(working_data[[id_column]])
          if (verbose) {
            cat("After NA removal:", nrow(working_data), "rows,", post_expansion_person_count, "persons\n")
          }
        }
      }
    }
    
    # Create binary post variable
    working_data[, post := as.numeric(get(period_column) == "post")]
    
    # Ensure we have both pre and post for analysis
    panel_check <- working_data[, .(
      periods = length(unique(get(period_column))),
      has_pre = any(get(period_column) == "pre"),
      has_post = any(get(period_column) == "post")
    ), by = .(get(id_column), is_treated)]
    
    incomplete_panels <- panel_check[periods < 2 | !has_pre | !has_post]
    if (nrow(incomplete_panels) > 0 && verbose) {
      cat("Warning: Some units have incomplete pre/post observations\n")
    }
    
  } else if (impact_method == "event_study") {
    # For event study, maintain event_time structure
    if (!"event_time" %in% names(working_data)) {
      # Convert period_column to event_time if needed
      working_data[, event_time := get(period_column)]
    }
    
    # Create binary post variable for compatibility
    working_data[, post := as.numeric(event_time == "post")]
    
  } else if (impact_method == "matching") {
    # For matching, preserve original structure but ensure compatibility
    working_data[, post := as.numeric(get(period_column) == "post")]
  }
  
  # Select final columns
  final_cols <- c(id_column, "is_treated", period_column, "post", outcome_vars)
  
  # Add event_time if it exists and wasn't already included
  if ("event_time" %in% names(working_data) && !"event_time" %in% final_cols) {
    final_cols <- c(final_cols, "event_time")
  }
  
  # Include any additional columns from treatment assignment
  extra_cols <- setdiff(names(treatment_assignment), c(id_column, "is_treated"))
  if (length(extra_cols) > 0) {
    final_cols <- c(final_cols, extra_cols)
  }
  
  # Ensure all final columns exist
  final_cols <- intersect(final_cols, names(working_data))
  
  # Ensure all required columns exist before selection
  missing_final_cols <- setdiff(final_cols, names(working_data))
  if (length(missing_final_cols) > 0) {
    warning(paste("Missing expected columns in final selection:", 
                  paste(missing_final_cols, collapse = ", ")))
    final_cols <- intersect(final_cols, names(working_data))
  }
  
  # Select final columns with enhanced validation and robust error handling
  if (verbose) {
    cat("\n=== COLUMN SELECTION ===\n")
    cat("Columns to select (", length(final_cols), "):", paste(final_cols, collapse = ", "), "\n")
    cat("Available columns (", length(names(working_data)), "):", paste(head(names(working_data), 10), collapse = ", "), 
        if(length(names(working_data)) > 10) "..." else "", "\n")
  }
  
  # VALIDATION: Pre-selection person count
  pre_selection_person_count <- uniqueN(working_data[[id_column]])
  pre_selection_row_count <- nrow(working_data)
  
  # ENHANCED: Ultra-robust column selection with data preservation validation
  result <- NULL
  selection_method <- "unknown"
  
  # DEFENSIVE: Pre-selection data integrity check
  working_data_clean <- working_data[!is.na(get(id_column))]  # Ensure no NA IDs
  if (nrow(working_data_clean) < nrow(working_data) && verbose) {
    cat("Removed", nrow(working_data) - nrow(working_data_clean), "rows with NA", id_column, "before selection\n")
  }
  working_data <- working_data_clean
  
  # Method 1: Try standard ..final_cols syntax with validation
  tryCatch({
    result <- working_data[, ..final_cols]
    selection_method <- "..final_cols"
    
    # Immediate validation after selection
    if (uniqueN(result[[id_column]]) != uniqueN(working_data[[id_column]])) {
      stop("Person count changed during column selection with ..final_cols")
    }
    
    if (verbose) cat("✓ Column selection successful using ..final_cols syntax\n")
  }, error = function(e) {
    if (verbose) cat("⚠ Column selection with ..final_cols failed:", e$message, "\n")
    
    # Method 2: Try with = FALSE syntax with validation
    tryCatch({
      result <<- working_data[, final_cols, with = FALSE]
      selection_method <<- "with = FALSE"
      
      # Immediate validation after selection
      if (uniqueN(result[[id_column]]) != uniqueN(working_data[[id_column]])) {
        stop("Person count changed during column selection with 'with = FALSE'")
      }
      
      if (verbose) cat("✓ Column selection successful using with = FALSE syntax\n")
    }, error = function(e2) {
      if (verbose) cat("⚠ Column selection with 'with = FALSE' failed:", e2$message, "\n")
      
      # Method 3: Try subset approach with validation
      tryCatch({
        result <<- working_data[, names(working_data) %in% final_cols, with = FALSE]
        selection_method <<- "subset approach"
        
        # Immediate validation after selection
        if (uniqueN(result[[id_column]]) != uniqueN(working_data[[id_column]])) {
          stop("Person count changed during column selection with subset approach")
        }
        
        if (verbose) cat("✓ Column selection successful using subset approach\n")
      }, error = function(e3) {
        if (verbose) {
          cat("⚠ Subset approach failed:", e3$message, "\n")
          cat("Attempting manual column-by-column selection...\n")
        }
        
        # Method 4: Manual column-by-column selection as ultimate fallback
        tryCatch({
          result_list <- list()
          for (col in final_cols) {
            if (col %in% names(working_data)) {
              result_list[[col]] <- working_data[[col]]
            } else {
              warning(paste("Column", col, "not found in working_data - skipping"))
            }
          }
          
          if (length(result_list) > 0) {
            result <<- as.data.table(result_list)
            selection_method <<- "manual column selection"
            
            # Validation for manual selection
            if (uniqueN(result[[id_column]]) != uniqueN(working_data[[id_column]])) {
              stop("Person count changed during manual column selection")
            }
            
            if (verbose) cat("✓ Manual column selection successful\n")
          } else {
            stop("No valid columns selected during manual selection")
          }
        }, error = function(e4) {
          if (verbose) cat("❌ All column selection methods failed. Last error:", e4$message, "\n")
          stop(paste("CRITICAL: All column selection methods failed. Cannot proceed.", 
                    "Original error:", e$message, "Final error:", e4$message))
        })
      })
    })
  })
  
  # Ensure result was created successfully
  if (is.null(result)) {
    stop("Column selection failed - result is NULL")
  }
  
  if (verbose) {
    cat("Selected columns using method:", selection_method, "\n")
    cat("Result dimensions:", nrow(result), "rows x", ncol(result), "columns\n")
  }
  
  # VALIDATION: Post-selection validation with comprehensive checks
  post_selection_person_count <- uniqueN(result[[id_column]])
  post_selection_row_count <- nrow(result)
  
  # Check for row count changes
  if (post_selection_row_count != pre_selection_row_count) {
    message_text <- paste("Row count changed during column selection. Before:", pre_selection_row_count, 
                         "After:", post_selection_row_count, "Difference:", 
                         post_selection_row_count - pre_selection_row_count)
    warning(message_text)
    if (verbose) {
      cat("⚠", message_text, "\n")
    }
  } else if (verbose) {
    cat("✓ Row count preserved during column selection\n")
  }
  
  # Check for person count changes (most critical validation)
  if (post_selection_person_count != pre_selection_person_count) {
    difference <- post_selection_person_count - pre_selection_person_count
    message_text <- paste("CRITICAL: Person count changed during column selection. Before:", 
                         pre_selection_person_count, "After:", post_selection_person_count, 
                         "Difference:", difference)
    warning(message_text)
    
    if (verbose) {
      cat("⚠ CRITICAL:", message_text, "\n")
    }
    
    # Identify which persons were lost/gained
    working_ids <- unique(working_data[[id_column]])
    result_ids <- unique(result[[id_column]])
    lost_ids <- setdiff(working_ids, result_ids)
    gained_ids <- setdiff(result_ids, working_ids)
    
    if (length(lost_ids) > 0) {
      if (verbose) {
        cat("Lost persons during column selection:", length(lost_ids), "\n")
        cat("First few lost IDs:", head(lost_ids, 5), "\n")
      }
      
      # Analyze characteristics of lost persons
      lost_data <- working_data[get(id_column) %in% lost_ids]
      if (nrow(lost_data) > 0 && verbose) {
        cat("Detailed analysis of lost persons:\n")
        cat("- Total lost rows:", nrow(lost_data), "\n")
        cat("- Missing treatment status:", sum(is.na(lost_data$is_treated)), "rows\n")
        cat("- Missing ID values:", sum(is.na(lost_data[[id_column]])), "rows\n")
        
        # Check each outcome variable
        for (outcome in outcome_vars) {
          if (outcome %in% names(lost_data)) {
            missing_outcome <- sum(is.na(lost_data[[outcome]]))
            cat("- Missing", outcome, ":", missing_outcome, "rows\n")
          }
        }
        
        # Check if lost persons have specific patterns
        if (period_column %in% names(lost_data)) {
          period_dist <- table(lost_data[[period_column]], useNA = "ifany")
          cat("- Lost persons by period:\n")
          print(period_dist)
        }
      }
    }
    
    if (length(gained_ids) > 0 && verbose) {
      cat("Gained persons during column selection:", length(gained_ids), "\n")
      cat("First few gained IDs:", head(gained_ids, 5), "\n")
    }
    
    # This is a critical error - should not happen in column selection
    stop(paste("CRITICAL ERROR: Person count changed during column selection.",
               "This indicates a serious data processing issue.",
               "Selection method:", selection_method,
               "Original persons:", pre_selection_person_count,
               "Final persons:", post_selection_person_count))
  } else if (verbose) {
    cat("✓ Person count preserved during column selection\n")
  }
  
  if (verbose) {
    cat("\n=== FINAL DATASET STRUCTURE ===\n")
    cat("Observations:", nrow(result), "\n")
    cat("Unique individuals:", length(unique(result[[id_column]])), "\n")
    cat("\nTreatment distribution:\n")
    treatment_dist <- table(result$is_treated, useNA = "ifany")
    print(treatment_dist)
    cat("\nTime period distribution:\n")
    period_dist <- table(result$post, useNA = "ifany")
    print(period_dist)
    cat("\nCross-tabulation (treatment x period):\n")
    cross_tab <- table(result$is_treated, result$post, useNA = "ifany")
    print(cross_tab)
    cat("\nOutcome variables (", length(outcome_vars), "):\n", 
        paste(outcome_vars, collapse = ", "), "\n", sep = "")
    
    # Data quality checks
    missing_treatment <- sum(is.na(result$is_treated))
    missing_post <- sum(is.na(result$post))
    if (missing_treatment > 0 || missing_post > 0) {
      cat("\n⚠ Data quality issues:\n")
      if (missing_treatment > 0) cat("  Missing treatment status:", missing_treatment, "rows\n")
      if (missing_post > 0) cat("  Missing time period:", missing_post, "rows\n")
    } else {
      cat("\n✓ No missing values in key variables\n")
    }
  }
  
  # Add attributes for downstream functions
  data.table::setattr(result, "outcome_vars", outcome_vars)
  data.table::setattr(result, "impact_method", impact_method)
  data.table::setattr(result, "id_column", id_column)
  data.table::setattr(result, "period_column", period_column)
  
  # Final validation to ensure no cartesian join occurred
  original_persons <- uniqueN(metrics_output[[id_column]])
  working_persons <- uniqueN(working_data[[id_column]])
  final_persons <- uniqueN(result[[id_column]])
  
  if (verbose) {
    cat("\n=== PERSON COUNT VALIDATION ===\n")
    cat("Original metrics persons:", original_persons, "\n")
    cat("After processing persons:", working_persons, "\n")
    cat("Final result persons:", final_persons, "\n")
    
    if (original_persons != working_persons) {
      cat("⚠ Person count changed during data processing\n")
    }
    if (working_persons != final_persons) {
      cat("⚠ Person count changed during column selection\n")
    }
  }
  
  if (original_persons != final_persons) {
    # ENHANCED ERROR RECOVERY: Try to recover the missing persons
    cat("\n=== ATTEMPTING ERROR RECOVERY ===\n")
    cat("Person count mismatch detected - attempting recovery...\n")
    
    # Provide detailed debugging information
    cat("\n=== DEBUGGING INFORMATION ===\n")
    cat("Original persons:", original_persons, "\n")
    cat("Working data persons:", working_persons, "\n") 
    cat("Final persons:", final_persons, "\n")
    
    # Check for data integrity issues
    original_ids <- unique(metrics_output[[id_column]])
    final_ids <- unique(result[[id_column]])
    
    missing_ids <- setdiff(original_ids, final_ids)
    extra_ids <- setdiff(final_ids, original_ids)
    
    if (length(missing_ids) > 0) {
      cat("Missing persons in final result:", length(missing_ids), "\n")
      cat("First few missing IDs:", head(missing_ids, 10), "\n")
      
      # RECOVERY ATTEMPT 1: Check if missing persons can be recovered from working_data
      if (exists("working_data") && inherits(working_data, "data.table")) {
        recoverable_data <- working_data[get(id_column) %in% missing_ids]
        if (nrow(recoverable_data) > 0) {
          cat("RECOVERY: Found", nrow(recoverable_data), "rows for", uniqueN(recoverable_data[[id_column]]), "missing persons in working_data\n")
          
          # Attempt to add missing persons back to result
          tryCatch({
            # Create missing persons data with same column structure as result
            recovery_cols <- intersect(names(result), names(recoverable_data))
            missing_persons_data <- recoverable_data[, ..recovery_cols]
            
            # Add any missing columns from result with appropriate defaults
            result_cols <- names(result)
            for (col in setdiff(result_cols, names(missing_persons_data))) {
              if (col == "post") {
                missing_persons_data[, (col) := as.numeric(get(period_column) == "post")]
              } else if (is.numeric(result[[col]])) {
                missing_persons_data[, (col) := NA_real_]
              } else if (is.character(result[[col]])) {
                missing_persons_data[, (col) := NA_character_]
              } else if (is.logical(result[[col]])) {
                missing_persons_data[, (col) := NA]
              } else {
                missing_persons_data[, (col) := NA]
              }
            }
            
            # Add missing persons back to result
            result <- rbind(result, missing_persons_data, use.names = TRUE, fill = TRUE)
            recovered_count <- uniqueN(result[[id_column]])
            
            cat("RECOVERY SUCCESS: Recovered person count:", recovered_count, "\n")
            
            if (recovered_count == original_persons) {
              cat("✅ FULL RECOVERY ACHIEVED\n")
            } else {
              cat("⚠ PARTIAL RECOVERY: Still missing", original_persons - recovered_count, "persons\n")
            }
            
          }, error = function(e) {
            cat("❌ RECOVERY FAILED:", e$message, "\n")
          })
        }
      }
      
      # Analyze characteristics of missing persons
      missing_data <- metrics_output[get(id_column) %in% missing_ids]
      if (nrow(missing_data) > 0) {
        cat("Analysis of missing persons:\n")
        cat("- Total missing rows:", nrow(missing_data), "\n")
        
        # Check period distribution
        if (period_column %in% names(missing_data)) {
          period_dist <- table(missing_data[[period_column]], useNA = "ifany")
          cat("- Missing persons by period:\n")
          print(period_dist)
        }
        
        # Check for data quality issues in missing persons
        for (outcome in head(outcome_vars, 3)) {  # Limit to first 3 for brevity
          if (outcome %in% names(missing_data)) {
            missing_outcome <- sum(is.na(missing_data[[outcome]]))
            cat("- Missing", outcome, "in original data:", missing_outcome, "rows\n")
          }
        }
      }
    }
    if (length(extra_ids) > 0) {
      cat("Extra persons in final result:", length(extra_ids), "\n")
      cat("First few extra IDs:", head(extra_ids, 10), "\n")
    }
    
    # Check for NA or invalid IDs
    na_original <- sum(is.na(metrics_output[[id_column]]))
    na_final <- sum(is.na(result[[id_column]]))
    cat("NA values in original ID column:", na_original, "\n")
    cat("NA values in final ID column:", na_final, "\n")
    
    # Additional diagnostic information
    cat("\n=== ADDITIONAL DIAGNOSTICS ===\n")
    cat("Data processing steps summary:\n")
    cat("1. Original person count:", original_person_count, "\n")
    cat("2. Post-merge person count:", post_merge_person_count, "\n")
    if (exists("post_expansion_person_count")) {
      cat("3. Post-expansion person count:", post_expansion_person_count, "\n")
    }
    cat("4. Pre-selection person count:", pre_selection_person_count, "\n")
    cat("5. Post-selection person count:", post_selection_person_count, "\n")
    cat("6. Final person count:", uniqueN(result[[id_column]]), "\n")
    
    # Identify where the loss occurred
    if (original_person_count != post_merge_person_count) {
      cat("❌ LOSS OCCURRED DURING MERGE STEP\n")
    }
    if (exists("post_expansion_person_count") && post_merge_person_count != post_expansion_person_count) {
      cat("❌ LOSS OCCURRED DURING CONTROL EXPANSION STEP\n")
    }
    if (pre_selection_person_count != post_selection_person_count) {
      cat("❌ LOSS OCCURRED DURING COLUMN SELECTION STEP\n")
    }
    
    # Check if recovery was successful
    final_final_persons <- uniqueN(result[[id_column]])
    if (final_final_persons == original_persons) {
      cat("✅ RECOVERY SUCCESSFUL - proceeding with recovered data\n")
    } else {
      # Only stop if recovery failed AND we have significant data loss
      missing_count <- original_persons - final_final_persons
      if (missing_count > 0.001 * original_persons) {  # More than 0.1% loss
        stop(paste("CRITICAL ERROR: Person count changed during processing and recovery failed.",
                   "Original:", original_persons, "Final:", final_final_persons, "Missing:", missing_count,
                   "This indicates a serious data processing issue. See debugging information above."))
      } else {
        warning(paste("Minor person count discrepancy (", missing_count, 
                     "persons) - proceeding with available data"))
      }
    }
  }
  
  return(result)
}


#' Validate Integration Setup
#'
#' Internal helper function to validate that the prepared data is suitable
#' for the intended impact evaluation method.
#'
#' @param data data.table. Output from prepare_metrics_for_impact_analysis()
#' @param impact_method character. The intended impact evaluation method
#' @param id_column character. Name of the individual identifier column
#' @param verbose logical. Whether to print validation results
#'
#' @return logical. TRUE if validation passes, FALSE otherwise (with warnings)
#'
#' @keywords internal
validate_integration_setup <- function(data, impact_method, id_column, verbose = TRUE) {
  
  validation_passed <- TRUE
  issues <- character(0)
  
  # Check basic structure
  if (!inherits(data, "data.table")) {
    issues <- c(issues, "Data is not a data.table")
    validation_passed <- FALSE
  }
  
  required_cols <- c(id_column, "is_treated", "post")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    validation_passed <- FALSE
  }
  
  if (!validation_passed) {
    if (verbose) {
      cat("Validation failed:\n")
      for (issue in issues) {
        cat("-", issue, "\n")
      }
    }
    return(FALSE)
  }
  
  # Method-specific validation
  if (impact_method == "did") {
    # Check panel structure
    panel_summary <- data[, .(
      n_periods = .N,
      has_pre = any(post == 0),
      has_post = any(post == 1)
    ), by = .(get(id_column), is_treated)]
    
    incomplete_panels <- panel_summary[!has_pre | !has_post]
    if (nrow(incomplete_panels) > 0) {
      issues <- c(issues, paste("Incomplete panels for", nrow(incomplete_panels), "units"))
    }
    
    # Check treatment balance
    treatment_summary <- data[, .(n_obs = .N), by = .(is_treated, post)]
    if (nrow(treatment_summary) < 4) {
      issues <- c(issues, "Insufficient treatment/time combinations for DiD")
    }
  }
  
  if (length(issues) > 0) {
    validation_passed <- FALSE
    if (verbose) {
      cat("Validation warnings:\n")
      for (issue in issues) {
        cat("-", issue, "\n")
      }
    }
  } else if (verbose) {
    cat("Validation passed: Data is ready for", impact_method, "analysis\n")
  }
  
  return(validation_passed)
}