# ===============================================================================
# LONGWORKR PACKAGE: IMPACT-METRICS INTEGRATION EXAMPLES
# ===============================================================================
# 
# This file contains practical examples demonstrating different scenarios for 
# integrating employment metrics calculation with impact evaluation methods.
# 
# These examples are designed to be run independently and demonstrate:
# 1. Basic workflow integration
# 2. Advanced scenarios (event studies, multiple treatments, matching)
# 3. Real-world applications
# 4. Best practices and troubleshooting
# 
# Author: longworkR Package Team
# ===============================================================================

library(longworkR)
library(data.table)
library(ggplot2)

# ===============================================================================
# EXAMPLE 1: BASIC WORKFORCE DEVELOPMENT PROGRAM EVALUATION
# ===============================================================================

#' Basic Workforce Development Program Impact Analysis
#' 
#' This example demonstrates evaluating a job training program's impact on 
#' employment outcomes using the complete metrics-to-impact workflow.

example_1_workforce_development <- function() {
  
  cat("=== EXAMPLE 1: WORKFORCE DEVELOPMENT PROGRAM ===\n")
  
  # Step 1: Load and prepare employment data
  # In practice, this would be your real employment records
  employment_data <- create_sample_employment_data(
    n_individuals = 200,
    n_periods = 4,
    treatment_rate = 0.4,
    seed = 12345
  )
  
  # Add event periods for program evaluation
  employment_data[, event_period := ifelse(
    as.Date(inizio) < as.Date("2020-07-01"), "pre", "post"
  )]
  
  cat("Sample data created:", nrow(employment_data), "employment records\n")
  cat("Unique individuals:", length(unique(employment_data$cf)), "\n")
  
  # Step 2: Calculate comprehensive employment metrics
  cat("\n--- Step 2: Calculate Employment Metrics ---\n")
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = employment_data,
    metrics = c("stability", "quality", "complexity"),
    period_column = "event_period",
    output_format = "wide"
  )
  
  cat("Calculated metrics for", nrow(metrics_result), "person-periods\n")
  cat("Available outcome variables:", length(setdiff(names(metrics_result), c("cf", "period"))), "\n")
  
  # Step 3: Define treatment assignment (job training program)
  cat("\n--- Step 3: Define Treatment Assignment ---\n")
  set.seed(12345)
  unique_individuals <- unique(employment_data$cf)
  
  treatment_data <- data.table(
    cf = unique_individuals,
    is_treated = rbinom(length(unique_individuals), 1, 0.4),
    # Add realistic baseline characteristics
    age_category = sample(c("young", "middle", "senior"), 
                         length(unique_individuals), 
                         replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    education_level = sample(c("high_school", "vocational", "university"), 
                           length(unique_individuals), 
                           replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    baseline_employment = runif(length(unique_individuals), 0, 1),
    region = sample(c("urban", "suburban", "rural"), 
                   length(unique_individuals), 
                   replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )
  
  cat("Treatment assignment complete:\n")
  cat("  Treated individuals:", sum(treatment_data$is_treated), 
      "(", round(100 * mean(treatment_data$is_treated), 1), "%)\n")
  cat("  Control individuals:", sum(1 - treatment_data$is_treated), 
      "(", round(100 * mean(1 - treatment_data$is_treated), 1), "%)\n")
  
  # Step 4: Prepare data for impact analysis
  cat("\n--- Step 4: Prepare Data for DiD Analysis ---\n")
  did_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "did",
    verbose = TRUE
  )
  
  # Step 5: Run difference-in-differences analysis
  cat("\n--- Step 5: Run Difference-in-Differences ---\n")
  
  # Select key employment outcomes
  key_outcomes <- c("employment_rate", "permanent_contract_rate", 
                   "avg_contract_quality", "employment_stability_index")
  available_outcomes <- intersect(key_outcomes, names(did_data))
  
  if (length(available_outcomes) > 0) {
    did_results <- difference_in_differences(
      data = did_data,
      outcome_vars = available_outcomes,
      treatment_var = "is_treated",
      time_var = "post",
      id_var = "cf",
      control_vars = c("baseline_employment"),
      verbose = TRUE
    )
    
    cat("DiD analysis completed for", length(available_outcomes), "outcomes\n")
    
    # Step 6: Display results
    cat("\n--- Step 6: Results Summary ---\n")
    if (!is.null(did_results$summary_table)) {
      print(did_results$summary_table)
    } else {
      cat("Treatment effects:\n")
      for (outcome in names(did_results$estimates)) {
        est <- did_results$estimates[[outcome]]
        if (!is.null(est$coefficient)) {
          cat(sprintf("  %s: %.4f (SE: %.4f)\n", 
                     outcome, est$coefficient, est$std_error))
        }
      }
    }
    
    return(list(
      data = did_data,
      results = did_results,
      treatment_data = treatment_data,
      metrics = metrics_result
    ))
  } else {
    cat("No suitable outcome variables found\n")
    return(list(data = did_data, treatment_data = treatment_data))
  }
}

# ===============================================================================
# EXAMPLE 2: EVENT STUDY ANALYSIS OF POLICY INTERVENTION
# ===============================================================================

#' Event Study Analysis of Employment Policy
#' 
#' This example shows how to conduct an event study analysis to examine
#' the dynamic effects of a policy intervention over time.

example_2_event_study <- function() {
  
  cat("=== EXAMPLE 2: EVENT STUDY ANALYSIS ===\n")
  
  # Create employment data with event time structure
  employment_data <- create_sample_employment_data(
    n_individuals = 150,
    n_periods = 6,
    treatment_rate = 0.5,
    seed = 54321
  )
  
  # Create realistic event time structure (relative to policy implementation)
  policy_date <- as.Date("2020-06-01")
  employment_data[, event_time := floor(
    as.numeric(as.Date(inizio) - policy_date) / 90
  )]  # Quarterly periods
  
  # Keep reasonable event time window (-2 to +2 quarters)
  employment_data <- employment_data[event_time >= -2 & event_time <= 2]
  employment_data[, event_period := paste0("t", ifelse(event_time >= 0, "+", ""), event_time)]
  
  cat("Event study data prepared with", length(unique(employment_data$event_time)), "time periods\n")
  
  # Calculate metrics with event time periods
  cat("\n--- Calculate Metrics by Event Time ---\n")
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = employment_data,
    metrics = c("stability", "quality"),
    period_column = "event_period",
    output_format = "wide"
  )
  
  # Treatment assignment (policy eligibility)
  treatment_data <- data.table(
    cf = unique(employment_data$cf),
    is_treated = rbinom(length(unique(employment_data$cf)), 1, 0.5),
    policy_eligible = sample(c(0, 1), length(unique(employment_data$cf)), replace = TRUE),
    sector = sample(c("manufacturing", "services", "construction"), 
                   length(unique(employment_data$cf)), replace = TRUE)
  )
  
  # Prepare for event study
  cat("\n--- Prepare for Event Study ---\n")
  event_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "event_study",
    verbose = TRUE
  )
  
  # Add event time information for event study analysis
  event_time_mapping <- employment_data[, .(event_time = unique(event_time)), by = event_period]
  event_data <- merge(event_data, event_time_mapping, by.x = "period", by.y = "event_period", all.x = TRUE)
  
  cat("Event study data ready with", nrow(event_data), "observations\n")
  cat("Event time range:", min(event_data$event_time, na.rm = TRUE), 
      "to", max(event_data$event_time, na.rm = TRUE), "\n")
  
  return(list(
    data = event_data,
    treatment_data = treatment_data,
    metrics = metrics_result,
    note = "Ready for event study analysis with event_study_design() function"
  ))
}

# ===============================================================================
# EXAMPLE 3: MULTIPLE TREATMENT GROUPS ANALYSIS
# ===============================================================================

#' Multiple Treatment Groups Analysis
#' 
#' This example demonstrates handling multiple treatment intensities or types,
#' such as different levels of employment support services.

example_3_multiple_treatments <- function() {
  
  cat("=== EXAMPLE 3: MULTIPLE TREATMENT GROUPS ===\n")
  
  # Create employment data
  employment_data <- create_sample_employment_data(
    n_individuals = 180,
    n_periods = 4,
    treatment_rate = 0.6,  # Higher overall treatment rate
    seed = 98765
  )
  
  employment_data[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  # Calculate comprehensive metrics
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = employment_data,
    metrics = "all",
    period_column = "event_period",
    output_format = "wide"
  )
  
  # Define multiple treatment groups
  cat("\n--- Define Multiple Treatment Groups ---\n")
  unique_individuals <- unique(employment_data$cf)
  
  treatment_data <- data.table(
    cf = unique_individuals,
    # Define treatment types: none, counseling only, training only, both
    treatment_type = sample(
      c("none", "counseling", "training", "both"), 
      length(unique_individuals), 
      replace = TRUE, 
      prob = c(0.3, 0.25, 0.25, 0.2)
    )
  )
  
  # Create binary treatment indicator and intensity measure
  treatment_data[, `:=`(
    is_treated = as.numeric(treatment_type != "none"),
    treatment_intensity = fcase(
      treatment_type == "none", 0,
      treatment_type == "counseling", 1,
      treatment_type == "training", 2,
      treatment_type == "both", 3
    ),
    has_counseling = as.numeric(treatment_type %in% c("counseling", "both")),
    has_training = as.numeric(treatment_type %in% c("training", "both"))
  )]
  
  # Add baseline characteristics
  treatment_data[, `:=`(
    baseline_employability = runif(.N, 0, 1),
    risk_category = sample(c("low", "medium", "high"), .N, replace = TRUE),
    urban = sample(c(0, 1), .N, replace = TRUE)
  )]
  
  cat("Treatment group distribution:\n")
  print(table(treatment_data$treatment_type))
  
  # Prepare data for analysis
  cat("\n--- Prepare for Multiple Treatment Analysis ---\n")
  multi_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "did",
    verbose = TRUE
  )
  
  cat("Multiple treatment data prepared\n")
  cat("Available for sub-group analysis by:\n")
  cat("  - treatment_type (4 categories)\n")
  cat("  - treatment_intensity (0-3 scale)\n")
  cat("  - has_counseling, has_training (binary indicators)\n")
  
  # Example analysis: Compare different treatment types
  available_outcomes <- intersect(
    c("employment_rate", "permanent_contract_rate", "career_stability_score"),
    names(multi_data)
  )
  
  if (length(available_outcomes) > 0) {
    cat("\n--- Sample Analysis: Training vs Counseling Effects ---\n")
    
    # Analyze training effect (training/both vs none/counseling)
    training_data <- copy(multi_data)
    training_data[, has_training_binary := as.numeric(has_training == 1)]
    
    # This would be analyzed with difference_in_differences using has_training as treatment_var
    cat("Data ready for training effect analysis\n")
    cat("Training group:", sum(training_data$has_training_binary), "individuals\n")
    cat("No training group:", sum(1 - training_data$has_training_binary), "individuals\n")
  }
  
  return(list(
    data = multi_data,
    treatment_data = treatment_data,
    metrics = metrics_result,
    analysis_note = "Multiple comparisons available: training vs no training, counseling vs no counseling, combined vs single interventions"
  ))
}

# ===============================================================================
# EXAMPLE 4: PROPENSITY SCORE MATCHING INTEGRATION
# ===============================================================================

#' Propensity Score Matching with Employment Metrics
#' 
#' This example shows how to integrate employment metrics calculation with
#' propensity score matching for treatment effect estimation.

example_4_propensity_matching <- function() {
  
  cat("=== EXAMPLE 4: PROPENSITY SCORE MATCHING ===\n")
  
  # Create employment data with rich baseline characteristics
  employment_data <- create_sample_employment_data(
    n_individuals = 250,
    n_periods = 3,
    treatment_rate = 0.35,
    seed = 13579
  )
  
  employment_data[, event_period := sample(c("pre", "post"), .N, replace = TRUE)]
  
  # Calculate metrics (focus on key outcomes for matching)
  cat("\n--- Calculate Employment Metrics ---\n")
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = employment_data,
    metrics = c("stability", "quality"),
    period_column = "event_period",
    output_format = "wide"
  )
  
  # Create rich treatment assignment with selection bias
  cat("\n--- Create Treatment Assignment with Selection ---\n")
  set.seed(13579)
  unique_individuals <- unique(employment_data$cf)
  
  # Generate correlated baseline characteristics
  n_ind <- length(unique_individuals)
  baseline_matrix <- mvtnorm::rmvnorm(
    n = n_ind,
    mean = c(0.5, 0.4, 0.6),  # age, education, prior_employment
    sigma = matrix(c(1, 0.3, 0.4, 0.3, 1, 0.5, 0.4, 0.5, 1), 3, 3) * 0.1
  )
  
  treatment_data <- data.table(
    cf = unique_individuals,
    age_score = pmax(0, pmin(1, baseline_matrix[, 1])),
    education_score = pmax(0, pmin(1, baseline_matrix[, 2])),  
    prior_employment_score = pmax(0, pmin(1, baseline_matrix[, 3])),
    urban = sample(c(0, 1), n_ind, replace = TRUE),
    has_children = sample(c(0, 1), n_ind, replace = TRUE, prob = c(0.6, 0.4))
  )
  
  # Treatment assignment depends on characteristics (selection bias)
  treatment_data[, propensity_score := plogis(
    -0.5 + 1.2 * age_score + 0.8 * education_score + 
    0.6 * prior_employment_score + 0.3 * urban - 0.4 * has_children
  )]
  
  treatment_data[, is_treated := rbinom(.N, 1, propensity_score)]
  
  cat("Treatment selection summary:\n")
  cat("  Overall treatment rate:", round(100 * mean(treatment_data$is_treated), 1), "%\n")
  cat("  Mean propensity score (treated):", 
      round(mean(treatment_data[is_treated == 1]$propensity_score), 3), "\n")
  cat("  Mean propensity score (control):", 
      round(mean(treatment_data[is_treated == 0]$propensity_score), 3), "\n")
  
  # Prepare for matching
  cat("\n--- Prepare for Propensity Score Matching ---\n")
  matching_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "matching",
    verbose = TRUE
  )
  
  # Check covariate balance before matching
  cat("\n--- Covariate Balance Assessment ---\n")
  covariates <- c("age_score", "education_score", "prior_employment_score", "urban", "has_children")
  
  balance_check <- matching_data[, .(
    treated_mean = mean(get(covariates[1]), na.rm = TRUE),
    control_mean = mean(get(covariates[1]), na.rm = TRUE)
  ), by = is_treated]
  
  for (cov in covariates) {
    treated_mean <- matching_data[is_treated == 1, mean(get(cov), na.rm = TRUE)]
    control_mean <- matching_data[is_treated == 0, mean(get(cov), na.rm = TRUE)]
    standardized_diff <- (treated_mean - control_mean) / 
                        sqrt((var(matching_data[is_treated == 1, get(cov)], na.rm = TRUE) + 
                              var(matching_data[is_treated == 0, get(cov)], na.rm = TRUE)) / 2)
    
    cat(sprintf("  %s: Std diff = %.3f\n", cov, standardized_diff))
  }
  
  cat("\nData ready for propensity score matching analysis\n")
  cat("Available covariates for matching:", paste(covariates, collapse = ", "), "\n")
  
  # Note: Actual matching would be done with propensity_score_matching() function
  return(list(
    data = matching_data,
    treatment_data = treatment_data,
    metrics = metrics_result,
    covariates = covariates,
    note = "Ready for propensity_score_matching() with rich set of covariates"
  ))
}

# ===============================================================================
# EXAMPLE 5: COMPREHENSIVE POLICY EVALUATION PIPELINE
# ===============================================================================

#' Comprehensive Policy Evaluation Pipeline
#' 
#' This example demonstrates a complete policy evaluation using multiple
#' methods and comprehensive diagnostics.

example_5_comprehensive_evaluation <- function() {
  
  cat("=== EXAMPLE 5: COMPREHENSIVE POLICY EVALUATION ===\n")
  
  # Simulate realistic employment program evaluation
  cat("\n--- Simulate Employment Program Data ---\n")
  
  # Create larger, more realistic dataset
  employment_data <- create_sample_employment_data(
    n_individuals = 300,
    n_periods = 8,
    treatment_rate = 0.45,
    seed = 24680
  )
  
  # Create realistic pre/post periods around program implementation
  program_date <- as.Date("2020-04-01")
  employment_data[, `:=`(
    days_from_program = as.numeric(as.Date(inizio) - program_date),
    event_period = ifelse(as.Date(inizio) < program_date, "pre", "post")
  )]
  
  # Calculate comprehensive metrics
  cat("\n--- Calculate Comprehensive Employment Metrics ---\n")
  metrics_result <- calculate_comprehensive_impact_metrics(
    data = employment_data,
    metrics = "all",  # All available metrics
    period_column = "event_period",
    output_format = "wide"
  )
  
  cat("Comprehensive metrics calculated:", ncol(metrics_result) - 2, "outcome variables\n")
  
  # Realistic treatment assignment with program eligibility
  cat("\n--- Define Program Eligibility and Participation ---\n")
  unique_individuals <- unique(employment_data$cf)
  
  treatment_data <- data.table(
    cf = unique_individuals
  )
  
  # Add realistic baseline characteristics
  treatment_data[, `:=`(
    age = sample(18:65, .N, replace = TRUE),
    education_years = sample(8:20, .N, replace = TRUE),
    unemployment_duration = rexp(.N, rate = 1/180),  # Average 6 months
    has_work_experience = sample(c(0, 1), .N, replace = TRUE, prob = c(0.3, 0.7)),
    geographic_mobility = runif(.N, 0, 1),
    local_unemployment_rate = runif(.N, 0.05, 0.15)
  )]
  
  # Program eligibility based on risk factors
  treatment_data[, program_eligible := as.numeric(
    (age >= 25 & age <= 55) &  # Age criteria
    (education_years <= 14) &  # Education criteria
    (unemployment_duration >= 90) &  # Long-term unemployment
    (local_unemployment_rate >= 0.08)  # High unemployment area
  )]
  
  # Program participation among eligibles (with some non-compliance)
  treatment_data[, is_treated := ifelse(
    program_eligible == 1,
    rbinom(.N, 1, 0.7),  # 70% take-up rate among eligibles
    rbinom(.N, 1, 0.05)  # 5% participation among ineligibles
  )]
  
  cat("Program eligibility and participation:\n")
  cat("  Eligible individuals:", sum(treatment_data$program_eligible), 
      "(", round(100 * mean(treatment_data$program_eligible), 1), "%)\n")
  cat("  Actual participants:", sum(treatment_data$is_treated), 
      "(", round(100 * mean(treatment_data$is_treated), 1), "%)\n")
  cat("  Take-up rate among eligibles:", 
      round(100 * mean(treatment_data[program_eligible == 1]$is_treated), 1), "%\n")
  
  # Prepare for comprehensive analysis
  cat("\n--- Prepare for Multiple Impact Evaluation Methods ---\n")
  
  # Main DiD analysis
  did_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "did",
    verbose = FALSE
  )
  
  # Matching analysis (for robustness)
  matching_data <- prepare_metrics_for_impact_analysis(
    metrics_output = metrics_result,
    treatment_assignment = treatment_data,
    impact_method = "matching",
    verbose = FALSE
  )
  
  cat("Data prepared for multiple analysis methods\n")
  
  # Select primary outcomes for analysis
  primary_outcomes <- c(
    "employment_rate", "permanent_contract_rate", "avg_contract_quality",
    "employment_stability_index", "career_complexity_score"
  )
  
  available_primary <- intersect(primary_outcomes, names(did_data))
  
  if (length(available_primary) > 0) {
    cat("\n--- Primary Impact Analysis (DiD) ---\n")
    
    primary_did_results <- difference_in_differences(
      data = did_data,
      outcome_vars = available_primary,
      treatment_var = "is_treated",
      time_var = "post",
      id_var = "cf",
      control_vars = c("age", "education_years", "local_unemployment_rate"),
      fixed_effects = "both",
      parallel_trends_test = TRUE,
      verbose = FALSE
    )
    
    cat("Primary DiD analysis completed for", length(available_primary), "outcomes\n")
    
    # Display key results
    if (!is.null(primary_did_results$estimates)) {
      cat("\nKey Treatment Effects:\n")
      for (outcome in names(primary_did_results$estimates)) {
        est <- primary_did_results$estimates[[outcome]]
        if (!is.null(est$coefficient) && !is.null(est$p_value)) {
          significance <- if (est$p_value < 0.01) "***" else 
                         if (est$p_value < 0.05) "**" else 
                         if (est$p_value < 0.1) "*" else ""
          cat(sprintf("  %s: %.4f (%.4f) %s [p=%.3f]\n", 
                     outcome, est$coefficient, est$std_error, significance, est$p_value))
        }
      }
    }
  }
  
  # Summary of evaluation completeness
  cat("\n--- Evaluation Summary ---\n")
  cat("Dataset characteristics:\n")
  cat("  Total individuals:", length(unique_individuals), "\n")
  cat("  Total person-period observations:", nrow(did_data), "\n")
  cat("  Treatment group size:", sum(did_data$is_treated), "\n")
  cat("  Control group size:", sum(1 - did_data$is_treated), "\n")
  cat("  Available outcome variables:", length(available_primary), "\n")
  
  return(list(
    did_data = did_data,
    matching_data = matching_data,
    treatment_data = treatment_data,
    metrics = metrics_result,
    did_results = if(exists("primary_did_results")) primary_did_results else NULL,
    primary_outcomes = available_primary,
    evaluation_summary = "Comprehensive evaluation ready for reporting and robustness checks"
  ))
}

# ===============================================================================
# UTILITY FUNCTIONS FOR EXAMPLES
# ===============================================================================

#' Create Sample Employment Data for Examples
#' 
#' Creates realistic sample employment data for demonstration purposes.
create_sample_employment_data <- function(n_individuals = 100, 
                                        n_periods = 4, 
                                        treatment_rate = 0.4,
                                        seed = 123) {
  set.seed(seed)
  
  # Generate individual IDs
  individuals <- 1:n_individuals
  
  # Generate employment records
  employment_records <- list()
  
  for (i in individuals) {
    n_jobs <- sample(1:n_periods, 1, prob = c(0.4, 0.3, 0.2, 0.1))
    
    for (j in 1:n_jobs) {
      start_date <- as.Date("2020-01-01") + sample(0:730, 1)
      duration <- sample(30:365, 1)
      
      employment_records[[length(employment_records) + 1]] <- data.table(
        cf = i,
        inizio = start_date,
        fine = start_date + duration,
        durata = duration,
        over_id = length(employment_records) + 1,
        prior = sample(c(0, 1), 1, prob = c(0.3, 0.7)),
        COD_TIPOLOGIA_CONTRATTUALE = sample(
          c("A.03.00", "A.03.01", "C.01.00", "A.07.00"), 1,
          prob = c(0.4, 0.2, 0.3, 0.1)
        ),
        arco = sample(c(0, 1), 1, prob = c(0.8, 0.2))
      )
    }
  }
  
  employment_data <- rbindlist(employment_records)
  return(employment_data)
}

#' Run All Integration Examples
#' 
#' Executes all integration examples in sequence for complete demonstration.
run_all_integration_examples <- function() {
  
  cat("===============================================================================\n")
  cat("LONGWORKR: IMPACT-METRICS INTEGRATION - ALL EXAMPLES\n")
  cat("===============================================================================\n\n")
  
  examples_results <- list()
  
  # Example 1: Basic workforce development
  examples_results$workforce_development <- tryCatch({
    example_1_workforce_development()
  }, error = function(e) {
    cat("Example 1 error:", e$message, "\n")
    NULL
  })
  
  cat("\n\n")
  
  # Example 2: Event study
  examples_results$event_study <- tryCatch({
    example_2_event_study()
  }, error = function(e) {
    cat("Example 2 error:", e$message, "\n")
    NULL
  })
  
  cat("\n\n")
  
  # Example 3: Multiple treatments
  examples_results$multiple_treatments <- tryCatch({
    example_3_multiple_treatments()
  }, error = function(e) {
    cat("Example 3 error:", e$message, "\n")
    NULL
  })
  
  cat("\n\n")
  
  # Example 4: Propensity matching
  examples_results$propensity_matching <- tryCatch({
    example_4_propensity_matching()
  }, error = function(e) {
    cat("Example 4 error:", e$message, "\n")
    NULL
  })
  
  cat("\n\n")
  
  # Example 5: Comprehensive evaluation
  examples_results$comprehensive_evaluation <- tryCatch({
    example_5_comprehensive_evaluation()
  }, error = function(e) {
    cat("Example 5 error:", e$message, "\n")
    NULL
  })
  
  cat("\n===============================================================================\n")
  cat("ALL EXAMPLES COMPLETED\n")
  cat("===============================================================================\n")
  
  return(examples_results)
}

# ===============================================================================
# USAGE INSTRUCTIONS
# ===============================================================================

# To run individual examples:
# result1 <- example_1_workforce_development()
# result2 <- example_2_event_study()
# result3 <- example_3_multiple_treatments()
# result4 <- example_4_propensity_matching() 
# result5 <- example_5_comprehensive_evaluation()

# To run all examples:
# all_results <- run_all_integration_examples()

cat("Integration examples loaded successfully!\n")
cat("Use example_1_workforce_development() through example_5_comprehensive_evaluation()\n")
cat("Or run run_all_integration_examples() for complete demonstration.\n")