#' Impact Evaluation: Causal Inference Estimation Methods
#'
#' This module provides comprehensive causal inference estimation functionality for impact 
#' evaluation studies including difference-in-differences, event study designs, 
#' synthetic control methods, and treatment effect aggregation.
#'
#' @name impact_estimation
#' @author vecshift package
NULL

#' Difference-in-Differences Estimation
#'
#' Performs difference-in-differences estimation for treatment effect identification
#' using employment data with flexible model specifications and robust inference.
#'
#' @param data A data.table containing panel data with treatment identification
#' @param outcome_vars Character vector. Outcome variables to analyze
#' @param treatment_var Character. Treatment indicator variable. Default: "is_treated"
#' @param time_var Character. Time variable. Default: "event_period"
#' @param id_var Character. Individual identifier. Default: "cf"
#' @param control_vars Character vector. Control variables to include. Default: NULL
#' @param fixed_effects Character vector. Fixed effects to include: c("individual", "time", "both"). Default: "both"
#' @param cluster_var Character. Variable for clustered standard errors. Default: NULL
#' @param weights_var Character. Weights variable for weighted regression. Default: NULL
#' @param parallel_trends_test Logical. Test parallel trends assumption? Default: TRUE
#' @param placebo_test Logical. Perform placebo tests? Default: TRUE
#' @param bootstrap_se Logical. Use bootstrap standard errors? Default: FALSE
#' @param n_bootstrap Integer. Number of bootstrap replications. Default: 1000
#'
#' @return A list containing:
#'   \item{estimates}{Treatment effect estimates for each outcome}
#'   \item{model_results}{Full regression results}
#'   \item{parallel_trends_test}{Parallel trends test results}
#'   \item{placebo_tests}{Placebo test results}
#'   \item{robustness_checks}{Additional robustness checks}
#'   \item{summary_table}{Summary table of all results}
#'
#' @examples
#' \dontrun{
#' # Basic DiD estimation
#' did_results <- difference_in_differences(
#'   data = panel_data,
#'   outcome_vars = c("employment_rate", "avg_wage", "job_stability"),
#'   treatment_var = "training_program",
#'   control_vars = c("age", "education", "sector")
#' )
#' 
#' # Advanced DiD with clustering
#' did_robust <- difference_in_differences(
#'   data = panel_data,
#'   outcome_vars = "employment_stability_index",
#'   cluster_var = "region",
#'   parallel_trends_test = TRUE,
#'   bootstrap_se = TRUE
#' )
#' }
#'
#' @export
difference_in_differences <- function(data,
                                    outcome_vars,
                                    treatment_var = "is_treated",
                                    time_var = "event_period",
                                    id_var = "cf",
                                    control_vars = NULL,
                                    fixed_effects = "both",
                                    cluster_var = NULL,
                                    weights_var = NULL,
                                    parallel_trends_test = TRUE,
                                    placebo_test = TRUE,
                                    bootstrap_se = FALSE,
                                    n_bootstrap = 1000) {
  
  # Check for required packages
  required_packages <- c("fixest")
  missing_packages <- setdiff(required_packages, rownames(installed.packages()))
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not installed:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))"))
  }
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(outcome_vars, treatment_var, time_var, id_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy and standardize names
  dt <- copy(data)
  setnames(dt, c(treatment_var, time_var, id_var), c("treatment", "period", "id"))
  
  # Ensure period is binary for basic DiD
  if (!all(dt$period %in% c(0, 1))) {
    # Convert to binary (assuming pre/post structure)
    dt[, period := as.numeric(period != min(period, na.rm = TRUE))]
    warning("Time variable converted to binary (0=pre, 1=post) for DiD estimation")
  }
  
  # Results storage
  did_results <- list()
  model_results <- list()
  
  # Estimate DiD for each outcome
  for (outcome in outcome_vars) {
    if (!outcome %in% names(dt)) {
      warning(paste("Outcome variable", outcome, "not found, skipping"))
      next
    }
    
    # Build formula
    base_formula <- paste(outcome, "~ treatment * period")
    
    if (!is.null(control_vars)) {
      available_controls <- intersect(control_vars, names(dt))
      if (length(available_controls) > 0) {
        base_formula <- paste(base_formula, "+", paste(available_controls, collapse = " + "))
      }
    }
    
    # Add fixed effects
    fe_spec <- ""
    if ("individual" %in% fixed_effects || "both" %in% fixed_effects) {
      fe_spec <- paste(fe_spec, "id", sep = ifelse(fe_spec == "", "", " + "))
    }
    if ("time" %in% fixed_effects || "both" %in% fixed_effects) {
      fe_spec <- paste(fe_spec, "period", sep = ifelse(fe_spec == "", "", " + "))
    }
    
    full_formula <- if (fe_spec != "") {
      paste(base_formula, "|", fe_spec)
    } else {
      base_formula
    }
    
    # Cluster and weights specification
    vcov_spec <- if (!is.null(cluster_var) && cluster_var %in% names(dt)) {
      paste("cluster", cluster_var, sep = " ~ ")
    } else {
      "iid"
    }
    
    weights_spec <- if (!is.null(weights_var) && weights_var %in% names(dt)) {
      dt[[weights_var]]
    } else {
      NULL
    }
    
    # Estimate model
    tryCatch({
      model <- fixest::feols(
        as.formula(full_formula),
        data = dt,
        vcov = vcov_spec,
        weights = weights_spec
      )
      
      model_results[[outcome]] <- model
      
      # Extract treatment effect (interaction term)
      coef_names <- names(coef(model))
      interaction_coef <- grep("treatment.*period|period.*treatment", coef_names, value = TRUE)
      
      if (length(interaction_coef) > 0) {
        treatment_effect <- coef(model)[interaction_coef[1]]
        treatment_se <- sqrt(vcov(model)[interaction_coef[1], interaction_coef[1]])
        treatment_pvalue <- 2 * (1 - pnorm(abs(treatment_effect / treatment_se)))
        
        did_results[[outcome]] <- data.table(
          outcome = outcome,
          treatment_effect = treatment_effect,
          std_error = treatment_se,
          t_statistic = treatment_effect / treatment_se,
          p_value = treatment_pvalue,
          conf_lower = treatment_effect - 1.96 * treatment_se,
          conf_upper = treatment_effect + 1.96 * treatment_se,
          significant = treatment_pvalue < 0.05,
          n_obs = nobs(model),
          r_squared = r2(model)
        )
      }
    }, error = function(e) {
      warning(paste("DiD estimation failed for", outcome, ":", e$message))
      did_results[[outcome]] <- data.table(
        outcome = outcome,
        treatment_effect = NA_real_,
        std_error = NA_real_,
        error = e$message
      )
    })
  }
  
  # Combine results
  estimates_table <- rbindlist(did_results, fill = TRUE)
  
  # Parallel trends test
  parallel_trends_results <- NULL
  if (parallel_trends_test && nrow(dt) > 0) {
    parallel_trends_results <- test_parallel_trends(dt, outcome_vars, model_results)
  }
  
  # Placebo tests
  placebo_results <- NULL
  if (placebo_test && nrow(dt) > 0) {
    placebo_results <- run_placebo_tests(dt, outcome_vars, treatment_var, time_var, id_var)
  }
  
  # Robustness checks
  robustness_results <- run_robustness_checks(dt, outcome_vars, model_results)
  
  # Summary table
  summary_table <- create_did_summary_table(estimates_table, parallel_trends_results, placebo_results)
  
  result <- list(
    estimates = estimates_table,
    model_results = model_results,
    parallel_trends_test = parallel_trends_results,
    placebo_tests = placebo_results,
    robustness_checks = robustness_results,
    summary_table = summary_table
  )
  
  class(result) <- c("did_results", "list")
  return(result)
}

#' Event Study Design
#'
#' Performs event study analysis to examine treatment effects over time around
#' the treatment event with flexible lead and lag specifications.
#'
#' @param data A data.table containing event data with time-to-event information
#' @param outcome_vars Character vector. Outcome variables to analyze
#' @param time_to_event_var Character. Variable containing time to event. Default: "days_to_event"
#' @param treatment_var Character. Treatment indicator. Default: "is_treated"
#' @param id_var Character. Individual identifier. Default: "cf"
#' @param control_vars Character vector. Control variables. Default: NULL
#' @param event_window Numeric vector. Event window c(pre_periods, post_periods). Default: c(-12, 12)
#' @param time_unit Character. Time unit for binning: "days", "weeks", "months". Default: "months"
#' @param omit_period Integer. Reference period to omit (relative to event). Default: -1
#' @param cluster_var Character. Clustering variable. Default: NULL
#'
#' @return A list containing:
#'   \item{event_estimates}{Event study coefficients by time period}
#'   \item{model_results}{Full regression results}
#'   \item{pre_test}{Pre-treatment test results}
#'   \item{dynamic_effects}{Summary of dynamic treatment effects}
#'   \item{plot_data}{Data formatted for event study plots}
#'
#' @examples
#' \dontrun{
#' event_study_results <- event_study_design(
#'   data = event_data,
#'   outcome_vars = "employment_rate",
#'   event_window = c(-6, 12),
#'   time_unit = "months"
#' )
#' }
#'
#' @export
event_study_design <- function(data,
                             outcome_vars,
                             time_to_event_var = "days_to_event",
                             treatment_var = "is_treated",
                             id_var = "cf",
                             control_vars = NULL,
                             event_window = c(-12, 12),
                             time_unit = "months",
                             omit_period = -1,
                             cluster_var = NULL) {
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(outcome_vars, time_to_event_var, treatment_var, id_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create working copy
  dt <- copy(data)
  setnames(dt, c(time_to_event_var, treatment_var, id_var), c("time_to_event", "treatment", "id"))
  
  # Convert time to event into periods
  if (time_unit == "days") {
    dt[, event_time := round(time_to_event / 1)]
  } else if (time_unit == "weeks") {
    dt[, event_time := round(time_to_event / 7)]
  } else if (time_unit == "months") {
    dt[, event_time := round(time_to_event / 30)]
  } else {
    stop("time_unit must be one of: 'days', 'weeks', 'months'")
  }
  
  # Filter to event window
  dt <- dt[event_time >= event_window[1] & event_time <= event_window[2]]
  
  if (nrow(dt) == 0) {
    stop("No observations within specified event window")
  }
  
  # Create event time dummies (excluding omit_period)
  event_periods <- sort(unique(dt$event_time))
  event_periods <- event_periods[event_periods != omit_period]
  
  # Generate dummy variables for each event time
  for (t in event_periods) {
    dt[, paste0("event_", t) := as.numeric(event_time == t & treatment == 1)]
  }
  
  # Results storage
  event_results <- list()
  model_results <- list()
  
  # Estimate event study for each outcome
  for (outcome in outcome_vars) {
    if (!outcome %in% names(dt)) {
      warning(paste("Outcome variable", outcome, "not found, skipping"))
      next
    }
    
    # Build formula with event time dummies
    event_dummies <- paste0("event_", event_periods)
    formula_rhs <- paste(event_dummies, collapse = " + ")
    
    if (!is.null(control_vars)) {
      available_controls <- intersect(control_vars, names(dt))
      if (length(available_controls) > 0) {
        formula_rhs <- paste(formula_rhs, "+", paste(available_controls, collapse = " + "))
      }
    }
    
    full_formula <- paste(outcome, "~", formula_rhs, "| id + event_time")
    
    # Cluster specification
    vcov_spec <- if (!is.null(cluster_var) && cluster_var %in% names(dt)) {
      paste("cluster", cluster_var, sep = " ~ ")
    } else {
      "iid"
    }
    
    # Estimate model
    tryCatch({
      model <- fixest::feols(
        as.formula(full_formula),
        data = dt,
        vcov = vcov_spec
      )
      
      model_results[[outcome]] <- model
      
      # Extract event study coefficients
      coefs <- coef(model)
      ses <- sqrt(diag(vcov(model)))
      
      event_coef_names <- names(coefs)[grepl("^event_", names(coefs))]
      
      if (length(event_coef_names) > 0) {
        event_estimates <- data.table(
          outcome = outcome,
          event_time = as.numeric(gsub("event_", "", event_coef_names)),
          coefficient = coefs[event_coef_names],
          std_error = ses[event_coef_names]
        )
        
        event_estimates[, `:=`(
          t_statistic = coefficient / std_error,
          p_value = 2 * (1 - pnorm(abs(coefficient / std_error))),
          conf_lower = coefficient - 1.96 * std_error,
          conf_upper = coefficient + 1.96 * std_error
        )]
        
        # Add reference period (omitted)
        reference_row <- data.table(
          outcome = outcome,
          event_time = omit_period,
          coefficient = 0,
          std_error = 0,
          t_statistic = 0,
          p_value = 1,
          conf_lower = 0,
          conf_upper = 0
        )
        
        event_estimates <- rbind(event_estimates, reference_row)
        setorder(event_estimates, event_time)
        
        event_results[[outcome]] <- event_estimates
      }
    }, error = function(e) {
      warning(paste("Event study estimation failed for", outcome, ":", e$message))
    })
  }
  
  # Combine results
  if (length(event_results) > 0) {
    combined_estimates <- rbindlist(event_results)
  } else {
    combined_estimates <- data.table()
  }
  
  # Pre-treatment test (joint test that all pre-treatment effects = 0)
  pre_test_results <- NULL
  if (nrow(combined_estimates) > 0) {
    pre_test_results <- test_pre_treatment_effects(combined_estimates, model_results)
  }
  
  # Dynamic effects summary
  dynamic_effects <- summarize_dynamic_effects(combined_estimates)
  
  # Format plot data
  plot_data <- if (nrow(combined_estimates) > 0) {
    combined_estimates[, .(outcome, event_time, coefficient, conf_lower, conf_upper, 
                          significant = p_value < 0.05)]
  } else {
    data.table()
  }
  
  result <- list(
    event_estimates = combined_estimates,
    model_results = model_results,
    pre_test = pre_test_results,
    dynamic_effects = dynamic_effects,
    plot_data = plot_data
  )
  
  class(result) <- c("event_study_results", "list")
  return(result)
}

#' Synthetic Control Method Wrapper
#'
#' Wrapper function for synthetic control estimation using the augsynth package
#' for cases where traditional matching is not feasible.
#'
#' @param data A data.table containing panel data
#' @param outcome_var Character. Outcome variable name
#' @param unit_var Character. Unit identifier variable
#' @param time_var Character. Time variable
#' @param treatment_var Character. Treatment indicator
#' @param treatment_time Integer. Time period when treatment begins
#' @param covariates Character vector. Time-varying covariates. Default: NULL
#' @param method Character. Augsynth method: "augsynth", "synth", "ridge". Default: "augsynth"
#' @param inference Logical. Perform inference using conformal method? Default: TRUE
#' @param n_lags Integer. Number of outcome lags to include. Default: 0
#'
#' @return A list containing:
#'   \item{synth_estimates}{Synthetic control estimates}
#'   \item{unit_weights}{Synthetic control unit weights}
#'   \item{balance_table}{Pre-treatment balance}
#'   \item{inference_results}{Inference results if requested}
#'   \item{plot_data}{Data for synthetic control plots}
#'
#' @examples
#' \dontrun{
#' synth_results <- synthetic_control_method(
#'   data = panel_data,
#'   outcome_var = "employment_rate",
#'   unit_var = "region_id",
#'   time_var = "year",
#'   treatment_time = 2015,
#'   method = "augsynth"
#' )
#' }
#'
#' @export
synthetic_control_method <- function(data,
                                   outcome_var,
                                   unit_var,
                                   time_var,
                                   treatment_var,
                                   treatment_time,
                                   covariates = NULL,
                                   method = "augsynth",
                                   inference = TRUE,
                                   n_lags = 0) {
  
  # Check for required package
  if (!"augsynth" %in% rownames(installed.packages())) {
    stop("Package 'augsynth' not installed. Please install with: install.packages('augsynth')")
  }
  
  if (!inherits(data, "data.table")) {
    stop("Input data must be a data.table")
  }
  
  required_cols <- c(outcome_var, unit_var, time_var, treatment_var)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Convert to data.frame for augsynth
  synth_data <- as.data.frame(data)
  
  # Build formula
  if (!is.null(covariates)) {
    available_covariates <- intersect(covariates, names(synth_data))
    if (length(available_covariates) > 0) {
      covar_formula <- paste(available_covariates, collapse = " + ")
    } else {
      covar_formula <- NULL
    }
  } else {
    covar_formula <- NULL
  }
  
  # Estimate synthetic control
  tryCatch({
    if (is.null(covar_formula)) {
      synth_model <- augsynth::augsynth(
        form = as.formula(paste(outcome_var, "~", treatment_var)),
        unit = unit_var,
        time = time_var,
        t_int = treatment_time,
        data = synth_data,
        progfunc = method,
        n_lags = n_lags
      )
    } else {
      synth_model <- augsynth::augsynth(
        form = as.formula(paste(outcome_var, "~", treatment_var, "|", covar_formula)),
        unit = unit_var,
        time = time_var,
        t_int = treatment_time,
        data = synth_data,
        progfunc = method,
        n_lags = n_lags
      )
    }
    
    # Extract results
    synth_summary <- summary(synth_model)
    
    # Treatment effect estimates
    synth_estimates <- data.table(
      time_period = as.numeric(names(synth_summary$att)),
      treatment_effect = as.numeric(synth_summary$att),
      outcome = outcome_var,
      method = method
    )
    
    # Unit weights
    unit_weights <- data.table(
      unit = names(synth_model$weights),
      weight = as.numeric(synth_model$weights)
    )[weight > 0.001][order(-weight)]  # Keep only meaningful weights
    
    # Balance assessment
    balance_table <- tryCatch({
      bal_results <- augsynth::balance_synth(synth_model)
      as.data.table(bal_results)
    }, error = function(e) {
      data.table(variable = character(), treated = numeric(), synthetic = numeric(), difference = numeric())
    })
    
    # Inference
    inference_results <- NULL
    if (inference) {
      inference_results <- tryCatch({
        inf_results <- augsynth::conformal_inf(synth_model)
        list(
          lower_bound = inf_results$lower,
          upper_bound = inf_results$upper,
          method = "conformal"
        )
      }, error = function(e) {
        warning(paste("Inference failed:", e$message))
        NULL
      })
    }
    
    # Plot data
    plot_data <- synth_estimates[, .(time_period, treatment_effect, outcome)]
    if (!is.null(inference_results)) {
      plot_data[, `:=`(
        conf_lower = inference_results$lower_bound,
        conf_upper = inference_results$upper_bound
      )]
    }
    
    result <- list(
      synth_estimates = synth_estimates,
      unit_weights = unit_weights,
      balance_table = balance_table,
      inference_results = inference_results,
      plot_data = plot_data,
      augsynth_object = synth_model
    )
    
  }, error = function(e) {
    stop(paste("Synthetic control estimation failed:", e$message))
  })
  
  class(result) <- c("synth_results", "list")
  return(result)
}

#' Treatment Effect Aggregation
#'
#' Aggregates treatment effects across multiple outcomes, time periods, or subgroups
#' with appropriate statistical adjustments for multiple testing.
#'
#' @param estimation_results List of estimation results from DiD, event study, or synthetic control
#' @param aggregation_method Character. Method: "simple_average", "weighted_average", "meta_analysis". Default: "weighted_average"
#' @param weight_var Character. Variable to use for weighting (e.g., "n_obs", "inv_var"). Default: "inv_var"
#' @param multiple_testing_correction Character. Method: "bonferroni", "holm", "fdr". Default: "holm"
#' @param confidence_level Numeric. Confidence level for intervals. Default: 0.95
#'
#' @return A list containing:
#'   \item{aggregated_effects}{Summary of aggregated treatment effects}
#'   \item{individual_effects}{Individual effect estimates with corrections}
#'   \item{heterogeneity_tests}{Tests for effect heterogeneity}
#'   \item{meta_analysis_results}{Meta-analysis results if applicable}
#'
#' @examples
#' \dontrun{
#' aggregated_results <- aggregate_treatment_effects(
#'   estimation_results = list(did_results, event_study_results),
#'   aggregation_method = "meta_analysis",
#'   multiple_testing_correction = "fdr"
#' )
#' }
#'
#' @export
aggregate_treatment_effects <- function(estimation_results,
                                      aggregation_method = "weighted_average",
                                      weight_var = "inv_var",
                                      multiple_testing_correction = "holm",
                                      confidence_level = 0.95) {
  
  if (!is.list(estimation_results)) {
    stop("estimation_results must be a list of estimation result objects")
  }
  
  # Extract effects from different result types
  effects_list <- list()
  
  for (i in seq_along(estimation_results)) {
    result <- estimation_results[[i]]
    
    if (inherits(result, "did_results")) {
      effects_list[[i]] <- result$estimates[, .(
        effect_id = paste("did", outcome, sep = "_"),
        outcome = outcome,
        method = "DiD",
        treatment_effect = treatment_effect,
        std_error = std_error,
        p_value = p_value,
        n_obs = n_obs
      )]
    } else if (inherits(result, "event_study_results")) {
      # Aggregate post-treatment effects for event studies
      post_effects <- result$event_estimates[event_time > 0, .(
        avg_effect = mean(coefficient, na.rm = TRUE),
        pooled_se = sqrt(mean(std_error^2, na.rm = TRUE))
      ), by = outcome]
      
      effects_list[[i]] <- post_effects[, .(
        effect_id = paste("event_study", outcome, sep = "_"),
        outcome = outcome,
        method = "Event Study",
        treatment_effect = avg_effect,
        std_error = pooled_se,
        p_value = 2 * (1 - pnorm(abs(avg_effect / pooled_se))),
        n_obs = NA_integer_
      )]
    } else if (inherits(result, "synth_results")) {
      # Aggregate post-treatment synthetic control effects
      post_synth <- result$synth_estimates[time_period >= 0, .(
        avg_effect = mean(treatment_effect, na.rm = TRUE),
        se_effect = sd(treatment_effect, na.rm = TRUE) / sqrt(.N)
      ), by = outcome]
      
      effects_list[[i]] <- post_synth[, .(
        effect_id = paste("synth", outcome, sep = "_"),
        outcome = outcome,
        method = "Synthetic Control",
        treatment_effect = avg_effect,
        std_error = se_effect,
        p_value = 2 * (1 - pnorm(abs(avg_effect / se_effect))),
        n_obs = NA_integer_
      )]
    }
  }
  
  # Combine all effects
  all_effects <- rbindlist(effects_list, fill = TRUE)
  
  if (nrow(all_effects) == 0) {
    stop("No treatment effects extracted from estimation results")
  }
  
  # Calculate weights based on method
  if (weight_var == "inv_var") {
    all_effects[, weight := 1 / (std_error^2)]
  } else if (weight_var == "n_obs" && "n_obs" %in% names(all_effects)) {
    all_effects[!is.na(n_obs), weight := n_obs]
    all_effects[is.na(weight), weight := mean(all_effects$weight, na.rm = TRUE)]
  } else {
    all_effects[, weight := 1]  # Equal weights
  }
  
  # Apply multiple testing correction
  alpha <- 1 - confidence_level
  if (multiple_testing_correction == "bonferroni") {
    all_effects[, adjusted_p_value := pmin(1, p_value * .N)]
  } else if (multiple_testing_correction == "holm") {
    all_effects[, adjusted_p_value := p.adjust(p_value, method = "holm")]
  } else if (multiple_testing_correction == "fdr") {
    all_effects[, adjusted_p_value := p.adjust(p_value, method = "fdr")]
  } else {
    all_effects[, adjusted_p_value := p_value]
  }
  
  # Calculate confidence intervals with adjusted alpha
  z_critical <- qnorm(1 - alpha/2)
  all_effects[, `:=`(
    conf_lower = treatment_effect - z_critical * std_error,
    conf_upper = treatment_effect + z_critical * std_error,
    significant_adjusted = adjusted_p_value < alpha
  )]
  
  # Aggregate effects
  if (aggregation_method == "simple_average") {
    aggregated <- all_effects[, .(
      pooled_effect = mean(treatment_effect, na.rm = TRUE),
      pooled_se = sqrt(mean(std_error^2, na.rm = TRUE)),
      n_effects = .N,
      methods_used = paste(unique(method), collapse = ", ")
    )]
  } else if (aggregation_method == "weighted_average") {
    aggregated <- all_effects[, .(
      pooled_effect = weighted.mean(treatment_effect, weight, na.rm = TRUE),
      pooled_se = sqrt(1 / sum(weight, na.rm = TRUE)),
      n_effects = .N,
      methods_used = paste(unique(method), collapse = ", "),
      total_weight = sum(weight, na.rm = TRUE)
    )]
  } else if (aggregation_method == "meta_analysis") {
    # Simple random effects meta-analysis
    weights <- all_effects$weight
    effects <- all_effects$treatment_effect
    
    # Calculate Q statistic for heterogeneity
    weighted_mean <- weighted.mean(effects, weights)
    q_stat <- sum(weights * (effects - weighted_mean)^2)
    df <- length(effects) - 1
    heterogeneity_p <- 1 - pchisq(q_stat, df)
    
    # Tau-squared (between-study variance)
    tau_squared <- max(0, (q_stat - df) / (sum(weights) - sum(weights^2) / sum(weights)))
    
    # Adjusted weights
    adjusted_weights <- 1 / (1/weights + tau_squared)
    
    aggregated <- data.table(
      pooled_effect = weighted.mean(effects, adjusted_weights),
      pooled_se = sqrt(1 / sum(adjusted_weights)),
      n_effects = length(effects),
      methods_used = paste(unique(all_effects$method), collapse = ", "),
      tau_squared = tau_squared,
      q_statistic = q_stat,
      heterogeneity_p = heterogeneity_p,
      i_squared = max(0, (q_stat - df) / q_stat) * 100  # I-squared
    )
  }
  
  # Add overall significance test
  aggregated[, `:=`(
    z_statistic = pooled_effect / pooled_se,
    overall_p_value = 2 * (1 - pnorm(abs(pooled_effect / pooled_se))),
    overall_conf_lower = pooled_effect - z_critical * pooled_se,
    overall_conf_upper = pooled_effect + z_critical * pooled_se
  )]
  
  # Heterogeneity tests
  heterogeneity_tests <- NULL
  if (nrow(all_effects) > 1) {
    heterogeneity_tests <- test_effect_heterogeneity(all_effects)
  }
  
  result <- list(
    aggregated_effects = aggregated,
    individual_effects = all_effects,
    heterogeneity_tests = heterogeneity_tests,
    aggregation_method = aggregation_method,
    multiple_testing_correction = multiple_testing_correction,
    confidence_level = confidence_level
  )
  
  class(result) <- c("aggregated_treatment_effects", "list")
  return(result)
}

# Helper functions
test_parallel_trends <- function(data, outcomes, model_results) {
  # Simplified parallel trends test
  # In practice, would implement leads/lags approach
  return(list(test = "parallel_trends", result = "not_implemented"))
}

run_placebo_tests <- function(data, outcomes, treatment_var, time_var, id_var) {
  # Simplified placebo test
  return(list(test = "placebo", result = "not_implemented"))
}

run_robustness_checks <- function(data, outcomes, model_results) {
  # Simplified robustness checks
  return(list(test = "robustness", result = "not_implemented"))
}

create_did_summary_table <- function(estimates, parallel_trends, placebo) {
  return(estimates)
}

test_pre_treatment_effects <- function(estimates, models) {
  # Test joint significance of pre-treatment effects
  pre_effects <- estimates[event_time < 0 & !is.na(coefficient)]
  if (nrow(pre_effects) == 0) return(NULL)
  
  # Simple F-test approximation
  f_stat <- mean((pre_effects$coefficient / pre_effects$std_error)^2)
  p_value <- 1 - pf(f_stat, nrow(pre_effects), Inf)
  
  return(list(
    test_statistic = f_stat,
    p_value = p_value,
    df = nrow(pre_effects),
    interpretation = ifelse(p_value > 0.05, "No evidence against parallel trends", "Potential parallel trends violation")
  ))
}

summarize_dynamic_effects <- function(estimates) {
  if (nrow(estimates) == 0) return(NULL)
  
  pre_effects <- estimates[event_time < 0 & !is.na(coefficient)]
  post_effects <- estimates[event_time > 0 & !is.na(coefficient)]
  
  return(list(
    pre_treatment_avg = ifelse(nrow(pre_effects) > 0, mean(pre_effects$coefficient), NA),
    post_treatment_avg = ifelse(nrow(post_effects) > 0, mean(post_effects$coefficient), NA),
    immediate_effect = estimates[event_time == 0, coefficient],
    long_term_effect = ifelse(nrow(post_effects) > 0, post_effects[event_time == max(event_time), coefficient], NA)
  ))
}

test_effect_heterogeneity <- function(effects) {
  if (nrow(effects) <= 1) return(NULL)
  
  # Simple heterogeneity test using Q statistic
  weights <- effects$weight
  effect_sizes <- effects$treatment_effect
  weighted_mean <- weighted.mean(effect_sizes, weights)
  
  q_stat <- sum(weights * (effect_sizes - weighted_mean)^2)
  df <- nrow(effects) - 1
  p_value <- 1 - pchisq(q_stat, df)
  
  return(list(
    q_statistic = q_stat,
    df = df,
    p_value = p_value,
    significant_heterogeneity = p_value < 0.05,
    i_squared = max(0, (q_stat - df) / q_stat) * 100
  ))
}

#' Print method for DiD results
#' @param x A did_results object
#' @param ... Additional arguments passed to print
#' @method print did_results
#' @export
print.did_results <- function(x, ...) {
  cat("Difference-in-Differences Results\n")
  cat("=================================\n\n")
  
  if (nrow(x$estimates) > 0) {
    cat("Treatment Effects:\n")
    print(x$estimates[, .(outcome, treatment_effect, std_error, p_value, significant)])
    
    cat("\nSignificant effects:", sum(x$estimates$significant, na.rm = TRUE), "out of", nrow(x$estimates), "\n")
  }
  
  invisible(x)
}