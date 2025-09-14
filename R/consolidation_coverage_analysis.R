#' Consolidation Coverage Analysis Functions
#'
#' @description
#' Functions to analyze and summarize the coverage metrics from the enhanced
#' consolidation process that removes intermediate unemployment periods.
#'
#' @name consolidation-coverage-analysis
#' @importFrom data.table data.table setorder copy
#' @importFrom stats median quantile sd
#' @importFrom utils head tail
NULL

#' Analyze Consolidation Coverage
#'
#' @description
#' Provides comprehensive analysis of consolidation coverage metrics from
#' consolidated employment data, including coverage distribution, unemployment
#' impact, and consolidation effectiveness statistics.
#'
#' @param consolidated_data data.table output from consolidate_employment() with
#'   coverage metrics (consolidation_coverage, unemployment_days_removed, etc.)
#' @param original_data Optional original data.table for comparison metrics
#' @param coverage_thresholds Numeric vector of coverage thresholds for classification
#'   (default: c(0.5, 0.75, 0.9, 0.95))
#' @param show_details Logical. If TRUE, includes detailed person-level statistics
#'
#' @return List containing:
#'   \itemize{
#'     \item{\code{overall_summary}}: Overall consolidation and coverage statistics
#'     \item{\code{coverage_distribution}}: Distribution of coverage ratios
#'     \item{\code{unemployment_impact}}: Analysis of unemployment removal impact
#'     \item{\code{consolidation_effectiveness}}: Consolidation reduction metrics
#'     \item{\code{coverage_categories}}: Classification by coverage thresholds
#'     \item{\code{person_details}}: Person-level details (if show_details = TRUE)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic coverage analysis
#' analysis <- analyze_consolidation_coverage(consolidated_data)
#' print(analysis$overall_summary)
#' 
#' # With original data comparison
#' analysis <- analyze_consolidation_coverage(
#'   consolidated_data,
#'   original_data = original_data
#' )
#' 
#' # Custom coverage thresholds
#' analysis <- analyze_consolidation_coverage(
#'   consolidated_data,
#'   coverage_thresholds = c(0.6, 0.8, 0.95),
#'   show_details = TRUE
#' )
#' }
analyze_consolidation_coverage <- function(consolidated_data,
                                         original_data = NULL,
                                         coverage_thresholds = c(0.5, 0.75, 0.9, 0.95),
                                         show_details = FALSE) {
  
  # Input validation
  if (!inherits(consolidated_data, "data.table")) {
    stop("consolidated_data must be a data.table object")
  }
  
  required_cols <- c("consolidation_coverage", "unemployment_days_removed", 
                     "n_periods_consolidated", "n_unemployment_periods_removed")
  missing_cols <- setdiff(required_cols, names(consolidated_data))
  if (length(missing_cols) > 0) {
    stop("Missing required coverage columns: ", paste(missing_cols, collapse = ", "))
  }
  
  dt <- copy(consolidated_data)
  
  # Filter to records with consolidation metrics (exclude non-consolidated periods)
  dt_with_coverage <- dt[!is.na(consolidation_coverage)]
  
  if (nrow(dt_with_coverage) == 0) {
    stop("No records with consolidation coverage metrics found")
  }
  
  # Overall Summary
  overall_summary <- list(
    total_periods = nrow(dt),
    consolidated_periods = nrow(dt_with_coverage),
    consolidation_rate = round(nrow(dt_with_coverage) / nrow(dt), 4),
    
    # Coverage statistics
    mean_coverage = round(mean(dt_with_coverage$consolidation_coverage, na.rm = TRUE), 4),
    median_coverage = round(median(dt_with_coverage$consolidation_coverage, na.rm = TRUE), 4),
    min_coverage = round(min(dt_with_coverage$consolidation_coverage, na.rm = TRUE), 4),
    max_coverage = round(max(dt_with_coverage$consolidation_coverage, na.rm = TRUE), 4),
    coverage_sd = round(sd(dt_with_coverage$consolidation_coverage, na.rm = TRUE), 4),
    
    # Unemployment removal statistics
    total_unemployment_removed = sum(dt_with_coverage$unemployment_days_removed, na.rm = TRUE),
    mean_unemployment_removed = round(mean(dt_with_coverage$unemployment_days_removed, na.rm = TRUE), 2),
    periods_with_unemployment_removed = sum(dt_with_coverage$unemployment_days_removed > 0, na.rm = TRUE),
    
    # Consolidation statistics
    total_periods_consolidated = sum(dt_with_coverage$n_periods_consolidated, na.rm = TRUE),
    mean_periods_per_consolidation = round(mean(dt_with_coverage$n_periods_consolidated, na.rm = TRUE), 2),
    total_unemployment_periods_removed = sum(dt_with_coverage$n_unemployment_periods_removed, na.rm = TRUE)
  )
  
  # Coverage Distribution
  coverage_quantiles <- quantile(dt_with_coverage$consolidation_coverage, 
                                probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1.0), 
                                na.rm = TRUE)
  
  coverage_distribution <- list(
    quantiles = round(coverage_quantiles, 4),
    perfect_coverage = sum(dt_with_coverage$consolidation_coverage >= 1.0, na.rm = TRUE),
    high_coverage = sum(dt_with_coverage$consolidation_coverage >= 0.9, na.rm = TRUE),
    good_coverage = sum(dt_with_coverage$consolidation_coverage >= 0.75, na.rm = TRUE),
    poor_coverage = sum(dt_with_coverage$consolidation_coverage < 0.5, na.rm = TRUE)
  )
  
  # Unemployment Impact Analysis
  unemployment_impact <- list(
    periods_with_no_unemployment = sum(dt_with_coverage$unemployment_days_removed == 0, na.rm = TRUE),
    periods_with_minor_unemployment = sum(dt_with_coverage$unemployment_days_removed > 0 & 
                                         dt_with_coverage$unemployment_days_removed <= 7, na.rm = TRUE),
    periods_with_moderate_unemployment = sum(dt_with_coverage$unemployment_days_removed > 7 & 
                                           dt_with_coverage$unemployment_days_removed <= 30, na.rm = TRUE),
    periods_with_major_unemployment = sum(dt_with_coverage$unemployment_days_removed > 30, na.rm = TRUE),
    
    max_unemployment_removed = max(dt_with_coverage$unemployment_days_removed, na.rm = TRUE),
    median_unemployment_when_present = median(dt_with_coverage$unemployment_days_removed[
      dt_with_coverage$unemployment_days_removed > 0], na.rm = TRUE)
  )
  
  # Consolidation Effectiveness
  consolidation_effectiveness <- list(
    periods_with_multiple_consolidation = sum(dt_with_coverage$n_periods_consolidated > 1, na.rm = TRUE),
    max_periods_consolidated = max(dt_with_coverage$n_periods_consolidated, na.rm = TRUE),
    efficiency_ratio = round(overall_summary$total_periods_consolidated / nrow(dt_with_coverage), 2)
  )
  
  # Add original data comparison if provided
  if (!is.null(original_data)) {
    original_periods <- nrow(original_data)
    consolidation_effectiveness$original_periods = original_periods
    consolidation_effectiveness$reduction_count = original_periods - nrow(dt)
    consolidation_effectiveness$reduction_percentage = round(
      (original_periods - nrow(dt)) / original_periods * 100, 2)
  }
  
  # Coverage Categories
  coverage_categories <- data.table(
    threshold = coverage_thresholds,
    count = sapply(coverage_thresholds, function(x) 
      sum(dt_with_coverage$consolidation_coverage >= x, na.rm = TRUE)),
    percentage = round(sapply(coverage_thresholds, function(x) 
      sum(dt_with_coverage$consolidation_coverage >= x, na.rm = TRUE) / nrow(dt_with_coverage) * 100), 2)
  )
  
  # Build result list
  result <- list(
    overall_summary = overall_summary,
    coverage_distribution = coverage_distribution,
    unemployment_impact = unemployment_impact,
    consolidation_effectiveness = consolidation_effectiveness,
    coverage_categories = coverage_categories
  )
  
  # Person-level details if requested
  if (show_details && "cf" %in% names(dt)) {
    person_details <- dt_with_coverage[, {
      .(
        periods = .N,
        mean_coverage = round(mean(consolidation_coverage, na.rm = TRUE), 4),
        total_unemployment_removed = sum(unemployment_days_removed, na.rm = TRUE),
        total_periods_consolidated = sum(n_periods_consolidated, na.rm = TRUE),
        perfect_coverage_periods = sum(consolidation_coverage >= 1.0, na.rm = TRUE),
        poor_coverage_periods = sum(consolidation_coverage < 0.5, na.rm = TRUE)
      )
    }, by = cf]
    
    result$person_details <- person_details
  }
  
  class(result) <- c("consolidation_coverage_analysis", "list")
  return(result)
}

#' Print Consolidation Coverage Analysis
#'
#' @param x consolidation_coverage_analysis object
#' @param ... Additional arguments
#'
#' @export
print.consolidation_coverage_analysis <- function(x, ...) {
  cat("=== Consolidation Coverage Analysis ===\n\n")
  
  # Overall Summary
  cat("Overall Statistics:\n")
  cat("  Total periods:", x$overall_summary$total_periods, "\n")
  cat("  Consolidated periods:", x$overall_summary$consolidated_periods, "\n")
  cat("  Consolidation rate:", x$overall_summary$consolidation_rate * 100, "%\n\n")
  
  # Coverage Statistics
  cat("Coverage Distribution:\n")
  cat("  Mean coverage:", x$overall_summary$mean_coverage, "\n")
  cat("  Median coverage:", x$overall_summary$median_coverage, "\n")
  cat("  Coverage range:", x$overall_summary$min_coverage, "-", x$overall_summary$max_coverage, "\n")
  cat("  Perfect coverage (≥1.0):", x$coverage_distribution$perfect_coverage, "periods\n")
  cat("  High coverage (≥0.9):", x$coverage_distribution$high_coverage, "periods\n")
  cat("  Poor coverage (<0.5):", x$coverage_distribution$poor_coverage, "periods\n\n")
  
  # Unemployment Impact
  cat("Unemployment Removal Impact:\n")
  cat("  Total unemployment days removed:", x$overall_summary$total_unemployment_removed, "\n")
  cat("  Periods with unemployment removed:", x$unemployment_impact$periods_with_no_unemployment, 
      "no removal,", sum(x$unemployment_impact$periods_with_minor_unemployment,
                         x$unemployment_impact$periods_with_moderate_unemployment,
                         x$unemployment_impact$periods_with_major_unemployment), "with removal\n")
  cat("  Max unemployment removed:", x$unemployment_impact$max_unemployment_removed, "days\n\n")
  
  # Consolidation Effectiveness
  cat("Consolidation Effectiveness:\n")
  cat("  Original periods consolidated:", x$overall_summary$total_periods_consolidated, "\n")
  cat("  Mean periods per consolidation:", x$overall_summary$mean_periods_per_consolidation, "\n")
  cat("  Max consolidation size:", x$consolidation_effectiveness$max_periods_consolidated, "periods\n")
  
  if (!is.null(x$consolidation_effectiveness$reduction_percentage)) {
    cat("  Overall reduction:", x$consolidation_effectiveness$reduction_percentage, "%\n")
  }
  
  cat("\n")
  invisible(x)
}

#' Summarize Coverage by Categories
#'
#' @description
#' Creates a summary of consolidation coverage by employment quality categories.
#' Useful for understanding the distribution of employment quality after
#' consolidation with unemployment removal.
#'
#' @param consolidated_data data.table with consolidation coverage metrics
#' @param coverage_breaks Numeric vector defining coverage category breaks
#'   (default: c(0, 0.5, 0.75, 0.9, 0.95, 1.0))
#' @param coverage_labels Character vector of category labels
#'
#' @return data.table with coverage categories and statistics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default categories
#' summary <- summarize_coverage_categories(consolidated_data)
#' print(summary)
#' 
#' # Custom categories
#' summary <- summarize_coverage_categories(
#'   consolidated_data,
#'   coverage_breaks = c(0, 0.6, 0.8, 0.95, 1.0),
#'   coverage_labels = c("Poor", "Fair", "Good", "Excellent")
#' )
#' }
summarize_coverage_categories <- function(consolidated_data,
                                        coverage_breaks = c(0, 0.5, 0.75, 0.9, 0.95, 1.0),
                                        coverage_labels = c("Very Poor", "Poor", "Good", "High", "Perfect")) {
  
  if (!inherits(consolidated_data, "data.table")) {
    stop("consolidated_data must be a data.table object")
  }
  
  if (!"consolidation_coverage" %in% names(consolidated_data)) {
    stop("consolidation_coverage column not found in consolidated_data")
  }
  
  # Filter to records with coverage metrics
  dt <- consolidated_data[!is.na(consolidation_coverage)]
  
  if (nrow(dt) == 0) {
    stop("No records with consolidation coverage found")
  }
  
  # Create coverage categories
  dt[, coverage_category := cut(consolidation_coverage, 
                               breaks = coverage_breaks,
                               labels = coverage_labels,
                               include.lowest = TRUE,
                               right = FALSE)]
  
  # Summarize by category
  summary <- dt[, {
    .(
      count = .N,
      percentage = round(.N / nrow(dt) * 100, 2),
      mean_coverage = round(mean(consolidation_coverage, na.rm = TRUE), 4),
      median_coverage = round(median(consolidation_coverage, na.rm = TRUE), 4),
      mean_unemployment_removed = round(mean(unemployment_days_removed, na.rm = TRUE), 2),
      mean_periods_consolidated = round(mean(n_periods_consolidated, na.rm = TRUE), 2),
      total_unemployment_removed = sum(unemployment_days_removed, na.rm = TRUE)
    )
  }, by = coverage_category]
  
  # Add totals row
  totals <- list(
    coverage_category = "TOTAL",
    count = nrow(dt),
    percentage = 100.0,
    mean_coverage = round(mean(dt$consolidation_coverage, na.rm = TRUE), 4),
    median_coverage = round(median(dt$consolidation_coverage, na.rm = TRUE), 4),
    mean_unemployment_removed = round(mean(dt$unemployment_days_removed, na.rm = TRUE), 2),
    mean_periods_consolidated = round(mean(dt$n_periods_consolidated, na.rm = TRUE), 2),
    total_unemployment_removed = sum(dt$unemployment_days_removed, na.rm = TRUE)
  )
  
  result <- rbind(summary, totals, fill = TRUE)
  setorder(result, -count)
  
  class(result) <- c("consolidation_coverage_summary", "data.table", "data.frame")
  return(result)
}

#' Create Coverage Report
#'
#' @description
#' Generates a comprehensive text report of consolidation coverage analysis
#' suitable for inclusion in research documents or analytical reports.
#'
#' @param coverage_analysis Output from analyze_consolidation_coverage()
#' @param include_recommendations Logical. Include interpretation and recommendations
#' @param file Optional file path to save the report
#'
#' @return Character vector containing the report (invisibly if file specified)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate coverage analysis
#' analysis <- analyze_consolidation_coverage(consolidated_data)
#' 
#' # Create report
#' report <- create_coverage_report(analysis, include_recommendations = TRUE)
#' cat(report, sep = "\n")
#' 
#' # Save to file
#' create_coverage_report(analysis, file = "coverage_report.txt")
#' }
create_coverage_report <- function(coverage_analysis,
                                 include_recommendations = TRUE,
                                 file = NULL) {
  
  if (!inherits(coverage_analysis, "consolidation_coverage_analysis")) {
    stop("coverage_analysis must be output from analyze_consolidation_coverage()")
  }
  
  x <- coverage_analysis
  
  # Build report lines
  report_lines <- c(
    "CONSOLIDATION COVERAGE ANALYSIS REPORT",
    "====================================",
    "",
    paste("Generated:", Sys.time()),
    "",
    "EXECUTIVE SUMMARY",
    "-----------------",
    paste("Total periods analyzed:", x$overall_summary$total_periods),
    paste("Consolidated periods:", x$overall_summary$consolidated_periods, 
          sprintf("(%.1f%%)", x$overall_summary$consolidation_rate * 100)),
    paste("Mean employment coverage:", sprintf("%.1f%%", x$overall_summary$mean_coverage * 100)),
    paste("Total unemployment days removed:", x$overall_summary$total_unemployment_removed),
    "",
    "COVERAGE DISTRIBUTION",
    "--------------------",
    paste("Perfect coverage (≥100%):", x$coverage_distribution$perfect_coverage, "periods"),
    paste("High coverage (≥90%):", x$coverage_distribution$high_coverage, "periods"),
    paste("Good coverage (≥75%):", x$coverage_distribution$good_coverage, "periods"), 
    paste("Poor coverage (<50%):", x$coverage_distribution$poor_coverage, "periods"),
    "",
    "Coverage Quantiles:",
    paste("  10th percentile:", sprintf("%.1f%%", x$coverage_distribution$quantiles[["10%"]] * 100)),
    paste("  25th percentile:", sprintf("%.1f%%", x$coverage_distribution$quantiles[["25%"]] * 100)),
    paste("  50th percentile:", sprintf("%.1f%%", x$coverage_distribution$quantiles[["50%"]] * 100)),
    paste("  75th percentile:", sprintf("%.1f%%", x$coverage_distribution$quantiles[["75%"]] * 100)),
    paste("  90th percentile:", sprintf("%.1f%%", x$coverage_distribution$quantiles[["90%"]] * 100)),
    "",
    "UNEMPLOYMENT REMOVAL IMPACT",
    "---------------------------",
    paste("Periods with no unemployment removed:", x$unemployment_impact$periods_with_no_unemployment),
    paste("Periods with minor unemployment (≤7 days):", x$unemployment_impact$periods_with_minor_unemployment),
    paste("Periods with moderate unemployment (8-30 days):", x$unemployment_impact$periods_with_moderate_unemployment),
    paste("Periods with major unemployment (>30 days):", x$unemployment_impact$periods_with_major_unemployment),
    paste("Maximum unemployment removed:", x$unemployment_impact$max_unemployment_removed, "days"),
    "",
    "CONSOLIDATION EFFECTIVENESS",
    "---------------------------",
    paste("Total original periods consolidated:", x$overall_summary$total_periods_consolidated),
    paste("Mean periods per consolidation:", sprintf("%.1f", x$overall_summary$mean_periods_per_consolidation)),
    paste("Maximum consolidation size:", x$consolidation_effectiveness$max_periods_consolidated, "periods"),
    paste("Efficiency ratio:", sprintf("%.1f", x$consolidation_effectiveness$efficiency_ratio))
  )
  
  # Add original data comparison if available
  if (!is.null(x$consolidation_effectiveness$reduction_percentage)) {
    report_lines <- c(report_lines,
      paste("Overall period reduction:", sprintf("%.1f%%", x$consolidation_effectiveness$reduction_percentage)),
      paste("Periods reduced:", x$consolidation_effectiveness$reduction_count, 
            "out of", x$consolidation_effectiveness$original_periods)
    )
  }
  
  # Add recommendations if requested
  if (include_recommendations) {
    report_lines <- c(report_lines, "",
      "INTERPRETATION AND RECOMMENDATIONS",
      "==================================",
      ""
    )
    
    # Coverage quality assessment
    mean_cov <- x$overall_summary$mean_coverage
    if (mean_cov >= 0.9) {
      report_lines <- c(report_lines, "✓ EXCELLENT: High employment coverage indicates strong job continuity")
    } else if (mean_cov >= 0.75) {
      report_lines <- c(report_lines, "✓ GOOD: Moderate employment coverage with some fragmentation")
    } else if (mean_cov >= 0.5) {
      report_lines <- c(report_lines, "⚠ FAIR: Moderate employment coverage suggests significant unemployment periods")
    } else {
      report_lines <- c(report_lines, "⚠ POOR: Low employment coverage indicates highly fragmented employment")
    }
    
    # Consolidation effectiveness assessment
    if (x$overall_summary$consolidation_rate > 0.3) {
      report_lines <- c(report_lines, "✓ Consolidation significantly reduced data complexity")
    } else {
      report_lines <- c(report_lines, "ℹ Limited consolidation suggests data already contained distinct employment periods")
    }
    
    # Unemployment removal impact
    if (x$overall_summary$total_unemployment_removed > 0) {
      report_lines <- c(report_lines, 
        paste("ℹ Removed", x$overall_summary$total_unemployment_removed, 
              "days of intermediate unemployment for cleaner analysis"))
    }
    
    report_lines <- c(report_lines, "",
      "Recommendations:",
      "- Use coverage metrics to assess employment quality",
      "- Consider coverage thresholds for employment stability analysis", 
      "- Compare coverage across different demographic groups",
      "- Use unemployment_days_removed for job security assessments"
    )
  }
  
  report_lines <- c(report_lines, "", "End of Report", "")
  
  # Output handling
  if (!is.null(file)) {
    writeLines(report_lines, file)
    message("Coverage report saved to: ", file)
    invisible(report_lines)
  } else {
    return(report_lines)
  }
}