#' Visualization Functions for Contract Survival Analysis
#'
#' @description
#' This module provides comprehensive visualization capabilities for survival
#' analysis results, including Kaplan-Meier curves, risk tables, and
#' comparative plots across contract types.
#'
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon geom_point geom_vline geom_text geom_tile geom_line geom_errorbarh labs theme_minimal theme element_text element_blank element_line scale_fill_gradient scale_fill_gradient2 scale_fill_viridis_c scale_color_manual scale_fill_manual scale_x_continuous scale_y_continuous scale_y_discrete annotate margin
#' @importFrom data.table data.table melt
#' @importFrom utils packageVersion
#' @name survival_visualization
NULL

#' Visualize Contract Survival Curves
#'
#' @description
#' Creates publication-ready Kaplan-Meier survival curves with confidence
#' intervals and optional risk tables for different contract types.
#'
#' @param survival_curves List output from estimate_contract_survival()
#' @param contract_types Character vector. Contract types to include (NULL for all)
#' @param show_confidence Logical. Display confidence intervals (default: TRUE)
#' @param show_censored Logical. Mark censored observations (default: TRUE)
#' @param show_median Logical. Add median survival lines (default: TRUE)
#' @param risk_table Logical. Include risk table below plot (default: FALSE)
#' @param title Character. Plot title
#' @param subtitle Character. Plot subtitle
#' @param x_label Character. X-axis label (default: "Time (days)")
#' @param y_label Character. Y-axis label (default: "Survival Probability")
#' @param color_palette Character or vector. RColorBrewer palette name (e.g., "Set2", "Dark2") 
#'   or custom color vector. For large numbers of categories (>8), colors are automatically
#'   extended using interpolation
#' @param theme_function Function. ggplot2 theme to apply
#'
#' @return A ggplot object or combined plot with risk table
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic survival curves
#' plot_survival <- visualize_contract_survival(
#'   survival_curves = survival_results,
#'   title = "Contract Duration by Type"
#' )
#' 
#' # With risk table
#' plot_with_risk <- visualize_contract_survival(
#'   survival_curves = survival_results,
#'   risk_table = TRUE,
#'   show_median = TRUE
#' )
#' }
visualize_contract_survival <- function(
    survival_curves,
    contract_types = NULL,
    show_confidence = TRUE,
    show_censored = TRUE,
    show_median = TRUE,
    risk_table = FALSE,
    title = "Survival Curves by Contract Type",
    subtitle = NULL,
    x_label = "Time (days)",
    y_label = "Survival Probability",
    color_palette = "Set2",
    theme_function = theme_minimal
) {
  
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualization")
  }
  
  # Determine which contract types to plot
  if (is.null(contract_types)) {
    contract_types <- names(survival_curves$survival_tables)
  }
  
  # Combine survival data for all contract types
  plot_data <- data.table()
  
  for (ct in contract_types) {
    if (ct %in% names(survival_curves$survival_tables)) {
      ct_data <- copy(survival_curves$survival_tables[[ct]])
      ct_data[, contract_type := ct]
      
      # Add time 0 for step function
      zero_row <- data.table(
        time = 0,
        survival_prob = 1,
        std_error = 0,
        lower_ci = 1,
        upper_ci = 1,
        n_risk = ct_data[1, n_risk],
        n_event = 0,
        contract_type = ct
      )
      
      plot_data <- rbind(plot_data, zero_row, ct_data)
    }
  }
  
  # Create main survival plot
  p <- ggplot(plot_data, aes(x = time, y = survival_prob, color = contract_type)) +
    geom_step(linewidth = 1) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      color = "Contract Type"
    ) +
    theme_function() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(expand = c(0.02, 0))
  
  # Add confidence intervals if requested
  if (show_confidence) {
    # Create stepped confidence interval data
    stepped_ci_data <- data.table()
    
    for (ct in unique(plot_data$contract_type)) {
      ct_data <- plot_data[contract_type == ct][order(time)]
      
      # Create stepped ribbon data by duplicating points
      stepped_data <- data.table()
      
      for (i in 1:nrow(ct_data)) {
        # Add current point
        stepped_data <- rbind(stepped_data, ct_data[i])
        
        # Add next point with same survival values (for horizontal step)
        if (i < nrow(ct_data)) {
          next_time <- ct_data[i + 1, time]
          step_point <- copy(ct_data[i])
          step_point[, time := next_time]
          stepped_data <- rbind(stepped_data, step_point)
        }
      }
      
      stepped_ci_data <- rbind(stepped_ci_data, stepped_data)
    }
    
    p <- p + geom_ribbon(
      data = stepped_ci_data,
      aes(ymin = lower_ci, ymax = upper_ci, fill = contract_type),
      alpha = 0.2
    )
  }
  
  # Add median survival lines if requested
  if (show_median) {
    median_data <- data.table(
      contract_type = names(survival_curves$median_survival),
      median_time = survival_curves$median_survival
    )
    
    p <- p + geom_vline(
      data = median_data,
      aes(xintercept = median_time, color = contract_type),
      linetype = "dashed",
      alpha = 0.6
    )
  }
  
  # Add censoring marks if requested
  if (show_censored) {
    censored_data <- plot_data[n_event == 0 & time > 0]
    if (nrow(censored_data) > 0) {
      p <- p + geom_point(
        data = censored_data,
        aes(x = time, y = survival_prob),
        shape = 3,
        size = 2
      )
    }
  }
  
  # Apply color palette
  n_colors <- length(contract_types)
  
  if (is.character(color_palette) && length(color_palette) == 1) {
    # Single palette name provided
    if (color_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      # Handle different numbers of categories
      if (n_colors <= 1) {
        # Single color case
        colors <- RColorBrewer::brewer.pal(3, color_palette)[1]
      } else if (n_colors <= 8) {
        # Use RColorBrewer directly for small numbers
        colors <- RColorBrewer::brewer.pal(max(3, n_colors), color_palette)
        if (n_colors < length(colors)) {
          colors <- colors[1:n_colors]
        }
      } else {
        # Generate extended color palette for large numbers
        base_colors <- RColorBrewer::brewer.pal(8, color_palette)
        
        # Interpolate additional colors using colorRampPalette
        color_func <- colorRampPalette(base_colors)
        colors <- color_func(n_colors)
      }
      
      p <- p + scale_color_manual(values = colors, name = "Contract Type") +
               scale_fill_manual(values = colors, name = "Contract Type", guide = "none")
    } else {
      # Fallback to default ggplot2 colors for unknown palette names
      warning(sprintf("Color palette '%s' not found in RColorBrewer. Using default ggplot2 colors.", color_palette))
    }
  } else if (is.vector(color_palette) && length(color_palette) >= n_colors) {
    # Use custom color vector if provided and sufficient
    colors <- color_palette[1:n_colors]
    p <- p + scale_color_manual(values = colors) +
             scale_fill_manual(values = colors)
  } else if (is.vector(color_palette) && length(color_palette) > 0 && length(color_palette) < n_colors) {
    # Extend insufficient custom color vector
    warning(sprintf("Color palette has %d colors but %d needed. Extending palette.", length(color_palette), n_colors))
    color_func <- colorRampPalette(color_palette)
    colors <- color_func(n_colors)
    p <- p + scale_color_manual(values = colors) +
             scale_fill_manual(values = colors)
  }
  
  # Add risk table if requested
  if (risk_table) {
    risk_table_plot <- create_risk_table(
      survival_curves = survival_curves,
      contract_types = contract_types,
      time_points = NULL
    )
    
    # Combine plots using patchwork if available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      combined_plot <- p / risk_table_plot + 
        patchwork::plot_layout(heights = c(3, 1))
      return(combined_plot)
    } else {
      warning("Package 'patchwork' not available. Returning main plot only.")
      return(p)
    }
  }
  
  return(p)
}

#' Create Risk Table for Survival Plot
#'
#' @description
#' Creates a risk table showing the number at risk at specified time points
#' for each contract type.
#'
#' @param survival_curves List output from estimate_contract_survival()
#' @param contract_types Character vector. Contract types to include
#' @param time_points Numeric vector. Time points for risk table (NULL for automatic)
#'
#' @return A ggplot object containing the risk table
#'
#' @export
create_risk_table <- function(
    survival_curves,
    contract_types = NULL,
    time_points = NULL
) {
  
  if (is.null(contract_types)) {
    contract_types <- names(survival_curves$survival_tables)
  }
  
  # Determine time points if not specified
  if (is.null(time_points)) {
    all_times <- numeric()
    for (ct in contract_types) {
      all_times <- c(all_times, survival_curves$survival_tables[[ct]]$time)
    }
    max_time <- max(all_times)
    time_points <- seq(0, max_time, length.out = min(10, max_time))
  }
  
  # Create risk table data
  risk_data <- data.table()
  
  for (ct in contract_types) {
    surv_table <- survival_curves$survival_tables[[ct]]
    
    for (tp in time_points) {
      # Find number at risk at this time point
      n_risk <- if (tp == 0) {
        surv_table[1, n_risk]
      } else {
        idx <- which(surv_table$time <= tp)
        if (length(idx) > 0) {
          surv_table[max(idx), n_risk]
        } else {
          surv_table[1, n_risk]
        }
      }
      
      risk_data <- rbind(risk_data, data.table(
        contract_type = ct,
        time = tp,
        n_risk = n_risk
      ))
    }
  }
  
  # Create risk table plot
  p_risk <- ggplot(risk_data, aes(x = time, y = contract_type)) +
    geom_text(aes(label = n_risk), size = 3) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold"),
      axis.title = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    scale_x_continuous(
      breaks = time_points,
      labels = round(time_points),
      limits = range(time_points)
    )
  
  return(p_risk)
}

#' Compare Survival Curves Visually
#'
#' @description
#' Creates a comparative visualization of survival curves with statistical
#' test results and hazard ratio annotations.
#'
#' @param data A data.table with contract information
#' @param survival_curves List output from estimate_contract_survival()
#' @param comparison_results List output from compare_contract_survival() or logical TRUE to compute it
#' @param groups_to_show Character vector. Specific contract types to display (NULL for all)
#' @param highlight_significant Logical. Highlight significant differences
#' @param show_pvalues Logical. Display p-values on plot
#' @param reference_group Character. Reference contract type for comparisons
#' @param max_groups Integer. Maximum number of groups to display (default: 10)
#' @param color_palette Character or vector. Color palette for the plot
#'
#' @return A ggplot object with comparative survival curves
#'
#' @export
plot_survival_comparison <- function(
    data,
    survival_curves,
    comparison_results = NULL,
    groups_to_show = NULL,
    highlight_significant = TRUE,
    show_pvalues = TRUE,
    reference_group = NULL,
    max_groups = 10,
    color_palette = "Set2"
) {
  
  # Handle comparison_results parameter
  if (is.logical(comparison_results) && comparison_results == TRUE) {
    # If TRUE was passed, compute the comparison results
    if (requireNamespace("survival", quietly = TRUE)) {
      # Create a simplified comparison for subtitle
      comparison_results <- list(
        test_results = list(
          logrank = list(
            chisq = NA,
            p_value = NA
          )
        )
      )
      # Note: Actual comparison would require the compare_contract_survival function
      warning("comparison_results = TRUE requires actual comparison data. Pass the output from compare_contract_survival() instead.")
    } else {
      comparison_results <- NULL
    }
  }
  
  # Filter groups if specified
  contract_types <- names(survival_curves$survival_tables)
  
  if (!is.null(groups_to_show)) {
    # Filter to specified groups
    contract_types <- intersect(groups_to_show, contract_types)
    if (length(contract_types) == 0) {
      stop("None of the specified groups were found in the survival curves")
    }
  } else if (length(contract_types) > max_groups) {
    # Limit to max_groups if too many
    warning(sprintf("Showing only first %d contract types. Use 'groups_to_show' parameter to specify which groups to display.", max_groups))
    
    # Select groups based on median survival time for better variety
    median_times <- sapply(contract_types, function(ct) {
      survival_curves$median_survival[[ct]]
    })
    
    # Sort by median survival and take evenly spaced groups
    sorted_types <- names(sort(median_times, na.last = TRUE))
    indices <- round(seq(1, length(sorted_types), length.out = max_groups))
    contract_types <- sorted_types[indices]
  }
  
  # Filter survival curves to only include selected contract types
  filtered_survival_curves <- survival_curves
  
  # Filter the survival tables and other components
  filtered_survival_curves$survival_tables <- survival_curves$survival_tables[contract_types]
  filtered_survival_curves$survival_fits <- survival_curves$survival_fits[contract_types]
  filtered_survival_curves$median_survival <- survival_curves$median_survival[contract_types]
  if (!is.null(survival_curves$confidence_intervals)) {
    filtered_survival_curves$confidence_intervals <- survival_curves$confidence_intervals[contract_types]
  }
  
  # Create base survival plot with filtered groups
  p <- visualize_contract_survival(
    survival_curves = filtered_survival_curves,
    contract_types = contract_types,
    show_confidence = TRUE,
    show_median = TRUE,
    title = "Contract Survival Comparison",
    subtitle = if (!is.null(comparison_results) && !is.na(comparison_results$test_results$logrank$chisq)) {
      sprintf("Log-rank p-value: %.4f", 
              1 - pchisq(comparison_results$test_results$logrank$chisq, 
                        length(contract_types) - 1))
    } else NULL,
    color_palette = color_palette
  )
  
  # Add statistical annotations if available
  if (!is.null(comparison_results) && show_pvalues && !is.na(comparison_results$test_results$logrank$chisq)) {
    # Extract p-value
    if (!is.null(comparison_results$test_results$logrank)) {
      p_value <- 1 - pchisq(
        comparison_results$test_results$logrank$chisq,
        df = length(contract_types) - 1
      )
      
      # Add p-value annotation
      p <- p + annotate(
        "text",
        x = Inf, y = 0.95,
        label = sprintf("p = %.3f", p_value),
        hjust = 1.1,
        vjust = 1,
        size = 4,
        fontface = if (p_value < 0.05) "bold" else "plain"
      )
    }
  }
  
  return(p)
}

#' Create Survival Heatmap by Contract Type
#'
#' @description
#' Creates a heatmap showing survival probabilities across time for
#' different contract types.
#'
#' @param survival_curves List output from estimate_contract_survival()
#' @param time_grid Numeric vector. Time points for heatmap (NULL for automatic)
#' @param color_scale Character. Color scale: "viridis", "heat", "cool"
#' @param show_values Logical. Display probability values in cells
#'
#' @return A ggplot heatmap object
#'
#' @export
plot_survival_heatmap <- function(
    survival_curves,
    time_grid = NULL,
    color_scale = "viridis",
    show_values = FALSE
) {
  
  # Determine time grid
  if (is.null(time_grid)) {
    max_times <- sapply(survival_curves$survival_tables, function(x) max(x$time))
    max_time <- max(max_times)
    time_grid <- seq(0, max_time, length.out = min(20, max_time))
  }
  
  # Create heatmap data
  heatmap_data <- data.table()
  
  for (ct in names(survival_curves$survival_tables)) {
    surv_table <- survival_curves$survival_tables[[ct]]
    
    for (t in time_grid) {
      surv_prob <- get_survival_at_time(surv_table, t)
      
      heatmap_data <- rbind(heatmap_data, data.table(
        contract_type = ct,
        time = t,
        survival_prob = surv_prob
      ))
    }
  }
  
  # Create heatmap
  p <- ggplot(heatmap_data, aes(x = time, y = contract_type, fill = survival_prob)) +
    geom_tile() +
    labs(
      title = "Survival Probability Heatmap",
      x = "Time (days)",
      y = "Contract Type",
      fill = "Survival\nProbability"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(face = "bold"),
      legend.position = "right"
    )
  
  # Apply color scale
  if (color_scale == "viridis") {
    # Check if ggplot2 has viridis scales (available since ggplot2 3.0.0)
    if (utils::packageVersion("ggplot2") >= "3.0.0") {
      p <- p + ggplot2::scale_fill_viridis_c(limits = c(0, 1))
    } else {
      # Fallback to viridis package or manual gradient
      if (requireNamespace("viridis", quietly = TRUE)) {
        p <- p + viridis::scale_fill_viridis(limits = c(0, 1))
      } else {
        warning("Viridis color scale not available. Using blue-purple gradient as fallback.")
        p <- p + scale_fill_gradient(
          low = "#440154", high = "#FDE725", 
          limits = c(0, 1),
          name = "Survival\nProbability"
        )
      }
    }
  } else if (color_scale == "heat") {
    p <- p + scale_fill_gradient2(
      low = "red", mid = "yellow", high = "green",
      midpoint = 0.5, limits = c(0, 1)
    )
  } else if (color_scale == "cool") {
    p <- p + scale_fill_gradient(low = "darkblue", high = "lightblue", limits = c(0, 1))
  }
  
  # Add values if requested
  if (show_values) {
    p <- p + geom_text(
      aes(label = sprintf("%.2f", survival_prob)),
      size = 3,
      color = "black"
    )
  }
  
  return(p)
}

#' Create Forest Plot of Median Survival Times
#'
#' @description
#' Creates a forest plot showing median survival times with confidence
#' intervals for each contract type.
#'
#' @param survival_curves List output from estimate_contract_survival()
#' @param order_by Character. How to order: "median", "alphabetical", "custom"
#' @param custom_order Character vector. Custom ordering of contract types
#' @param show_n Logical. Show sample sizes
#'
#' @return A ggplot forest plot object
#'
#' @export
plot_median_survival_forest <- function(
    survival_curves,
    order_by = "median",
    custom_order = NULL,
    show_n = TRUE
) {
  
  # Prepare data for forest plot
  forest_data <- data.table()
  
  for (ct in names(survival_curves$median_survival)) {
    # Get survival fit for CI
    km_fit <- survival_curves$survival_fits[[ct]]
    
    # Extract confidence intervals with proper error handling
    lower_ci <- NA_real_
    upper_ci <- NA_real_
    
    if (!is.null(km_fit)) {
      # Try to get median confidence intervals from quantile function
      tryCatch({
        ci_result <- quantile(km_fit, probs = 0.5)
        lower_ci <- ci_result$lower
        upper_ci <- ci_result$upper
        
        # Ensure confidence intervals are finite and valid
        if (is.na(lower_ci) || is.infinite(lower_ci) || lower_ci < 0) {
          lower_ci <- NA_real_
        }
        if (is.na(upper_ci) || is.infinite(upper_ci) || upper_ci < 0) {
          upper_ci <- NA_real_
        }
        
        # Additional validation: CI should be reasonable relative to median
        median_val <- survival_curves$median_survival[ct]
        if (!is.na(median_val) && !is.na(lower_ci) && !is.na(upper_ci)) {
          # Check if CI makes sense (lower <= median <= upper)
          if (lower_ci > median_val || upper_ci < median_val) {
            warning(sprintf("Contract type '%s': Invalid confidence intervals (lower: %.2f, median: %.2f, upper: %.2f). Setting to NA.", 
                           ct, lower_ci, median_val, upper_ci))
            lower_ci <- NA_real_
            upper_ci <- NA_real_
          }
        }
      }, error = function(e) {
        warning(sprintf("Contract type '%s': Cannot extract confidence intervals: %s", ct, e$message))
      })
    }
    
    forest_data <- rbind(forest_data, data.table(
      contract_type = ct,
      median = survival_curves$median_survival[ct],
      lower_ci = lower_ci,
      upper_ci = upper_ci,
      n = if (!is.null(km_fit)) km_fit$n else 0
    ))
  }
  
  # Remove rows with missing median values for ordering
  valid_data <- forest_data[!is.na(median)]
  
  if (nrow(valid_data) == 0) {
    stop("No valid median survival times available for forest plot")
  }
  
  # Order data
  if (order_by == "median") {
    forest_data <- forest_data[order(median, na.last = TRUE)]
  } else if (order_by == "alphabetical") {
    forest_data <- forest_data[order(contract_type)]
  } else if (order_by == "custom" && !is.null(custom_order)) {
    # Create factor with custom order, preserving all levels
    all_types <- unique(forest_data$contract_type)
    ordered_levels <- c(intersect(custom_order, all_types), 
                       setdiff(all_types, custom_order))
    forest_data[, contract_type := factor(contract_type, levels = ordered_levels)]
    forest_data <- forest_data[order(contract_type)]
  }
  
  # Create appropriate labels and Y-axis ordering
  if (show_n) {
    # Create labels with sample sizes
    forest_data[, display_label := sprintf("%s (n=%d)", contract_type, n)]
    
    # Create the plot with proper factor ordering
    # Use the median values for ordering but display the labels
    y_order <- forest_data[order(median, na.last = TRUE)]$display_label
    forest_data[, display_label := factor(display_label, levels = y_order)]
    
    # Create forest plot with sample size labels
    p <- ggplot(forest_data, aes(y = display_label)) +
      geom_point(aes(x = median), size = 3) +
      labs(y = "Contract Type")
    
  } else {
    # Create plot without sample sizes
    # Order contract types by median for Y-axis
    if (order_by == "median") {
      y_order <- forest_data[order(median, na.last = TRUE)]$contract_type
      forest_data[, contract_type := factor(contract_type, levels = y_order)]
    }
    
    p <- ggplot(forest_data, aes(y = contract_type)) +
      geom_point(aes(x = median), size = 3) +
      labs(y = "Contract Type")
  }
  
  # Add error bars only for non-missing confidence intervals
  ci_data <- forest_data[!is.na(lower_ci) & !is.na(upper_ci)]
  
  if (nrow(ci_data) > 0) {
    if (show_n) {
      p <- p + geom_errorbarh(
        data = ci_data,
        aes(y = display_label, xmin = lower_ci, xmax = upper_ci),
        height = 0.2
      )
    } else {
      p <- p + geom_errorbarh(
        data = ci_data,
        aes(y = contract_type, xmin = lower_ci, xmax = upper_ci),
        height = 0.2
      )
    }
  }
  
  # Complete the plot
  p <- p +
    labs(
      title = "Median Survival Times by Contract Type",
      x = "Median Survival Time (days)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "gray90")
    )
  
  # Add reference line at overall median
  overall_median <- median(forest_data$median, na.rm = TRUE)
  if (!is.na(overall_median)) {
    p <- p + geom_vline(
      xintercept = overall_median,
      linetype = "dashed",
      color = "red",
      alpha = 0.5
    )
  }
  
  # Add informative note if some CIs are missing
  missing_ci_count <- sum(is.na(forest_data$lower_ci) | is.na(forest_data$upper_ci))
  if (missing_ci_count > 0) {
    p <- p + labs(
      caption = sprintf("Note: Confidence intervals not available for %d contract type(s)", missing_ci_count)
    )
  }
  
  return(p)
}

#' Validate Animation Setup
#'
#' @description
#' Internal function to validate that animation prerequisites are met
#'
#' @keywords internal
validate_animation_setup <- function() {
  # Check for essential graphics devices
  if (!capabilities("png")) {
    stop("PNG graphics device is not available. This is required for animations.")
  }
  
  # Check temp directory accessibility
  temp_dir <- tempdir()
  if (!dir.exists(temp_dir)) {
    stop("Temporary directory is not accessible: ", temp_dir)
  }
  
  # Test write permissions in temp directory
  test_file <- file.path(temp_dir, paste0("gganimate_test_", Sys.getpid(), ".txt"))
  tryCatch({
    writeLines("test", test_file)
    file.remove(test_file)
  }, error = function(e) {
    stop("Cannot write to temporary directory: ", temp_dir, ". Error: ", e$message)
  })
  
  # Check gganimate version compatibility
  gganimate_version <- packageVersion("gganimate")
  if (gganimate_version < "1.0.0") {
    warning("gganimate version ", gganimate_version, " may have compatibility issues. Version >= 1.0.0 recommended.")
  }
  
  # Check for at least one working renderer backend
  has_working_renderer <- any(c(
    requireNamespace("gifski", quietly = TRUE),
    requireNamespace("magick", quietly = TRUE),
    capabilities("png")
  ))
  
  if (!has_working_renderer) {
    stop("No suitable animation renderer found. Install 'gifski' or 'magick' package.")
  }
  
  invisible(TRUE)
}

#' Create Animated Survival Curves
#'
#' @description
#' Creates an memory-efficient animated visualization of survival curves over time using gganimate.
#' Optimized to handle large datasets without memory overflow and robust against renderer failures.
#'
#' @param survival_curves List output from estimate_contract_survival()
#' @param animation_speed Numeric. Speed of animation in fps (default: 10)
#' @param save_as Character. File path to save animation (NULL to display only)
#' @param max_frames Integer. Maximum number of animation frames to reduce memory usage (default: 50)
#' @param sample_every Integer. Sample every nth time point to reduce data density (default: 1)
#' @param width Integer. Animation width in pixels (default: 800)
#' @param height Integer. Animation height in pixels (default: 600)
#'
#' @return An animated ggplot object or saved file path
#'
#' @export
animate_survival_curves <- function(
    survival_curves,
    animation_speed = 10,
    save_as = NULL,
    max_frames = 50,  # Reduced default for stability
    sample_every = 1,
    width = 800,
    height = 600
) {
  
  # Apply safety limits to prevent segfaults
  width <- min(width, 1200)    # Limit maximum width
  height <- min(height, 900)   # Limit maximum height
  max_frames <- min(max_frames, 30)  # Limit maximum frames
  animation_speed <- min(animation_speed, 12)  # Limit animation speed
  
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop("Package 'gganimate' is required for animations")
  }
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for efficient data processing")
  }
  
  # Validate animation prerequisites
  validate_animation_setup()
  
  # Validate input data
  if (!is.list(survival_curves) || is.null(survival_curves$survival_tables)) {
    stop("Invalid survival_curves input. Expected output from estimate_contract_survival()")
  }
  
  if (length(survival_curves$survival_tables) == 0) {
    stop("No survival tables found in survival_curves")
  }
  
  # Pre-process to find optimal frame sampling
  contract_types <- names(survival_curves$survival_tables)
  
  # Calculate total potential frames and determine sampling strategy
  total_points <- sum(sapply(contract_types, function(ct) {
    nrow(survival_curves$survival_tables[[ct]])
  }))
  
  # Adaptive sampling to stay within memory limits
  if (total_points > 5000) {  # More conservative threshold
    sample_every <- max(sample_every, ceiling(total_points / 2000))
    max_frames <- min(max_frames, 30)  # Further limit frames for large datasets
    message(sprintf("Large dataset detected (%d points). Using adaptive sampling (every %d points, max %d frames).", 
                   total_points, sample_every, max_frames))
  }
  
  # Create animation data using a simpler, more robust approach
  # This avoids complex path transitions that can cause segfaults
  
  # Find common time grid across all contract types
  all_times <- unique(unlist(lapply(contract_types, function(ct) {
    survival_curves$survival_tables[[ct]]$time
  })))
  all_times <- sort(all_times)
  
  # Sample time points to reduce frames
  if (length(all_times) > max_frames) {
    time_indices <- c(1, round(seq(2, length(all_times), length.out = max_frames - 1)))
    time_indices <- unique(time_indices)
    selected_times <- all_times[time_indices]
  } else {
    selected_times <- all_times
  }
  
  # Further sampling if needed
  if (sample_every > 1 && length(selected_times) > sample_every) {
    keep_indices <- c(1, seq(sample_every, length(selected_times), by = sample_every), length(selected_times))
    keep_indices <- unique(keep_indices)
    selected_times <- selected_times[keep_indices]
  }
  
  # Create animation data with consistent structure
  anim_data <- data.table()
  
  for (frame_idx in seq_along(selected_times)) {
    current_time <- selected_times[frame_idx]
    
    for (ct in contract_types) {
      surv_table <- survival_curves$survival_tables[[ct]]
      
      # Find survival probability at current time (step function)
      surv_prob <- if (current_time <= min(surv_table$time)) {
        1.0  # Before first event
      } else {
        # Find latest time <= current_time
        valid_times <- surv_table$time[surv_table$time <= current_time]
        if (length(valid_times) > 0) {
          latest_time <- max(valid_times)
          surv_table$survival_prob[surv_table$time == latest_time][1]
        } else {
          1.0
        }
      }
      
      # Add to animation data
      anim_data <- rbind(anim_data, data.table(
        contract_type = ct,
        time = current_time,
        survival_prob = surv_prob,
        frame = frame_idx
      ))
    }
  }
  
  # Clear intermediate objects to free memory
  rm(all_times, selected_times)
  gc()
  
  # Calculate actual frames before creating plot
  actual_frames <- min(max(anim_data$frame), max_frames)
  
  message(sprintf("Creating animation with %d frames from %d data points", 
                 actual_frames, nrow(anim_data)))
  
  # Ensure ggplot2 is properly loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for animations")
  }
  
  # Create animated plot with simpler approach to avoid segfaults
  # Use transition_time instead of transition_states for more robust animation
  p <- ggplot2::ggplot(anim_data, ggplot2::aes(x = time, y = survival_prob, color = contract_type)) +
    ggplot2::geom_step(direction = "hv", linewidth = 1.2, alpha = 0.8) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::labs(
      title = "Contract Survival Curves",
      subtitle = "Time: {closest_state}",
      x = "Time (days)",
      y = "Survival Probability",
      color = "Contract Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_x_continuous(expand = c(0.02, 0)) +
    gganimate::transition_states(
      frame,
      transition_length = 2,
      state_length = 1,
      wrap = FALSE
    ) +
    gganimate::ease_aes("linear")
  
  # Create a clean temporary directory for this animation
  anim_temp_dir <- file.path(tempdir(), paste0("longworkR_anim_", Sys.getpid()))
  
  # Ensure clean temp directory creation
  if (dir.exists(anim_temp_dir)) {
    unlink(anim_temp_dir, recursive = TRUE, force = TRUE)
  }
  
  tryCatch({
    dir.create(anim_temp_dir, recursive = TRUE, showWarnings = FALSE)
  }, error = function(e) {
    anim_temp_dir <<- tempdir()  # Fallback to default temp dir
    warning("Could not create dedicated temp directory, using default: ", e$message)
  })
  
  # Create animation with robust error handling and multiple fallback strategies
  anim <- NULL
  
  # Strategy 1: Try with gifski renderer (most reliable)
  if (is.null(anim) && requireNamespace("gifski", quietly = TRUE)) {
    tryCatch({
      message("Attempting animation with gifski renderer...")
      anim <- gganimate::animate(
        p,
        fps = animation_speed,
        nframes = actual_frames,
        width = width,
        height = height,
        res = 96,
        renderer = gganimate::gifski_renderer(
          loop = TRUE,
          width = width,
          height = height
        )
      )
      message("Success with gifski renderer")
    }, error = function(e) {
      message("gifski renderer failed: ", e$message)
      anim <<- NULL
    })
  }
  
  # Strategy 2: Try with magick renderer 
  if (is.null(anim) && requireNamespace("magick", quietly = TRUE)) {
    tryCatch({
      message("Attempting animation with magick renderer...")
      anim <- gganimate::animate(
        p,
        fps = animation_speed,
        nframes = actual_frames,
        width = width,
        height = height,
        res = 96,
        renderer = gganimate::magick_renderer(
          loop = TRUE
        )
      )
      message("Success with magick renderer")
    }, error = function(e) {
      message("magick renderer failed: ", e$message)
      anim <<- NULL
    })
  }
  
  # Strategy 3: Try with simple settings and no explicit renderer
  if (is.null(anim)) {
    tryCatch({
      message("Attempting animation with default settings...")
      # Temporarily set options for device handling
      old_dev_option <- getOption("gganimate.dev_args")
      options(gganimate.dev_args = list(type = "cairo-png"))
      
      anim <- gganimate::animate(
        p,
        fps = min(animation_speed, 8),  # Slower animation
        nframes = min(actual_frames, 20),  # Fewer frames
        width = min(width, 600),   # Smaller size
        height = min(height, 450),
        res = 72  # Lower resolution
      )
      
      # Restore options
      options(gganimate.dev_args = old_dev_option)
      message("Success with default renderer")
    }, error = function(e) {
      message("Default renderer failed: ", e$message)
      anim <<- NULL
    })
  }
  
  # Strategy 4: Last resort - very conservative settings
  if (is.null(anim)) {
    tryCatch({
      message("Attempting animation with minimal settings...")
      
      # Create a much simpler plot with explicit namespace references
      simple_p <- ggplot2::ggplot(anim_data[frame <= 10], ggplot2::aes(x = time, y = survival_prob, color = contract_type)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::labs(
          title = "Survival Curves",
          x = "Time (days)",
          y = "Survival Probability"
        ) +
        ggplot2::theme_minimal() +
        gganimate::transition_states(frame, transition_length = 1, state_length = 2)
      
      anim <- gganimate::animate(
        simple_p,
        fps = 5,
        nframes = min(10, actual_frames),
        width = 400,
        height = 300,
        res = 72
      )
      message("Success with minimal settings")
    }, error = function(e) {
      message("All animation strategies failed: ", e$message)
      stop("Unable to create animation. Please check gganimate installation and system graphics capabilities. Last error: ", e$message)
    })
  }
  
  # Clean up animation data to free memory
  rm(anim_data, p)
  gc()
  
  # Clean up temp directory
  if (dir.exists(anim_temp_dir) && anim_temp_dir != tempdir()) {
    unlink(anim_temp_dir, recursive = TRUE, force = TRUE)
  }
  
  # Save if requested
  if (!is.null(save_as) && !is.null(anim)) {
    # Handle directory vs file path
    if (dir.exists(save_as) || endsWith(save_as, "/")) {
      # save_as is a directory, create filename
      save_dir <- normalizePath(save_as, mustWork = FALSE)
      save_file <- file.path(save_dir, "survival_animation.gif")
    } else {
      # save_as is a file path
      save_file <- normalizePath(save_as, mustWork = FALSE)
      save_dir <- dirname(save_file)
    }
    
    # Ensure directory exists
    if (!dir.exists(save_dir)) {
      tryCatch({
        dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      }, error = function(e) {
        warning("Could not create directory: ", save_dir, ". Error: ", e$message)
        save_file <- file.path(tempdir(), "survival_animation.gif")
        save_dir <- tempdir()
      })
    }
    
    # Ensure file has .gif extension
    if (!grepl("\\.(gif|png|mp4)$", save_file, ignore.case = TRUE)) {
      save_file <- paste0(save_file, ".gif")
    }
    
    # Save animation with error handling
    tryCatch({
      gganimate::anim_save(save_file, animation = anim)
      message(sprintf("Animation saved to: %s", save_file))
      return(invisible(save_file))
    }, error = function(e) {
      warning("Failed to save animation: ", e$message)
      # Try saving to temp directory as fallback
      temp_file <- file.path(tempdir(), "survival_animation_backup.gif")
      tryCatch({
        gganimate::anim_save(temp_file, animation = anim)
        message(sprintf("Animation saved to temporary location: %s", temp_file))
        return(invisible(temp_file))
      }, error = function(e2) {
        warning("Failed to save animation even to temp directory: ", e2$message)
        return(anim)  # Return the animation object instead of failing completely
      })
    })
  }
  
  return(anim)
}