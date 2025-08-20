#' Visualization Functions for Contract Survival Analysis
#'
#' @description
#' This module provides comprehensive visualization capabilities for survival
#' analysis results, including Kaplan-Meier curves, risk tables, and
#' comparative plots across contract types.
#'
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon geom_point labs theme_minimal scale_fill_gradient scale_fill_gradient2 scale_fill_viridis_c
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
      
      p <- p + scale_color_manual(values = colors) +
               scale_fill_manual(values = colors)
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
#' @param comparison_results List output from compare_contract_survival()
#' @param highlight_significant Logical. Highlight significant differences
#' @param show_pvalues Logical. Display p-values on plot
#' @param reference_group Character. Reference contract type for comparisons
#'
#' @return A ggplot object with comparative survival curves
#'
#' @export
plot_survival_comparison <- function(
    data,
    survival_curves,
    comparison_results = NULL,
    highlight_significant = TRUE,
    show_pvalues = TRUE,
    reference_group = NULL
) {
  
  # Create base survival plot
  p <- visualize_contract_survival(
    survival_curves = survival_curves,
    show_confidence = TRUE,
    show_median = TRUE,
    title = "Contract Survival Comparison",
    subtitle = if (!is.null(comparison_results)) {
      sprintf("Log-rank p-value: %.4f", 
              1 - pchisq(comparison_results$test_results$logrank$chisq, 
                        length(unique(names(survival_curves$survival_tables))) - 1))
    } else NULL
  )
  
  # Add statistical annotations if available
  if (!is.null(comparison_results) && show_pvalues) {
    # Extract p-value
    if (!is.null(comparison_results$test_results$logrank)) {
      p_value <- 1 - pchisq(
        comparison_results$test_results$logrank$chisq,
        df = length(unique(names(survival_curves$survival_tables))) - 1
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
    
    forest_data <- rbind(forest_data, data.table(
      contract_type = ct,
      median = survival_curves$median_survival[ct],
      lower_ci = if (!is.null(km_fit$lower)) km_fit$lower else NA,
      upper_ci = if (!is.null(km_fit$upper)) km_fit$upper else NA,
      n = km_fit$n
    ))
  }
  
  # Order data
  if (order_by == "median") {
    forest_data <- forest_data[order(median)]
  } else if (order_by == "alphabetical") {
    forest_data <- forest_data[order(contract_type)]
  } else if (order_by == "custom" && !is.null(custom_order)) {
    forest_data[, contract_type := factor(contract_type, levels = custom_order)]
    forest_data <- forest_data[order(contract_type)]
  }
  
  # Create forest plot
  p <- ggplot(forest_data, aes(y = reorder(contract_type, median))) +
    geom_point(aes(x = median), size = 3) +
    geom_errorbarh(
      aes(xmin = lower_ci, xmax = upper_ci),
      height = 0.2
    ) +
    labs(
      title = "Median Survival Times by Contract Type",
      x = "Median Survival Time (days)",
      y = "Contract Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "gray90")
    )
  
  # Add sample sizes if requested
  if (show_n) {
    forest_data[, label := sprintf("%s (n=%d)", contract_type, n)]
    p <- p + scale_y_discrete(labels = forest_data$label)
  }
  
  # Add reference line at overall median
  overall_median <- median(forest_data$median, na.rm = TRUE)
  p <- p + geom_vline(
    xintercept = overall_median,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  )
  
  return(p)
}

#' Create Animated Survival Curves
#'
#' @description
#' Creates an animated visualization of survival curves over time using gganimate.
#'
#' @param survival_curves List output from estimate_contract_survival()
#' @param animation_speed Numeric. Speed of animation in fps
#' @param save_as Character. File path to save animation (NULL to display only)
#'
#' @return An animated ggplot object or saved file path
#'
#' @export
animate_survival_curves <- function(
    survival_curves,
    animation_speed = 10,
    save_as = NULL
) {
  
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop("Package 'gganimate' is required for animations")
  }
  
  # Prepare animation data
  anim_data <- data.table()
  
  for (ct in names(survival_curves$survival_tables)) {
    surv_table <- survival_curves$survival_tables[[ct]]
    
    # Create cumulative data for animation
    for (i in 1:nrow(surv_table)) {
      anim_data <- rbind(anim_data, data.table(
        contract_type = ct,
        time = surv_table$time[1:i],
        survival_prob = surv_table$survival_prob[1:i],
        frame = i
      ))
    }
  }
  
  # Create animated plot
  p <- ggplot(anim_data, aes(x = time, y = survival_prob, color = contract_type)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    labs(
      title = "Survival Curves Animation",
      subtitle = "Time: {frame_time}",
      x = "Time (days)",
      y = "Survival Probability"
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 1)) +
    gganimate::transition_reveal(frame)
  
  # Animate
  anim <- gganimate::animate(
    p,
    fps = animation_speed,
    nframes = max(anim_data$frame),
    width = 800,
    height = 600
  )
  
  # Save if requested
  if (!is.null(save_as)) {
    gganimate::anim_save(save_as, animation = anim)
    return(invisible(save_as))
  }
  
  return(anim)
}