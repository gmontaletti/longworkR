#' @title Example Usage for Ggraph-Based Transition Visualizations
#' @description
#' This file contains comprehensive examples demonstrating how to use the ggraph-based
#' visualization functions for employment transitions analysis. These examples showcase
#' all the main functions and their key parameters, along with accessibility features.
#'
#' @name ggraph-transitions-examples
NULL

#' Create Comprehensive Example of All Visualization Types
#'
#' This function demonstrates all four main visualization types provided by the
#' vecshift ggraph integration, showing how to create publication-ready network
#' visualizations with accessibility features.
#'
#' @param data Sample employment data (if NULL, generates synthetic data)
#' @param save_plots Logical. Whether to save plots to disk (default: FALSE)
#' @param output_dir Character. Directory to save plots (default: ".")
#'
#' @return List of ggplot objects showing different visualization types
#' @export
#'
#' @examples
#' \dontrun{
#' # Create all visualization examples
#' examples <- create_transitions_visualization_examples()
#' 
#' # Display the network plot
#' print(examples$network)
#' 
#' # Display the heatmap
#' print(examples$heatmap)
#' 
#' # Display the circular plot
#' print(examples$circular)
#' 
#' # Display the hierarchical plot
#' print(examples$hierarchical)
#' }
create_transitions_visualization_examples <- function(data = NULL,
                                                    save_plots = FALSE,
                                                    output_dir = ".") {
  
  # Generate or use provided data
  if (is.null(data)) {
    # Create sample employment data
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for example generation")
    }
    
    message("Generating sample employment data...")
    sample_data <- create_sample_employment_data(
      n_people = 30,
      n_periods = 4,
      date_range = c("2022-01-01", "2024-12-31"),
      include_overlaps = TRUE,
      include_gaps = TRUE,
      seed = 123
    )
    
    # Process through pipeline
    message("Processing data through vecshift pipeline...")
    pipeline_result <- process_employment_pipeline(
      original_data = sample_data,
      merge_columns = c("prior"),
      classify_status = TRUE
    )
    
    # Analyze transitions
    message("Analyzing employment transitions...")
    transitions <- analyze_employment_transitions(
      pipeline_result = pipeline_result,
      transition_variable = "stato",  # Use employment status
      min_unemployment_duration = 7,
      show_progress = FALSE
    )
  } else {
    transitions <- data
  }
  
  # Check if we have sufficient data
  if (nrow(transitions) < 2) {
    warning("Insufficient transition data for examples. Need at least 2 transition patterns.")
    return(list(message = "Insufficient data"))
  }
  
  message("Creating visualizations...")
  
  # 1. Network Visualization (Fruchterman-Reingold layout)
  message("  - Creating network visualization...")
  network_plot <- plot_transitions_network(
    transitions_data = transitions,
    layout = "fr",
    node_size_var = "strength",
    edge_width_var = "weight",
    node_color_var = "community",
    palette = "viridis",
    accessibility_mode = FALSE,
    show_labels = TRUE,
    label_repel = TRUE,
    title = "Employment Transitions Network",
    subtitle = "Fruchterman-Reingold layout showing employment state relationships"
  )
  
  # 2. Accessibility-focused network (high contrast)
  message("  - Creating accessibility-focused network...")
  network_accessible <- plot_transitions_network(
    transitions_data = transitions,
    layout = "kk",
    node_size_var = "degree",
    edge_width_var = "weight", 
    node_color_var = "fixed",
    palette = "viridis",
    accessibility_mode = TRUE,
    use_bw = TRUE,
    show_labels = TRUE,
    title = "Employment Transitions Network (Accessible)",
    subtitle = "High-contrast version optimized for accessibility"
  )
  
  # 3. Heatmap Visualization
  message("  - Creating heatmap visualization...")
  heatmap_plot <- plot_transitions_heatmap(
    transitions_data = transitions,
    cell_value = "both",
    normalize = "row",
    palette = "viridis",
    accessibility_mode = FALSE,
    title = "Employment Transitions Heatmap",
    subtitle = "Row-normalized transition probabilities with counts and percentages"
  )
  
  # 4. Heatmap with accessibility features
  message("  - Creating accessible heatmap...")
  heatmap_accessible <- plot_transitions_heatmap(
    transitions_data = transitions,
    cell_value = "weight",
    normalize = "none",
    palette = "viridis",
    accessibility_mode = TRUE,
    title = "Employment Transitions Heatmap (Accessible)",
    subtitle = "High-contrast heatmap showing raw transition counts"
  )
  
  # 5. Circular Visualization (Chord diagram)
  message("  - Creating circular visualization...")
  circular_plot <- plot_transitions_circular(
    transitions_data = transitions,
    circular_type = "chord",
    node_order = "frequency",
    show_flow_direction = TRUE,
    palette = "viridis",
    title = "Employment Transitions Chord Diagram",
    subtitle = "Circular layout emphasizing flow patterns between states"
  )
  
  # 6. Arc diagram version
  message("  - Creating arc diagram...")
  arc_plot <- plot_transitions_circular(
    transitions_data = transitions,
    circular_type = "arc",
    node_order = "alphabetical",
    show_flow_direction = TRUE,
    palette = "okabe_ito",
    accessibility_mode = FALSE,
    title = "Employment Transitions Arc Diagram",
    subtitle = "Linear arrangement with arced connections"
  )
  
  # 7. Hierarchical Visualization (Sugiyama layout)
  message("  - Creating hierarchical visualization...")
  hierarchical_plot <- plot_transitions_hierarchical(
    transitions_data = transitions,
    hierarchy_type = "sugiyama",
    layout_direction = "vertical",
    node_color_var = "level",
    palette = "viridis",
    show_levels = TRUE,
    title = "Employment Transitions Hierarchy",
    subtitle = "Sugiyama layout showing employment progression patterns"
  )
  
  # 8. Tree layout version
  message("  - Creating tree layout...")
  tree_plot <- plot_transitions_hierarchical(
    transitions_data = transitions,
    hierarchy_type = "tree",
    layout_direction = "horizontal",
    node_color_var = "degree",
    palette = "viridis",
    show_node_labels = TRUE,
    title = "Employment Transitions Tree",
    subtitle = "Tree layout showing hierarchical relationships"
  )
  
  # Compile results
  examples <- list(
    # Main visualization types
    network = network_plot,
    heatmap = heatmap_plot,
    circular = circular_plot,
    hierarchical = hierarchical_plot,
    
    # Accessibility versions
    network_accessible = network_accessible,
    heatmap_accessible = heatmap_accessible,
    
    # Alternative layouts
    arc_diagram = arc_plot,
    tree_layout = tree_plot,
    
    # Metadata
    data_summary = list(
      n_transitions = nrow(transitions),
      n_states = length(unique(c(transitions$from, transitions$to))),
      total_weight = sum(transitions$weight, na.rm = TRUE)
    )
  )
  
  # Save plots if requested
  if (save_plots) {
    message("Saving plots to disk...")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    plot_names <- c("network", "heatmap", "circular", "hierarchical", 
                   "network_accessible", "heatmap_accessible", "arc_diagram", "tree_layout")
    
    for (name in plot_names) {
      if (!is.null(examples[[name]])) {
        filename <- file.path(output_dir, paste0("transitions_", name, ".png"))
        ggplot2::ggsave(filename, examples[[name]], 
                       width = 10, height = 8, dpi = 300, bg = "white")
        message("  - Saved: ", filename)
      }
    }
  }
  
  message("Visualization examples completed successfully!")
  
  return(examples)
}

#' Demonstrate Accessibility Features
#'
#' Creates side-by-side comparisons of standard vs accessibility-optimized
#' visualizations to demonstrate the impact of accessibility features.
#'
#' @param transitions_data Data.table with transitions data
#'
#' @return List with standard and accessible versions of visualizations
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' sample_data <- create_sample_employment_data(n_people = 20)
#' pipeline_result <- process_employment_pipeline(sample_data)
#' transitions <- analyze_employment_transitions(pipeline_result)
#' 
#' # Create accessibility comparison
#' accessibility_demo <- demonstrate_accessibility_features(transitions)
#' 
#' # Display comparisons
#' print(accessibility_demo$standard_network)
#' print(accessibility_demo$accessible_network)
#' }
demonstrate_accessibility_features <- function(transitions_data) {
  
  message("Creating accessibility demonstration...")
  
  # Standard visualization
  standard_network <- plot_transitions_network(
    transitions_data,
    layout = "fr",
    palette = "main",  # Default vecshift palette
    accessibility_mode = FALSE,
    use_bw = FALSE,
    title = "Standard Network Visualization",
    subtitle = "Default colors and styling"
  )
  
  # Accessible visualization
  accessible_network <- plot_transitions_network(
    transitions_data,
    layout = "fr", 
    palette = "viridis",  # Colorblind-friendly
    accessibility_mode = TRUE,
    use_bw = TRUE,
    title = "Accessible Network Visualization", 
    subtitle = "High-contrast, colorblind-friendly design"
  )
  
  # Heatmap comparison
  standard_heatmap <- plot_transitions_heatmap(
    transitions_data,
    color_scale = "gradient",
    palette = "main",
    accessibility_mode = FALSE,
    title = "Standard Heatmap",
    subtitle = "Default color gradient"
  )
  
  accessible_heatmap <- plot_transitions_heatmap(
    transitions_data,
    color_scale = "gradient",
    palette = "viridis",
    accessibility_mode = TRUE,
    title = "Accessible Heatmap",
    subtitle = "High-contrast colorblind-friendly gradient"
  )
  
  # Generate accessibility report
  accessibility_report <- create_accessibility_report(
    transitions_data,
    layout = "fr",
    palette = "viridis"
  )
  
  return(list(
    standard_network = standard_network,
    accessible_network = accessible_network,
    standard_heatmap = standard_heatmap,
    accessible_heatmap = accessible_heatmap,
    accessibility_report = accessibility_report
  ))
}

#' Show Layout Algorithm Comparison
#'
#' Creates a comparison of different layout algorithms to help users
#' understand when to use each layout type.
#'
#' @param transitions_data Data.table with transitions data
#' @param layouts Character vector of layout names to compare
#'
#' @return List of plots showing different layout algorithms
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare different layouts
#' layout_comparison <- compare_layout_algorithms(transitions)
#' 
#' # Display different layouts
#' print(layout_comparison$fr)
#' print(layout_comparison$kk)
#' print(layout_comparison$circle)
#' }
compare_layout_algorithms <- function(transitions_data, 
                                    layouts = c("fr", "kk", "circle", "mds")) {
  
  message("Comparing layout algorithms...")
  
  results <- list()
  
  layout_descriptions <- list(
    fr = "Fruchterman-Reingold: Good for general networks, natural clustering",
    kk = "Kamada-Kawai: Preserves distances, ideal for smaller networks",
    circle = "Circular: Equal emphasis on all nodes, good for comparisons",
    mds = "Multidimensional Scaling: Preserves distance relationships",
    gem = "GEM Algorithm: Fast spring layout with good separation",
    graphopt = "Graphopt: Emphasizes community structure"
  )
  
  for (layout in layouts) {
    message("  - Creating ", layout, " layout...")
    
    plot <- plot_transitions_network(
      transitions_data,
      layout = layout,
      node_size_var = "strength",
      edge_width_var = "weight",
      node_color_var = "community",
      palette = "viridis",
      show_labels = TRUE,
      title = paste("Layout Comparison:", toupper(layout)),
      subtitle = layout_descriptions[[layout]] %||% paste("Layout algorithm:", layout)
    )
    
    results[[layout]] <- plot
  }
  
  message("Layout comparison completed!")
  
  return(results)
}

#' Best Practices Guide for Transition Visualizations
#'
#' Provides guidance on selecting the most appropriate visualization type
#' and parameters based on data characteristics and use case.
#'
#' @param transitions_data Data.table with transitions data
#' @param use_case Character. The intended use case: "exploration", "presentation", 
#'   "publication", "accessibility" (default: "exploration")
#'
#' @return List with recommended visualizations and explanations
#' @export
#'
#' @examples
#' \dontrun{
#' # Get visualization recommendations
#' recommendations <- get_visualization_recommendations(transitions, use_case = "publication")
#' 
#' # Display the recommended plot
#' print(recommendations$recommended_plot)
#' 
#' # View the explanation
#' cat(recommendations$explanation)
#' }
get_visualization_recommendations <- function(transitions_data, use_case = "exploration") {
  
  use_case <- match.arg(use_case, c("exploration", "presentation", "publication", "accessibility"))
  
  # Analyze data characteristics
  n_states <- length(unique(c(transitions_data$from, transitions_data$to)))
  n_transitions <- nrow(transitions_data)
  max_weight <- max(transitions_data$weight, na.rm = TRUE)
  density <- n_transitions / (n_states * (n_states - 1))
  
  message("Analyzing data characteristics...")
  message("  - States: ", n_states)
  message("  - Transitions: ", n_transitions)
  message("  - Density: ", round(density, 3))
  
  # Make recommendations based on characteristics and use case
  if (use_case == "exploration") {
    if (n_states <= 10 && density < 0.3) {
      recommended_viz <- "network"
      recommended_params <- list(layout = "fr", accessibility_mode = FALSE, show_labels = TRUE)
      explanation <- "Network visualization with Fruchterman-Reingold layout is ideal for exploring relationships in small to medium networks."
    } else if (n_states <= 20) {
      recommended_viz <- "circular"
      recommended_params <- list(circular_type = "chord", node_order = "frequency", show_flow_direction = TRUE)
      explanation <- "Circular chord diagram effectively shows flow patterns in medium-sized networks."
    } else {
      recommended_viz <- "heatmap"
      recommended_params <- list(normalize = "row", cell_value = "both", accessibility_mode = FALSE)
      explanation <- "Heatmap visualization scales well to larger numbers of states and provides clear quantitative information."
    }
  } else if (use_case == "presentation") {
    if (n_states <= 8) {
      recommended_viz <- "network"
      recommended_params <- list(layout = "kk", node_size_var = "strength", show_labels = TRUE, 
                                palette = "viridis")
      explanation <- "Clean network layout with clear labeling works well for presentations."
    } else {
      recommended_viz <- "circular"
      recommended_params <- list(circular_type = "arc", node_order = "frequency", 
                                accessibility_mode = FALSE)
      explanation <- "Arc diagram provides clear visual impact while remaining readable for presentations."
    }
  } else if (use_case == "publication") {
    if (density < 0.2) {
      recommended_viz <- "hierarchical"
      recommended_params <- list(hierarchy_type = "sugiyama", show_levels = TRUE, 
                                accessibility_mode = FALSE)
      explanation <- "Hierarchical layout provides clear structure suitable for academic publications."
    } else {
      recommended_viz <- "heatmap"
      recommended_params <- list(normalize = "row", cell_value = "weight", 
                                color_scale = "gradient", accessibility_mode = FALSE)
      explanation <- "Heatmap provides precise quantitative information preferred in academic contexts."
    }
  } else { # accessibility
    if (n_states <= 12) {
      recommended_viz <- "network"
      recommended_params <- list(layout = "circle", accessibility_mode = TRUE, use_bw = TRUE,
                                palette = "viridis", show_labels = TRUE)
      explanation <- "Circular network layout with high-contrast colors ensures accessibility compliance."
    } else {
      recommended_viz <- "heatmap"
      recommended_params <- list(normalize = "none", cell_value = "weight",
                                accessibility_mode = TRUE, text_color = "auto")
      explanation <- "High-contrast heatmap with clear text labels maximizes accessibility."
    }
  }
  
  message("Recommendation: ", recommended_viz, " visualization")
  
  # Create the recommended plot
  if (recommended_viz == "network") {
    plot <- do.call(plot_transitions_network, c(list(transitions_data = transitions_data), 
                                               recommended_params))
  } else if (recommended_viz == "heatmap") {
    plot <- do.call(plot_transitions_heatmap, c(list(transitions_data = transitions_data), 
                                               recommended_params))
  } else if (recommended_viz == "circular") {
    plot <- do.call(plot_transitions_circular, c(list(transitions_data = transitions_data), 
                                                recommended_params))
  } else if (recommended_viz == "hierarchical") {
    plot <- do.call(plot_transitions_hierarchical, c(list(transitions_data = transitions_data), 
                                                    recommended_params))
  }
  
  return(list(
    recommended_visualization = recommended_viz,
    recommended_parameters = recommended_params,
    recommended_plot = plot,
    explanation = explanation,
    data_characteristics = list(
      n_states = n_states,
      n_transitions = n_transitions,
      density = density,
      max_weight = max_weight
    ),
    alternative_options = .get_alternative_recommendations(n_states, density, use_case)
  ))
}

#' Internal helper to get alternative visualization recommendations
#' @keywords internal
.get_alternative_recommendations <- function(n_states, density, use_case) {
  
  alternatives <- list()
  
  if (n_states <= 15 && density < 0.25) {
    alternatives$network_fr <- "Network with Fruchterman-Reingold layout for natural clustering"
    alternatives$network_kk <- "Network with Kamada-Kawai layout for distance preservation"
  }
  
  if (n_states >= 8) {
    alternatives$heatmap_normalized <- "Row-normalized heatmap for probability visualization"
    alternatives$heatmap_raw <- "Raw count heatmap for absolute frequency analysis"
  }
  
  if (n_states <= 20) {
    alternatives$circular_chord <- "Chord diagram for flow emphasis"
    alternatives$circular_arc <- "Arc diagram for linear arrangement"
  }
  
  if (density < 0.15 && n_states >= 6) {
    alternatives$hierarchical_sugiyama <- "Sugiyama layout for directed flow visualization"
    alternatives$hierarchical_tree <- "Tree layout for hierarchical relationships"
  }
  
  return(alternatives)
}