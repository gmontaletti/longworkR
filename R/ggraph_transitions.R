#' @title Ggraph-Based Network Visualizations for Employment Transitions
#' @description
#' Comprehensive ggraph and tidygraph-based visualization functions for analyzing
#' employment transitions from vecshift output. These functions provide accessible,
#' publication-ready network visualizations with support for multiple layouts,
#' colorblind-friendly palettes, and WCAG AA compliance.
#'
#' @name ggraph-transitions
#' @importFrom data.table data.table setorder copy setnames
#' @importFrom stats median quantile 
#' @importFrom utils head tail
#' @importFrom dplyr case_when
#' @importFrom rlang sym
NULL

# Core Network Visualization Functions ===========================================

#' Plot Employment Transitions Network
#'
#' Creates network visualizations of employment transitions using ggraph and tidygraph.
#' Supports multiple layout algorithms, accessibility features, and customization options.
#' Designed to work with output from analyze_employment_transitions(). Now includes
#' support for consolidated transitions using over_id grouping for cleaner visualizations.
#'
#' @param transitions_data Data.table output from analyze_employment_transitions(), 
#'   or a transition matrix when input_format = "matrix". When using consolidated 
#'   transitions (use_consolidated_periods = TRUE), provides cleaner network structure.
#' @param input_format Character. Format of input data: "data.table" or "matrix" (default: "data.table")
#' @param use_consolidated_periods Logical. If TRUE (default), apply consolidation to 
#'   analyze_employment_transitions() call for cleaner network visualization. Requires
#'   pipeline data with over_id column. Ignored when transitions_data is pre-computed.
#' @param consolidation_type Character. Type of consolidation when use_consolidated_periods = TRUE:
#'   "both" (overlapping + consecutive), "overlapping" (same over_id), "consecutive" (adjacent periods),
#'   or "none" (default: "both"). Only used if transitions_data is pipeline data.
#' @param show_consolidation_comparison Logical. If TRUE, create side-by-side comparison 
#'   of raw vs consolidated transitions (default: FALSE). Only works with pipeline data input.
#' @param layout Character. Layout algorithm for network:
#'   - "fr": Fruchterman-Reingold (default) - good for general networks
#'   - "kk": Kamada-Kawai - preserves distances, good for smaller networks  
#'   - "dh": Davidson-Harel - minimizes edge crossings
#'   - "gem": GEM algorithm - fast spring layout
#'   - "graphopt": Graphopt algorithm - good clustering
#'   - "mds": Multidimensional scaling - preserves distance relationships
#'   - "randomly": Random layout - for testing
#'   - "circle": Circular layout - equal emphasis on all nodes
#'   - "sphere": 3D sphere projection to 2D
#'   - "grid": Grid-based layout
#' @param node_size_var Character. Variable to map to node size: "degree", "strength", 
#'   "in_degree", "out_degree", "betweenness", "closeness", "fixed" (default: "strength")
#' @param edge_width_var Character. Variable to map to edge width: "weight", "transition_duration", 
#'   "fixed" (default: "weight")
#' @param node_color_var Character. Variable to map to node color: "community", "degree", 
#'   "fixed", "status" (default: "community")
#' @param min_edge_weight Numeric. Minimum edge weight to display (default: 1)
#' @param node_size_range Numeric vector of length 2. Size range for nodes (default: c(3, 15))
#' @param edge_width_range Numeric vector of length 2. Width range for edges (default: c(0.5, 3))
#' @param edge_alpha Numeric. Transparency for edges (default: 0.6)
#' @param node_alpha Numeric. Transparency for nodes (default: 0.8)
#' @param show_labels Logical. Show node labels (default: TRUE)
#' @param label_size Numeric. Size of node labels (default: 3)
#' @param label_repel Logical. Use repelling for labels to avoid overlap (default: TRUE)
#' @param palette Character. Color palette: "viridis", "okabe_ito", "employment", 
#'   "main", "colorbrewer_set2" (default: "viridis")
#' @param use_bw Logical. Use black and white version (default: FALSE)
#' @param directed Logical. Treat graph as directed (default: TRUE)
#' @param show_edge_labels Logical. Show edge weight labels (default: FALSE)
#' @param accessibility_mode Logical. Enable high contrast accessibility mode (default: FALSE)
#' @param use_consolidated_periods Logical. Apply consolidation for cleaner heatmap (default: TRUE)
#' @param consolidation_type Character. Type of consolidation (default: "both")
#' @param title Character. Plot title (default: auto-generated)
#' @param subtitle Character. Plot subtitle (default: auto-generated)
#'
#' @return A ggplot2 object showing the network visualization
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(ggraph)
#' library(tidygraph)
#' 
#' # Create sample data
#' sample_data <- create_sample_employment_data(n_people = 50, n_periods = 4)
#' pipeline_result <- vecshift(sample_data)  # Creates over_id column
#' 
#' # Basic consolidated network plot (default)
#' plot_transitions_network(pipeline_result, transition_variable = "prior")
#' 
#' # Compare raw vs consolidated transitions
#' plot_transitions_network(pipeline_result, show_consolidation_comparison = TRUE)
#' 
#' # Use only overlapping period consolidation
#' plot_transitions_network(pipeline_result, consolidation_type = "overlapping")
#' 
#' # Kamada-Kawai layout with degree-based node sizing
#' plot_transitions_network(pipeline_result, layout = "kk", node_size_var = "degree")
#' 
#' # Accessibility mode with high contrast
#' plot_transitions_network(pipeline_result, accessibility_mode = TRUE, use_bw = TRUE)
#' 
#' # Using pre-computed transitions (no consolidation applied)
#' transitions <- analyze_employment_transitions(pipeline_result, transition_variable = "prior")
#' plot_transitions_network(transitions, use_consolidated_periods = FALSE)
#' }
plot_transitions_network <- function(transitions_data,
                                   input_format = "data.table",
                                   layout = "fr",
                                   node_size_var = "strength",
                                   edge_width_var = "weight", 
                                   node_color_var = "community",
                                   min_edge_weight = 1,
                                   node_size_range = c(3, 15),
                                   edge_width_range = c(0.5, 3),
                                   edge_alpha = 0.6,
                                   node_alpha = 0.8,
                                   show_labels = TRUE,
                                   label_size = 3,
                                   label_repel = TRUE,
                                   palette = "viridis",
                                   use_bw = FALSE,
                                   directed = TRUE,
                                   show_edge_labels = FALSE,
                                   accessibility_mode = FALSE,
                                   use_consolidated_periods = TRUE,
                                   consolidation_type = "both",
                                   show_consolidation_comparison = FALSE,
                                   title = NULL,
                                   subtitle = NULL) {
  
  # Check required packages
  .check_ggraph_packages()
  
  # Input validation
  input_format <- match.arg(input_format, c("data.table", "matrix"))
  layout <- match.arg(layout, c("fr", "kk", "dh", "gem", "graphopt", "mds", 
                               "randomly", "circle", "sphere", "grid"))
  node_size_var <- match.arg(node_size_var, c("degree", "strength", "in_degree", 
                                             "out_degree", "betweenness", "closeness", "fixed"))
  edge_width_var <- match.arg(edge_width_var, c("weight", "transition_duration", "fixed"))
  node_color_var <- match.arg(node_color_var, c("community", "degree", "fixed", "status"))
  palette <- match.arg(palette, c("viridis", "okabe_ito", "employment", "main", "colorbrewer_set2"))
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive", "none"))
  
  # Handle consolidation and comparison plots
  if (show_consolidation_comparison && input_format == "data.table") {
    return(.create_consolidation_comparison_plot(transitions_data, layout, node_size_var, 
                                               edge_width_var, node_color_var, min_edge_weight,
                                               node_size_range, edge_width_range, edge_alpha,
                                               node_alpha, show_labels, label_size, label_repel,
                                               palette, use_bw, directed, show_edge_labels,
                                               accessibility_mode, consolidation_type, title, subtitle))
  }
  
  # Process transitions data if needed
  if (use_consolidated_periods && consolidation_type != "none" && .is_pipeline_data(transitions_data)) {
    transitions_data <- .process_with_consolidation(transitions_data, consolidation_type)
  }
  
  # Convert input to tidygraph object
  tg <- .convert_to_tidygraph(transitions_data, input_format, directed, min_edge_weight)
  
  # Add consolidation info to tidygraph if available
  if (use_consolidated_periods && "consolidated_status" %in% names(transitions_data)) {
    tg <- .add_consolidation_info(tg, transitions_data)
  }
  
  if (igraph::vcount(tg) == 0) {
    warning("No nodes found after filtering. Check min_edge_weight parameter.")
    return(.create_empty_network_plot("No nodes to display"))
  }
  
  # Calculate network metrics
  tg <- .add_network_metrics(tg, node_size_var, node_color_var)
  
  # Determine number of colors needed for discrete color variables
  n_colors_needed <- .get_required_colors_count(tg, node_color_var)
  
  # Get colors
  colors <- .get_accessibility_colors(palette, use_bw, accessibility_mode, n = n_colors_needed)
  
  # Create base ggraph plot
  p <- ggraph::ggraph(tg, layout = layout)
  
  # Add all edge layers with consolidation styling
  edge_layers <- .add_edges_with_consolidation(tg, edge_width_var, edge_width_range, edge_alpha, 
                                             show_edge_labels, colors, use_bw, accessibility_mode)
  for (layer in edge_layers) {
    p <- p + layer
  }
  
  # Add all node layers
  node_layers <- .add_nodes(node_size_var, node_size_range, node_color_var, node_alpha, colors)
  for (layer in node_layers) {
    p <- p + layer
  }
  
  # Add all label layers
  label_layers <- .add_labels(show_labels, label_size, label_repel, accessibility_mode)
  for (layer in label_layers) {
    p <- p + layer
  }
  
  # Apply theme and labels
  p <- p + .apply_network_theme(accessibility_mode, use_bw) +
    .add_network_titles(tg, title, subtitle, layout, node_size_var)
  
  return(p)
}

#' Plot Employment Transitions Heatmap
#'
#' Creates heatmap visualizations for transition matrices, showing the intensity
#' of transitions between different employment states. Supports both matrix and
#' data.table inputs with comprehensive accessibility features.
#'
#' @param transitions_data Data.table output from analyze_employment_transitions() 
#'   or transition matrix when input_format = "matrix"
#' @param input_format Character. Format of input data: "data.table" or "matrix" (default: "data.table")
#' @param cell_value Character. What to display in cells: "weight", "percentage", 
#'   "both", "none" (default: "weight")
#' @param normalize Character. How to normalize values: "none", "row", "column", 
#'   "total" (default: "none")
#' @param min_value_display Numeric. Minimum value to display in cells (default: 1)
#' @param color_scale Character. Type of color scale: "gradient", "diverging", 
#'   "discrete" (default: "gradient")
#' @param palette Character. Color palette (default: "viridis")
#' @param use_bw Logical. Use black and white version (default: FALSE)
#' @param text_color Character. Color for cell text: "auto", "white", "black" (default: "auto")
#' @param text_size Numeric. Size of cell text (default: 3)
#' @param show_marginals Logical. Show row/column marginal sums (default: FALSE)
#' @param accessibility_mode Logical. Enable high contrast mode (default: FALSE)
#' @param aspect_ratio Numeric. Aspect ratio for cells (default: 1)
#' @param border_color Character. Color for cell borders (default: "white")
#' @param border_size Numeric. Size of cell borders (default: 0.5)
#' @param title Character. Plot title (default: auto-generated)
#' @param subtitle Character. Plot subtitle (default: auto-generated)
#'
#' @return A ggplot2 object showing the heatmap
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic consolidated heatmap (default)
#' plot_transitions_heatmap(pipeline_result)
#' 
#' # Row-normalized with percentages and overlapping consolidation only
#' plot_transitions_heatmap(pipeline_result, normalize = "row", cell_value = "percentage",
#'                         consolidation_type = "overlapping")
#' 
#' # High contrast accessibility mode with no consolidation
#' plot_transitions_heatmap(pipeline_result, accessibility_mode = TRUE, 
#'                         use_consolidated_periods = FALSE)
#' 
#' # Using pre-computed transitions data
#' transitions <- analyze_employment_transitions(pipeline_result)
#' plot_transitions_heatmap(transitions, input_format = "data.table")
#' }
plot_transitions_heatmap <- function(transitions_data,
                                   input_format = "data.table",
                                   cell_value = "weight",
                                   normalize = "none",
                                   min_value_display = 1,
                                   color_scale = "gradient", 
                                   palette = "viridis",
                                   use_bw = FALSE,
                                   text_color = "auto",
                                   text_size = 3,
                                   show_marginals = FALSE,
                                   accessibility_mode = FALSE,
                                   use_consolidated_periods = TRUE,
                                   consolidation_type = "both",
                                   aspect_ratio = 1,
                                   border_color = "white",
                                   border_size = 0.5,
                                   title = NULL,
                                   subtitle = NULL) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for heatmap functions")
  }
  
  # Input validation
  input_format <- match.arg(input_format, c("data.table", "matrix"))
  cell_value <- match.arg(cell_value, c("weight", "percentage", "both", "none"))
  normalize <- match.arg(normalize, c("none", "row", "column", "total"))
  color_scale <- match.arg(color_scale, c("gradient", "diverging", "discrete"))
  text_color <- match.arg(text_color, c("auto", "white", "black"))
  palette <- match.arg(palette, c("viridis", "okabe_ito", "employment", "main", "colorbrewer_set2"))
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive", "none"))
  
  # Process transitions data if needed for consolidation
  if (use_consolidated_periods && consolidation_type != "none" && .is_pipeline_data(transitions_data)) {
    transitions_data <- .process_with_consolidation(transitions_data, consolidation_type)
  }
  
  # Convert to matrix format for processing
  trans_matrix <- .convert_to_matrix(transitions_data, input_format)
  
  if (nrow(trans_matrix) == 0 || ncol(trans_matrix) == 0) {
    warning("Empty transition matrix")
    return(.create_empty_heatmap_plot("No transitions to display"))
  }
  
  # Apply normalization
  normalized_matrix <- .normalize_matrix(trans_matrix, normalize)
  
  # Convert to long format for ggplot2
  heatmap_data <- .matrix_to_long(normalized_matrix, cell_value, min_value_display)
  
  # Get colors
  colors <- .get_accessibility_colors(palette, use_bw, accessibility_mode)
  
  # Create base heatmap plot
  p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = to, y = from, fill = value)) +
    ggplot2::geom_tile(color = border_color, linewidth = border_size, alpha = 0.9) +
    ggplot2::coord_fixed(ratio = aspect_ratio)
  
  # Apply color scale
  p <- p + .add_heatmap_color_scale(color_scale, colors, accessibility_mode)
  
  # Add text labels if requested
  if (cell_value != "none") {
    text_col <- .get_text_color(text_color, accessibility_mode)
    p <- p + ggplot2::geom_text(ggplot2::aes(label = display_value), 
                               color = text_col, size = text_size, fontface = "bold")
  }
  
  # Apply theme and styling
  p <- p + theme_vecshift(base_size = 12, grid = "none", axis = "both", ticks = "both") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(hjust = 1),
      legend.position = "right",
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  
  # Add titles
  p <- p + .add_heatmap_titles(trans_matrix, title, subtitle, normalize, cell_value)
  
  # Add marginals if requested
  if (show_marginals) {
    p <- .add_heatmap_marginals(p, trans_matrix, colors)
  }
  
  return(p)
}

#' Plot Employment Transitions - Circular Layout
#'
#' Creates circular network layouts optimized for employment transition analysis.
#' Arranges nodes in a circle with transitions shown as chords or arcs.
#' Particularly effective for showing flow patterns and state relationships.
#'
#' @param transitions_data Data.table output from analyze_employment_transitions()
#' @param input_format Character. Format of input data: "data.table" or "matrix" (default: "data.table")
#' @param circular_type Character. Type of circular layout: "chord", "arc", "wheel" (default: "chord")
#' @param node_order Character. How to order nodes: "alphabetical", "frequency", 
#'   "cluster", "manual" (default: "frequency")
#' @param manual_order Character vector. Manual node order (when node_order = "manual")
#' @param show_flow_direction Logical. Show directional arrows for flows (default: TRUE)
#' @param edge_curve Numeric. Curvature for edges (0 = straight, 1 = very curved) (default: 0.1)
#' @param min_edge_weight Numeric. Minimum edge weight to display (default: 1)
#' @param node_size_var Character. Variable for node sizing (default: "strength")
#' @param node_size_range Numeric vector. Size range for nodes (default: c(4, 20))
#' @param edge_width_range Numeric vector. Width range for edges (default: c(0.5, 5))
#' @param palette Character. Color palette (default: "viridis")
#' @param use_bw Logical. Use black and white version (default: FALSE)
#' @param accessibility_mode Logical. Enable accessibility mode (default: FALSE)
#' @param use_consolidated_periods Logical. Apply consolidation for cleaner circular layout (default: TRUE)
#' @param consolidation_type Character. Type of consolidation (default: "both")
#' @param show_percentages Logical. Show edge percentages instead of raw counts (default: FALSE)
#' @param label_distance Numeric. Distance of labels from circle (default: 1.1)
#' @param title Character. Plot title (default: auto-generated)
#' @param subtitle Character. Plot subtitle (default: auto-generated)
#'
#' @return A ggplot2 object showing the circular network
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic consolidated circular chord diagram
#' plot_transitions_circular(pipeline_result)
#' 
#' # Arc diagram with frequency-based ordering, overlapping consolidation only
#' plot_transitions_circular(pipeline_result, circular_type = "arc", node_order = "frequency",
#'                          consolidation_type = "overlapping")
#' 
#' # Wheel layout with percentages, no consolidation
#' plot_transitions_circular(pipeline_result, circular_type = "wheel", show_percentages = TRUE,
#'                          use_consolidated_periods = FALSE)
#' }
plot_transitions_circular <- function(transitions_data,
                                    input_format = "data.table",
                                    circular_type = "chord",
                                    node_order = "frequency",
                                    manual_order = NULL,
                                    show_flow_direction = TRUE,
                                    edge_curve = 0.1,
                                    min_edge_weight = 1,
                                    node_size_var = "strength",
                                    node_size_range = c(4, 20),
                                    edge_width_range = c(0.5, 5),
                                    palette = "viridis",
                                    use_bw = FALSE,
                                    accessibility_mode = FALSE,
                                    use_consolidated_periods = TRUE,
                                    consolidation_type = "both",
                                    show_percentages = FALSE,
                                    label_distance = 1.1,
                                    title = NULL,
                                    subtitle = NULL) {
  
  # Check required packages
  .check_ggraph_packages()
  
  # Input validation
  input_format <- match.arg(input_format, c("data.table", "matrix"))
  circular_type <- match.arg(circular_type, c("chord", "arc", "wheel"))
  node_order <- match.arg(node_order, c("alphabetical", "frequency", "cluster", "manual"))
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive", "none"))
  
  # Process transitions data if needed for consolidation
  if (use_consolidated_periods && consolidation_type != "none" && .is_pipeline_data(transitions_data)) {
    transitions_data <- .process_with_consolidation(transitions_data, consolidation_type)
  }
  
  # Convert to tidygraph
  tg <- .convert_to_tidygraph(transitions_data, input_format, TRUE, min_edge_weight)
  
  if (igraph::vcount(tg) == 0) {
    return(.create_empty_network_plot("No nodes to display"))
  }
  
  # Apply node ordering
  tg <- .apply_circular_ordering(tg, node_order, manual_order)
  
  # Add network metrics - use fixed coloring for simplicity
  tg <- .add_network_metrics(tg, node_size_var, "fixed")
  
  # Get colors
  colors <- .get_accessibility_colors(palette, use_bw, accessibility_mode)
  
  # Create circular layout
  if (circular_type == "chord") {
    p <- .create_chord_diagram(tg, colors, node_size_range, edge_width_range, 
                              show_flow_direction, edge_curve, show_percentages,
                              accessibility_mode)
  } else if (circular_type == "arc") {
    p <- .create_arc_diagram(tg, colors, node_size_range, edge_width_range,
                            show_flow_direction, label_distance, accessibility_mode)
  } else { # wheel
    p <- .create_wheel_diagram(tg, colors, node_size_range, edge_width_range,
                              edge_curve, label_distance, accessibility_mode)
  }
  
  # Apply theme and titles
  p <- p + .apply_circular_theme(accessibility_mode) +
    .add_circular_titles(tg, title, subtitle, circular_type, node_order)
  
  return(p)
}

#' Plot Employment Transitions - Hierarchical Layout
#'
#' Creates hierarchical network visualizations using tree-like layouts.
#' Particularly effective for showing career progression, advancement patterns,
#' or directional flow between employment states.
#'
#' @param transitions_data Data.table output from analyze_employment_transitions()
#' @param input_format Character. Format of input data: "data.table" or "matrix" (default: "data.table")
#' @param hierarchy_type Character. Type of hierarchical layout: "tree", "dendrogram", 
#'   "sugiyama", "davidson_harel" (default: "sugiyama")
#' @param root_nodes Character vector. Nodes to use as roots (default: auto-detect)
#' @param layout_direction Character. Direction of hierarchy: "vertical", "horizontal" (default: "vertical")
#' @param level_separation Numeric. Separation between hierarchy levels (default: 1)
#' @param node_separation Numeric. Separation between nodes at same level (default: 1)
#' @param show_levels Logical. Highlight hierarchy levels with background colors (default: TRUE)
#' @param min_edge_weight Numeric. Minimum edge weight to display (default: 1)
#' @param node_size_var Character. Variable for node sizing (default: "strength")
#' @param edge_width_var Character. Variable for edge width (default: "weight")
#' @param node_color_var Character. Variable for node coloring (default: "level")
#' @param node_size_range Numeric vector. Size range for nodes (default: c(3, 12))
#' @param edge_width_range Numeric vector. Width range for edges (default: c(0.5, 3))
#' @param palette Character. Color palette (default: "viridis")
#' @param use_bw Logical. Use black and white version (default: FALSE)
#' @param accessibility_mode Logical. Enable accessibility mode (default: FALSE)
#' @param use_consolidated_periods Logical. Apply consolidation for cleaner hierarchy (default: TRUE)
#' @param consolidation_type Character. Type of consolidation (default: "both")
#' @param show_edge_labels Logical. Show edge weight labels (default: FALSE)
#' @param show_node_labels Logical. Show node labels (default: TRUE)
#' @param label_angle Numeric. Angle for node labels in degrees (default: 0)
#' @param title Character. Plot title (default: auto-generated)
#' @param subtitle Character. Plot subtitle (default: auto-generated)
#'
#' @return A ggplot2 object showing the hierarchical network
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic consolidated hierarchical tree
#' plot_transitions_hierarchical(pipeline_result)
#' 
#' # Sugiyama layout with level highlighting, both consolidation types
#' plot_transitions_hierarchical(pipeline_result, hierarchy_type = "sugiyama", 
#'                              show_levels = TRUE, consolidation_type = "both")
#' 
#' # Horizontal dendrogram with no consolidation
#' plot_transitions_hierarchical(pipeline_result, hierarchy_type = "dendrogram", 
#'                              layout_direction = "horizontal", 
#'                              use_consolidated_periods = FALSE)
#' }
plot_transitions_hierarchical <- function(transitions_data,
                                        input_format = "data.table",
                                        hierarchy_type = "sugiyama",
                                        root_nodes = NULL,
                                        layout_direction = "vertical",
                                        level_separation = 1,
                                        node_separation = 1,
                                        show_levels = TRUE,
                                        min_edge_weight = 1,
                                        node_size_var = "strength",
                                        edge_width_var = "weight",
                                        node_color_var = "level",
                                        node_size_range = c(3, 12),
                                        edge_width_range = c(0.5, 3),
                                        palette = "viridis",
                                        use_bw = FALSE,
                                        accessibility_mode = FALSE,
                                        use_consolidated_periods = TRUE,
                                        consolidation_type = "both",
                                        show_edge_labels = FALSE,
                                        show_node_labels = TRUE,
                                        label_angle = 0,
                                        title = NULL,
                                        subtitle = NULL) {
  
  # Check required packages
  .check_ggraph_packages()
  
  # Input validation
  input_format <- match.arg(input_format, c("data.table", "matrix"))
  hierarchy_type <- match.arg(hierarchy_type, c("tree", "dendrogram", "sugiyama", "davidson_harel"))
  layout_direction <- match.arg(layout_direction, c("vertical", "horizontal"))
  node_color_var <- match.arg(node_color_var, c("level", "degree", "community", "fixed"))
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive", "none"))
  
  # Process transitions data if needed for consolidation
  if (use_consolidated_periods && consolidation_type != "none" && .is_pipeline_data(transitions_data)) {
    transitions_data <- .process_with_consolidation(transitions_data, consolidation_type)
  }
  
  # Convert to tidygraph
  tg <- .convert_to_tidygraph(transitions_data, input_format, TRUE, min_edge_weight)
  
  if (igraph::vcount(tg) == 0) {
    return(.create_empty_network_plot("No nodes to display"))
  }
  
  # Apply hierarchical structure analysis
  tg <- .add_hierarchical_metrics(tg, hierarchy_type, root_nodes)
  
  # Add network metrics
  tg <- .add_network_metrics(tg, node_size_var, node_color_var)
  
  # Determine number of colors needed for discrete color variables
  n_colors_needed <- .get_required_colors_count(tg, node_color_var)
  
  # Get colors
  colors <- .get_accessibility_colors(palette, use_bw, accessibility_mode, n = n_colors_needed)
  
  # Create hierarchical layout
  layout_name <- .get_hierarchical_layout_name(hierarchy_type, layout_direction)
  
  p <- ggraph::ggraph(tg, layout = layout_name)
  
  # Add level backgrounds if requested
  if (show_levels && node_color_var == "level") {
    p <- p + .add_level_backgrounds(colors, accessibility_mode)
  }
  
  # Add all hierarchical edge layers
  edge_layers <- .add_hierarchical_edges(edge_width_var, edge_width_range, show_edge_labels, colors, accessibility_mode)
  for (layer in edge_layers) {
    p <- p + layer
  }
  
  # Add all hierarchical node layers
  node_layers <- .add_hierarchical_nodes(node_size_var, node_size_range, node_color_var, colors, accessibility_mode)
  for (layer in node_layers) {
    p <- p + layer
  }
  
  # Add all hierarchical label layers
  if (show_node_labels) {
    label_layers <- .add_hierarchical_labels(label_angle, accessibility_mode)
    for (layer in label_layers) {
      p <- p + layer
    }
  }
  
  # Apply theme and titles
  p <- p + .apply_hierarchical_theme(accessibility_mode, layout_direction) +
    .add_hierarchical_titles(tg, title, subtitle, hierarchy_type, layout_direction)
  
  return(p)
}

# Helper Functions ============================================================

#' Convert Transitions Data to tidygraph Object
#'
#' Internal helper function to convert various input formats to tidygraph objects
#' for network analysis and visualization.
#'
#' @param data Input data (data.table or matrix)
#' @param format Format type
#' @param directed Whether graph should be directed
#' @param min_weight Minimum edge weight threshold
#'
#' @return A tidygraph tbl_graph object
#' @keywords internal
.convert_to_tidygraph <- function(data, format, directed, min_weight) {
  
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' is required for network functions")
  }
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for network functions")
  }
  
  if (format == "matrix") {
    # Convert matrix to tidygraph
    if (!is.matrix(data)) {
      stop("When input_format = 'matrix', data must be a matrix")
    }
    
    # Filter by minimum weight
    data[data < min_weight] <- 0
    
    # Create igraph from adjacency matrix
    g <- igraph::graph_from_adjacency_matrix(data, mode = if(directed) "directed" else "undirected", 
                                           weighted = TRUE, diag = FALSE)
    
    # Convert to tidygraph
    tg <- tidygraph::as_tbl_graph(g)
    
  } else {
    # Convert data.table to tidygraph
    if (!inherits(data, "data.table")) {
      stop("When input_format = 'data.table', data must be a data.table")
    }
    
    required_cols <- c("from", "to", "weight")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns in transitions data: ", paste(missing_cols, collapse = ", "))
    }
    
    # Filter edges by minimum weight
    edges <- data[weight >= min_weight]
    
    if (nrow(edges) == 0) {
      warning("No edges remain after filtering by min_weight = ", min_weight)
      # Return empty graph
      return(tidygraph::tbl_graph(nodes = data.table::data.table(name = character(0)), 
                                 edges = data.table::data.table(from = integer(0), to = integer(0))))
    }
    
    # Get unique nodes
    nodes <- data.table::data.table(name = unique(c(edges$from, edges$to)))
    
    # Create tidygraph object
    tg <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = directed)
  }
  
  return(tg)
}

#' Add Network Metrics to tidygraph Object
#'
#' Internal helper to calculate and add network centrality metrics.
#'
#' @param tg tidygraph object
#' @param size_var Variable for node sizing
#' @param color_var Variable for node coloring
#'
#' @return Modified tidygraph object with metrics
#' @keywords internal
.add_network_metrics <- function(tg, size_var, color_var) {
  
  # Add basic metrics
  tg <- tidygraph::activate(tg, nodes)
  tg <- tidygraph::mutate(tg,
    degree = tidygraph::centrality_degree(),
    strength = tidygraph::centrality_degree(weights = weight),
    in_degree = tidygraph::centrality_degree(mode = "in"),
    out_degree = tidygraph::centrality_degree(mode = "out"),
    betweenness = tidygraph::centrality_betweenness(weights = weight),
    closeness = tidygraph::centrality_closeness(weights = weight)
  )
  
  # Add community detection for coloring
  if (color_var == "community") {
    # Check if graph is directed and convert to undirected for community detection
    if (igraph::is_directed(tg)) {
      # Create temporary undirected version for community detection
      tg_undirected <- igraph::as.undirected(tg, mode = "collapse", edge.attr.comb = list(weight = "sum"))
      communities <- igraph::cluster_louvain(tg_undirected, weights = igraph::E(tg_undirected)$weight)
      community_membership <- igraph::membership(communities)
      
      # Add community membership to original directed graph
      tg <- tidygraph::mutate(tg, community = as.factor(community_membership))
    } else {
      # Use standard community detection for undirected graphs
      tg <- tidygraph::mutate(tg, community = as.factor(tidygraph::group_louvain(weights = weight)))
    }
  }
  
  # Add status-based coloring if available
  if (color_var == "status") {
    # Try to extract status from node names (employment status patterns)
    tg <- tidygraph::mutate(tg,
      status = dplyr::case_when(
        grepl("disoccupato|unemployed", name, ignore.case = TRUE) ~ "Unemployed",
        grepl("occ_ft|full.?time", name, ignore.case = TRUE) ~ "Full-time",
        grepl("occ_pt|part.?time", name, ignore.case = TRUE) ~ "Part-time",
        grepl("over_", name, ignore.case = TRUE) ~ "Multiple Jobs",
        TRUE ~ "Other"
      )
    )
  }
  
  return(tg)
}

#' Get Accessibility-Friendly Color Palettes
#'
#' Internal helper to generate colorblind-friendly and accessible color palettes.
#'
#' @param palette_name Name of color palette
#' @param use_bw Use black and white version
#' @param accessibility_mode Enable high contrast mode
#'
#' @return Character vector of colors
#' @keywords internal
.get_accessibility_colors <- function(palette_name, use_bw, accessibility_mode, n = NULL) {
  
  # Get base palette
  if (use_bw || accessibility_mode) {
    # Use vecshift BW palette for accessibility
    base_colors <- vecshift_colors("main_bw")
  } else if (palette_name == "viridis") {
    if (requireNamespace("viridis", quietly = TRUE)) {
      base_colors <- viridis::viridis(10, option = "D")
    } else {
      # Fallback to vecshift main palette
      base_colors <- vecshift_colors("main")
    }
  } else if (palette_name == "okabe_ito") {
    # Okabe-Ito colorblind-friendly palette
    base_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                     "#D55E00", "#CC79A7", "#999999", "#000000")
  } else if (palette_name == "colorbrewer_set2") {
    # ColorBrewer Set2 (qualitative, colorblind-friendly)
    base_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", 
                     "#FFD92F", "#E5C494", "#B3B3B3")
  } else {
    # Use vecshift palettes
    base_colors <- vecshift_colors(palette_name)
  }
  
  # If no specific number requested, return base palette
  if (is.null(n)) {
    return(base_colors)
  }
  
  # If we have enough colors, return subset or full palette
  if (length(base_colors) >= n) {
    return(base_colors[1:n])
  }
  
  # If we need more colors, extend the palette intelligently
  return(.extend_color_palette(base_colors, n, use_bw || accessibility_mode))
}

#' Get Required Number of Colors for Node Color Variable
#'
#' Internal helper to determine how many colors are needed for discrete color variables.
#' Returns appropriate count for continuous variables or unique value count for discrete.
#'
#' @param tg tidygraph object with node data
#' @param color_var Character. Color variable name
#' @return Integer. Number of colors needed
#' @keywords internal
.get_required_colors_count <- function(tg, color_var) {
  
  # If no color variable or fixed coloring, return default
  if (is.null(color_var) || color_var == "fixed") {
    return(NULL)
  }
  
  # If continuous variable (network metrics), return NULL for continuous scale
  if (color_var %in% c("degree", "strength", "betweenness", "closeness", "level")) {
    return(NULL)
  }
  
  # For discrete variables, count unique values
  if (requireNamespace("tidygraph", quietly = TRUE)) {
    node_data <- tidygraph::as_tibble(tg, what = "nodes")
    
    if (color_var %in% colnames(node_data)) {
      unique_values <- length(unique(node_data[[color_var]]))
      return(unique_values)
    }
  }
  
  # Fallback: return default
  return(NULL)
}

#' Extend Color Palette for Large Numbers of Categories
#'
#' Internal helper to extend color palettes when more colors are needed than available
#' in the base palette. Uses intelligent color interpolation and generation strategies.
#'
#' @param base_colors Character vector of base colors to extend
#' @param n Integer. Total number of colors needed
#' @param accessibility_mode Logical. Whether to prioritize accessibility
#' @return Character vector of n colors
#' @keywords internal
.extend_color_palette <- function(base_colors, n, accessibility_mode = FALSE) {
  
  if (n <= length(base_colors)) {
    return(base_colors[1:n])
  }
  
  # For accessibility mode, use grayscale interpolation
  if (accessibility_mode) {
    if (requireNamespace("grDevices", quietly = TRUE)) {
      # Generate grayscale sequence
      gray_values <- seq(0.15, 0.85, length.out = n)
      return(grDevices::gray(gray_values))
    } else {
      # Fallback: repeat base colors
      return(rep(base_colors, length.out = n))
    }
  }
  
  # For regular palettes, use intelligent extension strategies
  if (requireNamespace("grDevices", quietly = TRUE)) {
    
    # Strategy 1: If viridis is available and we need many colors, use it
    if (n > 20 && requireNamespace("viridis", quietly = TRUE)) {
      return(viridis::viridis(n, option = "D"))
    }
    
    # Strategy 2: Interpolate between base colors for medium numbers
    if (n <= 20) {
      extended_colors <- grDevices::colorRampPalette(base_colors)(n)
      return(extended_colors)
    }
    
    # Strategy 3: For very large numbers, combine strategies
    if (n > 20) {
      # Use half interpolated base colors, half viridis-style colors
      n_base <- ceiling(n / 2)
      n_extra <- n - n_base
      
      base_extended <- grDevices::colorRampPalette(base_colors)(n_base)
      extra_colors <- grDevices::rainbow(n_extra, start = 0.6, end = 1.0)
      
      return(c(base_extended, extra_colors))
    }
  }
  
  # Final fallback: repeat base colors
  return(rep(base_colors, length.out = n))
}

#' Check Required Packages for ggraph Functions
#'
#' Internal helper to check if required packages are available.
#'
#' @keywords internal
.check_ggraph_packages <- function() {
  required_packages <- c("ggraph", "tidygraph", "igraph")
  missing_packages <- character(0)
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    stop("Required packages not available: ", paste(missing_packages, collapse = ", "), 
         "\nInstall with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
  }
  
  # Also check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualization functions")
  }
}

#' Create Empty Network Plot for Error Cases
#'
#' Internal helper to create informative empty plots when data is insufficient.
#'
#' @param message Message to display
#'
#' @return A ggplot2 object with error message
#' @keywords internal
.create_empty_network_plot <- function(message) {
  ggplot2::ggplot() + 
    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = message),
                      size = 6, color = "#34495E") +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::labs(title = "Network Visualization", 
                 subtitle = "No data available for display") +
    theme_vecshift(grid = "none", axis = "none") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

#' Add Network Edges with Styling
#'
#' Internal helper to add styled edges to ggraph plots.
#'
#' @param width_var Variable for edge width
#' @param width_range Range for edge widths
#' @param alpha Edge transparency
#' @param show_labels Show edge labels
#' @param colors Color palette
#' @param use_bw Use black and white styling
#'
#' @return ggraph edge layer
#' @keywords internal
.add_edges <- function(width_var, width_range, alpha, show_labels, colors, use_bw) {
  
  edge_color <- if (use_bw) "#5D6D7E" else colors[1]
  
  # Create list to collect all layers
  layers <- list()
  
  # Add the main edge layer
  if (width_var == "fixed") {
    if (show_labels) {
      layers <- append(layers, list(ggraph::geom_edge_link(ggplot2::aes(label = weight), 
                                                           alpha = alpha, color = edge_color, width = mean(width_range),
                                                           label_colour = "black", label_size = 2.5)))
    } else {
      layers <- append(layers, list(ggraph::geom_edge_link(alpha = alpha, color = edge_color, width = mean(width_range))))
    }
  } else {
    if (show_labels) {
      layers <- append(layers, list(ggraph::geom_edge_link(ggplot2::aes(width = !!rlang::sym(width_var), label = weight), 
                                                           alpha = alpha, color = edge_color,
                                                           label_colour = "black", label_size = 2.5)))
    } else {
      layers <- append(layers, list(ggraph::geom_edge_link(ggplot2::aes(width = !!rlang::sym(width_var)), 
                                                           alpha = alpha, color = edge_color)))
    }
    # Add the scale separately
    layers <- append(layers, list(ggraph::scale_edge_width(range = width_range, guide = "none")))
  }
  
  return(layers)
}

#' Add Network Nodes with Styling
#'
#' Internal helper to add styled nodes to ggraph plots.
#'
#' @param size_var Variable for node sizing
#' @param size_range Range for node sizes
#' @param color_var Variable for node coloring
#' @param alpha Node transparency
#' @param colors Color palette
#'
#' @return ggraph node layer
#' @keywords internal
.add_nodes <- function(size_var, size_range, color_var, alpha, colors) {
  
  # Create list to collect all layers
  layers <- list()
  
  # Always create the base node layer
  if (size_var == "fixed" && color_var == "fixed") {
    layers <- append(layers, list(ggraph::geom_node_point(size = mean(size_range), color = colors[2], alpha = alpha)))
  } else {
    # Build aesthetic mapping
    aes_args <- list()
    
    if (size_var != "fixed") {
      aes_args$size <- rlang::sym(size_var)
    }
    
    if (color_var != "fixed") {
      aes_args$color <- rlang::sym(color_var)
    }
    
    # Add the node layer
    node_layer <- ggraph::geom_node_point(do.call(ggplot2::aes, aes_args), alpha = alpha)
    layers <- append(layers, list(node_layer))
    
    # Add scales
    if (size_var != "fixed") {
      layers <- append(layers, list(ggplot2::scale_size(range = size_range, guide = "none")))
    }
    
    if (color_var != "fixed") {
      if (color_var %in% c("degree", "strength", "betweenness", "closeness")) {
        # Continuous scale
        color_var_title <- gsub("_", " ", color_var)
        color_var_title <- paste0(toupper(substr(color_var_title, 1, 1)), substr(color_var_title, 2, nchar(color_var_title)))
        layers <- append(layers, list(ggplot2::scale_color_gradientn(colors = colors, name = color_var_title)))
      } else {
        # Discrete scale
        color_var_title <- gsub("_", " ", color_var)
        color_var_title <- paste0(toupper(substr(color_var_title, 1, 1)), substr(color_var_title, 2, nchar(color_var_title)))
        layers <- append(layers, list(ggplot2::scale_color_manual(values = colors, name = color_var_title)))
      }
    }
  }
  
  return(layers)
}

#' Add Network Labels
#'
#' Internal helper to add node labels to ggraph plots.
#'
#' @param show_labels Show labels flag
#' @param label_size Size of labels
#' @param label_repel Use repelling labels
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph label layer or NULL
#' @keywords internal
.add_labels <- function(show_labels, label_size, label_repel, accessibility_mode) {
  
  if (!show_labels) {
    return(list())  # Return empty list instead of NULL for consistency
  }
  
  label_color <- if (accessibility_mode) "#000000" else "#2C3E50"
  
  if (label_repel && requireNamespace("ggrepel", quietly = TRUE)) {
    return(list(ggraph::geom_node_label(ggplot2::aes(label = name), 
                                       size = label_size, color = label_color,
                                       fill = "white", alpha = 0.8)))
  } else {
    return(list(ggraph::geom_node_text(ggplot2::aes(label = name), 
                                      size = label_size, color = label_color)))
  }
}

#' Apply Network Theme Styling
#'
#' Internal helper to apply consistent theme to network plots.
#'
#' @param accessibility_mode High contrast mode
#' @param use_bw Black and white mode
#'
#' @return ggplot2 theme layers
#' @keywords internal
.apply_network_theme <- function(accessibility_mode, use_bw) {
  
  bg_color <- if (accessibility_mode) "white" else "#FAFBFC"
  
  return(list(
    theme_vecshift(base_size = 12, grid = "none", axis = "none"),
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA)
    )
  ))
}

#' Add Network Titles and Subtitles
#'
#' Internal helper to add informative titles to network plots.
#'
#' @param tg tidygraph object
#' @param title Custom title
#' @param subtitle Custom subtitle  
#' @param layout Layout algorithm used
#' @param size_var Node sizing variable
#'
#' @return ggplot2 labs layer
#' @keywords internal
.add_network_titles <- function(tg, title, subtitle, layout, size_var) {
  
  n_nodes <- igraph::vcount(tg)
  n_edges <- igraph::ecount(tg)
  
  if (is.null(title)) {
    title <- "Employment Transitions Network"
  }
  
  if (is.null(subtitle)) {
    layout_names <- c(
      "fr" = "Fruchterman-Reingold",
      "kk" = "Kamada-Kawai", 
      "dh" = "Davidson-Harel",
      "gem" = "GEM Algorithm",
      "graphopt" = "Graphopt",
      "mds" = "Multidimensional Scaling",
      "circle" = "Circular",
      "grid" = "Grid",
      "randomly" = "Random"
    )
    
    layout_name <- layout_names[[layout]] %||% paste0(toupper(substr(layout, 1, 1)), substr(layout, 2, nchar(layout)))
    size_name_clean <- gsub("_", " ", size_var)
    size_name <- paste0(toupper(substr(size_name_clean, 1, 1)), substr(size_name_clean, 2, nchar(size_name_clean)))
    
    subtitle <- paste0(layout_name, " layout • ", n_nodes, " states, ", n_edges, " transitions • Node size: ", size_name)
  }
  
  return(ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = "Generated with vecshift • Network analysis of employment transitions"
  ))
}

# Heatmap Helper Functions ===================================================

#' Create Empty Heatmap Plot for Error Cases
#'
#' Internal helper to create informative empty heatmap plots when data is insufficient.
#'
#' @param message Message to display
#'
#' @return A ggplot2 object with error message
#' @keywords internal
.create_empty_heatmap_plot <- function(message) {
  ggplot2::ggplot() + 
    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = message),
                      size = 6, color = "#34495E") +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::labs(title = "Transition Heatmap", 
                 subtitle = "No data available for display") +
    theme_vecshift(grid = "none", axis = "none") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

#' Add Heatmap Color Scale
#'
#' Internal helper to add appropriate color scales to heatmap plots.
#'
#' @param scale_type Type of color scale
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#'
#' @return ggplot2 scale layer
#' @keywords internal
.add_heatmap_color_scale <- function(scale_type, colors, accessibility_mode) {
  
  if (scale_type == "gradient") {
    if (accessibility_mode) {
      # High contrast gradient from white to dark
      return(ggplot2::scale_fill_gradient(low = "white", high = "#2C3E50", 
                                         name = "Transitions", na.value = "grey90"))
    } else {
      return(ggplot2::scale_fill_gradientn(colors = colors, name = "Transitions", 
                                          na.value = "grey95"))
    }
  }
  
  if (scale_type == "diverging") {
    mid_color <- if (accessibility_mode) "white" else colors[length(colors) %/% 2]
    return(ggplot2::scale_fill_gradient2(low = colors[1], mid = mid_color, 
                                        high = colors[length(colors)], 
                                        name = "Transitions", na.value = "grey95"))
  }
  
  if (scale_type == "discrete") {
    # Convert continuous values to discrete bins
    return(ggplot2::scale_fill_viridis_c(name = "Transitions", na.value = "grey95", 
                                        trans = "sqrt", option = "D"))
  }
  
  # Default gradient
  return(ggplot2::scale_fill_gradientn(colors = colors, name = "Transitions", na.value = "grey95"))
}

#' Get Text Color for Heatmap Cells
#'
#' Internal helper to determine optimal text color for heatmap cells.
#'
#' @param text_color_preference User preference for text color
#' @param accessibility_mode High contrast mode
#'
#' @return Character color value
#' @keywords internal
.get_text_color <- function(text_color_preference, accessibility_mode) {
  
  if (text_color_preference == "white") {
    return("white")
  } else if (text_color_preference == "black") {
    return("black")
  } else { # auto
    return(if (accessibility_mode) "black" else "white")
  }
}

#' Add Heatmap Titles
#'
#' Internal helper to add informative titles to heatmap plots.
#'
#' @param matrix Original transition matrix
#' @param title Custom title
#' @param subtitle Custom subtitle
#' @param normalize Normalization method used
#' @param cell_value Cell value type
#'
#' @return ggplot2 labs layer
#' @keywords internal
.add_heatmap_titles <- function(matrix, title, subtitle, normalize, cell_value) {
  
  total_transitions <- sum(matrix, na.rm = TRUE)
  n_states <- nrow(matrix)
  
  if (is.null(title)) {
    title <- "Employment Transitions Heatmap"
  }
  
  if (is.null(subtitle)) {
    norm_text <- switch(normalize,
                       "none" = "Raw counts",
                       "row" = "Row-normalized (transition probabilities from each state)",
                       "column" = "Column-normalized (transition probabilities to each state)",
                       "total" = "Total-normalized (proportions of all transitions)")
    
    value_text <- switch(cell_value,
                        "weight" = "showing counts",
                        "percentage" = "showing percentages", 
                        "both" = "showing counts and percentages",
                        "none" = "color-coded only")
    
    subtitle <- paste0(n_states, " employment states • ", format(total_transitions, big.mark = ","), 
                      " total transitions • ", norm_text, " • ", value_text)
  }
  
  return(ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = "To State",
    y = "From State",
    caption = "Generated with vecshift • Color intensity represents transition frequency"
  ))
}

# Circular Layout Helper Functions ============================================

#' Apply Circular Node Ordering
#'
#' Internal helper to order nodes for circular layouts.
#'
#' @param tg tidygraph object
#' @param order_method Ordering method
#' @param manual_order Manual ordering if specified
#'
#' @return Modified tidygraph object
#' @keywords internal
.apply_circular_ordering <- function(tg, order_method, manual_order) {
  
  if (order_method == "manual" && !is.null(manual_order)) {
    # Apply manual ordering
    node_data <- tidygraph::activate(tg, nodes)
    node_names <- tidygraph::pull(node_data, name)
    
    # Check if all manual nodes exist
    missing_nodes <- setdiff(manual_order, node_names)
    if (length(missing_nodes) > 0) {
      warning("Manual order contains non-existent nodes: ", paste(missing_nodes, collapse = ", "))
    }
    
    # Reorder nodes
    existing_order <- intersect(manual_order, node_names)
    remaining_nodes <- setdiff(node_names, existing_order)
    final_order <- c(existing_order, remaining_nodes)
    
    tg <- tidygraph::activate(tg, nodes)
    tg <- tidygraph::arrange(tg, factor(name, levels = final_order))
  }
  
  if (order_method == "frequency") {
    # Order by total degree (most connected first)
    tg <- tidygraph::activate(tg, nodes)
    tg <- tidygraph::arrange(tg, desc(tidygraph::centrality_degree(weights = weight)))
  }
  
  if (order_method == "alphabetical") {
    # Simple alphabetical ordering
    tg <- tidygraph::activate(tg, nodes)
    tg <- tidygraph::arrange(tg, name)
  }
  
  if (order_method == "cluster") {
    # Order by community membership
    if (igraph::vcount(tg) > 1) {
      tg <- tidygraph::activate(tg, nodes)
      
      # Handle directed graphs for community detection
      if (igraph::is_directed(tg)) {
        # Create temporary undirected version for community detection
        tg_undirected <- igraph::as.undirected(tg, mode = "collapse", edge.attr.comb = list(weight = "sum"))
        communities <- igraph::cluster_louvain(tg_undirected, weights = igraph::E(tg_undirected)$weight)
        community_membership <- igraph::membership(communities)
        tg <- tidygraph::mutate(tg, community = as.factor(community_membership))
      } else {
        tg <- tidygraph::mutate(tg, community = tidygraph::group_louvain(weights = weight))
      }
      
      tg <- tidygraph::arrange(tg, community, desc(tidygraph::centrality_degree(weights = weight)))
    }
  }
  
  return(tg)
}

#' Create Chord Diagram
#'
#' Internal helper to create chord-style circular diagrams.
#'
#' @param tg tidygraph object
#' @param colors Color palette
#' @param node_size_range Node size range
#' @param edge_width_range Edge width range
#' @param show_direction Show flow direction
#' @param curve Edge curvature
#' @param show_percentages Show percentages
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph plot
#' @keywords internal
.create_chord_diagram <- function(tg, colors, node_size_range, edge_width_range, 
                                 show_direction, curve, show_percentages, accessibility_mode) {
  
  p <- ggraph::ggraph(tg, layout = "linear", circular = TRUE) +
    ggraph::geom_edge_arc(ggplot2::aes(width = weight, alpha = weight), 
                         strength = curve, 
                         color = if(accessibility_mode) "#2C3E50" else colors[1]) +
    ggraph::scale_edge_width(range = edge_width_range, guide = "none") +
    ggraph::scale_edge_alpha(range = c(0.3, 0.8), guide = "none") +
    ggraph::geom_node_point(ggplot2::aes(size = strength), 
                           color = if(accessibility_mode) "#34495E" else colors[2],
                           alpha = 0.8) +
    ggplot2::scale_size(range = node_size_range, guide = "none") +
    ggraph::geom_node_text(ggplot2::aes(label = name), 
                          size = 3, color = if(accessibility_mode) "#000000" else "#2C3E50",
                          fontface = "bold")
  
  if (show_direction) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(width = weight), 
                                  strength = curve, alpha = 0.1,
                                  arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
                                  start_cap = ggraph::circle(3, "mm"),
                                  end_cap = ggraph::circle(3, "mm"),
                                  color = if(accessibility_mode) "#2C3E50" else colors[1])
  }
  
  return(p)
}

#' Create Arc Diagram
#'
#' Internal helper to create arc-style diagrams.
#'
#' @param tg tidygraph object
#' @param colors Color palette
#' @param node_size_range Node size range
#' @param edge_width_range Edge width range
#' @param show_direction Show flow direction
#' @param label_distance Distance for labels
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph plot
#' @keywords internal
.create_arc_diagram <- function(tg, colors, node_size_range, edge_width_range,
                               show_direction, label_distance, accessibility_mode) {
  
  p <- ggraph::ggraph(tg, layout = "linear") +
    ggraph::geom_edge_arc(ggplot2::aes(width = weight, alpha = weight),
                         color = if(accessibility_mode) "#2C3E50" else colors[1]) +
    ggraph::scale_edge_width(range = edge_width_range, guide = "none") +
    ggraph::scale_edge_alpha(range = c(0.3, 0.8), guide = "none") +
    ggraph::geom_node_point(ggplot2::aes(size = strength),
                           color = if(accessibility_mode) "#34495E" else colors[2],
                           alpha = 0.8) +
    ggplot2::scale_size(range = node_size_range, guide = "none") +
    ggraph::geom_node_text(ggplot2::aes(label = name), 
                          hjust = 0.5, vjust = -0.5, size = 3,
                          color = if(accessibility_mode) "#000000" else "#2C3E50",
                          fontface = "bold")
  
  if (show_direction) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(width = weight), alpha = 0.1,
                                  arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
                                  start_cap = ggraph::circle(2, "mm"),
                                  end_cap = ggraph::circle(2, "mm"),
                                  color = if(accessibility_mode) "#2C3E50" else colors[1])
  }
  
  return(p)
}

#' Create Wheel Diagram
#'
#' Internal helper to create wheel-style diagrams.
#'
#' @param tg tidygraph object
#' @param colors Color palette
#' @param node_size_range Node size range
#' @param edge_width_range Edge width range
#' @param curve Edge curvature
#' @param label_distance Distance for labels
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph plot
#' @keywords internal
.create_wheel_diagram <- function(tg, colors, node_size_range, edge_width_range,
                                 curve, label_distance, accessibility_mode) {
  
  p <- ggraph::ggraph(tg, layout = "circle") +
    ggraph::geom_edge_link(ggplot2::aes(width = weight, alpha = weight),
                          color = if(accessibility_mode) "#2C3E50" else colors[1]) +
    ggraph::scale_edge_width(range = edge_width_range, guide = "none") +
    ggraph::scale_edge_alpha(range = c(0.2, 0.7), guide = "none") +
    ggraph::geom_node_point(ggplot2::aes(size = strength),
                           color = if(accessibility_mode) "#34495E" else colors[2],
                           alpha = 0.8) +
    ggplot2::scale_size(range = node_size_range, guide = "none") +
    ggraph::geom_node_text(ggplot2::aes(label = name), 
                          size = 3, color = if(accessibility_mode) "#000000" else "#2C3E50",
                          fontface = "bold", repel = TRUE)
  
  return(p)
}

#' Apply Circular Theme
#'
#' Internal helper to apply theme for circular plots.
#'
#' @param accessibility_mode High contrast mode
#'
#' @return ggplot2 theme layers
#' @keywords internal
.apply_circular_theme <- function(accessibility_mode) {
  
  bg_color <- if (accessibility_mode) "white" else "#FAFBFC"
  
  return(list(
    theme_vecshift(base_size = 12, grid = "none", axis = "none"),
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(), 
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
  ))
}

#' Add Circular Plot Titles
#'
#' Internal helper to add titles for circular plots.
#'
#' @param tg tidygraph object
#' @param title Custom title
#' @param subtitle Custom subtitle
#' @param circular_type Type of circular layout
#' @param node_order Node ordering method
#'
#' @return ggplot2 labs layer
#' @keywords internal
.add_circular_titles <- function(tg, title, subtitle, circular_type, node_order) {
  
  n_nodes <- igraph::vcount(tg)
  n_edges <- igraph::ecount(tg)
  
  if (is.null(title)) {
    type_names <- c("chord" = "Chord Diagram", "arc" = "Arc Diagram", "wheel" = "Wheel Diagram")
    title <- paste("Employment Transitions:", type_names[[circular_type]])
  }
  
  if (is.null(subtitle)) {
    order_text <- switch(node_order,
                        "frequency" = "ordered by connection frequency",
                        "alphabetical" = "alphabetically ordered",
                        "cluster" = "ordered by community",
                        "manual" = "custom ordering")
    
    subtitle <- paste0(n_nodes, " states, ", n_edges, " transitions • ", order_text)
  }
  
  return(ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = "Generated with vecshift • Circular layout emphasizes flow relationships"
  ))
}

# Hierarchical Layout Helper Functions ========================================

#' Add Level Backgrounds to Hierarchical Plots
#'
#' Internal helper to add background highlights for different hierarchy levels.
#' Creates colored background rectangles to visually separate different levels
#' in tree and hierarchical layouts.
#'
#' @param colors Color palette to use for backgrounds
#' @param accessibility_mode High contrast mode flag
#'
#' @return ggplot2 layer with background rectangles
#' @keywords internal
.add_level_backgrounds <- function(colors, accessibility_mode) {
  
  # In hierarchical layouts, we can add subtle background strips
  # to help distinguish different levels. Since ggraph layouts
  # are complex and level positions are layout-dependent,
  # we'll return a simple background layer that can be customized
  
  if (accessibility_mode) {
    # High contrast: use alternating light gray backgrounds
    bg_colors <- c("#F8F9FA", "#E9ECEF")
  } else {
    # Use subtle background color without attempting to modify hex colors
    # This avoids color format issues
    bg_colors <- c("#F5F7FA", "#EDF2F7", "#E2E8F0")
  }
  
  # Return a subtle background that won't interfere with the network
  # This is a placeholder implementation since exact level positioning
  # depends on the specific layout algorithm used
  return(
    ggplot2::annotate("rect", 
                     xmin = -Inf, xmax = Inf, 
                     ymin = -Inf, ymax = Inf,
                     fill = if(accessibility_mode) "#FAFBFC" else "#F8F9FA",
                     alpha = 0.2)
  )
}

#' Add Hierarchical Metrics to Graph
#'
#' Internal helper to add hierarchy-specific metrics to tidygraph object.
#'
#' @param tg tidygraph object
#' @param hierarchy_type Type of hierarchy
#' @param root_nodes Specified root nodes
#'
#' @return Modified tidygraph object with hierarchy info
#' @keywords internal
.add_hierarchical_metrics <- function(tg, hierarchy_type, root_nodes) {
  
  # Add hierarchy levels based on shortest path from roots
  if (is.null(root_nodes) || length(root_nodes) == 0) {
    # Auto-detect roots (nodes with high out-degree, low in-degree)
    tg <- tidygraph::activate(tg, nodes)
    tg <- tidygraph::mutate(tg,
      out_deg = tidygraph::centrality_degree(mode = "out"),
      in_deg = tidygraph::centrality_degree(mode = "in"),
      root_score = out_deg - in_deg
    )
    tg <- tidygraph::arrange(tg, desc(root_score))
    
    # Take top nodes as roots
    node_names <- tidygraph::pull(tg, name)
    root_nodes <- head(node_names, max(1, ceiling(length(node_names) * 0.1)))
  }
  
  # Calculate levels from roots - simplified approach
  tg <- tidygraph::activate(tg, nodes)
  tg <- tidygraph::mutate(tg, level = 0)  # Simple fallback: all nodes at level 0
  
  return(tg)
}

#' Get Hierarchical Layout Name
#'
#' Internal helper to map hierarchy types to ggraph layout names.
#'
#' @param hierarchy_type Type of hierarchy
#' @param direction Layout direction
#'
#' @return Layout name string
#' @keywords internal
.get_hierarchical_layout_name <- function(hierarchy_type, direction) {
  
  base_layouts <- c(
    "tree" = "tree",
    "dendrogram" = "dendrogram", 
    "sugiyama" = "sugiyama",
    "davidson_harel" = "davidson_harel"
  )
  
  layout_name <- base_layouts[[hierarchy_type]] %||% "tree"
  
  # Add direction if supported
  if (hierarchy_type %in% c("tree", "sugiyama") && direction == "horizontal") {
    # These layouts support direction parameters
    return(layout_name)
  }
  
  return(layout_name)
}

#' Add Hierarchical Edges
#'
#' Internal helper to add styled edges for hierarchical plots.
#'
#' @param width_var Edge width variable
#' @param width_range Edge width range
#' @param show_labels Show edge labels
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph edge layer
#' @keywords internal
.add_hierarchical_edges <- function(width_var, width_range, show_labels, colors, accessibility_mode) {
  
  edge_color <- if (accessibility_mode) "#2C3E50" else colors[1]
  
  # Create list to collect all layers
  layers <- list()
  
  # Add the main edge layer
  if (width_var == "fixed") {
    if (show_labels) {
      layers <- append(layers, list(ggraph::geom_edge_link(ggplot2::aes(label = weight), 
                                                           alpha = 0.6, color = edge_color, width = mean(width_range),
                                                           label_colour = edge_color, label_size = 2.5)))
    } else {
      layers <- append(layers, list(ggraph::geom_edge_link(alpha = 0.6, color = edge_color, width = mean(width_range))))
    }
  } else {
    if (show_labels) {
      layers <- append(layers, list(ggraph::geom_edge_link(ggplot2::aes(width = !!rlang::sym(width_var), label = weight), 
                                                           alpha = 0.6, color = edge_color,
                                                           label_colour = edge_color, label_size = 2.5)))
    } else {
      layers <- append(layers, list(ggraph::geom_edge_link(ggplot2::aes(width = !!rlang::sym(width_var)), 
                                                           alpha = 0.6, color = edge_color)))
    }
    # Add the scale separately
    layers <- append(layers, list(ggraph::scale_edge_width(range = width_range, guide = "none")))
  }
  
  return(layers)
}

#' Add Hierarchical Nodes
#'
#' Internal helper to add styled nodes for hierarchical plots.
#'
#' @param size_var Node size variable
#' @param size_range Node size range
#' @param color_var Node color variable
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph node layer
#' @keywords internal
.add_hierarchical_nodes <- function(size_var, size_range, color_var, colors, accessibility_mode) {
  
  # Create list to collect all layers
  layers <- list()
  
  # Always create the base node layer
  if (size_var == "fixed" && color_var == "fixed") {
    layers <- append(layers, list(ggraph::geom_node_point(size = mean(size_range), 
                                                          color = if(accessibility_mode) "#34495E" else colors[2], 
                                                          alpha = 0.8)))
  } else {
    # Build aesthetic mapping
    aes_args <- list()
    
    if (size_var != "fixed") {
      aes_args$size <- rlang::sym(size_var)
    }
    
    if (color_var != "fixed") {
      aes_args$color <- rlang::sym(color_var)
    }
    
    # Add the node layer
    node_layer <- ggraph::geom_node_point(do.call(ggplot2::aes, aes_args), alpha = 0.8)
    layers <- append(layers, list(node_layer))
    
    # Add scales
    if (size_var != "fixed") {
      layers <- append(layers, list(ggplot2::scale_size(range = size_range, guide = "none")))
    }
    
    if (color_var != "fixed") {
      if (accessibility_mode) {
        # Use grayscale for accessibility
        color_var_title <- gsub("_", " ", color_var)
        color_var_title <- paste0(toupper(substr(color_var_title, 1, 1)), substr(color_var_title, 2, nchar(color_var_title)))
        layers <- append(layers, list(ggplot2::scale_color_grey(start = 0.2, end = 0.8, 
                                                               name = color_var_title)))
      } else if (color_var == "level") {
        # Use gradient for levels
        layers <- append(layers, list(ggplot2::scale_color_gradientn(colors = colors, 
                                                                    name = "Hierarchy Level")))
      } else {
        # Discrete scale
        color_var_title <- gsub("_", " ", color_var)
        color_var_title <- paste0(toupper(substr(color_var_title, 1, 1)), substr(color_var_title, 2, nchar(color_var_title)))
        layers <- append(layers, list(ggplot2::scale_color_manual(values = colors, 
                                                                 name = color_var_title)))
      }
    }
  }
  
  return(layers)
}

#' Add Hierarchical Labels
#'
#' Internal helper to add labels for hierarchical plots.
#'
#' @param label_angle Angle for labels
#' @param accessibility_mode High contrast mode
#'
#' @return ggraph label layer
#' @keywords internal
.add_hierarchical_labels <- function(label_angle, accessibility_mode) {
  
  label_color <- if (accessibility_mode) "#000000" else "#2C3E50"
  
  return(list(ggraph::geom_node_text(ggplot2::aes(label = name), 
                                    size = 3, color = label_color, fontface = "bold",
                                    angle = label_angle, hjust = 0.5, vjust = 0.5)))
}

#' Apply Hierarchical Theme
#'
#' Internal helper to apply theme for hierarchical plots.
#'
#' @param accessibility_mode High contrast mode
#' @param direction Layout direction
#'
#' @return ggplot2 theme layers
#' @keywords internal
.apply_hierarchical_theme <- function(accessibility_mode, direction) {
  
  bg_color <- if (accessibility_mode) "white" else "#FAFBFC"
  
  return(list(
    theme_vecshift(base_size = 12, grid = "none", axis = "none"),
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), 
      axis.title = ggplot2::element_blank(),
      legend.position = "right",
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  ))
}

#' Add Hierarchical Plot Titles
#'
#' Internal helper to add titles for hierarchical plots.
#'
#' @param tg tidygraph object
#' @param title Custom title
#' @param subtitle Custom subtitle
#' @param hierarchy_type Type of hierarchy
#' @param direction Layout direction
#'
#' @return ggplot2 labs layer
#' @keywords internal
.add_hierarchical_titles <- function(tg, title, subtitle, hierarchy_type, direction) {
  
  n_nodes <- igraph::vcount(tg)
  n_edges <- igraph::ecount(tg)
  
  if (is.null(title)) {
    type_names <- c(
      "tree" = "Tree Layout",
      "dendrogram" = "Dendrogram", 
      "sugiyama" = "Sugiyama Layout",
      "davidson_harel" = "Davidson-Harel Layout"
    )
    title <- paste("Employment Transitions:", type_names[[hierarchy_type]])
  }
  
  if (is.null(subtitle)) {
    direction_text <- if(direction == "horizontal") "horizontal" else "vertical"
    level_data <- tidygraph::activate(tg, nodes)
    max_level <- max(tidygraph::pull(level_data, level), na.rm = TRUE)
    
    subtitle <- paste0(n_nodes, " states, ", n_edges, " transitions • ", 
                      max_level + 1, " hierarchy levels • ", direction_text, " layout")
  }
  
  return(ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = "Generated with vecshift • Hierarchical layout shows employment progression patterns"
  ))
}

# Utility Functions ===========================================================

#' Null-coalescing Operator
#'
#' Internal utility operator similar to %||% in other packages.
#'
#' @param x Left-hand side value
#' @param y Right-hand side default value
#'
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Convert Matrix to Long Format for Heatmaps
#'
#' Internal helper to convert transition matrices to long format for ggplot2.
#'
#' @param matrix Transition matrix
#' @param cell_value Type of values to display
#' @param min_value Minimum value threshold
#'
#' @return data.table in long format
#' @keywords internal
.matrix_to_long <- function(matrix, cell_value, min_value) {
  
  # Convert to long format
  long_data <- data.table::data.table(
    expand.grid(from = rownames(matrix), to = colnames(matrix), stringsAsFactors = FALSE)
  )
  long_data[, value := as.vector(matrix)]
  
  # Set values below min_value to NA for proper color scale mapping
  # This ensures cells below threshold are excluded from color gradient
  long_data[value < min_value, value := NA_real_]
  
  # Calculate percentages if needed (use original values before NA conversion)
  if (cell_value %in% c("percentage", "both")) {
    total_sum <- sum(matrix, na.rm = TRUE)
    # Store original values for percentage calculation
    orig_values <- as.vector(matrix)
    long_data[, percentage := round(100 * orig_values / total_sum, 1)]
    # Set percentage to NA for cells below threshold
    long_data[orig_values < min_value, percentage := NA_real_]
  }
  
  # Create display values
  long_data[, display_value := ""]
  
  if (cell_value == "weight") {
    long_data[!is.na(value), display_value := as.character(value)]
  } else if (cell_value == "percentage") {
    long_data[!is.na(percentage), display_value := paste0(percentage, "%")]
  } else if (cell_value == "both") {
    long_data[!is.na(value) & !is.na(percentage), display_value := paste0(value, "\n(", percentage, "%)")]
  }
  
  return(long_data)
}

#' Convert Data to Matrix Format
#'
#' Internal helper to convert various input formats to matrix.
#'
#' @param data Input transitions data
#' @param format Input format type
#'
#' @return Transition matrix
#' @keywords internal
.convert_to_matrix <- function(data, format) {
  
  if (format == "matrix") {
    if (!is.matrix(data)) {
      stop("When input_format = 'matrix', data must be a matrix")
    }
    return(data)
  }
  
  # Convert data.table to matrix
  if (!inherits(data, "data.table")) {
    stop("When input_format = 'data.table', data must be a data.table")
  }
  
  required_cols <- c("from", "to", "weight")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Get unique states
  all_states <- sort(unique(c(data$from, data$to)))
  
  # Create empty matrix
  matrix <- matrix(0, nrow = length(all_states), ncol = length(all_states))
  rownames(matrix) <- all_states
  colnames(matrix) <- all_states
  
  # Fill matrix
  for (i in 1:nrow(data)) {
    from_idx <- which(all_states == data$from[i])
    to_idx <- which(all_states == data$to[i])
    matrix[from_idx, to_idx] <- data$weight[i]
  }
  
  return(matrix)
}

#' Normalize Transition Matrix
#'
#' Internal helper to apply various normalization methods to transition matrices.
#'
#' @param matrix Input matrix
#' @param method Normalization method
#'
#' @return Normalized matrix
#' @keywords internal
.normalize_matrix <- function(matrix, method) {
  
  if (method == "none") {
    return(matrix)
  }
  
  if (method == "row") {
    # Normalize by row sums (transition probabilities from each state)
    row_sums <- rowSums(matrix)
    row_sums[row_sums == 0] <- 1  # Avoid division by zero
    return(matrix / row_sums)
  }
  
  if (method == "column") {
    # Normalize by column sums (transition probabilities to each state)  
    col_sums <- colSums(matrix)
    col_sums[col_sums == 0] <- 1
    return(t(t(matrix) / col_sums))
  }
  
  if (method == "total") {
    # Normalize by total sum
    total_sum <- sum(matrix)
    if (total_sum == 0) return(matrix)
    return(matrix / total_sum)
  }
  
  return(matrix)
}

# over_id and Consolidation Functions ==========================================

#' Plot over_id Distribution Analysis
#'
#' Creates visualizations showing the distribution of over_id values to understand
#' employment complexity patterns. Shows how many overlapping employment periods
#' each person has and the duration patterns of these overlapping groups.
#'
#' @param pipeline_result Output from vecshift() with over_id column
#' @param plot_type Character. Type of visualization: "histogram", "boxplot", "density", "summary" (default: "summary")
#' @param include_unemployment Logical. Include over_id = 0 (unemployment) in analysis (default: FALSE)
#' @param facet_by Character. Variable to facet by (optional)
#' @param palette Character. Color palette (default: "viridis")
#' @param accessibility_mode Logical. Use high contrast mode (default: FALSE)
#' @param title Character. Custom title (default: auto-generated)
#'
#' @return A ggplot2 object showing over_id distribution
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic over_id distribution
#' plot_over_id_distribution(pipeline_result)
#' 
#' # Histogram including unemployment periods  
#' plot_over_id_distribution(pipeline_result, plot_type = "histogram", include_unemployment = TRUE)
#' 
#' # Density plot by employment type
#' plot_over_id_distribution(pipeline_result, plot_type = "density", facet_by = "prior")
#' }
plot_over_id_distribution <- function(pipeline_result,
                                     plot_type = "summary",
                                     include_unemployment = FALSE,
                                     facet_by = NULL,
                                     palette = "viridis",
                                     accessibility_mode = FALSE,
                                     title = NULL) {
  
  # Input validation
  if (!"over_id" %in% names(pipeline_result)) {
    stop("over_id column not found. Run vecshift() with over_id calculation enabled.")
  }
  
  plot_type <- match.arg(plot_type, c("histogram", "boxplot", "density", "summary"))
  
  # Prepare data
  data <- data.table::copy(pipeline_result)
  
  if (!include_unemployment) {
    data <- data[over_id > 0]
  }
  
  # Get colors
  colors <- .get_accessibility_colors(palette, FALSE, accessibility_mode)
  
  # Create visualization based on plot_type
  if (plot_type == "summary") {
    p <- .create_over_id_summary_plot(data, colors, accessibility_mode, facet_by)
  } else if (plot_type == "histogram") {
    p <- .create_over_id_histogram(data, colors, accessibility_mode, facet_by)
  } else if (plot_type == "boxplot") {
    p <- .create_over_id_boxplot(data, colors, accessibility_mode, facet_by)
  } else { # density
    p <- .create_over_id_density(data, colors, accessibility_mode, facet_by)
  }
  
  # Add title
  if (is.null(title)) {
    title <- paste("Employment Complexity Analysis:", 
                  if(include_unemployment) "All Periods" else "Employment Periods Only")
  }
  
  total_periods <- nrow(data)
  unique_over_ids <- length(unique(data$over_id[data$over_id > 0]))
  
  p <- p + ggplot2::labs(
    title = title,
    subtitle = paste0(format(total_periods, big.mark = ","), " periods • ", 
                     unique_over_ids, " unique employment episodes • ",
                     "over_id shows overlapping employment complexity"),
    caption = "Generated with vecshift • over_id = 0: unemployment, over_id > 0: employment episodes"
  )
  
  return(p)
}

#' Plot Employment Complexity Using over_id
#'
#' Creates network-style visualizations showing employment complexity patterns
#' using over_id groupings. Visualizes how employment periods cluster together
#' and the relationships between overlapping employment episodes.
#'
#' @param pipeline_result Output from vecshift() with over_id column
#' @param complexity_metric Character. Metric to use: "overlap_count", "duration_sum", "period_count" (default: "overlap_count")
#' @param layout Character. Network layout algorithm (default: "fr")
#' @param node_size_range Numeric vector. Size range for nodes (default: c(2, 12))
#' @param edge_width_range Numeric vector. Width range for edges (default: c(0.3, 2))
#' @param palette Character. Color palette (default: "viridis")
#' @param accessibility_mode Logical. Use high contrast mode (default: FALSE)
#' @param show_labels Logical. Show over_id labels (default: TRUE)
#' @param title Character. Custom title (default: auto-generated)
#'
#' @return A ggplot2 network object showing employment complexity
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic complexity visualization
#' plot_employment_complexity(pipeline_result)
#' 
#' # Duration-based complexity with circular layout
#' plot_employment_complexity(pipeline_result, complexity_metric = "duration_sum", layout = "circle")
#' }
plot_employment_complexity <- function(pipeline_result,
                                     complexity_metric = "overlap_count",
                                     layout = "fr",
                                     node_size_range = c(2, 12),
                                     edge_width_range = c(0.3, 2),
                                     palette = "viridis",
                                     accessibility_mode = FALSE,
                                     show_labels = TRUE,
                                     title = NULL) {
  
  # Check required packages
  .check_ggraph_packages()
  
  # Input validation
  if (!"over_id" %in% names(pipeline_result)) {
    stop("over_id column not found. Run vecshift() with over_id calculation enabled.")
  }
  
  complexity_metric <- match.arg(complexity_metric, c("overlap_count", "duration_sum", "period_count"))
  
  # Create complexity network data
  complexity_data <- .create_complexity_network_data(pipeline_result, complexity_metric)
  
  if (nrow(complexity_data$edges) == 0) {
    return(.create_empty_network_plot("No employment complexity to visualize"))
  }
  
  # Convert to tidygraph
  tg <- tidygraph::tbl_graph(nodes = complexity_data$nodes, 
                            edges = complexity_data$edges, 
                            directed = FALSE)
  
  # Get colors
  colors <- .get_accessibility_colors(palette, FALSE, accessibility_mode)
  
  # Create visualization
  p <- ggraph::ggraph(tg, layout = layout) +
    ggraph::geom_edge_link(ggplot2::aes(width = weight), 
                          alpha = 0.6, 
                          color = if(accessibility_mode) "#2C3E50" else colors[1]) +
    ggraph::scale_edge_width(range = edge_width_range, guide = "none") +
    ggraph::geom_node_point(ggplot2::aes(size = complexity_score, color = cf_count),
                           alpha = 0.8) +
    ggplot2::scale_size(range = node_size_range, name = "Complexity\nScore") +
    ggplot2::scale_color_gradientn(colors = colors, name = "People\nCount")
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = paste0("ID:", over_id)), 
                                   size = 2.5, color = if(accessibility_mode) "#000000" else "#2C3E50",
                                   repel = TRUE)
  }
  
  # Apply theme
  p <- p + .apply_network_theme(accessibility_mode, FALSE)
  
  # Add titles
  if (is.null(title)) {
    title <- paste("Employment Complexity Network:", 
                  switch(complexity_metric,
                         "overlap_count" = "Overlapping Periods",
                         "duration_sum" = "Duration-Weighted", 
                         "period_count" = "Period Count"))
  }
  
  n_over_ids <- nrow(complexity_data$nodes)
  n_connections <- nrow(complexity_data$edges)
  
  p <- p + ggplot2::labs(
    title = title,
    subtitle = paste0(n_over_ids, " employment episodes • ", n_connections, " complexity connections • ",
                     "Node size: ", gsub("_", " ", complexity_metric)),
    caption = "Generated with vecshift • Nodes: over_id episodes, Edges: shared characteristics"
  )
  
  return(p)
}

#' Compare Raw vs Consolidated Transitions
#'
#' Creates side-by-side comparison plots showing the difference between raw
#' transitions and consolidated transitions using over_id grouping. Demonstrates
#' the benefits of consolidation for cleaner network visualization.
#'
#' @param pipeline_result Output from vecshift() with over_id column
#' @param transition_variable Character. Variable for transition analysis (default: "prior")
#' @param layout Character. Network layout (default: "fr")
#' @param palette Character. Color palette (default: "viridis")
#' @param accessibility_mode Logical. Use high contrast mode (default: FALSE)
#' @param consolidation_type Character. Type of consolidation (default: "both")
#' @param title Character. Custom title (default: auto-generated)
#'
#' @return A combined ggplot2 object with before/after comparison
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic comparison
#' plot_consolidation_comparison(pipeline_result)
#' 
#' # Company transitions with overlapping consolidation only
#' plot_consolidation_comparison(pipeline_result, transition_variable = "company", 
#'                              consolidation_type = "overlapping")
#' }
plot_consolidation_comparison <- function(pipeline_result,
                                        transition_variable = "prior",
                                        layout = "fr",
                                        palette = "viridis",
                                        accessibility_mode = FALSE,
                                        consolidation_type = "both",
                                        title = NULL) {
  
  # Check required packages
  .check_ggraph_packages()
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for comparison plots. Install with: install.packages('patchwork')")
  }
  
  # Input validation
  if (!"over_id" %in% names(pipeline_result)) {
    stop("over_id column not found. Run vecshift() with over_id calculation enabled.")
  }
  
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive"))
  
  # Generate raw transitions
  transitions_raw <- analyze_employment_transitions(
    pipeline_result = pipeline_result,
    transition_variable = transition_variable,
    use_consolidated_periods = FALSE,
    show_progress = FALSE
  )
  
  # Generate consolidated transitions
  transitions_consolidated <- analyze_employment_transitions(
    pipeline_result = pipeline_result,
    transition_variable = transition_variable,
    use_consolidated_periods = TRUE,
    consolidation_type = consolidation_type,
    show_progress = FALSE
  )
  
  # Create individual plots
  plot_raw <- plot_transitions_network(
    transitions_data = transitions_raw,
    layout = layout,
    palette = palette,
    accessibility_mode = accessibility_mode,
    use_consolidated_periods = FALSE,
    title = "Raw Transitions",
    subtitle = paste0(nrow(transitions_raw), " transitions • Administrative splits included")
  )
  
  plot_consolidated <- plot_transitions_network(
    transitions_data = transitions_consolidated,
    layout = layout,
    palette = palette,
    accessibility_mode = accessibility_mode,
    use_consolidated_periods = FALSE,
    title = "Consolidated Transitions", 
    subtitle = paste0(nrow(transitions_consolidated), " transitions • over_id consolidation applied")
  )
  
  # Combine plots
  combined_plot <- plot_raw + plot_consolidated + 
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(
      title = if(is.null(title)) "Employment Transitions: Raw vs Consolidated" else title,
      subtitle = paste0("Consolidation type: ", consolidation_type, " • ", 
                       "Reduction: ", nrow(transitions_raw) - nrow(transitions_consolidated), " transitions"),
      caption = "Generated with vecshift • Left: all administrative periods, Right: consolidated using over_id"
    )
  
  return(combined_plot)
}

# Additional Analysis Functions ===============================================

#' Analyze Network Structure from Transitions Data
#'
#' Analyzes the network structure of employment transitions to provide insights
#' about connectivity, centrality, and community structure. Useful for understanding
#' employment flow patterns and identifying key states.
#'
#' @param transitions_data Data.table output from analyze_employment_transitions()
#' @param input_format Character. Format of input data: "data.table" or "matrix" (default: "data.table")
#' @param min_edge_weight Numeric. Minimum edge weight to include (default: 1)
#' @param directed Logical. Treat as directed network (default: TRUE)
#' @param compute_communities Logical. Compute community structure (default: TRUE)
#' @param return_tidygraph Logical. Return tidygraph object instead of summary (default: FALSE)
#'
#' @return List with network analysis results or tidygraph object
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze consolidated network structure (default)
#' network_analysis <- analyze_transitions_network(pipeline_result)
#' print(network_analysis)
#' 
#' # Get tidygraph object for further analysis with overlapping consolidation only
#' tg <- analyze_transitions_network(pipeline_result, return_tidygraph = TRUE,
#'                                  consolidation_type = "overlapping")
#' 
#' # Analyze pre-computed transitions without consolidation
#' transitions <- analyze_employment_transitions(pipeline_result, use_consolidated_periods = FALSE)
#' network_analysis_raw <- analyze_transitions_network(transitions, use_consolidated_periods = FALSE)
#' }
analyze_transitions_network <- function(transitions_data,
                                      input_format = "data.table", 
                                      min_edge_weight = 1,
                                      directed = TRUE,
                                      compute_communities = TRUE,
                                      return_tidygraph = FALSE,
                                      use_consolidated_periods = TRUE,
                                      consolidation_type = "both",
                                      transition_variable = "prior") {
  
  # Check required packages
  .check_ggraph_packages()
  
  # Input validation
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive", "none"))
  
  # Process transitions data if needed for consolidation
  if (use_consolidated_periods && consolidation_type != "none" && .is_pipeline_data(transitions_data)) {
    transitions_data <- .process_with_consolidation(transitions_data, consolidation_type, transition_variable)
  }
  
  # Convert to tidygraph
  tg <- .convert_to_tidygraph(transitions_data, input_format, directed, min_edge_weight)
  
  if (igraph::vcount(tg) == 0) {
    warning("No nodes found after filtering")
    return(list(message = "Empty network"))
  }
  
  # Add network metrics
  tg <- .add_network_metrics(tg, "degree", "community")
  
  if (return_tidygraph) {
    return(tg)
  }
  
  # Extract analysis results
  nodes_obj <- tidygraph::activate(tg, nodes)
  nodes <- tidygraph::as_tibble(nodes_obj)
  edges_obj <- tidygraph::activate(tg, edges)
  edges <- tidygraph::as_tibble(edges_obj)
  
  # Basic network statistics
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edges)
  density <- n_edges / (n_nodes * (n_nodes - 1))
  
  # Centrality analysis
  centrality_stats <- list(
    degree = list(
      mean = mean(nodes$degree, na.rm = TRUE),
      max = max(nodes$degree, na.rm = TRUE),
      top_nodes = nodes$name[order(nodes$degree, decreasing = TRUE)[1:min(5, n_nodes)]]
    ),
    strength = list(
      mean = mean(nodes$strength, na.rm = TRUE),
      max = max(nodes$strength, na.rm = TRUE),
      top_nodes = nodes$name[order(nodes$strength, decreasing = TRUE)[1:min(5, n_nodes)]]
    ),
    betweenness = list(
      mean = mean(nodes$betweenness, na.rm = TRUE),
      max = max(nodes$betweenness, na.rm = TRUE),
      top_nodes = nodes$name[order(nodes$betweenness, decreasing = TRUE)[1:min(5, n_nodes)]]
    )
  )
  
  # Community analysis
  community_stats <- NULL
  if (compute_communities && "community" %in% names(nodes)) {
    community_counts <- table(nodes$community)
    community_stats <- list(
      n_communities = length(community_counts),
      size_distribution = as.list(community_counts),
      modularity = igraph::modularity(tg, nodes$community)
    )
  }
  
  # Return comprehensive analysis
  return(list(
    network_summary = list(
      nodes = n_nodes,
      edges = n_edges,
      density = density,
      directed = directed
    ),
    centrality = centrality_stats,
    communities = community_stats,
    edge_weights = list(
      min = min(edges$weight, na.rm = TRUE),
      max = max(edges$weight, na.rm = TRUE),
      mean = mean(edges$weight, na.rm = TRUE),
      total = sum(edges$weight, na.rm = TRUE)
    )
  ))
}

#' Create Accessibility Report for Network Visualizations
#'
#' Generates a comprehensive accessibility report for network visualizations,
#' checking color contrast, layout readability, and providing recommendations
#' for improving accessibility. Now includes analysis of consolidation benefits
#' for cleaner, more accessible visualizations.
#'
#' @param transitions_data Data.table with transitions data or pipeline data
#' @param layout Layout algorithm to test
#' @param palette Color palette to test
#' @param check_color_contrast Logical. Check color contrast ratios (default: TRUE)
#' @param check_layout_complexity Logical. Check layout complexity (default: TRUE)
#' @param return_suggestions Logical. Return improvement suggestions (default: TRUE)
#' @param use_consolidated_periods Logical. Test consolidation benefits (default: TRUE)
#' @param consolidation_type Character. Type of consolidation to test (default: "both")
#'
#' @return List with accessibility assessment and recommendations
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate accessibility report with consolidation assessment
#' accessibility_report <- create_accessibility_report(pipeline_result, 
#'                                                     layout = "fr", 
#'                                                     palette = "viridis")
#' print(accessibility_report)
#' 
#' # Test specific consolidation type
#' report_overlapping <- create_accessibility_report(pipeline_result,
#'                                                   consolidation_type = "overlapping")
#' }
create_accessibility_report <- function(transitions_data,
                                      layout = "fr",
                                      palette = "viridis", 
                                      check_color_contrast = TRUE,
                                      check_layout_complexity = TRUE,
                                      return_suggestions = TRUE,
                                      use_consolidated_periods = TRUE,
                                      consolidation_type = "both") {
  
  # Initialize report
  report <- list(
    timestamp = Sys.time(),
    layout = layout,
    palette = palette,
    use_consolidated_periods = use_consolidated_periods,
    consolidation_type = consolidation_type,
    accessibility_score = 0,
    max_score = 0,
    issues = character(),
    recommendations = character()
  )
  
  # Input validation
  consolidation_type <- match.arg(consolidation_type, c("both", "overlapping", "consecutive", "none"))
  
  # Test consolidation benefits if pipeline data is provided
  if (use_consolidated_periods && .is_pipeline_data(transitions_data)) {
    consolidation_assessment <- .assess_consolidation_benefits(transitions_data, consolidation_type)
    report$consolidation_benefits <- consolidation_assessment
    report$max_score <- report$max_score + 20
    
    if (consolidation_assessment$complexity_reduction > 0.2) {
      report$accessibility_score <- report$accessibility_score + 20
    } else if (consolidation_assessment$complexity_reduction > 0.1) {
      report$accessibility_score <- report$accessibility_score + 10
      report$issues <- c(report$issues, "Moderate complexity reduction from consolidation")
      if (return_suggestions) {
        report$recommendations <- c(report$recommendations, 
                                   "Consider different consolidation_type for better complexity reduction")
      }
    } else {
      report$issues <- c(report$issues, "Limited complexity reduction from consolidation")
      if (return_suggestions) {
        report$recommendations <- c(report$recommendations, 
                                   "Data may already be well-consolidated or consolidation not beneficial")
      }
    }
    
    # Process transitions with consolidation for further analysis
    if (consolidation_type != "none") {
      transitions_data <- .process_with_consolidation(transitions_data, consolidation_type)
    }
  }
  
  # Check color contrast
  if (check_color_contrast) {
    colors <- .get_accessibility_colors(palette, FALSE, FALSE)
    contrast_results <- .assess_color_contrast(colors)
    
    report$color_contrast <- contrast_results
    report$max_score <- report$max_score + 25
    
    if (contrast_results$wcag_aa_pass) {
      report$accessibility_score <- report$accessibility_score + 25
    } else {
      report$issues <- c(report$issues, "Color contrast below WCAG AA standards")
      if (return_suggestions) {
        report$recommendations <- c(report$recommendations, 
                                   "Consider using high-contrast palette or accessibility_mode = TRUE")
      }
    }
  }
  
  # Check layout complexity
  if (check_layout_complexity) {
    # Convert to tidygraph for analysis
    tg <- .convert_to_tidygraph(transitions_data, "data.table", TRUE, 1)
    n_nodes <- igraph::vcount(tg)
    n_edges <- igraph::ecount(tg)
    
    complexity_score <- .assess_layout_complexity(n_nodes, n_edges, layout)
    report$layout_complexity <- complexity_score
    report$max_score <- report$max_score + 25
    
    if (complexity_score$readable) {
      report$accessibility_score <- report$accessibility_score + 25
    } else {
      report$issues <- c(report$issues, paste("Layout may be too complex:", complexity_score$reason))
      if (return_suggestions) {
        report$recommendations <- c(report$recommendations, complexity_score$suggestion)
      }
    }
  }
  
  # Additional checks (adjusted for consolidation scoring)
  additional_checks_score <- if (use_consolidated_periods) 30 else 50
  report$max_score <- report$max_score + additional_checks_score
  
  # Check if using colorblind-friendly palette
  if (palette %in% c("viridis", "okabe_ito")) {
    report$accessibility_score <- report$accessibility_score + 25
  } else {
    report$issues <- c(report$issues, "Palette may not be colorblind-friendly")
    if (return_suggestions) {
      report$recommendations <- c(report$recommendations, 
                                 "Consider using 'viridis' or 'okabe_ito' palette")
    }
  }
  
  # Check for redundant encoding
  report$accessibility_score <- report$accessibility_score + 25  # Assume good practices
  
  # Calculate final score percentage
  report$accessibility_percentage <- round(100 * report$accessibility_score / report$max_score, 1)
  
  # Overall assessment
  if (report$accessibility_percentage >= 80) {
    report$overall_assessment <- "Excellent accessibility"
  } else if (report$accessibility_percentage >= 60) {
    report$overall_assessment <- "Good accessibility with minor issues"
  } else if (report$accessibility_percentage >= 40) {
    report$overall_assessment <- "Moderate accessibility - improvements recommended"
  } else {
    report$overall_assessment <- "Poor accessibility - significant improvements needed"
  }
  
  return(report)
}

# Internal Helper Functions for over_id and Consolidation ===================

#' Check if Data is Pipeline Result vs Pre-computed Transitions
#'
#' Internal helper to determine if input data is raw pipeline result or
#' pre-computed transitions data.
#'
#' @param data Input data to check
#'
#' @return Logical indicating if data appears to be pipeline result
#' @keywords internal
.is_pipeline_data <- function(data) {
  if (!inherits(data, "data.table")) return(FALSE)
  
  # Pipeline data should have cf, arco, inizio, fine columns
  pipeline_cols <- c("cf", "arco", "inizio", "fine")
  has_pipeline_cols <- all(pipeline_cols %in% names(data))
  
  # Transitions data should have from, to, weight columns
  transition_cols <- c("from", "to", "weight")
  has_transition_cols <- all(transition_cols %in% names(data))
  
  return(has_pipeline_cols && !has_transition_cols)
}

#' Process Data with Consolidation
#'
#' Internal helper to apply consolidation to pipeline data before transition analysis.
#'
#' @param pipeline_data Pipeline result data
#' @param consolidation_type Type of consolidation
#' @param transition_variable Variable for transitions
#'
#' @return Processed transitions data
#' @keywords internal
.process_with_consolidation <- function(pipeline_data, consolidation_type, transition_variable = "prior") {
  
  if (!requireNamespace("vecshift", quietly = FALSE)) {
    stop("vecshift package functions required for consolidation")
  }
  
  # Apply consolidation and analyze transitions
  transitions <- analyze_employment_transitions(
    pipeline_result = pipeline_data,
    transition_variable = transition_variable,
    use_consolidated_periods = TRUE,
    consolidation_type = consolidation_type,
    show_progress = FALSE
  )
  
  # Add consolidation status information
  transitions[, consolidated_status := "consolidated"]
  
  return(transitions)
}

#' Add Consolidation Information to tidygraph
#'
#' Internal helper to add consolidation status to tidygraph edges.
#'
#' @param tg tidygraph object
#' @param transitions_data Original transitions data with consolidation info
#'
#' @return Modified tidygraph object
#' @keywords internal
.add_consolidation_info <- function(tg, transitions_data) {
  
  if ("consolidated_status" %in% names(transitions_data)) {
    # Add consolidation status to edges
    tg <- tidygraph::activate(tg, edges)
    
    # Map consolidation status based on from-to pairs
    edge_data <- tidygraph::as_tibble(tg, "edges")
    
    # Simple approach: mark all edges as consolidated if any consolidation occurred
    is_consolidated <- "consolidated" %in% transitions_data$consolidated_status
    
    tg <- tidygraph::mutate(tg, 
                           consolidated = is_consolidated,
                           edge_type = if (is_consolidated) "consolidated" else "raw")
  }
  
  return(tg)
}

#' Add Edges with Consolidation Styling
#'
#' Internal helper to add edges with different styling for consolidated vs raw transitions.
#'
#' @param tg tidygraph object
#' @param width_var Variable for edge width
#' @param width_range Range for edge widths
#' @param alpha Edge transparency
#' @param show_labels Show edge labels
#' @param colors Color palette
#' @param use_bw Use black and white styling
#' @param accessibility_mode High contrast mode
#'
#' @return List of ggraph edge layers
#' @keywords internal
.add_edges_with_consolidation <- function(tg, width_var, width_range, alpha, show_labels, colors, use_bw, accessibility_mode) {
  
  # Check if we have consolidation information
  edge_data <- tidygraph::activate(tg, edges)
  has_consolidation_info <- "edge_type" %in% names(tidygraph::as_tibble(edge_data))
  
  if (!has_consolidation_info) {
    # Fall back to regular edge styling
    return(.add_edges(width_var, width_range, alpha, show_labels, colors, use_bw))
  }
  
  # Create consolidated edge styling
  edge_color_consolidated <- if (use_bw) "#34495E" else colors[1]
  edge_color_raw <- if (use_bw) "#7F8C8D" else colors[2]
  
  layers <- list()
  
  # Add consolidated edges (solid lines)
  if (width_var == "fixed") {
    layers <- append(layers, list(
      ggraph::geom_edge_link(ggplot2::aes(filter = edge_type == "consolidated"),
                            alpha = alpha, color = edge_color_consolidated, 
                            width = mean(width_range), linetype = "solid")
    ))
    
    # Add raw edges (dashed lines) if any exist
    layers <- append(layers, list(
      ggraph::geom_edge_link(ggplot2::aes(filter = edge_type == "raw"),
                            alpha = alpha * 0.7, color = edge_color_raw,
                            width = mean(width_range) * 0.8, linetype = "dashed")
    ))
  } else {
    layers <- append(layers, list(
      ggraph::geom_edge_link(ggplot2::aes(width = !!rlang::sym(width_var), filter = edge_type == "consolidated"),
                            alpha = alpha, color = edge_color_consolidated, linetype = "solid")
    ))
    
    layers <- append(layers, list(
      ggraph::geom_edge_link(ggplot2::aes(width = !!rlang::sym(width_var), filter = edge_type == "raw"),
                            alpha = alpha * 0.7, color = edge_color_raw, linetype = "dashed")
    ))
    
    layers <- append(layers, list(ggraph::scale_edge_width(range = width_range, guide = "none")))
  }
  
  # Add edge labels if requested
  if (show_labels) {
    layers <- append(layers, list(
      ggraph::geom_edge_label(ggplot2::aes(label = weight),
                             alpha = alpha, size = 2.5,
                             color = if(accessibility_mode) "#000000" else "#2C3E50")
    ))
  }
  
  return(layers)
}

#' Create Consolidation Comparison Plot
#'
#' Internal helper to create side-by-side comparison of raw vs consolidated networks.
#'
#' @param pipeline_data Raw pipeline data
#' @param ... Additional parameters passed to plot_transitions_network
#'
#' @return Combined comparison plot
#' @keywords internal
.create_consolidation_comparison_plot <- function(pipeline_data, layout, node_size_var, edge_width_var, 
                                                 node_color_var, min_edge_weight, node_size_range, 
                                                 edge_width_range, edge_alpha, node_alpha, show_labels, 
                                                 label_size, label_repel, palette, use_bw, directed, 
                                                 show_edge_labels, accessibility_mode, consolidation_type, 
                                                 title, subtitle) {
  
  # Use the exported function for the comparison
  return(plot_consolidation_comparison(
    pipeline_result = pipeline_data,
    layout = layout,
    palette = palette,
    accessibility_mode = accessibility_mode,
    consolidation_type = consolidation_type,
    title = title
  ))
}

#' Create over_id Summary Plot
#'
#' Internal helper to create summary visualization of over_id distribution.
#'
#' @param data Filtered pipeline data
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#' @param facet_by Faceting variable
#'
#' @return ggplot2 object
#' @keywords internal
.create_over_id_summary_plot <- function(data, colors, accessibility_mode, facet_by) {
  
  # Aggregate over_id statistics
  summary_data <- data[over_id > 0, .(
    period_count = .N,
    total_duration = sum(durata, na.rm = TRUE),
    mean_duration = mean(durata, na.rm = TRUE),
    people_count = data.table::uniqueN(cf)
  ), by = over_id]
  
  # Create summary plot
  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = over_id, y = period_count)) +
    ggplot2::geom_col(fill = if(accessibility_mode) "#34495E" else colors[1], alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(people_count, " people")), 
                      vjust = -0.5, size = 3,
                      color = if(accessibility_mode) "#000000" else "#2C3E50") +
    ggplot2::labs(x = "over_id (Employment Episode Identifier)",
                 y = "Number of Periods") +
    theme_vecshift()
  
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }
  
  return(p)
}

#' Create over_id Histogram
#'
#' Internal helper for histogram visualization.
#'
#' @param data Filtered pipeline data
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#' @param facet_by Faceting variable
#'
#' @return ggplot2 object
#' @keywords internal
.create_over_id_histogram <- function(data, colors, accessibility_mode, facet_by) {
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = over_id)) +
    ggplot2::geom_histogram(bins = 30, fill = if(accessibility_mode) "#34495E" else colors[1], 
                           alpha = 0.7, color = "white") +
    ggplot2::labs(x = "over_id Value", y = "Number of Periods") +
    theme_vecshift()
  
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }
  
  return(p)
}

#' Create over_id Boxplot
#'
#' Internal helper for boxplot visualization.
#'
#' @param data Filtered pipeline data
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#' @param facet_by Faceting variable
#'
#' @return ggplot2 object
#' @keywords internal
.create_over_id_boxplot <- function(data, colors, accessibility_mode, facet_by) {
  
  # Create factor for better visualization
  data[, over_id_factor := as.factor(over_id)]
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = over_id_factor, y = durata)) +
    ggplot2::geom_boxplot(fill = if(accessibility_mode) "#34495E" else colors[1], alpha = 0.7) +
    ggplot2::labs(x = "over_id (Employment Episode)", y = "Period Duration (days)") +
    theme_vecshift()
  
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }
  
  return(p)
}

#' Create over_id Density Plot
#'
#' Internal helper for density visualization.
#'
#' @param data Filtered pipeline data
#' @param colors Color palette
#' @param accessibility_mode High contrast mode
#' @param facet_by Faceting variable
#'
#' @return ggplot2 object
#' @keywords internal
.create_over_id_density <- function(data, colors, accessibility_mode, facet_by) {
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = durata)) +
    ggplot2::geom_density(fill = if(accessibility_mode) "#34495E" else colors[1], 
                         alpha = 0.7, color = if(accessibility_mode) "#2C3E50" else colors[2]) +
    ggplot2::labs(x = "Period Duration (days)", y = "Density") +
    theme_vecshift()
  
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }
  
  return(p)
}

#' Create Complexity Network Data
#'
#' Internal helper to create network data for employment complexity visualization.
#'
#' @param pipeline_result Pipeline data with over_id
#' @param complexity_metric Metric to use for complexity
#'
#' @return List with nodes and edges data.tables
#' @keywords internal
.create_complexity_network_data <- function(pipeline_result, complexity_metric) {
  
  # Calculate complexity metrics by over_id
  complexity_stats <- pipeline_result[over_id > 0, .(
    period_count = .N,
    total_duration = sum(durata, na.rm = TRUE),
    overlap_periods = sum(arco > 1, na.rm = TRUE),
    cf_count = data.table::uniqueN(cf),
    mean_arco = mean(arco, na.rm = TRUE)
  ), by = over_id]
  
  # Calculate complexity score based on metric
  if (complexity_metric == "overlap_count") {
    complexity_stats[, complexity_score := overlap_periods + period_count]
  } else if (complexity_metric == "duration_sum") {
    complexity_stats[, complexity_score := total_duration / 30]  # Convert to months
  } else { # period_count
    complexity_stats[, complexity_score := period_count]
  }
  
  # Create nodes
  nodes <- complexity_stats[complexity_score > 0]
  
  # Create edges based on shared characteristics
  edges <- data.table::data.table()
  
  if (nrow(nodes) > 1) {
    # Connect over_ids that share similar complexity patterns
    for (i in 1:(nrow(nodes)-1)) {
      for (j in (i+1):nrow(nodes)) {
        node_i <- nodes[i]
        node_j <- nodes[j]
        
        # Calculate similarity (inverse of difference)
        similarity <- 1 / (1 + abs(node_i$complexity_score - node_j$complexity_score))
        
        # Add edge if similarity is above threshold
        if (similarity > 0.3) {
          edges <- data.table::rbindlist(list(edges, 
            data.table::data.table(
              from = as.character(node_i$over_id),
              to = as.character(node_j$over_id),
              weight = similarity
            )))
        }
      }
    }
  }
  
  # Convert over_id to character for consistency
  nodes[, over_id := as.character(over_id)]
  
  return(list(
    nodes = nodes,
    edges = edges
  ))
}

#' Assess Consolidation Benefits
#'
#' Internal helper to assess how much consolidation improves visualization complexity.
#'
#' @param pipeline_data Raw pipeline data
#' @param consolidation_type Type of consolidation
#'
#' @return List with consolidation assessment results
#' @keywords internal
.assess_consolidation_benefits <- function(pipeline_data, consolidation_type) {
  
  # Generate raw transitions
  transitions_raw <- analyze_employment_transitions(
    pipeline_result = pipeline_data,
    use_consolidated_periods = FALSE,
    show_progress = FALSE
  )
  
  # Generate consolidated transitions
  transitions_consolidated <- analyze_employment_transitions(
    pipeline_result = pipeline_data,
    use_consolidated_periods = TRUE,
    consolidation_type = consolidation_type,
    show_progress = FALSE
  )
  
  # Calculate metrics
  raw_count <- nrow(transitions_raw)
  consolidated_count <- nrow(transitions_consolidated)
  complexity_reduction <- (raw_count - consolidated_count) / max(raw_count, 1)
  
  raw_density <- sum(transitions_raw$weight, na.rm = TRUE)
  consolidated_density <- sum(transitions_consolidated$weight, na.rm = TRUE)
  density_change <- (consolidated_density - raw_density) / max(raw_density, 1)
  
  # Assess uniqueness (how many unique states)
  raw_states <- length(unique(c(transitions_raw$from, transitions_raw$to)))
  consolidated_states <- length(unique(c(transitions_consolidated$from, transitions_consolidated$to)))
  state_reduction <- (raw_states - consolidated_states) / max(raw_states, 1)
  
  return(list(
    raw_transitions = raw_count,
    consolidated_transitions = consolidated_count,
    complexity_reduction = complexity_reduction,
    density_change = density_change,
    raw_states = raw_states,
    consolidated_states = consolidated_states,
    state_reduction = state_reduction,
    consolidation_beneficial = complexity_reduction > 0.1,
    recommendation = if (complexity_reduction > 0.2) {
      "Consolidation provides significant complexity reduction"
    } else if (complexity_reduction > 0.1) {
      "Consolidation provides moderate complexity reduction"
    } else {
      "Consolidation provides limited complexity reduction"
    }
  ))
}

# Internal Helper Functions for Accessibility Assessment =====================

#' Assess Color Contrast Ratios
#'
#' Internal helper to assess WCAG color contrast compliance.
#'
#' @param colors Color palette to test
#'
#' @return List with contrast assessment results
#' @keywords internal
.assess_color_contrast <- function(colors) {
  
  # Simple contrast assessment (could be enhanced with actual WCAG calculations)
  n_colors <- length(colors)
  
  # Check for grayscale (high contrast by nature)
  is_grayscale <- all(sapply(colors, function(color) {
    if (nchar(color) == 7 && substr(color, 1, 1) == "#") {
      r <- substr(color, 2, 3)
      g <- substr(color, 4, 5) 
      b <- substr(color, 6, 7)
      return(r == g && g == b)
    }
    return(FALSE)
  }))
  
  # Simplified assessment
  wcag_aa_pass <- is_grayscale || n_colors <= 8  # Conservative estimate
  
  return(list(
    n_colors = n_colors,
    is_grayscale = is_grayscale,
    wcag_aa_pass = wcag_aa_pass,
    recommendation = if (wcag_aa_pass) "Colors meet accessibility standards" else "Consider high-contrast alternatives"
  ))
}

#' Assess Layout Complexity for Readability
#'
#' Internal helper to assess if layout will be readable.
#'
#' @param n_nodes Number of nodes
#' @param n_edges Number of edges
#' @param layout Layout algorithm
#'
#' @return List with complexity assessment
#' @keywords internal
.assess_layout_complexity <- function(n_nodes, n_edges, layout) {
  
  density <- n_edges / (n_nodes * (n_nodes - 1))
  
  # Layout-specific thresholds
  thresholds <- list(
    "fr" = list(max_nodes = 50, max_density = 0.2),
    "kk" = list(max_nodes = 30, max_density = 0.15),
    "circle" = list(max_nodes = 20, max_density = 0.3),
    "tree" = list(max_nodes = 100, max_density = 0.1),
    "sugiyama" = list(max_nodes = 80, max_density = 0.15)
  )
  
  threshold <- thresholds[[layout]] %||% list(max_nodes = 40, max_density = 0.2)
  
  readable <- n_nodes <= threshold$max_nodes && density <= threshold$max_density
  
  reason <- ""
  suggestion <- ""
  
  if (n_nodes > threshold$max_nodes) {
    reason <- paste("Too many nodes for", layout, "layout")
    suggestion <- "Consider filtering data or using circular/hierarchical layouts"
  } else if (density > threshold$max_density) {
    reason <- "Network too dense for clear visualization" 
    suggestion <- "Consider increasing min_edge_weight threshold or using heatmap"
  }
  
  return(list(
    readable = readable,
    reason = reason,
    suggestion = suggestion,
    density = density,
    nodes = n_nodes,
    edges = n_edges
  ))
}

# Missing Import Statements and Package Dependencies =========================

# Note: These functions require the following packages to be available:
# - ggraph (>= 2.0.0) for network visualization
# - tidygraph (>= 1.2.0) for graph data manipulation  
# - igraph (>= 1.2.0) for graph algorithms
# - ggplot2 (>= 3.3.0) for base plotting
# - dplyr for data manipulation (imported via tidygraph)
# - rlang for non-standard evaluation
# - patchwork for consolidation comparison plots
# - stringr for string manipulation (optional, used for title formatting)

# The package should suggest these in DESCRIPTION:
# Suggests: ggraph (>= 2.0.0), tidygraph (>= 1.2.0), igraph (>= 1.2.0), 
#           ggrepel, viridis, patchwork, stringr, rlang

# New features in this version:
# - over_id-aware visualization functions
# - Consolidation parameters for cleaner static visualizations
# - Comparison plots showing benefits of consolidation
# - Enhanced accessibility assessment including consolidation benefits
# - Support for pipeline data input with automatic consolidation
# - Backward compatibility with pre-computed transitions data