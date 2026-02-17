# Plot Employment Transitions - Circular Layout

Creates circular network layouts optimized for employment transition
analysis. Arranges nodes in a circle with transitions shown as chords or
arcs. Particularly effective for showing flow patterns and state
relationships.

## Usage

``` r
plot_transitions_circular(
  transitions_data,
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
  consolidation_mode = "temporal",
  consolidation_type = "both",
  show_percentages = FALSE,
  label_distance = 1.1,
  title = NULL,
  subtitle = NULL
)
```

## Arguments

- transitions_data:

  Data.table output from analyze_employment_transitions()

- input_format:

  Character. Format of input data: "data.table" or "matrix" (default:
  "data.table")

- circular_type:

  Character. Type of circular layout: "chord", "arc", "wheel" (default:
  "chord")

- node_order:

  Character. How to order nodes: "alphabetical", "frequency", "cluster",
  "manual" (default: "frequency")

- manual_order:

  Character vector. Manual node order (when node_order = "manual")

- show_flow_direction:

  Logical. Show directional arrows for flows (default: TRUE)

- edge_curve:

  Numeric. Curvature for edges (0 = straight, 1 = very curved) (default:
  0.1)

- min_edge_weight:

  Numeric. Minimum edge weight to display (default: 1)

- node_size_var:

  Character. Variable for node sizing (default: "strength")

- node_size_range:

  Numeric vector. Size range for nodes (default: c(4, 20))

- edge_width_range:

  Numeric vector. Width range for edges (default: c(0.5, 5))

- palette:

  Character. Color palette (default: "viridis")

- use_bw:

  Logical. Use black and white version (default: FALSE)

- accessibility_mode:

  Logical. Enable accessibility mode (default: FALSE)

- consolidation_mode:

  Character. Consolidation mode: "temporal", "employer", or "none"
  (default: "temporal")

- consolidation_type:

  Character. Type of consolidation (default: "both")

- show_percentages:

  Logical. Show edge percentages instead of raw counts (default: FALSE)

- label_distance:

  Numeric. Distance of labels from circle (default: 1.1)

- title:

  Character. Plot title (default: auto-generated)

- subtitle:

  Character. Plot subtitle (default: auto-generated)

## Value

A ggplot2 object showing the circular network

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic consolidated circular chord diagram
plot_transitions_circular(pipeline_result)

# Arc diagram with frequency-based ordering, overlapping consolidation only
plot_transitions_circular(pipeline_result, circular_type = "arc", node_order = "frequency",
                         consolidation_type = "overlapping")

# Wheel layout with percentages, no consolidation
plot_transitions_circular(pipeline_result, circular_type = "wheel", show_percentages = TRUE,
                         consolidation_mode = "none")
} # }
```
