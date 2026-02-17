# Plot Employment Transitions Network

Creates network visualizations of employment transitions using ggraph
and tidygraph. Supports multiple layout algorithms, accessibility
features, and customization options. Designed to work with output from
analyze_employment_transitions(). Now includes support for consolidated
transitions using over_id grouping for cleaner visualizations.

## Usage

``` r
plot_transitions_network(
  transitions_data,
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
  consolidation_mode = "temporal",
  consolidation_type = "both",
  show_consolidation_comparison = FALSE,
  title = NULL,
  subtitle = NULL
)
```

## Arguments

- transitions_data:

  Data.table output from analyze_employment_transitions(), or a
  transition matrix when input_format = "matrix". When using
  consolidated transitions (consolidation_mode = "temporal"), provides
  cleaner network structure.

- input_format:

  Character. Format of input data: "data.table" or "matrix" (default:
  "data.table")

- layout:

  Character. Layout algorithm for network:

  - "fr": Fruchterman-Reingold (default) - good for general networks

  - "kk": Kamada-Kawai - preserves distances, good for smaller networks

  - "dh": Davidson-Harel - minimizes edge crossings

  - "gem": GEM algorithm - fast spring layout

  - "graphopt": Graphopt algorithm - good clustering

  - "mds": Multidimensional scaling - preserves distance relationships

  - "randomly": Random layout - for testing

  - "circle": Circular layout - equal emphasis on all nodes

  - "sphere": 3D sphere projection to 2D

  - "grid": Grid-based layout

- node_size_var:

  Character. Variable to map to node size: "degree", "strength",
  "in_degree", "out_degree", "betweenness", "closeness", "fixed"
  (default: "strength")

- edge_width_var:

  Character. Variable to map to edge width: "weight",
  "transition_duration", "fixed" (default: "weight")

- node_color_var:

  Character. Variable to map to node color: "community", "degree",
  "fixed", "status" (default: "community")

- min_edge_weight:

  Numeric. Minimum edge weight to display (default: 1)

- node_size_range:

  Numeric vector of length 2. Size range for nodes (default: c(3, 15))

- edge_width_range:

  Numeric vector of length 2. Width range for edges (default: c(0.5, 3))

- edge_alpha:

  Numeric. Transparency for edges (default: 0.6)

- node_alpha:

  Numeric. Transparency for nodes (default: 0.8)

- show_labels:

  Logical. Show node labels (default: TRUE)

- label_size:

  Numeric. Size of node labels (default: 3)

- label_repel:

  Logical. Use repelling for labels to avoid overlap (default: TRUE)

- palette:

  Character. Color palette: "viridis", "okabe_ito", "employment",
  "main", "colorbrewer_set2" (default: "viridis")

- use_bw:

  Logical. Use black and white version (default: FALSE)

- directed:

  Logical. Treat graph as directed (default: TRUE)

- show_edge_labels:

  Logical. Show edge weight labels (default: FALSE)

- accessibility_mode:

  Logical. Enable high contrast accessibility mode (default: FALSE)

- consolidation_mode:

  Character. Consolidation mode for analyze_employment_transitions()
  call: "temporal" (default), "employer", or "none". Used for cleaner
  network visualization. Requires pipeline data with over_id column.
  Ignored when transitions_data is pre-computed.

- consolidation_type:

  Character. Type of consolidation (default: "both")

- show_consolidation_comparison:

  Logical. If TRUE, create side-by-side comparison of raw vs
  consolidated transitions (default: FALSE). Only works with pipeline
  data input.

- title:

  Character. Plot title (default: auto-generated)

- subtitle:

  Character. Plot subtitle (default: auto-generated)

## Value

A ggplot2 object showing the network visualization

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(ggraph)
library(tidygraph)

# Create sample data
sample_data <- create_sample_employment_data(n_people = 50, n_periods = 4)
pipeline_result <- vecshift(sample_data)  # Creates over_id column

# Basic consolidated network plot (default)
plot_transitions_network(pipeline_result, transition_variable = "prior")

# Compare raw vs consolidated transitions
plot_transitions_network(pipeline_result, show_consolidation_comparison = TRUE)

# Use only overlapping period consolidation
plot_transitions_network(pipeline_result, consolidation_type = "overlapping")

# Kamada-Kawai layout with degree-based node sizing
plot_transitions_network(pipeline_result, layout = "kk", node_size_var = "degree")

# Accessibility mode with high contrast
plot_transitions_network(pipeline_result, accessibility_mode = TRUE, use_bw = TRUE)

# Using pre-computed transitions (no consolidation applied)
transitions <- analyze_employment_transitions(pipeline_result, transition_variable = "prior")
plot_transitions_network(transitions, consolidation_mode = "none")
} # }
```
