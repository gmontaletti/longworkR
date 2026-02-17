# Create Consolidation Comparison Plot

Internal helper to create side-by-side comparison of raw vs consolidated
networks.

## Usage

``` r
.create_consolidation_comparison_plot(
  pipeline_data,
  layout,
  node_size_var,
  edge_width_var,
  node_color_var,
  min_edge_weight,
  node_size_range,
  edge_width_range,
  edge_alpha,
  node_alpha,
  show_labels,
  label_size,
  label_repel,
  palette,
  use_bw,
  directed,
  show_edge_labels,
  accessibility_mode,
  consolidation_type,
  title,
  subtitle
)
```

## Arguments

- pipeline_data:

  Raw pipeline data

- ...:

  Additional parameters passed to plot_transitions_network

## Value

Combined comparison plot
