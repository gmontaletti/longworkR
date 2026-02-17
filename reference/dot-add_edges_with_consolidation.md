# Add Edges with Consolidation Styling

Internal helper to add edges with different styling for consolidated vs
raw transitions.

## Usage

``` r
.add_edges_with_consolidation(
  tg,
  width_var,
  width_range,
  alpha,
  show_labels,
  colors,
  use_bw,
  accessibility_mode
)
```

## Arguments

- tg:

  tidygraph object

- width_var:

  Variable for edge width

- width_range:

  Range for edge widths

- alpha:

  Edge transparency

- show_labels:

  Show edge labels

- colors:

  Color palette

- use_bw:

  Use black and white styling

- accessibility_mode:

  High contrast mode

## Value

List of ggraph edge layers
