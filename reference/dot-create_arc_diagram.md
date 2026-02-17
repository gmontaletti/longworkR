# Create Arc Diagram

Internal helper to create arc-style diagrams.

## Usage

``` r
.create_arc_diagram(
  tg,
  colors,
  node_size_range,
  edge_width_range,
  show_direction,
  label_distance,
  accessibility_mode
)
```

## Arguments

- tg:

  tidygraph object

- colors:

  Color palette

- node_size_range:

  Node size range

- edge_width_range:

  Edge width range

- show_direction:

  Show flow direction

- label_distance:

  Distance for labels

- accessibility_mode:

  High contrast mode

## Value

ggraph plot
