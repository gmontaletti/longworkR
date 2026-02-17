# Create Wheel Diagram

Internal helper to create wheel-style diagrams.

## Usage

``` r
.create_wheel_diagram(
  tg,
  colors,
  node_size_range,
  edge_width_range,
  curve,
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

- curve:

  Edge curvature

- label_distance:

  Distance for labels

- accessibility_mode:

  High contrast mode

## Value

ggraph plot
