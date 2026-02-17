# Create Chord Diagram

Internal helper to create chord-style circular diagrams.

## Usage

``` r
.create_chord_diagram(
  tg,
  colors,
  node_size_range,
  edge_width_range,
  show_direction,
  curve,
  show_percentages,
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

- curve:

  Edge curvature

- show_percentages:

  Show percentages

- accessibility_mode:

  High contrast mode

## Value

ggraph plot
