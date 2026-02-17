# Create Interactive Employment Transition Visualization

Creates an interactive network visualization of employment transitions
using g6r. Provides multiple layout options, interactive behaviors, and
customization features suitable for exploring employment pattern data.

## Usage

``` r
plot_interactive_transitions(
  transitions_data,
  layout = "force",
  width = "100%",
  height = "600px",
  show_labels = TRUE,
  enable_zoom = TRUE,
  enable_drag = TRUE,
  enable_select = TRUE,
  show_minimap = TRUE,
  show_tooltip = TRUE,
  edge_bundling = FALSE,
  animation_duration = 1000,
  node_color_palette = NULL,
  accessibility_mode = FALSE,
  show_consolidation_legend = TRUE,
  ...
)
```

## Arguments

- transitions_data:

  Output from analyze_employment_transitions() or transition matrix. Can
  be consolidated or non-consolidated transition data.

- layout:

  Character string specifying the layout algorithm. Options include:
  "force" (force-directed), "circular", "radial", "dagre"
  (hierarchical), "concentric", "grid", "fruchterman", "kamada_kawai".
  Default: "force"

- width:

  Width of the visualization container. Default: "100%"

- height:

  Height of the visualization container. Default: "600px"

- show_labels:

  Logical. Whether to show node labels. Default: TRUE

- enable_zoom:

  Logical. Whether to enable zoom interactions. Default: TRUE

- enable_drag:

  Logical. Whether to enable drag interactions. Default: TRUE

- enable_select:

  Logical. Whether to enable selection interactions. Default: TRUE

- show_minimap:

  Logical. Whether to show minimap plugin. Default: TRUE

- show_tooltip:

  Logical. Whether to show hover tooltips. Default: TRUE

- edge_bundling:

  Logical. Whether to enable edge bundling for cleaner visualization of
  dense networks. Default: FALSE

- animation_duration:

  Numeric. Duration of layout animations in milliseconds. Default: 1000.
  Set to 0 to disable animations.

- node_color_palette:

  Character vector of colors for nodes. Default: NULL (uses viridis)

- accessibility_mode:

  Logical. Whether to optimize for accessibility with high contrast and
  redundant encoding. Default: FALSE

- show_consolidation_legend:

  Logical. Whether to show legend explaining consolidated vs raw
  transitions when applicable. Default: TRUE

- ...:

  Additional parameters passed to convert_transitions_to_g6r()

## Value

A g6R htmlwidget object that can be displayed in R or embedded in Shiny
apps

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(g6R)

# Create and process sample data (see convert_transitions_to_g6r examples)
transitions <- analyze_employment_transitions(result, transition_variable = "company")

# Basic interactive visualization
plot_interactive_transitions(transitions)

# Customized visualization with different layout
plot_interactive_transitions(
  transitions,
  layout = "circular",
  show_minimap = FALSE,
  accessibility_mode = TRUE,
  height = "800px"
)

# Focus on significant transitions only
plot_interactive_transitions(
  transitions,
  layout = "dagre",
  min_weight_threshold = 5,
  edge_bundling = TRUE
)
} # }
```
