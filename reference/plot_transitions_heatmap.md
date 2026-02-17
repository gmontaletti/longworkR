# Plot Employment Transitions Heatmap

Creates heatmap visualizations for transition matrices, showing the
intensity of transitions between different employment states. Supports
both matrix and data.table inputs with comprehensive accessibility
features.

## Usage

``` r
plot_transitions_heatmap(
  transitions_data,
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
  consolidation_mode = "temporal",
  consolidation_type = "both",
  aspect_ratio = 1,
  border_color = "white",
  border_size = 0.5,
  title = NULL,
  subtitle = NULL
)
```

## Arguments

- transitions_data:

  Data.table output from analyze_employment_transitions() or transition
  matrix when input_format = "matrix"

- input_format:

  Character. Format of input data: "data.table" or "matrix" (default:
  "data.table")

- cell_value:

  Character. What to display in cells: "weight", "percentage", "both",
  "none" (default: "weight")

- normalize:

  Character. How to normalize values: "none", "row", "column", "total"
  (default: "none")

- min_value_display:

  Numeric. Minimum value to display in cells (default: 1)

- color_scale:

  Character. Type of color scale: "gradient", "diverging", "discrete"
  (default: "gradient")

- palette:

  Character. Color palette (default: "viridis")

- use_bw:

  Logical. Use black and white version (default: FALSE)

- text_color:

  Character. Color for cell text: "auto", "white", "black" (default:
  "auto")

- text_size:

  Numeric. Size of cell text (default: 3)

- show_marginals:

  Logical. Show row/column marginal sums (default: FALSE)

- accessibility_mode:

  Logical. Enable high contrast mode (default: FALSE)

- consolidation_mode:

  Character. Consolidation mode: "temporal", "employer", or "none"
  (default: "temporal")

- consolidation_type:

  Character. Type of consolidation when consolidation_mode != "none"
  (default: "both")

- aspect_ratio:

  Numeric. Aspect ratio for cells (default: 1)

- border_color:

  Character. Color for cell borders (default: "white")

- border_size:

  Numeric. Size of cell borders (default: 0.5)

- title:

  Character. Plot title (default: auto-generated)

- subtitle:

  Character. Plot subtitle (default: auto-generated)

## Value

A ggplot2 object showing the heatmap

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic consolidated heatmap (default)
plot_transitions_heatmap(pipeline_result)

# Row-normalized with percentages and overlapping consolidation only
plot_transitions_heatmap(pipeline_result, normalize = "row", cell_value = "percentage",
                        consolidation_type = "overlapping")

# High contrast accessibility mode with no consolidation
plot_transitions_heatmap(pipeline_result, accessibility_mode = TRUE, 
                        consolidation_mode = "none")

# Using pre-computed transitions data
transitions <- analyze_employment_transitions(pipeline_result)
plot_transitions_heatmap(transitions, input_format = "data.table")
} # }
```
