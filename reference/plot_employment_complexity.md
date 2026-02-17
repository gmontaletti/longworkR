# Plot Employment Complexity Using over_id

Creates network-style visualizations showing employment complexity
patterns using over_id groupings. Visualizes how employment periods
cluster together and the relationships between overlapping employment
episodes.

## Usage

``` r
plot_employment_complexity(
  pipeline_result,
  complexity_metric = "overlap_count",
  layout = "fr",
  node_size_range = c(2, 12),
  edge_width_range = c(0.3, 2),
  palette = "viridis",
  accessibility_mode = FALSE,
  show_labels = TRUE,
  title = NULL
)
```

## Arguments

- pipeline_result:

  Output from vecshift() with over_id column

- complexity_metric:

  Character. Metric to use: "overlap_count", "duration_sum",
  "period_count" (default: "overlap_count")

- layout:

  Character. Network layout algorithm (default: "fr")

- node_size_range:

  Numeric vector. Size range for nodes (default: c(2, 12))

- edge_width_range:

  Numeric vector. Width range for edges (default: c(0.3, 2))

- palette:

  Character. Color palette (default: "viridis")

- accessibility_mode:

  Logical. Use high contrast mode (default: FALSE)

- show_labels:

  Logical. Show over_id labels (default: TRUE)

- title:

  Character. Custom title (default: auto-generated)

## Value

A ggplot2 network object showing employment complexity

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic complexity visualization
plot_employment_complexity(pipeline_result)

# Duration-based complexity with circular layout
plot_employment_complexity(pipeline_result, complexity_metric = "duration_sum", layout = "circle")
} # }
```
