# Plot over_id Distribution Analysis

Creates visualizations showing the distribution of over_id values to
understand employment complexity patterns. Shows how many overlapping
employment periods each person has and the duration patterns of these
overlapping groups.

## Usage

``` r
plot_over_id_distribution(
  pipeline_result,
  plot_type = "summary",
  include_unemployment = FALSE,
  facet_by = NULL,
  palette = "viridis",
  accessibility_mode = FALSE,
  title = NULL
)
```

## Arguments

- pipeline_result:

  Output from vecshift() with over_id column

- plot_type:

  Character. Type of visualization: "histogram", "boxplot", "density",
  "summary" (default: "summary")

- include_unemployment:

  Logical. Include over_id = 0 (unemployment) in analysis (default:
  FALSE)

- facet_by:

  Character. Variable to facet by (optional)

- palette:

  Character. Color palette (default: "viridis")

- accessibility_mode:

  Logical. Use high contrast mode (default: FALSE)

- title:

  Character. Custom title (default: auto-generated)

## Value

A ggplot2 object showing over_id distribution

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic over_id distribution
plot_over_id_distribution(pipeline_result)

# Histogram including unemployment periods  
plot_over_id_distribution(pipeline_result, plot_type = "histogram", include_unemployment = TRUE)

# Density plot by employment type
plot_over_id_distribution(pipeline_result, plot_type = "density", facet_by = "prior")
} # }
```
