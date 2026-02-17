# Plot Duration Analysis

Analyzes and visualizes the duration of employment periods by status,
showing distributions, comparisons, and patterns in employment lengths.

## Usage

``` r
plot_duration_analysis(
  data,
  duration_col = "durata",
  status_col = "stato",
  person_col = "cf",
  plot_type = "boxplot",
  group_by = NULL,
  facet_by = NULL,
  log_scale = FALSE,
  show_outliers = TRUE,
  palette = "employment",
  use_bw = FALSE,
  alpha = 0.7,
  min_duration = 1,
  max_duration = NULL
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- duration_col:

  Character. Column name for duration (default: "durata")

- status_col:

  Character. Column name for employment status (default: "stato")

- person_col:

  Character. Column name for person identifier (default: "cf")

- plot_type:

  Character. Type of plot: "boxplot", "violin", "histogram", "density"
  (default: "boxplot")

- group_by:

  Character. Column to group by (default: NULL)

- facet_by:

  Character. Column to use for faceting (default: NULL)

- log_scale:

  Logical. Use log scale for duration (default: FALSE)

- show_outliers:

  Logical. Show outliers in boxplot (default: TRUE)

- palette:

  Character. Color palette to use (default: "employment")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- alpha:

  Numeric. Transparency (default: 0.7)

- min_duration:

  Integer. Minimum duration to include (default: 1)

- max_duration:

  Integer. Maximum duration to include (default: NULL)

## Value

A ggplot2 object showing duration analysis

## Examples

``` r
if (FALSE) { # \dontrun{
# Boxplot of durations by employment status
plot_duration_analysis(data, plot_type = "boxplot")

# Violin plot with log scale
plot_duration_analysis(data, plot_type = "violin", log_scale = TRUE)

# Histogram faceted by status
plot_duration_analysis(data, plot_type = "histogram", facet_by = "stato")
} # }
```
