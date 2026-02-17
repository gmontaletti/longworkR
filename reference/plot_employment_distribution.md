# Plot Employment Status Distribution

Creates distribution plots showing how employment statuses change over
time or across different dimensions. Supports various visualization
types including stacked areas, bars, and proportional views.

## Usage

``` r
plot_employment_distribution(
  data,
  time_col = "inizio",
  status_col = "stato",
  agg_period = "month",
  plot_type = "area",
  group_by = NULL,
  facet_by = NULL,
  palette = "employment",
  use_bw = FALSE,
  show_counts = TRUE,
  smooth_trend = FALSE,
  alpha = 0.7
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- time_col:

  Character. Column name for time periods (default: "inizio")

- status_col:

  Character. Column name for employment status (default: "stato")

- agg_period:

  Character. Time aggregation period: "month", "quarter", "year"
  (default: "month")

- plot_type:

  Character. Type of plot: "area", "bar", "proportion" (default: "area")

- group_by:

  Character. Column to group by (default: NULL)

- facet_by:

  Character. Column to use for faceting (default: NULL)

- palette:

  Character. Color palette to use (default: "employment")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- show_counts:

  Logical. Show actual counts vs proportions (default: TRUE)

- smooth_trend:

  Logical. Add smooth trend lines (default: FALSE)

- alpha:

  Numeric. Transparency (default: 0.7)

## Value

A ggplot2 object showing employment status distributions

## Examples

``` r
if (FALSE) { # \dontrun{
# Area plot of employment distribution over time
plot_employment_distribution(data, plot_type = "area")

# Bar chart by quarter
plot_employment_distribution(data, agg_period = "quarter", plot_type = "bar")

# Proportional view
plot_employment_distribution(data, plot_type = "proportion", show_counts = FALSE)
} # }
```
