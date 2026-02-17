# Plot Employment Summary Dashboard

Creates a comprehensive dashboard-style summary plot combining multiple
visualizations to provide an overview of employment patterns,
statistics, and key insights from the vecshift analysis.

## Usage

``` r
plot_employment_summary(
  data,
  person_col = "cf",
  time_col = "inizio",
  end_col = "fine",
  status_col = "stato",
  duration_col = "durata",
  dashboard_type = "overview",
  time_unit = "month",
  palette = "employment",
  use_bw = FALSE,
  include_stats = TRUE,
  alpha = 0.7
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- person_col:

  Character. Column name for person identifier (default: "cf")

- time_col:

  Character. Column name for time periods (default: "inizio")

- end_col:

  Character. Column name for period end dates (default: "fine")

- status_col:

  Character. Column name for employment status (default: "stato")

- duration_col:

  Character. Column name for duration (default: "durata")

- dashboard_type:

  Character. Type of dashboard: "overview", "executive" (default:
  "overview")

- time_unit:

  Character. Time aggregation unit: "month", "quarter" (default:
  "month")

- palette:

  Character. Color palette to use (default: "employment")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- include_stats:

  Logical. Include statistical summaries (default: TRUE)

- alpha:

  Numeric. Transparency (default: 0.7)

## Value

A ggplot2 object showing employment dashboard

## Examples

``` r
if (FALSE) { # \dontrun{
# Overview dashboard
plot_employment_summary(data, dashboard_type = "overview")

# Executive summary
plot_employment_summary(data, dashboard_type = "executive")
} # }
```
