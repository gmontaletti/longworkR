# Plot Employment Heatmap

Creates heatmap visualizations showing employment density patterns over
time and across populations, useful for identifying seasonal patterns,
regional differences, or temporal clustering of employment events.

## Usage

``` r
plot_employment_heatmap(
  data,
  person_col = "cf",
  time_col = "inizio",
  end_col = "fine",
  status_col = "stato",
  heatmap_type = "density",
  time_unit = "month",
  group_by = NULL,
  facet_by = NULL,
  n_people = 50,
  palette = "main",
  use_bw = FALSE,
  show_values = FALSE,
  alpha = 0.9
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

- heatmap_type:

  Character. Type of heatmap: "density", "status", "duration",
  "transitions" (default: "density")

- time_unit:

  Character. Time aggregation unit: "month", "quarter", "year" (default:
  "month")

- group_by:

  Character. Column to group by for comparison (default: NULL)

- facet_by:

  Character. Column to use for faceting (default: NULL)

- n_people:

  Integer. Maximum number of people to show (default: 50)

- palette:

  Character. Color palette to use (default: "main")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- show_values:

  Logical. Show values in heatmap cells (default: FALSE)

- alpha:

  Numeric. Transparency (default: 0.9)

## Value

A ggplot2 object showing employment heatmap

## Examples

``` r
if (FALSE) { # \dontrun{
# Employment density heatmap
plot_employment_heatmap(data, heatmap_type = "density")

# Status distribution heatmap
plot_employment_heatmap(data, heatmap_type = "status")

# Duration patterns by month
plot_employment_heatmap(data, heatmap_type = "duration", time_unit = "month")
} # }
```
