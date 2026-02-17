# Plot Employment Transition Flows

Creates alluvial flow diagrams showing how individuals transition
between employment statuses over time. Uses ggalluvial to create proper
flow ribbons that connect employment categories across multiple time
periods, showing both the stacked distribution at each time point and
the flowing transitions between them.

## Usage

``` r
plot_transition_flows(
  data,
  person_col = "cf",
  time_col = "inizio",
  status_col = "stato",
  plot_type = "alluvial",
  agg_period = "year",
  min_freq = 10,
  max_categories = 8,
  palette = "employment",
  use_bw = FALSE,
  alpha = 0.7,
  show_percentages = TRUE,
  stratum_width = 0.3
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- person_col:

  Character. Column name for person identifier (default: "cf")

- time_col:

  Character. Column name for time periods (default: "inizio")

- status_col:

  Character. Column name for employment status (default: "stato")

- plot_type:

  Character. Type of plot: "alluvial", "flow", "sankey" (default:
  "alluvial")

- agg_period:

  Character. Time aggregation: "year", "quarter", "month" (default:
  "year")

- min_freq:

  Integer. Minimum frequency to include status-period combinations
  (default: 10)

- max_categories:

  Integer. Maximum employment categories to show (default: 8)

- palette:

  Character. Color palette to use (default: "employment")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- alpha:

  Numeric. Flow transparency (default: 0.7)

- show_percentages:

  Logical. Show percentages in stratum labels (default: TRUE)

- stratum_width:

  Numeric. Width of the stacked bars/strata (default: 0.3)

## Value

A ggplot2 object showing transition flows with proper alluvial ribbons

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic alluvial diagram showing flows across years
plot_transition_flows(data, plot_type = "alluvial", agg_period = "year")

# Flow-only visualization (no stacked bars)
plot_transition_flows(data, plot_type = "flow", agg_period = "quarter")

# Sankey summary (first to last status)
plot_transition_flows(data, plot_type = "sankey")
} # }
```
