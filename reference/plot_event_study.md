# Plot Event Study Results

Creates event study plots showing treatment effects over time relative
to the treatment event. Includes confidence intervals, reference
periods, and parallel trends assessment visualization.

## Usage

``` r
plot_event_study(
  event_study_results,
  outcome_vars = NULL,
  show_pre_trends = TRUE,
  reference_line = TRUE,
  event_line = TRUE,
  use_bw = FALSE,
  point_size = 3,
  line_size = 1,
  ribbon_alpha = 0.3,
  ncol = 2,
  y_limits = NULL,
  x_limits = NULL
)
```

## Arguments

- event_study_results:

  List. Output from event_study_design() function

- outcome_vars:

  Character vector. Outcomes to plot. Default: NULL (all outcomes)

- show_pre_trends:

  Logical. Highlight pre-treatment trends. Default: TRUE

- reference_line:

  Logical. Show reference line at zero effect. Default: TRUE

- event_line:

  Logical. Show vertical line at event time. Default: TRUE

- use_bw:

  Logical. Use black and white theme. Default: FALSE

- point_size:

  Numeric. Size of coefficient points. Default: 3

- line_size:

  Numeric. Size of connecting lines. Default: 1

- ribbon_alpha:

  Numeric. Transparency for confidence ribbons. Default: 0.3

- ncol:

  Integer. Number of columns for multiple outcomes. Default: 2

- y_limits:

  Numeric vector. Y-axis limits. Default: NULL (automatic)

- x_limits:

  Numeric vector. X-axis limits. Default: NULL (automatic)

## Value

A ggplot2 object

## See also

[`event_study_design`](https://gmontaletti.github.io/longworkR/reference/event_study_design.md),
[`plot_did_results`](https://gmontaletti.github.io/longworkR/reference/plot_did_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic event study plot
event_plot <- plot_event_study(
  event_study_results = event_results,
  show_pre_trends = TRUE,
  reference_line = TRUE
)

# Multiple outcomes with custom styling
multi_event_plot <- plot_event_study(
  event_study_results = event_results,
  outcome_vars = c("employment_rate", "wage"),
  use_bw = TRUE,
  ncol = 1
)
} # }
```
