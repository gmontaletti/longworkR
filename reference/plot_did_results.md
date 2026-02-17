# Plot Difference-in-Differences Results

Creates comprehensive DiD visualization showing parallel trends,
treatment effects, and robustness checks. Includes pre-post comparison
and treatment-control evolution over time.

## Usage

``` r
plot_did_results(
  did_results,
  plot_type = "trends",
  outcome_vars = NULL,
  show_treatment_period = TRUE,
  parallel_trends_test = TRUE,
  use_bw = FALSE,
  line_size = 1.2,
  point_size = 3,
  ribbon_alpha = 0.3,
  ncol = 2,
  y_limits = NULL
)
```

## Arguments

- did_results:

  List. Output from difference_in_differences() function

- plot_type:

  Character. Type of plot: "trends", "effects", "robustness", "all".
  Default: "trends"

- outcome_vars:

  Character vector. Outcomes to plot. Default: NULL (all outcomes)

- show_treatment_period:

  Logical. Highlight treatment period. Default: TRUE

- parallel_trends_test:

  Logical. Show parallel trends test results. Default: TRUE

- use_bw:

  Logical. Use black and white theme. Default: FALSE

- line_size:

  Numeric. Size of trend lines. Default: 1.2

- point_size:

  Numeric. Size of points. Default: 3

- ribbon_alpha:

  Numeric. Transparency for confidence ribbons. Default: 0.3

- ncol:

  Integer. Number of columns for multiple outcomes. Default: 2

- y_limits:

  Numeric vector. Y-axis limits. Default: NULL (automatic)

## Value

A ggplot2 object or list of plots (when plot_type = "all")

## See also

[`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md),
[`plot_event_study`](https://gmontaletti.github.io/longworkR/reference/plot_event_study.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic DiD trends plot
did_plot <- plot_did_results(
  did_results = did_estimation_results,
  plot_type = "trends",
  show_treatment_period = TRUE
)

# Treatment effects plot
effects_plot <- plot_did_results(
  did_results = did_estimation_results,
  plot_type = "effects",
  use_bw = TRUE
)
} # }
```
