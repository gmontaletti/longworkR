# Plot Synthetic Control Results

Creates synthetic control plots showing actual vs synthetic unit
comparisons, treatment effects over time, and unit weights. Includes gap
plots and placebo test visualization.

## Usage

``` r
plot_synthetic_control(
  synth_results,
  plot_type = "trends",
  treatment_time = NULL,
  show_treatment_period = TRUE,
  show_inference = TRUE,
  use_bw = FALSE,
  line_size = 1.2,
  point_size = 2,
  ribbon_alpha = 0.3,
  n_weights = 10,
  weight_threshold = 0.01
)
```

## Arguments

- synth_results:

  List. Output from synthetic_control_method() function

- plot_type:

  Character. Type of plot: "trends", "gaps", "weights", "placebo",
  "all". Default: "trends"

- treatment_time:

  Numeric. Time when treatment began. Default: NULL (auto-detect)

- show_treatment_period:

  Logical. Highlight treatment period. Default: TRUE

- show_inference:

  Logical. Show inference confidence bands if available. Default: TRUE

- use_bw:

  Logical. Use black and white theme. Default: FALSE

- line_size:

  Numeric. Size of trend lines. Default: 1.2

- point_size:

  Numeric. Size of points. Default: 2

- ribbon_alpha:

  Numeric. Transparency for confidence ribbons. Default: 0.3

- n_weights:

  Integer. Maximum number of weights to show. Default: 10

- weight_threshold:

  Numeric. Minimum weight to display. Default: 0.01

## Value

A ggplot2 object or list of plots (when plot_type = "all")

## See also

[`synthetic_control_method`](https://gmontaletti.github.io/longworkR/reference/synthetic_control_method.md),
[`plot_did_results`](https://gmontaletti.github.io/longworkR/reference/plot_did_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic synthetic control trends
synth_plot <- plot_synthetic_control(
  synth_results = synth_estimation_results,
  plot_type = "trends",
  treatment_time = 2015
)

# Unit weights plot
weights_plot <- plot_synthetic_control(
  synth_results = synth_estimation_results,
  plot_type = "weights",
  n_weights = 8
)
} # }
```
