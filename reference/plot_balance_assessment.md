# Plot Covariate Balance Assessment

Creates comprehensive covariate balance plots showing standardized mean
differences before and after matching. Includes Love plots, density
comparisons, and balance tables for propensity score matching
diagnostics.

## Usage

``` r
plot_balance_assessment(
  matching_results,
  plot_type = "love",
  variables = NULL,
  threshold = 0.1,
  use_bw = FALSE,
  show_sample_sizes = TRUE,
  standardized = TRUE,
  point_size = 3,
  line_size = 0.8,
  alpha = 0.7,
  ncol = 2
)
```

## Arguments

- matching_results:

  List. Output from propensity_score_matching() function

- plot_type:

  Character. Type of balance plot: "love", "density", "histogram",
  "violin", "all". Default: "love"

- variables:

  Character vector. Variables to include in plots. Default: NULL (all
  variables)

- threshold:

  Numeric. Balance threshold line for Love plot. Default: 0.1

- use_bw:

  Logical. Use black and white theme for accessibility. Default: FALSE

- show_sample_sizes:

  Logical. Display sample sizes in plot titles. Default: TRUE

- standardized:

  Logical. Show standardized differences in Love plot. Default: TRUE

- point_size:

  Numeric. Size of points in Love plot. Default: 3

- line_size:

  Numeric. Size of connecting lines. Default: 0.8

- alpha:

  Numeric. Transparency for density plots. Default: 0.7

- ncol:

  Integer. Number of columns for faceted plots. Default: 2

## Value

A ggplot2 object or list of plots (when plot_type = "all")

## See also

[`propensity_score_matching`](https://gmontaletti.github.io/longworkR/reference/propensity_score_matching.md),
[`plot_treatment_control_comparison`](https://gmontaletti.github.io/longworkR/reference/plot_treatment_control_comparison.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming matching_results from propensity_score_matching()
balance_plot <- plot_balance_assessment(
  matching_results = ps_match_results,
  plot_type = "love",
  threshold = 0.1
)

# Multiple balance plots
all_balance_plots <- plot_balance_assessment(
  matching_results = ps_match_results,
  plot_type = "all",
  use_bw = TRUE
)
} # }
```
