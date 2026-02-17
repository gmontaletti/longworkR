# Plot Impact Evaluation Summary

Creates comprehensive multi-panel summary plots combining multiple
impact evaluation results into publication-ready figures. Shows
treatment effects, robustness checks, and key diagnostics in coordinated
panels.

## Usage

``` r
plot_impact_summary(
  results_list,
  panel_layout = "grid",
  include_panels = c("balance", "effects"),
  outcome_focus = NULL,
  use_bw = FALSE,
  panel_titles = TRUE,
  shared_legend = TRUE,
  figure_title = NULL,
  figure_subtitle = NULL,
  width_ratios = NULL,
  height_ratios = NULL
)
```

## Arguments

- results_list:

  List. Multiple estimation results from different methods

- panel_layout:

  Character. Layout type: "horizontal", "vertical", "grid". Default:
  "grid"

- include_panels:

  Character vector. Panels to include: "balance", "trends", "effects",
  "robustness". Default: c("balance", "effects")

- outcome_focus:

  Character. Primary outcome to highlight. Default: NULL

- use_bw:

  Logical. Use black and white theme. Default: FALSE

- panel_titles:

  Logical. Show individual panel titles. Default: TRUE

- shared_legend:

  Logical. Use shared legend across panels. Default: TRUE

- figure_title:

  Character. Overall figure title. Default: NULL

- figure_subtitle:

  Character. Figure subtitle. Default: NULL

- width_ratios:

  Numeric vector. Relative widths of panels. Default: NULL

- height_ratios:

  Numeric vector. Relative heights of panels. Default: NULL

## Value

A ggplot2 object or gridExtra arrangement

## See also

[`plot_balance_assessment`](https://gmontaletti.github.io/longworkR/reference/plot_balance_assessment.md),
[`plot_did_results`](https://gmontaletti.github.io/longworkR/reference/plot_did_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create comprehensive summary
summary_plot <- plot_impact_summary(
  results_list = list(
    matching = matching_results,
    did = did_results,
    event_study = event_results
  ),
  panel_layout = "grid",
  include_panels = c("balance", "trends", "effects")
)

# Horizontal layout for presentation
horizontal_summary <- plot_impact_summary(
  results_list = results_list,
  panel_layout = "horizontal",
  use_bw = TRUE,
  figure_title = "Employment Program Impact Evaluation"
)
} # }
```
