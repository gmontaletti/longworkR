# Plot Treatment vs Control Group Comparisons

Creates comprehensive pre- and post-treatment outcome comparisons
between treatment and control groups. Shows means, distributions, and
change patterns for impact evaluation visualization.

## Usage

``` r
plot_treatment_control_comparison(
  data,
  outcome_vars,
  treatment_var = "is_treated",
  time_var = "post_treatment",
  id_var = "cf",
  comparison_type = "means",
  use_bw = FALSE,
  show_ci = TRUE,
  ci_level = 0.95,
  dodge_width = 0.8,
  error_bar_width = 0.2,
  alpha = 0.7,
  ncol = 2
)
```

## Arguments

- data:

  Data.table. Panel data with treatment/control groups and outcomes

- outcome_vars:

  Character vector. Outcome variables to compare

- treatment_var:

  Character. Treatment indicator variable. Default: "is_treated"

- time_var:

  Character. Time variable (pre/post). Default: "post_treatment"

- id_var:

  Character. Individual identifier. Default: "cf"

- comparison_type:

  Character. Type of comparison: "means", "distributions", "changes",
  "all". Default: "means"

- use_bw:

  Logical. Use black and white theme. Default: FALSE

- show_ci:

  Logical. Show confidence intervals. Default: TRUE

- ci_level:

  Numeric. Confidence level for intervals. Default: 0.95

- dodge_width:

  Numeric. Dodge width for grouped bars. Default: 0.8

- error_bar_width:

  Numeric. Width of error bar caps. Default: 0.2

- alpha:

  Numeric. Transparency for distributions. Default: 0.7

- ncol:

  Integer. Number of columns for faceted plots. Default: 2

## Value

A ggplot2 object or list of plots (when comparison_type = "all")

## See also

[`plot_balance_assessment`](https://gmontaletti.github.io/longworkR/reference/plot_balance_assessment.md),
[`plot_did_results`](https://gmontaletti.github.io/longworkR/reference/plot_did_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic means comparison
comparison_plot <- plot_treatment_control_comparison(
  data = panel_data,
  outcome_vars = c("employment_rate", "avg_wage"),
  comparison_type = "means"
)

# Distribution comparisons
dist_plots <- plot_treatment_control_comparison(
  data = panel_data,
  outcome_vars = "job_stability",
  comparison_type = "distributions",
  use_bw = TRUE
)
} # }
```
