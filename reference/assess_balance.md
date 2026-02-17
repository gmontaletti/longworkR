# Balance Assessment for Matched Data

Comprehensive balance assessment for matched treatment and control
groups including standardized mean differences, variance ratios, and
distributional balance tests.

## Usage

``` r
assess_balance(
  matched_data,
  treatment_var = "is_treated",
  balance_variables,
  weight_var = NULL,
  thresholds = list(mean_diff = 0.1, variance_ratio = 2)
)
```

## Arguments

- matched_data:

  Data.table with matched observations

- treatment_var:

  Character. Name of treatment indicator variable. Default: "is_treated"

- balance_variables:

  Character vector. Variables to assess balance for

- weight_var:

  Character. Name of weight variable (for weighted balance). Default:
  NULL

- thresholds:

  Named list. Balance thresholds: list(mean_diff = 0.1, variance_ratio =
  2)

## Value

A list containing:

- balance_table:

  Data.table with balance statistics for each variable

- overall_balance:

  Overall balance summary

- distributional_tests:

  Results of distributional balance tests

- balance_plots:

  Data for creating balance plots

## Examples

``` r
if (FALSE) { # \dontrun{
balance_results <- assess_balance(
  matched_data = ps_match$matched_data,
  balance_variables = c("age", "education", "wage", "sector"),
  thresholds = list(mean_diff = 0.1, variance_ratio = 2)
)
} # }
```
