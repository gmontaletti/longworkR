# Data Quality Diagnostics for Propensity Score Matching

Provides comprehensive diagnostics for data quality issues that can
affect propensity score matching, including missing value patterns,
non-finite values, and categorical variable issues.

## Usage

``` r
diagnose_matching_data(
  data,
  treatment_var = "is_treated",
  matching_variables,
  exact_match_vars = NULL,
  factor_level_threshold = 5
)
```

## Arguments

- data:

  A data.table containing the data to be assessed

- treatment_var:

  Character. Name of treatment indicator variable

- matching_variables:

  Character vector. Variables to include in assessment

- exact_match_vars:

  Character vector. Variables requiring exact matches. Default: NULL

- factor_level_threshold:

  Numeric. Minimum observations per factor level. Default: 5

## Value

A list containing:

- overall_summary:

  Overall data quality summary

- variable_analysis:

  Detailed analysis by variable

- missing_patterns:

  Missing data patterns across observations

- recommendations:

  Specific recommendations for data cleaning

## Examples

``` r
if (FALSE) { # \dontrun{
# Assess data quality before matching
quality_report <- diagnose_matching_data(
  data = my_data,
  treatment_var = "is_treated",
  matching_variables = c("age", "education", "sector"),
  exact_match_vars = c("gender")
)
print(quality_report$overall_summary)
} # }
```
