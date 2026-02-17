# Treatment Effect Aggregation

Aggregates treatment effects across multiple outcomes, time periods, or
subgroups with appropriate statistical adjustments for multiple testing.

## Usage

``` r
aggregate_treatment_effects(
  estimation_results,
  aggregation_method = "weighted_average",
  weight_var = "inv_var",
  multiple_testing_correction = "holm",
  confidence_level = 0.95
)
```

## Arguments

- estimation_results:

  List of estimation results from DiD, event study, or synthetic control

- aggregation_method:

  Character. Method: "simple_average", "weighted_average",
  "meta_analysis". Default: "weighted_average"

- weight_var:

  Character. Variable to use for weighting (e.g., "n_obs", "inv_var").
  Default: "inv_var"

- multiple_testing_correction:

  Character. Method: "bonferroni", "holm", "fdr". Default: "holm"

- confidence_level:

  Numeric. Confidence level for intervals. Default: 0.95

## Value

A list containing:

- aggregated_effects:

  Summary of aggregated treatment effects

- individual_effects:

  Individual effect estimates with corrections

- heterogeneity_tests:

  Tests for effect heterogeneity

- meta_analysis_results:

  Meta-analysis results if applicable

## Examples

``` r
if (FALSE) { # \dontrun{
aggregated_results <- aggregate_treatment_effects(
  estimation_results = list(did_results, event_study_results),
  aggregation_method = "meta_analysis",
  multiple_testing_correction = "fdr"
)
} # }
```
