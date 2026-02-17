# Difference-in-Differences Estimation

Performs difference-in-differences estimation for treatment effect
identification using employment data with flexible model specifications
and robust inference.

## Usage

``` r
difference_in_differences(
  data,
  outcome_vars,
  treatment_var = "is_treated",
  time_var = "event_period",
  id_var = "cf",
  control_vars = NULL,
  fixed_effects = "both",
  cluster_var = NULL,
  weights_var = NULL,
  parallel_trends_test = TRUE,
  placebo_test = TRUE,
  bootstrap_se = FALSE,
  n_bootstrap = 1000,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.table containing panel data with treatment identification

- outcome_vars:

  Character vector. Outcome variables to analyze

- treatment_var:

  Character. Treatment indicator variable. Default: "is_treated"

- time_var:

  Character. Time variable. Default: "event_period"

- id_var:

  Character. Individual identifier. Default: "cf"

- control_vars:

  Character vector. Control variables to include. Default: NULL

- fixed_effects:

  Character vector. Fixed effects to include: c("individual", "time",
  "both"). Default: "both"

- cluster_var:

  Character. Variable for clustered standard errors. Default: NULL

- weights_var:

  Character. Weights variable for weighted regression. Default: NULL

- parallel_trends_test:

  Logical. Test parallel trends assumption? Default: TRUE

- placebo_test:

  Logical. Perform placebo tests? Default: TRUE

- bootstrap_se:

  Logical. Use bootstrap standard errors? Default: FALSE

- n_bootstrap:

  Integer. Number of bootstrap replications. Default: 1000

- verbose:

  Logical. Print diagnostic information? Default: TRUE

## Value

A list containing:

- estimates:

  Treatment effect estimates for each outcome

- model_results:

  Full regression results

- parallel_trends_test:

  Parallel trends test results

- placebo_tests:

  Placebo test results

- robustness_checks:

  Additional robustness checks

- summary_table:

  Summary table of all results

## See also

[`calculate_comprehensive_impact_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_impact_metrics.md)
for computing employment metrics,
[`prepare_metrics_for_impact_analysis`](https://gmontaletti.github.io/longworkR/reference/prepare_metrics_for_impact_analysis.md)
for preparing metrics data,
[`vignette("impact-metrics-integration", package = "longworkR")`](https://gmontaletti.github.io/longworkR/articles/impact-metrics-integration.md)
for complete integration workflow

## Examples

``` r
if (FALSE) { # \dontrun{
# ===== INTEGRATION WITH EMPLOYMENT METRICS =====
# Complete workflow: metrics calculation → data preparation → DiD analysis

# Step 1: Calculate employment metrics
metrics_result <- calculate_comprehensive_impact_metrics(
  data = employment_data,
  metrics = c("stability", "quality", "complexity"),
  output_format = "wide"
)

# Step 2: Define treatment assignment
treatment_data <- data.table(
  cf = unique(employment_data$cf),
  is_treated = sample(c(0, 1), length(unique(employment_data$cf)), replace = TRUE)
)

# Step 3: Prepare for DiD analysis
did_data <- prepare_metrics_for_impact_analysis(
  metrics_output = metrics_result,
  treatment_assignment = treatment_data,
  impact_method = "did"
)

# Step 4: Run DiD on employment metrics
did_results <- difference_in_differences(
  data = did_data,
  outcome_vars = c("employment_rate", "permanent_contract_rate", "career_stability_score"),
  treatment_var = "is_treated",
  time_var = "post",
  id_var = "cf"
)

# ===== TRADITIONAL DiD EXAMPLES =====
# Basic DiD estimation with pre-prepared panel data
did_basic <- difference_in_differences(
  data = panel_data,
  outcome_vars = c("employment_rate", "avg_wage", "job_stability"),
  treatment_var = "training_program",
  control_vars = c("age", "education", "sector")
)

# Advanced DiD with clustering and robustness checks
did_robust <- difference_in_differences(
  data = panel_data,
  outcome_vars = "employment_stability_index",
  cluster_var = "region",
  parallel_trends_test = TRUE,
  bootstrap_se = TRUE
)

# ===== MULTIPLE OUTCOMES ANALYSIS =====
# Analyze multiple employment metrics simultaneously
multi_outcome_did <- difference_in_differences(
  data = did_data,
  outcome_vars = c(
    "employment_rate", "permanent_contract_rate",
    "avg_contract_quality", "transition_success_rate",
    "career_complexity_score", "employment_stability_index"
  ),
  treatment_var = "is_treated",
  time_var = "post",
  id_var = "cf",
  fixed_effects = "both",
  parallel_trends_test = TRUE
)
} # }
```
