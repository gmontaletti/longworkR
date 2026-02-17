# Propensity Score Matching for Impact Evaluation (Person-Level Matching)

Performs propensity score matching to identify comparable control units
for treatment effect estimation at the person level. This function first
aggregates event-level data to person-level characteristics, performs
matching on people, then returns all events for matched individuals.
This is crucial for employment data where we want to match people (not
individual employment spells) based on their stable characteristics.

## Usage

``` r
propensity_score_matching(
  data,
  treatment_var = "is_treated",
  person_id_var = "cf",
  matching_variables,
  exact_match_vars = NULL,
  person_aggregation = "first",
  method = "nearest",
  ratio = 1,
  caliper = NULL,
  replace = FALSE,
  estimand = "ATT",
  link = "logit",
  distance_metric = "glm",
  missing_data_action = "complete_cases",
  imputation_method = "median",
  min_complete_cases = 0.5,
  max_missing_proportion = 0.3,
  factor_level_threshold = 5,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.table containing treatment and control observations (can be
  event-level)

- treatment_var:

  Character. Name of treatment indicator variable. Default: "is_treated"

- person_id_var:

  Character. Name of person identifier variable (e.g., "cf"). Default:
  "cf"

- matching_variables:

  Character vector. Variables to include in propensity score model

- exact_match_vars:

  Character vector. Variables requiring exact matches. Default: NULL

- person_aggregation:

  Character. How to aggregate person characteristics: "first", "last",
  "mode", "mean". Default: "first"

- method:

  Character. Matching method: "nearest", "optimal", "full", "genetic".
  Default: "nearest"

- ratio:

  Numeric. Ratio of control to treatment units. Default: 1

- caliper:

  Numeric. Maximum allowable distance for matches. Default: NULL (no
  caliper)

- replace:

  Logical. Allow replacement in matching? Default: FALSE

- estimand:

  Character. Target estimand: "ATT", "ATE", "ATC". Default: "ATT"

- link:

  Character. Link function for propensity model: "logit", "probit".
  Default: "logit"

- distance_metric:

  Character. Distance metric: "glm", "gam", "gbm", "randomforest".
  Default: "glm"

- missing_data_action:

  Character. How to handle missing data: "complete_cases" (default),
  "impute", "exclude_vars"

- imputation_method:

  Character. For numeric variables: "median" (default), "mean". For
  categorical: "mode"

- min_complete_cases:

  Numeric. Minimum proportion of complete cases required to proceed
  (0-1). Default: 0.5

- max_missing_proportion:

  Numeric. Maximum proportion of missing values allowed per variable
  (0-1). Default: 0.3

- factor_level_threshold:

  Numeric. Minimum observations per factor level. Default: 5

- verbose:

  Logical. Print detailed missing data diagnostics? Default: TRUE

## Value

A list containing:

- matched_data:

  Data.table with ALL events for matched individuals (both treated and
  control). Includes event_time column with values: "pre"
  (pre-treatment), "post" (post-treatment), "control" (control group),
  or NA

- matched_persons:

  Data.table with person-level characteristics used for matching

- match_matrix:

  Matrix showing which persons were matched

- propensity_scores:

  Propensity scores for all persons

- balance_before:

  Balance statistics before matching (person-level)

- balance_after:

  Balance statistics after matching (person-level)

- match_summary:

  Summary of matching procedure

- common_support:

  Information about common support region

- data_quality_report:

  Report on missing data handling and data quality issues

- aggregation_report:

  Report on person-level aggregation process

## Examples

``` r
if (FALSE) { # \dontrun{
# Process employment data first
employment_data <- vecshift(raw_employment_data)

# Add treatment indicator (e.g., policy intervention)
employment_data[, is_treated := some_treatment_condition]

# Basic person-level propensity score matching
ps_match <- propensity_score_matching(
  data = employment_data,
  person_id_var = "cf",
  matching_variables = c("age", "education", "sector", "region"),
  exact_match_vars = c("gender"),
  person_aggregation = "first",
  method = "nearest",
  ratio = 2,
  missing_data_action = "complete_cases",
  min_complete_cases = 0.7
)

# Result contains all events for matched persons
matched_employment_data <- ps_match$matched_data
person_characteristics <- ps_match$matched_persons

# Advanced matching with imputation for missing values
ps_match_imputed <- propensity_score_matching(
  data = employment_data,
  person_id_var = "cf",
  matching_variables = c("age", "education", "prior_employment", "wage"),
  person_aggregation = "mode",  # Use mode for categorical variables
  method = "optimal",
  caliper = 0.1,
  distance_metric = "gbm",
  missing_data_action = "impute",
  imputation_method = "median"
)
} # }
```
