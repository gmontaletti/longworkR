# Calculate Career Transition Metrics with Duration and Salary Analysis

Analyzes career transitions incorporating contract duration expectations
from survival analysis and salary progression. Identifies moves to
longer-lasting contracts and salary improvements as positive career
transitions.

## Usage

``` r
calculate_career_transition_metrics(
  data,
  survival_data = NULL,
  id_column = "cf",
  time_period_column = NULL,
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  salary_column = NULL,
  date_column = "inizio",
  min_transition_gap = 1
)
```

## Arguments

- data:

  A data.table containing employment records

- survival_data:

  Optional. Pre-computed survival analysis results with median durations
  by contract type. If NULL, will compute basic duration statistics.

- id_column:

  Character. Name of person identifier column. Default: "cf"

- time_period_column:

  Character. Optional column for grouping by time periods. Default: NULL

- contract_code_column:

  Character. Column containing contract type codes. Default:
  "COD_TIPOLOGIA_CONTRATTUALE"

- salary_column:

  Character. Column containing salary information. Default: NULL

- date_column:

  Character. Name of date column. Default: "inizio"

- min_transition_gap:

  Numeric. Minimum gap (days) between jobs to count as transition.
  Default: 1

## Value

A data.table with transition metrics:

- cf:

  Person identifier

- time_period:

  Time period (if specified)

- total_transitions:

  Total number of employment transitions

- duration_improvements:

  Transitions to longer-lasting contract types

- duration_deteriorations:

  Transitions to shorter-lasting contract types

- salary_improvements:

  Transitions with salary increases (if salary data available)

- salary_deteriorations:

  Transitions with salary decreases (if salary data available)

- fulltime_improvements:

  Transitions to full-time from part-time

- fulltime_deteriorations:

  Transitions from full-time to part-time

- composite_improvement_rate:

  Overall rate of positive career transitions

- career_advancement_index:

  Comprehensive career progression score

## Deprecated in v0.9.0

Use
[`career_profile()`](https://gmontaletti.github.io/longworkR/reference/career_profile.md)
with `indicators = c("core", "transitions")` instead. Scheduled for
removal in v1.0.0.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze transitions with survival data
survival_results <- estimate_contract_survival(employment_data)
transitions <- calculate_career_transition_metrics(
  data = employment_data,
  survival_data = survival_results,
  salary_column = "monthly_wage"
)
} # }
```
