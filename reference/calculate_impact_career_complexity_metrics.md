# Calculate Career Complexity Metrics

Calculates career complexity metrics including concurrent employment
patterns, employment diversity measures, and complexity indices.

## Usage

``` r
calculate_impact_career_complexity_metrics(
  data,
  id_column = "cf",
  period_column = "event_period",
  complexity_variables = c("over_id", "arco", "prior")
)
```

## Arguments

- data:

  A data.table containing employment records with event identification

- id_column:

  Character. Name of person identifier column. Default: "cf"

- period_column:

  Character. Column indicating pre/post event period. Default:
  "event_period"

- complexity_variables:

  Character vector. Variables to use for complexity calculation.
  Default: c("over_id", "arco", "prior")

## Value

A data.table with career complexity metrics:

- cf:

  Person identifier

- period:

  Pre or post event period

- max_concurrent_jobs:

  Maximum number of concurrent jobs

- avg_concurrent_jobs:

  Average number of concurrent jobs

- concurrent_employment_days:

  Days with multiple concurrent jobs

- concurrent_employment_rate:

  Proportion of employment with multiple jobs

- employment_diversity_index:

  Shannon diversity index of employment types

- job_complexity_score:

  Overall job complexity score

- career_fragmentation_index:

  Measure of career fragmentation

## Examples

``` r
if (FALSE) { # \dontrun{
complexity_metrics <- calculate_career_complexity_metrics(
  data = event_data,
  complexity_variables = c("over_id", "arco", "sector", "contract_type")
)
} # }
```
