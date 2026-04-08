# Calculate Career Complexity Metrics for General Career Analysis

Calculates career complexity metrics for generalized career trajectory
analysis, extending beyond pre/post event evaluation. Analyzes
concurrent employment patterns, employment diversity measures, and
complexity indices across any time period.

## Usage

``` r
calculate_career_complexity_metrics(
  data,
  id_column = "cf",
  time_period_column = NULL,
  complexity_variables = c("over_id", "arco", "prior")
)
```

## Arguments

- data:

  A data.table containing employment records

- id_column:

  Character. Name of person identifier column. Default: "cf"

- time_period_column:

  Character. Optional column for grouping by time periods. If NULL,
  analyzes entire career trajectory. Default: NULL

- complexity_variables:

  Character vector. Variables to use for complexity calculation.
  Default: c("over_id", "arco", "prior")

## Value

A data.table with career complexity metrics:

- cf:

  Person identifier

- time_period:

  Time period (if time_period_column provided)

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

- career_complexity_index:

  Overall job complexity score

- career_fragmentation_index:

  Measure of career fragmentation

## Details

The complexity score has been enhanced for better discriminatory power,
using an improved formula that provides greater variability across
different career patterns.

## Deprecated in v0.9.0

Use
[`career_profile()`](https://gmontaletti.github.io/longworkR/reference/career_profile.md)
with `indicators = c("core", "complexity")` instead. Scheduled for
removal in v1.0.0.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze overall career complexity
career_complexity <- calculate_career_complexity_metrics(
  data = employment_data,
  complexity_variables = c("over_id", "arco", "sector", "contract_type")
)

# Analyze complexity by year
yearly_complexity <- calculate_career_complexity_metrics(
  data = employment_data,
  time_period_column = "year"
)
} # }
```
