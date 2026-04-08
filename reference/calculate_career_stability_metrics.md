# Calculate Career Stability Metrics for General Career Analysis

Calculates employment stability metrics for generalized career
trajectory analysis, extending beyond pre/post event evaluation.
Analyzes employment rates, spell durations, and job turnover measures
across any time period or overall career.

## Usage

``` r
calculate_career_stability_metrics(
  data,
  id_column = "cf",
  time_period_column = NULL,
  date_column = "inizio",
  employment_indicator = "over_id",
  min_spell_duration = 7
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

- date_column:

  Character. Name of date column. Default: "inizio"

- employment_indicator:

  Character. Column indicating employment status. Default: "over_id"

- min_spell_duration:

  Numeric. Minimum duration (days) to count as employment spell.
  Default: 7

## Value

A data.table with employment stability metrics:

- cf:

  Person identifier

- time_period:

  Time period (if time_period_column provided)

- days_employed:

  Total days employed in period

- days_unemployed:

  Total days unemployed in period

- employment_rate:

  Proportion of period employed

- employment_spells:

  Number of distinct employment periods

- unemployment_spells:

  Number of distinct unemployment periods

- avg_employment_spell:

  Average duration of employment spells

- avg_unemployment_spell:

  Average duration of unemployment spells

- max_employment_spell:

  Longest employment spell in period

- job_turnover_rate:

  Employment spells per year in period

- employment_stability_index:

  Composite stability measure (0-1)

- employment_security_index:

  Backward compatibility alias for employment_stability_index

## Deprecated in v0.9.0

Use
[`career_profile()`](https://gmontaletti.github.io/longworkR/reference/career_profile.md)
with `indicators = c("core", "stability")` instead. Scheduled for
removal in v1.0.0.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze overall career stability
career_stability <- calculate_career_stability_metrics(
  data = employment_data,
  min_spell_duration = 14
)

# Analyze stability by year
yearly_stability <- calculate_career_stability_metrics(
  data = employment_data,
  time_period_column = "year"
)
} # }
```
