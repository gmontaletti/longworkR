# Calculate Employment Stability Metrics

Calculates employment stability metrics for pre- and post-event periods
including employment rates, spell durations, and job turnover measures.

## Usage

``` r
calculate_employment_stability_metrics(
  data,
  id_column = "cf",
  period_column = "event_period",
  date_column = "inizio",
  employment_indicator = "over_id",
  min_spell_duration = 7
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

- period:

  Pre or post event period

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

## Examples

``` r
if (FALSE) { # \dontrun{
stability_metrics <- calculate_employment_stability_metrics(
  data = event_data,
  min_spell_duration = 14
)
} # }
```
