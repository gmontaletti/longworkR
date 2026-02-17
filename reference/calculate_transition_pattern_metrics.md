# Calculate Transition Pattern Metrics

Calculates transition pattern metrics including time to next job,
contract type changes, and transition frequency measures.

## Usage

``` r
calculate_transition_pattern_metrics(
  data,
  id_column = "cf",
  period_column = "event_period",
  date_column = "inizio"
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

## Value

A data.table with transition pattern metrics:

- cf:

  Person identifier

- period:

  Pre or post event period

- avg_time_to_next_job:

  Average days from job end to next job start

- median_time_to_next_job:

  Median days from job end to next job start

- job_search_success_rate:

  Proportion of unemployment spells ending in employment

- contract_type_changes:

  Number of contract type changes

- upward_transitions:

  Number of transitions to better contract types

- downward_transitions:

  Number of transitions to worse contract types

- transition_frequency:

  Number of job transitions per year

- employment_continuity_index:

  Measure of employment continuity

## Examples

``` r
if (FALSE) { # \dontrun{
transition_metrics <- calculate_transition_pattern_metrics(
  data = event_data
)
} # }
```
