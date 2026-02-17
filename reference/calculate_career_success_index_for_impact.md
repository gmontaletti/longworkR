# Calculate Career Success Index for Impact Analysis

Internal helper function that calculates the unified career success
index from component metrics calculated in the impact system.

## Usage

``` r
calculate_career_success_index_for_impact(
  metric_results,
  id_column,
  period_column
)
```

## Arguments

- metric_results:

  List containing calculated metric results

- id_column:

  Character. Name of person identifier column

- period_column:

  Character. Column indicating pre/post event period

## Value

A data.table with career_success_index values
