# Aggregate Event-Level Data to Person-Level for Matching

Helper function to aggregate event-level employment data to person-level
characteristics for propensity score matching. Handles different
aggregation methods for various variable types.

## Usage

``` r
aggregate_to_person_level(
  data,
  person_id_var,
  variables,
  aggregation_method = "first",
  verbose = TRUE
)
```

## Arguments

- data:

  data.table with event-level data

- person_id_var:

  Character. Name of person identifier column

- variables:

  Character vector. Variables to aggregate

- aggregation_method:

  Character. Aggregation method: "first", "last", "mode", "mean"

- verbose:

  Logical. Print aggregation diagnostics?

## Value

List containing aggregated data.table and aggregation report
