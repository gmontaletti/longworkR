# Generate consolidated data for metrics when not available from main function

Generate consolidated data for metrics when not available from main
function

## Usage

``` r
.generate_consolidated_data_for_metrics(
  original_data,
  consolidation_mode,
  employer_var,
  min_lag,
  consolidation_type
)
```

## Arguments

- original_data:

  Original data.table

- consolidation_mode:

  Consolidation mode to apply

- employer_var:

  Employer variable (if applicable)

- min_lag:

  Lag threshold (if applicable)

- consolidation_type:

  Consolidation type (if applicable)

## Value

Consolidated data.table
