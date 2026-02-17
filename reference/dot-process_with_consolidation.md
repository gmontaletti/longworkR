# Process Data with Consolidation

Internal helper to apply consolidation to pipeline data before
transition analysis.

## Usage

``` r
.process_with_consolidation(
  pipeline_data,
  consolidation_type,
  transition_variable = "prior"
)
```

## Arguments

- pipeline_data:

  Pipeline result data

- consolidation_type:

  Type of consolidation

- transition_variable:

  Variable for transitions

## Value

Processed transitions data
