# Validate Integration Setup

Internal helper function to validate that the prepared data is suitable
for the intended impact evaluation method.

## Usage

``` r
validate_integration_setup(data, impact_method, id_column, verbose = TRUE)
```

## Arguments

- data:

  data.table. Output from prepare_metrics_for_impact_analysis()

- impact_method:

  character. The intended impact evaluation method

- id_column:

  character. Name of the individual identifier column

- verbose:

  logical. Whether to print validation results

## Value

logical. TRUE if validation passes, FALSE otherwise (with warnings)
