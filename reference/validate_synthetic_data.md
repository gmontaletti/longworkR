# Validate Synthetic Data Quality

Compares synthetic data against expected patterns and distributions to
ensure it maintains realistic characteristics for testing purposes.

## Usage

``` r
validate_synthetic_data(synthetic_data, reference_stats = NULL)
```

## Arguments

- synthetic_data:

  data.table. The synthetic dataset to validate

- reference_stats:

  list. Optional reference statistics to compare against

## Value

A list containing validation results and quality metrics
