# Helper function to create date ranges for temporal periods OPTIMIZED: Fully vectorized period range calculation for maximum performance

Helper function to create date ranges for temporal periods OPTIMIZED:
Fully vectorized period range calculation for maximum performance

## Usage

``` r
.create_period_ranges(
  period_numbers,
  period_type = "quarterly",
  reference_date
)
```

## Arguments

- period_numbers:

  Integer vector of period numbers

- period_type:

  Character. "quarterly" or "monthly"

- reference_date:

  Date. Reference date for period calculation

## Value

data.table with period start and end dates
