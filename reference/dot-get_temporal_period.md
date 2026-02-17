# Helper function to determine temporal period from date

Helper function to determine temporal period from date

## Usage

``` r
.get_temporal_period(dates, period_type = "quarterly", reference_date = NULL)
```

## Arguments

- dates:

  Date vector

- period_type:

  Character. "quarterly" or "monthly"

- reference_date:

  Date. Reference date for period calculation. Default: min date

## Value

Integer vector of period numbers
