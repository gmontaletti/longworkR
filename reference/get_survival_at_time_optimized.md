# Optimized Helper: Get Survival Probability at Specific Time

Optimized version using data.table's fast binary search for probability
lookup.

## Usage

``` r
get_survival_at_time_optimized(surv_table, query_time)
```

## Arguments

- surv_table:

  A data.table with time and survival_prob columns

- time:

  Numeric. Time point to evaluate

## Value

Numeric. Survival probability at specified time
