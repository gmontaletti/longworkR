# Helper: Get Survival Probability at Specific Time

Internal helper function to extract survival probability at a given time
from a survival table using step function logic.

## Usage

``` r
get_survival_at_time(surv_table, query_time)
```

## Arguments

- surv_table:

  A data.table with time and survival_prob columns

- time:

  Numeric. Time point to evaluate

## Value

Numeric. Survival probability at specified time
