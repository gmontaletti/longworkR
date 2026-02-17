# Calculate Median Survival Probability (DEPRECATED)

DEPRECATED: This function has been removed because the median survival
probability calculation was fundamentally flawed. Use
add_contract_survival_metrics() instead to get proper median survival
times with confidence intervals.

## Usage

``` r
calculate_median_survival_probability(
  data,
  survival_curves,
  contract_type_var,
  duration_var = "survival_time"
)
```

## Arguments

- data:

  A data.table with contract information

- survival_curves:

  List output from estimate_contract_survival

- contract_type_var:

  Character. Contract type variable name

- duration_var:

  Character. Duration variable name

## Value

Enhanced data.table with survival metrics (no median_survival_prob
column)
