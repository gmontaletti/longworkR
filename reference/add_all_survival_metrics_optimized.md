# Add All Survival Metrics Using Optimized Vectorized Operations

High-performance implementation that adds all survival metrics using
vectorized data.table operations and pre-computed lookup tables instead
of loops. Adds median survival times with confidence intervals and
current survival probabilities.

## Usage

``` r
add_all_survival_metrics_optimized(
  data,
  survival_curves,
  contract_type_var,
  duration_var = "survival_time",
  calculate_median_prob = FALSE
)
```

## Arguments

- data:

  A data.table with contract information

- survival_curves:

  List output from estimate_contract_survival_optimized

- contract_type_var:

  Character. Contract type variable name

- duration_var:

  Character. Duration variable name

- calculate_median_prob:

  Logical. DEPRECATED - parameter ignored (for backward compatibility)

## Value

Enhanced data.table with survival metrics:

- `contract_type_median`: Median survival time for contract type

- `median_ci_lower`: Lower bound of 95% confidence interval

- `median_ci_upper`: Upper bound of 95% confidence interval

- `survival_prob`: Current survival probability at observed duration
