# Optimized Contract Survival Estimation

High-performance version of estimate_contract_survival with data.table
optimizations and caching for improved performance on large datasets.

## Usage

``` r
estimate_contract_survival_optimized(
  data,
  contract_type_var,
  duration_var = "survival_time",
  censored_var = "censored",
  confidence_level = 0.95
)
```

## Arguments

- data:

  A data.table with contract information

- contract_type_var:

  Character. Contract type variable name

- duration_var:

  Character. Duration variable name

- censored_var:

  Character. Censoring indicator variable name

- confidence_level:

  Numeric. Confidence level for estimates

## Value

List containing survival analysis results with pre-computed lookup
tables
