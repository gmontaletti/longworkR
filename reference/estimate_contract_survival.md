# Estimate Contract Survival Curves

Calculates Kaplan-Meier survival curves for each contract type, handling
right-censoring appropriately.

## Usage

``` r
estimate_contract_survival(
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

List containing:

- `survival_fits`: survfit objects by contract type

- `median_survival`: Named vector of median survival times

- `survival_tables`: Survival probability tables

- `confidence_intervals`: CI for survival estimates
