# Add Survival Probabilities to Data

Adds the current survival probability for each contract based on its
duration and contract type.

## Usage

``` r
add_survival_probabilities(
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

Enhanced data.table with survival_prob column
