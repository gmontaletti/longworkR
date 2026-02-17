# Compare Survival Curves Across Contract Types

Performs statistical tests to compare survival curves between different
contract types (log-rank test, Wilcoxon test).

## Usage

``` r
compare_contract_survival(
  data,
  contract_type_var,
  duration_var = "survival_time",
  censored_var = "censored",
  test_type = "both"
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

- test_type:

  Character. Type of test: "logrank", "wilcoxon", or "both"

## Value

List containing:

- `test_results`: Statistical test results

- `pairwise_comparisons`: Pairwise comparison p-values

- `hazard_ratios`: Estimated hazard ratios between groups
