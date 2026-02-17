# Calculate Contract Quality Metrics

Calculates contract quality metrics including temporary to permanent
transitions, contract type distributions, and quality improvements over
time.

## Usage

``` r
calculate_contract_quality_metrics(
  data,
  id_column = "cf",
  period_column = "event_period",
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  permanent_codes = c("C.01.00"),
  temporary_codes = c("A.03.00", "A.03.01", "A.09.00"),
  internship_codes = c("A.07.00", "A.07.01")
)
```

## Arguments

- data:

  A data.table containing employment records with event identification

- id_column:

  Character. Name of person identifier column. Default: "cf"

- period_column:

  Character. Column indicating pre/post event period. Default:
  "event_period"

- contract_code_column:

  Character. Column containing actual contract type codes. Default:
  "COD_TIPOLOGIA_CONTRATTUALE"

- permanent_codes:

  Character vector. Contract codes indicating permanent contracts.
  Default: c("C.01.00")

- temporary_codes:

  Character vector. Contract codes indicating temporary contracts.
  Default: c("A.03.00", "A.03.01", "A.09.00")

- internship_codes:

  Character vector. Contract codes indicating internship/apprenticeship
  contracts. Default: c("A.07.00", "A.07.01")

## Value

A data.table with contract quality metrics:

- cf:

  Person identifier

- period:

  Pre or post event period

- permanent_contract_days:

  Days in permanent contracts

- temporary_contract_days:

  Days in temporary contracts

- permanent_contract_rate:

  Proportion of employment in permanent contracts

- internship_contract_rate:

  Proportion of employment in internship contracts (if internship_codes
  provided)

- internship_contract_days:

  Days in internship contracts (if internship_codes provided)

- temp_to_perm_transitions:

  Number of temporary to permanent transitions

- temp_to_internship_transitions:

  Number of temporary to internship transitions (if internship_codes
  provided)

- internship_to_perm_transitions:

  Number of internship to permanent transitions (if internship_codes
  provided)

- perm_to_temp_transitions:

  Number of permanent to temporary transitions

- contract_stability_trend:

  Trend in contract stability over time

- average_contract_quality:

  Average contract quality score

- contract_improvement_rate:

  Rate of contract quality improvement

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with Italian employment contract codes
quality_metrics <- calculate_contract_quality_metrics(
  data = event_data,
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  permanent_codes = c("C.01.00"),  # Permanent contract codes
  temporary_codes = c("A.03.00", "A.03.01", "A.09.00"),  # Fixed-term contract codes
  internship_codes = c("A.07.00", "A.07.01")  # Apprenticeship/internship codes
)

# Example with custom contract column and codes
quality_metrics <- calculate_contract_quality_metrics(
  data = event_data,
  contract_code_column = "contract_type",
  permanent_codes = c("PERMANENT", "INDETERMINATE"),
  temporary_codes = c("FIXED_TERM", "TEMPORARY"),
  internship_codes = NULL  # Two-tier classification only
)
} # }
```
