# Add Contract Survival Metrics to Employment Data

Enhances employment data with survival analysis metrics including
survival time, censoring indicators, median survival times, and
confidence intervals for each contract type.

## Usage

``` r
add_contract_survival_metrics(
  data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  id_var = "cf",
  start_var = "INIZIO",
  end_var = "FINE",
  confidence_level = 0.95
)
```

## Arguments

- data:

  A data.table from vecshift() containing employment segments

- contract_type_var:

  Character. Name of the contract type variable (default:
  "COD_TIPOLOGIA_CONTRATTUALE")

- id_var:

  Character. Person identifier variable (default: "cf")

- start_var:

  Character. Contract start date variable (default: "INIZIO")

- end_var:

  Character. Contract end date variable (default: "FINE")

- confidence_level:

  Numeric. Confidence level for survival estimates (default: 0.95)

## Value

Enhanced data.table with additional columns:

- `survival_time`: Duration or time to censoring

- `censored`: 1 if censored at max(FINE), 0 otherwise

- `contract_type_median`: Median survival time for contract type
  (accounting for censoring)

- `median_ci_lower`: Lower bound of 95% confidence interval for median
  survival

- `median_ci_upper`: Upper bound of 95% confidence interval for median
  survival

- `survival_prob`: Current survival probability at observed duration

## Examples

``` r
if (FALSE) { # \dontrun{
# Process employment data with vecshift
processed_data <- vecshift(employment_data)

# Add contract type information
processed_data[, COD_TIPOLOGIA_CONTRATTUALE := contract_codes]

# Add survival metrics
survival_data <- add_contract_survival_metrics(
  data = processed_data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE"
)
} # }
```
