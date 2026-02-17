# Compute Contract Quality Indicators for Temporal Periods

Calculates data-driven contract quality metrics within each temporal
period using survival analysis methodology. Contract quality is
determined by the median survival duration of each contract type -
contracts with longer median survival times receive higher quality
scores (0.3-1.0 scale). This approach replaces arbitrary fixed weights
with empirical employment durability metrics derived from actual
survival data.

## Usage

``` r
.compute_contract_quality(
  data,
  period_type = "quarterly",
  id_var = "cf",
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  employment_intensity_var = "prior",
  employment_var = "arco",
  duration_var = "durata",
  start_date_var = "inizio",
  end_date_var = "fine",
  reference_date = NULL
)
```

## Arguments

- data:

  data.table with employment records

- period_type:

  Character. "quarterly" or "monthly"

- id_var:

  Character. Individual identifier column. Default: "cf"

- contract_type_var:

  Character. Contract type column. Default: "COD_TIPOLOGIA_CONTRATTUALE"

- employment_intensity_var:

  Character. Employment intensity column (1=full-time, \<1=part-time).
  Default: "prior"

- employment_var:

  Character. Employment status column (arco \>= 1 = employed). Default:
  "arco"

- duration_var:

  Character. Duration column in days. Default: "durata"

- start_date_var:

  Character. Contract start date column. Default: "inizio"

- end_date_var:

  Character. Contract end date column. Default: "fine"

- reference_date:

  Date. Reference date for periods. Default: NULL (min start date)

## Value

data.table with contract quality metrics by person and period including:

- avg_contract_quality: Duration-weighted average quality score
  (0.3-1.0)

- total_employment_days: Total employment days in period

- n_contracts: Number of contracts in period

- pct_permanent: Proportion of time in permanent contracts (A.01.00)

- pct_full_time: Proportion of time in full-time employment

- median_survival_based_quality: Flag indicating survival-based
  calculation

## Details

The function performs Kaplan-Meier survival analysis on each contract
type to estimate median survival times, then normalizes these to create
quality scores where:

- Longest surviving contract types receive quality score = 1.0

- Shortest surviving contract types receive quality score = 0.3

- Intermediate contract types receive linearly scaled scores

- Unknown contract types receive default score = 0.3

- Part-time contracts receive 0.8x adjustment factor

**Survival Analysis Method**: Uses
estimate_contract_survival_optimized() to compute Kaplan-Meier survival
curves for each contract type. Contracts ending at the maximum
observation date are treated as censored observations.

**Fallback Mechanism**: If survival analysis fails, uses simple
duration-based quality scores calculated from mean contract durations by
type.

**Quality Score Range**: All scores are bounded between 0.3-1.0 to
ensure meaningful differentiation while avoiding extreme values that
could dominate analyses.
