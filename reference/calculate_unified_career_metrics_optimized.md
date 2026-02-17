# Ultra-Optimized Unified Career Metrics Calculation (Internal)

Single-pass calculation of multiple career metrics for maximum
performance. This internal function combines quality, stability, and
complexity calculations in a single data.table operation to minimize
memory usage and maximize speed.

## Usage

``` r
calculate_unified_career_metrics_optimized(
  data,
  survival_data = NULL,
  id_column = "cf",
  time_period_column = NULL,
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  employment_intensity_column = "prior",
  complexity_variables = c("over_id", "arco", "prior"),
  salary_column = NULL,
  min_spell_duration = 7,
  enhance_variability = TRUE,
  include_transitions = FALSE
)
```

## Arguments

- data:

  A data.table containing employment records

- survival_data:

  Optional survival analysis results

- id_column:

  Character. Person identifier column name

- time_period_column:

  Character. Time period column name (optional)

- contract_code_column:

  Character. Contract type column name

- employment_intensity_column:

  Character. Employment intensity column name

- complexity_variables:

  Character vector. Variables for complexity calculation

- salary_column:

  Character. Salary column name (optional)

- min_spell_duration:

  Numeric. Minimum spell duration filter

- enhance_variability:

  Logical. Use enhanced transformations

- include_transitions:

  Logical. Include transition metrics

## Value

A data.table with unified career metrics
