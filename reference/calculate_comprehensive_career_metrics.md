# Comprehensive Career Metrics Analysis

Calculates all career evaluation metrics (quality index, transitions,
stability, complexity) in a single function call with consistent
formatting and validation. The main metric is now the unified career
quality index that replaces separate quality and risk metrics.

## Usage

``` r
calculate_comprehensive_career_metrics(
  data,
  survival_data = NULL,
  metrics = "all",
  id_column = "cf",
  time_period_column = NULL,
  output_format = "wide",
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  employment_intensity_column = "prior",
  complexity_variables = c("over_id", "arco", "prior"),
  salary_column = NULL,
  min_spell_duration = 7,
  enhance_variability = TRUE
)
```

## Arguments

- data:

  A data.table containing employment records

- survival_data:

  Optional. Pre-computed survival analysis results for enhanced
  analysis.

- metrics:

  Character vector. Metrics to calculate. Options: c("quality",
  "transitions", "stability", "complexity", "all"). Default: "all"

- id_column:

  Character. Name of person identifier column. Default: "cf"

- time_period_column:

  Character. Optional column for grouping by time periods. Default: NULL

- output_format:

  Character. Output format: "wide", "long", or "list". Default: "wide"

- contract_code_column:

  Character. Column containing contract type codes. Default:
  "COD_TIPOLOGIA_CONTRATTUALE"

- employment_intensity_column:

  Character. Column indicating employment intensity (prior). Default:
  "prior"

- complexity_variables:

  Character vector. Variables used for complexity calculation. Default:
  c("over_id", "arco", "prior")

- salary_column:

  Character. Column containing salary information. Default: NULL

- min_spell_duration:

  Numeric. Minimum duration (days) to include in analysis. Default: 7

- enhance_variability:

  Logical. Use enhanced transformations for better metric
  discrimination. Improves CV by 50%+ for career quality metrics.
  Default: TRUE

## Value

Based on output_format:

- wide:

  Single data.table with all metrics as columns

- long:

  Long-format data.table with metric_name and metric_value columns

- list:

  Named list with separate data.tables for each metric type

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample employment data
employment_data <- readRDS("data/sample.rds")

# Comprehensive career analysis with all metrics
career_metrics <- calculate_comprehensive_career_metrics(
  data = employment_data,
  metrics = "all",
  salary_column = "monthly_wage"
)

# Quality index and stability analysis only
index_stability <- calculate_comprehensive_career_metrics(
  data = employment_data,
  metrics = c("quality", "stability"),
  output_format = "list"
)

# Time-period analysis with enhanced survival data
employment_data[, year := year(inizio)]
survival_results <- estimate_contract_survival_optimized(
  data = employment_data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  duration_var = "durata",
  censored_var = "censored"
)

yearly_metrics <- calculate_comprehensive_career_metrics(
  data = employment_data,
  survival_data = survival_results,
  time_period_column = "year",
  complexity_variables = c("over_id", "arco", "prior", "COD_TIPOLOGIA_CONTRATTUALE"),
  min_spell_duration = 14
)
} # }
```
