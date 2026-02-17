# Calculate Career Success Metrics Combining Quality and Stability (Optimized)

Computes a unified career success index that combines quality,
stability, and opportunity measures from survival analysis. This single
index replaces the separate quality and risk metrics to provide a
coherent 0-1 scale where higher values indicate better career outcomes
with balanced stability and growth potential.

## Usage

``` r
calculate_career_success_metrics(
  data,
  survival_data = NULL,
  id_column = "cf",
  time_period_column = NULL,
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  employment_intensity_column = "prior",
  min_spell_duration = 7,
  enhance_variability = FALSE
)
```

## Arguments

- data:

  A data.table containing employment records

- survival_data:

  Optional. Pre-computed survival analysis results. If NULL, will
  compute basic duration measures using optimized aggregation.

- id_column:

  Character. Name of person identifier column. Default: "cf"

- time_period_column:

  Character. Optional column for grouping by time periods. Default: NULL

- contract_code_column:

  Character. Column containing contract type codes. Default:
  "COD_TIPOLOGIA_CONTRATTUALE"

- employment_intensity_column:

  Character. Column indicating employment intensity (prior). Default:
  "prior"

- min_spell_duration:

  Numeric. Minimum duration (days) to include in analysis. Default: 7

- enhance_variability:

  Logical. Use enhanced transformations for better metric
  discrimination. Default: FALSE

## Value

A data.table with comprehensive career metrics:

- cf:

  Person identifier

- time_period:

  Time period (if specified)

- total_employment_days:

  Total days in employment

- contract_quality_score:

  Duration-weighted average contract quality (0-1)

- employment_intensity_score:

  Duration-weighted average employment intensity (0-1)

- career_stability_score:

  Stability based on duration consistency (0-1)

- growth_opportunity_score:

  Access to diverse, high-quality contracts (0-1)

- career_success_index:

  Unified career success index (0-1)

- career_advancement_index:

  Career progression and transition success score (0-1)

## Details

**PERFORMANCE OPTIMIZED**: Uses data.table operations, vectorization,
and memory-efficient algorithms for processing large datasets (17M+
records) in \<5 minutes.

The comprehensive career success index combines multiple dimensions:

- **Contract Quality (35%)**: Based on survival analysis median
  durations

- **Employment Intensity (25%)**: Full-time vs part-time employment
  patterns

- **Career Stability (25%)**: Contract duration consistency and low
  volatility

- **Growth Opportunity (15%)**: Access to diverse, higher-quality
  contract types

Unlike separate quality/risk metrics that were highly correlated, this
unified approach balances stability (preferring consistent employment)
with opportunity (rewarding access to better contracts) in a single
interpretable 0-1 scale.

## Note

This comprehensive success index combines contract quality, employment
intensity, career stability, and growth opportunity into a single
interpretable 0-1 scale metric.

## See also

[`estimate_contract_survival_optimized`](https://gmontaletti.github.io/longworkR/reference/estimate_contract_survival_optimized.md)
for survival analysis,
[`calculate_comprehensive_career_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_career_metrics.md)
for multiple metric analysis

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample employment data
employment_data <- readRDS("data/sample.rds")

# Basic comprehensive career analysis
career_index <- calculate_career_success_metrics(
  data = employment_data
)

# With survival analysis for enhanced quality assessment
survival_results <- estimate_contract_survival_optimized(
  data = employment_data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  duration_var = "durata",
  censored_var = "censored"
)

enhanced_index <- calculate_career_success_metrics(
  data = employment_data,
  survival_data = survival_results
)

# Time-period analysis
employment_data[, year := year(inizio)]
yearly_index <- calculate_career_success_metrics(
  data = employment_data,
  survival_data = survival_results,
  time_period_column = "year"
)
} # }
```
