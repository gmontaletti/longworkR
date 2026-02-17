# Summarize Consolidation Metrics

Creates a comprehensive summary report of consolidation metrics
including overall consolidation ratio, per-person statistics, and
employer-specific consolidation patterns (when applicable). Provides
both statistical summaries and human-readable interpretations of
consolidation effectiveness.

## Usage

``` r
summarize_consolidation(
  consolidation_metrics,
  include_distribution = TRUE,
  include_employer_details = TRUE
)
```

## Arguments

- consolidation_metrics:

  List of consolidation metrics from extract_consolidation_metrics()

- include_distribution:

  Logical. If TRUE (default), includes detailed distribution analysis of
  consolidation patterns across persons.

- include_employer_details:

  Logical. If TRUE (default), includes detailed employer-specific
  analysis when consolidation_mode involves employer consolidation.

## Value

List containing:

- `overview`: High-level consolidation summary with key metrics

- `effectiveness`: Consolidation effectiveness assessment and
  interpretation

- `person_summary`: Aggregated person-level statistics

- `distribution_summary`: Distribution of consolidation patterns
  (optional)

- `employer_summary`: Employer-specific consolidation analysis (when
  applicable), including:

  - `employer_variable`: Name of employer identification variable used

  - `lag_threshold_days`: Gap threshold used for employer consolidation

  - `employer_retention_statistics`: Statistics on employer retention
    during consolidation

  - `top_employers_by_contracts`: Top employers ranked by contract
    volume

  - `consolidation_potential`: Percentage of employers with
    consolidation opportunities

- `temporal_summary`: Temporal consolidation analysis (when applicable)

- `recommendations`: Suggested interpretations and next steps

## Details

This function transforms raw consolidation metrics into interpretable
insights:

- **Consolidation Effectiveness**: Quantifies reduction in data
  complexity

- **Person-Level Patterns**: Identifies consolidation variation across
  individuals

- **Method Assessment**: Evaluates consolidation method appropriateness

- **Quality Indicators**: Flags potential data quality issues or
  patterns

The summary adapts to different consolidation modes:

- **none**: Baseline summary confirming no consolidation applied

- **temporal**: Focus on over_id consolidation effectiveness and
  overlapping employment

- **employer**: Emphasis on same-employer consolidation and job change
  detection

- **both**: Combined assessment of sequential consolidation benefits

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract metrics from transition analysis
metrics <- extract_consolidation_metrics(
  original_data = original_result,
  consolidated_data = consolidated_result,
  consolidation_mode = "temporal",
  consolidation_type = "both"
)

# Generate comprehensive summary
summary <- summarize_consolidation(metrics)

# Print key findings
print(summary$overview)
print(summary$effectiveness)
print(summary$recommendations)

# Focus on person-level patterns
print(summary$person_summary)

# Employer consolidation summary
summary_employer <- summarize_consolidation(
  metrics_employer,
  include_employer_details = TRUE
)
print(summary_employer$employer_summary)

# Access employer-specific information
print(metrics_employer$employer_specific$unique_employers)
print(metrics_employer$employer_specific$n_unique_employers)
print(summary_employer$employer_summary$employer_retention_statistics)
} # }
```
