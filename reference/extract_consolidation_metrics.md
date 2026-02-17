# Extract Consolidation Metrics from Employment Transition Analysis

**Phase 2 Optimized Function** Calculates detailed consolidation metrics
by comparing original pipeline_result with consolidated result from
analyze_employment_transitions(). Provides insights into how many
persons and contracts are affected by consolidation across all
consolidation modes (temporal, employer, both, none).

Now includes intelligent consolidation data generation when actual
consolidated data is not available, plus single-pass aggregation
patterns for maximum efficiency.

## Usage

``` r
extract_consolidation_metrics(
  original_data,
  consolidated_data = NULL,
  consolidation_mode = "none",
  employer_var = NULL,
  min_lag = 8,
  consolidation_type = "both"
)
```

## Arguments

- original_data:

  Original pipeline_result data.table from vecshift output

- consolidated_data:

  Consolidated data.table after consolidation (if any)

- consolidation_mode:

  Character string specifying consolidation mode used

- employer_var:

  Character string specifying employer variable (if applicable)

- min_lag:

  Numeric gap threshold for employer consolidation (if applicable)

- consolidation_type:

  Character string specifying temporal consolidation type (if
  applicable)

## Value

List containing consolidation metrics:

- `consolidation_summary`: Overall consolidation statistics

- `person_level`: Person-level consolidation metrics

- `distribution`: Distribution of consolidation patterns

- `employer_specific`: Employer-specific metrics (when applicable),
  including:

  - `unique_employers`: Vector of unique employer IDs

  - `n_unique_employers`: Count of unique employers

  - `employer_details`: Per-employer consolidation metrics data.table

  - `employer_summary`: Aggregated employer statistics

  - `employer_consolidation`: Person-employer combination metrics

  - `employer_person_stats`: Person-employer statistics

- `temporal_specific`: Temporal consolidation metrics (when applicable)

## Details

The function analyzes consolidation impact across multiple dimensions:

- **Overall Impact**: Total contracts vs consolidated periods

- **Person-Level**: Individual consolidation patterns and averages

- **Distribution Analysis**: How many contracts consolidated into single
  periods

- **Employer Analysis**: Consolidation patterns by employer (when
  applicable)

Handles all consolidation modes:

- **none**: Returns baseline metrics (no consolidation)

- **temporal**: Analyzes over_id-based consolidation

- **employer**: Analyzes same-employer consolidation

- **both**: Analyzes sequential consolidation effects

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample data and run transition analysis
result <- readRDS("data/sample.rds")

# Run with temporal consolidation
transitions <- analyze_employment_transitions(
  pipeline_result = result,
  consolidation_mode = "temporal",
  consolidation_type = "both"
)

# Extract consolidation metrics
metrics <- extract_consolidation_metrics(
  original_data = result,
  consolidated_data = transitions_data_used_internally,  # Would need to be captured
  consolidation_mode = "temporal",
  consolidation_type = "both"
)

# View overall impact
print(metrics$consolidation_summary)
print(metrics$person_level)
} # }
```
