# Comprehensive Impact Metrics Calculation

Calculates all impact evaluation metrics (stability, quality,
complexity, transitions) in a single function call with consistent
formatting and validation. This function serves as the first step in the
integrated workflow for causal inference analysis. Optionally integrates
comprehensive career metrics from the career analysis system.

## Usage

``` r
calculate_comprehensive_impact_metrics(
  data,
  metrics = "all",
  career_metrics = FALSE,
  id_column = "cf",
  period_column = "event_period",
  output_format = "wide",
  contract_code_column = "COD_TIPOLOGIA_CONTRATTUALE",
  permanent_codes = c("C.01.00"),
  temporary_codes = c("A.03.00", "A.03.01", "A.09.00"),
  internship_codes = c("A.07.00", "A.07.01")
)
```

## Arguments

- data:

  A data.table containing employment records with event identification

- metrics:

  Character vector. Metrics to calculate. Options: c("stability",
  "quality", "complexity", "transitions", "all"). When career_metrics =
  TRUE, "all" includes both impact and career metrics. Default: "all"

- career_metrics:

  Logical. Whether to include comprehensive career metrics from the
  career analysis system. Default: FALSE (maintains backward
  compatibility)

- id_column:

  Character. Name of person identifier column. Default: "cf"

- period_column:

  Character. Column indicating pre/post event period. Default:
  "event_period"

- output_format:

  Character. Output format: "wide", "long", or "list". Default: "wide"

- contract_code_column:

  Character. Column containing actual contract type codes (for quality
  metrics). Default: "COD_TIPOLOGIA_CONTRATTUALE"

- permanent_codes:

  Character vector. Contract codes indicating permanent contracts (for
  quality metrics). Default: c("C.01.00")

- temporary_codes:

  Character vector. Contract codes indicating temporary contracts (for
  quality metrics). Default: c("A.03.00", "A.03.01", "A.09.00")

- internship_codes:

  Character vector. Contract codes indicating internship/apprenticeship
  contracts (for quality metrics). Default: c("A.07.00", "A.07.01")

## Value

Based on output_format:

- wide:

  Single data.table with all metrics as columns, including
  career_success_index when multiple metrics are calculated. When
  career_metrics = TRUE, includes 35+ career metrics

- long:

  Long-format data.table with metric_name and metric_value columns

- list:

  Named list with separate data.tables for each metric type, with
  career_success_index integrated when available. When career_metrics =
  TRUE, includes career metrics in separate list element

## Details

This function is designed to work seamlessly with the impact evaluation
pipeline:

**Integration Workflow:**

1.  **Metrics Calculation**: Use this function to compute comprehensive
    employment metrics across pre/post periods

2.  **Data Preparation**: Use
    [`prepare_metrics_for_impact_analysis`](https://gmontaletti.github.io/longworkR/reference/prepare_metrics_for_impact_analysis.md)
    to bridge metrics output to causal inference format

3.  **Impact Analysis**: Apply methods like
    [`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md)
    using the prepared data

The computed metrics serve as outcome variables in causal inference
studies, allowing researchers to measure treatment effects on various
aspects of employment quality and career trajectories.

**Career Metrics Integration:** When `career_metrics = TRUE`, this
function integrates all validated career metrics from the career
analysis system, including 35+ career metrics such as:

- `career_success_index`: Unified career success measure (0-1)

- `career_advancement_index`: Career progression and transition success
  (0-1)

- `contract_quality_score`: Duration-weighted contract quality (0-1)

- `employment_intensity_score`: Full-time vs part-time patterns (0-1)

- `career_stability_score`: Contract duration consistency (0-1)

- `growth_opportunity_score`: Access to diverse contract types (0-1)

- Career transition, complexity, and stability metrics

These metrics maintain their validated calculations and naming from the
career system.

## See also

[`calculate_comprehensive_career_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_career_metrics.md)
for comprehensive career metrics calculation,
[`prepare_metrics_for_impact_analysis`](https://gmontaletti.github.io/longworkR/reference/prepare_metrics_for_impact_analysis.md)
for bridging metrics to impact analysis,
[`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md)
for causal inference using the metrics,
[`vignette("impact-metrics-integration", package = "longworkR")`](https://gmontaletti.github.io/longworkR/articles/impact-metrics-integration.md)
for complete workflow examples

## Examples

``` r
if (FALSE) { # \dontrun{
# ===== BASIC METRICS CALCULATION =====
# Calculate all metrics for impact analysis
all_metrics <- calculate_comprehensive_impact_metrics(
  data = event_data,
  metrics = "all",
  output_format = "wide"
)

# Calculate specific metrics for focused analysis
stability_quality <- calculate_comprehensive_impact_metrics(
  data = event_data,
  metrics = c("stability", "quality"),
  output_format = "list"
)

# ===== CAREER METRICS INTEGRATION =====
# Include comprehensive career metrics from the career analysis system
all_metrics_with_career <- calculate_comprehensive_impact_metrics(
  data = event_data,
  metrics = "all",
  career_metrics = TRUE,
  output_format = "wide"
)
# Result includes 35+ career metrics: career_success_index, career_advancement_index,
# contract_quality_score, employment_intensity_score, etc.

# Career metrics with specific impact metrics
career_focused <- calculate_comprehensive_impact_metrics(
  data = event_data,
  metrics = c("stability", "quality"),
  career_metrics = TRUE,
  output_format = "list"
)
# Includes both impact and career metrics in separate list elements

# ===== INTEGRATION WITH IMPACT EVALUATION =====
# Complete workflow for causal inference

# Step 1: Calculate comprehensive metrics (including career metrics)
metrics_result <- calculate_comprehensive_impact_metrics(
  data = employment_data,
  metrics = c("stability", "quality", "complexity"),
  career_metrics = TRUE,  # Include 35+ career metrics
  output_format = "wide"
)

# Step 2: Define treatment assignment
treatment_data <- data.table(
  cf = unique(employment_data$cf),
  is_treated = sample(c(0, 1), length(unique(employment_data$cf)), replace = TRUE)
)

# Step 3: Prepare for impact analysis
did_data <- prepare_metrics_for_impact_analysis(
  metrics_output = metrics_result,
  treatment_assignment = treatment_data,
  impact_method = "did"
)

# Step 4: Run difference-in-differences analysis
did_results <- difference_in_differences(
  data = did_data,
  outcome_vars = c("employment_rate", "career_success_index", "career_advancement_index"),
  treatment_var = "is_treated",
  time_var = "post",
  id_var = "cf"
)

# ===== OUTPUT FORMAT OPTIONS =====
# Wide format: All metrics as columns (best for analysis)
metrics_wide <- calculate_comprehensive_impact_metrics(
  data = event_data,
  output_format = "wide"
)

# Long format: Metric names as variables (best for plotting)
metrics_long <- calculate_comprehensive_impact_metrics(
  data = event_data,
  output_format = "long"
)

# List format: Separate tables per metric category (best for detailed examination)
metrics_list <- calculate_comprehensive_impact_metrics(
  data = event_data,
  output_format = "list"
)
} # }
```
