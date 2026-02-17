# Prepare Metrics Output for Impact Analysis

This function serves as a bridge between the output of
[`calculate_comprehensive_impact_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_impact_metrics.md)
and impact evaluation methods like
[`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md).
It restructures the metrics data to create a proper panel structure
suitable for causal inference methods.

## Usage

``` r
prepare_metrics_for_impact_analysis(
  metrics_output,
  treatment_assignment,
  impact_method = c("did", "event_study", "matching"),
  id_column = "cf",
  period_column = "period",
  outcome_vars = NULL,
  auto_detect_outcomes = TRUE,
  verbose = TRUE
)
```

## Arguments

- metrics_output:

  data.table or list. Output from
  [`calculate_comprehensive_impact_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_impact_metrics.md)

- treatment_assignment:

  data.table. A data.table with columns for the individual identifier
  (default: "cf") and treatment indicator (default: "is_treated"). This
  maps individuals to their treatment status.

- impact_method:

  character. The intended impact evaluation method. One of "did"
  (difference-in-differences), "event_study", or "matching". This
  affects how the data is restructured. Default: "did"

- id_column:

  character. Name of the individual identifier column. Default: "cf"

- period_column:

  character. Name of the period column in metrics_output. Default:
  "period"

- outcome_vars:

  character vector or NULL. Names of outcome variables to include in the
  analysis. If NULL and auto_detect_outcomes is TRUE, automatically
  detects numeric metrics columns. Default: NULL

- auto_detect_outcomes:

  logical. Whether to automatically detect outcome variables from the
  metrics output. Default: TRUE

- verbose:

  logical. Whether to print diagnostic information. Default: TRUE

## Value

data.table with the following structure:

- **Individual identifiers**: Column specified by id_column

- **Treatment indicator**: "is_treated" (0/1)

- **Time variables**: Both original period column and binary "post"
  variable

- **Outcome variables**: Selected metrics suitable for analysis

- **Panel structure**: Proper pre/post observations for all units

The output is directly compatible with
[`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md)
and other impact evaluation functions in the package.

## Details

This function handles several key transformations needed for impact
analysis:

### Data Structure Transformations

- **Panel Creation**: For treated units with "pre"/"post" periods,
  maintains both observations

- **Control Expansion**: For control units (typically with "control"
  period), creates duplicate observations with "pre" and "post" labels
  to enable proper DiD estimation

- **Time Variable Creation**: Creates binary time variables (post = 0/1)
  suitable for regression analysis

- **Treatment Interaction**: Ensures proper interaction terms can be
  created for DiD estimation

### Outcome Variable Selection

The function automatically detects numeric metrics as potential
outcomes, excluding ID and time variables. Common detected outcomes
include:

- Employment stability metrics (employment_rate, employment_spells)

- Contract quality metrics (permanent_contract_rate,
  avg_contract_quality)

- Career complexity metrics (transition_complexity, career_entropy)

- Transition metrics (avg_transition_duration, transition_success_rate)

### Compatibility with Impact Methods

- **DiD**: Creates standard panel with binary post treatment variable

- **Event Study**: Maintains event_time structure for relative time
  analysis

- **Matching**: Preserves original structure for propensity score
  matching

## See also

[`calculate_comprehensive_impact_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_impact_metrics.md),
[`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md),
[`propensity_score_matching`](https://gmontaletti.github.io/longworkR/reference/propensity_score_matching.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ===== BASIC WORKFLOW EXAMPLE =====
# Load sample data
sample_data <- readRDS(system.file("data", "sample.rds", package = "longworkR"))

# Step 1: Calculate comprehensive employment metrics
metrics_result <- calculate_comprehensive_impact_metrics(
  data = sample_data,
  metrics = c("stability", "quality", "complexity"),
  output_format = "wide"
)

# Step 2: Define treatment assignment
treatment_data <- data.table(
  cf = unique(sample_data$cf),
  is_treated = sample(c(0, 1), length(unique(sample_data$cf)), replace = TRUE),
  # Add baseline covariates for matching/controls
  baseline_employment = runif(length(unique(sample_data$cf))),
  region = sample(c("urban", "rural"), length(unique(sample_data$cf)), replace = TRUE)
)

# Step 3: Prepare for DiD analysis
did_data <- prepare_metrics_for_impact_analysis(
  metrics_output = metrics_result,
  treatment_assignment = treatment_data,
  impact_method = "did",
  verbose = TRUE
)

# Step 4: Run difference-in-differences
did_results <- difference_in_differences(
  data = did_data,
  outcome_vars = c("employment_rate", "permanent_contract_rate", "career_stability_score"),
  treatment_var = "is_treated",
  time_var = "post",
  id_var = "cf",
  control_vars = c("baseline_employment")
)

# ===== ADVANCED SCENARIOS =====

# Event Study Analysis
# First create event time structure in original data
sample_data[, event_time := ifelse(event_period == "pre", -1, 
                                  ifelse(event_period == "post", 1, 0))]

# Recalculate metrics with event time
metrics_event <- calculate_comprehensive_impact_metrics(
  data = sample_data,
  metrics = "all",
  period_column = "event_time"
)

# Prepare for event study
event_data <- prepare_metrics_for_impact_analysis(
  metrics_output = metrics_event,
  treatment_assignment = treatment_data,
  impact_method = "event_study"
)

# Multiple Treatment Groups
treatment_multi <- copy(treatment_data)
treatment_multi[, `:=`(
  treatment_type = sample(c("none", "training", "subsidies", "both"), .N, replace = TRUE),
  is_treated = as.numeric(treatment_type != "none")
)]

multi_data <- prepare_metrics_for_impact_analysis(
  metrics_output = metrics_result,
  treatment_assignment = treatment_multi,
  impact_method = "did"
)

# Propensity Score Matching Setup
matching_data <- prepare_metrics_for_impact_analysis(
  metrics_output = metrics_result,
  treatment_assignment = treatment_data,
  impact_method = "matching",
  outcome_vars = c("employment_rate", "contract_quality_score"),
  auto_detect_outcomes = FALSE  # Specify outcomes explicitly
)

# ===== DATA VALIDATION =====
# Check prepared data structure
validation <- validate_integration_setup(
  data = did_data,
  impact_method = "did",
  id_column = "cf",
  verbose = TRUE
)

if (validation) {
  cat("Data successfully prepared for impact analysis\n")
}
} # }
```
