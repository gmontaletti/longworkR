# Compute Temporal Employment Indicators from vecshift Data

Computes comprehensive employment indicators (employment rate, contract
quality, wage statistics, stability measures, and transition patterns)
at quarterly or monthly intervals from vecshift-processed employment
data. Supports flexible aggregation and consolidation options for robust
impact evaluation analysis.

## Usage

``` r
compute_temporal_employment_indicators(
  data,
  period_type = "quarterly",
  indicators = c("employment_rate", "contract_quality", "stability_index"),
  id_var = "cf",
  start_date_var = "inizio",
  end_date_var = "fine",
  duration_var = "durata",
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  employment_intensity_var = "prior",
  employment_var = "arco",
  wage_var = NULL,
  over_id_var = "over_id",
  use_consolidation = TRUE,
  reference_date = NULL,
  employment_threshold = 1,
  quality_weights = NULL,
  group_vars = NULL,
  output_format = "long",
  verbose = FALSE
)
```

## Arguments

- data:

  data.table containing vecshift-processed employment records

- period_type:

  Character. Temporal aggregation: "quarterly" or "monthly". Default:
  "quarterly"

- indicators:

  Character vector. Indicators to compute. Options:

  - "employment_rate": Proportion of time employed in each period

  - "contract_quality": Duration-weighted average contract quality (0-1
    scale)

  - "wage_statistics": Average wages, wage growth (when available)

  - "stability_index": Employment stability and contract duration
    metrics

  - "contract_types": Distribution of contract types by period

  - "transition_patterns": Employment state transitions between periods

  Default: c("employment_rate", "contract_quality", "stability_index")

- id_var:

  Character. Individual identifier column name. Default: "cf"

- start_date_var:

  Character. Contract start date column. Default: "inizio"

- end_date_var:

  Character. Contract end date column. Default: "fine"

- duration_var:

  Character. Contract duration column (days). Default: "durata"

- contract_type_var:

  Character. Contract type code column. Default:
  "COD_TIPOLOGIA_CONTRATTUALE"

- employment_intensity_var:

  Character. Employment intensity column (1=full-time). Default: "prior"

- employment_var:

  Character. Employment status column (arco \>= 1 = employed). Default:
  "arco"

- wage_var:

  Character. Wage/salary column. Default: NULL (auto-detect)

- over_id_var:

  Character. Consolidation identifier from vecshift. Default: "over_id"

- use_consolidation:

  Logical. Use over_id for period consolidation? Default: TRUE

- reference_date:

  Date. Reference date for period numbering. Default: NULL (min date)

- employment_threshold:

  Numeric. Minimum days to count as employed in period. Default: 1

- quality_weights:

  **DEPRECATED**. This parameter is ignored. Contract quality is now
  calculated using Kaplan-Meier survival analysis to determine median
  survival duration for each contract type, providing data-driven
  quality scores (0.3-1.0) rather than arbitrary fixed weights. This
  scientific approach ensures quality metrics reflect actual employment
  durability patterns in your data.

- group_vars:

  Character vector. Variables for subgroup analysis. Default: NULL

- output_format:

  Character. "long" (period-person rows) or "wide" (period columns).
  Default: "long"

- verbose:

  Logical. Print progress information? Default: FALSE

## Value

data.table with temporal employment indicators:

- **cf**: Individual identifier

- **period**: Temporal period number (1, 2, 3, ...)

- **period_start/period_end**: Period date range

- **employment_rate**: Proportion of period employed (0-1)

- **contract_quality**: Duration-weighted average quality (0-1)

- **avg_wage**: Average wage when employed (if available)

- **stability_index**: Employment stability score (0-1)

- **n_contracts**: Number of contracts in period

- **total_days_employed**: Total employment days in period

- **pct_permanent**: Proportion of time in permanent contracts

- **transition_from/transition_to**: Employment transitions (if
  requested)

Additional columns depend on requested indicators and group variables.

## Details

**Period Calculation**: Periods start from the reference date (default:
earliest contract date). Quarterly periods span ~90 days, monthly
periods ~30 days. Contracts spanning multiple periods contribute
proportionally to each period.

**Consolidation**: When use_consolidation=TRUE, overlapping contracts
with same over_id are consolidated before analysis, providing cleaner
employment episode identification.

**Employment Rate**: Calculated as days_employed / period_duration,
capped at 1.0 for periods with overlapping contracts. Minimum employment
threshold filters noise.

**Contract Quality**: Calculated using Kaplan-Meier survival analysis to
determine median survival duration for each contract type. Contract
types with longer median survival times receive higher quality scores
(0.3-1.0 scale), where 1.0 represents the most durable contract types
and 0.3 the least durable. This empirical, data-driven approach replaces
arbitrary fixed weights (e.g., permanent contracts = 1.0, temporary =
0.5) with actual employment durability metrics derived from survival
analysis of the employment data itself.

**Stability Index**: Combines contract duration consistency, employment
continuity, and transition frequency into a 0-1 stability measure.

## See also

[`difference_in_differences`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md)
for impact evaluation,
[`analyze_employment_transitions`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md)
for transition analysis,
[`calculate_career_success_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_career_success_metrics.md)
for career metrics

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample vecshift data
employment_data <- readRDS("data/sample.rds")

# Basic quarterly employment indicators with survival-based contract quality
quarterly_indicators <- compute_temporal_employment_indicators(
  data = employment_data,
  period_type = "quarterly",
  indicators = c("employment_rate", "contract_quality", "stability_index")
)
# Note: contract_quality automatically uses survival analysis to determine
# quality scores based on median survival duration by contract type

# Monthly indicators with wage analysis
monthly_indicators <- compute_temporal_employment_indicators(
  data = employment_data,
  period_type = "monthly",
  indicators = c("employment_rate", "contract_quality", "wage_statistics"),
  wage_var = "retribuzione_media"
)

# Comprehensive analysis with transitions
comprehensive_indicators <- compute_temporal_employment_indicators(
  data = employment_data,
  indicators = c("employment_rate", "contract_quality", "stability_index",
                "contract_types", "transition_patterns"),
  group_vars = c("gender", "education"),
  output_format = "wide"
)

# Examine survival-based quality scores by contract type
# This shows how different contract types are ranked by durability
quality_by_type <- comprehensive_indicators[, .(
  avg_quality = mean(avg_contract_quality, na.rm = TRUE),
  median_quality = median(avg_contract_quality, na.rm = TRUE)
), by = contract_type][order(-avg_quality)]
print(quality_by_type)  # Higher scores = more durable contract types

# Integration with impact evaluation
treatment_assignment <- data.table(
  cf = unique(employment_data$cf),
  treatment_group = sample(c("control", "training", "subsidy"),
                          length(unique(employment_data$cf)), replace = TRUE)
)

indicators_with_treatment <- merge(quarterly_indicators, treatment_assignment, by = "cf")

# Difference-in-differences analysis
did_analysis <- difference_in_differences(
  data = indicators_with_treatment,
  outcome_vars = c("employment_rate", "contract_quality", "stability_index"),
  treatment_var = "treatment_group",
  time_var = "period",
  id_var = "cf"
)
} # }
```
