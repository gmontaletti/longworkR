# Create Temporal Indicators Summary for Multiple Groups

Generates summary statistics of temporal employment indicators across
treatment groups or other categorical variables for impact evaluation
analysis. Provides group comparisons and statistical tests.

## Usage

``` r
create_temporal_indicators_summary(
  temporal_data,
  group_var = NULL,
  indicator_vars = NULL,
  summary_stats = c("mean", "median", "sd", "n"),
  statistical_tests = TRUE,
  period_range = NULL,
  output_format = "summary"
)
```

## Arguments

- temporal_data:

  data.table output from compute_temporal_employment_indicators()

- group_var:

  Character. Grouping variable name (e.g., "treatment_group"). Default:
  NULL

- indicator_vars:

  Character vector. Indicator variables to summarize. Default:
  auto-detect

- summary_stats:

  Character vector. Statistics to compute: c("mean", "median", "sd",
  "n"). Default: all

- statistical_tests:

  Logical. Perform group comparison tests? Default: TRUE

- period_range:

  Integer vector. Periods to include in analysis. Default: NULL (all)

- output_format:

  Character. "summary" or "detailed". Default: "summary"

## Value

data.table with summary statistics by group and period

## Examples

``` r
if (FALSE) { # \dontrun{
# Create temporal indicators
indicators <- compute_temporal_employment_indicators(employment_data)

# Add treatment assignment
indicators[, treatment_group := sample(c("control", "treatment"), nrow(indicators), replace = TRUE)]

# Generate group summary
group_summary <- create_temporal_indicators_summary(
  indicators,
  group_var = "treatment_group",
  indicator_vars = c("employment_rate", "contract_quality"),
  statistical_tests = TRUE
)
} # }
```
