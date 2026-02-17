# Assess Treatment Event Quality

Provides diagnostic information about treatment event identification
including timing distributions, balance assessment, and data quality
metrics.

## Usage

``` r
assess_treatment_event_quality(
  event_data,
  assessment_variables = NULL,
  output_format = "summary"
)
```

## Arguments

- event_data:

  Data.table from identify_treatment_events()

- assessment_variables:

  Character vector of variables to include in balance assessment

- output_format:

  Character. Output format: "summary", "detailed", or "both"

## Value

A list containing:

- event_summary:

  Summary statistics of identified events

- timing_distribution:

  Distribution of event timing

- balance_table:

  Balance assessment between treatment and control

- data_quality:

  Data quality metrics

- recommendations:

  Recommendations for improving identification

## Examples

``` r
if (FALSE) { # \dontrun{
assessment <- assess_treatment_event_quality(
  event_data = identified_events,
  assessment_variables = c("age", "sector", "prior_employment")
)
print(assessment$event_summary)
} # }
```
