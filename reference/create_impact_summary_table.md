# Create Publication-Ready Impact Summary Tables

Generates professional summary tables for impact evaluation results
including treatment effects, standard errors, confidence intervals, and
statistical significance indicators. Tables are formatted for
publication and can be exported in multiple formats.

## Usage

``` r
create_impact_summary_table(
  analysis_results,
  table_type = "main_results",
  significance_levels = c(0.01, 0.05, 0.1),
  round_digits = 3,
  include_ci = TRUE,
  ci_level = 0.95,
  include_sample_sizes = TRUE,
  table_style = "academic",
  variable_labels = NULL,
  footer_text = NULL,
  caption = NULL
)
```

## Arguments

- analysis_results:

  List. Complete analysis results from impact evaluation

- table_type:

  Character. Type of summary table: "main_results", "robustness",
  "balance", "descriptive", "all". Default: "main_results"

- significance_levels:

  Numeric vector. Significance levels for indicators. Default: c(0.01,
  0.05, 0.10)

- round_digits:

  Integer. Number of decimal places for rounding. Default: 3

- include_ci:

  Logical. Include confidence intervals. Default: TRUE

- ci_level:

  Numeric. Confidence interval level. Default: 0.95

- include_sample_sizes:

  Logical. Include sample size information. Default: TRUE

- table_style:

  Character. Table styling: "academic", "policy", "minimal". Default:
  "academic"

- variable_labels:

  List. Custom variable labels for display. Default: NULL

- footer_text:

  Character. Custom footer text. Default: NULL

- caption:

  Character. Table caption. Default: auto-generated

## Value

A list containing formatted tables (data.table objects) and metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate main results table
main_table <- create_impact_summary_table(
  analysis_results = results,
  table_type = "main_results",
  significance_levels = c(0.01, 0.05, 0.10)
)

# Generate all summary tables
all_tables <- create_impact_summary_table(
  analysis_results = results,
  table_type = "all",
  table_style = "policy"
)
} # }
```
