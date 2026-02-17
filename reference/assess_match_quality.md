# Match Quality Diagnostics

Provides comprehensive diagnostics for matching quality including common
support, match distances, and recommendations for improving matches.

## Usage

``` r
assess_match_quality(matching_result, diagnostic_plots = TRUE)
```

## Arguments

- matching_result:

  List object from propensity_score_matching() or
  coarsened_exact_matching()

- diagnostic_plots:

  Logical. Generate data for diagnostic plots? Default: TRUE

## Value

A list containing:

- quality_summary:

  Overall quality assessment

- match_distances:

  Distribution of matching distances

- common_support_analysis:

  Common support region analysis

- recommendations:

  Specific recommendations for improvement

- diagnostic_data:

  Data for creating diagnostic plots

## Examples

``` r
if (FALSE) { # \dontrun{
diagnostics <- assess_match_quality(
  matching_result = ps_match,
  diagnostic_plots = TRUE
)
print(diagnostics$quality_summary)
} # }
```
