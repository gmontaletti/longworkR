# Test g6r Accessibility Features

Tests and demonstrates accessibility features in g6r visualizations,
including colorblind-friendly palettes and high contrast modes.

## Usage

``` r
test_g6r_accessibility(transitions_data, test_scenarios = "all")
```

## Arguments

- transitions_data:

  Employment transitions data

- test_scenarios:

  Character vector of accessibility scenarios to test. Options:
  "colorblind", "high_contrast", "large_nodes", "all". Default: "all"

## Value

List of g6r plots demonstrating different accessibility modes

## Examples

``` r
if (FALSE) { # \dontrun{
# Test accessibility features
demo_data <- generate_g6r_demo_data()
pipeline_result <- process_employment_pipeline(demo_data, merge_columns = "company")
transitions <- analyze_employment_transitions(pipeline_result, "company")

accessibility_tests <- test_g6r_accessibility(transitions)

# View different accessibility modes
accessibility_tests$colorblind_safe
accessibility_tests$high_contrast
accessibility_tests$large_elements
} # }
```
