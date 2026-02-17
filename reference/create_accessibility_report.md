# Create Accessibility Report for Network Visualizations

Generates a comprehensive accessibility report for network
visualizations, checking color contrast, layout readability, and
providing recommendations for improving accessibility. Now includes
analysis of consolidation benefits for cleaner, more accessible
visualizations.

## Usage

``` r
create_accessibility_report(
  transitions_data,
  layout = "fr",
  palette = "viridis",
  check_color_contrast = TRUE,
  check_layout_complexity = TRUE,
  return_suggestions = TRUE,
  consolidation_mode = "temporal",
  consolidation_type = "both"
)
```

## Arguments

- transitions_data:

  Data.table with transitions data or pipeline data

- layout:

  Layout algorithm to test

- palette:

  Color palette to test

- check_color_contrast:

  Logical. Check color contrast ratios (default: TRUE)

- check_layout_complexity:

  Logical. Check layout complexity (default: TRUE)

- return_suggestions:

  Logical. Return improvement suggestions (default: TRUE)

- consolidation_mode:

  Character. Consolidation mode to test: "temporal", "employer", or
  "none" (default: "temporal")

- consolidation_type:

  Character. Type of consolidation to test (default: "both")

## Value

List with accessibility assessment and recommendations

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate accessibility report with consolidation assessment
accessibility_report <- create_accessibility_report(pipeline_result, 
                                                    layout = "fr", 
                                                    palette = "viridis")
print(accessibility_report)

# Test specific consolidation type
report_overlapping <- create_accessibility_report(pipeline_result,
                                                  consolidation_type = "overlapping")
} # }
```
