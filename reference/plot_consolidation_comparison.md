# Compare Raw vs Consolidated Transitions

Creates side-by-side comparison plots showing the difference between raw
transitions and consolidated transitions using over_id grouping.
Demonstrates the benefits of consolidation for cleaner network
visualization.

## Usage

``` r
plot_consolidation_comparison(
  pipeline_result,
  transition_variable = "prior",
  layout = "fr",
  palette = "viridis",
  accessibility_mode = FALSE,
  consolidation_type = "both",
  title = NULL
)
```

## Arguments

- pipeline_result:

  Output from vecshift() with over_id column

- transition_variable:

  Character. Variable for transition analysis (default: "prior")

- layout:

  Character. Network layout (default: "fr")

- palette:

  Character. Color palette (default: "viridis")

- accessibility_mode:

  Logical. Use high contrast mode (default: FALSE)

- consolidation_type:

  Character. Type of consolidation (default: "both")

- title:

  Character. Custom title (default: auto-generated)

## Value

A combined ggplot2 object with before/after comparison

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic comparison
plot_consolidation_comparison(pipeline_result)

# Company transitions with overlapping consolidation only
plot_consolidation_comparison(pipeline_result, transition_variable = "company", 
                             consolidation_type = "overlapping")
} # }
```
