# Compare Employment Transitions Between Groups

Creates side-by-side interactive visualizations to compare employment
transitions between different groups (e.g., regions, demographics, time
periods).

## Usage

``` r
compare_transitions_between_groups(
  transitions_list,
  group_names = NULL,
  layout = "force",
  sync_layouts = TRUE,
  ...
)
```

## Arguments

- transitions_list:

  Named list of transition data for each group

- group_names:

  Character vector of group names (optional, uses list names if NULL)

- layout:

  Character string specifying layout algorithm. Default: "force"

- sync_layouts:

  Logical. Whether to synchronize node positions across groups for
  easier comparison. Default: TRUE

- ...:

  Additional parameters passed to plot_interactive_transitions()

## Value

List of g6R htmlwidgets, one for each group

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare transitions between regions
north_transitions <- analyze_employment_transitions(north_data, "company")
south_transitions <- analyze_employment_transitions(south_data, "company")

# Create comparison
comparison_plots <- compare_transitions_between_groups(
  transitions_list = list(
    "North Region" = north_transitions,
    "South Region" = south_transitions
  ),
  layout = "circular",
  min_weight_threshold = 3
)

# Display in Shiny or combine in HTML document
comparison_plots$`North Region`
comparison_plots$`South Region`
} # }
```
