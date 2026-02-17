# Create Time-Based Animated Employment Transitions

Creates an animated visualization showing how employment transitions
evolve over different time periods. Requires employment data with time
period information.

## Usage

``` r
plot_transitions_over_time(
  pipeline_results,
  time_periods,
  transition_variable,
  animation_speed = 2000,
  layout = "force",
  ...
)
```

## Arguments

- pipeline_results:

  List of pipeline results for different time periods

- time_periods:

  Character vector of time period labels (same length as
  pipeline_results)

- transition_variable:

  Character string specifying the variable to analyze transitions for

- animation_speed:

  Numeric. Speed of animation in milliseconds between frames. Default:
  2000

- layout:

  Character string specifying layout algorithm. Default: "force"

- ...:

  Additional parameters passed to plot_interactive_transitions()

## Value

A g6R htmlwidget with time-based animation controls

## Examples

``` r
if (FALSE) { # \dontrun{
# Create time series of employment data
results_2022 <- process_employment_pipeline(data_2022, merge_columns = "company")
results_2023 <- process_employment_pipeline(data_2023, merge_columns = "company") 
results_2024 <- process_employment_pipeline(data_2024, merge_columns = "company")

# Create animated visualization
plot_transitions_over_time(
  pipeline_results = list(results_2022, results_2023, results_2024),
  time_periods = c("2022", "2023", "2024"),
  transition_variable = "company",
  animation_speed = 3000
)
} # }
```
