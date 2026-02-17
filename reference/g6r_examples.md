# Employment Transitions g6r Examples and Tutorials

This section provides comprehensive examples for using g6r with
employment transition data. Examples range from basic usage to advanced
interactive features.

Creates realistic sample employment data suitable for demonstrating g6r
interactive transition visualizations.

## Usage

``` r
generate_g6r_demo_data(
  n_persons = 50,
  n_companies = 10,
  time_span_years = 3,
  transition_probability = 0.3,
  seed = 42
)
```

## Arguments

- n_persons:

  Number of persons to generate. Default: 50

- n_companies:

  Number of different companies. Default: 10

- time_span_years:

  Number of years to span. Default: 3

- transition_probability:

  Probability of job transition per year. Default: 0.3

- seed:

  Random seed for reproducibility. Default: 42

## Value

data.table with employment records suitable for vecshift pipeline

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample data
sample_data <- generate_g6r_demo_data()

# Process through pipeline
pipeline_result <- process_employment_pipeline(
  sample_data, 
  merge_columns = c("company", "salary", "region")
)

# Analyze transitions
transitions <- analyze_employment_transitions(
  pipeline_result, 
  transition_variable = "company"
)

# Create interactive visualization
plot_interactive_transitions(transitions)
} # }
```
