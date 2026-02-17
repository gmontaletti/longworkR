# Create Sample Employment Data for Visualization Testing

Generates synthetic employment data that demonstrates various employment
patterns and can be used for testing and demonstrating the visualization
functions.

## Usage

``` r
create_sample_employment_data(
  n_people = 20,
  n_periods = 4,
  date_range = c("2022-01-01", "2024-12-31"),
  include_overlaps = TRUE,
  include_gaps = TRUE,
  seed = 123
)
```

## Arguments

- n_people:

  Integer. Number of individuals to simulate (default: 20)

- n_periods:

  Integer. Average number of employment periods per person (default: 4)

- date_range:

  Character vector of length 2. Start and end dates (default:
  c("2022-01-01", "2024-12-31"))

- include_overlaps:

  Logical. Include overlapping employment periods (default: TRUE)

- include_gaps:

  Logical. Include unemployment gaps (default: TRUE)

- seed:

  Integer. Random seed for reproducibility (default: 123)

## Value

A data.table with sample employment data ready for vecshift analysis

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
sample_data <- create_sample_employment_data(n_people = 10, n_periods = 3)

# Apply vecshift transformation
result <- vecshift(sample_data)

# Create visualizations
plot_employment_timeline(result)
plot_employment_distribution(result)
} # }
```
