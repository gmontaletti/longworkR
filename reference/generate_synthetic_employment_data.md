# Generate Synthetic Employment Data

Creates synthetic employment data that mimics the structure and
statistical properties of real employment datasets while ensuring no
real personal data is included. This function is used to generate
public-safe test data for the longworkR package.

## Usage

``` r
generate_synthetic_employment_data(
  n_individuals = 4252,
  n_contracts = 476400,
  start_date = as.Date("2021-01-01"),
  end_date = as.Date("2024-12-31"),
  seed = 12345
)
```

## Arguments

- n_individuals:

  Integer. Number of unique individuals to generate (default: 4252)

- n_contracts:

  Integer. Total number of employment contracts to generate (default:
  476400)

- start_date:

  Date. Earliest possible contract start date (default: "2021-01-01")

- end_date:

  Date. Latest possible contract end date (default: "2024-12-31")

- seed:

  Integer. Random seed for reproducibility (default: 12345)

## Value

A data.table with synthetic employment data matching the structure of
vecshift-processed employment records

## Details

The synthetic data generator creates realistic employment patterns
including:

- Multiple contracts per individual with realistic durations

- Employment states (occupied part-time, full-time, unemployed,
  overlaps)

- Contract types following Italian employment classification codes

- Demographic information (age, gender, education level)

- Geographic distribution across Italian provinces

- Salary information with realistic distributions

- Employment transitions and consolidation periods (over_id)

- Impact evaluation attributes for DiD and policy analysis

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate default synthetic dataset
synthetic_data <- generate_synthetic_employment_data()

# Generate smaller dataset for testing
test_data <- generate_synthetic_employment_data(
  n_individuals = 100,
  n_contracts = 1000
)

# Save synthetic data for package distribution
saveRDS(synthetic_data, "data/synthetic_sample.rds")
} # }
```
