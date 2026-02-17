# Vectorized employer trajectory calculation helper function

Vectorized employer trajectory calculation helper function

## Usage

``` r
calculate_employer_trajectories_vectorized(
  dt,
  quarters_dt,
  start_col,
  end_col,
  person_col,
  arco_col,
  employer_col,
  reference_employers,
  quarter_days
)
```

## Arguments

- dt:

  Employment data

- quarters_dt:

  Quarter data for all persons

- start_col:

  Start date column name

- end_col:

  End date column name

- person_col:

  Person ID column name

- arco_col:

  Arco column name for employment status

- employer_col:

  Employer column name

- reference_employers:

  data.table with reference employers per person

- quarter_days:

  Number of days per quarter

## Value

data.table with employer trajectories
