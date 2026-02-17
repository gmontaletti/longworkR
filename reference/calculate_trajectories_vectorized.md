# Vectorized trajectory calculation helper function

Vectorized trajectory calculation helper function

## Usage

``` r
calculate_trajectories_vectorized(
  dt,
  quarters_dt,
  start_col,
  end_col,
  person_col,
  arco_col,
  employment_thresholds,
  quarter_days,
  reference_dates = NULL
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

- employment_thresholds:

  Numeric vector with employment classification thresholds

- quarter_days:

  Number of days per quarter

- reference_dates:

  Optional data.table with reference dates and person_vars to merge

## Value

data.table with employment trajectories
