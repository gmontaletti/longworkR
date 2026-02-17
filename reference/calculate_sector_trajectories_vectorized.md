# Vectorized sector trajectory calculation helper function

Vectorized sector trajectory calculation helper function

## Usage

``` r
calculate_sector_trajectories_vectorized(
  dt,
  quarters_dt,
  start_col,
  end_col,
  person_col,
  arco_col,
  sector_col,
  reference_sectors,
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

- sector_col:

  Sector column name

- reference_sectors:

  data.table with reference sectors per person

- quarter_days:

  Number of days per quarter

## Value

data.table with sector trajectories
