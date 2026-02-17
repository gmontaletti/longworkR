# Vectorized professional trajectory calculation helper function

Vectorized professional trajectory calculation helper function

## Usage

``` r
calculate_professional_trajectories_vectorized(
  dt,
  quarters_dt,
  start_col,
  end_col,
  person_col,
  arco_col,
  qualifica_col,
  reference_codes,
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

- qualifica_col:

  Professional code column name

- reference_codes:

  data.table with reference professional codes per person

- quarter_days:

  Number of days per quarter

## Value

data.table with professional trajectories
