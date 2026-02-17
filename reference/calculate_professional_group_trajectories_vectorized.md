# Calculate professional group trajectories vectorized (internal helper function)

Calculate professional group trajectories vectorized (internal helper
function)

## Usage

``` r
calculate_professional_group_trajectories_vectorized(
  dt,
  quarters_dt,
  start_col,
  end_col,
  person_col,
  arco_col,
  group_col,
  reference_groups,
  quarter_days,
  group_mapping
)
```

## Arguments

- dt:

  data.table with employment contracts

- quarters_dt:

  data.table with quarters per person

- start_col:

  Start date column name

- end_col:

  End date column name

- person_col:

  Person ID column name

- arco_col:

  Employment intensity column name

- group_col:

  Professional group column name

- reference_groups:

  data.table with reference groups per person

- quarter_days:

  Number of days per quarter

- group_mapping:

  Named vector mapping groups (keeps original group names)

## Value

data.table with professional group trajectories
