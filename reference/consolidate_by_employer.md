# Consolidate Employment Contracts by Employer

Helper function that consolidates consecutive contracts with the same
employer within a specified time gap, while never consolidating
contracts from different employers.

## Usage

``` r
consolidate_by_employer(pipeline_result, employer_var, min_lag = 8)
```

## Arguments

- pipeline_result:

  data.table object from process_employment_pipeline()

- employer_var:

  Character string specifying column name containing employer
  identifiers

- min_lag:

  Numeric value specifying maximum gap (in days) between contracts from
  the same employer to be consolidated (default: 8)

## Value

data.table with consolidated employment periods

## Details

This function:

- Groups contracts by person (cf) and employer

- Identifies consecutive contracts within min_lag days

- Consolidates them preserving appropriate column values

- Never consolidates across different employers

Column consolidation strategy:

- Dates: Use first 'inizio', last 'fine'

- Duration: Recalculate as fine - inizio + 1

- Numeric columns: Use sum for additive values, mean for rates

- Character columns: Use first value (mode if available)
