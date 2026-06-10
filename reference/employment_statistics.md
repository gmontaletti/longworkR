# Extract employment statistics

Computes summary statistics from consolidated employment data produced
by the vecshift/longworkR consolidation chain. Employment and
unemployment spells are distinguished via the `arco` indicator
(`arco == 1` employment, `arco == 0` unemployment); durations are taken
from the `durata` column. On consolidated data overlaps are already
resolved, so `arco` takes only the values 0 and 1.

## Usage

``` r
employment_statistics(data)
```

## Arguments

- data:

  A data.table (or data.frame) of consolidated employment spells
  containing at least the columns `cf` (person identifier), `durata`
  (spell duration in days), and `arco` (concurrent employment indicator,
  0 for unemployment spells).

## Value

A named list with elements:

- `total_records`: total number of spells

- `unique_persons`: number of distinct `cf` values

- `total_employment_days`: sum of `durata` for employment spells

- `total_unemployment_days`: sum of `durata` for unemployment spells

- `avg_employment_duration`: mean `durata` of employment spells

- `avg_unemployment_duration`: mean `durata` of unemployment spells

`NA` values in `durata` are ignored (`na.rm = TRUE`).

## Examples

``` r
dt <- data.table::data.table(
  cf = c("A", "A", "B"),
  durata = c(100, 30, 200),
  arco = c(1, 0, 1)
)
employment_statistics(dt)
#> $total_records
#> [1] 3
#> 
#> $unique_persons
#> [1] 2
#> 
#> $total_employment_days
#> [1] 300
#> 
#> $total_unemployment_days
#> [1] 30
#> 
#> $avg_employment_duration
#> [1] 150
#> 
#> $avg_unemployment_duration
#> [1] 30
#> 
```
