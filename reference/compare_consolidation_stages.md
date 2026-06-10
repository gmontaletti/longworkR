# Compare consolidation stages

Builds a compact comparison report across consolidation stages of an
employment data pipeline. For each stage it reports the number of
records and the number of unique persons, making it easy to verify how
much each consolidation step reduces the dataset.

## Usage

``` r
compare_consolidation_stages(stages)
```

## Arguments

- stages:

  A named list of data.tables (or data.frames), one per consolidation
  stage, each containing a `cf` person identifier column. List names are
  used as stage labels.

## Value

A data.table with one row per stage and columns:

- `stage`: stage label (list name)

- `records`: number of rows in the stage dataset

- `unique_persons`: number of distinct `cf` values

## Examples

``` r
raw <- data.table::data.table(cf = c("A", "A", "B", "C"))
consolidated <- data.table::data.table(cf = c("A", "B", "C"))
compare_consolidation_stages(list(raw = raw, consolidated = consolidated))
#>           stage records unique_persons
#>          <char>   <int>          <int>
#> 1:          raw       4              3
#> 2: consolidated       3              3
```
