# Create Empty Statistics Result Structure

Helper function to create consistent empty result structures when no
transitions are found. Eliminates code duplication across multiple
locations.

## Usage

``` r
.create_empty_statistics_result(
  statistics_variables,
  pipeline_result,
  output_transition_matrix = FALSE
)
```

## Arguments

- statistics_variables:

  Character vector of statistics variable names

- pipeline_result:

  data.table used to determine variable types

- output_transition_matrix:

  Logical; if TRUE, return empty matrix instead of data.table

## Value

Either an empty matrix or an empty data.table with proper column
structure
