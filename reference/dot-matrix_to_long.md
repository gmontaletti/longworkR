# Convert Matrix to Long Format for Heatmaps

Internal helper to convert transition matrices to long format for
ggplot2.

## Usage

``` r
.matrix_to_long(matrix, cell_value, min_value)
```

## Arguments

- matrix:

  Transition matrix

- cell_value:

  Type of values to display

- min_value:

  Minimum value threshold

## Value

data.table in long format
