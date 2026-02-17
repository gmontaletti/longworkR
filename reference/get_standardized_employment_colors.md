# Get Standardized Employment Colors

Internal function to ensure consistent employment status colors across
all plotting functions. This function guarantees that the same
employment state (e.g., unemployment, full-time, part-time) gets the
same color in all visualizations, which is critical for publication
quality and preventing reader confusion.

## Usage

``` r
get_standardized_employment_colors(statuses)
```

## Arguments

- statuses:

  Character vector of employment statuses present in the data

## Value

Named character vector of hex colors, where names are employment
statuses
