# Calculate Mode Using Optimized data.table Operations

Fast mode (most frequent value) calculation using data.table's `.N`
counter instead of base R's
[`table()`](https://rdrr.io/r/base/table.html) function. Significantly
faster for large character vectors.

## Usage

``` r
.calculate_mode_optimized(x, na.rm = TRUE)
```

## Arguments

- x:

  Character or factor vector

- na.rm:

  Logical; if TRUE, remove NA values before calculation

## Value

Most frequent value in x, or NA if x is empty/all NA

## Details

Performance: ~5x faster than table() + which.max() for 100K element
vectors
