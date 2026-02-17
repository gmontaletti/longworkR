# Get Line Type Patterns for Time Series

Returns line type specifications for time series plots to improve visual
distinction in black and white visualizations.

## Usage

``` r
vecshift_linetypes(categories, style = "employment", reverse = FALSE)
```

## Arguments

- categories:

  Character vector. Categories to map to line types

- style:

  Character. Line type style:

  - "employment": For employment status data

  - "basic": Basic line types

  - "varied": Maximum variety in line patterns

- reverse:

  Logical. Whether to reverse the line type order (default: FALSE)

## Value

A named character vector of ggplot2 line types

## See also

[`scale_linetype_vecshift`](https://gmontaletti.github.io/longworkR/reference/scale_linetype_vecshift.md)

## Examples

``` r
# Get line types for employment statuses
employment_lines <- vecshift_linetypes(c("occ_ft", "occ_pt", "disoccupato"))
print(employment_lines)
#>      occ_ft      occ_pt disoccupato 
#>     "solid"    "dashed"  "longdash" 

# Get basic line types
basic_lines <- vecshift_linetypes(c("A", "B", "C"), style = "basic")
```
