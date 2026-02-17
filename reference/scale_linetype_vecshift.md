# Line Type Scale for Vecshift Visualizations

A specialized line type scale that maps categories to distinct line
patterns optimized for black and white visualizations and accessibility.

## Usage

``` r
scale_linetype_vecshift(
  categories = NULL,
  style = "employment",
  reverse = FALSE,
  ...
)
```

## Arguments

- categories:

  Character vector. Categories to map (if NULL, uses data)

- style:

  Character. Line type style (see
  [`vecshift_linetypes`](https://gmontaletti.github.io/longworkR/reference/vecshift_linetypes.md))

- reverse:

  Logical. Whether to reverse line type order (default: FALSE)

- ...:

  Additional arguments passed to ggplot2 scale functions

## Value

A ggplot2 linetype scale function

## See also

[`vecshift_linetypes`](https://gmontaletti.github.io/longworkR/reference/vecshift_linetypes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# For employment data
ggplot(data, aes(x = time, y = count, linetype = employment_status)) +
  geom_line() +
  scale_linetype_vecshift(style = "employment")

# For general categories
ggplot(data, aes(x = time, y = value, linetype = category)) +
  geom_line() +
  scale_linetype_vecshift(style = "basic")
} # }
```
