# Black and White Fill Scale with Pattern Support

A specialized fill scale that combines the improved black and white
vecshift palette with pattern support for maximum accessibility and
distinction in printed materials or colorblind-accessible
visualizations.

## Usage

``` r
scale_fill_vecshift_bw(
  patterns = "employment",
  use_ggpattern = TRUE,
  border_color = "black",
  border_size = 0.3,
  alpha = 0.8,
  ...
)
```

## Arguments

- patterns:

  Character. Pattern type to use (see
  [`vecshift_patterns`](https://gmontaletti.github.io/longworkR/reference/vecshift_patterns.md))

- use_ggpattern:

  Logical. Whether to use ggpattern if available (default: TRUE)

- border_color:

  Character. Border color for bars/areas (default: "black")

- border_size:

  Numeric. Border line width (default: 0.3)

- alpha:

  Numeric. Fill transparency (default: 0.8)

- ...:

  Additional arguments passed to ggplot2 scale functions

## Value

A ggplot2 scale function with pattern support

## See also

[`vecshift_patterns`](https://gmontaletti.github.io/longworkR/reference/vecshift_patterns.md),
[`vecshift_colors`](https://gmontaletti.github.io/longworkR/reference/vecshift_colors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Basic usage with patterns
ggplot(data, aes(x, y, fill = employment_status)) +
  geom_col() +
  scale_fill_vecshift_bw("employment")

# With custom border
ggplot(data, aes(x, y, fill = contract_type)) +
  geom_col() +
  scale_fill_vecshift_bw("contracts", border_color = "#2C3E50")
} # }
```
