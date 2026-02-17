# Vecshift Color Scale for ggplot2

Color scales that use vecshift color palettes for ggplot2
visualizations.

## Usage

``` r
scale_color_vecshift(
  palette = "main",
  discrete = TRUE,
  reverse = FALSE,
  alpha = 1,
  ...
)

scale_colour_vecshift(
  palette = "main",
  discrete = TRUE,
  reverse = FALSE,
  alpha = 1,
  ...
)

scale_fill_vecshift(
  palette = "main",
  discrete = TRUE,
  reverse = FALSE,
  alpha = 1,
  ...
)
```

## Arguments

- palette:

  Character. Palette name (see
  [`vecshift_colors`](https://gmontaletti.github.io/longworkR/reference/vecshift_colors.md))

- discrete:

  Logical. Use discrete colors (TRUE) or continuous (FALSE)

- reverse:

  Logical. Reverse color order

- alpha:

  Numeric. Transparency level (0-1)

- ...:

  Additional arguments passed to ggplot2 scale functions

## Value

A ggplot2 scale function

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Discrete color scale
ggplot(data, aes(x, y, color = employment_status)) +
  geom_point() +
  scale_color_vecshift("employment")

# Fill scale
ggplot(data, aes(x, y, fill = contract_type)) +
  geom_bar(stat = "identity") +
  scale_fill_vecshift("contracts")
} # }
```
