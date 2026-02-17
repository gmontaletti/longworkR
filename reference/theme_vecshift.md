# Vecshift ggplot2 Theme

A clean, minimalist ggplot2 theme designed specifically for employment
data visualization. Emphasizes clarity, accessibility, and professional
appearance while reducing chart junk and focusing attention on the data.

## Usage

``` r
theme_vecshift(
  base_size = 12,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22,
  grid = "major",
  axis = "both",
  ticks = "both"
)
```

## Arguments

- base_size:

  Numeric. Base font size (default: 12)

- base_family:

  Character. Base font family (default: "")

- base_line_size:

  Numeric. Base line size (default: base_size/22)

- base_rect_size:

  Numeric. Base rectangle line size (default: base_size/22)

- grid:

  Character. Which grid lines to show: "major", "minor", "both", "none"
  (default: "major")

- axis:

  Character. Which axis lines to show: "both", "x", "y", "none"
  (default: "both")

- ticks:

  Character. Which axis ticks to show: "both", "x", "y", "none"
  (default: "both")

## Value

A ggplot2 theme object

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Basic usage
ggplot(data, aes(x, y)) +
  geom_point() +
  theme_vecshift()

# Minimal grid
ggplot(data, aes(x, y)) +
  geom_line() +
  theme_vecshift(grid = "none")

# Larger text for presentations
ggplot(data, aes(x, y)) +
  geom_bar(stat = "identity") +
  theme_vecshift(base_size = 16)
} # }
```
