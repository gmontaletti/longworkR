# Extend Color Palette for Large Numbers of Categories

Internal helper to extend color palettes when more colors are needed
than available in the base palette. Uses intelligent color interpolation
and generation strategies.

## Usage

``` r
.extend_color_palette(base_colors, n, accessibility_mode = FALSE)
```

## Arguments

- base_colors:

  Character vector of base colors to extend

- n:

  Integer. Total number of colors needed

- accessibility_mode:

  Logical. Whether to prioritize accessibility

## Value

Character vector of n colors
