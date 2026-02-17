# Preview Vecshift Color Palette

Creates a visual preview of a vecshift color palette to help with
selection and accessibility testing.

## Usage

``` r
preview_vecshift_colors(
  palette = "main",
  n = NULL,
  show_hex = TRUE,
  show_names = TRUE
)
```

## Arguments

- palette:

  Character. Palette name to preview

- n:

  Integer. Number of colors to show (default: all available)

- show_hex:

  Logical. Whether to show hex codes (default: TRUE)

- show_names:

  Logical. Whether to show color names if available (default: TRUE)

## Value

A ggplot2 object showing the color palette

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview main palette
preview_vecshift_colors()

# Preview employment colors with names
preview_vecshift_colors("employment")

# Preview 5 colors from main palette
preview_vecshift_colors("main", n = 5)
} # }
```
