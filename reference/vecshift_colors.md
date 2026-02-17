# Get Vecshift Color Palette

Returns color palettes specifically designed for employment data
visualization with accessibility as a primary concern. All palettes are
tested for colorblind accessibility and provide sufficient contrast
ratios.

## Usage

``` r
vecshift_colors(palette = "main", n = NULL, reverse = FALSE, alpha = 1)
```

## Arguments

- palette:

  Character. Type of palette to return:

  - "main": Primary dark-toned palette (default)

  - "desaturated": Desaturated version for subtle distinctions

  - "bw": Black and white version for printing

  - "employment": Specialized colors for employment status

  - "transitions": Colors for temporal transitions

  - "contracts": Colors for contract types

- n:

  Integer. Number of colors to return (default: all available)

- reverse:

  Logical. Whether to reverse the color order (default: FALSE)

- alpha:

  Numeric. Transparency level between 0 and 1 (default: 1)

## Value

A character vector of hex color codes

## See also

[`scale_color_vecshift`](https://gmontaletti.github.io/longworkR/reference/scale_color_vecshift.md),
[`theme_vecshift`](https://gmontaletti.github.io/longworkR/reference/theme_vecshift.md)

## Examples

``` r
# Get main dark-toned palette
vecshift_colors()
#>  [1] "#2C3E50" "#E74C3C" "#3498DB" "#F39C12" "#27AE60" "#9B59B6" "#34495E"
#>  [8] "#E67E22" "#1ABC9C" "#95A5A6"

# Get 5 desaturated colors
vecshift_colors("desaturated", n = 5)
#> [1] "#5D6D7E" "#CD6155" "#5DADE2" "#F7DC6F" "#58D68D"

# Get black and white palette
vecshift_colors("bw")
#>  [1] "#2C3E50" "#5D6D7E" "#7F8C8D" "#95A5A6" "#BDC3C7" "#D5DBDB" "#E8EAED"
#>  [8] "#F4F6F7" "#FAFBFC" "#FFFFFF"

# Employment-specific colors
vecshift_colors("employment")
#> disoccupato      occ_ft      occ_pt  over_ft_ft  over_pt_pt  over_ft_pt 
#>   "#E74C3C"   "#27AE60"   "#F39C12"   "#9B59B6"   "#E67E22"   "#1ABC9C" 
#>  transition     unknown 
#>   "#3498DB"   "#95A5A6" 
```
