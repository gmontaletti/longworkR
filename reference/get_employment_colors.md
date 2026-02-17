# Get Employment-Specific Colors

Convenience function to get colors for specific employment statuses
based on vecshift output conventions.

## Usage

``` r
get_employment_colors(statuses = NULL, alpha = 1)
```

## Arguments

- statuses:

  Character vector. Employment statuses to get colors for

- alpha:

  Numeric. Transparency level (0-1)

## Value

Named character vector of hex colors

## Examples

``` r
# Get colors for common employment statuses
get_employment_colors(c("occ_ft", "occ_pt", "disoccupato"))
#>      occ_ft      occ_pt disoccupato 
#>   "#27AE60"   "#F39C12"   "#E74C3C" 

# Get all employment colors
get_employment_colors()
#> disoccupato      occ_ft      occ_pt  over_ft_ft  over_pt_pt  over_ft_pt 
#>   "#E74C3C"   "#27AE60"   "#F39C12"   "#9B59B6"   "#E67E22"   "#1ABC9C" 
#>  transition     unknown 
#>   "#3498DB"   "#95A5A6" 
```
