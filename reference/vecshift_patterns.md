# Get Vecshift Pattern Specifications

Returns pattern specifications for bar charts and area plots to improve
visual distinction in black and white or colorblind-accessible
visualizations. Patterns can be used with the ggpattern package or as
fallback specifications.

## Usage

``` r
vecshift_patterns(type = "employment", n = NULL, reverse = FALSE)
```

## Arguments

- type:

  Character. Type of patterns to return:

  - "employment": Patterns for employment status data

  - "contracts": Patterns for contract types

  - "basic": Basic pattern set for general use

  - "dense": High-density patterns for small areas

- n:

  Integer. Number of patterns to return (default: all available)

- reverse:

  Logical. Whether to reverse the pattern order (default: FALSE)

## Value

A named list of pattern specifications with the following elements:

- pattern: Pattern type (e.g., "stripe", "crosshatch", "circle")

- angle: Angle for directional patterns (degrees)

- density: Pattern density (0-1 scale)

- spacing: Spacing between pattern elements

## See also

[`scale_fill_vecshift_bw`](https://gmontaletti.github.io/longworkR/reference/scale_fill_vecshift_bw.md),
[`vecshift_colors`](https://gmontaletti.github.io/longworkR/reference/vecshift_colors.md)

## Examples

``` r
# Get employment patterns
emp_patterns <- vecshift_patterns("employment")
print(emp_patterns)
#> $disoccupato
#> $disoccupato$pattern
#> [1] "stripe"
#> 
#> $disoccupato$angle
#> [1] 45
#> 
#> $disoccupato$density
#> [1] 0.6
#> 
#> $disoccupato$spacing
#> [1] 0.02
#> 
#> $disoccupato$description
#> [1] "Diagonal lines for unemployment"
#> 
#> 
#> $occ_ft
#> $occ_ft$pattern
#> [1] "none"
#> 
#> $occ_ft$angle
#> [1] 0
#> 
#> $occ_ft$density
#> [1] 1
#> 
#> $occ_ft$spacing
#> [1] 0
#> 
#> $occ_ft$description
#> [1] "Solid fill for full-time employment"
#> 
#> 
#> $occ_pt
#> $occ_pt$pattern
#> [1] "circle"
#> 
#> $occ_pt$angle
#> [1] 0
#> 
#> $occ_pt$density
#> [1] 0.4
#> 
#> $occ_pt$spacing
#> [1] 0.03
#> 
#> $occ_pt$description
#> [1] "Dots for part-time employment"
#> 
#> 
#> $over_ft_ft
#> $over_ft_ft$pattern
#> [1] "crosshatch"
#> 
#> $over_ft_ft$angle
#> [1] 45
#> 
#> $over_ft_ft$density
#> [1] 0.5
#> 
#> $over_ft_ft$spacing
#> [1] 0.02
#> 
#> $over_ft_ft$description
#> [1] "Crosshatch for FT overlaps"
#> 
#> 
#> $over_pt_pt
#> $over_pt_pt$pattern
#> [1] "stripe"
#> 
#> $over_pt_pt$angle
#> [1] 135
#> 
#> $over_pt_pt$density
#> [1] 0.5
#> 
#> $over_pt_pt$spacing
#> [1] 0.025
#> 
#> $over_pt_pt$description
#> [1] "Reverse diagonal for PT overlaps"
#> 
#> 
#> $over_ft_pt
#> $over_ft_pt$pattern
#> [1] "weave"
#> 
#> $over_ft_pt$angle
#> [1] 0
#> 
#> $over_ft_pt$density
#> [1] 0.4
#> 
#> $over_ft_pt$spacing
#> [1] 0.02
#> 
#> $over_ft_pt$description
#> [1] "Weave pattern for mixed overlaps"
#> 
#> 
#> $transition
#> $transition$pattern
#> [1] "stripe"
#> 
#> $transition$angle
#> [1] 0
#> 
#> $transition$density
#> [1] 0.3
#> 
#> $transition$spacing
#> [1] 0.04
#> 
#> $transition$description
#> [1] "Horizontal lines for transitions"
#> 
#> 

# Get basic patterns
basic_patterns <- vecshift_patterns("basic", n = 4)
```
