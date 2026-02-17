# Get Required Number of Colors for Node Color Variable

Internal helper to determine how many colors are needed for discrete
color variables. Returns appropriate count for continuous variables or
unique value count for discrete.

## Usage

``` r
.get_required_colors_count(tg, color_var)
```

## Arguments

- tg:

  tidygraph object with node data

- color_var:

  Character. Color variable name

## Value

Integer. Number of colors needed
