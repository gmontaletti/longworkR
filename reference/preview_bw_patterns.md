# Preview Pattern and Color Combinations

Creates a visual preview of pattern and color combinations to help with
selection for black and white visualizations.

## Usage

``` r
preview_bw_patterns(
  pattern_type = "employment",
  show_patterns = TRUE,
  show_colors = FALSE
)
```

## Arguments

- pattern_type:

  Character. Type of patterns to preview

- show_patterns:

  Logical. Whether to show pattern descriptions (default: TRUE)

- show_colors:

  Logical. Whether to show hex color codes (default: FALSE)

## Value

A ggplot2 object showing pattern and color combinations

## See also

[`vecshift_patterns`](https://gmontaletti.github.io/longworkR/reference/vecshift_patterns.md),
[`preview_vecshift_colors`](https://gmontaletti.github.io/longworkR/reference/preview_vecshift_colors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview employment patterns
preview_bw_patterns("employment")

# Preview contract patterns
preview_bw_patterns("contracts")
} # }
```
