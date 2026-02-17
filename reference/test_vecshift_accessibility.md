# Test Color Accessibility

Tests a vecshift color palette for accessibility issues including
colorblind accessibility and contrast ratios.

## Usage

``` r
test_vecshift_accessibility(palette = "main", n = NULL, return_data = FALSE)
```

## Arguments

- palette:

  Character. Palette name to test

- n:

  Integer. Number of colors to test (default: all available)

- return_data:

  Logical. Return test data instead of summary (default: FALSE)

## Value

Either a summary message or detailed test data

## Examples

``` r
if (FALSE) { # \dontrun{
# Test main palette accessibility
test_vecshift_accessibility()

# Test employment palette
test_vecshift_accessibility("employment")
} # }
```
