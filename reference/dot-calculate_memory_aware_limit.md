# Calculate Memory-Aware Sample Size Limit (Internal)

Determines the maximum sample size for distance matrix calculations
based on available system memory. This prevents memory exhaustion when
computing silhouette scores or other metrics requiring O(n²) distance
matrices.

## Usage

``` r
.calculate_memory_aware_limit(
  memory_fraction = 0.33,
  p = 10,
  method = c("clustering", "silhouette"),
  verbose = FALSE
)
```

## Arguments

- memory_fraction:

  Numeric. Fraction of available RAM to use (default 0.33 = 33%)

- p:

  Integer. Number of features (for overhead calculation)

- method:

  Character. Purpose: "clustering" (default) or "silhouette" (stricter
  limits)

- verbose:

  Logical. Print memory info?

## Value

Integer. Maximum safe sample size for distance matrix calculations
