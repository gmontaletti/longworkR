# Calculate Weighted Median Using Optimized Algorithm

Efficiently calculates weighted median using
[`matrixStats::weightedMedian()`](https://rdrr.io/pkg/matrixStats/man/weightedMedian.html)
instead of the memory-intensive
[`rep()`](https://rdrr.io/r/base/rep.html) approach. This optimization
dramatically reduces memory usage and computation time for large
datasets with high-weight observations.

## Usage

``` r
.calculate_weighted_median_optimized(values, weights, na.rm = TRUE)
```

## Arguments

- values:

  Numeric vector of values

- weights:

  Numeric vector of weights (same length as values)

- na.rm:

  Logical; if TRUE, remove NA values before computation (default: TRUE)

## Value

Numeric weighted median value, or `NA_real_` if input is empty or
invalid

## Details

**Performance Characteristics:**

Benchmark results show substantial improvements over the rep()-based
approach:

- **Speed**: 31-59x faster (0.06ms vs 1.8-3.6ms for 1000 observations)

- **Memory**: 96-99\\

- **Scalability**: O(n log n) complexity independent of weight magnitude

The old approach using `rep(values, times = weights)` creates massive
temporary vectors. For example, with 1,000 observations and average
weight of 365 days (typical employment duration):

- Replicated vector: 365,000 elements

- Memory allocation: 2.92 MB per calculation

- Two calculations per transition (from + to): 5.84 MB

- 100 unique transitions: 584 MB temporary memory

The optimized
[`matrixStats::weightedMedian()`](https://rdrr.io/pkg/matrixStats/man/weightedMedian.html)
approach:

- Works directly with original vectors (no replication)

- Memory: O(n) instead of O(n × w) where w = average weight

- Numerically stable for arbitrarily large weights

- Handles weighted median computation using efficient sorting algorithm

**Edge Case Handling:**

- Empty vectors: Returns `NA_real_`

- All NA values: Returns `NA_real_`

- Zero weights: Returns `NA_real_`

- Mismatched lengths: Stops with error

- Negative weights: Automatically converted to absolute values by
  matrixStats

## Optimization

This function replaces memory-intensive vector replication with direct
weighted median calculation. The optimization is most beneficial when:

- Weights are large (e.g., employment durations in days)

- Many weighted medians need to be calculated (e.g., for each
  transition)

- Working with memory-constrained environments

## See also

[`weightedMedian`](https://rdrr.io/pkg/matrixStats/man/weightedMedian.html)
for the underlying implementation

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate weighted median of salaries by employment duration
salaries <- c(30000, 35000, 40000, 45000, 50000)
durations <- c(365, 730, 180, 90, 1095)  # days employed

# Old approach (memory-intensive):
# median(rep(salaries, times = durations))

# Optimized approach:
.calculate_weighted_median_optimized(salaries, durations)

# With NA handling:
salaries_na <- c(30000, NA, 40000, 45000, 50000)
.calculate_weighted_median_optimized(salaries_na, durations, na.rm = TRUE)
} # }
```
