# RDD Placebo Tests and Density Tests

Performs placebo tests using alternative cutoffs and density tests to
check for manipulation of the running variable around the cutoff.

## Usage

``` r
rdd_placebo_tests(
  data,
  running_variable,
  cutoff,
  bandwidth,
  placebo_cutoffs = NULL,
  outcome,
  n_placebo_tests = 10,
  density_test_method = "mccrary",
  bin_size = NULL
)
```

## Arguments

- data:

  A data.table or data.frame containing the analysis dataset

- running_variable:

  Character string specifying the running variable name

- cutoff:

  Numeric value specifying the true cutoff point

- bandwidth:

  Numeric value specifying the bandwidth to use

- placebo_cutoffs:

  Numeric vector of placebo cutoffs to test (default: quantiles around
  main cutoff)

- outcome:

  Character string specifying the outcome variable for placebo tests

- n_placebo_tests:

  Integer specifying number of placebo cutoffs to test (default: 10)

- density_test_method:

  Character string: "mccrary" for McCrary (2008) test or "cattaneo" for
  Cattaneo et al. (2018)

- bin_size:

  Numeric value for density test bin size (auto-selected if NULL)

## Value

A list with the following components:

- placebo_results: Results from placebo cutoff tests

- density_test: Results from density discontinuity test

- placebo_plot_data: Data for plotting placebo test results

- density_plot_data: Data for plotting density around cutoff

## Details

This function performs two key validity tests for RDD:

1.  Placebo Tests: Estimate treatment effects at alternative cutoff
    points where no true discontinuity should exist. Significant effects
    suggest specification problems or confounding factors.

2.  Density Tests: Test whether the density of the running variable is
    continuous at the cutoff. A discontinuity suggests manipulation of
    the assignment variable, violating RDD assumptions.

The McCrary (2008) test uses local linear regression of log densities.
The Cattaneo et al. (2018) test provides more robust inference with bias
correction and improved confidence intervals.

## References

McCrary, J. (2008). Manipulation of the running variable in the
regression discontinuity design: A density test. Journal of
Econometrics, 142(2), 698-714.

Cattaneo, M. D., Jansson, M., & Ma, X. (2018). Manipulation testing
based on density discontinuity. The Stata Journal, 18(1), 234-261.

## Examples

``` r
if (FALSE) { # \dontrun{
# Placebo and density tests
placebo_results <- rdd_placebo_tests(
  data = employment_data,
  running_variable = "age",
  outcome = "employment_rate",
  cutoff = 65,
  bandwidth = 5
)

# Check for significant placebo effects
print(placebo_results$placebo_results[p_value < 0.05])

# Check density test
print(placebo_results$density_test)
} # }
```
