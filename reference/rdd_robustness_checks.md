# RDD Robustness Checks

Performs robustness checks for regression discontinuity design by
varying key specification choices including bandwidth, polynomial order,
and kernel.

## Usage

``` r
rdd_robustness_checks(
  rdd_results,
  data,
  bandwidth_multipliers = c(0.5, 0.75, 1.25, 1.5, 2),
  polynomial_orders = c(1, 2, 3),
  kernels = c("triangular", "rectangular", "epanechnikov"),
  include_bias_correction = TRUE,
  placebo_cutoffs = c(-0.5, 0.5)
)
```

## Arguments

- rdd_results:

  Object of class "rdd_results" from regression_discontinuity()

- data:

  Original data used in RDD estimation

- bandwidth_multipliers:

  Numeric vector of multipliers for original bandwidth (default: c(0.5,
  0.75, 1.25, 1.5, 2.0))

- polynomial_orders:

  Integer vector of polynomial orders to test (default: c(1, 2, 3))

- kernels:

  Character vector of kernel functions to test (default: c("triangular",
  "rectangular", "epanechnikov"))

- include_bias_correction:

  Logical indicating whether to test with/without bias correction
  (default: TRUE)

- placebo_cutoffs:

  Numeric vector of placebo cutoffs relative to true cutoff (default:
  c(-0.5, 0.5) in bandwidth units)

## Value

A list with the following components:

- bandwidth_robustness: Results varying bandwidth

- polynomial_robustness: Results varying polynomial order

- kernel_robustness: Results varying kernel

- bias_correction_robustness: Results with/without bias correction

- placebo_tests: Results for placebo cutoffs

- summary_table: Summary table of all robustness checks

## Details

This function systematically varies key RDD specification choices to
assess the robustness of the main results. It tests:

- Different bandwidth choices (multiples of optimal bandwidth)

- Different polynomial orders (local linear vs. quadratic vs. cubic)

- Different kernel functions (triangular, rectangular, Epanechnikov)

- Bias correction on/off

- Placebo cutoffs where no effect should be present

## Examples

``` r
if (FALSE) { # \dontrun{
# Run main RDD
rdd_results <- regression_discontinuity(
  data = employment_data,
  outcome = "employment_rate",
  running_variable = "age",
  cutoff = 65
)

# Robustness checks
robustness <- rdd_robustness_checks(
  rdd_results = rdd_results,
  data = employment_data,
  bandwidth_multipliers = c(0.5, 1.0, 1.5, 2.0)
)
} # }
```
