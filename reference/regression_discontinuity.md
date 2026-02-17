# Main Regression Discontinuity Design Estimation

Performs regression discontinuity design estimation with automatic
bandwidth selection and supports both sharp and fuzzy RDD designs.
Includes bias-corrected estimates and robust inference procedures.

## Usage

``` r
regression_discontinuity(
  data,
  outcome,
  running_variable,
  cutoff = 0,
  treatment_variable = NULL,
  design = "sharp",
  bandwidth_method = "mserd",
  bandwidth_value = NULL,
  kernel = "triangular",
  polynomial_order = 1,
  bias_correction = TRUE,
  robust_inference = TRUE,
  cluster_variable = NULL,
  covariates = NULL,
  alpha = 0.05
)
```

## Arguments

- data:

  A data.table or data.frame containing the analysis dataset

- outcome:

  Character string specifying the outcome variable name

- running_variable:

  Character string specifying the running variable name

- cutoff:

  Numeric value specifying the cutoff point (default: 0)

- treatment_variable:

  Character string specifying treatment variable name (for fuzzy RDD)

- design:

  Character string: "sharp" or "fuzzy" RDD design (default: "sharp")

- bandwidth_method:

  Character string: "mserd" (mean squared error), "cerrd" (coverage
  error), or "manual"

- bandwidth_value:

  Numeric value for manual bandwidth selection (ignored if
  bandwidth_method != "manual")

- kernel:

  Character string: "triangular", "rectangular", or "epanechnikov"
  (default: "triangular")

- polynomial_order:

  Integer specifying polynomial order (default: 1, recommended: 1-2)

- bias_correction:

  Logical indicating whether to apply bias correction (default: TRUE)

- robust_inference:

  Logical indicating whether to use robust standard errors (default:
  TRUE)

- cluster_variable:

  Character string specifying cluster variable for clustered standard
  errors

- covariates:

  Character vector of covariate names to include (optional)

- alpha:

  Numeric value for confidence level (default: 0.05 for 95\\ CI)

## Value

A list with the following components:

- estimates: Data.table with treatment effect estimates

- bandwidth: Selected or specified bandwidth

- sample_sizes: Sample sizes within bandwidth

- first_stage: First stage results (fuzzy RDD only)

- diagnostics: Model diagnostics and fit statistics

- method_details: Details of estimation method and parameters

## Details

The function implements local polynomial regression around the cutoff
with optimal bandwidth selection following Calonico et al. (2014). For
fuzzy RDD, it uses two-stage least squares with the assignment rule as
instrument.

Bandwidth selection methods:

- "mserd": Minimizes mean squared error of the RDD estimator

- "cerrd": Minimizes coverage error of confidence intervals

- "manual": Uses user-specified bandwidth

The function provides bias-corrected estimates using higher-order
polynomials following Calonico et al. (2014) robust bias-correction
procedure.

## References

Calonico, S., Cattaneo, M. D., & Titiunik, R. (2014). Robust
nonparametric confidence intervals for regression-discontinuity designs.
Econometrica, 82(6), 2295-2326.

Imbens, G. W., & Lemieux, T. (2008). Regression discontinuity designs: A
guide to practice. Journal of Econometrics, 142(2), 615-635.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sharp RDD
rdd_results <- regression_discontinuity(
  data = employment_data,
  outcome = "employment_rate",
  running_variable = "age",
  cutoff = 65,
  design = "sharp"
)

# Fuzzy RDD with covariates
fuzzy_results <- regression_discontinuity(
  data = employment_data,
  outcome = "earnings",
  running_variable = "test_score",
  treatment_variable = "program_participation",
  cutoff = 70,
  design = "fuzzy",
  covariates = c("age", "gender", "education")
)
} # }
```
