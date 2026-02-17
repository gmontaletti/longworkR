# Synthetic Control Method Wrapper

Wrapper function for synthetic control estimation using the augsynth
package for cases where traditional matching is not feasible.

## Usage

``` r
synthetic_control_method(
  data,
  outcome_var,
  unit_var,
  time_var,
  treatment_var,
  treatment_time,
  covariates = NULL,
  method = "augsynth",
  inference = TRUE,
  n_lags = 0
)
```

## Arguments

- data:

  A data.table containing panel data

- outcome_var:

  Character. Outcome variable name

- unit_var:

  Character. Unit identifier variable

- time_var:

  Character. Time variable

- treatment_var:

  Character. Treatment indicator

- treatment_time:

  Integer. Time period when treatment begins

- covariates:

  Character vector. Time-varying covariates. Default: NULL

- method:

  Character. Augsynth method: "augsynth", "synth", "ridge". Default:
  "augsynth"

- inference:

  Logical. Perform inference using conformal method? Default: TRUE

- n_lags:

  Integer. Number of outcome lags to include. Default: 0

## Value

A list containing:

- synth_estimates:

  Synthetic control estimates

- unit_weights:

  Synthetic control unit weights

- balance_table:

  Pre-treatment balance

- inference_results:

  Inference results if requested

- plot_data:

  Data for synthetic control plots

## Examples

``` r
if (FALSE) { # \dontrun{
synth_results <- synthetic_control_method(
  data = panel_data,
  outcome_var = "employment_rate",
  unit_var = "region_id",
  time_var = "year",
  treatment_time = 2015,
  method = "augsynth"
)
} # }
```
