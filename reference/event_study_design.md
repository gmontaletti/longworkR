# Event Study Design

Performs event study analysis to examine treatment effects over time
around the treatment event with flexible lead and lag specifications.

## Usage

``` r
event_study_design(
  data,
  outcome_vars,
  time_to_event_var = "days_to_event",
  treatment_var = "is_treated",
  id_var = "cf",
  control_vars = NULL,
  event_window = c(-12, 12),
  time_unit = "months",
  omit_period = -1,
  cluster_var = NULL
)
```

## Arguments

- data:

  A data.table containing event data with time-to-event information

- outcome_vars:

  Character vector. Outcome variables to analyze

- time_to_event_var:

  Character. Variable containing time to event. Default: "days_to_event"

- treatment_var:

  Character. Treatment indicator. Default: "is_treated"

- id_var:

  Character. Individual identifier. Default: "cf"

- control_vars:

  Character vector. Control variables. Default: NULL

- event_window:

  Numeric vector. Event window c(pre_periods, post_periods). Default:
  c(-12, 12)

- time_unit:

  Character. Time unit for binning: "days", "weeks", "months". Default:
  "months"

- omit_period:

  Integer. Reference period to omit (relative to event). Default: -1

- cluster_var:

  Character. Clustering variable. Default: NULL

## Value

A list containing:

- event_estimates:

  Event study coefficients by time period

- model_results:

  Full regression results

- pre_test:

  Pre-treatment test results

- dynamic_effects:

  Summary of dynamic treatment effects

- plot_data:

  Data formatted for event study plots

## Examples

``` r
if (FALSE) { # \dontrun{
event_study_results <- event_study_design(
  data = event_data,
  outcome_vars = "employment_rate",
  event_window = c(-6, 12),
  time_unit = "months"
)
} # }
```
