# Coarsened Exact Matching (CEM) with Person-Level Matching

Performs coarsened exact matching to create balanced treatment and
control groups by automatically binning continuous variables and exactly
matching on specified factors. This function first aggregates
event-level data to person-level characteristics, performs matching on
people, then returns all events for matched individuals. This
implementation provides both native CEM (when available) and a fallback
implementation that doesn't require X11/tcltk dependencies.

## Usage

``` r
coarsened_exact_matching(
  data,
  treatment_var = "is_treated",
  person_id_var = "cf",
  matching_variables,
  person_aggregation = "first",
  automatic_binning = TRUE,
  cutpoints = NULL,
  k2k = FALSE,
  control_ratio = 1,
  keep_all = TRUE,
  use_native_cem = TRUE,
  n_bins = 4,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.table containing treatment and control observations (can be
  event-level)

- treatment_var:

  Character. Name of treatment indicator variable. Default: "is_treated"

- person_id_var:

  Character. Name of person identifier variable (e.g., "cf"). Default:
  "cf"

- matching_variables:

  Character vector. Variables to include in matching

- person_aggregation:

  Character. How to aggregate person characteristics: "first", "last",
  "mode", "mean". Default: "first"

- automatic_binning:

  Logical. Automatically bin continuous variables? Default: TRUE

- cutpoints:

  Named list. Custom cutpoints for continuous variables. Default: NULL

- k2k:

  Logical. Perform k-to-k matching (1:1 ratio)? Default: FALSE

- control_ratio:

  Numeric. Ratio of control to treatment units (e.g., 2 for 2:1).
  Default: 1

- keep_all:

  Logical. Keep all matched observations? Default: TRUE

- use_native_cem:

  Logical. Try to use native CEM package? Default: TRUE

- n_bins:

  Integer. Number of bins for automatic binning. Default: 4

- verbose:

  Logical. Print detailed information? Default: TRUE

## Value

A list containing:

- matched_data:

  Data.table with ALL events for matched individuals (both treated and
  control). Includes event_time column with values: "pre"
  (pre-treatment), "post" (post-treatment), "control" (control group),
  or NA

- matched_persons:

  Data.table with person-level characteristics used for matching

- match_summary:

  Summary of CEM procedure

- imbalance_measures:

  L1 and other imbalance statistics

- strata_info:

  Information about matching strata

- aggregation_report:

  Report on person-level aggregation process

- implementation_used:

  Which implementation was used: "native_cem" or "fallback"

## Examples

``` r
if (FALSE) { # \dontrun{
# Process employment data first
employment_data <- vecshift(raw_employment_data)

# Add treatment indicator (e.g., policy intervention)
employment_data[, is_treated := some_treatment_condition]

# Basic CEM with person-level matching
cem_match <- coarsened_exact_matching(
  data = employment_data,
  person_id_var = "cf",
  matching_variables = c("age", "education", "sector"),
  person_aggregation = "first",
  automatic_binning = TRUE
)

# Result contains all events for matched persons
matched_employment_data <- cem_match$matched_data
person_characteristics <- cem_match$matched_persons

# CEM with 2:1 control-to-treatment ratio
cem_match_2to1 <- coarsened_exact_matching(
  data = employment_data,
  person_id_var = "cf",
  matching_variables = c("age", "wage", "experience"),
  control_ratio = 2,  # 2 controls per treated unit
  cutpoints = list(
    age = c(25, 35, 45, 55),
    wage = c(1000, 2000, 3000, 4000)
  )
)

# Force use of fallback implementation with custom ratio
cem_match_fallback <- coarsened_exact_matching(
  data = employment_data,
  person_id_var = "cf",
  matching_variables = c("age", "education", "sector"),
  control_ratio = 2,
  use_native_cem = FALSE
)
} # }
```
