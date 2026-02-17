# Create Treatment and Control Groups

Creates matched treatment and control groups for impact evaluation based
on event identification results.

## Usage

``` r
create_treatment_control_groups(
  event_data,
  control_conditions = NULL,
  matching_variables = NULL,
  exact_match_variables = NULL,
  control_ratio = 1,
  caliper = 0.1,
  exclude_future_treated = TRUE,
  replacement = FALSE
)
```

## Arguments

- event_data:

  Data.table from identify_treatment_events()

- control_conditions:

  Optional conditions for control group eligibility. Similar format to
  treatment_conditions in identify_treatment_events()

- matching_variables:

  Character vector of variables to use for matching. Default: NULL (no
  matching, uses random eligible controls). Note: If provided, this
  serves as a reminder but actual matching should be done using
  propensity_score_matching() after group creation

- exact_match_variables:

  Character vector of variables requiring exact matches. Default: NULL

- control_ratio:

  Numeric. Ratio of control to treatment units. Default: 1

- caliper:

  Numeric. Maximum distance for propensity score matching. Default: 0.1

- exclude_future_treated:

  Logical. Exclude future-treated units from control group? Default:
  TRUE

- replacement:

  Logical. Sample control units with replacement? Default: FALSE

## Value

A data.table with treatment and control group assignments including:

- group_assignment:

  Factor: "treatment" or "control"

- match_id:

  Matching pair identifier

- propensity_score:

  Estimated propensity score (if matching used)

- match_distance:

  Distance to matched unit

- control_weight:

  Weight for control observations

## Examples

``` r
if (FALSE) { # \dontrun{
# Create treatment and control groups
groups <- create_treatment_control_groups(
  event_data = identified_events,
  matching_variables = c("age", "sector", "region"),
  exact_match_variables = c("gender"),
  control_ratio = 2
)
} # }
```
