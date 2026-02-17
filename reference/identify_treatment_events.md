# Identify Treatment Groups for Impact Evaluation

Identifies people who experienced treatment events and collects their
complete employment histories for impact evaluation. Uses a
person-centered approach that compares entire career trajectories of
treated vs. control individuals.

## Usage

``` r
identify_treatment_events(
  data,
  treatment_conditions,
  event_window = c(-365, 365),
  min_pre_period = 90,
  min_post_period = 90,
  multiple_events = "first",
  require_employment_before = TRUE,
  id_column = "cf",
  date_column = "inizio",
  verbose = FALSE
)
```

## Arguments

- data:

  A data.table containing employment records (output from vecshift)

- treatment_conditions:

  List of conditions defining treatment events. Each condition can be:

  - A string expression (e.g., "COD_TIPOLOGIA_CONTRATTUALE ==
    'C.01.00'")

  - A named list with 'column', 'operator', and 'value'

  - A function that takes the data and returns a logical vector

- event_window:

  Numeric vector of length 2 defining the event window relative to the
  treatment date. Default: c(-365, 365) (1 year before/after)

- min_pre_period:

  Minimum number of days required before the event. Default: 90

- min_post_period:

  Minimum number of days required after the event. Default: 90

- multiple_events:

  How to handle multiple treatment events per person:

  - "first": Use only the first treatment event date

  - "last": Use only the last treatment event date

  - "all": Keep all treatment events (creates multiple treatment dates
    per person) Default: "first"

- require_employment_before:

  Logical. Require employment before the treatment event? Default: TRUE

- id_column:

  Character. Name of the person identifier column. Default: "cf"

- date_column:

  Character. Name of the date column to use for event timing. Default:
  "inizio"

- verbose:

  Logical. Print debugging information? Default: FALSE

## Value

A data.table with ALL employment events for both treated and control
people including:

- cf:

  Person identifier

- is_treated:

  Logical indicator: TRUE for people who experienced treatment, FALSE
  for controls

- treatment_event_date:

  Date of first/last treatment event (NA for control people)

- days_to_event:

  Days from each observation to treatment event (negative = before)

- in_event_window:

  Logical indicator for observations within event window

- pre_event_period:

  Logical indicator for pre-treatment observations

- post_event_period:

  Logical indicator for post-treatment observations

- treatment_condition_met:

  Description of which condition triggered treatment

- ...:

  All original employment data columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Identify people who got permanent contracts and collect their full careers
impact_data <- identify_treatment_events(
  data = employment_data,
  treatment_conditions = list("COD_TIPOLOGIA_CONTRATTUALE == 'C.01.00'"),
  event_window = c(-180, 365),
  multiple_events = "first"
)

# Result contains:
# - ALL employment events for people who got permanent contracts (is_treated=TRUE)
# - ALL employment events for people who never got permanent contracts (is_treated=FALSE)
# - treatment_event_date marks when treated people first got permanent contracts

# Multiple conditions example
impact_data <- identify_treatment_events(
  data = employment_data,
  treatment_conditions = list(
    list(column = "COD_TIPOLOGIA_CONTRATTUALE", operator = "==", value = "C.01.00"),
    list(column = "durata", operator = ">", value = 365)
  ),
  event_window = c(-365, 730)
)
} # }
```
