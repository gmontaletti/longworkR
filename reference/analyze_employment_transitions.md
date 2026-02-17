# Analyze Employment Transitions from Pipeline Output

Analyzes employment transitions from the output of
process_employment_pipeline(). Identifies transitions between employment
periods that are separated by unemployment periods and provides
transition pattern analysis. The function allows specification of which
variable to use for transition analysis and which variables to compute
statistics for. Supports optional inline consolidation via the
`consolidation_mode` parameter, or users can pre-consolidate data using
the composable consolidation functions
([`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md),
[`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md),
[`consolidate_short_gaps()`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)).

## Usage

``` r
analyze_employment_transitions(
  pipeline_result,
  transition_variable = NULL,
  statistics_variables = NULL,
  min_unemployment_duration = 1,
  max_unemployment_duration = NULL,
  consolidation_mode = "none",
  consolidation_type = "both",
  employer_var = NULL,
  min_lag = 8,
  output_transition_matrix = FALSE,
  eval_chain = "last",
  show_progress = TRUE
)
```

## Arguments

- pipeline_result:

  Output from process_employment_pipeline(). Must be a data.table with
  columns: cf (person identifier), arco (employment overlap count),
  inizio/fine (period dates), durata (period duration), and optionally
  over_id (overlap identifier, required when consolidation_mode is
  "temporal" or "both").

- transition_variable:

  Character string specifying the variable to use for transition
  analysis (from/to values). If NULL (default), uses the first
  non-standard attribute in the data.table.

- statistics_variables:

  Character vector specifying variables to compute summary statistics
  for. If NULL (default), uses all non-standard attributes except the
  transition variable.

- min_unemployment_duration:

  Minimum duration (in days) of unemployment period to consider a
  transition (default: 1).

- max_unemployment_duration:

  Maximum duration (in days) of unemployment period to consider a
  transition. If NULL (default), no upper limit is applied. When not
  NULL, only transitions with unemployment duration between
  min_unemployment_duration and max_unemployment_duration (inclusive)
  are included.

- consolidation_mode:

  Character string specifying the consolidation strategy to apply before
  transition analysis (default: "none"). Options:

  - `"none"`: No consolidation (default, backward-compatible)

  - `"temporal"`: Consolidate overlapping periods using over_id

  - `"employer"`: Consolidate consecutive contracts from the same
    employer

  - `"both"`: Apply employer consolidation first, then temporal

- consolidation_type:

  Character string specifying the temporal consolidation approach
  (default: "both"). Only used when `consolidation_mode` includes
  temporal consolidation. Options: "both", "overlapping",
  "employment_only".

- employer_var:

  Character string specifying the column name containing employer
  identifiers. Required when `consolidation_mode` is "employer" or
  "both". Ignored for other modes (with a warning if provided).

- min_lag:

  Numeric value specifying the maximum gap in days between consecutive
  contracts from the same employer to be consolidated (default: 8). Only
  used with employer-based consolidation modes.

- output_transition_matrix:

  Logical. If TRUE, returns a square transition matrix instead of the
  normal aggregated data.table. Rows represent "from" states, columns
  represent "to" states, and values are transition weights (counts).
  Non-populated cells contain zeros. Matrix uses unique values from the
  transition_variable as row/column names (default: FALSE).

- eval_chain:

  Character string specifying how to handle chained values in from/to
  columns that contain "-\>" separators (default: "last"). Options:

  - `"last"`: Extract the last value from chains like
    "val1-\>val2-\>val3" (returns "val3")

  - `"first"`: Extract the first value from chains (returns "val1")

  - `"none"`: Leave chain values unchanged (returns
    "val1-\>val2-\>val3")

  When there is only one value (no "-\>"), the original value is always
  used regardless of this parameter.

- show_progress:

  Logical. If TRUE (default), displays a progress bar showing the
  current processing step, percentage completion, and estimated time
  remaining. Uses the 'progress' package if available, falls back to
  utils::txtProgressBar or simple messages if not available.

## Value

When `output_transition_matrix` is FALSE (default), returns a data.table
with columns:

- `from`: Value before transition (from transition_variable, processed
  according to eval_chain parameter)

- `to`: Value after transition (from transition_variable, processed
  according to eval_chain parameter)

- `weight`: Number of transitions

- `transition_duration`: Mean unemployment duration

- `[variable]_from_median/[variable]_from_mode`: For each statistics
  variable, duration-weighted aggregated values from the "from" period
  (median for numeric, mode for character)

- `[variable]_to_median/[variable]_to_mode`: For each statistics
  variable, duration-weighted aggregated values from the "to" period
  (median for numeric, mode for character)

When `output_transition_matrix` is TRUE, returns a square matrix where
rows represent "from" states, columns represent "to" states, and values
are transition weights (counts). Non-populated cells contain zeros.

Note: If the transition variable is also in statistics_variables, it
will have corresponding transition_variable_from_median and
transition_variable_to_median columns.

## Details

A transition occurs when there are consecutive employment periods (arco
\>= 1) separated by an unemployment period (arco = 0) of at least the
minimum duration. The function analyzes patterns in the "from" → "to"
transitions for one specified transition variable, while computing
summary statistics for additional variables.

The `eval_chain` parameter provides flexible handling of chained values
that contain "-\>" separators, allowing extraction of the first value,
last value, or preservation of the complete chain for complex transition
analysis scenarios.

For each transition, the function provides:

- **from**: Value in the employment period before unemployment
  (transition variable)

- **to**: Value in the employment period after unemployment (transition
  variable)

- **weight**: Number of transitions (.N)

- **transition_duration**: Mean duration of intermediate unemployment
  periods

- **For numeric statistics variables**: variable_from_median,
  variable_to_median

- **For character statistics variables**: variable_from_mode,
  variable_to_mode

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample employment data with transitions
employment_data <- data.table(
  id = 1:6,
  cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
  INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01",
                     "2023-02-01", "2023-06-01", "2023-10-01")),
  FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31",
                   "2023-04-30", "2023-08-31", "2023-12-31")),
  prior = c(1, 0, 1, 1, 1, 0),
  company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
  salary = c(50000, 25000, 60000, 55000, 65000, 30000)
)

# Process through pipeline
result <- process_employment_pipeline(
  original_data = employment_data,
  merge_columns = c("company", "salary")
)

# Analyze company transitions with temporal consolidation
transitions <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "company",
  statistics_variables = c("salary"),
  consolidation_mode = "temporal",
  consolidation_type = "both"
)

# Compare with no consolidation
transitions_original <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "company",
  statistics_variables = c("salary"),
  consolidation_mode = "none"
)

# Employer-based consolidation
transitions_employer <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",
  consolidation_mode = "employer",
  employer_var = "employer_id",
  min_lag = 8
)

# Sequential consolidation (both methods)
transitions_both <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",
  consolidation_mode = "both",
  employer_var = "employer_id",
  min_lag = 8,
  consolidation_type = "both"
)

# Analyze salary transitions with company statistics and minimum unemployment duration
transitions_salary <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "salary",
  statistics_variables = c("company"),
  min_unemployment_duration = 7,
  show_progress = FALSE
)

# Analyze transitions with duration constraints (unemployment between 7-30 days)
transitions_constrained <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "company",
  min_unemployment_duration = 7,
  max_unemployment_duration = 30
)

# Get transition matrix instead of data.table
transition_matrix <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "company",
  output_transition_matrix = TRUE
)
print(transition_matrix)

# Example using eval_chain parameter with chained values
# Suppose your data has chained company transitions like "CompanyA->CompanyB->CompanyC"
sample_data_chains <- copy(result)
sample_data_chains[, company := ifelse(company == "CompanyA", "StartupA->CompanyA->MegaCorp", company)]

# Extract last company in chain (default behavior)
transitions_last <- analyze_employment_transitions(
  pipeline_result = sample_data_chains,
  transition_variable = "company",
  eval_chain = "last"  # "StartupA->CompanyA->MegaCorp" becomes "MegaCorp"
)

# Extract first company in chain
transitions_first <- analyze_employment_transitions(
  pipeline_result = sample_data_chains,
  transition_variable = "company",
  eval_chain = "first"  # "StartupA->CompanyA->MegaCorp" becomes "StartupA"
)

# Keep full chain unchanged
transitions_full <- analyze_employment_transitions(
  pipeline_result = sample_data_chains,
  transition_variable = "company",
  eval_chain = "none"  # Keeps "StartupA->CompanyA->MegaCorp" as is
)

# ========================================================================
# CONSOLIDATION MODE EXAMPLES: Temporal vs Employer-Based
# ========================================================================

# Create realistic example data with employer information
result_with_employer <- copy(result)
result_with_employer[, employer_id := c("ACME_CORP", "ACME_CORP", "BETA_LTD",
                                       "GAMMA_INC", "GAMMA_INC", "DELTA_CO")]

# Example 1: TEMPORAL CONSOLIDATION (default behavior)
# Consolidates based on over_id and time proximity, regardless of employer
transitions_temporal <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",
  consolidation_mode = "temporal",      # Uses over_id for consolidation
  consolidation_type = "both"          # Consolidate overlapping + consecutive
)

# Example 2: EMPLOYER-BASED CONSOLIDATION
# Only consolidates contracts from same employer within time gap
transitions_employer <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",
  consolidation_mode = "employer",      # NEW: employer-based consolidation
  employer_var = "employer_id",         # Column containing employer identifiers
  min_lag = 8                 # Max 8-day gap for same-employer consolidation
)

# Example 3: Compare consolidation approaches
# Temporal: May consolidate CompanyA -> CompanyB if temporally adjacent
# Employer: Never consolidates CompanyA -> CompanyB (different employers)
print("Temporal consolidation results:")
print(transitions_temporal)
print("Employer-based consolidation results:")
print(transitions_employer)

# Example 4: Stricter employer-based consolidation (7-day gap)
# Useful for identifying very close contract renewals vs. true separations
transitions_strict <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",
  consolidation_mode = "employer",
  employer_var = "employer_id",
  min_lag = 7                  # Only 7-day gap allowed
)

# Example 5: Permissive employer-based consolidation (90-day gap)
# Useful for seasonal employers or project-based work patterns
transitions_permissive <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",
  consolidation_mode = "employer",
  employer_var = "employer_id",
  min_lag = 90                 # Allow 3-month gaps for same employer
)

# Example 6: Real-world use case comparison
# When you want to analyze job mobility vs. contract administration:

# For JOB MOBILITY analysis (focus on employer changes):
job_mobility <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "employer_id",   # Analyze employer-to-employer transitions
  consolidation_mode = "employer",
  employer_var = "employer_id",
  min_lag = 8
)

# For ROLE/POSITION analysis within and between employers:
role_transitions <- analyze_employment_transitions(
  pipeline_result = result_with_employer,
  transition_variable = "company",       # Analyze role/position transitions
  consolidation_mode = "employer",       # But respect employer boundaries
  employer_var = "employer_id",
  min_lag = 8
)

# Output will include columns like:
# from, to (salary values for transitions)
# company_from_mode, company_to_mode (company statistics)
# salary_from_median, salary_to_median (if salary is in statistics_variables)
} # }
```
