# Analyze Employment Transitions with Consolidation Metrics

Wrapper function around analyze_employment_transitions() that captures
and returns detailed consolidation metrics alongside transition analysis
results. This function provides comprehensive insight into the
consolidation process across all consolidation modes (none, temporal,
employer, both).

## Usage

``` r
analyze_employment_transitions_with_metrics(
  pipeline_result,
  transition_variable = NULL,
  statistics_variables = NULL,
  min_unemployment_duration = 1,
  max_unemployment_duration = NULL,
  consolidation_type = "both",
  consolidation_mode = "none",
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

- consolidation_type:

  Character string specifying the temporal consolidation approach
  (default: "both"). Only used when `consolidation_mode` includes
  temporal consolidation. Options: "both", "overlapping",
  "employment_only".

- consolidation_mode:

  Character string specifying the consolidation strategy to apply before
  transition analysis (default: "none"). Options:

  - `"none"`: No consolidation (default, backward-compatible)

  - `"temporal"`: Consolidate overlapping periods using over_id

  - `"employer"`: Consolidate consecutive contracts from the same
    employer

  - `"both"`: Apply employer consolidation first, then temporal

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

List containing:

- `transition_results`: Results from analyze_employment_transitions()

- `consolidation_metrics`: Detailed consolidation metrics from
  extract_consolidation_metrics()

- `consolidation_summary`: Quick summary report from
  summarize_consolidation()

## Details

This wrapper function provides a complete picture of both transition
patterns and consolidation impact. It's particularly useful for:

- **Method Comparison**: Understanding consolidation effectiveness
  across modes

- **Quality Assessment**: Evaluating consolidation impact on data
  structure

- **Research Applications**: Documenting consolidation methodology and
  results

- **Performance Analysis**: Understanding consolidation computational
  benefits

The function handles all consolidation modes seamlessly:

- **none**: Provides baseline transition analysis with no-consolidation
  metrics

- **temporal**: Captures over_id-based consolidation patterns and
  effectiveness

- **employer**: Documents same-employer consolidation impact and
  patterns

- **both**: Tracks sequential consolidation effects and combined
  benefits

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample data
result <- readRDS("data/sample.rds")

# Analyze with temporal consolidation and capture metrics
analysis <- analyze_employment_transitions_with_metrics(
  pipeline_result = result,
  transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
  consolidation_mode = "temporal",
  consolidation_type = "both",
  show_progress = TRUE
)

# View transition results
print(analysis$transition_results)

# Examine consolidation impact
print(analysis$consolidation_summary)

# Detailed consolidation metrics
str(analysis$consolidation_metrics)

# Employer-based consolidation with metrics
analysis_employer <- analyze_employment_transitions_with_metrics(
  pipeline_result = result,
  consolidation_mode = "employer",
  employer_var = "COD_TIPOLOGIA_CONTRATTUALE",  # Using contract type as employer proxy
  min_lag = 8
)

# Compare consolidation effectiveness
print("Temporal consolidation ratio:", analysis$consolidation_metrics$consolidation_summary$consolidation_ratio)
print("Employer consolidation ratio:", analysis_employer$consolidation_metrics$consolidation_summary$consolidation_ratio)
} # }
```
