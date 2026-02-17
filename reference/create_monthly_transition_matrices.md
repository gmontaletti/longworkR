# Create Monthly Transition Matrices from Vecshift Data with Advanced Memory Management

Generates a named list of transition matrices, one for each time period
(month/quarter/custom), from vecshift employment data. Features
intelligent memory management for large datasets (290,575+ unique
states) with automatic fallback mechanisms to prevent memory errors.
Supports both sparse and dense matrix formats for optimal performance
across different dataset sizes.

## Usage

``` r
create_monthly_transition_matrices(
  pipeline_result,
  transition_variable = NULL,
  time_column = "fine",
  time_format = c("monthly", "quarterly", "custom"),
  custom_period_days = NULL,
  date_range = NULL,
  name_format = c("date", "period", "custom"),
  custom_names = NULL,
  matrix_format = c("dense", "sparse"),
  consolidation_type = "both",
  min_unemployment_duration = 1,
  max_unemployment_duration = NULL,
  matrix_type = c("frequency", "probability"),
  normalize_by = "row",
  eval_chain = "last",
  include_summary = TRUE,
  show_progress = TRUE,
  use_global_state_space = FALSE,
  memory_limit_gb = 1
)
```

## Arguments

- pipeline_result:

  Output from process_employment_pipeline(). Must be a data.table with
  columns: cf (person identifier), arco (employment overlap count),
  inizio/fine (period dates), durata (period duration), and the
  transition variable. Optionally over_id if consolidation is used.

- transition_variable:

  Character string specifying the variable to use for transition
  analysis (from/to values). If NULL (default), uses the first
  non-standard attribute in the data.table.

- time_column:

  Character string specifying the date column for transition timing.
  Default: "fine" (end date of employment periods).

- time_format:

  Character string specifying time period format. One of: "monthly"
  (default), "quarterly", "custom".

- custom_period_days:

  Integer number of days for custom periods. Required when time_format =
  "custom".

- date_range:

  Optional vector of two Date objects specifying analysis start and end
  dates. If NULL (default), uses full range from data.

- name_format:

  Character string specifying matrix naming convention. One of: "date"
  (default), "period", "custom".

- custom_names:

  Optional character vector of custom period names. Required when
  name_format = "custom".

- matrix_format:

  Character string specifying matrix output format. Options:

  - "dense" (default): Standard R matrices, suitable for small to medium
    datasets

  - "sparse": Matrix package sparse matrices, optimal for large datasets
    with many zero entries

  For datasets with \>10,000 states, the function automatically switches
  to sparse format regardless of this setting to prevent memory issues.

- consolidation_type:

  Character string specifying consolidation approach (default: "both").
  Options: "both", "overlapping", "consecutive", "none".

- min_unemployment_duration:

  Minimum duration (in days) of unemployment period to consider a
  transition (default: 1).

- max_unemployment_duration:

  Maximum duration (in days) of unemployment period to consider a
  transition. If NULL (default), no upper limit is applied.

- matrix_type:

  Character string specifying output matrix type. One of: "frequency"
  (default), "probability".

- normalize_by:

  Character string for probability matrices. One of: "row" (default),
  "column", "total".

- eval_chain:

  Character string specifying how to handle chained values (default:
  "last"). Options: "last", "first", "none".

- include_summary:

  Logical. If TRUE (default), includes summary information and metadata
  in the output.

- show_progress:

  Logical. If TRUE (default), displays progress messages.

- use_global_state_space:

  Logical. Memory management mode selection (default: FALSE).

  - FALSE (default): Uses period-specific state spaces for maximum
    memory efficiency. Each matrix only includes states present in that
    period, dramatically reducing memory usage for large datasets.

  - TRUE: Uses global state space for consistent matrix dimensions
    across all periods. All matrices have identical structure, but
    requires significant memory for large datasets (e.g., 290,575+
    states may require \>1GB RAM).

- memory_limit_gb:

  Numeric. Memory threshold in GB for automatic fallback protection
  (default: 1.0). When estimated memory usage for dense matrices exceeds
  this limit, the function automatically switches to period-specific
  state spaces regardless of use_global_state_space setting. This
  prevents out-of-memory errors on large datasets. Set higher (e.g.,
  4.0) if you have sufficient RAM and need global state space
  consistency.

## Value

A named list containing:

- **matrices**: Named list of transition matrices, one per time period

- **metadata** (if include_summary = TRUE): Information about the
  analysis including:

  - global_state_space: All unique states used across matrices

  - period_info: Time period boundaries and names

  - matrix_dimensions: Consistent dimensions (nrow x ncol)

  - total_periods: Number of time periods analyzed

  - periods_with_transitions: Number of periods containing transitions

  - analysis_parameters: Function parameters used

## Details

This function creates time-series transition matrices with advanced
memory management:

1.  **Memory Assessment**: Estimates memory requirements before
    allocation

2.  **State Space Strategy**: Chooses between global or period-specific
    state spaces

3.  **Smart Fallback**: Automatically switches modes when memory limits
    are exceeded

4.  **Matrix Format Selection**: Uses sparse matrices for large state
    spaces (\>10,000 states)

5.  **Progress Monitoring**: Reports memory usage and performance
    warnings

**Key Features:**

- **Memory Efficiency**: Intelligent memory management prevents
  out-of-memory errors

- **Automatic Optimization**: Switches between global/local state spaces
  based on memory constraints

- **Sparse Matrix Support**: Uses Matrix package for large datasets

- **Progress Reporting**: Real-time memory usage and performance
  feedback

- **Flexible Periods**: Monthly, quarterly, or custom time periods
  supported

- **Consistent Dimensions**: Matrices maintain identical structure when
  using global state space

- **Transition Assignment**: Uses "end" assignment - transitions
  assigned when "to" state begins

**Memory Management Modes:**

- **Global State Space** (use_global_state_space = TRUE): All matrices
  use same dimensions, higher memory usage

- **Period-Specific State Space** (use_global_state_space = FALSE,
  default): Each matrix uses only its states, memory efficient

- **Automatic Fallback**: Switches to period-specific mode when memory
  exceeds limit

**Matrix Naming:**

- **date**: "jan2022", "feb2022" (monthly), "2022q1", "2022q2"
  (quarterly)

- **period**: "period_1", "period_2", etc.

- **custom**: User-provided names

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample data
sample_data <- readRDS("data/sample.rds")

# Basic monthly transition matrices (memory-efficient, default)
monthly_matrices <- create_monthly_transition_matrices(
  sample_data,
  transition_variable = "COD_TIPOLOGIA_CONTRATTUALE"
)

# Access individual matrices
jan_matrix <- monthly_matrices$matrices$jan2022
feb_matrix <- monthly_matrices$matrices$feb2022

# Check memory usage and matrix info
monthly_matrices$metadata$memory_mode  # Shows which mode was used
monthly_matrices$metadata$estimated_memory_gb  # Memory estimation

# Global state space mode (consistent dimensions, higher memory)
global_matrices <- create_monthly_transition_matrices(
  sample_data,
  transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
  use_global_state_space = TRUE,
  memory_limit_gb = 2.0  # Allow higher memory usage
)

# All matrices have identical dimensions in global mode
identical(dim(global_matrices$matrices$jan2022),
          dim(global_matrices$matrices$feb2022))  # TRUE

# Sparse matrices for large datasets
sparse_matrices <- create_monthly_transition_matrices(
  sample_data,
  transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
  matrix_format = "sparse",
  show_progress = TRUE  # Monitor memory usage
)

# Quarterly matrices with probability normalization
quarterly_prob <- create_monthly_transition_matrices(
  sample_data,
  transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
  time_format = "quarterly",
  matrix_type = "probability",
  normalize_by = "row"
)

# Custom 90-day periods
custom_matrices <- create_monthly_transition_matrices(
  sample_data,
  transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
  time_format = "custom",
  custom_period_days = 90,
  name_format = "custom",
  custom_names = c("Q1_extended", "Q2_extended", "Q3_extended", "Q4_extended")
)

# Monitor performance on large datasets
large_result <- create_monthly_transition_matrices(
  large_data,
  show_progress = TRUE,      # Shows memory warnings
  memory_limit_gb = 0.5,    # Conservative memory limit
  matrix_format = "sparse"  # Use sparse matrices
)
} # }
```
