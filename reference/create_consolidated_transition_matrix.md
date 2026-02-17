# Create Consolidated Transition Matrix Using over_id

Creates transition matrices using consolidated employment periods
leveraging the over_id functionality from vecshift() output. This
function provides cleaner transition matrices by consolidating
overlapping and/or consecutive employment periods before analyzing
transitions, reducing administrative noise and providing more accurate
career movement patterns.

## Usage

``` r
create_consolidated_transition_matrix(
  pipeline_result,
  transition_variable,
  consolidation_type = "both",
  matrix_type = c("frequency", "probability"),
  include_comparison = TRUE,
  normalize_by = "row",
  min_unemployment_duration = 1,
  max_unemployment_duration = NULL,
  show_progress = TRUE
)
```

## Arguments

- pipeline_result:

  Output from process_employment_pipeline() or vecshift(). Must be a
  data.table with columns: cf (person identifier), arco (employment
  overlap count), inizio/fine (period dates), durata (period duration),
  over_id (overlap identifier), and the specified transition_variable.

- transition_variable:

  Character string specifying the variable to create transitions for
  (e.g., "employment_type", "company", "salary_level"). This variable
  defines the states in the transition matrix.

- consolidation_type:

  Character string specifying consolidation approach (default: "both"):

  - `"both"`: Complete consolidation - overlapping then consecutive
    periods

  - `"overlapping"`: Only consolidate segments with same over_id \> 0

  - `"consecutive"`: Only merge contiguous periods regardless of over_id

  - `"none"`: No consolidation, equivalent to raw matrix

- matrix_type:

  Character vector specifying output matrix types (default:
  c("frequency", "probability")):

  - `"frequency"`: Raw transition counts

  - `"probability"`: Normalized transition probabilities

  - `"both"`: Both frequency and probability matrices

- include_comparison:

  Logical. If TRUE (default), includes comparison between raw and
  consolidated matrices with improvement metrics.

- normalize_by:

  Character string specifying probability normalization method when
  matrix_type includes "probability" (default: "row"):

  - `"row"`: Row normalization - P(to\|from), each row sums to 1

  - `"column"`: Column normalization - P(from\|to), each column sums to
    1

  - `"total"`: Total normalization - P(from,to), entire matrix sums to 1

- min_unemployment_duration:

  Minimum duration (days) of unemployment period to consider a
  transition (default: 1).

- max_unemployment_duration:

  Maximum duration (days) of unemployment period to consider a
  transition. If NULL (default), no upper limit is applied.

- show_progress:

  Logical. If TRUE (default), displays progress messages.

## Value

A list containing:

- `consolidated_matrix`: Primary consolidated transition matrix
  (frequency or probability)

- `consolidated_frequency_matrix`: Consolidated frequency matrix (if
  "both" requested)

- `consolidated_probability_matrix`: Consolidated probability matrix (if
  "both" requested)

- `raw_matrix`: Raw transition matrix for comparison (if
  include_comparison = TRUE)

- `matrix_comparison`: data.table with comparison metrics (if
  include_comparison = TRUE):

  - `metric`: Comparison metric name

  - `raw_value`: Value for raw matrix

  - `consolidated_value`: Value for consolidated matrix

  - `improvement`: Improvement measure (lower sparsity, complexity
    reduction, etc.)

- `consolidation_impact`: Summary of consolidation benefits

- `matrix_statistics`: Detailed matrix statistics and properties

All results include attributes:

- `analysis_parameters`: Parameters used for matrix creation

- `consolidation_summary`: Summary of consolidation applied

- `matrix_properties`: Mathematical properties of resulting matrices

## Details

This function leverages the over_id column to create consolidated
transition matrices:

- **over_id = 0**: Unemployment periods (no active contracts)

- **over_id \> 0**: Employment periods with same value for
  overlapping/continuous contracts

Key benefits of consolidated matrices:

- **Reduced Noise**: Eliminates transitions between administrative
  contract splits

- **True Career Moves**: Focuses on genuine employment transitions
  between different states

- **Cleaner Patterns**: Consolidates overlapping contracts into single
  employment episodes

- **Better Analysis**: More accurate transition probabilities and
  frequencies

- **Matrix Comparison**: Quantifies improvement over raw transition
  matrices

The function supports multiple consolidation types:

- `"both"`: Consolidate overlapping periods first, then merge
  consecutive periods

- `"overlapping"`: Only consolidate segments with same over_id \> 0

- `"consecutive"`: Only merge periods that are contiguous in time

- `"none"`: No consolidation (equivalent to raw transition matrix)

Matrix types and normalization:

- **Frequency**: Raw transition counts (default)

- **Probability**: Normalized transition probabilities

- **Row normalization**: Each row sums to 1 (conditional probability
  given "from" state)

- **Column normalization**: Each column sums to 1 (reverse conditional
  probability)

- **Total normalization**: Entire matrix sums to 1 (joint probability)

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample employment data with overlapping contracts
employment_data <- data.table(
  id = 1:10,
  cf = c(rep("PERSON001", 6), rep("PERSON002", 4)),
  INIZIO = as.Date(c("2023-01-01", "2023-02-15", "2023-03-01", "2023-05-01",
                     "2023-08-01", "2023-10-01", "2023-01-15", "2023-04-01",
                     "2023-07-01", "2023-09-15")),
  FINE = as.Date(c("2023-02-28", "2023-04-30", "2023-04-15", "2023-06-30",
                   "2023-09-30", "2023-12-31", "2023-03-31", "2023-06-15",
                   "2023-08-31", "2023-12-31")),
  prior = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1),
  company = c("CompanyA", "CompanyB", "CompanyA", "CompanyC", "CompanyD", "CompanyE",
              "CompanyF", "CompanyG", "CompanyH", "CompanyI"),
  employment_type = c("FT", "PT", "FT", "FT", "PT", "FT", "FT", "PT", "FT", "FT")
)

# Process through vecshift to get over_id
result <- vecshift(employment_data)

# Create consolidated transition matrix for companies with full comparison
consolidated_result <- create_consolidated_transition_matrix(
  pipeline_result = result,
  transition_variable = "company",
  consolidation_type = "both",
  matrix_type = "both",
  include_comparison = TRUE,
  normalize_by = "row"
)

# View consolidated frequency matrix
print(consolidated_result$consolidated_frequency_matrix)

# View consolidated probability matrix
print(consolidated_result$consolidated_probability_matrix)

# Compare improvement over raw matrix
print(consolidated_result$matrix_comparison)

# View consolidation impact
print(consolidated_result$consolidation_impact)

# Create employment type transitions with overlapping consolidation only
employment_transitions <- create_consolidated_transition_matrix(
  pipeline_result = result,
  transition_variable = "employment_type",
  consolidation_type = "overlapping",
  matrix_type = "probability",
  normalize_by = "row",
  min_unemployment_duration = 7,
  include_comparison = FALSE
)
print(employment_transitions$consolidated_matrix)

# Compare different consolidation strategies
no_consolidation <- create_consolidated_transition_matrix(
  result, "company", "none", "frequency", FALSE
)
overlapping_only <- create_consolidated_transition_matrix(
  result, "company", "overlapping", "frequency", FALSE
)
full_consolidation <- create_consolidated_transition_matrix(
  result, "company", "both", "frequency", FALSE
)

# Output demonstrates:
# - Raw matrix: May show A->A transitions due to overlapping contracts
# - Consolidated: Shows true A->B career transitions
# - Reduced sparsity and cleaner transition patterns
# - Quantified improvement metrics
} # }
```
