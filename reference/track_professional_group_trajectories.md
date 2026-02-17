# Track professional group trajectories over time

This function analyzes how individuals move between professional groups
after completing a specific contract type. It tracks whether individuals
stay in the same professional group, move to different groups, become
unemployed, or have no information available over a specified number of
quarters.

## Usage

``` r
track_professional_group_trajectories(
  data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  contract_type_value,
  group_col,
  n_quarters = 4,
  person_id = "cf",
  start_date_col = "inizio",
  end_date_col = "fine",
  arco_col = "arco",
  return_plot = TRUE,
  palette = "professional_group",
  use_bw = FALSE,
  plot_title = NULL,
  plot_subtitle = NULL,
  person_vars = NULL,
  chunk_size = 10000,
  language = "it"
)
```

## Arguments

- data:

  data.table containing employment contract data processed by vecshift

- contract_type_var:

  Character string specifying the contract type variable name

- contract_type_value:

  Character string specifying the contract type to analyze

- group_col:

  Character string specifying the professional group column name
  (required)

- n_quarters:

  Integer specifying number of quarters to track (1-20)

- person_id:

  Character string specifying the person identifier column

- start_date_col:

  Character string specifying the contract start date column

- end_date_col:

  Character string specifying the contract end date column

- arco_col:

  Character string specifying the employment intensity column

- return_plot:

  Logical indicating whether to create an alluvial plot

- palette:

  Character string specifying the color palette to use

- use_bw:

  Logical indicating whether to use black and white colors

- plot_title:

  Character string for plot title (auto-generated if NULL)

- plot_subtitle:

  Character string for plot subtitle (auto-generated if NULL)

- person_vars:

  Character vector of person-level variable names to include in the
  output data (e.g., c("eta", "sesso", "istruzione")). Variables are
  extracted from the first target contract for each person. Default is
  NULL (no additional variables).

- chunk_size:

  Integer specifying the chunk size for processing large datasets

- language:

  Character. Language for status labels: "it" for Italian, "en" for
  English (default: "it")

## Value

A list containing:

- `data`: data.table with individual-level quarterly group trajectories

- `summary`: data.table with aggregated trajectory patterns

- `transitions`: data.table with aggregated transition counts between
  quarters, including columns: from_quarter, to_quarter, from_status,
  to_status, person_vars (if provided), count

- `plot`: ggplot2 alluvial plot (if return_plot = TRUE)

- `parameters`: List of function parameters used

## Details

The function requires exactly 4 unique professional groups in the data
and uses the actual group names found in the data (no relabeling).

The function identifies individuals who completed the specified contract
type and tracks their professional group transitions over the following
quarters. For each person-quarter, the function shows which of the 4
professional groups the person is working in, or:

- **Group Name**: Person works in one of the 4 professional groups
  (actual group names from data)

- **Not Working**: Person has contracts but with arco = 0
  (unemployment/inactivity)

- **No Information**: Person has no contracts in the quarter

The function validates that exactly 4 unique professional groups exist
in the target contracts and uses the actual group names found in the
data:

- Groups preserve their original names from the data

- Colors assigned automatically: first group (Red), second group (Blue),
  third group (Green), fourth group (Orange)

- Additional categories: "Not Working" (Grey), "No Information" (Dark
  Grey)

## See also

[`track_contract_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_contract_trajectories.md),
[`track_sector_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_sector_trajectories.md),
[`track_employer_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_employer_trajectories.md),
[`theme_vecshift`](https://gmontaletti.github.io/longworkR/reference/theme_vecshift.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Track professional group trajectories after temporary contracts
result <- track_professional_group_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  group_col = "professional_category",
  n_quarters = 4
)

# View the summary of group trajectory patterns
print(result$summary)

# Display the alluvial plot
print(result$plot)

# Track trajectories with custom visualization
result_custom <- track_professional_group_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  group_col = "job_level",
  n_quarters = 6,
  plot_title = "Professional Group Mobility After Temporary Contracts"
)

# Black and white visualization
result_bw <- track_professional_group_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  group_col = "professional_category",
  use_bw = TRUE
)
} # }
```
