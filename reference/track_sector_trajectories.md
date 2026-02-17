# Track Sector Trajectories

Tracks economic sector transitions for individuals who experienced a
specific contract type, following their sector changes for 4 quarters
(91 days each) after the end date of their chronologically first
occurrence of that contract type.

## Usage

``` r
track_sector_trajectories(
  data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  contract_type_value,
  sector_col,
  n_quarters = 4,
  person_id = "cf",
  start_date_col = "inizio",
  end_date_col = "fine",
  arco_col = "arco",
  return_plot = TRUE,
  palette = "sector",
  use_bw = FALSE,
  plot_title = NULL,
  plot_subtitle = NULL,
  chunk_size = 10000,
  person_vars = NULL,
  language = "it"
)
```

## Arguments

- data:

  A data.table containing vecshift-processed employment data

- contract_type_var:

  Character. Column name containing contract type codes (default:
  "COD_TIPOLOGIA_CONTRATTUALE")

- contract_type_value:

  Character. Specific contract type value to track

- sector_col:

  Character. Column name containing sector codes (required parameter, no
  default)

- n_quarters:

  Integer. Number of quarters to track after reference date (default: 4)

- person_id:

  Character. Column name for person identifier (default: "cf")

- start_date_col:

  Character. Column name for contract start dates (default: "inizio")

- end_date_col:

  Character. Column name for contract end dates (default: "fine")

- arco_col:

  Character. Column name for employment status indicator where 0 =
  unemployment and \> 0 = employment (default: "arco")

- return_plot:

  Logical. Whether to return the alluvial plot (default: TRUE)

- palette:

  Character. Color palette to use: "sector", "main", "desaturated", "bw"
  (default: "sector")

- use_bw:

  Logical. Force black and white palette (default: FALSE)

- plot_title:

  Character. Custom plot title (default: auto-generated)

- plot_subtitle:

  Character. Custom plot subtitle (default: auto-generated)

- chunk_size:

  Integer. Number of persons to process per chunk for memory management
  (default: 10000)

- person_vars:

  Character vector of person-level variable names to include in the
  output data (e.g., c("eta", "sesso", "istruzione")). Variables are
  extracted from the first target contract for each person. Default is
  NULL (no additional variables).

- language:

  Character. Language for status labels: "it" for Italian, "en" for
  English (default: "it")

## Value

A list containing:

- `data`: data.table with individual-level quarterly sector trajectories

- `summary`: data.table with aggregated trajectory patterns

- `transitions`: data.table with aggregated transition counts between
  quarters, including columns: from_quarter, to_quarter, from_status,
  to_status, person_vars (if provided), count

- `plot`: ggplot2 alluvial plot (if return_plot = TRUE)

- `parameters`: List of function parameters used

## Details

This function identifies individuals who had a specific contract type,
uses the end date of their chronologically FIRST occurrence (earliest
start date) as a reference point, then tracks sector transitions in the
subsequent quarters. Sector status is determined by comparing the
quarter's sector with the reference contract's sector.

Employment is determined by the `arco` variable where:

- `arco = 0`: Indicates unemployment (not working)

- `arco > 0`: Indicates employment (working)

When multiple contracts exist within a quarter:

- Employment contracts (`arco > 0`) take priority over unemployment
  periods

- Within each quarter, the sector is taken from the contract with the
  latest end date

- Sector status is classified as:

  - "Same Sector": Sector matches the reference contract sector

  - "Different Sector": Sector differs from the reference contract

  - "Not Working": All contracts in the quarter have arco = 0
    (unemployment)

  - "No Information": No contract data available for the quarter

The function only tracks forward in time from the reference date. When
multiple contracts exist within a quarter, employment contracts
(`arco > 0`) take priority, and among those, the contract with the
latest end date provides the sector code.

## See also

[`track_contract_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_contract_trajectories.md),
[`track_professional_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_professional_trajectories.md),
[`track_employer_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_employer_trajectories.md),
[`theme_vecshift`](https://gmontaletti.github.io/longworkR/reference/theme_vecshift.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Track sector trajectories after temporary contracts using ATECO codes
result <- track_sector_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  sector_col = "ATECO_2007_SEZIONE",
  n_quarters = 4
)

# View the summary of sector trajectory patterns
print(result$summary)

# Display the alluvial plot
print(result$plot)

# Track trajectories with industry division codes
result_industry <- track_sector_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  sector_col = "ATECO_2007_DIVISIONE",
  n_quarters = 6
)

# Custom visualization with black and white
result_bw <- track_sector_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  sector_col = "ATECO_2007_SEZIONE",
  use_bw = TRUE,
  plot_title = "Sector Trajectories After Temporary Contracts"
)
} # }
```
