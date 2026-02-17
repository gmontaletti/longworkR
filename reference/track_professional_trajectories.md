# Track Professional Trajectories

Tracks professional code trajectories for individuals who experienced a
specific contract type, following their professional code changes for 4
quarters (91 days each) after the end date of their chronologically
first occurrence of that contract type.

## Usage

``` r
track_professional_trajectories(
  data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  contract_type_value,
  n_quarters = 4,
  person_id = "cf",
  start_date_col = "inizio",
  end_date_col = "fine",
  arco_col = "arco",
  qualifica_col = "qualifica",
  return_plot = TRUE,
  palette = "professional",
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

  A data.table containing vecshift-processed employment data

- contract_type_var:

  Character. Column name containing contract type codes (default:
  "COD_TIPOLOGIA_CONTRATTUALE")

- contract_type_value:

  Character. Specific contract type value to track

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

- qualifica_col:

  Character. Column name for professional codes (default: "qualifica")

- return_plot:

  Logical. Whether to return the alluvial plot (default: TRUE)

- palette:

  Character. Color palette to use: "professional", "main",
  "desaturated", "bw" (default: "professional")

- use_bw:

  Logical. Force black and white palette (default: FALSE)

- plot_title:

  Character. Custom plot title (default: auto-generated)

- plot_subtitle:

  Character. Custom plot subtitle (default: auto-generated)

- person_vars:

  Character vector of person-level variable names to include in the
  output data (e.g., c("eta", "sesso", "istruzione")). Variables are
  extracted from the first target contract for each person. Default is
  NULL (no additional variables).

- chunk_size:

  Integer. Number of persons to process per chunk for memory management
  (default: 10000)

- language:

  Character. Language for status labels: "it" for Italian, "en" for
  English (default: "it")

## Value

A list containing:

- `data`: data.table with individual-level quarterly professional
  trajectories

- `summary`: data.table with aggregated trajectory patterns

- `transitions`: data.table with aggregated transition counts between
  quarters, including columns: from_quarter, to_quarter, from_status,
  to_status, person_vars (if provided), count

- `plot`: ggplot2 alluvial plot (if return_plot = TRUE)

- `parameters`: List of function parameters used

## Details

This function identifies individuals who had a specific contract type,
uses the end date of their chronologically FIRST occurrence (earliest
start date) as a reference point, then tracks professional code changes
in the subsequent quarters. Professional status is determined by
comparing the quarter's professional code with the reference contract's
professional code.

Professional code tracking is determined by the `qualifica` variable
where:

- Employment contracts (`arco > 0`) are prioritized over unemployment
  contracts (`arco = 0`)

- Within each quarter, the professional code is taken from the contract
  with the latest end date

- Professional status is classified as:

  - "Same Code": Professional code matches the reference contract

  - "Different Code": Professional code differs from the reference
    contract

  - "Not Working": All contracts in the quarter have arco = 0
    (unemployment)

  - "No Information": No contract data available for the quarter

The function only tracks forward in time from the reference date. When
multiple contracts exist within a quarter, employment contracts
(`arco > 0`) take priority, and among those, the contract with the
latest end date provides the professional code.

## See also

[`track_contract_trajectories`](https://gmontaletti.github.io/longworkR/reference/track_contract_trajectories.md),
[`theme_vecshift`](https://gmontaletti.github.io/longworkR/reference/theme_vecshift.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Track professional trajectories after temporary contracts
result <- track_professional_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  n_quarters = 4
)

# View the summary of professional trajectory patterns
print(result$summary)

# Display the alluvial plot
print(result$plot)

# Custom visualization with black and white
result_bw <- track_professional_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  use_bw = TRUE,
  plot_title = "Professional Trajectories After Temporary Contracts"
)
} # }
```
