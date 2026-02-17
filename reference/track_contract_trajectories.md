# Track Contract Trajectories

Tracks employment trajectories for individuals who experienced a
specific contract type, following their employment status for 4 quarters
(91 days each) after the end date of their chronologically first
occurrence of that contract type.

## Usage

``` r
track_contract_trajectories(
  data,
  contract_type_var = "COD_TIPOLOGIA_CONTRATTUALE",
  contract_type_value,
  n_quarters = 4,
  person_id = "cf",
  start_date_col = "inizio",
  end_date_col = "fine",
  arco_col = "arco",
  employment_thresholds = c(1, 40, 99),
  return_plot = TRUE,
  palette = "employment",
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

- employment_thresholds:

  Numeric vector. Three percentage values defining employment status
  classification thresholds (default: c(1, 40, 99))

- return_plot:

  Logical. Whether to return the alluvial plot (default: TRUE)

- palette:

  Character. Color palette to use: "employment", "main", "desaturated",
  "bw" (default: "employment")

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

- `data`: data.table with individual-level quarterly trajectories

- `summary`: data.table with aggregated trajectory patterns

- `transitions`: data.table with aggregated transition counts between
  quarters, including columns: from_quarter, to_quarter, from_status,
  to_status, person_vars (if provided), count

- `plot`: ggplot2 alluvial plot (if return_plot = TRUE)

- `parameters`: List of function parameters used

## Details

This function identifies individuals who had a specific contract type,
uses the end date of their chronologically FIRST occurrence (earliest
start date) as a reference point, then tracks employment status in the
subsequent quarters. Employment status is calculated based on the
percentage of days employed within each quarter, with overlapping
contracts counting as employment time.

Employment is determined by the `arco` variable where:

- `arco = 0`: Indicates unemployment (not working)

- `arco > 0`: Indicates employment (working)

All contract periods (both `arco = 0` and `arco > 0`) are included in
the analysis, but only periods where `arco > 0` are counted as
employment days. Employment status is then classified based on the
percentage of days employed within each 91-day quarter using
configurable thresholds (default: 1%, 40%, 99%):

- "Not Working": \< employment_thresholds1% of days employed

- "Partially Working": \>= employment_thresholds1% and \<=
  employment_thresholds2% of days employed

- "Mostly Working": \> employment_thresholds2% and \<=
  employment_thresholds3% of days employed

- "Fully Working": \> employment_thresholds3% of days employed

- "No Information": No contract data available for the quarter
  (days_employed = NA)

The function only tracks forward in time from the reference date. Any
employment is counted as working time, regardless of contract intensity
or prior employment status. Overlapping contracts are handled by
counting the union of employment periods using vectorized operations for
maximum performance.

## See also

[`theme_vecshift`](https://gmontaletti.github.io/longworkR/reference/theme_vecshift.md),
[`vecshift_colors`](https://gmontaletti.github.io/longworkR/reference/vecshift_colors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Track trajectories after temporary contracts
result <- track_contract_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  n_quarters = 4
)

# View the summary of trajectory patterns
print(result$summary)

# Display the alluvial plot
print(result$plot)

# Custom visualization with black and white
result_bw <- track_contract_trajectories(
  data = employment_data,
  contract_type_value = "B.01.00",
  use_bw = TRUE,
  plot_title = "Employment Trajectories After Temporary Contracts"
)

# Large dataset with chunked processing
result_large <- track_contract_trajectories(
  data = large_employment_data,
  contract_type_value = "B.01.00",
  chunk_size = 5000
)
} # }
```
