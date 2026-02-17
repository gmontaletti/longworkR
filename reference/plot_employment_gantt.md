# Plot Employment Gantt Chart

Creates Gantt chart visualizations of employment periods, ideal for
showing individual employment histories, project timelines, or contract
overlaps with precise temporal detail. The function uses standardized
employment colors for consistency across all longworkR visualizations
and supports explicit person selection for publication reproducibility.

## Usage

``` r
plot_employment_gantt(
  data,
  person_ids = NULL,
  max_persons = 10,
  sort_by = "total_duration",
  title = "Employment Gantt Chart",
  show_legend = TRUE,
  time_unit = "months",
  color_palette = "employment",
  person_col = "cf",
  time_col = "inizio",
  end_col = "fine",
  status_col = "stato",
  contract_col = "id",
  facet_by = NULL,
  date_breaks = "2 months",
  show_contract_ids = FALSE,
  show_durations = FALSE,
  palette = "employment",
  use_bw = FALSE,
  bar_height = 0.6,
  alpha = 0.8,
  text_size = 2.5
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments.
  Required columns: cf (person ID), inizio (start date), fine (end
  date), stato (employment status), durata (duration in days)

- person_ids:

  Character or numeric vector. Specific person identifiers to visualize.
  When provided, shows only these persons and ignores max_persons
  parameter. When NULL (default), uses max_persons to select first N
  individuals. **For reproducible research**, always specify person_ids
  explicitly rather than relying on automatic selection. Example: c(6,
  8, 21) or c("Person_A", "Person_B")

- max_persons:

  Integer. Maximum number of people to show when person_ids is NULL.
  Used only for backward compatibility and exploratory analysis
  (default: 10)

- sort_by:

  Character. Sort method for person selection when using max_persons:
  "total_duration", "first_contract", "last_contract" (default:
  "total_duration"). Not used when person_ids is specified

- title:

  Character. Main title for the plot (default: "Employment Gantt Chart")

- show_legend:

  Logical. Whether to show the employment status legend (default: TRUE).
  Set to FALSE for multi-panel figures where legend appears elsewhere

- time_unit:

  Character. Time unit for axis labels: "days", "weeks", "months",
  "years". Currently used for documentation; axis formatting is
  automatic (default: "months")

- color_palette:

  Character. Color palette name. Use "employment" for standardized
  employment colors (recommended) or "main" for general palette
  (default: "employment")

- person_col:

  Character. Column name for person identifier (default: "cf")

- time_col:

  Character. Column name for period start dates (default: "inizio")

- end_col:

  Character. Column name for period end dates (default: "fine")

- status_col:

  Character. Column name for employment status (default: "stato")

- contract_col:

  Character. Column name for contract ID numbers (default: "id")

- facet_by:

  Character. Column to use for faceting into subplots (default: NULL)

- date_breaks:

  Character. Date breaks for x-axis labels (default: "2 months")

- show_contract_ids:

  Logical. Show contract ID numbers on employment bars (default: FALSE).
  Useful for detailed contract analysis but can clutter visualization
  with many contracts

- show_durations:

  Logical. Show duration labels (e.g., "3m", "1.2y") on bars (default:
  FALSE). Automatically formats as days (d), months (m), or years (y)
  based on duration length

- palette:

  Character. Alternative parameter name for color_palette (default:
  "employment")

- use_bw:

  Logical. Use black and white palette for print publications (default:
  FALSE)

- bar_height:

  Numeric. Height of Gantt bars as proportion of available space
  (default: 0.6)

- alpha:

  Numeric. Transparency level for bars, 0 (transparent) to 1 (opaque)
  (default: 0.8)

- text_size:

  Numeric. Size of text labels for contract IDs and durations (default:
  2.5)

## Value

A ggplot2 object containing the employment Gantt chart. The plot
includes milestone markers for contract start/end dates and uses
standardized employment colors for consistency across publications.

## Details

The function creates professional Gantt charts with the following
features:

- **Standardized Color Consistency**: Uses the same employment status
  colors across all longworkR functions to prevent reader confusion

- **Publication Reproducibility**: Supports explicit person_ids
  parameter to ensure identical visualizations across analyses

- **Accessible Design**: Colorblind-friendly palettes and clear visual
  hierarchy

- **Flexible Annotations**: Optional contract IDs and duration labels

- **Professional Theming**: Uses theme_vecshift() for publication-ready
  output

**Color Consistency**: This function automatically uses standardized
employment colors from get_standardized_employment_colors() to ensure
that the same employment state (e.g., "occ_ft", "disoccupato") gets
identical colors in all visualizations. This is critical for multi-panel
figures and cross-referenced publications.

## Color Standards

This function uses standardized employment colors to ensure consistency:

- Same employment status always gets same color across all plots

- Colors are colorblind-accessible and print-friendly

- Consistent with plot_employment_gantt_advanced() and other functions

## See also

[`plot_employment_gantt_advanced`](https://gmontaletti.github.io/longworkR/reference/plot_employment_gantt_advanced.md)
for combined Gantt and metrics visualization,
[`plot_employment_timeline`](https://gmontaletti.github.io/longworkR/reference/plot_employment_timeline.md)
for simpler timeline plots,
[`theme_vecshift`](https://gmontaletti.github.io/longworkR/reference/theme_vecshift.md)
for the underlying plot theme,
[`get_standardized_employment_colors`](https://gmontaletti.github.io/longworkR/reference/get_standardized_employment_colors.md)
for color consistency

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample employment data
employment_data <- readRDS("data/sample.rds")

# Basic Gantt chart (backward compatible - shows first 10 people)
plot_employment_gantt(employment_data)

# Reproducible research: specify exact persons
plot_employment_gantt(employment_data, person_ids = c(6, 8, 21))

# Publication-ready chart with annotations
plot_employment_gantt(
  employment_data,
  person_ids = c("Person_A", "Person_B", "Person_C"),
  show_contract_ids = TRUE,
  show_durations = TRUE,
  title = "Employment Histories: Sample Cohort"
)

# Black and white version for print
plot_employment_gantt(
  employment_data,
  person_ids = c(6, 8),
  use_bw = TRUE,
  show_legend = TRUE
)

# Detailed analysis with contract focus
plot_employment_gantt(
  employment_data,
  max_persons = 5,
  show_contract_ids = TRUE,
  date_breaks = "1 month",
  alpha = 0.9
)
} # }
```
