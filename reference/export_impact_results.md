# Export Impact Evaluation Results in Multiple Formats

Exports impact evaluation results and summary tables in multiple formats
including CSV, Excel, JSON, and R data formats. Supports batch export
and customizable file organization.

## Usage

``` r
export_impact_results(
  analysis_results,
  export_formats = c("csv", "xlsx"),
  output_dir = getwd(),
  file_prefix = "impact_analysis",
  include_raw_data = TRUE,
  include_summary_tables = TRUE,
  include_plots = FALSE,
  plot_format = "png",
  excel_sheets = TRUE,
  compression = TRUE,
  timestamp = FALSE
)
```

## Arguments

- analysis_results:

  List. Complete analysis results from impact evaluation

- export_formats:

  Character vector. Export formats: "csv", "xlsx", "json", "rds",
  "rdata". Default: c("csv", "xlsx")

- output_dir:

  Character. Output directory path. Default: current working directory

- file_prefix:

  Character. Prefix for output files. Default: "impact_analysis"

- include_raw_data:

  Logical. Include raw analysis data. Default: TRUE

- include_summary_tables:

  Logical. Include formatted summary tables. Default: TRUE

- include_plots:

  Logical. Export plots as separate files. Default: FALSE

- plot_format:

  Character. Plot export format if included: "png", "pdf", "svg".
  Default: "png"

- excel_sheets:

  Logical. Create separate Excel sheets for different components.
  Default: TRUE

- compression:

  Logical. Use compression for file formats that support it. Default:
  TRUE

- timestamp:

  Logical. Add timestamp to filenames. Default: FALSE

## Value

A list containing paths to exported files organized by format

## Examples

``` r
if (FALSE) { # \dontrun{
# Export to CSV and Excel
exported_files <- export_impact_results(
  analysis_results = results,
  export_formats = c("csv", "xlsx"),
  file_prefix = "employment_program_impact"
)

# Export everything including plots
complete_export <- export_impact_results(
  analysis_results = results,
  export_formats = c("csv", "xlsx", "json"),
  include_plots = TRUE,
  plot_format = "png"
)
} # }
```
