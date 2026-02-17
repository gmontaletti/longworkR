# Export Survival Analysis Results

Exports survival analysis results in various formats for reporting and
further analysis.

## Usage

``` r
export_survival_results(
  survival_results,
  output_format = "csv",
  output_dir = ".",
  prefix = "survival_analysis"
)
```

## Arguments

- survival_results:

  List output from survival analysis functions

- output_format:

  Character. Format: "csv", "excel", "rdata"

- output_dir:

  Character. Directory for output files

- prefix:

  Character. Prefix for output filenames

## Value

Character vector of created file paths
