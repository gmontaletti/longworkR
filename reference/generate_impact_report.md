# Generate Comprehensive Impact Evaluation Report

Creates a professional HTML or PDF report for impact evaluation studies
using R Markdown. Integrates all analysis components including event
identification, matching results, impact estimates, robustness checks,
and visualizations. Supports customizable templates and follows academic
standards for impact evaluation reporting.

## Usage

``` r
generate_impact_report(
  analysis_results,
  output_format = "html",
  output_file = "impact_evaluation_report",
  output_dir = getwd(),
  template = "academic",
  title = "Impact Evaluation Report",
  subtitle = NULL,
  author = "vecshift Analysis",
  date = Sys.Date(),
  include_sections = c("executive_summary", "methodology", "results", "robustness",
    "conclusions", "technical_appendix"),
  include_plots = TRUE,
  include_tables = TRUE,
  table_format = "kable",
  plot_theme = "minimal",
  colorblind_friendly = TRUE,
  dpi = 300,
  keep_tex = FALSE,
  quiet = TRUE
)
```

## Arguments

- analysis_results:

  List. Complete analysis results from impact evaluation pipeline

- output_format:

  Character. Output format: "html", "pdf", or "both". Default: "html"

- output_file:

  Character. Output file name (without extension). Default:
  "impact_evaluation_report"

- output_dir:

  Character. Output directory path. Default: current working directory

- template:

  Character. Report template: "academic", "policy", "technical",
  "executive". Default: "academic"

- title:

  Character. Report title. Default: "Impact Evaluation Report"

- subtitle:

  Character. Report subtitle. Default: NULL

- author:

  Character. Author name(s). Default: "vecshift Analysis"

- date:

  Character. Report date. Default: current date

- include_sections:

  Character vector. Sections to include: "executive_summary",
  "methodology", "results", "robustness", "conclusions",
  "technical_appendix". Default: all sections

- include_plots:

  Logical. Include visualization plots. Default: TRUE

- include_tables:

  Logical. Include summary tables. Default: TRUE

- table_format:

  Character. Table format for output: "kable", "gt", "flextable".
  Default: "kable"

- plot_theme:

  Character. ggplot2 theme for plots: "minimal", "classic", "bw".
  Default: "minimal"

- colorblind_friendly:

  Logical. Use colorblind-friendly palettes. Default: TRUE

- dpi:

  Numeric. Plot resolution (DPI) for output. Default: 300

- keep_tex:

  Logical. Keep .tex file for PDF output. Default: FALSE

- quiet:

  Logical. Suppress R Markdown rendering messages. Default: TRUE

## Value

Character. Path to generated report file(s)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate comprehensive HTML report
report_path <- generate_impact_report(
  analysis_results = complete_analysis,
  output_format = "html",
  template = "academic",
  title = "Employment Program Impact Evaluation",
  author = "Policy Research Team"
)

# Generate executive summary in PDF
exec_report <- generate_impact_report(
  analysis_results = complete_analysis,
  output_format = "pdf",
  template = "executive",
  include_sections = c("executive_summary", "results")
)
} # }
```
