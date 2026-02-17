# Visualize Contract Survival Curves

Creates publication-ready Kaplan-Meier survival curves with confidence
intervals and optional risk tables for different contract types.

## Usage

``` r
visualize_contract_survival(
  survival_curves,
  contract_types = NULL,
  show_confidence = TRUE,
  show_censored = TRUE,
  show_median = TRUE,
  risk_table = FALSE,
  title = "Survival Curves by Contract Type",
  subtitle = NULL,
  x_label = "Time (days)",
  y_label = "Survival Probability",
  color_palette = "Set2",
  theme_function = theme_minimal
)
```

## Arguments

- survival_curves:

  List output from estimate_contract_survival()

- contract_types:

  Character vector. Contract types to include (NULL for all)

- show_confidence:

  Logical. Display confidence intervals (default: TRUE)

- show_censored:

  Logical. Mark censored observations (default: TRUE)

- show_median:

  Logical. Add median survival lines (default: TRUE)

- risk_table:

  Logical. Include risk table below plot (default: FALSE)

- title:

  Character. Plot title

- subtitle:

  Character. Plot subtitle

- x_label:

  Character. X-axis label (default: "Time (days)")

- y_label:

  Character. Y-axis label (default: "Survival Probability")

- color_palette:

  Character or vector. RColorBrewer palette name (e.g., "Set2", "Dark2")
  or custom color vector. For large numbers of categories (\>8), colors
  are automatically extended using interpolation

- theme_function:

  Function. ggplot2 theme to apply

## Value

A ggplot object or combined plot with risk table

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic survival curves
plot_survival <- visualize_contract_survival(
  survival_curves = survival_results,
  title = "Contract Duration by Type"
)

# With risk table
plot_with_risk <- visualize_contract_survival(
  survival_curves = survival_results,
  risk_table = TRUE,
  show_median = TRUE
)
} # }
```
