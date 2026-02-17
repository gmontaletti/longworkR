# RDD Visualization

Creates comprehensive visualizations for regression discontinuity design
results, including scatter plots with fitted lines, density plots, and
robustness checks.

## Usage

``` r
rdd_plot(
  rdd_results,
  data,
  plot_type = "main",
  bin_width = NULL,
  confidence_level = 0.95,
  color_scheme = "default",
  show_points = TRUE,
  point_alpha = 0.3,
  bandwidth_lines = TRUE,
  title_main = NULL,
  subtitle = NULL
)
```

## Arguments

- rdd_results:

  Object of class "rdd_results" from regression_discontinuity()

- data:

  Original data used in RDD estimation

- plot_type:

  Character string: "main", "density", "robustness", or "all"

- bin_width:

  Numeric value for binning observations (NULL for automatic)

- confidence_level:

  Numeric value for confidence intervals (default: 0.95)

- color_scheme:

  Character string: "default", "colorblind", or "grayscale"

- show_points:

  Logical indicating whether to show individual data points

- point_alpha:

  Numeric value for point transparency (0-1)

- bandwidth_lines:

  Logical indicating whether to show bandwidth boundaries

- title_main:

  Character string for main plot title

- subtitle:

  Character string for subtitle

## Value

A ggplot2 object or list of ggplot2 objects (if plot_type = "all")

## Details

This function creates publication-ready RDD visualizations with the
following features:

- Binned scatter plots showing local means with confidence intervals

- Fitted regression lines on both sides of the cutoff

- Bandwidth boundaries and cutoff line

- Density plots to check for manipulation

- Robustness check visualizations

The function automatically bins observations and calculates local means
to reduce overplotting while preserving the discontinuity pattern. It
uses colorblind-friendly palettes and follows best practices for RDD
visualization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic RDD plot
p1 <- rdd_plot(
  rdd_results = rdd_results,
  data = employment_data,
  plot_type = "main"
)

# All plots with custom styling
plots <- rdd_plot(
  rdd_results = rdd_results,
  data = employment_data,
  plot_type = "all",
  color_scheme = "colorblind",
  title_main = "Employment RDD Results"
)
} # }
```
