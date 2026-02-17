# Compare Survival Curves Visually

Creates a comparative visualization of survival curves with statistical
test results and hazard ratio annotations.

## Usage

``` r
plot_survival_comparison(
  data,
  survival_curves,
  comparison_results = NULL,
  groups_to_show = NULL,
  highlight_significant = TRUE,
  show_pvalues = TRUE,
  reference_group = NULL,
  max_groups = 10,
  color_palette = "Set2"
)
```

## Arguments

- data:

  A data.table with contract information

- survival_curves:

  List output from estimate_contract_survival()

- comparison_results:

  List output from compare_contract_survival() or logical TRUE to
  compute it

- groups_to_show:

  Character vector. Specific contract types to display (NULL for all)

- highlight_significant:

  Logical. Highlight significant differences

- show_pvalues:

  Logical. Display p-values on plot

- reference_group:

  Character. Reference contract type for comparisons

- max_groups:

  Integer. Maximum number of groups to display (default: 10)

- color_palette:

  Character or vector. Color palette for the plot

## Value

A ggplot object with comparative survival curves
