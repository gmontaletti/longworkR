# Create Forest Plot of Median Survival Times

Creates a forest plot showing median survival times with confidence
intervals for each contract type.

## Usage

``` r
plot_median_survival_forest(
  survival_curves,
  order_by = "median",
  custom_order = NULL,
  show_n = TRUE
)
```

## Arguments

- survival_curves:

  List output from estimate_contract_survival()

- order_by:

  Character. How to order: "median", "alphabetical", "custom"

- custom_order:

  Character vector. Custom ordering of contract types

- show_n:

  Logical. Show sample sizes

## Value

A ggplot forest plot object
