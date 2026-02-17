# Create Survival Heatmap by Contract Type

Creates a heatmap showing survival probabilities across time for
different contract types.

## Usage

``` r
plot_survival_heatmap(
  survival_curves,
  time_grid = NULL,
  color_scale = "viridis",
  show_values = FALSE
)
```

## Arguments

- survival_curves:

  List output from estimate_contract_survival()

- time_grid:

  Numeric vector. Time points for heatmap (NULL for automatic)

- color_scale:

  Character. Color scale: "viridis", "heat", "cool"

- show_values:

  Logical. Display probability values in cells

## Value

A ggplot heatmap object
