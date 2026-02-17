# Quick Employment Data Visualization

Creates a quick overview visualization combining timeline and
distribution plots for rapid exploration of vecshift results.

## Usage

``` r
quick_employment_viz(
  data,
  n_people_timeline = 8,
  palette = "employment",
  use_bw = FALSE,
  title = "Vecshift Analysis Summary"
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- n_people_timeline:

  Integer. Number of people to show in timeline (default: 8)

- palette:

  Character. Color palette to use (default: "employment")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- title:

  Character. Overall title for the plot (default: "Vecshift Analysis
  Summary")

## Value

A ggplot2 object with combined visualizations

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick visualization
result <- vecshift(sample_data)
quick_employment_viz(result)

# Black and white version
quick_employment_viz(result, use_bw = TRUE)
} # }
```
