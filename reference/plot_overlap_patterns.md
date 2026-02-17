# Plot Overlapping Employment Patterns

Visualizes patterns of overlapping employment (multiple concurrent
jobs), showing the intensity and duration of overlaps across individuals
or time.

## Usage

``` r
plot_overlap_patterns(
  data,
  person_col = "cf",
  time_col = "inizio",
  end_col = "fine",
  arco_col = "arco",
  status_col = "stato",
  plot_type = "heatmap",
  facet_by = NULL,
  n_people = 20,
  min_overlap = 2,
  palette = "main",
  use_bw = FALSE,
  alpha = 0.8
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- person_col:

  Character. Column name for person identifier (default: "cf")

- time_col:

  Character. Column name for time periods (default: "inizio")

- end_col:

  Character. Column name for period end dates (default: "fine")

- arco_col:

  Character. Column name for overlap count (default: "arco")

- status_col:

  Character. Column name for employment status (default: "stato")

- plot_type:

  Character. Type of plot: "heatmap", "timeline", "distribution"
  (default: "heatmap")

- facet_by:

  Character. Column to use for faceting (default: NULL)

- n_people:

  Integer. Maximum number of people to show (default: 20)

- min_overlap:

  Integer. Minimum overlap level to show (default: 2)

- palette:

  Character. Color palette to use (default: "main")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- alpha:

  Numeric. Transparency (default: 0.8)

## Value

A ggplot2 object showing overlap patterns

## Examples

``` r
if (FALSE) { # \dontrun{
# Heatmap of overlap intensity
plot_overlap_patterns(data, plot_type = "heatmap")

# Timeline view of overlaps
plot_overlap_patterns(data, plot_type = "timeline", n_people = 10)

# Distribution of overlap levels
plot_overlap_patterns(data, plot_type = "distribution")
} # }
```
