# Visualize Career Cluster Profiles

Creates comprehensive visualizations of career trajectory clusters
showing key metrics, cluster sizes, and comparative profiles across
segments.

## Usage

``` r
plot_cluster_profiles(
  cluster_result,
  metrics = c("employment_rate", "career_advancement_index",
    "employment_stability_index"),
  plot_type = "radar",
  language = "en",
  color_palette = "Set2",
  title = NULL,
  theme_base = theme_minimal()
)
```

## Arguments

- cluster_result:

  List. Output from cluster_career_trajectories() function

- metrics:

  Character vector. Metrics to visualize. Options: "employment_rate",
  "career_advancement_index", "contract_quality_score",
  "employment_stability_index", "career_complexity_index",
  "avg_employment_spell", "job_turnover_rate", "all". Default:
  c("employment_rate", "career_advancement_index",
  "employment_stability_index")

- plot_type:

  Character. Type of plot: "radar" (spider/radar chart), "bar" (grouped
  bars), "heatmap" (cluster × metric heatmap), "all" (multiple plots).
  Default: "radar"

- language:

  Character. Label language: "en" (English), "it" (Italian), "both".
  Default: "en"

- color_palette:

  Character. Color palette name or vector of colors. Default: "Set2"

- title:

  Character. Plot title. Default: auto-generated

- theme_base:

  ggplot2 theme. Base theme for plots. Default: theme_minimal()

## Value

A ggplot2 object or list of ggplot2 objects (if plot_type = "all")

## Details

**Plot Types:**

- **radar**: Spider/radar chart showing cluster profiles across metrics
  (best for 3-6 metrics)

- **bar**: Grouped bar chart comparing clusters on each metric

- **heatmap**: Heatmap showing cluster × metric matrix with color
  intensity

- **all**: Returns a list with all three plot types

**Metric Normalization:** All metrics are normalized to 0-1 scale for
fair comparison across different units.

## See also

[`cluster_career_trajectories`](https://gmontaletti.github.io/longworkR/reference/cluster_career_trajectories.md)
for creating career clusters

## Examples

``` r
if (FALSE) { # \dontrun{
# Cluster careers
career_metrics <- calculate_comprehensive_career_metrics(employment_data)
clusters <- cluster_career_trajectories(career_metrics)

# Radar chart of cluster profiles
plot_cluster_profiles(
  clusters,
  metrics = c("employment_rate", "career_advancement_index",
              "employment_stability_index", "contract_quality_score"),
  plot_type = "radar",
  language = "en"
)

# Bar chart with Italian labels
plot_cluster_profiles(
  clusters,
  metrics = "all",
  plot_type = "bar",
  language = "it"
)

# Heatmap visualization
plot_cluster_profiles(
  clusters,
  plot_type = "heatmap",
  color_palette = "RdYlGn"
)

# Generate all plot types
all_plots <- plot_cluster_profiles(clusters, plot_type = "all")
print(all_plots$radar)
print(all_plots$bar)
print(all_plots$heatmap)
} # }
```
