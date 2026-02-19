# longworkR

[![pkgdown](https://github.com/gmontaletti/longworkR/actions/workflows/pkgdown.yml/badge.svg)](https://gmontaletti.github.io/longworkR/)

Longitudinal Employment Analytics for vecshift Data

## Overview

`longworkR` provides advanced analytics and visualization tools for
longitudinal employment data processed by the `vecshift` package. It
includes survival analysis, impact evaluation, network analysis, and
comprehensive visualization capabilities.

## Installation

``` r
# Install vecshift first (required dependency)
install.packages("vecshift")  # or devtools::install_local("../vecshift")

# Install longworkR
devtools::install_local(".")  # from longworkR directory
```

## Features

### Employment Consolidation (New in v0.6.0)

- **Progressive consolidation**: Three composable functions
- **9x performance improvement**: Handles 10M+ records efficiently
- **Focused functions**:
  [`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md),
  [`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md),
  [`consolidate_short_gaps()`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
- **Type-safe**: Preserves all column types
- See
  [`vignette("consolidation-strategies")`](https://gmontaletti.github.io/longworkR/articles/consolidation-strategies.md)
  for comprehensive guide

### Survival Analysis

- Contract survival curves and hazard functions
- Comparative survival analysis across groups
- Median survival time calculations
- Risk tables and survival visualizations

### Impact Evaluation

- **Difference-in-Differences (DiD)**: With parallel trends testing
- **Propensity Score Matching (PSM)**: Multiple algorithms available
- **Event Study Design**: Dynamic treatment effects
- **Regression Discontinuity (RDD)**: Threshold-based interventions
- **Synthetic Control Method**: For policy evaluation

### Network Analysis

- Employment transition networks
- Consolidated period analysis using over_id
- Transition matrices and flow diagrams
- Network metrics and centrality measures

### Visualization

- **ggraph**: Static network visualizations
- **g6r**: Interactive network exploration
- **Custom themes**: Consistent visual styling
- **Accessibility**: Colorblind-friendly palettes

## Quick Start

``` r
library(vecshift)
library(longworkR)

# Process data with vecshift
data <- vecshift(employment_records)

# Consolidate employment periods (new in v0.6.0)
consolidated <- data |>
  consolidate_overlapping() |>  # Merge concurrent jobs
  consolidate_adjacent() |>     # Merge touching periods
  consolidate_short_gaps(30)    # Bridge 30-day gaps

# Analyze transitions
transitions <- analyze_employment_transitions(consolidated)

# Create visualizations
plot_transitions_network(transitions)

# Survival analysis
survival_results <- estimate_contract_survival(consolidated)
plot_survival_comparison(survival_results)

# Impact evaluation
treatment_events <- identify_treatment_events(
  consolidated,
  treatment_conditions = list("contract_type == 'permanent'")
)

did_results <- difference_in_differences(
  treatment_events,
  outcome_vars = c("employment_rate", "wage_growth")
)
```

## Key Functions

### Consolidation Functions (New in v0.6.0)

- [`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md) -
  Merge concurrent employment periods
- [`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md) -
  Merge touching employment periods
- [`consolidate_short_gaps()`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md) -
  Bridge short unemployment gaps

### Analysis Functions

- [`analyze_employment_transitions()`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md) -
  Transition analysis on consolidated data
- [`analyze_consolidated_periods()`](https://gmontaletti.github.io/longworkR/reference/analyze_consolidated_periods.md) -
  Period analysis using over_id
- [`create_consolidated_transition_matrix()`](https://gmontaletti.github.io/longworkR/reference/create_consolidated_transition_matrix.md) -
  Transition matrices

### Impact Evaluation

- [`identify_treatment_events()`](https://gmontaletti.github.io/longworkR/reference/identify_treatment_events.md) -
  Event identification
- [`propensity_score_matching()`](https://gmontaletti.github.io/longworkR/reference/propensity_score_matching.md) -
  PSM implementation
- [`difference_in_differences()`](https://gmontaletti.github.io/longworkR/reference/difference_in_differences.md) -
  DiD estimation
- [`event_study_design()`](https://gmontaletti.github.io/longworkR/reference/event_study_design.md) -
  Event study analysis

### Visualization

- [`plot_transitions_network()`](https://gmontaletti.github.io/longworkR/reference/plot_transitions_network.md) -
  Network diagrams
- [`plot_survival_comparison()`](https://gmontaletti.github.io/longworkR/reference/plot_survival_comparison.md) -
  Survival curves
- [`plot_impact_summary()`](https://gmontaletti.github.io/longworkR/reference/plot_impact_summary.md) -
  Impact evaluation results
- [`plot_interactive_transitions()`](https://gmontaletti.github.io/longworkR/reference/plot_interactive_transitions.md) -
  Interactive g6r visualizations

## Dependencies

- **Required**: vecshift, data.table, ggplot2, survival
- **Visualization**: ggraph, tidygraph, viridis, RColorBrewer
- **Optional**: g6r, networkD3, plotly, gganimate
- **Impact Evaluation**: augsynth, MatchIt, rdrobust, lfe, fixest

## Documentation

For detailed documentation, see: - Package vignettes:
`browseVignettes("longworkR")` - Function help: `?function_name` -
Reference materials: `../reference/longworkR/`

## Citation

To cite longworkR in publications, please use:

Montaletti, G. (2025). *longworkR: Longitudinal Employment Analytics for
vecshift Data* (Version 0.8.1) \[R package\].
<https://github.com/gmontaletti/longworkR>

BibTeX entry:

``` bibtex
@software{montaletti2025longworkr,
  author = {Montaletti, Giampaolo},
  title = {longworkR: Longitudinal Employment Analytics for vecshift Data},
  version = {0.8.1},
  year = {2025},
  url = {https://github.com/gmontaletti/longworkR}
}
```

## License

MIT + file LICENSE

## Contributing

Please report issues and submit pull requests on the project repository.
