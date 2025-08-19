# longworkR

Longitudinal Employment Analytics for vecshift Data

## Overview

`longworkR` provides advanced analytics and visualization tools for longitudinal employment data processed by the `vecshift` package. It includes survival analysis, impact evaluation, network analysis, and comprehensive visualization capabilities.

## Installation

```r
# Install vecshift first (required dependency)
install.packages("vecshift")  # or devtools::install_local("../vecshift")

# Install longworkR
devtools::install_local(".")  # from longworkR directory
```

## Features

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

```r
library(vecshift)
library(longworkR)

# Process data with vecshift
data <- vecshift(employment_records)

# Analyze transitions
transitions <- analyze_employment_transitions(
  data,
  consolidation_type = "both"
)

# Create visualizations
plot_transitions_network(transitions)

# Survival analysis
survival_results <- estimate_contract_survival(data)
plot_survival_comparison(survival_results)

# Impact evaluation
treatment_events <- identify_treatment_events(
  data,
  treatment_conditions = list("contract_type == 'permanent'")
)

did_results <- difference_in_differences(
  treatment_events,
  outcome_vars = c("employment_rate", "wage_growth")
)
```

## Key Functions

### Analysis Functions
- `analyze_employment_transitions()` - Transition analysis with consolidation
- `analyze_consolidated_periods()` - Period analysis using over_id
- `create_consolidated_transition_matrix()` - Transition matrices

### Impact Evaluation
- `identify_treatment_events()` - Event identification
- `propensity_score_matching()` - PSM implementation
- `difference_in_differences()` - DiD estimation
- `event_study_design()` - Event study analysis

### Visualization
- `plot_transitions_network()` - Network diagrams
- `plot_survival_comparison()` - Survival curves
- `plot_impact_summary()` - Impact evaluation results
- `plot_interactive_transitions()` - Interactive g6r visualizations

## Dependencies

- **Required**: vecshift, data.table, ggplot2, survival
- **Visualization**: ggraph, tidygraph, viridis, RColorBrewer
- **Optional**: g6r, networkD3, plotly, gganimate
- **Impact Evaluation**: augsynth, MatchIt, rdrobust, lfe, fixest

## Documentation

For detailed documentation, see:
- Package vignettes: `browseVignettes("longworkR")`
- Function help: `?function_name`
- Reference materials: `../reference/longworkR/`

## License

MIT + file LICENSE

## Contributing

Please report issues and submit pull requests on the project repository.