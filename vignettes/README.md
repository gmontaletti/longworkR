# Vignettes Directory

This directory contains documentation and examples for the longworkR package integration workflow between employment metrics calculation and impact evaluation methods.

## Files

### 1. `impact-metrics-integration.Rmd`
**Comprehensive Integration Workflow Vignette**

A complete tutorial demonstrating the three-step integration process:
1. Calculate employment metrics using `calculate_comprehensive_impact_metrics()`
2. Bridge to impact analysis using `prepare_metrics_for_impact_analysis()`
3. Run causal inference using methods like `difference_in_differences()`

**Topics covered:**
- Basic workflow integration
- Advanced scenarios (event studies, multiple treatments)
- Data validation and troubleshooting
- Visualization and reporting
- Best practices

**To build:** Use `devtools::build_vignettes()` or `knitr::knit()` to generate HTML output.

### 2. `integration-examples.R`
**Practical Integration Examples**

Five comprehensive examples demonstrating different real-world scenarios:

1. **Basic Workforce Development Program** (`example_1_workforce_development()`)
   - Standard DiD analysis of job training program
   - Complete workflow from data to results

2. **Event Study Analysis** (`example_2_event_study()`)
   - Dynamic treatment effects over time
   - Policy intervention with multiple time periods

3. **Multiple Treatment Groups** (`example_3_multiple_treatments()`)
   - Complex treatment scenarios (counseling, training, both)
   - Sub-group analysis methods

4. **Propensity Score Matching** (`example_4_propensity_matching()`)
   - Selection bias correction
   - Rich covariate matching

5. **Comprehensive Policy Evaluation** (`example_5_comprehensive_evaluation()`)
   - Full evaluation pipeline
   - Multiple methods and robustness checks

**Usage:**
```r
source("vignettes/integration-examples.R")
result1 <- example_1_workforce_development()
all_results <- run_all_integration_examples()
```

## Integration Workflow Overview

The longworkR package provides a seamless integration between employment metrics and causal inference:

```
Raw Employment Data
        ↓
calculate_comprehensive_impact_metrics()
        ↓  
Employment Metrics (pre/post periods)
        ↓
prepare_metrics_for_impact_analysis()
        ↓
Panel Data for Causal Analysis
        ↓
difference_in_differences() / matching / etc.
        ↓
Treatment Effect Estimates
```

## Key Functions

- `calculate_comprehensive_impact_metrics()`: Compute employment stability, quality, complexity, and transition metrics
- `prepare_metrics_for_impact_analysis()`: Bridge function that transforms metrics into panel format
- `difference_in_differences()`: DiD estimation with employment metrics as outcomes
- `validate_integration_setup()`: Data validation for the integration workflow

## Getting Started

1. **Read the vignette first**: `vignette("impact-metrics-integration", package = "longworkR")`
2. **Try the basic example**: Run `example_1_workforce_development()` 
3. **Explore advanced scenarios**: Use examples 2-5 for specific use cases
4. **Apply to your data**: Follow the patterns with your own employment data

## Requirements

- R packages: longworkR, data.table, ggplot2
- Sample employment data (provided by longworkR package)
- Treatment assignment data (user-defined based on research design)

For questions or issues with the integration workflow, see the package documentation or contact the maintainers.