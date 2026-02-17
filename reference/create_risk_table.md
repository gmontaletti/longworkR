# Create Risk Table for Survival Plot

Creates a risk table showing the number at risk at specified time points
for each contract type.

## Usage

``` r
create_risk_table(survival_curves, contract_types = NULL, time_points = NULL)
```

## Arguments

- survival_curves:

  List output from estimate_contract_survival()

- contract_types:

  Character vector. Contract types to include

- time_points:

  Numeric vector. Time points for risk table (NULL for automatic)

## Value

A ggplot object containing the risk table
