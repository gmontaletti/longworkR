# Synthetic Sample Employment Data with Policy Events

An extended synthetic dataset containing longitudinal employment records
with simulated policy intervention events. Specifically designed for
demonstrating impact evaluation methods including
difference-in-differences (DiD), propensity score matching (PSM), and
regression discontinuity design (RDD).

## Usage

``` r
synthetic_sample
```

## Format

A data.table with 476,400 rows and 30 columns:

- id:

  Unique record identifier

- cf:

  Person identifier (codice fiscale anonymized)

- inizio:

  Contract start date (Date)

- fine:

  Contract end date (Date)

- arco:

  Concurrent employment indicator (logical)

- prior:

  Employment intensity (1 = full-time, 0-1 = part-time/other)

- over_id:

  Employment period identifier for consolidation

- durata:

  Contract duration in days

- stato:

  Employment state/status

- qualifica:

  Job qualification code

- ateco:

  Economic activity sector code (ATECO)

- ore:

  Working hours per week

- retribuzione:

  Salary/compensation amount

- COD_TIPOLOGIA_CONTRATTUALE:

  Contract type code (X.01.00 format)

- eta:

  Age at contract start

- sesso:

  Gender (M/F)

- istruzione:

  Education level

- datore:

  Employer identifier (anonymized)

- area:

  Geographic area code

- troncata:

  Truncation indicator for administrative censoring

- provincia:

  Province code

- did_attribute:

  Difference-in-differences attribute for matching

- did_distance:

  Distance metric for DiD matching

- did_match_quality:

  Quality score for DiD match

- event_start:

  Policy event start date

- event_end:

  Policy event end date

- pol_attribute:

  Policy evaluation attribute

- pol_distance:

  Distance metric for policy matching

- pol_match_quality:

  Quality score for policy match

- idpol:

  Policy identifier linking individuals to specific interventions

## Source

Synthetic data generated for impact evaluation demonstrations

## Details

This synthetic dataset extends the base sample with:

- Simulated policy intervention events (`event_start`, `event_end`)

- Policy identifiers (`idpol`) linking individuals to interventions

- Enhanced matching variables for causal inference

The data is specifically designed for:

- Difference-in-differences (DiD) analysis

- Propensity score matching (PSM)

- Regression discontinuity design (RDD)

- Comparative impact evaluation across multiple policies

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(synthetic_sample)

# Identify policy events
events <- identify_treatment_events(
  synthetic_sample,
  id_var = "cf",
  event_date_var = "event_start"
)

# Run DiD analysis
did_results <- estimate_impact_did(
  synthetic_sample,
  outcome_var = "durata",
  treatment_var = "idpol"
)
} # }
```
