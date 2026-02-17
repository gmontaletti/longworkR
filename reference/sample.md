# Sample Employment Data

A sample dataset containing longitudinal employment records processed by
the vecshift package. This dataset is suitable for testing and
demonstrating longworkR functions for career trajectory analysis,
survival analysis, and impact evaluation.

## Usage

``` r
sample
```

## Format

A data.table with 434,103 rows and 28 columns:

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

- COMUNE_LAVORATORE:

  Municipality code for worker

- stato:

  Employment state/status

- did_attribute:

  Difference-in-differences attribute for matching

- did_distance:

  Distance metric for DiD matching

- did_match_quality:

  Quality score for DiD match

- pol_attribute:

  Policy evaluation attribute

- pol_distance:

  Distance metric for policy matching

- pol_match_quality:

  Quality score for policy match

## Source

Synthetic data based on Italian administrative employment records

## Details

This dataset has been processed by the vecshift package and includes:

- Employment period consolidation via `over_id`

- Concurrent employment detection via `arco`

- Employment intensity adjustments via `prior`

- Impact evaluation matching variables (DiD and policy attributes)

The data is suitable for:

- Career trajectory analysis

- Contract survival analysis

- Employment transition network analysis

- Impact evaluation (DiD, PSM, RDD methods)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(sample)

# Basic exploration
str(sample)
summary(sample)

# Analyze employment transitions
transitions <- analyze_employment_transitions(sample)

# Estimate contract survival
survival_results <- estimate_contract_survival_optimized(sample)
} # }
```
