# Consolidate Overlapping Employment Periods

Merges concurrent employment periods (multiple jobs held at the same
time). Identifies overlapping employment using the `over_id` column from
vecshift processing and consolidates them into single periods with
aggregated attributes.

## Usage

``` r
consolidate_overlapping(data, variable_handling = "weight", engine = "v2")
```

## Arguments

- data:

  A data.table containing employment periods processed by vecshift, with
  columns: `cf` (person ID), `inizio` (start date), `fine` (end date),
  `durata` (duration), `over_id` (overlapping period identifier), and
  optionally `arco` (employment indicator).

- variable_handling:

  Character string specifying aggregation strategy for variables:
  `"weight"` uses weighted mean/mode (default), `"first"` takes first
  non-NA value

- engine:

  Character string specifying the consolidation engine: `"v2"` (default)
  uses the collapse-native engine for maximum performance, `"v1"` uses
  the original data.table J-expression engine for backward
  compatibility.

## Value

A data.table with consolidated employment periods, where:

- Periods with the same `over_id > 0` are merged into single periods

- `inizio` is the earliest start date in the group

- `fine` is the latest end date in the group

- `durata` is recalculated as `fine - inizio + 1`

- `n_periods_consolidated` indicates how many periods were merged

- Qualitative variables use weighted mode (most frequent by duration)

- Quantitative variables use weighted mean

- Original column types are preserved

## Details

This function is designed for employment data where concurrent jobs are
identified by vecshift's `over_id` column. All periods sharing the same
`cf` and `over_id > 0` are considered overlapping and consolidated.

**Consolidation rules**:

- Employment periods with `over_id > 0`: grouped by `cf` and `over_id`

- Other periods (unemployment, single jobs): kept as-is (unique group
  per record)

- If `arco` column is missing, it's created (1 when `over_id > 0`, 0
  otherwise)

**Aggregation by column type**:

- **Dates** (`inizio`, `fine`): min/max across group

- **Duration** (`durata`): recomputed as `fine - inizio + 1`

- **Numeric/Integer**: weighted mean (preserves integer type)

- **Character/Factor**: weighted mode (sum durations by value, pick max)

- **Logical**: majority rule (mean \>= 0.5)

**Special columns**:

- `arco`: maximum value in group

- `over_id`: first non-zero value (or first if all zero)

- `stato`: preferentially selects employment states when `arco > 0`

**Performance:**

This function is fully vectorized and optimized for performance:

- Handles 10M+ employment records efficiently

- 9x faster than previous consolidation implementations (Phase 3)

- Phase 4 optimization: 1.2-3x additional speedup via single-period
  worker bypass

- Memory efficient: \< 1x input data size

- Base throughput: ~41,000 records/second (Phase 3)

- Optimized throughput: ~50,000-120,000 records/second (Phase 4, dataset
  dependent)

Phase 4 automatically skips consolidation for single-period workers with
over_id == 0 (no overlapping possible). Performance scales with
percentage of such workers.

**Composability:**

Designed to be the first step in a consolidation chain. After merging
concurrent employment, you typically want to merge adjacent periods and
optionally bridge short gaps:

    data |>
      consolidate_overlapping() |>  # Step 1: Merge concurrent jobs
      consolidate_adjacent() |>     # Step 2: Merge touching periods
      consolidate_short_gaps(30)    # Step 3: Bridge short gaps

## See also

[`consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md)
to merge touching employment periods
[`consolidate_by_employer`](https://gmontaletti.github.io/longworkR/reference/consolidate_by_employer.md)
to merge same-employer periods
[`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
to bridge short unemployment gaps

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample data
data <- readRDS("data/sample.rds")

# Consolidate overlapping employment periods
consolidated <- consolidate_overlapping(data)

# Check consolidation results
cat("Original records:", nrow(data), "\n")
cat("After consolidation:", nrow(consolidated), "\n")
cat("Periods consolidated:",
    sum(consolidated$n_periods_consolidated > 1, na.rm = TRUE), "\n")

# View a person with overlapping employment
person_data <- data[cf == 165 & over_id > 0]
person_consolidated <- consolidate_overlapping(person_data)

# Chain with other consolidation functions
fully_consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()

# Integration with analyze_employment_transitions()
# Pre-consolidate data before transition analysis
consolidated <- consolidate_overlapping(data)
transitions <- analyze_employment_transitions(consolidated)

# Performance with large datasets
large_data <- readRDS("data/large_sample.rds")  # 500K records
system.time({
  result <- consolidate_overlapping(large_data)
})  # Completes in seconds, not minutes
} # }
```
