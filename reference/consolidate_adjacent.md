# Consolidate Adjacent Employment Periods

Consolidates contiguous employment periods with no gap or unemployment
between them. Two employment periods are adjacent if the end of one is
immediately followed by the start of the next (no days between).
Unemployment periods act as barriers that prevent consolidation.

## Usage

``` r
consolidate_adjacent(data, variable_handling = "weight", engine = "v2")
```

## Arguments

- data:

  data.table with employment records. Must contain columns: `cf`,
  `inizio`, `fine`, `durata`. The `arco` column is used if present to
  identify employment vs unemployment periods.

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

data.table with adjacent employment periods consolidated. Includes all
original columns plus `n_periods_consolidated` indicating how many
periods were merged (1 means no consolidation occurred for that record).

## Details

**What makes periods "adjacent":**

Two employment periods are adjacent if:

- They belong to the same person (`cf`)

- They are consecutive in time (no gap days between them)

- Both are employment periods (`arco > 0` or missing)

- There is no unemployment period between them

**How unemployment acts as a barrier:**

Unemployment periods (`arco == 0`) prevent consolidation. For example,
if you have Employment-Unemployment-Employment, these will NOT be
consolidated even if the dates are adjacent. Use
[`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
if you want to bridge unemployment gaps.

**Difference from overlapping consolidation:**

- [`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md):
  Merges concurrent employment (same `over_id`)

- `consolidate_adjacent`: Merges sequential employment with no gap

**Aggregation rules:**

When consolidating periods, the function:

- Uses `min(inizio)` and `max(fine)` for date range

- Recalculates `durata` as the full span

- Uses weighted mode for qualitative variables (e.g., contract type)

- Uses weighted mean for quantitative variables (e.g., salary)

- Weights are based on the `durata` of each period

**Performance:**

Fully vectorized implementation with exceptional performance:

- Handles 10M+ employment records efficiently

- 9x faster than previous consolidation implementations (Phase 3)

- Phase 4 optimization: 1.2-3x additional speedup via single-period
  worker bypass

- Memory efficient: \< 1x input data size

- Base throughput: ~41,000 records/second (Phase 3)

- Optimized throughput: ~50,000-120,000 records/second (Phase 4, dataset
  dependent)

Phase 4 automatically skips consolidation for single-period workers (no
adjacent periods possible). Performance scales with percentage of
single-period workers:

- 20% singles: ~1.2x speedup

- 40% singles: ~1.4x speedup

- 50% singles: ~1.7x speedup

- 70% singles: ~2.9x speedup

**Composability:**

This function is designed to be chained with other consolidation
functions:

    data |>
      consolidate_overlapping() |>  # First merge concurrent
      consolidate_adjacent() |>     # Then merge adjacent
      consolidate_short_gaps(30)    # Finally bridge short gaps

The order matters: always consolidate overlapping employment first, then
adjacent periods, and finally bridge gaps if needed.

## See also

[`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md)
for concurrent employment consolidation

[`consolidate_by_employer`](https://gmontaletti.github.io/longworkR/reference/consolidate_by_employer.md)
for same-employer consolidation

[`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
for gap-bridging consolidation

[`consolidation_helpers`](https://gmontaletti.github.io/longworkR/reference/consolidation_helpers.md)
for internal aggregation functions

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic: Consolidate 3 consecutive employment periods
data <- data.table::data.table(
  cf = rep(1, 3),
  inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
  durata = c(31, 28, 31),
  arco = c(1, 1, 1)
)

result <- consolidate_adjacent(data)
nrow(result)  # 1 (all three periods merged)
result$n_periods_consolidated  # 3

# With gaps: periods separated by days won't consolidate
data_with_gap <- data.table::data.table(
  cf = rep(1, 3),
  inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01")),
  fine = as.Date(c("2023-01-31", "2023-02-28", "2023-04-30")),
  durata = c(31, 28, 30),
  arco = c(1, 1, 1)
)
# Periods 1-2 are adjacent (Jan 31 → Feb 1)
# Period 3 has a gap (Feb 28 → Apr 1 = 32 days)

result <- consolidate_adjacent(data_with_gap)
nrow(result)  # 2 (first two merged, third separate)

# With unemployment barrier
data_barrier <- data.table::data.table(
  cf = rep(1, 3),
  inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
  durata = c(31, 28, 31),
  arco = c(1, 0, 1)  # Middle period is unemployment
)

result <- consolidate_adjacent(data_barrier)
nrow(result)  # 3 (unemployment blocks consolidation)

# Chaining: after consolidate_overlapping()
data <- readRDS("data/sample.rds")
result <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()

cat("Original records:", nrow(data), "\n")
cat("After consolidation:", nrow(result), "\n")

# Integration with transition analysis
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()
transitions <- analyze_employment_transitions(consolidated)

# Edge case: empty data
empty_data <- data.table::data.table(
  cf = integer(),
  inizio = as.Date(character()),
  fine = as.Date(character()),
  durata = integer()
)
result_empty <- consolidate_adjacent(empty_data)  # Returns empty data.table

# Edge case: single record
single_record <- data[1]
result_single <- consolidate_adjacent(single_record)  # Returns as-is
} # }
```
