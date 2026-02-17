# Consolidate Employment Periods with Short Gaps

Consolidates employment periods separated by unemployment gaps up to a
specified threshold. This bridges short unemployment periods between
jobs, useful for analyzing labor market attachment and employment
stability.

## Usage

``` r
consolidate_short_gaps(data, max_gap_days = 8, variable_handling = "first")
```

## Arguments

- data:

  data.table with employment records. Must contain columns: `cf`,
  `inizio`, `fine`, `durata`. The `arco` column is used if present to
  identify employment vs unemployment periods.

- max_gap_days:

  Maximum gap in days to consolidate across (default: 8). Common values:

  - 7-8 days: Very short breaks, weekly consolidation (default)

  - 14 days: Bi-weekly consolidation

  - 30 days: Monthly gaps

  - 90 days: Quarterly employment analysis

- variable_handling:

  Character string specifying aggregation strategy for variables:
  `"first"` takes first non-NA value (default), `"weight"` uses weighted
  mean/mode

## Value

data.table with periods consolidated across short gaps. Includes all
original columns plus:

- `n_periods_consolidated`: Number of periods merged

- `non_working_days`: Total unemployment days within consolidated period

## Details

**How gaps are bridged:**

This function consolidates employment periods when they are separated by
gaps of `max_gap_days` or fewer. **Important:** Unemployment periods
with duration \> `max_gap_days` act as consolidation barriers. Short
unemployment periods (\<= threshold) can be bridged. For example, with
`max_gap_days = 30`:

- Employment (Jan 1-15) → Gap (10 days) → Employment (Jan 26-31) =
  CONSOLIDATED

- Employment (Jan 1-15) → Gap (50 days) → Employment (Mar 6-31) = NOT
  consolidated

- Employment (Jan 1-15) → Unemployment (20 days) → Employment =
  CONSOLIDATED

- Employment (Jan 1-15) → Unemployment (40 days) → Employment = NOT
  consolidated

**What non_working_days represents:**

The `non_working_days` column tracks the total unemployment days that
were bridged within each consolidated period. This allows you to analyze
the "quality" of employment continuity even after consolidation.

**Use cases for different thresholds:**

- 7-14 days:

  Very short breaks, sick leave, brief unemployment

- 30 days:

  Monthly analysis, standard employment continuity (default)

- 60-90 days:

  Seasonal work, quarterly analysis

- 180+ days:

  Long-term labor market attachment

**Difference from other consolidation functions:**

- [`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md):
  Merges concurrent employment (same time)

- [`consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md):
  Merges touching periods (no gap, employment only)

- `consolidate_short_gaps`: Bridges short gaps and brief unemployment
  periods; long unemployment (\> threshold) acts as barrier

**Aggregation rules:**

When consolidating periods, the function:

- Uses `min(inizio)` and `max(fine)` for date range

- Recalculates `durata` as the full span (including gaps)

- Counts total unemployment days as `non_working_days`

- Uses weighted mode for qualitative variables

- Uses weighted mean for quantitative variables

- Weights are based on employment durations (unemployment excluded)

**Performance:**

Fully vectorized implementation with exceptional performance:

- Handles 10M+ employment records efficiently

- **Phase 5 optimization: 10-15x faster than Phase 3 baseline**

- Phase 4: Single-period worker bypass (1.2-3x additional speedup)

- Phase 3: Vectorized consolidation (9x faster than baseline)

- Memory efficient: \< 1x input data size

- Optimized throughput: ~150,000-200,000 records/second

**Optimization strategy:**

- Phase 4 skips gap bridging for single-period workers (no gaps to
  bridge)

- Phase 5 optimizes .consolidate_groups() by:

  - Splitting single-record consolidation groups (no aggregation needed)

  - Simplified first-value aggregation (avoids expensive weighted mode)

  - Results in 10-15x speedup with variable_handling = "first" (default)

Performance scales with percentage of single-period workers and
consolidation group sizes. The function efficiently processes
large-scale employment data.

**Composability:**

This function is designed to be the final step in a consolidation chain:

    data |>
      consolidate_overlapping() |>  # First merge concurrent
      consolidate_adjacent() |>     # Then merge adjacent
      consolidate_short_gaps(30)    # Finally bridge short gaps

Always run this function last, after overlapping and adjacent
consolidation. This ensures the most accurate gap calculations and
consolidation results.

## See also

[`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md)
for concurrent employment consolidation

[`consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md)
for contiguous period consolidation

[`consolidate_by_employer`](https://gmontaletti.github.io/longworkR/reference/consolidate_by_employer.md)
for same-employer consolidation

[`consolidation_helpers`](https://gmontaletti.github.io/longworkR/reference/consolidation_helpers.md)
for internal aggregation functions

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic: 15-day gap with max_gap=30
data <- data.table::data.table(
  cf = rep(1, 5),
  inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01",
                     "2023-03-01", "2023-04-01")),
  fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15",
                   "2023-03-15", "2023-04-15")),
  durata = c(15, 6, 15, 15, 15),
  arco = c(1, 0, 1, 0, 1)
)

result30 <- consolidate_short_gaps(data, max_gap_days = 30)
nrow(result30)  # 1 (all periods consolidated)
result30$non_working_days  # 21 days (6 + 15 unemployment periods)

# Threshold test: same data with different max_gap
result10 <- consolidate_short_gaps(data, max_gap_days = 10)
nrow(result10)  # 2 (splits at 15-day gap)

# Multiple gaps
data_multi <- data.table::data.table(
  cf = rep(1, 7),
  inizio = as.Date(c("2023-01-01", "2023-01-08", "2023-02-01",
                     "2023-02-08", "2023-03-01", "2023-03-08",
                     "2023-05-01")),
  fine = as.Date(c("2023-01-07", "2023-01-31", "2023-02-07",
                   "2023-02-28", "2023-03-07", "2023-03-31",
                   "2023-05-31")),
  durata = c(7, 24, 7, 21, 7, 24, 31),
  arco = c(1, 1, 1, 1, 1, 1, 1)
)
# Gaps: 0, 0, 0, 0, 0, 30 days

result <- consolidate_short_gaps(data_multi, max_gap_days = 30)
nrow(result)  # 2 (breaks at 30-day gap before May)

# Full chain: overlapping → adjacent → short_gaps
data <- readRDS("data/sample.rds")
final <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)

cat("Original records:", nrow(data), "\n")
cat("Final consolidated:", nrow(final), "\n")
cat("Total reduction:", round((1 - nrow(final)/nrow(data)) * 100, 1), "%\n")

# Analyze non_working_days
summary(final$non_working_days)
hist(final$non_working_days, main = "Distribution of unemployment days bridged")

# Integration with analyze_employment_transitions()
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)
transitions <- analyze_employment_transitions(consolidated)

# Comparison of different thresholds
result_strict <- data |> consolidate_short_gaps(7)   # 1 week
result_medium <- data |> consolidate_short_gaps(30)  # 1 month
result_lenient <- data |> consolidate_short_gaps(90) # 3 months

cat("Strict (7d):", nrow(result_strict), "periods\n")
cat("Medium (30d):", nrow(result_medium), "periods\n")
cat("Lenient (90d):", nrow(result_lenient), "periods\n")
} # }
```
