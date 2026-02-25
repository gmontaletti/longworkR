# Consolidate Employment Periods by Employer

Consolidates consecutive employment periods with the same employer,
merging contracts separated by short gaps into single employment spells.
Two periods are consolidated if they belong to the same person, the same
employer, and the gap between them does not exceed `max_gap_days`.
Unemployment periods act as barriers that prevent consolidation.

## Usage

``` r
consolidate_by_employer(
  data,
  employer_var,
  max_gap_days = 8,
  variable_handling = "weight",
  engine = "v2"
)
```

## Arguments

- data:

  data.table with employment records. Must contain columns: `cf`,
  `inizio`, `fine`, `durata`. The `arco` column is used if present to
  identify employment vs unemployment periods.

- employer_var:

  Character string specifying the column name containing employer
  identifiers (e.g., `"datore"`, `"employer_id"`).

- max_gap_days:

  Numeric value specifying the maximum gap in days between consecutive
  contracts from the same employer to be consolidated (default: 8). Must
  be non-negative.

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

data.table with employer-consolidated employment periods. Includes all
original columns plus `n_periods_consolidated` indicating how many
periods were merged (1 means no consolidation occurred for that record).

## Details

**What makes periods consolidatable:**

Two employment periods are consolidated if:

- They belong to the same person (`cf`)

- They share the same employer (`employer_var`)

- The gap between them is at most `max_gap_days` days

- Both are employment periods (`arco > 0` or missing)

- There is no unemployment period between them

**How unemployment acts as a barrier:**

Unemployment periods (`arco == 0`) prevent consolidation. For example,
if a worker has Employment(Employer A)-Unemployment-Employment(Employer
A), these will NOT be consolidated even if the employer is the same and
the gap is within threshold.

**Difference from other consolidation functions:**

- [`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md):
  Merges concurrent employment (same `over_id`)

- [`consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md):
  Merges sequential employment with no gap, regardless of employer

- `consolidate_by_employer`: Merges sequential employment by the same
  employer within a gap threshold

- [`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md):
  Bridges short unemployment gaps regardless of employer

**Aggregation rules:**

When consolidating periods, the function delegates aggregation to the
shared consolidation engine (`.consolidate_groups`):

- Uses `min(inizio)` and `max(fine)` for date range

- Recalculates `durata` as the full span

- Uses weighted mode for qualitative variables (e.g., contract type)

- Uses weighted mean for quantitative variables (e.g., salary)

- Weights are based on the `durata` of each period

**Performance:**

Fully vectorized implementation with Phase 4 single-period worker bypass
optimization. Workers with only one period are excluded from
consolidation logic since no same-employer merging is possible.

**Composability:**

This function is designed to be chained with other consolidation
functions:

    data |>
      consolidate_overlapping() |>       # First merge concurrent
      consolidate_by_employer("datore") |> # Then merge same-employer
      consolidate_adjacent() |>           # Then merge adjacent
      consolidate_short_gaps(30)          # Finally bridge short gaps

The recommended position for employer consolidation is after overlapping
consolidation and before adjacent/gap consolidation.

## See also

[`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md)
for concurrent employment consolidation

[`consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md)
for contiguous period consolidation

[`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
for gap-bridging consolidation

[`consolidation_helpers`](https://gmontaletti.github.io/longworkR/reference/consolidation_helpers.md)
for internal aggregation functions

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic: Consolidate contracts with the same employer
data <- data.table::data.table(
  cf = rep(1, 4),
  inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-07-01", "2023-04-01")),
  fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30", "2023-06-30")),
  durata = c(90, 91, 92, 91),
  arco = c(1, 1, 1, 1),
  datore = c("A", "A", "A", "B")
)

result <- consolidate_by_employer(data, employer_var = "datore")
nrow(result)  # 2 (employer A periods merged, employer B separate)

# With gap threshold: only merge if gap <= max_gap_days
data2 <- data.table::data.table(
  cf = rep(1, 3),
  inizio = as.Date(c("2023-01-01", "2023-04-05", "2023-07-01")),
  fine = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
  durata = c(90, 87, 92),
  arco = c(1, 1, 1),
  datore = c("A", "A", "A")
)

# Gap between first two: 5 days (merged with default max_gap_days=8)
result1 <- consolidate_by_employer(data2, "datore", max_gap_days = 8)
nrow(result1)  # All merged into 1 record

# Gap between first two: 5 days (NOT merged with max_gap_days=3)
result2 <- consolidate_by_employer(data2, "datore", max_gap_days = 3)
nrow(result2)  # 2 records (first separate, last two merged)

# Chaining in a pipeline
data <- readRDS("data/sample.rds")
result <- data |>
  consolidate_overlapping() |>
  consolidate_by_employer("datore") |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)

cat("Original records:", nrow(data), "\n")
cat("After consolidation:", nrow(result), "\n")
} # }
```
