# Consolidate Employment Periods by Employer and Short Gaps (Combined)

Performs employer consolidation followed by short-gap bridging in a
single pass, avoiding the overhead of two independent function calls.
This is semantically equivalent to
`consolidate_by_employer(employer_var) |> consolidate_short_gaps(gap_max_gap_days)`
but eliminates one data copy, one sort, one single-period worker split,
and one recombination step, yielding a 15-25\\

## Usage

``` r
consolidate_employer_gaps(
  data,
  employer_var,
  employer_max_gap_days = 8,
  gap_max_gap_days = 8,
  variable_handling = "first",
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

- employer_max_gap_days:

  Numeric value specifying the maximum gap in days between consecutive
  contracts from the same employer to be consolidated (default: 8). Must
  be non-negative.

- gap_max_gap_days:

  Numeric value specifying the maximum gap in days to bridge during
  short-gap consolidation (default: 8). Must be non-negative.

- variable_handling:

  Character string specifying aggregation strategy for variables:
  `"first"` takes first non-NA value (default), `"weight"` uses weighted
  mean/mode.

- engine:

  Character string specifying the consolidation engine: `"v2"` (default)
  uses the collapse-native engine for maximum performance, `"v1"` uses
  the original data.table J-expression engine for backward
  compatibility.

## Value

data.table with consolidated employment periods. Includes all original
columns plus:

- `n_periods_consolidated`: Number of periods merged in the short-gap
  phase (matches sequential pipeline output)

- `non_working_days`: Total unemployment days bridged within each
  consolidated period

## Details

**Two-phase consolidation:**

The function internally performs two sequential aggregation passes on a
single data copy:

1.  **Phase A — Employer consolidation**: Merges consecutive employment
    periods with the same employer within `employer_max_gap_days`.
    Unemployment periods act as barriers that prevent consolidation.
    Identical to
    [`consolidate_by_employer`](https://gmontaletti.github.io/longworkR/reference/consolidate_by_employer.md).

2.  **Phase B — Short-gap bridging**: On the Phase A result, bridges
    remaining short gaps up to `gap_max_gap_days`. Long unemployment
    periods (\> threshold) act as barriers. Identical to
    [`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md).

**Equivalence guarantee:**

The output is identical to the sequential pipeline:


    data |>
      consolidate_by_employer(employer_var,
        max_gap_days = employer_max_gap_days,
        variable_handling = variable_handling,
        engine = engine) |>
      consolidate_short_gaps(
        max_gap_days = gap_max_gap_days,
        variable_handling = variable_handling,
        engine = engine)

**Performance advantage:**

Compared to the sequential pipeline, this function eliminates:

- One
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html)
  of the full dataset

- One [`setkey()`](https://rdrr.io/pkg/data.table/man/setkey.html) sort
  pass

- One single-period worker split and recombination

- One [`rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
  recombination

This yields approximately 15-25\\ pipeline on typical datasets.

## See also

[`consolidate_by_employer`](https://gmontaletti.github.io/longworkR/reference/consolidate_by_employer.md)
for standalone employer consolidation

[`consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
for standalone gap-bridging consolidation

[`consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md)
for concurrent employment consolidation

[`consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md)
for contiguous period consolidation

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
data <- readRDS("data/sample.rds")
result <- consolidate_employer_gaps(data, "datore",
  employer_max_gap_days = 8,
  gap_max_gap_days = 30
)

# Equivalent sequential pipeline (slower)
ref <- data |>
  consolidate_by_employer("datore", max_gap_days = 8,
    variable_handling = "first") |>
  consolidate_short_gaps(max_gap_days = 30,
    variable_handling = "first")

identical(result, ref)  # TRUE

# In a full consolidation chain
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_employer_gaps("datore",
    employer_max_gap_days = 8,
    gap_max_gap_days = 30)

cat("Original records:", nrow(data), "\n")
cat("After consolidation:", nrow(consolidated), "\n")
} # }
```
