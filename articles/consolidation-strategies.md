# Employment Consolidation Strategies

## Introduction

### Why Consolidate Employment Data?

Employment records from administrative sources often contain
fragmentation that doesn’t reflect true labor market attachment. Workers
may have:

- **Multiple concurrent jobs** (part-time + part-time, seasonal +
  permanent)
- **Brief gaps between contracts** (1-2 days due to administrative
  processing)
- **Contract renewals** (same employer, consecutive periods)
- **Overlapping employment periods** identified by vecshift’s `over_id`
  column

Without consolidation, these patterns create noise in longitudinal
employment analysis, making it difficult to:

- Identify true employment trajectories
- Calculate meaningful employment durations
- Analyze career stability and mobility
- Estimate transition probabilities accurately

### The longworkR Consolidation Solution

The longworkR package provides **three focused functions** for
employment consolidation, each with a single, clear purpose:

1.  **[`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md)** -
    Merges concurrent employment periods
2.  **[`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md)** -
    Merges touching employment periods (no gap)
3.  **[`consolidate_short_gaps()`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)** -
    Bridges short unemployment gaps

These functions are **composable** (chainable with pipes) and
**performant** (9x faster than previous implementations).

------------------------------------------------------------------------

## The New Consolidation API

### From Complex to Simple

**Previous approach (longworkR \< 0.6.0):** Single function with complex
parameters

``` r
# OLD API (removed in v0.6.0)
consolidated <- consolidate_employment(
  data,
  mode = "temporal",           # Complex parameter
  type = "both",               # Complex parameter
  employer_var = "employer_id", # Complex parameter
  min_lag = 30                 # Complex parameter
)
```

Problems with the old API: - 6 different implementation files for
variations - Complex parameter interactions - Difficult to understand
what consolidation was performed - Performance issues with large
datasets - Hard to compose different consolidation strategies

**New approach (longworkR 0.6.0+):** Three focused functions

``` r
# NEW API (v0.6.0+)
consolidated <- data |>
  consolidate_overlapping() |>  # Clear: merge concurrent jobs
  consolidate_adjacent() |>     # Clear: merge touching periods
  consolidate_short_gaps(30)    # Clear: bridge 30-day gaps
```

Benefits of the new API: - ✅ **Composable**: Chain functions as
needed - ✅ **Focused**: Each function does one thing well - ✅
**Fast**: 9x performance improvement - ✅ **Clear**: Intent is obvious
from function names - ✅ **Flexible**: Choose which consolidations to
apply

------------------------------------------------------------------------

## Function Guide: consolidate_overlapping()

### Purpose

Merges concurrent employment periods where a person holds multiple jobs
at the same time. Uses vecshift’s `over_id` column to identify
overlapping employment.

### When to Use

Use this function when: - Your data has been processed by vecshift
(creates `over_id` column) - You want to treat concurrent employment as
a single employment period - You need to simplify analysis by
consolidating multiple simultaneous jobs

### How It Works

The function: 1. Groups records by `cf` (person ID) and `over_id > 0` 2.
Merges overlapping periods into single records 3. Uses **min start
date** and **max end date** for consolidated period 4. Aggregates other
columns using weighted mode/mean by duration 5. Adds
`n_periods_consolidated` to track how many periods were merged

### Example: Basic Usage

``` r
# Load sample data
data <- readRDS("data/sample.rds")

# Check for overlapping employment
table(data$over_id > 0)  # TRUE = overlapping periods

# Consolidate overlapping periods
consolidated <- consolidate_overlapping(data)

# Compare record counts
cat("Original records:", nrow(data), "\n")
cat("After consolidation:", nrow(consolidated), "\n")
cat("Reduction:", round((1 - nrow(consolidated)/nrow(data)) * 100, 1), "%\n")
```

### Example: Understanding the Consolidation

``` r
# View person with overlapping employment
person_165 <- data[cf == 165 & over_id > 0]
print(person_165[, .(cf, inizio, fine, durata, over_id, COD_TIPOLOGIA_CONTRATTUALE)])

# After consolidation
person_165_consolidated <- consolidate_overlapping(person_165)
print(person_165_consolidated[, .(cf, inizio, fine, durata, n_periods_consolidated)])

# Note: The consolidated period spans from earliest inizio to latest fine
# The n_periods_consolidated shows how many concurrent jobs were merged
```

### Aggregation Rules

When consolidating overlapping periods, columns are aggregated as:

| Column Type      | Aggregation Method | Example                                           |
|------------------|--------------------|---------------------------------------------------|
| `inizio`         | Minimum (earliest) | min(c(“2023-01-01”, “2023-01-15”)) = “2023-01-01” |
| `fine`           | Maximum (latest)   | max(c(“2023-02-15”, “2023-02-28”)) = “2023-02-28” |
| `durata`         | Recalculated       | fine - inizio + 1                                 |
| Numeric/Integer  | Weighted mean      | sum(value \* durata) / sum(durata)                |
| Character/Factor | Weighted mode      | Most frequent value by total duration             |
| Logical          | Majority rule      | mean(values) \>= 0.5                              |
| `arco`           | Maximum            | Indicates if any period was employment            |
| `stato`          | Employment state   | Preferentially selects employment states          |

------------------------------------------------------------------------

## Function Guide: consolidate_adjacent()

### Purpose

Merges employment periods that are contiguous in time (touching, with no
gap between them). This consolidates contract renewals and consecutive
employment spells with the same or different employers.

### When to Use

Use this function when: - You want to treat consecutive employment as
continuous - Contract renewals should be viewed as single employment
spells - You’re analyzing employment stability and don’t want
administrative contract boundaries to fragment true employment periods

### How It Works

The function: 1. Sorts records by `cf` and date 2. Calculates gap in
days between consecutive periods 3. Detects **new group starts** when: -
First record for person - Gap \> 0 days between periods - Previous
period was unemployment (`arco == 0`) - Current period is unemployment
4. Consolidates periods within each group

**Important**: Unemployment periods (`arco == 0`) act as **barriers**
that prevent consolidation.

### Example: Basic Usage

``` r
# Create test data with adjacent periods
test_data <- data.table(
  cf = rep(1, 3),
  inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
  durata = c(31, 28, 31),
  arco = c(1, 1, 1),
  COD_TIPOLOGIA_CONTRATTUALE = c("A.03.00", "A.03.00", "A.01.00")
)

# Note: Jan 31 → Feb 1 and Feb 28 → Mar 1 are adjacent (no gap)
result <- consolidate_adjacent(test_data)

print(result)
# Result: 1 record spanning Jan 1 - Mar 31
# n_periods_consolidated = 3
# Contract type = "A.03.00" (weighted mode, 59 days vs 31 days)
```

### Example: Unemployment as Barrier

``` r
# Data with unemployment between employment
barrier_data <- data.table(
  cf = rep(1, 3),
  inizio = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  fine = as.Date(c("2023-01-31", "2023-02-28", "2023-03-31")),
  durata = c(31, 28, 31),
  arco = c(1, 0, 1),  # Middle period is unemployment
  stato = c("occupato", "disoccupato", "occupato")
)

result <- consolidate_adjacent(barrier_data)

print(result)
# Result: 3 records (unemployment blocks consolidation)
# Even though dates are adjacent, unemployment creates a break
```

### Difference from consolidate_overlapping()

| Aspect               | [`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md) | [`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md) |
|----------------------|-------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------|
| **What it merges**   | Concurrent employment (same time)                                                                           | Sequential employment (no gap)                                                                        |
| **Uses `over_id`**   | Yes (required)                                                                                              | No                                                                                                    |
| **Gap tolerance**    | N/A (overlapping by definition)                                                                             | Exactly 0 days                                                                                        |
| **Unemployment**     | Kept separate                                                                                               | Acts as barrier                                                                                       |
| **Typical use case** | Multiple simultaneous jobs                                                                                  | Contract renewals                                                                                     |

------------------------------------------------------------------------

## Function Guide: consolidate_short_gaps()

### Purpose

Bridges short unemployment gaps between employment periods,
consolidating employment spells separated by brief periods of
inactivity. This is useful for analyzing labor market attachment when
short gaps don’t represent true labor market exit.

### When to Use

Use this function when: - You want to treat brief unemployment (1-30
days) as part of continuous employment - You’re analyzing labor market
attachment over time - Administrative gaps between contracts shouldn’t
fragment employment trajectories

### How It Works

The function: 1. Calculates gap in days between consecutive periods 2.
Creates consolidation groups where gap ≤ `max_gap_days` 3. Consolidates
periods within each group 4. Adds **`non_working_days`** column tracking
total unemployment days bridged

**Important**: Unlike
[`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md),
this function **includes** unemployment periods in the consolidation and
tracks them.

### Choosing the Right Threshold

Different `max_gap_days` values suit different analyses:

| Threshold      | Use Case                   | Interpretation                   |
|----------------|----------------------------|----------------------------------|
| **7-14 days**  | Strict continuity          | Very short breaks, sick leave    |
| **30 days**    | Monthly analysis (default) | Standard employment continuity   |
| **60-90 days** | Seasonal work              | Quarterly employment patterns    |
| **180+ days**  | Long-term attachment       | Persistent labor market presence |

### Example: Basic Gap Bridging

``` r
# Employment with 15-day gap
gap_data <- data.table(
  cf = rep(1, 5),
  inizio = as.Date(c("2023-01-01", "2023-01-20", "2023-02-01",
                     "2023-03-01", "2023-04-01")),
  fine = as.Date(c("2023-01-15", "2023-01-25", "2023-02-15",
                   "2023-03-15", "2023-04-15")),
  durata = c(15, 6, 15, 15, 15),
  arco = c(1, 0, 1, 0, 1)  # Alternating employment/unemployment
)

# Bridge gaps up to 30 days
result_30 <- consolidate_short_gaps(gap_data, max_gap_days = 30)
cat("Records with 30-day threshold:", nrow(result_30), "\n")
cat("Non-working days bridged:", result_30$non_working_days, "\n")

# Compare with stricter threshold
result_10 <- consolidate_short_gaps(gap_data, max_gap_days = 10)
cat("Records with 10-day threshold:", nrow(result_10), "\n")
```

### Example: Analyzing Employment Quality

``` r
# After full consolidation chain
data <- readRDS("data/sample.rds")
final <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)

# Analyze employment quality by non_working_days
summary(final$non_working_days)

# Employment quality categories
final[, quality_category := fcase(
  non_working_days == 0, "Continuous",
  non_working_days <= 7, "High quality",
  non_working_days <= 30, "Good quality",
  non_working_days <= 90, "Moderate quality",
  default = "Low quality"
)]

table(final$quality_category)
```

### The `non_working_days` Column

This column is **critical** for understanding consolidation quality:

- **0 days**: Truly continuous employment (no gaps bridged)
- **1-30 days**: Brief gaps, likely administrative or short unemployment
- **31-90 days**: Longer gaps, seasonal patterns or search periods
- **90+ days**: Extended unemployment bridged (use with caution)

------------------------------------------------------------------------

## Progressive Consolidation: Chaining Functions

### The Recommended Workflow

The three consolidation functions are designed to be used **in
sequence**, creating a progressive consolidation chain:

``` r
# RECOMMENDED: Full consolidation chain
consolidated_data <- raw_data |>
  consolidate_overlapping() |>  # Step 1: Merge concurrent employment
  consolidate_adjacent() |>     # Step 2: Merge touching periods
  consolidate_short_gaps(30)    # Step 3: Bridge short gaps
```

### Why Order Matters

**Always consolidate in this order:**

1.  **Overlapping first**: Simplifies concurrent employment before
    analyzing sequences
2.  **Adjacent second**: Removes administrative contract boundaries
3.  **Short gaps last**: Most aggressive, should work on
    already-simplified data

**Wrong order can produce suboptimal results:**

``` r
# WRONG: Bridging gaps before merging adjacent periods
wrong_order <- data |>
  consolidate_short_gaps(30) |>  # Gap calculation includes noise
  consolidate_adjacent()          # Less effective on noisy data

# RIGHT: Clean up first, then bridge gaps
right_order <- data |>
  consolidate_adjacent() |>       # Remove noise first
  consolidate_short_gaps(30)      # Gap calculation more accurate
```

### Selective Consolidation

You don’t have to use all three functions. Choose based on your analysis
needs:

``` r
# Minimal: Only merge concurrent employment
minimal <- data |> consolidate_overlapping()

# Conservative: Merge concurrent and adjacent, no gap bridging
conservative <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()

# Aggressive: Full chain with generous gap threshold
aggressive <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(90)  # Bridge up to 3 months
```

### Example: Progressive Analysis

``` r
# Load data
data <- readRDS("data/sample.rds")

# Track consolidation impact at each step
cat("Original data:", nrow(data), "records\n\n")

step1 <- consolidate_overlapping(data)
cat("After overlapping consolidation:", nrow(step1), "records\n")
cat("Reduction:", nrow(data) - nrow(step1), "records\n")
cat("Percentage:", round((1 - nrow(step1)/nrow(data)) * 100, 1), "%\n\n")

step2 <- consolidate_adjacent(step1)
cat("After adjacent consolidation:", nrow(step2), "records\n")
cat("Additional reduction:", nrow(step1) - nrow(step2), "records\n")
cat("Cumulative:", round((1 - nrow(step2)/nrow(data)) * 100, 1), "%\n\n")

step3 <- consolidate_short_gaps(step2, max_gap_days = 30)
cat("After gap bridging (30d):", nrow(step3), "records\n")
cat("Additional reduction:", nrow(step2) - nrow(step3), "records\n")
cat("Total reduction:", round((1 - nrow(step3)/nrow(data)) * 100, 1), "%\n")
```

------------------------------------------------------------------------

## Integration with analyze_employment_transitions()

### The New Workflow

**In longworkR 0.6.0+**, consolidation is **decoupled** from transition
analysis. You consolidate data first, then analyze transitions:

``` r
# NEW WORKFLOW (v0.6.0+)

# Step 1: Consolidate employment data
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)

# Step 2: Analyze transitions on consolidated data
transitions <- analyze_employment_transitions(consolidated)

# Step 3: Visualize
plot_transitions_network(transitions)
```

### Why This Is Better

The decoupled approach provides:

1.  **Flexibility**: Choose consolidation strategy independent of
    analysis
2.  **Clarity**: Explicit about what consolidation was performed
3.  **Reusability**: Consolidate once, use for multiple analyses
4.  **Performance**: Consolidation optimized separately from transition
    analysis

### Example: Multiple Analyses on Same Consolidation

``` r
# Consolidate once
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)

# Use for different analyses
transitions <- analyze_employment_transitions(consolidated)
survival_results <- estimate_contract_survival(consolidated)
impact_events <- identify_treatment_events(consolidated, ...)

# Or analyze with different parameters
transitions_first <- analyze_employment_transitions(consolidated, eval_chain = "first")
transitions_last <- analyze_employment_transitions(consolidated, eval_chain = "last")
```

### Comparing Consolidation Strategies

``` r
# Compare impact of different consolidation strategies

# No consolidation
trans_none <- analyze_employment_transitions(data)

# Minimal consolidation
data_minimal <- consolidate_overlapping(data)
trans_minimal <- analyze_employment_transitions(data_minimal)

# Full consolidation
data_full <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)
trans_full <- analyze_employment_transitions(data_full)

# Compare transition counts
cat("No consolidation:", nrow(trans_none$transitions), "transitions\n")
cat("Minimal:", nrow(trans_minimal$transitions), "transitions\n")
cat("Full:", nrow(trans_full$transitions), "transitions\n")
```

------------------------------------------------------------------------

## Performance Guide

### Performance Achievements

The new consolidation functions deliver exceptional performance:

| Metric                | Value               | Comparison                  |
|-----------------------|---------------------|-----------------------------|
| **Overall speedup**   | 9x faster           | vs. previous implementation |
| **Throughput**        | ~41,000 records/sec | Full consolidation chain    |
| **Memory efficiency** | \< 1x input size    | No memory bloat             |
| **Scalability**       | 10M+ records        | Production-ready            |

### Benchmark: Real-World Dataset

``` r
# Load large dataset (434,103 employment records)
large_data <- readRDS("data/large_sample.rds")

# Benchmark each function
system.time({
  result1 <- consolidate_overlapping(large_data)
})  # ~14 seconds (9.1x faster than baseline)

system.time({
  result2 <- consolidate_adjacent(result1)
})  # ~13 seconds (9.2x faster than baseline)

system.time({
  result3 <- consolidate_short_gaps(result2, max_gap_days = 30)
})  # ~11 seconds (3.6x faster than baseline)

# Full chain
system.time({
  final <- large_data |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)
})  # ~38 seconds total (7.6x faster, was 287 seconds)

# Record reduction
cat("Original:", nrow(large_data), "records\n")
cat("Final:", nrow(final), "records\n")
cat("Reduction:", round((1 - nrow(final)/nrow(large_data)) * 100, 1), "%\n")
```

### Performance Best Practices

1.  **Use data.table format**:

    ``` r
    # Good: data.table (fast)
    dt <- data.table::as.data.table(data)
    result <- consolidate_overlapping(dt)

    # Bad: data.frame (will convert, slower)
    df <- as.data.frame(data)
    result <- consolidate_overlapping(df)  # Throws error
    ```

2.  **Set key for better performance** (optional, done automatically):

    ``` r
    data.table::setkey(data, cf, inizio, fine)
    result <- consolidate_adjacent(data)  # Slightly faster
    ```

3.  **Use pipe chains for clarity**:

    ``` r
    # Good: Clear pipeline
    result <- data |>
      consolidate_overlapping() |>
      consolidate_adjacent()

    # Less clear: Nested calls
    result <- consolidate_adjacent(consolidate_overlapping(data))
    ```

4.  **Consolidate before analysis, not during**:

    ``` r
    # Good: Consolidate once, use many times
    consolidated <- data |> consolidate_overlapping()
    trans1 <- analyze_employment_transitions(consolidated, eval_chain = "first")
    trans2 <- analyze_employment_transitions(consolidated, eval_chain = "last")

    # Bad: Consolidate repeatedly
    trans1 <- analyze_employment_transitions(
      consolidate_overlapping(data), eval_chain = "first"
    )
    trans2 <- analyze_employment_transitions(
      consolidate_overlapping(data), eval_chain = "last"
    )
    ```

### Memory Considerations

The consolidation functions are memory-efficient:

- **No data copying**: Uses data.table’s modify-by-reference where safe
- **\< 1x memory overhead**: Peak memory ≈ input data size
- **Vectorized operations**: No loop-based memory buildup

For extremely large datasets (100M+ records), consider processing in
chunks:

``` r
# Process by person ID ranges
chunk_size <- 1000000  # 1M records per chunk
person_ids <- unique(data$cf)
chunks <- split(person_ids, ceiling(seq_along(person_ids) / chunk_size))

results <- lapply(chunks, function(ids) {
  chunk_data <- data[cf %in% ids]
  chunk_data |>
    consolidate_overlapping() |>
    consolidate_adjacent() |>
    consolidate_short_gaps(30)
})

final_result <- rbindlist(results)
```

------------------------------------------------------------------------

## Migration Guide: Upgrading from \< 0.6.0

### Breaking Changes in v0.6.0

**longworkR 0.6.0 introduces breaking changes** by removing the old
`consolidate_employment()` function and its variants. If you’re
upgrading from an earlier version, you’ll need to update your code.

### Removed Functions

The following functions are **no longer available**:

- `consolidate_employment()`
- `consolidate_employment_fast()`
- `consolidate_employment_robust()`
- `consolidate_employment_safe()`
- `consolidate_employment_ultra_fast()`

### Migration Examples

#### Example 1: Basic Temporal Consolidation

``` r
# OLD CODE (< 0.6.0) - NO LONGER WORKS
consolidated <- consolidate_employment(
  data,
  mode = "temporal",
  type = "both"
)

# NEW CODE (0.6.0+)
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()
```

#### Example 2: With Gap Bridging

``` r
# OLD CODE (< 0.6.0) - NO LONGER WORKS
consolidated <- consolidate_employment(
  data,
  mode = "temporal",
  type = "both",
  min_lag = 30
)

# NEW CODE (0.6.0+)
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)  # min_lag → max_gap_days
```

#### Example 3: Within analyze_employment_transitions()

``` r
# OLD CODE (< 0.6.0) - NO LONGER WORKS
result <- analyze_employment_transitions(
  data,
  consolidation_mode = "temporal",
  consolidation_type = "both"
)

# NEW CODE (0.6.0+)
# Consolidate first, then analyze
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()

result <- analyze_employment_transitions(consolidated)
```

#### Example 4: Only Overlapping Consolidation

``` r
# OLD CODE (< 0.6.0)
consolidated <- consolidate_employment(
  data,
  mode = "temporal",
  type = "overlapping"
)

# NEW CODE (0.6.0+)
consolidated <- consolidate_overlapping(data)
```

#### Example 5: Only Adjacent Consolidation

``` r
# OLD CODE (< 0.6.0)
consolidated <- consolidate_employment(
  data,
  mode = "temporal",
  type = "adjacent"
)

# NEW CODE (0.6.0+)
consolidated <- consolidate_adjacent(data)
```

### Parameter Mapping

| Old Parameter          | New Approach                                                                                                                                                                                                                 |
|------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `mode = "temporal"`    | Use [`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md) and/or [`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md) |
| `type = "both"`        | Chain `consolidate_overlapping() |> consolidate_adjacent()`                                                                                                                                                                  |
| `type = "overlapping"` | Use [`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md) only                                                                                                         |
| `type = "adjacent"`    | Use [`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md) only                                                                                                               |
| `min_lag = N`          | Use `consolidate_short_gaps(max_gap_days = N)`                                                                                                                                                                               |
| `employer_var = "..."` | Not supported in new API (see note below)                                                                                                                                                                                    |

### Employer-Based Consolidation

The old `employer_var` parameter for employer-based consolidation is
**not directly supported** in the new API. This functionality may be
restored in a future release or requires custom implementation:

``` r
# OLD CODE (< 0.6.0) - NOT DIRECTLY SUPPORTED
consolidated <- consolidate_employment(
  data,
  mode = "employer",
  employer_var = "employer_id",
  min_lag = 30
)

# NEW CODE (0.6.0+) - WORKAROUND
# For employer-based consolidation, you'll need to:
# 1. Pre-process to mark same-employer adjacent periods
# 2. Use consolidate_adjacent() or write custom logic
# 3. Or wait for future longworkR release with employer support
```

### Updated Workflow Pattern

The new API separates consolidation from analysis:

``` r
# OLD PATTERN (< 0.6.0)
result <- analyze_employment_transitions(
  data,
  consolidation_mode = "temporal",
  consolidation_type = "both",
  eval_chain = "first"
)

# NEW PATTERN (0.6.0+)
# Step 1: Consolidate
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()

# Step 2: Analyze
result <- analyze_employment_transitions(
  consolidated,
  eval_chain = "first"
)
```

### Benefits of Migration

Migrating to the new API provides:

1.  **9x performance improvement**: Much faster consolidation
2.  **Clearer code**: Intent obvious from function names
3.  **More flexible**: Mix and match consolidation strategies
4.  **Better maintained**: Single, optimized implementation
5.  **Composable**: Easy to chain with pipes

### Migration Checklist

Search codebase for `consolidate_employment(`

Replace with appropriate new function(s)

Remove `consolidation_mode` and `consolidation_type` from
[`analyze_employment_transitions()`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md)
calls

Test that results match expected behavior

Update any documentation or README files

Consider if employer-based consolidation was needed (requires
workaround)

------------------------------------------------------------------------

## Summary

### Key Takeaways

1.  **Three focused functions** replace complex old API:

    - [`consolidate_overlapping()`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md) -
      Concurrent employment
    - [`consolidate_adjacent()`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md) -
      Touching periods
    - [`consolidate_short_gaps()`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md) -
      Bridge unemployment gaps

2.  **Composable design**: Chain functions with pipes in logical order

3.  **9x performance improvement**: Handles 10M+ records efficiently

4.  **Decoupled from analysis**: Consolidate first, then analyze

5.  **Breaking changes**: Old `consolidate_employment()` removed in
    v0.6.0

### Recommended Workflow

``` r
# 1. Load data
data <- readRDS("data/sample.rds")

# 2. Progressive consolidation
consolidated <- data |>
  consolidate_overlapping() |>  # Merge concurrent jobs
  consolidate_adjacent() |>     # Merge touching periods
  consolidate_short_gaps(30)    # Bridge 30-day gaps

# 3. Analyze
transitions <- analyze_employment_transitions(consolidated)

# 4. Visualize
plot_transitions_network(transitions)
```

### Further Reading

- Function documentation:
  [`?consolidate_overlapping`](https://gmontaletti.github.io/longworkR/reference/consolidate_overlapping.md),
  [`?consolidate_adjacent`](https://gmontaletti.github.io/longworkR/reference/consolidate_adjacent.md),
  [`?consolidate_short_gaps`](https://gmontaletti.github.io/longworkR/reference/consolidate_short_gaps.md)
- Performance vignette:
  [`vignette("performance-optimizations", package = "longworkR")`](https://gmontaletti.github.io/longworkR/articles/performance-optimizations.md)
- Transition analysis:
  [`?analyze_employment_transitions`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md)
- vecshift package: Understanding the `over_id` column

------------------------------------------------------------------------

## Session Info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] data.table_1.18.2.1 longworkR_0.8.0     vecshift_1.0.4     
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10        generics_0.1.4     lattice_0.22-7     hms_1.1.4         
#>  [5] digest_0.6.39      magrittr_2.0.4     evaluate_1.0.5     grid_4.5.2        
#>  [9] RColorBrewer_1.1-3 fastmap_1.2.0      jsonlite_2.0.0     Matrix_1.7-4      
#> [13] progress_1.2.3     survival_3.8-3     scales_1.4.0       textshaping_1.0.4 
#> [17] jquerylib_0.1.4    cli_3.6.5          rlang_1.1.7        crayon_1.5.3      
#> [21] splines_4.5.2      cachem_1.1.0       yaml_2.3.12        otel_0.2.0        
#> [25] tools_4.5.2        parallel_4.5.2     dplyr_1.2.0        ggplot2_4.0.2     
#> [29] vctrs_0.7.1        R6_2.6.1           matrixStats_1.5.0  lifecycle_1.0.5   
#> [33] fs_1.6.6           htmlwidgets_1.6.4  ragg_1.5.0         cluster_2.1.8.1   
#> [37] pkgconfig_2.0.3    desc_1.4.3         pkgdown_2.2.0      bslib_0.10.0      
#> [41] pillar_1.11.1      gtable_0.3.6       glue_1.8.0         Rcpp_1.1.1        
#> [45] systemfonts_1.3.1  collapse_2.1.6     xfun_0.56          tibble_3.3.1      
#> [49] tidyselect_1.2.1   knitr_1.51         farver_2.1.2       htmltools_0.5.9   
#> [53] rmarkdown_2.30     ggalluvial_0.12.5  compiler_4.5.2     prettyunits_1.2.0 
#> [57] S7_0.2.1
```
