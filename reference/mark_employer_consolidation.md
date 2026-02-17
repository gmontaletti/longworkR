# Mark Records for Employer Consolidation (Lightweight)

**Lightweight Performance-Optimized Function**

Marks records that would be consolidated based on employer without
performing the actual consolidation. This function adds a single column
to the original dataset indicating which records would be consolidated
together, providing the same logic as consolidate_employment() but
avoiding expensive consolidation operations for performance-critical
applications.

Records are marked for consolidation when they meet all criteria:

- Same person (cf)

- Same employer (employer_var)

- Gap between contracts \<= min_lag days

## Usage

``` r
mark_employer_consolidation(
  data,
  employer_var,
  min_lag = 30,
  consolidation_col = "consolidation_group"
)
```

## Arguments

- data:

  The input data.table (modified by reference)

- employer_var:

  Name of the employer column

- min_lag:

  Minimum gap in days between contracts for same employer to consolidate
  (default 30)

- consolidation_col:

  Name of the new column to add (default "consolidation_group")

## Value

The original data.table with one additional column marking consolidation
groups. Records with the same consolidation_group value would be
consolidated together. Uses data.table reference semantics for maximum
performance.

## Details

**Algorithm:**

1.  Sort records by person (cf), employer, and start date (inizio)

2.  For each person-employer combination, check gaps between consecutive
    contracts

3.  If gap \<= min_lag, mark as same consolidation group

4.  If gap \> min_lag, start new consolidation group

5.  Create unique consolidation group IDs across all persons

**Performance Features:**

- Uses data.table reference semantics (:=) to avoid copying

- Single pass through data with vectorized operations

- Handles 14M+ records efficiently

- Memory-efficient group ID generation

**Use Cases:**

- Performance-critical applications needing consolidation info without
  actual consolidation

- Quality checks and data exploration before running expensive
  consolidation

- Custom consolidation workflows that need group identification first

- Memory-constrained environments where full consolidation is
  prohibitive

## Examples

``` r
if (FALSE) { # \dontrun{
# Load sample data
dt <- readRDS("data/sample.rds")

# Mark records for consolidation (modifies dt by reference)
mark_employer_consolidation(
  data = dt,
  employer_var = "datore",
  min_lag = 8,
  consolidation_col = "consol_group"
)

# Now dt has a new column 'consol_group'
# Records with the same consol_group value would consolidate together
dt[, .N, by = consol_group]  # Count records per consolidation group

# Identify which records would be consolidated
consolidation_candidates <- dt[, .N > 1, by = consol_group][V1 == TRUE]
print(paste("Consolidation groups:", nrow(consolidation_candidates)))

# Use custom column name
mark_employer_consolidation(dt, "employer_id", min_lag = 15, consolidation_col = "merge_groups")
} # }
```
