# Convert Integer Columns to Numeric (Optimized)

Efficiently converts all integer columns in a data.table to numeric type
to prevent type conflicts during consolidation operations. Uses
vectorized operations for maximum performance.

## Usage

``` r
.convert_types_optimized(data, modify_in_place = FALSE)
```

## Arguments

- data:

  data.table object to process

- modify_in_place:

  Logical; if TRUE, modifies data in place using :=. If FALSE (default),
  creates a copy first to preserve original data.

## Value

data.table with integer columns converted to numeric

## Details

This optimization addresses memory and performance issues caused by:

- Multiple redundant type checking loops across the codebase

- Type conflicts during data.table aggregation operations

- Unnecessary data copies for type conversions

The function uses vectorized sapply() to identify integer columns in a
single pass, then batch-converts them to numeric using data.table's
efficient := operator. This approach is significantly faster than
iterating through columns individually, especially for wide datasets.

Memory efficiency:

- With modify_in_place=TRUE: Zero additional memory allocation

- With modify_in_place=FALSE: Single copy() operation

- Replaces 3+ redundant copies in .apply_temporal_consolidation()

- Eliminates repeated type checking loops (lines 778-825, 4047-4058)
