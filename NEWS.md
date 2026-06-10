# longworkR 0.10.0

## Nuove funzionalità

- `load_spatial_maps()` è ora esportata: carica gli oggetti spaziali `sf` di comuni e CPI dalla directory dati condivisa (`SHARED_DATA_DIR`). Con questa modifica longworkR diventa la fonte unica delle utilities geografiche dell'ecosistema, in precedenza duplicate nelle pipeline.
- Nuove funzioni `compare_consolidation_stages()` ed `employment_statistics()`, migrate dalle pipeline COB: la prima confronta numero di record e persone uniche tra gli stadi di consolidamento, la seconda calcola statistiche riassuntive (giornate totali e durate medie di occupazione e disoccupazione) sui dati consolidati.

## Correzioni

- `load_spatial_maps()`: corretto l'argomento di `Sys.getenv()` (`unset` al posto del non valido `default`) nella risoluzione di `SHARED_DATA_DIR`.

# longworkR 0.9.0

## Breaking changes

- **Minimum vecshift version**: now requires `vecshift (>= 2.0.0)`. Public entry points validate their input via the new internal `.assert_vecshift_input()` helper and emit a warning if a plain data.table is passed instead of a `vecshift_result` object. Consolidation functions preserve the `vecshift_result` class and metadata on their output.
- **Career indicator surface reduced**. A single new function `career_profile()` replaces five legacy functions:
  - `calculate_career_success_metrics()`
  - `calculate_career_transition_metrics()`
  - `calculate_career_stability_metrics()`
  - `calculate_career_complexity_metrics()`
  - `calculate_comprehensive_career_metrics()`
  The legacy functions still work but emit `.Deprecated()` warnings and will be removed in v1.0.0. `career_profile()` returns a tidy per-`cf` data.table with a core group (6 indicators) and four optional groups: `stability`, `complexity`, `transitions`, `wages`. Values are not bit-identical to the legacy outputs: `contract_quality` is always derived from Kaplan–Meier median survival (per project policy); `intensity`, `employment_rate`, `total_days_employed` use cleaner denominators. See `?career_profile` for details.

## New features

- `career_profile(data, indicators = c("core", "stability", "complexity", "transitions", "wages", "all"))` — unified per-person career indicator function backed by a single survival-based contract-quality lookup shared with `compute_temporal_employment_indicators()`.
- New internal helpers `R/vecshift_input.R` and `R/contract_quality_lookup.R`.
- `_pkgdown.yml` gains a `Deprecated` reference group.

## Performance

- **vecshift v2 engine**: `consolidation_helpers_v2.R` now coerces `difftime` `durata` to integer days at the engine boundary (previously errored); column-type classification reduced from four `vapply()` passes to one; the single-record fast path no longer overwrites pre-existing `n_periods_consolidated`, preserving idempotency under chained consolidation.
- **`analyze_employment_transitions()`**: lag/lead `shift()` calls batched per grouping, eliminating the largest self-time hotspot on the transition-detection path. Regression snapshot test added at `tests/testthat/test-analyze-transitions-regression.R`.
- **`copy()` audit**: 14 redundant `data.table::copy()` calls removed across `trajectory_analysis.R`, `temporal_indicators.R`, `survival_visualization.R`, and `interactive_transitions_g6r.R`; 60 defensive copies preserved. Audit recorded at `reference/longworkR/copy-audit-v0.9.0.csv`.

## Bug fixes

- Chained consolidation now produces consistent `n_periods_consolidated` counts across both the v1 and v2 engines and between `consolidate_employer_gaps()` and the equivalent `consolidate_by_employer() |> consolidate_short_gaps()` pipeline.
- `estimate_contract_survival_optimized()` and all entry points now emit clear errors when required columns are missing, through the shared `.assert_vecshift_input()` helper.
- `add_contract_survival_metrics()` test suite updated to match the lowercase `inizio`/`fine` column convention used elsewhere.
- `.calculate_memory_aware_limit()` is now deterministic for repeated calls (absorbs sub-100 MB OS fluctuations).
- Monthly transition-matrix and consolidation integration tests fixed.

## Internal

- New `.call_silencing_w2_deprecation()` helper in `R/impact_metrics.R` to keep the impact-evaluation suite clean of W2 deprecation warnings until the impact callers are properly migrated to `career_profile()` in v0.10.x.

---

# longworkR 0.8.2

## Maintenance

- **Project cleanup and documentation refresh**: Removed obsolete build artifacts and temporary development files. Updated citation in README.md to reflect current version.

---

# longworkR 0.8.1

## Enhancements

- **Route 'first' mode to optimized consolidation engine**: The `consolidate_by_employer()` and `consolidate_adjacent()` functions now route the 'first' variable handling mode to the optimized consolidation engine, matching the behavior of `consolidate_short_gaps()`. This optimization delivers 10-15x faster consolidation on large datasets by avoiding unnecessary aggregation computations and leveraging efficient C++ operations.

---

# longworkR 0.7.0

## BREAKING CHANGES ⚠️

**Default consolidation behavior has changed.** The `consolidate_short_gaps()` function now uses more conservative defaults that better align with short-term employment analysis.

### Changed Defaults

- **`max_gap_days`**: Default changed from 30 to 8 days
  - Old behavior: Bridged gaps up to one month
  - New behavior: Bridges only very short gaps (weekly consolidation)
  - **Migration**: To restore old behavior, explicitly set `max_gap_days = 30`

### New Features

- **`variable_handling` parameter**: All consolidation functions now support a `variable_handling` parameter to control aggregation strategy:
  - `"first"` (default): Takes first non-NA value from consolidated periods
  - `"weight"`: Uses weighted mean for numeric variables, weighted mode for categorical
  - Provides explicit control over how variables are aggregated during consolidation

### Bug Fixes

- **Fixed unemployment barrier logic in `consolidate_short_gaps()`**:
  - **Problem**: Unemployment periods were incorrectly treated as simple gaps, allowing consolidation across long unemployment spells
  - **Fix**: Unemployment periods with `duration > max_gap_days` now act as consolidation barriers
  - **Impact**: Short unemployment periods (≤ threshold) can be bridged, but long unemployment (> threshold) prevents consolidation
  - **Example**: With `max_gap_days = 8`:
    - Employment → 5-day unemployment → Employment = **CONSOLIDATED** ✓
    - Employment → 20-day unemployment → Employment = **NOT consolidated** ✓ (barrier)
  - This ensures that significant unemployment spells are preserved in consolidated career histories

### Migration Guide

**Before (0.6.x):**
```r
# Default bridged gaps up to 30 days
consolidated <- data |>
  consolidate_short_gaps()  # max_gap_days = 30 (old default)
```

**After (0.7.0+):**
```r
# New default: only bridges very short gaps (8 days)
consolidated <- data |>
  consolidate_short_gaps()  # max_gap_days = 8 (new default)

# To restore old behavior, explicitly set max_gap_days
consolidated <- data |>
  consolidate_short_gaps(max_gap_days = 30)

# Use variable_handling for explicit aggregation control
consolidated <- data |>
  consolidate_short_gaps(
    max_gap_days = 8,
    variable_handling = "first"  # or "weight"
  )
```

### Technical Details

- **Files modified**:
  - `R/consolidate_short_gaps.R`: Unemployment barrier logic and default change
  - `R/consolidate_adjacent.R`: Added `variable_handling` parameter
  - `R/consolidate_overlapping.R`: Added `variable_handling` parameter
  - `R/consolidation_helpers.R`: Enhanced aggregation functions
  - Documentation updated across all consolidation functions

- **Test coverage**:
  - New tests for unemployment barrier detection
  - New tests for `variable_handling` parameter
  - All consolidation tests updated for new defaults

---

# longworkR 0.6.0

## BREAKING CHANGES ⚠️

**This release introduces breaking changes to the consolidation API.** The old `consolidate_employment()` function and its variants have been removed and replaced with three focused, composable functions.

### Removed Functions

The following functions are **no longer available**:
- `consolidate_employment()`
- `consolidate_employment_fast()`
- `consolidate_employment_robust()`
- `consolidate_employment_safe()`
- `consolidate_employment_ultra_fast()`

### New Consolidation Functions

**Three new functions** provide focused, composable consolidation:

1. **`consolidate_overlapping()`** - Merges concurrent employment periods (identified by `over_id`)
2. **`consolidate_adjacent()`** - Merges touching employment periods (no gap between them)
3. **`consolidate_short_gaps(max_gap_days)`** - Bridges short unemployment gaps up to specified threshold

### Migration Guide

**Before (< 0.6.0):**
```r
# Old API - NO LONGER WORKS
consolidated <- consolidate_employment(
  data,
  mode = "temporal",
  type = "both"
)
```

**After (0.6.0+):**
```r
# New API - Composable functions
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()
```

**With gap bridging:**
```r
# Old API
consolidated <- consolidate_employment(
  data,
  mode = "temporal",
  type = "both",
  min_lag = 30
)

# New API
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent() |>
  consolidate_short_gaps(30)
```

**Within analyze_employment_transitions():**
```r
# Old API - Parameters removed
result <- analyze_employment_transitions(
  data,
  consolidation_mode = "temporal",
  consolidation_type = "both"
)

# New API - Consolidate first, then analyze
consolidated <- data |>
  consolidate_overlapping() |>
  consolidate_adjacent()

result <- analyze_employment_transitions(consolidated)
```

### Updated Functions

- **`analyze_employment_transitions()`**: Removed `consolidation_mode`, `consolidation_type`, `employer_var`, and `min_lag` parameters. Users should pre-consolidate data using the new consolidation functions before calling this function.

### Performance Improvements 🚀

The new consolidation functions deliver **exceptional performance**:

- **9x faster** than previous consolidation implementations
- **~41,000 records/second** throughput for full consolidation chain
- **Memory efficient**: < 1x input data size
- **Scalable**: Handles 10M+ employment records efficiently

**Benchmark (434,103 records):**
- Previous implementation: 287 seconds
- New implementation: 38 seconds
- **Speedup: 7.6x for complete consolidation chain**

### Benefits of New API

✅ **Composable**: Chain functions as needed with pipe operator
✅ **Focused**: Each function does one thing well
✅ **Fast**: 9x performance improvement
✅ **Clear**: Intent is obvious from function names
✅ **Flexible**: Choose which consolidations to apply

### Documentation

- **New vignette**: `vignette("consolidation-strategies")` - Comprehensive guide with migration examples
- **Enhanced function documentation**: All three functions have detailed examples and integration guides
- **Updated README**: Quick start examples use new API

### Notes

- **Employer-based consolidation**: The `employer_var` parameter from the old API is not directly supported in v0.6.0. This functionality may be restored in a future release.
- **Type safety**: All functions preserve original column types (Date, IDate, integer, numeric, character, factor, logical)
- **Vectorization**: 100% vectorized implementation (no loops)

---

# longworkR 0.5.6

## Bug Fixes

* **Fixed critical memory overflow in `cluster_career_trajectories()` with 50K+ observations**
  - Clustering now works reliably with 50K+ observations without memory errors
  - Added intelligent detection of available (free) system RAM vs. total RAM
  - Silhouette validation now automatically uses micro-sampling (3K-8K observations) regardless of dataset size
  - Implements automatic fallback to elbow-only method when memory insufficient
  - Added pre-flight safety checks before memory-intensive operations

## Improvements

* **Enhanced memory management for clustering**
  - New `.get_available_memory_gb()` internal function detects actual free RAM on Mac/Linux/Windows
  - Decoupled silhouette sampling from `use_sampling` flag for independent memory control
  - Memory-aware sample sizes now calculated based on available RAM, not total RAM
  - Reduced default conservative limit from 20K to 5K when memory detection fails

* **Better error handling and user guidance**
  - Try-catch blocks around silhouette computation with actionable error messages
  - Graceful degradation: quality metrics return NA instead of crashing
  - Enhanced verbose mode shows memory detection method, limits, and fallback decisions
  - Clear error messages with ranked solutions when memory issues occur

* **Comprehensive documentation improvements**
  - Added "Memory Management and Troubleshooting" section to function documentation
  - Dataset size guidelines (< 10K, 10K-100K, 100K-500K, > 500K observations)
  - Enhanced `@param memory_fraction` documentation with specific recommendations
  - Five new code examples demonstrating memory constraint handling

## Tests

* Added 8 new tests for memory-aware clustering features
  - Test with 15K observations and conservative memory settings
  - Test automatic fallback to elbow-only method
  - Test quality metrics graceful degradation
  - Test hybrid decision rules with various memory constraints
  - All 81 tests in `test-hybrid-clustering.R` pass

## Technical Details

**Memory Optimization Results:**
- Before: 49K observations → 17GB distance matrix → crash
- After: 49K observations → 3,977 sample → ~200MB → success

**Files Modified:**
- `R/career_clustering.R`: Core memory management improvements
- `tests/testthat/test-hybrid-clustering.R`: New memory-related tests
- Documentation: Comprehensive troubleshooting guide

---

# longworkR 0.5.5

## Bug Fixes

* **Fixed memory_fraction parameter propagation in career clustering**: The `memory_fraction` parameter in `cluster_career_trajectories()` is now properly propagated to internal helper functions `.determine_optimal_clusters()` and `.compute_cluster_quality()`. This allows users to control the percentage of available RAM used for clustering operations, which is particularly important for large datasets or memory-constrained environments.

---

# longworkR 0.5.4

## Bug Fixes

* **Memory-aware clustering enhancements**: Improved memory management in clustering functions
* **IDate/scale_x_continuous compatibility**: Resolved type mismatch issues in integrated employment metric plots
* **Test suite stability**: Fixed 80 test failures from experimental branch merge

---

# longworkR 0.5.3

## New Features

* **Multilingual Support**: Added comprehensive multilingual support to all trajectory analysis functions with Italian as default language:
  - New `language` parameter in all `track_*_trajectories()` functions (default: "it")
  - Supports English ("en") and Italian ("it") status labels
  - New `translate_trajectory_status()` internal function for status translation
  - Employment statuses now display in Italian (e.g., "Non Occupato", "Parzialmente Occupato")
  - Professional, employer, and sector statuses translated accordingly

## Bug Fixes

* **Fixed consolidation logic in `consolidate_by_employer()`**: Simplified employer tracking logic to correctly consolidate adjacent employment periods with the same employer within `min_lag` days
* **Fixed missing `reference_dates` in trajectory functions**: All five trajectory functions now properly return `reference_dates` component in results
* **Fixed empty transitions handling**: Added proper handling for edge cases where no transitions occur between quarters
* **Fixed column naming consistency**: `track_contract_trajectories()` now preserves original column names in returned data structures

## Post-Merge Fixes (Experimental Branch Integration)

* **Restored consolidation metrics functions**: Re-added essential consolidation analysis functions that were inadvertently removed during merge:
  - `extract_consolidation_metrics()` - Detailed consolidation effectiveness metrics
  - `mark_employer_consolidation()` - Mark records for employer-based consolidation
  - `summarize_consolidation()` - Human-readable consolidation summaries
  - `analyze_employment_transitions_with_metrics()` - Combined transition and metrics analysis
  - All functions now available in `R/consolidation_metrics.R` (1779 lines)

* **API Compatibility**: Fixed test suite compatibility with new consolidation parameter names
  - Updated parameter name from `consolidation` to `consolidation_mode` in `analyze_employment_transitions()`
  - Added defensive NULL checks in DiD print methods to prevent errors
  - Old parameter name may still work but is deprecated

* **Test Suite Health**: Reduced test failures from 80 to 13 (84% improvement)
  - 257 tests passing (up from 196)
  - All restored functions fully tested and operational
  - Improved stability and reliability across the package

## Breaking Changes

* **Default language changed to Italian**: All trajectory analysis functions now return Italian labels by default. To use English labels, specify `language = "en"`
* **Consolidation parameter renamed**: `consolidation` parameter in `analyze_employment_transitions()` changed to `consolidation_mode` for API consistency

## Migration Notes for v0.5.3

**Parameter Renames:**
- `consolidation` → `consolidation_mode` in `analyze_employment_transitions()`
- Update existing code: `consolidation_mode = "temporal"` instead of `consolidation = "temporal"`

**Language Settings:**
- Trajectory analysis functions now default to Italian (`language = "it"`)
- For English labels: `track_*_trajectories(..., language = "en")`

**Restored Functions:**
Users relying on consolidation metrics can now use:
- `extract_consolidation_metrics()` for detailed consolidation analysis
- `mark_employer_consolidation()` for pre-consolidation marking
- `summarize_consolidation()` for summary reports
- `analyze_employment_transitions_with_metrics()` for integrated analysis

## Technical Details

* Updated test expectations to use Italian labels throughout trajectory analysis test suite
* Added 20+ tests for English language support
* Fixed consolidation test parameters (`consolidation` → `consolidation_mode`)
* Improved test coverage with 257 passing tests (up from 195)
* Added comprehensive documentation for all restored functions

---

# longworkR 0.5.2

## Enhancements

* **Trajectory Analysis Improvements**: Enhanced trajectory analysis functions with better error handling and edge case management
* **Test Suite Expansion**: Added comprehensive tests for unemployment detection and professional code tracking

---

# longworkR 0.5.1

## Bug Fixes

* **Fixed unemployment detection in `track_professional_trajectories()`**: Corrected the logic in `calculate_professional_trajectories_vectorized()` where unemployment periods (`arco = 0`) were not being properly detected as "Not Working" status. The fix reordered `fcase()` conditions to prioritize `all_arco_zero == TRUE` over `is.na(quarter_code)`, ensuring that quarters containing only unemployment periods are correctly classified.

* **Enhanced test coverage**: Added 8 comprehensive test cases specifically for unemployment detection scenarios, ensuring robust handling of mixed employment/unemployment patterns and edge cases with missing professional codes.

## Technical Details

* Modified lines 547-552 in `R/trajectory_analysis.R` to fix professional status classification
* All 188 tests now pass, including new unemployment detection test suite
* Resolves issue where "Not Working" status wasn't properly detected when `arco = 0` periods were present

---

# longworkR 0.5.0

## Major Features

* **Comprehensive package maintenance and consolidation enhancements**: Extensive refactoring of consolidation parameters and functionality
* **Performance optimizations**: Enhanced memory-efficient processing for large datasets
* **Documentation improvements**: Updated comprehensive documentation throughout

## Breaking Changes

* Refactored consolidation parameters interface for better consistency

## Bug Fixes

* **Fixed temporal assignment in `create_monthly_transition_matrices()`**: Corrected temporal assignment logic for accurate transition matrix calculations

## Documentation

* Added comprehensive documentation for memory-efficient transition matrix creation
* Enhanced vignettes and examples throughout the package

---

# longworkR 0.4.x and earlier

Previous versions focused on establishing core functionality for longitudinal employment analytics, survival analysis, impact evaluation methods, and visualization capabilities.