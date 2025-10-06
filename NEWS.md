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

## Breaking Changes

* **Default language changed to Italian**: All trajectory analysis functions now return Italian labels by default. To use English labels, specify `language = "en"`

## Technical Details

* Updated test expectations to use Italian labels throughout trajectory analysis test suite
* Added 20+ tests for English language support
* Fixed consolidation test parameters (`consolidation` → `consolidation_type`)
* Improved test coverage with 195 passing tests in trajectory analysis

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