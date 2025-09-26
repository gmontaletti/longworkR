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