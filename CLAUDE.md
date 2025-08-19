# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**longworkR** is an R package providing advanced analytics and visualization tools for longitudinal employment data processed by the `vecshift` package. It specializes in survival analysis, impact evaluation methods (DiD, PSM, RDD), network analysis of employment transitions, and comprehensive visualizations.

## Development Commands

### Build and Check
```bash
# Build package
R CMD build .

# Check package (comprehensive)
R CMD check longworkR_*.tar.gz

# Install package locally
R CMD INSTALL .
# or from R:
devtools::install(".")

# Load for development
devtools::load_all()
```

### Testing
```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-analyze-employment-transitions.R')"

# Run tests with coverage
Rscript -e "covr::package_coverage()"
```

### Documentation
```bash
# Generate documentation from roxygen2 comments
Rscript -e "devtools::document()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"

# Check documentation
Rscript -e "devtools::check_man()"
```

## Architecture

### Core Dependencies
- **vecshift**: Required parent package that provides the core data processing functionality
- **data.table**: Used for efficient data manipulation throughout
- **ggplot2/ggraph**: Primary visualization frameworks
- **survival**: Core survival analysis functionality

### Module Organization

1. **Transition Analysis** (`analyze_employment_transitions.R`)
   - Processes employment transitions with chain evaluation (first/last/none)
   - Handles consolidation using over_id periods
   - Creates transition matrices and network data structures

2. **Impact Evaluation Suite** (`impact_*.R` files)
   - `impact_estimation.R`: Core estimation methods (DiD, PSM)
   - `impact_evaluation.R`: Treatment event identification
   - `impact_matching.R`: Propensity score matching algorithms
   - `impact_metrics.R`: Metric calculations
   - `impact_rdd.R`: Regression discontinuity design
   - `impact_reporting.R`: Results formatting
   - `impact_visualization.R`: Impact-specific plots

3. **Survival Analysis** (`survival_*.R`)
   - Contract survival curves and hazard functions
   - Comparative analysis across groups
   - Visualization of survival metrics

4. **Visualization Framework**
   - `ggraph_transitions.R`: Static network visualizations
   - `interactive_transitions_g6r.R`: Interactive network exploration
   - `theme_vecshift.R`: Consistent theming system
   - `vecshift_plots.R` & `vecshift_plots_extensions.R`: Core plot functions

### Data Flow
1. Data processed by vecshift package → 
2. longworkR functions analyze transitions/impacts/survival →
3. Results returned as data.table objects with visualization methods

### Key Design Patterns
- Functions accept vecshift output directly
- Chain values (e.g., "state1->state2->state3") handled with eval_chain parameter
- Consolidation periods use over_id from vecshift
- All major functions return both data and visualization objects
- Colorblind-friendly palettes used by default

## Testing Approach
- Test suite uses testthat v3
- Tests located in `tests/testthat/`
- Each major module has corresponding test file
- Focus on data transformation correctness and edge cases

## Sample Data
- **Location**: `data/sample.rds` 
- **Format**: R data.table object with vecshift-processed employment records
- **Purpose**: Testing and development of career evaluation functions
- **Usage**: Load with `readRDS("data/sample.rds")` for function testing
- **Structure**: Contains typical vecshift output columns including:
  - `cf`: Person identifier
  - `inizio`, `fine`: Contract start/end dates
  - `durata`: Contract duration in days
  - `over_id`: Employment period identifier for consolidation
  - `arco`: Concurrent employment indicator
  - `prior`: Employment intensity (1 = full-time, 0 = part-time/other)
  - `COD_TIPOLOGIA_CONTRATTUALE`: Contract type codes (X.01.00 format)
  - Salary/wage information when available

## Project Memory and Conventions

### File Organization
- **Temporary files and drafts**: Store in `../reference/longworkR/` directory
- **Test files (temporary)**: Create in `../reference/longworkR/` before integration
- **Documentation drafts**: Draft .md files in `../reference/longworkR/`
- **TODO lists and notes**: Maintain in `../reference/longworkR/`

### Cleanup Process
- The r-project-maintainer agent will move non-compilation artifacts to `../reference/longworkR/`
- This keeps the main package directory clean for CRAN submission
- The reference directory is gitignored to prevent accidental commits

### Version Control
- Git repository initialized with comprehensive .gitignore
- GitHub repository to be created at: `https://github.com/[username]/longworkR`
- Initial commit includes all package structure and documentation
- Future commits should follow conventional commit messages

### Development Workflow
1. Create drafts and experimental code in `../reference/longworkR/`
2. Test thoroughly before moving to main package directory
3. Use r-project-maintainer for cleanup before builds
4. Ensure all tests pass before committing to main branch
- temporary and complementary tests file, .md documents and note and todo lists should be created in ../reference/longworkR directory. On cleaning the packege agent-r-project-maintainer will move all the artefacts not needed for project compilation there.
- the project is using renv(), use it when testing
- annotate the .claude directory should'nt be moved during cleanup