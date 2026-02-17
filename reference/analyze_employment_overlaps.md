# Analyze Employment Overlaps Using over_id

Provides comprehensive analysis of overlapping employment periods using
the over_id functionality from vecshift() output. This function
specifically focuses on periods with multiple simultaneous employment
contracts (arco \> 1) to understand patterns of multiple job holding,
employment intensity, and temporal overlap characteristics.

## Usage

``` r
analyze_employment_overlaps(
  segments,
  analysis_level = "person",
  min_overlap_duration = 1,
  include_temporal_patterns = TRUE,
  group_by = NULL,
  temporal_aggregation = "month",
  include_unemployment_context = TRUE
)
```

## Arguments

- segments:

  Input data from vecshift() with over_id column. Must be a data.table
  with required columns: cf, inizio, fine, arco, durata, over_id, and
  optionally additional employment attributes for detailed analysis.

- analysis_level:

  Character string specifying analysis granularity (default: "person"):

  - `"person"`: Person-level analysis with individual overlap statistics

  - `"aggregate"`: Population-level aggregate overlap patterns

  - `"detailed"`: Both person and aggregate analysis with comprehensive
    breakdowns

- min_overlap_duration:

  Minimum duration (days) to consider as meaningful overlap (default:
  1). Helps filter out very short-term overlapping contracts.

- include_temporal_patterns:

  Logical. If TRUE (default), analyzes when overlaps occur most
  frequently (monthly/seasonal patterns). Requires date columns.

- group_by:

  Optional character string specifying grouping variable (e.g.,
  "industry", "region") for comparative overlap analysis across
  different categories. Must be a column name in segments (default:
  NULL).

- temporal_aggregation:

  Character string specifying temporal aggregation for pattern analysis
  when include_temporal_patterns = TRUE (default: "month"):

  - `"month"`: Monthly overlap frequency patterns

  - `"quarter"`: Quarterly overlap frequency patterns

  - `"year"`: Annual overlap frequency patterns

- include_unemployment_context:

  Logical. If TRUE (default), includes context about unemployment
  periods for comparison with overlapping employment patterns.

## Value

When `analysis_level` is "person", returns a list containing:

- `person_overlaps`: data.table with person-level overlap statistics:

  - `cf`: Person identifier

  - `total_employment_periods`: Total employment segments (arco \>= 1)

  - `overlapping_episodes`: Count of periods with arco \> 1

  - `single_job_periods`: Count of periods with arco = 1

  - `overlap_frequency`: Proportion of employment periods with overlaps

  - `avg_concurrent_jobs`: Mean arco for employment periods

  - `max_concurrent_jobs`: Maximum simultaneous jobs (peak employment
    complexity)

  - `total_overlap_days`: Sum of days spent in overlapping employment

  - `total_employment_days`: Sum of all employment days

  - `overlap_intensity_pct`: Percentage of employment time with multiple
    jobs

  - `avg_overlap_duration`: Mean duration of overlapping episodes

  - `max_overlap_duration`: Longest overlapping episode duration

  - `unique_over_id_groups`: Count of distinct overlapping employment
    episodes

  - `employment_complexity_score`: Weighted complexity measure

When `analysis_level` is "aggregate", returns aggregate-level analysis.

When `analysis_level` is "detailed", returns comprehensive analysis
including:

- `person_overlaps`: Person-level statistics

- `aggregate_overlaps`: Population-level aggregate patterns

- `overlap_duration_distribution`: Distribution of overlap durations

- `employment_intensity_distribution`: Distribution of concurrent job
  counts

- `temporal_patterns`: Temporal overlap frequency (if
  include_temporal_patterns = TRUE)

- `group_comparisons`: Comparative analysis by group (if group_by
  specified)

All results include attributes:

- `analysis_parameters`: Analysis configuration used

- `overlap_validation`: Data validation and quality checks specific to
  overlaps

- `computation_time`: Time taken for analysis

## Details

The over_id column from vecshift() identifies continuous overlapping
employment periods:

- **over_id = 0**: Unemployment periods (no active contracts)

- **over_id \> 0**: Employment periods with same value for
  overlapping/continuous contracts

- **Same over_id**: All contracts belonging to same continuous
  overlapping time period

This function analyzes employment overlap patterns including:

- **Overlap Episodes**: Periods where arco \> 1 (multiple concurrent
  jobs)

- **Employment Intensity**: How many jobs are held simultaneously

- **Duration Patterns**: How long overlapping employment lasts

- **Temporal Analysis**: When overlaps occur most frequently

- **Person-level Patterns**: Individual multiple job holding behaviors

- **Overlap Efficiency**: Time spent in multiple vs single employment

Key metrics calculated:

- **Total overlapping episodes**: Count of periods with arco \> 1

- **Average/maximum concurrent jobs**: Employment intensity measures

- **Overlap duration distribution**: How long multiple employment lasts

- **Overlap intensity percentage**: Proportion of employment time with
  multiple jobs

- **Peak employment complexity**: Maximum simultaneous jobs per person

- **Temporal patterns**: Monthly/seasonal overlap frequency (optional)

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample employment data with overlapping periods
employment_data <- data.table(
  id = 1:12,
  cf = c(rep("PERSON001", 8), rep("PERSON002", 4)),
  INIZIO = as.Date(c("2023-01-01", "2023-02-01", "2023-02-15", "2023-05-01",
                     "2023-05-15", "2023-08-01", "2023-10-01", "2023-11-01",
                     "2023-01-15", "2023-03-01", "2023-06-01", "2023-09-01")),
  FINE = as.Date(c("2023-01-31", "2023-04-30", "2023-04-30", "2023-07-31",
                   "2023-07-15", "2023-09-30", "2023-10-31", "2023-12-31",
                   "2023-02-28", "2023-05-31", "2023-08-31", "2023-12-31")),
  prior = c(1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1),
  company = c("CompanyA", "CompanyB", "CompanyA", "CompanyC", "CompanyD",
              "CompanyE", "CompanyF", "CompanyG", "CompanyH", "CompanyI",
              "CompanyJ", "CompanyK"),
  industry = c("Tech", "Retail", "Tech", "Finance", "Tech", "Retail",
               "Finance", "Tech", "Healthcare", "Education", "Finance", "Tech")
)

# Apply vecshift to get segments with over_id
segments <- vecshift(employment_data)

# Comprehensive overlap analysis
overlap_analysis <- analyze_employment_overlaps(
  segments = segments,
  analysis_level = "detailed",
  min_overlap_duration = 1,
  include_temporal_patterns = TRUE,
  temporal_aggregation = "month"
)

# View person-level overlap patterns
print(overlap_analysis$person_overlaps)

# View aggregate overlap statistics
print(overlap_analysis$aggregate_overlaps)

# View overlap duration distribution
print(overlap_analysis$overlap_duration_distribution)

# View employment intensity patterns
print(overlap_analysis$employment_intensity_distribution)

# View temporal patterns (when overlaps occur)
if (!is.null(overlap_analysis$temporal_patterns)) {
  print(overlap_analysis$temporal_patterns)
}

# Person-level analysis only
person_overlaps <- analyze_employment_overlaps(
  segments = segments,
  analysis_level = "person",
  min_overlap_duration = 7,  # Focus on overlaps lasting at least a week
  include_temporal_patterns = FALSE
)
print(person_overlaps$person_overlaps)

# Aggregate analysis with industry comparison
industry_overlaps <- analyze_employment_overlaps(
  segments = segments,
  analysis_level = "aggregate",
  group_by = "industry",
  min_overlap_duration = 1
)
print(industry_overlaps$aggregate_overlaps)
print(industry_overlaps$group_comparisons)

# Quarterly temporal patterns
quarterly_patterns <- analyze_employment_overlaps(
  segments = segments,
  analysis_level = "detailed",
  include_temporal_patterns = TRUE,
  temporal_aggregation = "quarter"
)
print(quarterly_patterns$temporal_patterns)

# Check analysis validation and parameters
print(attr(overlap_analysis, "analysis_parameters"))
print(attr(overlap_analysis, "overlap_validation"))

# Example output interpretation:
# - person_overlaps shows which individuals have complex employment patterns
# - overlap_intensity_pct indicates how much time is spent in multiple employment
# - employment_complexity_score provides weighted measure of job juggling behavior
# - temporal_patterns reveal seasonal trends in multiple job holding
# - group_comparisons show industry-specific overlap behaviors
} # }
```
