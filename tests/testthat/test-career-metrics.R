library(data.table)

# Load the package functions
devtools::load_all()

test_that("calculate_career_quality_metrics works with basic data", {
  # Create test data
  dt <- data.table(
    cf = rep(c("person1", "person2"), each = 6),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01", "2022-01-01", "2022-06-01",
                       "2020-02-01", "2020-07-01", "2021-02-01", "2021-07-01", "2022-02-01", "2022-07-01")),
    fine = as.Date(c("2020-05-31", "2020-12-31", "2021-05-31", "2021-12-31", "2022-05-31", "2022-12-31",
                     "2020-06-30", "2021-01-31", "2021-06-30", "2022-01-31", "2022-06-30", "2023-01-31")),
    durata = c(151, 214, 150, 214, 150, 214, 150, 214, 149, 214, 149, 214),
    over_id = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "A.01.00", "A.03.00", "A.01.00", "A.07.00",
                                   "A.03.00", "A.01.00", "A.07.00", "A.01.00", "A.03.00", "A.01.00"),
    prior = c(1, 0, 2, 1, 3, 1, 0, 1, 0, 2, 1, 3)
  )
  
  # Create mock survival data (required now)
  survival_data <- list(
    median_survival = c("A.01.00" = 365, "A.03.00" = 180, "A.07.00" = 120)
  )
  
  # Test basic functionality
  result <- calculate_career_quality_metrics(
    dt,
    survival_data = survival_data
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_true(all(c("cf", "total_employment_days", "contract_quality_score", 
                    "career_quality_index") %in% names(result)))
  
  # Check that quality scores are between 0 and 1
  expect_true(all(result$career_quality_index >= 0 & result$career_quality_index <= 1))
  expect_true(all(result$contract_quality_score >= 0 & result$contract_quality_score <= 1))
  
  # Check that component scores exist
  expect_true(all(c("employment_intensity_score", "career_stability_score", "growth_opportunity_score") %in% names(result)))
})

test_that("calculate_career_quality_metrics handles time periods", {
  # Create test data with time periods
  dt <- data.table(
    cf = rep("person1", 4),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    durata = c(150, 150, 150, 150),
    over_id = 1:4,
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "A.01.00", "A.03.00"),
    prior = c(1, 0, 2, 1),
    year = c(2020, 2020, 2021, 2021)
  )
  
  # Create mock survival data
  survival_data <- list(
    median_survival = c("A.01.00" = 365, "A.03.00" = 180)
  )
  
  result <- calculate_career_quality_metrics(
    dt,
    survival_data = survival_data,
    time_period_column = "year"
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)  # 1 person × 2 years
  expect_true("time_period" %in% names(result))
  expect_equal(sort(unique(result$time_period)), c(2020, 2021))
})

test_that("calculate_career_transition_metrics works with basic data", {
  # Create test data with transitions
  dt <- data.table(
    cf = rep("person1", 6),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01", "2022-01-01", "2022-06-01")),
    durata = c(150, 150, 150, 150, 150, 150),
    over_id = 1:6,
    COD_TIPOLOGIA_CONTRATTUALE = c("A.03.00", "A.01.00", "A.03.00", "A.01.00", "A.01.00", "A.01.00"),
    prior = c(0, 1, 0, 1, 2, 3),
    fine = as.Date(c("2020-05-30", "2020-10-29", "2021-05-30", "2021-10-29", "2022-05-30", "2022-10-29"))
  )
  
  # Create mock survival data
  survival_data <- list(
    median_survival = c("A.01.00" = 365, "A.03.00" = 180, "A.07.00" = 120)
  )
  
  result <- calculate_career_transition_metrics(
    dt,
    survival_data = survival_data
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(all(c("cf", "total_transitions", "duration_improvements", 
                    "composite_improvement_rate") %in% names(result)))
  
  # Should have at least some transitions
  expect_true(result$total_transitions > 0)
  expect_true(result$composite_improvement_rate >= 0)
})

test_that("calculate_career_transition_metrics handles salary data", {
  # Create test data with salary information
  dt <- data.table(
    cf = rep("person1", 4),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    durata = c(150, 150, 150, 150),
    over_id = 1:4,
    COD_TIPOLOGIA_CONTRATTUALE = c("A.03.00", "A.01.00", "A.01.00", "A.01.00"),
    prior = c(0, 1, 1, 2),
    fine = as.Date(c("2020-05-30", "2020-10-29", "2021-05-30", "2021-10-29")),
    monthly_wage = c(1000, 1200, 1300, 1500)
  )
  
  result <- calculate_career_transition_metrics(
    dt,
    salary_column = "monthly_wage"
  )
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("salary_improvements", "salary_deteriorations") %in% names(result)))
  
  # With increasing salaries, should have improvements
  expect_true(result$salary_improvements > 0)
  expect_equal(result$salary_deteriorations, 0)
})

test_that("calculate_career_quality_metrics works with comprehensive index", {
  # Create test data
  dt <- data.table(
    cf = rep(c("person1", "person2"), each = 3),
    durata = c(100, 200, 300, 150, 250, 180),
    over_id = rep(1:3, 2),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "A.07.00", "A.03.00", "A.01.00", "A.07.00"),
    prior = c(1, 0, 2, 1, 3, 0)
  )
  
  # Create mock survival data
  survival_data <- list(
    median_survival = c("A.01.00" = 365, "A.03.00" = 180, "A.07.00" = 120)
  )
  
  result <- calculate_career_quality_metrics(
    dt,
    survival_data = survival_data
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_true(all(c("cf", "contract_quality_score", "career_stability_score", 
                    "career_quality_index") %in% names(result)))
  
  # All scores should be between 0 and 1
  expect_true(all(result$contract_quality_score >= 0 & result$contract_quality_score <= 1))
  expect_true(all(result$career_quality_index >= 0 & result$career_quality_index <= 1))
  expect_true(all(result$career_stability_score >= 0 & result$career_stability_score <= 1))
})

test_that("calculate_comprehensive_career_metrics integrates all metrics", {
  # Create comprehensive test data
  dt <- data.table(
    cf = rep(c("person1", "person2"), each = 4),
    inizio = as.Date(rep(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01"), 2)),
    durata = c(150, 150, 150, 150, 180, 120, 200, 140),
    over_id = rep(1:4, 2),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.03.00", "A.01.00", "A.01.00", "A.01.00", 
                                   "A.07.00", "A.03.00", "A.01.00", "A.01.00"),
    prior = c(0, 1, 2, 2, 0, 0, 1, 2),
    fine = as.Date(rep(c("2020-05-30", "2020-10-29", "2021-05-30", "2021-10-29"), 2)),
    monthly_wage = c(1000, 1200, 1300, 1400, 800, 900, 1100, 1300)
  )
  
  # Create mock survival data for comprehensive testing
  survival_data <- list(
    median_survival = c("A.01.00" = 365, "A.03.00" = 180, "A.07.00" = 120)
  )
  
  # Test all metrics in wide format
  result_wide <- calculate_comprehensive_career_metrics(
    dt,
    survival_data = survival_data,
    metrics = "all",
    output_format = "wide",
    salary_column = "monthly_wage"
  )
  
  expect_s3_class(result_wide, "data.table")
  expect_equal(nrow(result_wide), 2)
  
  # Should contain metrics from all categories
  quality_cols <- grep("career_quality_index|contract_quality|intensity|stability_score|growth_opportunity", names(result_wide), value = TRUE)
  transition_cols <- grep("transition|improvement", names(result_wide), value = TRUE)
  stability_cols <- grep("employment_rate|employment_spells|turnover|employment_stability", names(result_wide), value = TRUE)
  
  expect_true(length(quality_cols) > 0)
  expect_true(length(transition_cols) > 0)
  expect_true(length(stability_cols) > 0)
  
  # Test list format
  result_list <- calculate_comprehensive_career_metrics(
    dt,
    survival_data = survival_data,
    metrics = c("quality", "transitions"),
    output_format = "list",
    salary_column = "monthly_wage"
  )
  
  expect_type(result_list, "list")
  expect_true(all(c("quality", "transitions") %in% names(result_list)))
  expect_s3_class(result_list$quality, "data.table")
  expect_s3_class(result_list$transitions, "data.table")
  
  # Test long format
  result_long <- calculate_comprehensive_career_metrics(
    dt,
    survival_data = survival_data,
    metrics = c("quality", "stability"),
    output_format = "long"
  )
  
  expect_s3_class(result_long, "data.table")
  expect_true(all(c("cf", "metric_name", "metric_value", "metric_category") %in% names(result_long)))
  expect_true(all(unique(result_long$metric_category) %in% c("quality", "stability", "other")))
})

test_that("career metrics handle edge cases", {
  # Test with single observation
  dt_single <- data.table(
    cf = "person1",
    inizio = as.Date("2020-01-01"),
    durata = 100,
    over_id = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00",
    prior = 1,
    fine = as.Date("2020-04-10")
  )
  
  # Need survival data for the function to work
  survival_data_single <- list(
    median_survival = c("A.01.00" = 365)
  )
  
  quality_result <- calculate_career_quality_metrics(dt_single, survival_data = survival_data_single)
  expect_s3_class(quality_result, "data.table")
  expect_equal(nrow(quality_result), 1)
  
  transition_result <- calculate_career_transition_metrics(dt_single)
  expect_s3_class(transition_result, "data.table")
  expect_equal(transition_result$total_transitions, 0)
  
  comprehensive_result <- calculate_career_quality_metrics(dt_single, survival_data = survival_data_single)
  expect_s3_class(comprehensive_result, "data.table")
  expect_equal(nrow(comprehensive_result), 1)
  
  # Test with no employment periods
  dt_empty <- data.table(
    cf = "person1",
    durata = 100,
    over_id = 0,  # No employment
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00",
    prior = 1
  )
  
  # Test that function handles empty employment data
  expect_warning(
    result_empty <- calculate_career_quality_metrics(dt_empty),
    "No valid employment observations found"
  )
})

test_that("career metrics validate input parameters", {
  dt <- data.table(
    cf = "person1",
    durata = 100,
    over_id = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00",
    prior = 1
  )
  
  # Create basic survival data for validation tests
  survival_data_basic <- list(
    median_survival = c("A.01.00" = 365)
  )
  
  # Test invalid data type
  expect_error(
    calculate_career_quality_metrics(as.data.frame(dt), survival_data = survival_data_basic),
    "Input data must be a data.table"
  )
  
  # Test missing required columns
  dt_missing <- dt[, -"durata"]
  expect_error(
    calculate_career_quality_metrics(dt_missing, survival_data = survival_data_basic),
    "Missing required columns"
  )
  
  # Test function works without survival_data (uses fallback)
  result_no_survival <- calculate_career_quality_metrics(dt)
  expect_s3_class(result_no_survival, "data.table")
  
  # Test invalid output format
  expect_error(
    calculate_comprehensive_career_metrics(dt, output_format = "invalid"),
    "output_format must be one of"
  )
  
  # Test invalid metrics
  expect_error(
    calculate_comprehensive_career_metrics(dt, metrics = "invalid"),
    "Invalid metrics specified"
  )
})

test_that("career metrics handle contract code variations", {
  # Test with different contract code formats
  dt <- data.table(
    cf = rep("person1", 3),
    durata = c(150, 200, 180),
    over_id = 1:3,
    COD_TIPOLOGIA_CONTRATTUALE = c("PERMANENT", "TEMPORARY", "INTERNSHIP"),
    prior = c(1, 0, 1)
  )
  
  # Create survival data for custom contract codes
  survival_data_custom <- list(
    median_survival = c("PERMANENT" = 500, "TEMPORARY" = 150, "INTERNSHIP" = 100)
  )
  
  result <- calculate_career_quality_metrics(
    dt,
    survival_data = survival_data_custom
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(result$growth_opportunity_score >= 0)  # Check growth opportunity component
})

test_that("career transition metrics compute improvements correctly", {
  # Create data with clear improvements
  dt <- data.table(
    cf = "person1",
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01")),
    durata = c(150, 150, 150),
    over_id = 1:3,
    COD_TIPOLOGIA_CONTRATTUALE = c("A.03.00", "A.07.00", "A.01.00"),  # temp -> internship -> permanent
    prior = c(0, 1, 2),  # part-time -> full-time -> more full-time
    fine = as.Date(c("2020-05-30", "2020-10-29", "2021-05-30")),
    monthly_wage = c(1000, 1200, 1500)  # increasing salary
  )
  
  survival_data <- list(
    median_survival = c("A.01.00" = 365, "A.03.00" = 180, "A.07.00" = 240)
  )
  
  result <- calculate_career_transition_metrics(
    dt,
    survival_data = survival_data,
    salary_column = "monthly_wage"
  )
  
  expect_equal(result$total_transitions, 2)
  expect_true(result$duration_improvements > 0)
  expect_true(result$fulltime_improvements > 0)
  expect_true(result$salary_improvements > 0)
  expect_true(result$career_progression_index > 0.5)  # Should be high with all improvements
})

test_that("employment diversity index handles zero diversity correctly", {
  # Test data with no diversity (all same employment type)
  dt <- data.table(
    cf = "person1",
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    durata = c(30, 30, 30),
    over_id = 1:3,
    arco = c(1, 1, 1),
    prior = c(1, 1, 1), # All same
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.01.00", "A.01.00") # All same
  )
  
  result <- calculate_career_complexity_metrics(dt)
  
  # Should be exactly 0, not a tiny negative number
  expect_equal(result$employment_diversity_index, 0.0)
  expect_true(result$employment_diversity_index >= 0)
})

test_that("employment diversity index varies with different employment types", {
  # Test data with varying diversity levels
  dt <- data.table(
    cf = rep(c("no_diversity", "some_diversity", "high_diversity"), each = 4),
    inizio = as.Date(rep(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01"), 3)),
    durata = rep(c(30, 30, 30, 30), 3),
    over_id = rep(1:4, 3),
    arco = rep(c(1, 1, 1, 1), 3),
    prior = c(
      1, 1, 1, 1,  # no diversity
      0, 1, 0, 1,  # some diversity (2 types)
      0, 1, 2, 3   # high diversity (4 types)
    )
  )
  
  result <- calculate_career_complexity_metrics(dt)
  
  no_div <- result[cf == "no_diversity", employment_diversity_index]
  some_div <- result[cf == "some_diversity", employment_diversity_index]
  high_div <- result[cf == "high_diversity", employment_diversity_index]
  
  # Check ordering: high > some > no
  expect_true(high_div > some_div)
  expect_true(some_div > no_div)
  expect_equal(no_div, 0.0)
  
  # Check that all values are non-negative
  expect_true(all(result$employment_diversity_index >= 0))
})

test_that("complexity_variables parameter works with different variables", {
  dt <- data.table(
    cf = rep("person1", 3),
    inizio = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    durata = c(30, 30, 30),
    over_id = 1:3,
    arco = c(1, 1, 1),
    prior = c(1, 1, 1), # No diversity
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "A.07.00"), # High diversity
    qualifica = c("Q1", "Q1", "Q1") # No diversity
  )
  
  # Test with prior (should be 0)
  result_prior <- calculate_career_complexity_metrics(dt, complexity_variables = c("prior"))
  expect_equal(result_prior$employment_diversity_index, 0.0)
  
  # Test with contract types (should be > 0)
  result_contract <- calculate_career_complexity_metrics(dt, complexity_variables = c("COD_TIPOLOGIA_CONTRATTUALE"))
  expect_true(result_contract$employment_diversity_index > 0)
  
  # Test with qualifica (should be 0)
  result_qual <- calculate_career_complexity_metrics(dt, complexity_variables = c("qualifica"))
  expect_equal(result_qual$employment_diversity_index, 0.0)
})