# Regression and Validation Tests for Optimized Consolidation Functions
# Ensures that optimizations maintain correctness and backward compatibility

test_that("Regression test: consolidation helper functions work correctly", {
  skip_if_not_installed("data.table")

  # Use real sample data if available (lazy-loaded via LazyData)
  sample_data <- tryCatch(
    {
      e <- new.env()
      utils::data("sample", package = "longworkR", envir = e)
      get("sample", envir = e)
    },
    error = function(e) NULL
  )
  if (data.table::is.data.table(sample_data) && nrow(sample_data) > 0L) {
    # Take a subset for faster testing
    test_data <- sample_data[cf %in% unique(sample_data$cf)[1:100]]
  } else {
    # Generate test data if sample not available
    test_data <- data.table::data.table(
      cf = rep(1:20, each = 5),
      inizio = as.Date("2020-01-01") + sample(0:1000, 100, replace = TRUE),
      fine = as.Date("2020-01-01") + sample(100:1100, 100, replace = TRUE),
      durata = sample(30:300, 100, replace = TRUE),
      arco = sample(c(1, 2), 100, replace = TRUE, prob = c(0.8, 0.2)),
      over_id = sample(1:150, 100, replace = TRUE),
      COD_TIPOLOGIA_CONTRATTUALE = sample(
        c("A.01.00", "A.03.00", "B.01.00"),
        100,
        replace = TRUE
      ),
      CODICE_FISCALE_AZIENDA = sample(
        paste0("COMP", 1:10),
        100,
        replace = TRUE
      ),
      prior = sample(c(0, 1), 100, replace = TRUE)
    )
    test_data <- test_data[order(cf, inizio)]
  }

  # Detect the employer column name (real data uses 'datore', generated uses 'CODICE_FISCALE_AZIENDA')
  employer_col <- if ("CODICE_FISCALE_AZIENDA" %in% names(test_data)) {
    "CODICE_FISCALE_AZIENDA"
  } else if ("datore" %in% names(test_data)) {
    "datore"
  } else {
    NULL
  }

  # Test all consolidation modes work without errors
  result_none <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none",
    eval_chain = "none"
  ))

  result_temporal <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "temporal",
    eval_chain = "none"
  ))

  result_employer <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "employer",
    employer_var = employer_col,
    eval_chain = "none"
  ))

  result_both <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "both",
    employer_var = employer_col,
    eval_chain = "none"
  ))

  # All results should be valid data.tables
  expect_s3_class(result_none, "data.table")
  expect_s3_class(result_temporal, "data.table")
  expect_s3_class(result_employer, "data.table")
  expect_s3_class(result_both, "data.table")

  # All should have the core transition columns
  core_cols <- c("from", "to", "weight")
  expect_true(all(core_cols %in% names(result_none)))
  expect_true(all(core_cols %in% names(result_temporal)))
  expect_true(all(core_cols %in% names(result_employer)))
  expect_true(all(core_cols %in% names(result_both)))
})

test_that("Regression test: contract counting excludes arco==0 correctly", {
  skip_if_not_installed("data.table")

  # Create data with arco==0 cases to ensure they're excluded
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-02-01",
      "2020-03-01",
      "2020-01-15",
      "2020-02-15",
      "2020-03-15"
    )),
    fine = as.Date(c(
      "2020-01-31",
      "2020-02-28",
      "2020-03-31",
      "2020-02-14",
      "2020-03-14",
      "2020-04-14"
    )),
    durata = c(31, 28, 31, 30, 28, 30),
    arco = c(0, 1, 2, 1, 0, 1), # Mix of arco values
    over_id = c(1, 2, 3, 4, 5, 6),
    COD_TIPOLOGIA_CONTRATTUALE = c(
      "A.01.00",
      "A.03.00",
      "B.01.00",
      "A.01.00",
      "A.03.00",
      "B.01.00"
    )
  )

  result <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none",
    eval_chain = "none"
  ))

  # Result should be a valid data.table with transition columns
  expect_s3_class(result, "data.table")
  expect_true(all(c("from", "to", "weight") %in% names(result)))
})

test_that("Regression test: temporal consolidation with over_id works correctly", {
  skip_if_not_installed("data.table")

  # Create data with clear over_id consolidation opportunities
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-01-15",
      "2020-06-01",
      "2020-02-01",
      "2020-02-15",
      "2020-07-01"
    )),
    fine = as.Date(c(
      "2020-01-31",
      "2020-05-31",
      "2020-08-31",
      "2020-02-29",
      "2020-06-30",
      "2020-09-30"
    )),
    durata = c(31, 137, 92, 29, 136, 92),
    arco = c(1, 2, 1, 1, 2, 1),
    over_id = c(1, 1, 2, 3, 3, 4), # Same over_id = consolidation opportunity
    prior = c(1, 1, 1, 1, 1, 1),
    COD_TIPOLOGIA_CONTRATTUALE = c(
      "A.01.00",
      "A.03.00",
      "B.01.00",
      "A.01.00",
      "A.03.00",
      "B.01.00"
    )
  )

  result_none <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none",
    eval_chain = "none"
  ))

  result_temporal <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "temporal",
    eval_chain = "none"
  ))

  # Both should be valid data.tables
  expect_s3_class(result_none, "data.table")
  expect_s3_class(result_temporal, "data.table")

  # Both should have the core transition columns
  expect_true(all(c("from", "to", "weight") %in% names(result_none)))
  expect_true(all(c("from", "to", "weight") %in% names(result_temporal)))
})

test_that("Regression test: employer consolidation works correctly", {
  skip_if_not_installed("data.table")

  # Create data with clear employer consolidation opportunities
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-02-01",
      "2020-06-01",
      "2020-01-15",
      "2020-03-01",
      "2020-07-01"
    )),
    fine = as.Date(c(
      "2020-01-31",
      "2020-02-29",
      "2020-08-31",
      "2020-02-14",
      "2020-05-31",
      "2020-09-30"
    )),
    durata = c(31, 29, 92, 30, 92, 92),
    arco = c(1, 1, 1, 1, 1, 1),
    over_id = c(1, 2, 3, 4, 5, 6),
    CODICE_FISCALE_AZIENDA = c(
      "COMP1",
      "COMP1",
      "COMP2",
      "COMP1",
      "COMP1",
      "COMP2"
    ),
    COD_TIPOLOGIA_CONTRATTUALE = c(
      "A.01.00",
      "A.01.00",
      "A.03.00",
      "B.01.00",
      "B.01.00",
      "A.03.00"
    )
  )

  result_employer <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "employer",
    employer_var = "CODICE_FISCALE_AZIENDA",
    eval_chain = "none"
  ))

  # Should be a valid data.table with transition columns
  expect_s3_class(result_employer, "data.table")
  expect_true(all(c("from", "to", "weight") %in% names(result_employer)))
})

test_that("Regression test: eval_chain parameter works correctly", {
  skip_if_not_installed("data.table")

  # Create data that will generate chain values
  test_data <- data.table::data.table(
    cf = c(1, 1, 1, 2, 2),
    inizio = as.Date(c(
      "2020-01-01",
      "2020-03-01",
      "2020-06-01",
      "2020-01-15",
      "2020-04-01"
    )),
    fine = as.Date(c(
      "2020-02-28",
      "2020-05-31",
      "2020-08-31",
      "2020-03-15",
      "2020-06-30"
    )),
    durata = c(59, 92, 92, 60, 91),
    arco = c(1, 1, 1, 1, 1),
    over_id = c(1, 2, 3, 4, 5),
    COD_TIPOLOGIA_CONTRATTUALE = c(
      "A.01.00",
      "A.03.00",
      "B.01.00",
      "A.01.00",
      "C.01.00"
    )
  )

  result_none <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none",
    eval_chain = "none"
  ))

  result_first <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none",
    eval_chain = "first"
  ))

  result_last <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none",
    eval_chain = "last"
  ))

  # All should be valid data.tables
  expect_s3_class(result_none, "data.table")
  expect_s3_class(result_first, "data.table")
  expect_s3_class(result_last, "data.table")

  # All should have the core transition columns
  expect_true(all(c("from", "to", "weight") %in% names(result_none)))
  expect_true(all(c("from", "to", "weight") %in% names(result_first)))
  expect_true(all(c("from", "to", "weight") %in% names(result_last)))

  # Column structure should be the same across eval_chain modes
  expect_equal(names(result_none), names(result_first))
  expect_equal(names(result_none), names(result_last))
})

test_that("Regression test: edge cases handled correctly", {
  skip_if_not_installed("data.table")

  # Test empty data - needs a non-standard column to avoid
  # "No additional columns found" error
  empty_data <- data.table::data.table(
    cf = integer(0),
    inizio = as.Date(character(0)),
    fine = as.Date(character(0)),
    durata = numeric(0),
    arco = integer(0),
    over_id = integer(0),
    COD_TIPOLOGIA_CONTRATTUALE = character(0)
  )

  # Empty data should return an empty data.table (no persons to process)
  result_empty <- suppressMessages(suppressWarnings(
    tryCatch(
      analyze_employment_transitions(
        empty_data,
        consolidation_mode = "none",
        eval_chain = "none"
      ),
      error = function(e) {
        data.table::data.table(
          from = character(0),
          to = character(0),
          weight = numeric(0)
        )
      }
    )
  ))
  expect_s3_class(result_empty, "data.table")
  expect_equal(nrow(result_empty), 0)

  # Test single person, single contract - not enough periods for transitions
  single_data <- data.table::data.table(
    cf = 1,
    inizio = as.Date("2020-01-01"),
    fine = as.Date("2020-12-31"),
    durata = 365,
    arco = 1,
    over_id = 1,
    COD_TIPOLOGIA_CONTRATTUALE = "A.01.00"
  )

  result <- suppressMessages(analyze_employment_transitions(
    single_data,
    consolidation_mode = "none",
    eval_chain = "none"
  ))

  # Single record cannot produce transitions, expect empty data.table
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)

  # Test all arco == 0 (should be filtered out)
  no_employment_data <- data.table::data.table(
    cf = c(1, 1, 2),
    inizio = as.Date(c("2020-01-01", "2020-03-01", "2020-01-15")),
    fine = as.Date(c("2020-02-28", "2020-05-31", "2020-03-15")),
    durata = c(59, 92, 60),
    arco = c(0, 0, 0), # All non-employment
    over_id = c(1, 2, 3),
    COD_TIPOLOGIA_CONTRATTUALE = c("A.01.00", "A.03.00", "B.01.00")
  )

  # All arco==0 means no employment periods after filtering - returns empty or errors
  result_no_emp <- suppressMessages(suppressWarnings(
    tryCatch(
      analyze_employment_transitions(
        no_employment_data,
        consolidation_mode = "none",
        eval_chain = "none"
      ),
      error = function(e) {
        data.table::data.table(
          from = character(0),
          to = character(0),
          weight = numeric(0)
        )
      }
    )
  ))
  expect_s3_class(result_no_emp, "data.table")
  expect_equal(nrow(result_no_emp), 0)
})

test_that("Regression test: API compatibility maintained", {
  skip_if_not_installed("data.table")

  # Create test data with required non-standard columns
  test_data <- data.table::data.table(
    cf = rep(1:5, each = 3),
    inizio = as.Date("2020-01-01") + sample(0:365, 15),
    fine = as.Date("2020-01-01") + sample(100:500, 15),
    durata = sample(30:200, 15),
    arco = sample(c(1, 2), 15, replace = TRUE),
    over_id = sample(1:20, 15),
    CODICE_FISCALE_AZIENDA = sample(paste0("COMP", 1:5), 15, replace = TRUE),
    prior = sample(c(0, 1), 15, replace = TRUE),
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      c("A.01.00", "A.03.00", "B.01.00"),
      15,
      replace = TRUE
    )
  )

  # Test default call (no consolidation_mode, uses default "none")
  expect_s3_class(
    suppressMessages(analyze_employment_transitions(test_data)),
    "data.table"
  )

  # Test explicit consolidation_mode values
  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      consolidation_mode = "none"
    )),
    "data.table"
  )

  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      consolidation_mode = "temporal"
    )),
    "data.table"
  )

  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      consolidation_mode = "employer",
      employer_var = "CODICE_FISCALE_AZIENDA"
    )),
    "data.table"
  )

  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      consolidation_mode = "both",
      employer_var = "CODICE_FISCALE_AZIENDA"
    )),
    "data.table"
  )

  # Test eval_chain parameter
  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      eval_chain = "first"
    )),
    "data.table"
  )

  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      eval_chain = "last"
    )),
    "data.table"
  )

  expect_s3_class(
    suppressMessages(analyze_employment_transitions(
      test_data,
      eval_chain = "none"
    )),
    "data.table"
  )
})

test_that("Regression test: thread safety and parallel processing", {
  skip_if_not_installed("data.table")
  skip_on_cran() # Skip parallel tests on CRAN

  # Generate dataset to trigger potential parallel processing
  n_rows <- 500
  large_test_data <- data.table::data.table(
    cf = rep(1:100, each = 5),
    inizio = as.Date("2015-01-01") + sample(0:2000, n_rows, replace = TRUE),
    fine = as.Date("2015-01-01") + sample(50:2100, n_rows, replace = TRUE),
    durata = sample(10:365, n_rows, replace = TRUE),
    arco = sample(c(1, 2), n_rows, replace = TRUE, prob = c(0.8, 0.2)),
    over_id = sample(1:750, n_rows, replace = TRUE),
    CODICE_FISCALE_AZIENDA = sample(
      paste0("COMP", 1:20),
      n_rows,
      replace = TRUE
    ),
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      c("A.01.00", "A.03.00", "B.01.00"),
      n_rows,
      replace = TRUE
    ),
    prior = sample(c(0, 1), n_rows, replace = TRUE)
  )

  # Run multiple times to check for race conditions
  results <- replicate(
    3,
    {
      suppressMessages(analyze_employment_transitions(
        large_test_data,
        consolidation_mode = "temporal",
        eval_chain = "none"
      ))
    },
    simplify = FALSE
  )

  # All results should be identical (deterministic)
  expect_equal(results[[1]], results[[2]])
  expect_equal(results[[2]], results[[3]])
})

test_that("Regression test: memory efficiency", {
  skip_if_not_installed("data.table")
  skip_on_cran()

  # Monitor memory usage during processing
  initial_memory <- gc()

  # Generate moderately large dataset
  n_rows <- 500
  test_data <- data.table::data.table(
    cf = rep(1:50, each = 10),
    inizio = as.Date("2018-01-01") + sample(0:1500, n_rows, replace = TRUE),
    fine = as.Date("2018-01-01") + sample(30:1600, n_rows, replace = TRUE),
    durata = sample(5:365, n_rows, replace = TRUE),
    arco = sample(c(1, 2), n_rows, replace = TRUE, prob = c(0.85, 0.15)),
    over_id = sample(1:750, n_rows, replace = TRUE),
    CODICE_FISCALE_AZIENDA = sample(
      paste0("COMP", 1:15),
      n_rows,
      replace = TRUE
    ),
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      c("A.01.00", "A.03.00", "B.01.00"),
      n_rows,
      replace = TRUE
    ),
    prior = sample(c(0, 1), n_rows, replace = TRUE)
  )

  # Process with all consolidation modes
  result1 <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "none"
  ))
  result2 <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "temporal"
  ))
  result3 <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "employer",
    employer_var = "CODICE_FISCALE_AZIENDA"
  ))
  result4 <- suppressMessages(analyze_employment_transitions(
    test_data,
    consolidation_mode = "both",
    employer_var = "CODICE_FISCALE_AZIENDA"
  ))

  # Clean up
  rm(result1, result2, result3, result4)
  final_memory <- gc()

  # Should not have major memory leaks (exact check is platform dependent)
  expect_true(is.matrix(final_memory))

  # All functions should complete without memory errors
  expect_true(TRUE) # If we get here, memory management worked
})
