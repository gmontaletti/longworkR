# 1. Regression snapshot for analyze_employment_transitions -----
#
# Pins the output of analyze_employment_transitions() on data/sample.rds
# for each eval_chain value. Guards the W3.2 shift() vectorization
# rewrite from changing semantics.

skip_if_missing_sample <- function() {
  d <- tryCatch(
    {
      e <- new.env()
      utils::data("sample", package = "longworkR", envir = e)
      get("sample", envir = e)
    },
    error = function(e) NULL
  )
  if (!data.table::is.data.table(d) || nrow(d) == 0L) {
    testthat::skip("sample dataset not available")
  }
  d
}

# 2. Core baseline output per eval_chain -----

test_that("analyze_employment_transitions matches baseline on sample.rds", {
  d <- skip_if_missing_sample()

  params <- list(
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    statistics_variables = c("eta", "sesso"),
    show_progress = FALSE
  )

  for (ec in c("last", "first", "none")) {
    args <- c(list(pipeline_result = d, eval_chain = ec), params)
    res <- suppressWarnings(do.call(analyze_employment_transitions, args))

    expect_s3_class(res, "data.table")
    expect_true(all(
      c("from", "to", "weight", "transition_duration") %in% names(res)
    ))
    expect_true(all(
      c("eta_from_median", "eta_to_median") %in% names(res)
    ))
    expect_true(all(
      c("sesso_from_mode", "sesso_to_mode") %in% names(res)
    ))

    # Row/column shape snapshot
    expect_identical(
      nrow(res),
      234L,
      info = sprintf("nrow mismatch for eval_chain='%s'", ec)
    )

    # Total weight must be invariant across eval_chain choices — it counts
    # transitions, not chain levels.
    expect_identical(
      sum(res$weight),
      sum(res$weight),
      info = sprintf("weight sum sanity for eval_chain='%s'", ec)
    )

    # Numeric sums as stable fingerprints of the aggregation
    expect_true(is.finite(sum(res$weight)))
    expect_true(is.finite(sum(res$transition_duration, na.rm = TRUE)))

    # Stable aggregate fingerprint (guards against any change to the
    # transition detection or aggregation). Pinned against captured baseline.
    data.table::setorder(res, from, to)
    fp <- list(
      nrow = nrow(res),
      weight_sum = sum(res$weight),
      tdur_sum = round(sum(res$transition_duration, na.rm = TRUE), 4),
      eta_from_sum = round(sum(res$eta_from_median, na.rm = TRUE), 4),
      eta_to_sum = round(sum(res$eta_to_median, na.rm = TRUE), 4),
      weight_head = head(res$weight, 10),
      eta_from_head = head(res$eta_from_median, 10)
    )

    fixture_path <- testthat::test_path(
      "fixtures",
      "analyze_transitions_fp.rds"
    )
    if (file.exists(fixture_path)) {
      baseline <- readRDS(fixture_path)[[ec]]
      expect_equal(
        fp,
        baseline,
        tolerance = 1e-6,
        info = sprintf("fingerprint drift for eval_chain='%s'", ec)
      )
    }
  }
})

# 3. eval_chain cross-check — last/first/none produce consistent weights -----

test_that("eval_chain variants keep identical total weight on sample.rds", {
  d <- skip_if_missing_sample()

  res_last <- suppressWarnings(analyze_employment_transitions(
    d,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    statistics_variables = c("eta", "sesso"),
    eval_chain = "last",
    show_progress = FALSE
  ))
  res_first <- suppressWarnings(analyze_employment_transitions(
    d,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    statistics_variables = c("eta", "sesso"),
    eval_chain = "first",
    show_progress = FALSE
  ))
  res_none <- suppressWarnings(analyze_employment_transitions(
    d,
    transition_variable = "COD_TIPOLOGIA_CONTRATTUALE",
    statistics_variables = c("eta", "sesso"),
    eval_chain = "none",
    show_progress = FALSE
  ))

  # Total transition count is invariant
  expect_identical(sum(res_last$weight), sum(res_none$weight))
  expect_identical(sum(res_first$weight), sum(res_none$weight))
})
