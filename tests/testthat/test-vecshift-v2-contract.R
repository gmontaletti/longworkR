# Tests for the vecshift v2 input contract helpers
#
# Covers .assert_vecshift_input() and .preserve_vecshift_class() and the
# downstream guarantee that consolidation functions preserve the
# vecshift_result class on their output.

# 1. helpers ----------------------------------------------------------------

make_minimal_dt <- function() {
  data.table::data.table(
    cf = c("A", "A", "B"),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2020-03-01")),
    fine = as.Date(c("2020-05-31", "2020-12-31", "2020-09-30")),
    durata = c(151L, 214L, 214L),
    arco = c(1L, 1L, 1L),
    over_id = c(1L, 2L, 3L),
    id = 1:3,
    prior = c(1, 1, 1)
  )
}

build_real_vecshift_result <- function() {
  raw <- data.table::data.table(
    cf = c("A", "A", "B"),
    inizio = as.Date(c("2020-01-01", "2020-06-01", "2020-03-01")),
    fine = as.Date(c("2020-05-31", "2020-12-31", "2020-09-30")),
    id = 1:3,
    prior = c(1, 1, 1)
  )
  vecshift::vecshift(raw)
}

# 2. .assert_vecshift_input -------------------------------------------------

test_that(".assert_vecshift_input accepts a data.table with required columns", {
  dt <- make_minimal_dt()
  # On a system with vecshift v2 a plain data.table will warn — that is the
  # expected behaviour. We only check that the function does not error.
  expect_error(
    suppressWarnings(longworkR:::.assert_vecshift_input(dt)),
    NA
  )
})

test_that(".assert_vecshift_input errors on non-data.table input", {
  expect_error(
    longworkR:::.assert_vecshift_input(list(a = 1)),
    "must be a data.table"
  )
  expect_error(
    longworkR:::.assert_vecshift_input(data.frame(cf = "A")),
    "must be a data.table"
  )
})

test_that(".assert_vecshift_input errors when required columns are missing", {
  dt <- make_minimal_dt()
  dt[, durata := NULL]
  expect_error(
    suppressWarnings(longworkR:::.assert_vecshift_input(dt)),
    "durata"
  )
})

test_that(".assert_vecshift_input warns on plain data.table when vecshift v2 is installed", {
  skip_if_not_installed("vecshift", minimum_version = "2.0.0")
  dt <- make_minimal_dt()
  expect_warning(
    longworkR:::.assert_vecshift_input(dt),
    "vecshift_result"
  )
})

test_that(".assert_vecshift_input accepts a real vecshift_result without warning", {
  skip_if_not_installed("vecshift", minimum_version = "2.0.0")
  vs <- build_real_vecshift_result()
  expect_silent(
    longworkR:::.assert_vecshift_input(
      vs,
      required_cols = c("cf", "inizio", "fine", "durata", "arco")
    )
  )
})

test_that(".assert_vecshift_input errors when granularity is not 'day'", {
  skip_if_not_installed("vecshift", minimum_version = "2.0.0")
  vs <- build_real_vecshift_result()
  meta <- attr(vs, "vecshift_metadata")
  if (is.null(meta)) {
    meta <- list()
  }
  meta$granularity <- "hour"
  attr(vs, "vecshift_metadata") <- meta
  expect_error(
    longworkR:::.assert_vecshift_input(vs),
    "day-granularity"
  )
})

# 3. .preserve_vecshift_class -----------------------------------------------

test_that(".preserve_vecshift_class round-trips class and metadata", {
  skip_if_not_installed("vecshift", minimum_version = "2.0.0")
  vs <- build_real_vecshift_result()
  out <- data.table::copy(vs)
  class(out) <- c("data.table", "data.frame")
  attr(out, "vecshift_metadata") <- NULL
  res <- longworkR:::.preserve_vecshift_class(out, vs)
  expect_true(inherits(res, "vecshift_result"))
  expect_identical(
    attr(res, "vecshift_metadata"),
    attr(vs, "vecshift_metadata")
  )
})

test_that(".preserve_vecshift_class is a no-op for plain data.tables", {
  dt <- make_minimal_dt()
  out <- data.table::copy(dt)
  res <- longworkR:::.preserve_vecshift_class(out, dt)
  expect_false(inherits(res, "vecshift_result"))
})

# 4. integration: consolidation preserves class -----------------------------

test_that("consolidate_overlapping preserves vecshift_result on output", {
  skip_if_not_installed("vecshift", minimum_version = "2.0.0")
  vs <- build_real_vecshift_result()
  out <- consolidate_overlapping(vs)
  expect_true(inherits(out, "vecshift_result"))
  expect_true(data.table::is.data.table(out))
})
