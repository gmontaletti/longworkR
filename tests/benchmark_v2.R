#!/usr/bin/env Rscript
# Benchmark: consolidation engine v1 vs v2
# Compares the original data.table J-expression engine (v1) with the
# collapse-native engine (v2) across all consolidation functions.
#
# Usage:
#   Rscript tests/benchmark_v2.R
#   BENCHMARK_FULL=1 Rscript tests/benchmark_v2.R  # include 1M records

library(longworkR)
library(data.table)

cat("=== longworkR Consolidation Engine v2 Benchmark ===\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version:", R.version.string, "\n")
cat("data.table version:", as.character(packageVersion("data.table")), "\n")

# Check if collapse is available (used by v2 engine)
if (requireNamespace("collapse", quietly = TRUE)) {
  cat("collapse version:", as.character(packageVersion("collapse")), "\n")
} else {
  cat("collapse: NOT INSTALLED (v2 engine may not work)\n")
}
cat("\n")

# 1. Generate synthetic benchmark data -----

generate_benchmark_data <- function(n_records) {
  n_persons <- max(1L, as.integer(n_records / 3))
  dt <- data.table(
    cf = rep(seq_len(n_persons), each = 3)[seq_len(n_records)],
    inizio = as.IDate("2020-01-01") + sample(0:1000, n_records, replace = TRUE),
    durata = sample(30:365, n_records, replace = TRUE),
    arco = sample(c(0L, 1L, 1L, 1L), n_records, replace = TRUE),
    over_id = sample(
      c(0L, 0L, 0L, 1L, 1L, 2L, 2L, 3L),
      n_records,
      replace = TRUE
    ),
    stato = sample(
      c("fulltime", "parttime", "inoccupato"),
      n_records,
      replace = TRUE
    ),
    retribuzione = round(runif(n_records, 800, 3000), 2),
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      paste0("A.", sprintf("%02d", 1:5), ".00"),
      n_records,
      replace = TRUE
    ),
    datore = sample(
      seq_len(max(1L, n_records %/% 10)),
      n_records,
      replace = TRUE
    ),
    qualifica = sample(
      c("6.4.1", "5.2.1", "3.1.2", "7.3.1"),
      n_records,
      replace = TRUE
    ),
    prior = sample(c(0, 1, 1, 1), n_records, replace = TRUE)
  )
  dt[, fine := inizio + durata - 1L]
  setkey(dt, cf, inizio)
  return(dt)
}

# 2. Define benchmark sizes -----

sizes <- c(10000L, 100000L)
if (interactive() || Sys.getenv("BENCHMARK_FULL") == "1") {
  sizes <- c(sizes, 1000000L)
}

cat(
  "Benchmark sizes:",
  paste(format(sizes, big.mark = ","), collapse = ", "),
  "records\n\n"
)

# 3. Run per-function benchmarks -----

results <- list()

for (n in sizes) {
  cat(sprintf("\n--- %s records ---\n", format(n, big.mark = ",")))
  set.seed(42)
  dt <- generate_benchmark_data(n)

  fns <- list(
    consolidate_overlapping = list(fn = consolidate_overlapping, args = list()),
    consolidate_adjacent = list(fn = consolidate_adjacent, args = list()),
    consolidate_short_gaps = list(
      fn = consolidate_short_gaps,
      args = list(max_gap_days = 30)
    )
  )

  for (fn_name in names(fns)) {
    fn <- fns[[fn_name]]$fn
    args <- fns[[fn_name]]$args

    # v1
    gc(verbose = FALSE)
    t1 <- system.time({
      r1 <- do.call(
        fn,
        c(list(data = data.table::copy(dt), engine = "v1"), args)
      )
    })

    # v2
    gc(verbose = FALSE)
    t2 <- system.time({
      r2 <- do.call(
        fn,
        c(list(data = data.table::copy(dt), engine = "v2"), args)
      )
    })

    elapsed_v1 <- max(t1["elapsed"], 0.001)
    elapsed_v2 <- max(t2["elapsed"], 0.001)
    throughput_v1 <- round(n / elapsed_v1)
    throughput_v2 <- round(n / elapsed_v2)
    speedup <- round(elapsed_v1 / elapsed_v2, 1)

    cat(sprintf("  %s:\n", fn_name))
    cat(sprintf(
      "    v1: %.3fs (%s rec/sec)\n",
      t1["elapsed"],
      format(throughput_v1, big.mark = ",")
    ))
    cat(sprintf(
      "    v2: %.3fs (%s rec/sec)\n",
      t2["elapsed"],
      format(throughput_v2, big.mark = ",")
    ))
    cat(sprintf("    Speedup: %.1fx\n", speedup))
    cat(sprintf(
      "    Rows match: %s  |  Cols match: %s\n",
      nrow(r1) == nrow(r2),
      identical(sort(names(r1)), sort(names(r2)))
    ))

    results[[paste(fn_name, n, sep = "_")]] <- data.table(
      func = fn_name,
      n = n,
      v1_sec = t1["elapsed"],
      v2_sec = t2["elapsed"],
      speedup = speedup,
      rows_match = nrow(r1) == nrow(r2)
    )
  }
}

# 4. Full chain comparison -----

cat("\n--- Full chain: overlapping -> adjacent -> short_gaps ---\n")
for (n in sizes[seq_len(min(2L, length(sizes)))]) {
  set.seed(42)
  dt <- generate_benchmark_data(n)

  gc(verbose = FALSE)
  t1 <- system.time({
    r1 <- dt |>
      consolidate_overlapping(engine = "v1") |>
      consolidate_adjacent(engine = "v1") |>
      consolidate_short_gaps(max_gap_days = 30, engine = "v1")
  })

  gc(verbose = FALSE)
  t2 <- system.time({
    r2 <- dt |>
      consolidate_overlapping(engine = "v2") |>
      consolidate_adjacent(engine = "v2") |>
      consolidate_short_gaps(max_gap_days = 30, engine = "v2")
  })

  elapsed_v1 <- max(t1["elapsed"], 0.001)
  elapsed_v2 <- max(t2["elapsed"], 0.001)
  speedup <- round(elapsed_v1 / elapsed_v2, 1)

  cat(sprintf("  %s records:\n", format(n, big.mark = ",")))
  cat(sprintf("    v1 chain: %.3fs\n", t1["elapsed"]))
  cat(sprintf("    v2 chain: %.3fs\n", t2["elapsed"]))
  cat(sprintf("    Chain speedup: %.1fx\n", speedup))
  cat(sprintf("    Rows match: %s\n", nrow(r1) == nrow(r2)))

  results[[paste("full_chain", n, sep = "_")]] <- data.table(
    func = "full_chain",
    n = n,
    v1_sec = t1["elapsed"],
    v2_sec = t2["elapsed"],
    speedup = speedup,
    rows_match = nrow(r1) == nrow(r2)
  )
}

# 5. Real data benchmark (if available) -----

sample_file <- "data/sample.rds"
if (file.exists(sample_file)) {
  cat("\n--- Real sample data ---\n")
  real_data <- readRDS(sample_file)
  n_real <- nrow(real_data)
  cat(sprintf(
    "  Records: %s  |  Persons: %s\n",
    format(n_real, big.mark = ","),
    format(length(unique(real_data$cf)), big.mark = ",")
  ))

  gc(verbose = FALSE)
  t1 <- system.time({
    r1 <- real_data |>
      consolidate_overlapping(engine = "v1") |>
      consolidate_adjacent(engine = "v1") |>
      consolidate_short_gaps(max_gap_days = 30, engine = "v1")
  })

  gc(verbose = FALSE)
  t2 <- system.time({
    r2 <- real_data |>
      consolidate_overlapping(engine = "v2") |>
      consolidate_adjacent(engine = "v2") |>
      consolidate_short_gaps(max_gap_days = 30, engine = "v2")
  })

  elapsed_v1 <- max(t1["elapsed"], 0.001)
  elapsed_v2 <- max(t2["elapsed"], 0.001)
  speedup <- round(elapsed_v1 / elapsed_v2, 1)

  cat(sprintf("  v1 full chain: %.3fs\n", t1["elapsed"]))
  cat(sprintf("  v2 full chain: %.3fs\n", t2["elapsed"]))
  cat(sprintf("  Speedup: %.1fx\n", speedup))
  cat(sprintf("  Rows match: %s\n", nrow(r1) == nrow(r2)))
  cat(sprintf(
    "  Output rows: v1=%s  v2=%s\n",
    format(nrow(r1), big.mark = ","),
    format(nrow(r2), big.mark = ",")
  ))

  results[["real_data"]] <- data.table(
    func = "real_data_chain",
    n = n_real,
    v1_sec = t1["elapsed"],
    v2_sec = t2["elapsed"],
    speedup = speedup,
    rows_match = nrow(r1) == nrow(r2)
  )
}

# 6. Summary -----

cat("\n=== Summary ===\n")
summary_dt <- rbindlist(results)
print(summary_dt)

if (any(!summary_dt$rows_match)) {
  cat("\nWARNING: Some row counts did not match between v1 and v2.\n")
} else {
  cat("\nAll row counts matched between v1 and v2.\n")
}

cat("\nBenchmark complete.\n")
