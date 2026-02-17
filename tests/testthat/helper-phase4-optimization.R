# Helper Functions for Phase 4 Optimization Testing
# Single-Period Worker Split Strategy

#' Generate Mixed Dataset with Single and Multi-Period Workers
#'
#' Creates a test dataset with a controlled mix of workers having single
#' vs. multiple employment periods. Useful for testing and benchmarking
#' the Phase 4 single-period worker split optimization.
#'
#' @param n_single Number of workers with exactly 1 employment period
#' @param n_multi Number of workers with multiple employment periods
#' @param periods_per_multi Average number of periods for multi-period workers (default: 3)
#' @param add_overlaps Logical, whether to add overlapping periods (over_id > 0)
#' @param add_gaps Logical, whether to add gaps between periods
#' @param seed Random seed for reproducibility
#'
#' @return data.table with employment records
#' @keywords internal
generate_mixed_employment_data <- function(
  n_single = 500,
  n_multi = 500,
  periods_per_multi = 3,
  add_overlaps = TRUE,
  add_gaps = TRUE,
  seed = 12345
) {
  set.seed(seed)

  library(data.table)

  # 1. Generate single-period workers -----
  single_workers <- data.table(
    cf = paste0("S", 1:n_single),
    inizio = as.Date("2020-01-01") + sample(0:730, n_single, replace = TRUE),
    durata = sample(90:365, n_single, replace = TRUE)
  )
  single_workers[, fine := inizio + durata - 1]
  single_workers[, arco := 1L]
  single_workers[, over_id := 0L]
  single_workers[,
    COD_TIPOLOGIA_CONTRATTUALE := sample(
      c("A.01.00", "A.03.00", "A.04.00"),
      nrow(single_workers),
      replace = TRUE
    )
  ]
  single_workers[,
    prior := sample(
      c(0, 1),
      nrow(single_workers),
      replace = TRUE,
      prob = c(0.3, 0.7)
    )
  ]

  # 2. Generate multi-period workers -----
  multi_records <- lapply(1:n_multi, function(i) {
    n_periods <- rpois(1, lambda = periods_per_multi - 1) + 2 # At least 2 periods
    n_periods <- min(n_periods, 10) # Cap at 10

    cf_id <- paste0("M", i)

    # Generate sequential periods with possible gaps
    start_date <- as.Date("2020-01-01") + sample(0:365, 1)

    periods <- data.table(
      cf = cf_id,
      period_num = 1:n_periods
    )

    # Generate dates with gaps or adjacent
    periods[, inizio := start_date]
    periods[, durata := sample(60:180, .N, replace = TRUE)]

    for (j in 2:n_periods) {
      prev_end <- periods[j - 1, inizio + durata - 1]

      if (add_gaps && runif(1) < 0.5) {
        # Add gap (unemployment)
        gap_days <- sample(1:60, 1)
        periods[j, inizio := prev_end + gap_days + 1]
      } else {
        # Adjacent (next day)
        periods[j, inizio := prev_end + 1]
      }
    }

    periods[, fine := inizio + durata - 1]
    periods[, arco := 1L]

    # Initialize over_id to 0
    periods[, over_id := 0L]

    # Add overlapping periods if requested
    if (add_overlaps && runif(1) < 0.3) {
      # Mark some periods as overlapping
      overlap_indices <- sample(1:n_periods, min(2, n_periods))
      periods[
        overlap_indices,
        over_id := sample(1:5, length(overlap_indices), replace = TRUE)
      ]
    }

    periods[,
      COD_TIPOLOGIA_CONTRATTUALE := sample(
        c("A.01.00", "A.03.00", "A.04.00"),
        .N,
        replace = TRUE
      )
    ]
    periods[, prior := sample(c(0, 1), .N, replace = TRUE, prob = c(0.3, 0.7))]
    periods[, period_num := NULL]

    return(periods)
  })

  multi_workers <- rbindlist(multi_records)

  # 3. Combine datasets -----
  all_data <- rbindlist(
    list(single_workers, multi_workers),
    use.names = TRUE,
    fill = TRUE
  )

  # 4. Sort by cf and date -----
  setkey(all_data, cf, inizio, fine)

  # 5. Add some additional columns for realism -----
  all_data[, retribuzione := round(runif(.N, 1000, 3000), 2)]
  all_data[,
    ore := sample(c(20, 30, 40), .N, replace = TRUE, prob = c(0.2, 0.3, 0.5))
  ]

  return(all_data)
}

#' Get Single-Period Worker Statistics from Dataset
#'
#' @param data data.table with employment records
#' @return List with statistics
#' @keywords internal
get_single_period_stats <- function(data) {
  periods_per_worker <- data[, .N, by = cf]

  n_single <- sum(periods_per_worker$N == 1)
  n_multi <- sum(periods_per_worker$N > 1)
  total_workers <- nrow(periods_per_worker)

  pct_single <- round(n_single / total_workers * 100, 1)
  pct_multi <- round(n_multi / total_workers * 100, 1)

  avg_periods_multi <- if (n_multi > 0) {
    round(mean(periods_per_worker[N > 1, N]), 2)
  } else {
    NA_real_
  }

  list(
    total_workers = total_workers,
    n_single = n_single,
    n_multi = n_multi,
    pct_single = pct_single,
    pct_multi = pct_multi,
    avg_periods_multi = avg_periods_multi,
    total_records = nrow(data),
    records_single = sum(periods_per_worker[N == 1, N]),
    records_multi = sum(periods_per_worker[N > 1, N])
  )
}
