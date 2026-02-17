# Helper functions for test data generation

# Generate realistic employment data with configurable characteristics
generate_realistic_employment_data <- function(
  n_records,
  n_persons = NULL,
  n_employers = NULL,
  temporal_consolidation_rate = 0.3,
  employer_consolidation_rate = 0.2,
  contract_types = c("A.01.00", "A.03.00", "B.01.00", "C.01.00", "D.01.00"),
  date_range = c(as.Date("2010-01-01"), as.Date("2023-12-31")),
  seed = 42
) {
  set.seed(seed)

  if (is.null(n_persons)) {
    n_persons <- max(10, n_records %/% 8) # Average 8 contracts per person
  }

  if (is.null(n_employers)) {
    n_employers <- max(5, n_records %/% 50) # Average 50 contracts per employer
  }

  # Generate person IDs with realistic distribution (power law)
  cf_weights <- rexp(n_persons, rate = 0.1)
  cf_probs <- cf_weights / sum(cf_weights)
  person_ids <- sample(1:n_persons, n_records, replace = TRUE, prob = cf_probs)

  # Generate employer distribution
  employer_weights <- rexp(n_employers, rate = 0.05)
  employer_probs <- employer_weights / sum(employer_weights)
  employer_ids <- sample(
    paste0("COMP", sprintf("%04d", 1:n_employers)),
    n_records,
    replace = TRUE,
    prob = employer_probs
  )

  # Generate dates with realistic patterns
  start_dates <- sample(
    seq(date_range[1], date_range[2], by = "day"),
    n_records,
    replace = TRUE
  )

  # Generate contract durations with realistic distribution
  durations <- pmax(1, round(rexp(n_records, rate = 1 / 180))) # Average ~180 days, min 1
  end_dates <- start_dates + durations

  # Create base data
  data <- data.table::data.table(
    cf = person_ids,
    inizio = start_dates,
    fine = end_dates,
    durata = as.numeric(durations),
    arco = sample(
      c(0, 1, 2),
      n_records,
      replace = TRUE,
      prob = c(0.05, 0.75, 0.2)
    ),
    CODICE_FISCALE_AZIENDA = employer_ids,
    COD_TIPOLOGIA_CONTRATTUALE = sample(
      contract_types,
      n_records,
      replace = TRUE
    ),
    prior = sample(c(0, 1), n_records, replace = TRUE, prob = c(0.3, 0.7))
  )

  # Sort by person and date for realistic ordering
  data <- data[order(cf, inizio)]

  # Generate over_id with consolidation opportunities
  data[, over_id := seq_len(.N), by = cf]

  # Introduce temporal consolidation opportunities
  if (temporal_consolidation_rate > 0) {
    consolidation_candidates <- data[,
      .I[runif(.N) < temporal_consolidation_rate],
      by = cf
    ]$V1
    if (length(consolidation_candidates) > 0) {
      # Create pairs of contracts with same over_id
      pairs <- matrix(
        sample(
          consolidation_candidates,
          2 * (length(consolidation_candidates) %/% 2)
        ),
        ncol = 2
      )
      for (i in seq_len(nrow(pairs))) {
        same_over_id <- max(data$over_id) + i
        data[pairs[i, ], over_id := same_over_id]
      }
    }
  }

  # Add some realistic columns for contract analysis
  data[,
    retribuzioni_lorde_somma := round(runif(.N, 1000, 5000) * durata / 30, 2)
  ]
  data[, ore_lavorate_somma := round(runif(.N, 20, 40) * durata / 7, 1)]

  return(data)
}
