#' Generate Synthetic Employment Data
#'
#' Creates synthetic employment data that mimics the structure and statistical 
#' properties of real employment datasets while ensuring no real personal data 
#' is included. This function is used to generate public-safe test data for 
#' the longworkR package.
#'
#' @param n_individuals Integer. Number of unique individuals to generate (default: 4252)
#' @param n_contracts Integer. Total number of employment contracts to generate (default: 476400)
#' @param start_date Date. Earliest possible contract start date (default: "2021-01-01")
#' @param end_date Date. Latest possible contract end date (default: "2024-12-31")
#' @param seed Integer. Random seed for reproducibility (default: 12345)
#'
#' @return A data.table with synthetic employment data matching the structure 
#'         of vecshift-processed employment records
#'
#' @details
#' The synthetic data generator creates realistic employment patterns including:
#' \itemize{
#'   \item Multiple contracts per individual with realistic durations
#'   \item Employment states (occupied part-time, full-time, unemployed, overlaps)
#'   \item Contract types following Italian employment classification codes
#'   \item Demographic information (age, gender, education level)
#'   \item Geographic distribution across Italian provinces
#'   \item Salary information with realistic distributions
#'   \item Employment transitions and consolidation periods (over_id)
#'   \item Impact evaluation attributes for DiD and policy analysis
#' }
#'
#' @examples
#' \dontrun{
#' # Generate default synthetic dataset
#' synthetic_data <- generate_synthetic_employment_data()
#' 
#' # Generate smaller dataset for testing
#' test_data <- generate_synthetic_employment_data(
#'   n_individuals = 100, 
#'   n_contracts = 1000
#' )
#' 
#' # Save synthetic data for package distribution
#' saveRDS(synthetic_data, "data/synthetic_sample.rds")
#' }
#'
#' @export
generate_synthetic_employment_data <- function(
    n_individuals = 4252,
    n_contracts = 476400,
    start_date = as.Date("2021-01-01"),
    end_date = as.Date("2024-12-31"),
    seed = 12345
) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package is required for synthetic data generation")
  }
  
  library(data.table)
  
  # Define reference data for realistic generation
  contract_types <- c("A.01.00", "A.02.00", "A.03.00", "A.04.02", "B.01.00", 
                     "C.01.00", "D.01.00", "E.01.00", "F.01.00", "G.01.00")
  
  qualifica_codes <- c("1.1.1", "2.1.1", "3.1.1", "4.1.1", "5.1.1", "6.1.1",
                      "7.1.1", "8.1.1", "8.2.2", "9.1.1")
  
  ateco_codes <- c("01.1", "47.1", "56.1", "62.0", "66.2", "68.1", "77.1", 
                  "82.9", "85.2", "87.1", "88.1", "93.1", "96.0", "97.0")
  
  provinces <- c("MILANO", "ROMA", "TORINO", "NAPOLI", "PALERMO", "GENOVA", 
                "BOLOGNA", "FIRENZE", "BARI", "CATANIA", "VENEZIA", "VERONA",
                "MESSINA", "PADOVA", "TRIESTE", "BRESCIA", "PRATO", "TARANTO",
                "REGGIO CALABRIA", "MODENA", "REGGIO EMILIA", "PERUGIA", 
                "RAVENNA", "LIVORNO", "CAGLIARI", "FOGGIA", "RIMINI", "SALERNO",
                "FERRARA", "SASSARI", "MONZA", "SIRACUSA", "PESCARA", "BERGAMO",
                "LATINA", "VICENZA", "TERNI", "FORLÌ", "TRENTO", "NOVARA",
                "PIACENZA", "ANCONA", "ANDRIA", "AREZZO", "UDINE", "CESENA",
                "LECCE", "PESARO", "BARLETTA", "PAVIA")
  
  areas <- c("Milano-metr", "Roma-metr", "Torino-metr", "Napoli-metr", 
            "Pianura", "Montagna", "Collina", "Costa")
  
  education_levels <- c("licenza elementare", "licenza media o inferiore", 
                       "diploma superiore", "universitaria", 
                       "post-universitaria")
  
  genders <- c("M", "F")
  
  stati <- c("occ_pt", "occ_ft", "disoccupato", "over_pt_pt", "over_ft_ft", 
            "over_pt_ft", "over_ft_pt")
  
  # Generate individual-level characteristics
  individuals <- data.table(
    cf = 1:n_individuals,
    eta_base = sample(18:65, n_individuals, replace = TRUE, 
                     prob = c(rep(0.1, 10), rep(0.15, 20), rep(0.1, 18))),
    sesso = sample(genders, n_individuals, replace = TRUE, 
                  prob = c(0.52, 0.48)),
    istruzione = sample(education_levels, n_individuals, replace = TRUE,
                       prob = c(0.05, 0.25, 0.45, 0.22, 0.03)),
    provincia = sample(provinces, n_individuals, replace = TRUE),
    area = sample(areas, n_individuals, replace = TRUE)
  )
  
  # Generate contracts with realistic patterns
  contracts_per_individual <- rpois(n_individuals, lambda = n_contracts / n_individuals)
  contracts_per_individual[contracts_per_individual == 0] <- 1
  
  # Adjust to match target total
  total_contracts <- sum(contracts_per_individual)
  if (total_contracts != n_contracts) {
    # Randomly adjust some individuals
    diff <- n_contracts - total_contracts
    if (diff > 0) {
      # Add contracts
      add_indices <- sample(n_individuals, abs(diff), replace = TRUE)
      for (i in add_indices) contracts_per_individual[i] <- contracts_per_individual[i] + 1
    } else {
      # Remove contracts
      remove_indices <- sample(which(contracts_per_individual > 1), abs(diff), replace = TRUE)
      for (i in remove_indices) contracts_per_individual[i] <- contracts_per_individual[i] - 1
    }
  }
  
  # Generate contract records
  synthetic_data <- data.table()
  id_counter <- 1
  over_id_counter <- 1
  
  for (i in 1:n_individuals) {
    n_contracts_individual <- contracts_per_individual[i]
    individual_data <- individuals[cf == i]
    
    # Generate contract start dates for this individual
    start_dates <- sort(sample(seq(start_date, end_date - 30, by = "day"),
                              n_contracts_individual, replace = TRUE))
    
    individual_contracts <- data.table(
      id = c(id_counter:(id_counter + n_contracts_individual - 1)),
      cf = rep(i, n_contracts_individual),
      inizio = start_dates,
      fine = start_dates + sample(1:1461, n_contracts_individual, replace = TRUE,
                                 prob = exp(-seq(1, 1461)/200)),
      arco = sample(0:3, n_contracts_individual, replace = TRUE, 
                   prob = c(0.25, 0.6, 0.1, 0.05)),
      prior = sample(0:1, n_contracts_individual, replace = TRUE,
                    prob = c(0.65, 0.35)),
      over_id = sample(c(0, over_id_counter:(over_id_counter + n_contracts_individual)), 
                      n_contracts_individual, replace = TRUE),
      stato = sample(stati, n_contracts_individual, replace = TRUE,
                    prob = c(0.3, 0.35, 0.2, 0.05, 0.03, 0.04, 0.03)),
      qualifica = sample(c(NA, qualifica_codes), n_contracts_individual, 
                        replace = TRUE, prob = c(0.3, rep(0.07, 10))),
      ateco = sample(c(NA, ateco_codes), n_contracts_individual, 
                    replace = TRUE, prob = c(0.3, rep(0.05, 14))),
      ore = sample(c(NA, 4, 8, 12, 15, 20, 25, 27, 30, 35, 40, 48), 
                  n_contracts_individual, replace = TRUE,
                  prob = c(0.3, 0.05, 0.08, 0.05, 0.08, 0.15, 0.1, 0.05, 0.05, 0.04, 0.03, 0.02)),
      COD_TIPOLOGIA_CONTRATTUALE = sample(c(NA, contract_types), 
                                         n_contracts_individual, replace = TRUE,
                                         prob = c(0.3, rep(0.07, 10))),
      eta = individual_data$eta_base + sample(-2:5, n_contracts_individual, replace = TRUE),
      sesso = rep(individual_data$sesso, n_contracts_individual),
      istruzione = rep(individual_data$istruzione, n_contracts_individual),
      datore = sample(5251580:5958004, n_contracts_individual, replace = TRUE),
      area = rep(individual_data$area, n_contracts_individual),
      troncata = sample(c(0, NA), n_contracts_individual, replace = TRUE, 
                       prob = c(0.7, 0.3)),
      provincia = rep(individual_data$provincia, n_contracts_individual)
    )
    
    # Fix end dates that go beyond the limit
    individual_contracts[fine > end_date, fine := end_date]
    
    # Calculate durata
    individual_contracts[, durata := as.numeric(fine - inizio + 1)]
    
    # Generate salary data with realistic distributions
    individual_contracts[, retribuzione := ifelse(
      is.na(qualifica), NA,
      pmax(0, round(rnorm(n_contracts_individual, 
                         mean = 15000 + (eta - 25) * 500 + ifelse(prior == 1, 5000, 0),
                         sd = 8000)))
    )]
    
    # Add DiD attributes
    individual_contracts[, did_attribute := sample(0:1, n_contracts_individual, 
                                                   replace = TRUE, prob = c(0.97, 0.03))]
    individual_contracts[, did_distance := ifelse(did_attribute == 1,
                                                  sample(0:1373, sum(did_attribute), replace = TRUE),
                                                  NA)]
    individual_contracts[, did_match_quality := ifelse(
      did_attribute == 1, "matched", 
      ifelse(is.na(qualifica), NA, "none")
    )]
    
    # Add event dates for DiD analysis
    individual_contracts[, event_start := ifelse(
      did_attribute == 1, 
      as.Date("2022-01-01") + sample(0:500, sum(did_attribute), replace = TRUE),
      NA
    )]
    individual_contracts[, event_end := ifelse(
      !is.na(event_start), 
      event_start + sample(1:365, sum(!is.na(event_start)), replace = TRUE),
      NA
    )]
    
    # Convert back to IDate
    individual_contracts[, event_start := as.IDate(event_start)]
    individual_contracts[, event_end := as.IDate(event_end)]
    
    # Add policy attributes
    individual_contracts[, pol_attribute := sample(0:1, n_contracts_individual, 
                                                   replace = TRUE, prob = c(0.96, 0.04))]
    individual_contracts[, pol_distance := ifelse(pol_attribute == 1,
                                                  sample(0:656, sum(pol_attribute), replace = TRUE),
                                                  NA)]
    individual_contracts[, pol_match_quality := ifelse(
      pol_attribute == 1, 
      sample(c("matched", "synthetic"), sum(pol_attribute), replace = TRUE),
      ifelse(is.na(qualifica), NA, "none")
    )]
    individual_contracts[, idpol := ifelse(pol_attribute == 1,
                                          sample(5:540326, sum(pol_attribute), replace = TRUE),
                                          NA)]
    
    # Handle unemployment records (stato == "disoccupato")
    unemployment_mask <- individual_contracts$stato == "disoccupato"
    if (any(unemployment_mask)) {
      cols_to_na <- c("id", "qualifica", "ateco", "ore", "retribuzione", 
                     "COD_TIPOLOGIA_CONTRATTUALE", "eta", "sesso", "istruzione",
                     "datore", "area", "troncata", "provincia", "did_attribute",
                     "did_distance", "did_match_quality", "pol_distance", "pol_match_quality", "idpol")
      
      for (col in cols_to_na) {
        if (col %in% names(individual_contracts)) {
          individual_contracts[unemployment_mask, (col) := NA]
        }
      }
      # Set id to 0 for unemployment records
      individual_contracts[unemployment_mask, id := 0]
      individual_contracts[unemployment_mask, over_id := 0]
    }
    
    synthetic_data <- rbind(synthetic_data, individual_contracts)
    id_counter <- id_counter + n_contracts_individual
    over_id_counter <- over_id_counter + n_contracts_individual
  }
  
  # Convert date columns to IDate
  synthetic_data[, inizio := as.IDate(inizio)]
  synthetic_data[, fine := as.IDate(fine)]
  
  # Final data quality adjustments
  synthetic_data[durata < 1, durata := 1]
  synthetic_data[eta < 16, eta := 16]
  synthetic_data[eta > 70, eta := 70]
  synthetic_data[retribuzione > 1000000, retribuzione := sample(10000:50000, 1)]
  
  # Reorder columns to match original structure
  column_order <- c("id", "cf", "inizio", "fine", "arco", "prior", "over_id", 
                   "durata", "stato", "qualifica", "ateco", "ore", "retribuzione",
                   "COD_TIPOLOGIA_CONTRATTUALE", "eta", "sesso", "istruzione", 
                   "datore", "area", "troncata", "provincia", "did_attribute",
                   "did_distance", "did_match_quality", "event_start", "event_end",
                   "pol_attribute", "pol_distance", "pol_match_quality", "idpol")
  
  synthetic_data <- synthetic_data[, ..column_order]
  
  # Set appropriate classes
  setattr(synthetic_data, "class", c("data.table", "data.frame"))
  
  return(synthetic_data)
}


#' Validate Synthetic Data Quality
#'
#' Compares synthetic data against expected patterns and distributions
#' to ensure it maintains realistic characteristics for testing purposes.
#'
#' @param synthetic_data data.table. The synthetic dataset to validate
#' @param reference_stats list. Optional reference statistics to compare against
#'
#' @return A list containing validation results and quality metrics
#'
#' @export
validate_synthetic_data <- function(synthetic_data, reference_stats = NULL) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package is required for data validation")
  }
  
  library(data.table)
  
  validation_results <- list(
    dimensions = list(
      rows = nrow(synthetic_data),
      cols = ncol(synthetic_data),
      individuals = length(unique(synthetic_data$cf))
    ),
    
    data_quality = list(
      missing_rates = sapply(synthetic_data, function(x) mean(is.na(x))),
      date_range = list(
        min_start = min(synthetic_data$inizio, na.rm = TRUE),
        max_start = max(synthetic_data$inizio, na.rm = TRUE),
        min_end = min(synthetic_data$fine, na.rm = TRUE),
        max_end = max(synthetic_data$fine, na.rm = TRUE)
      )
    ),
    
    distributions = list(
      contract_duration = summary(synthetic_data$durata),
      age_distribution = summary(synthetic_data$eta),
      salary_distribution = summary(synthetic_data$retribuzione),
      gender_balance = table(synthetic_data$sesso, useNA = "ifany"),
      employment_states = table(synthetic_data$stato, useNA = "ifany")
    ),
    
    structural_checks = list(
      has_required_columns = all(c("cf", "inizio", "fine", "durata", "stato") %in% names(synthetic_data)),
      valid_dates = all(synthetic_data$inizio <= synthetic_data$fine, na.rm = TRUE),
      positive_duration = all(synthetic_data$durata > 0, na.rm = TRUE),
      reasonable_ages = all(synthetic_data$eta >= 16 & synthetic_data$eta <= 70, na.rm = TRUE)
    )
  )
  
  # Overall quality score
  quality_checks <- c(
    validation_results$structural_checks$has_required_columns,
    validation_results$structural_checks$valid_dates,
    validation_results$structural_checks$positive_duration,
    validation_results$structural_checks$reasonable_ages,
    validation_results$dimensions$rows > 100000,  # Sufficient size
    validation_results$dimensions$individuals > 1000  # Sufficient individuals
  )
  
  validation_results$overall_quality_score <- mean(quality_checks)
  
  return(validation_results)
}