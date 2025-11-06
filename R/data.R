#' Sample Employment Data
#'
#' A sample dataset containing longitudinal employment records processed by the
#' vecshift package. This dataset is suitable for testing and demonstrating
#' longworkR functions for career trajectory analysis, survival analysis, and
#' impact evaluation.
#'
#' @format A data.table with 434,103 rows and 28 columns:
#' \describe{
#'   \item{id}{Unique record identifier}
#'   \item{cf}{Person identifier (codice fiscale anonymized)}
#'   \item{inizio}{Contract start date (Date)}
#'   \item{fine}{Contract end date (Date)}
#'   \item{arco}{Concurrent employment indicator (logical)}
#'   \item{prior}{Employment intensity (1 = full-time, 0-1 = part-time/other)}
#'   \item{over_id}{Employment period identifier for consolidation}
#'   \item{durata}{Contract duration in days}
#'   \item{qualifica}{Job qualification code}
#'   \item{ateco}{Economic activity sector code (ATECO)}
#'   \item{ore}{Working hours per week}
#'   \item{retribuzione}{Salary/compensation amount}
#'   \item{COD_TIPOLOGIA_CONTRATTUALE}{Contract type code (X.01.00 format)}
#'   \item{eta}{Age at contract start}
#'   \item{sesso}{Gender (M/F)}
#'   \item{istruzione}{Education level}
#'   \item{datore}{Employer identifier (anonymized)}
#'   \item{area}{Geographic area code}
#'   \item{troncata}{Truncation indicator for administrative censoring}
#'   \item{provincia}{Province code}
#'   \item{COMUNE_LAVORATORE}{Municipality code for worker}
#'   \item{stato}{Employment state/status}
#'   \item{did_attribute}{Difference-in-differences attribute for matching}
#'   \item{did_distance}{Distance metric for DiD matching}
#'   \item{did_match_quality}{Quality score for DiD match}
#'   \item{pol_attribute}{Policy evaluation attribute}
#'   \item{pol_distance}{Distance metric for policy matching}
#'   \item{pol_match_quality}{Quality score for policy match}
#' }
#'
#' @details
#' This dataset has been processed by the vecshift package and includes:
#' \itemize{
#'   \item Employment period consolidation via \code{over_id}
#'   \item Concurrent employment detection via \code{arco}
#'   \item Employment intensity adjustments via \code{prior}
#'   \item Impact evaluation matching variables (DiD and policy attributes)
#' }
#'
#' The data is suitable for:
#' \itemize{
#'   \item Career trajectory analysis
#'   \item Contract survival analysis
#'   \item Employment transition network analysis
#'   \item Impact evaluation (DiD, PSM, RDD methods)
#' }
#'
#' @source Synthetic data based on Italian administrative employment records
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(sample)
#'
#' # Basic exploration
#' str(sample)
#' summary(sample)
#'
#' # Analyze employment transitions
#' transitions <- analyze_employment_transitions(sample)
#'
#' # Estimate contract survival
#' survival_results <- estimate_contract_survival_optimized(sample)
#' }
"sample"

#' Synthetic Sample Employment Data with Policy Events
#'
#' An extended synthetic dataset containing longitudinal employment records
#' with simulated policy intervention events. Specifically designed for
#' demonstrating impact evaluation methods including difference-in-differences
#' (DiD), propensity score matching (PSM), and regression discontinuity
#' design (RDD).
#'
#' @format A data.table with 476,400 rows and 30 columns:
#' \describe{
#'   \item{id}{Unique record identifier}
#'   \item{cf}{Person identifier (codice fiscale anonymized)}
#'   \item{inizio}{Contract start date (Date)}
#'   \item{fine}{Contract end date (Date)}
#'   \item{arco}{Concurrent employment indicator (logical)}
#'   \item{prior}{Employment intensity (1 = full-time, 0-1 = part-time/other)}
#'   \item{over_id}{Employment period identifier for consolidation}
#'   \item{durata}{Contract duration in days}
#'   \item{stato}{Employment state/status}
#'   \item{qualifica}{Job qualification code}
#'   \item{ateco}{Economic activity sector code (ATECO)}
#'   \item{ore}{Working hours per week}
#'   \item{retribuzione}{Salary/compensation amount}
#'   \item{COD_TIPOLOGIA_CONTRATTUALE}{Contract type code (X.01.00 format)}
#'   \item{eta}{Age at contract start}
#'   \item{sesso}{Gender (M/F)}
#'   \item{istruzione}{Education level}
#'   \item{datore}{Employer identifier (anonymized)}
#'   \item{area}{Geographic area code}
#'   \item{troncata}{Truncation indicator for administrative censoring}
#'   \item{provincia}{Province code}
#'   \item{did_attribute}{Difference-in-differences attribute for matching}
#'   \item{did_distance}{Distance metric for DiD matching}
#'   \item{did_match_quality}{Quality score for DiD match}
#'   \item{event_start}{Policy event start date}
#'   \item{event_end}{Policy event end date}
#'   \item{pol_attribute}{Policy evaluation attribute}
#'   \item{pol_distance}{Distance metric for policy matching}
#'   \item{pol_match_quality}{Quality score for policy match}
#'   \item{idpol}{Policy identifier linking individuals to specific interventions}
#' }
#'
#' @details
#' This synthetic dataset extends the base sample with:
#' \itemize{
#'   \item Simulated policy intervention events (\code{event_start}, \code{event_end})
#'   \item Policy identifiers (\code{idpol}) linking individuals to interventions
#'   \item Enhanced matching variables for causal inference
#' }
#'
#' The data is specifically designed for:
#' \itemize{
#'   \item Difference-in-differences (DiD) analysis
#'   \item Propensity score matching (PSM)
#'   \item Regression discontinuity design (RDD)
#'   \item Comparative impact evaluation across multiple policies
#' }
#'
#' @source Synthetic data generated for impact evaluation demonstrations
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(synthetic_sample)
#'
#' # Identify policy events
#' events <- identify_treatment_events(
#'   synthetic_sample,
#'   id_var = "cf",
#'   event_date_var = "event_start"
#' )
#'
#' # Run DiD analysis
#' did_results <- estimate_impact_did(
#'   synthetic_sample,
#'   outcome_var = "durata",
#'   treatment_var = "idpol"
#' )
#' }
"synthetic_sample"
