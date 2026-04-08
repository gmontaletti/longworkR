#' Unified per-person career profile
#'
#' Computes a tidy per-`cf` data.table of career indicators from a
#' `vecshift` output. `career_profile()` is the single entry point that
#' replaces the legacy family of `calculate_career_*_metrics()` functions
#' in longworkR v0.9.0. It always returns a *core* set of six indicators
#' and accepts an `indicators` vector to opt into optional groups.
#'
#' Contract quality is derived from survival analysis only: per project
#' policy (`CLAUDE.md` CRITICAL CORRECTION), fixed weights are forbidden.
#' The shared survival-based lookup is built once via
#' [`.build_contract_quality_lookup()`][contract_quality_lookup] and
#' reused across all indicator groups.
#'
#' @section Boundary with `analyze_employment_transitions()`:
#' The `"transitions"` group returns only per-`cf` *summaries*
#' (`n_upward`, `n_downward`, `quality_slope`). Full pairwise transition
#' matrices and network objects remain the responsibility of
#' [analyze_employment_transitions()].
#'
#' @section Class preservation:
#' `career_profile()` is an aggregation: it collapses the input to one
#' row per `cf`. The result is therefore a plain `data.table` (keyed by
#' `cf`), even when the input carries the `vecshift_result` S3 class.
#' Period-level vecshift metadata does not apply to the aggregated output.
#'
#' @param data A `data.table` (or `vecshift_result`) produced by
#'   `vecshift::vecshift()`. Must contain at least `cf`, `inizio`, `fine`,
#'   `durata`, `arco`, `over_id`, and `prior`.
#' @param indicators Character vector. Which indicator groups to include.
#'   `"core"` is always returned. Valid values:
#'   `c("core", "stability", "complexity", "transitions", "wages", "all")`.
#'   `"all"` expands to all four optional groups. Defaults to `"core"`.
#' @param quality_source Character scalar. How `contract_quality` is
#'   computed. Only `"survival"` is accepted in v0.9.0; any other value
#'   errors with a message flagging the reserved future-release API.
#' @param reference_date Optional `Date` or date-coercible scalar used as
#'   the right-censoring anchor for `employment_rate` and `gap_share`.
#'   Defaults to `max(data$fine)`.
#'
#' @return A `data.table` with one row per `cf`, keyed by `cf`. Columns
#'   depend on `indicators`:
#'
#' **Core (always returned)**
#' \describe{
#'   \item{`cf`}{Person identifier.}
#'   \item{`n_periods`}{Distinct consolidated employment periods
#'     (`uniqueN(over_id[arco >= 1])`).}
#'   \item{`total_days_employed`}{Total days spent in `arco >= 1` spells.}
#'   \item{`employment_rate`}{`total_days_employed / observation_window`,
#'     clamped to `[0, 1]`.}
#'   \item{`contract_quality`}{Duration-weighted mean of survival-derived
#'     quality scores per contract type (`arco >= 1` rows only).}
#'   \item{`intensity`}{Duration-weighted mean of `prior` where
#'     `arco >= 1`.}
#' }
#'
#' **Stability** (`"stability"` or `"all"`)
#' \describe{
#'   \item{`spell_cv`}{Coefficient of variation of per-`over_id` total
#'     durations.}
#'   \item{`gap_share`}{Share of observation window in unemployment
#'     (`arco == 0`).}
#'   \item{`max_tenure_days`}{Longest single `over_id` cumulative
#'     duration.}
#' }
#'
#' **Complexity** (`"complexity"` or `"all"`)
#' \describe{
#'   \item{`n_contract_types`}{Number of distinct contract-type codes.}
#'   \item{`employer_hhi`}{Herfindahl index over employer durata-weighted
#'     shares. `NA` if no employer column is detected.}
#'   \item{`type_entropy`}{Shannon entropy (natural log) of contract-type
#'     duration shares.}
#' }
#'
#' **Transitions** (`"transitions"` or `"all"`)
#' \describe{
#'   \item{`n_upward`}{Count of consecutive `over_id` pairs where
#'     contract quality increases.}
#'   \item{`n_downward`}{Count of consecutive pairs where it decreases.}
#'   \item{`quality_slope`}{OLS slope of quality against `over_id` rank.
#'     `NA` if fewer than two `over_id`s.}
#' }
#'
#' **Wages** (`"wages"` or `"all"`)
#' \describe{
#'   \item{`wage_mean`}{Duration-weighted mean wage.}
#'   \item{`wage_median`}{Duration-weighted median wage
#'     (via `matrixStats::weightedMedian`).}
#'   \item{`wage_growth`}{`(last - first) / first` where the wage is
#'     computed per `over_id`; `NA` with fewer than two `over_id`s.}
#' }
#'
#' @examples
#' \dontrun{
#' sample_data <- readRDS(system.file("extdata", "sample.rds",
#'                                    package = "longworkR"))
#'
#' # Core only
#' core <- career_profile(sample_data)
#'
#' # Core + stability
#' stab <- career_profile(sample_data, indicators = c("core", "stability"))
#'
#' # Everything
#' full <- career_profile(sample_data, indicators = "all")
#' }
#'
#' @seealso [analyze_employment_transitions()] for full transition
#'   matrices, [estimate_contract_survival_optimized()] for the underlying
#'   survival estimator.
#'
#' @importFrom data.table is.data.table setkeyv setorderv copy :=
#' @importFrom data.table uniqueN setnames
#' @importFrom collapse GRP fsum fmean
#' @importFrom matrixStats weightedMedian
#' @importFrom stats lm coef setNames
#' @export
career_profile <- function(
  data,
  indicators = "core",
  quality_source = "survival",
  reference_date = NULL
) {
  # 1. Validate inputs -----
  .assert_vecshift_input(
    data,
    required_cols = c(
      "cf",
      "inizio",
      "fine",
      "durata",
      "arco",
      "over_id",
      "prior"
    )
  )

  if (!is.character(quality_source) || length(quality_source) != 1L) {
    stop("`quality_source` must be a single character value.", call. = FALSE)
  }
  if (!identical(quality_source, "survival")) {
    stop(
      "`quality_source = \"",
      quality_source,
      "\"` is reserved for future releases; ",
      "only \"survival\" is supported in longworkR v0.9.0.",
      call. = FALSE
    )
  }

  valid_groups <- c(
    "core",
    "stability",
    "complexity",
    "transitions",
    "wages",
    "all"
  )
  if (!is.character(indicators) || length(indicators) == 0L) {
    stop(
      "`indicators` must be a non-empty character vector.",
      call. = FALSE
    )
  }
  bad <- setdiff(indicators, valid_groups)
  if (length(bad) > 0L) {
    stop(
      "Unknown indicator group(s): ",
      paste(shQuote(bad), collapse = ", "),
      ". Valid values: ",
      paste(shQuote(valid_groups), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if ("all" %in% indicators) {
    indicators <- c("core", "stability", "complexity", "transitions", "wages")
  }
  indicators <- unique(c("core", indicators))

  # 2. Handle empty input -----
  empty_out <- .empty_career_profile(indicators)
  if (nrow(data) == 0L) {
    return(empty_out)
  }

  # 3. Reference date for observation window -----
  if (is.null(reference_date)) {
    reference_date <- max(data[["fine"]], na.rm = TRUE)
  } else {
    reference_date <- tryCatch(
      as.Date(reference_date),
      error = function(e) {
        stop(
          "`reference_date` must be a Date or coercible to one.",
          call. = FALSE
        )
      }
    )
  }

  # 4. Build shared survival-based quality lookup (once) -----
  quality_lookup <- .build_contract_quality_lookup(data)
  has_type_col <- "COD_TIPOLOGIA_CONTRATTUALE" %in% names(data)

  # Detect employer column once
  employer_col <- .detect_employer_column(data)

  # Detect wage column once
  wage_col <- .detect_wage_column(data)
  if ("wages" %in% indicators && is.null(wage_col)) {
    message(
      "No wage column detected (searched: retribuzione, ",
      "retribuzione_giornaliera, wage, salary, stipendio); ",
      "wage indicators will be NA."
    )
  }

  # 5. Attach row-level contract quality (arco >= 1 only) -----
  # Use a shallow copy of the columns we need to avoid mutating the caller.
  work <- data[,
    .SD,
    .SDcols = intersect(
      c(
        "cf",
        "inizio",
        "fine",
        "durata",
        "arco",
        "over_id",
        "prior",
        "COD_TIPOLOGIA_CONTRATTUALE",
        employer_col,
        wage_col
      ),
      names(data)
    )
  ]

  if (has_type_col && length(quality_lookup) > 0L) {
    work[, .cq := quality_lookup[COD_TIPOLOGIA_CONTRATTUALE]]
  } else {
    work[, .cq := NA_real_]
  }

  # 6. Compute core indicators -----
  core <- .career_profile_core(work, reference_date)

  out <- core
  data.table::setkeyv(out, "cf")

  # 7. Optional groups -----
  if ("stability" %in% indicators) {
    stab <- .career_profile_stability(work, reference_date)
    out <- out[stab, on = "cf"]
  }

  if ("complexity" %in% indicators) {
    comp <- .career_profile_complexity(work, employer_col)
    out <- out[comp, on = "cf"]
  }

  if ("transitions" %in% indicators) {
    tr <- .career_profile_transitions(work)
    out <- out[tr, on = "cf"]
  }

  if ("wages" %in% indicators) {
    wg <- .career_profile_wages(work, wage_col)
    out <- out[wg, on = "cf"]
  }

  data.table::setkeyv(out, "cf")
  out[]
}


# 2. Core indicators -----

.career_profile_core <- function(work, reference_date) {
  # arco >= 1 mask for employment-only aggregates
  employed <- work[arco >= 1L]

  # Employment window anchor per cf from full data (min inizio)
  anchor <- work[,
    list(first_inizio = min(inizio, na.rm = TRUE)),
    by = cf
  ]

  # Employed aggregates per cf
  emp <- employed[,
    list(
      n_periods = data.table::uniqueN(over_id),
      total_days_employed = sum(durata, na.rm = TRUE),
      contract_quality = .wmean(.cq, durata),
      intensity = .wmean(prior, durata)
    ),
    by = cf
  ]

  out <- anchor[emp, on = "cf"]
  # Anchor may legitimately match only for cf present in emp; use coalesce
  out[is.na(first_inizio), first_inizio := reference_date]

  window_days <- as.numeric(reference_date - out$first_inizio) + 1
  window_days[window_days <= 0] <- NA_real_
  out[,
    employment_rate := pmin(
      1,
      pmax(0, total_days_employed / window_days)
    )
  ]

  # Include cf with no employed rows (all-unemployed): pad with zeros
  all_cf <- unique(work$cf)
  missing_cf <- setdiff(all_cf, out$cf)
  if (length(missing_cf) > 0L) {
    pad <- data.table::data.table(
      cf = missing_cf,
      first_inizio = as.Date(NA),
      n_periods = 0L,
      total_days_employed = 0,
      contract_quality = NA_real_,
      intensity = NA_real_,
      employment_rate = 0
    )
    out <- data.table::rbindlist(list(out, pad), use.names = TRUE)
  }

  out[, first_inizio := NULL]
  out[, list(
    cf,
    n_periods = as.integer(n_periods),
    total_days_employed = as.numeric(total_days_employed),
    employment_rate = as.numeric(employment_rate),
    contract_quality = as.numeric(contract_quality),
    intensity = as.numeric(intensity)
  )]
}


# 3. Stability indicators -----

.career_profile_stability <- function(work, reference_date) {
  employed <- work[arco >= 1L]

  # Per over_id total durations (employed only)
  spell_dur <- employed[,
    list(spell_days = sum(durata, na.rm = TRUE)),
    by = list(cf, over_id)
  ]

  stab_emp <- spell_dur[,
    list(
      spell_cv = .cv(spell_days),
      max_tenure_days = if (.N > 0L) max(spell_days, na.rm = TRUE) else 0
    ),
    by = cf
  ]

  # Gap share: unemployment days / observation window
  gap_days <- work[
    arco == 0L,
    list(gap_days = sum(durata, na.rm = TRUE)),
    by = cf
  ]
  anchor <- work[,
    list(first_inizio = min(inizio, na.rm = TRUE)),
    by = cf
  ]
  anchor[, window_days := as.numeric(reference_date - first_inizio) + 1]
  anchor[window_days <= 0, window_days := NA_real_]

  gap <- gap_days[anchor, on = "cf"]
  gap[is.na(gap_days), gap_days := 0]
  gap[, gap_share := pmin(1, pmax(0, gap_days / window_days))]

  out <- stab_emp[gap[, list(cf, gap_share)], on = "cf"]
  # cf with no employed rows get NA spell_cv and max_tenure_days = 0
  out[is.na(max_tenure_days), max_tenure_days := 0]

  out[, list(
    cf,
    spell_cv = as.numeric(spell_cv),
    gap_share = as.numeric(gap_share),
    max_tenure_days = as.numeric(max_tenure_days)
  )]
}


# 4. Complexity indicators -----

.career_profile_complexity <- function(work, employer_col) {
  employed <- work[arco >= 1L]
  has_type <- "COD_TIPOLOGIA_CONTRATTUALE" %in% names(employed)

  all_cf <- unique(work$cf)

  # 4.1 n_contract_types & type_entropy -----
  if (has_type && nrow(employed) > 0L) {
    type_shares <- employed[
      !is.na(COD_TIPOLOGIA_CONTRATTUALE),
      list(d = sum(durata, na.rm = TRUE)),
      by = list(cf, COD_TIPOLOGIA_CONTRATTUALE)
    ]
    type_summary <- type_shares[,
      list(
        n_contract_types = data.table::uniqueN(COD_TIPOLOGIA_CONTRATTUALE),
        type_entropy = .shannon(d)
      ),
      by = cf
    ]
  } else {
    type_summary <- data.table::data.table(
      cf = all_cf,
      n_contract_types = NA_integer_,
      type_entropy = NA_real_
    )
  }

  # 4.2 employer_hhi -----
  if (!is.null(employer_col) && nrow(employed) > 0L) {
    emp_shares <- employed[
      !is.na(get(employer_col)),
      list(d = sum(durata, na.rm = TRUE)),
      by = c("cf", employer_col)
    ]
    hhi_summary <- emp_shares[,
      list(employer_hhi = .hhi(d)),
      by = cf
    ]
  } else {
    hhi_summary <- data.table::data.table(
      cf = all_cf,
      employer_hhi = NA_real_
    )
  }

  # 4.3 Assemble (left-join onto the full cf set) -----
  base <- data.table::data.table(cf = all_cf)
  out <- type_summary[base, on = "cf"]
  out <- hhi_summary[out, on = "cf"]

  out[, list(
    cf,
    n_contract_types = as.integer(n_contract_types),
    employer_hhi = as.numeric(employer_hhi),
    type_entropy = as.numeric(type_entropy)
  )]
}


# 5. Transition indicators -----

.career_profile_transitions <- function(work) {
  employed <- work[arco >= 1L & !is.na(over_id)]
  all_cf <- unique(work$cf)

  if (nrow(employed) == 0L) {
    return(data.table::data.table(
      cf = all_cf,
      n_upward = 0L,
      n_downward = 0L,
      quality_slope = NA_real_
    ))
  }

  # Per-over_id quality: durata-weighted mean of .cq
  per_over <- employed[,
    list(
      q = .wmean(.cq, durata),
      inizio = min(inizio, na.rm = TRUE)
    ),
    by = list(cf, over_id)
  ]
  data.table::setorderv(per_over, c("cf", "inizio", "over_id"))
  per_over[, rank := seq_len(.N), by = cf]
  per_over[, q_prev := data.table::shift(q, type = "lag"), by = cf]
  per_over[, delta := q - q_prev]

  counts <- per_over[,
    list(
      n_upward = sum(delta > 0, na.rm = TRUE),
      n_downward = sum(delta < 0, na.rm = TRUE),
      quality_slope = .ols_slope(rank, q)
    ),
    by = cf
  ]

  base <- data.table::data.table(cf = all_cf)
  out <- counts[base, on = "cf"]
  out[is.na(n_upward), n_upward := 0L]
  out[is.na(n_downward), n_downward := 0L]

  out[, list(
    cf,
    n_upward = as.integer(n_upward),
    n_downward = as.integer(n_downward),
    quality_slope = as.numeric(quality_slope)
  )]
}


# 6. Wage indicators -----

.career_profile_wages <- function(work, wage_col) {
  all_cf <- unique(work$cf)
  na_out <- data.table::data.table(
    cf = all_cf,
    wage_mean = NA_real_,
    wage_median = NA_real_,
    wage_growth = NA_real_
  )

  if (is.null(wage_col) || !wage_col %in% names(work)) {
    return(na_out)
  }

  employed <- work[arco >= 1L & !is.na(get(wage_col))]
  if (nrow(employed) == 0L) {
    return(na_out)
  }

  data.table::setnames(employed, wage_col, ".wg", skip_absent = TRUE)

  # Aggregate over_id wages (durata-weighted) for growth
  per_over <- employed[,
    list(
      wg = .wmean(.wg, durata),
      inizio = min(inizio, na.rm = TRUE)
    ),
    by = list(cf, over_id)
  ]
  data.table::setorderv(per_over, c("cf", "inizio", "over_id"))

  growth <- per_over[,
    list(
      wage_growth = if (.N >= 2L && is.finite(wg[1L]) && wg[1L] != 0) {
        (wg[.N] - wg[1L]) / wg[1L]
      } else {
        NA_real_
      }
    ),
    by = cf
  ]

  stats_dt <- employed[,
    list(
      wage_mean = .wmean(.wg, durata),
      wage_median = .wmedian(.wg, durata)
    ),
    by = cf
  ]

  base <- data.table::data.table(cf = all_cf)
  out <- stats_dt[base, on = "cf"]
  out <- growth[out, on = "cf"]

  out[, list(
    cf,
    wage_mean = as.numeric(wage_mean),
    wage_median = as.numeric(wage_median),
    wage_growth = as.numeric(wage_growth)
  )]
}


# 7. Small helpers -----

.wmean <- function(x, w) {
  if (length(x) == 0L) {
    return(NA_real_)
  }
  # Replicate v1 semantics: denominator is sum(w), not sum(w[!is.na(x)]).
  denom <- sum(w, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  num <- sum(x * w, na.rm = TRUE)
  num / denom
}

.wmedian <- function(x, w) {
  if (length(x) == 0L) {
    return(NA_real_)
  }
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  matrixStats::weightedMedian(x[ok], w = w[ok], na.rm = TRUE)
}

.cv <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  m <- mean(x)
  if (!is.finite(m) || m == 0) {
    return(NA_real_)
  }
  stats::sd(x) / m
}

.shannon <- function(d) {
  d <- d[!is.na(d) & d > 0]
  if (length(d) == 0L) {
    return(NA_real_)
  }
  p <- d / sum(d)
  -sum(p * log(p))
}

.hhi <- function(d) {
  d <- d[!is.na(d) & d > 0]
  if (length(d) == 0L) {
    return(NA_real_)
  }
  p <- d / sum(d)
  sum(p * p)
}

.ols_slope <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  x <- x[ok]
  y <- y[ok]
  if (stats::sd(x) == 0) {
    return(NA_real_)
  }
  fit <- tryCatch(
    stats::coef(stats::lm(y ~ x))[2L],
    error = function(e) NA_real_
  )
  as.numeric(fit)
}

.detect_employer_column <- function(data) {
  candidates <- c(
    "datore_lavoro",
    "datore",
    "employer",
    "cod_datlav",
    "id_datore",
    "datore_id"
  )
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0L) NULL else hit[1L]
}

.detect_wage_column <- function(data) {
  candidates <- c(
    "retribuzione",
    "retribuzione_giornaliera",
    "wage",
    "salary",
    "stipendio"
  )
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0L) NULL else hit[1L]
}

.empty_career_profile <- function(indicators) {
  out <- data.table::data.table(
    cf = character(0),
    n_periods = integer(0),
    total_days_employed = numeric(0),
    employment_rate = numeric(0),
    contract_quality = numeric(0),
    intensity = numeric(0)
  )
  if ("stability" %in% indicators) {
    out[, `:=`(
      spell_cv = numeric(0),
      gap_share = numeric(0),
      max_tenure_days = numeric(0)
    )]
  }
  if ("complexity" %in% indicators) {
    out[, `:=`(
      n_contract_types = integer(0),
      employer_hhi = numeric(0),
      type_entropy = numeric(0)
    )]
  }
  if ("transitions" %in% indicators) {
    out[, `:=`(
      n_upward = integer(0),
      n_downward = integer(0),
      quality_slope = numeric(0)
    )]
  }
  if ("wages" %in% indicators) {
    out[, `:=`(
      wage_mean = numeric(0),
      wage_median = numeric(0),
      wage_growth = numeric(0)
    )]
  }
  data.table::setkeyv(out, "cf")
  out
}
