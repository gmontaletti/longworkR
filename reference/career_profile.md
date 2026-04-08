# Unified per-person career profile

Computes a tidy per-`cf` data.table of career indicators from a
`vecshift` output. `career_profile()` is the single entry point that
replaces the legacy family of `calculate_career_*_metrics()` functions
in longworkR v0.9.0. It always returns a *core* set of six indicators
and accepts an `indicators` vector to opt into optional groups.

## Usage

``` r
career_profile(
  data,
  indicators = "core",
  quality_source = "survival",
  reference_date = NULL
)
```

## Arguments

- data:

  A `data.table` (or `vecshift_result`) produced by
  [`vecshift::vecshift()`](https://gmontaletti.github.io/vecshift/reference/vecshift.html).
  Must contain at least `cf`, `inizio`, `fine`, `durata`, `arco`,
  `over_id`, and `prior`.

- indicators:

  Character vector. Which indicator groups to include. `"core"` is
  always returned. Valid values:
  `c("core", "stability", "complexity", "transitions", "wages", "all")`.
  `"all"` expands to all four optional groups. Defaults to `"core"`.

- quality_source:

  Character scalar. How `contract_quality` is computed. Only
  `"survival"` is accepted in v0.9.0; any other value errors with a
  message flagging the reserved future-release API.

- reference_date:

  Optional `Date` or date-coercible scalar used as the right-censoring
  anchor for `employment_rate` and `gap_share`. Defaults to
  `max(data$fine)`.

## Value

A `data.table` with one row per `cf`, keyed by `cf`. Columns depend on
`indicators`:

**Core (always returned)**

- `cf`:

  Person identifier.

- `n_periods`:

  Distinct consolidated employment periods
  (`uniqueN(over_id[arco >= 1])`).

- `total_days_employed`:

  Total days spent in `arco >= 1` spells.

- `employment_rate`:

  `total_days_employed / observation_window`, clamped to `[0, 1]`.

- `contract_quality`:

  Duration-weighted mean of survival-derived quality scores per contract
  type (`arco >= 1` rows only).

- `intensity`:

  Duration-weighted mean of `prior` where `arco >= 1`.

**Stability** (`"stability"` or `"all"`)

- `spell_cv`:

  Coefficient of variation of per-`over_id` total durations.

- `gap_share`:

  Share of observation window in unemployment (`arco == 0`).

- `max_tenure_days`:

  Longest single `over_id` cumulative duration.

**Complexity** (`"complexity"` or `"all"`)

- `n_contract_types`:

  Number of distinct contract-type codes.

- `employer_hhi`:

  Herfindahl index over employer durata-weighted shares. `NA` if no
  employer column is detected.

- `type_entropy`:

  Shannon entropy (natural log) of contract-type duration shares.

**Transitions** (`"transitions"` or `"all"`)

- `n_upward`:

  Count of consecutive `over_id` pairs where contract quality increases.

- `n_downward`:

  Count of consecutive pairs where it decreases.

- `quality_slope`:

  OLS slope of quality against `over_id` rank. `NA` if fewer than two
  `over_id`s.

**Wages** (`"wages"` or `"all"`)

- `wage_mean`:

  Duration-weighted mean wage.

- `wage_median`:

  Duration-weighted median wage (via
  [`matrixStats::weightedMedian`](https://rdrr.io/pkg/matrixStats/man/weightedMedian.html)).

- `wage_growth`:

  `(last - first) / first` where the wage is computed per `over_id`;
  `NA` with fewer than two `over_id`s.

## Details

Contract quality is derived from survival analysis only: per project
policy (`CLAUDE.md` CRITICAL CORRECTION), fixed weights are forbidden.
The shared survival-based lookup is built once via
[`.build_contract_quality_lookup()`](https://gmontaletti.github.io/longworkR/reference/contract_quality_lookup.md)
and reused across all indicator groups.

## Boundary with [`analyze_employment_transitions()`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md)

The `"transitions"` group returns only per-`cf` *summaries* (`n_upward`,
`n_downward`, `quality_slope`). Full pairwise transition matrices and
network objects remain the responsibility of
[`analyze_employment_transitions()`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md).

## Class preservation

`career_profile()` is an aggregation: it collapses the input to one row
per `cf`. The result is therefore a plain `data.table` (keyed by `cf`),
even when the input carries the `vecshift_result` S3 class. Period-level
vecshift metadata does not apply to the aggregated output.

## See also

[`analyze_employment_transitions()`](https://gmontaletti.github.io/longworkR/reference/analyze_employment_transitions.md)
for full transition matrices,
[`estimate_contract_survival_optimized()`](https://gmontaletti.github.io/longworkR/reference/estimate_contract_survival_optimized.md)
for the underlying survival estimator.

## Examples

``` r
if (FALSE) { # \dontrun{
sample_data <- readRDS(system.file("extdata", "sample.rds",
                                   package = "longworkR"))

# Core only
core <- career_profile(sample_data)

# Core + stability
stab <- career_profile(sample_data, indicators = c("core", "stability"))

# Everything
full <- career_profile(sample_data, indicators = "all")
} # }
```
