# Shared survival-based contract-quality lookup

Internal helper that builds a single named numeric vector of quality
scores indexed by contract type, derived from Kaplan-Meier median
survival durations. Consumed by
[`career_profile()`](https://gmontaletti.github.io/longworkR/reference/career_profile.md)
and by
[`compute_temporal_employment_indicators()`](https://gmontaletti.github.io/longworkR/reference/compute_temporal_employment_indicators.md)
to guarantee one source of truth for contract quality. Fixed weights are
explicitly forbidden by project policy (`CLAUDE.md` CRITICAL
CORRECTION).
