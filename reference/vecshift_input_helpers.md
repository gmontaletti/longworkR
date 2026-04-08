# Internal helpers for the vecshift v2 input contract

These helpers centralize the validation that every public entry point
consuming `vecshift` output performs. They check that the input is a
`data.table`, that it carries the columns the caller actually needs, and
(when `vecshift` \>= 2.0.0 is installed) that the object is a
`vecshift_result` produced with day-level granularity. Class and
metadata are reattached to consolidation outputs so downstream code
keeps seeing the same S3 type.
