# 1. Deterministic sample subset for regression tests -----

# Returns the packaged sample data, subset to a deterministic set of persons
# unless LONGWORKR_FULL_REGRESSION=true requests the full dataset.
load_sample_for_regression <- function(n_people = 2000L) {
  sample_data <- tryCatch(
    {
      e <- new.env()
      utils::data("sample", package = "longworkR", envir = e)
      get("sample", envir = e)
    },
    error = function(e) NULL
  )

  if (!data.table::is.data.table(sample_data) || nrow(sample_data) == 0L) {
    return(NULL)
  }

  if (identical(Sys.getenv("LONGWORKR_FULL_REGRESSION"), "true")) {
    return(data.table::copy(sample_data))
  }

  # First n_people unique person identifiers in sorted order: deterministic,
  # no RNG involved.
  persone <- sort(unique(sample_data$cf))
  persone <- persone[seq_len(min(n_people, length(persone)))]
  data.table::copy(sample_data[cf %in% persone])
}
