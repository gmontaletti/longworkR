# Plot Employment Timeline

Creates timeline visualizations of individual or grouped employment
histories, showing employment periods, unemployment gaps, and
overlapping employment situations. Optimized for temporal analysis of
employment patterns.

## Usage

``` r
plot_employment_timeline(
  data,
  person_col = "cf",
  time_col = "inizio",
  end_col = "fine",
  status_col = "stato",
  facet_by = NULL,
  n_people = 10,
  date_breaks = "3 months",
  show_overlaps = TRUE,
  show_gaps = TRUE,
  palette = "employment",
  use_bw = FALSE,
  height_per_person = 1,
  alpha = 0.8,
  border_color = "white",
  border_size = 0.3
)
```

## Arguments

- data:

  Data.table output from vecshift() containing employment segments

- person_col:

  Character. Column name for person identifier (default: "cf")

- time_col:

  Character. Column name for time periods (default: "inizio")

- end_col:

  Character. Column name for period end dates (default: "fine")

- status_col:

  Character. Column name for employment status (default: "stato")

- facet_by:

  Character. Column to use for faceting (default: NULL)

- n_people:

  Integer. Maximum number of people to show (default: 10)

- date_breaks:

  Character. Date breaks for x-axis (default: "3 months")

- show_overlaps:

  Logical. Highlight overlapping employment periods (default: TRUE)

- show_gaps:

  Logical. Show unemployment gaps (default: TRUE)

- palette:

  Character. Color palette to use (default: "employment")

- use_bw:

  Logical. Use black and white palette (default: FALSE)

- height_per_person:

  Numeric. Height per person in timeline (default: 1)

- alpha:

  Numeric. Transparency for employment bars (default: 0.8)

- border_color:

  Character. Border color for employment bars (default: "white")

- border_size:

  Numeric. Border size (default: 0.3)

## Value

A ggplot2 object showing employment timelines

## See also

[`plot_employment_gantt`](https://gmontaletti.github.io/longworkR/reference/plot_employment_gantt.md),
[`vecshift_colors`](https://gmontaletti.github.io/longworkR/reference/vecshift_colors.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)

# Create sample data
sample_data <- data.table(
  cf = rep(c("Person_A", "Person_B"), each = 3),
  inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01",
                     "2023-02-01", "2023-06-01", "2023-10-01")),
  fine = as.Date(c("2023-03-31", "2023-07-31", "2023-12-31",
                   "2023-05-31", "2023-09-30", "2023-12-31")),
  arco = c(1, 0, 1, 1, 1, 2),
  prior = c(1, 0, 1, 0, 1, 1),
  id = c(1, 0, 2, 3, 4, 5),
  durata = c(89, 122, 153, 119, 122, 92),
  stato = c("occ_ft", "disoccupato", "occ_ft", "occ_pt", "occ_ft", "over_ft_ft")
)

# Basic timeline
plot_employment_timeline(sample_data)

# Timeline with faceting
plot_employment_timeline(sample_data, facet_by = "cf")

# Black and white version
plot_employment_timeline(sample_data, use_bw = TRUE)
} # }
```
