# Create comune to CPI lookup table

Creates a lookup table mapping each comune to its corresponding CPI
(Centro Per l'Impiego). The function performs spatial join using
sf::st_join to assign each comune centroid to the CPI polygon it falls
within. Falls back to existing cpi column in comuni data if spatial join
fails or produces NA values.

## Usage

``` r
create_comune_cpi_lookup()
```

## Value

A data.table with columns:

- comune_code: PRO_COM_T code for the comune (6 digits with leading
  zero)

- comune_name: Name of the comune (COMUNE field)

- cpi_code: Unique code for the CPI (cpi field)

- cpi_name: Name of the CPI area (denominazione field)

## Details

The function handles edge cases including:

- Comuni outside CPI boundaries (uses existing cpi column)

- Comuni on boundary lines (assigns to largest overlapping CPI)

- Missing values (uses existing cpi column as fallback)

## Examples

``` r
lookup <- create_comune_cpi_lookup()
#> Loading spatial data...
#> Error in Sys.getenv("SHARED_DATA_DIR", default = "~/Documents/funzioni/shared_data"): unused argument (default = "~/Documents/funzioni/shared_data")
head(lookup)
#> Error: object 'lookup' not found
```
