# Generate and save comune-CPI lookup table

Main function to create the comune-CPI lookup table and save it to disk.
This function should be run once to precompute the lookup table, which
can then be loaded quickly by add_cpi_to_data().

## Usage

``` r
generate_comune_cpi_lookup(output_path = NULL)
```

## Arguments

- output_path:

  Path where to save the RDS file. Defaults to
  data/maps/comune_cpi_lookup.rds in the project directory.

## Value

Invisibly returns the lookup data.table and prints summary statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate and save lookup table with default path
generate_comune_cpi_lookup()
} # }
```
