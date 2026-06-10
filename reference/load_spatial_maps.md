# Load comuni and CPI spatial data

Loads the comuni and CPI spatial objects from the `maps/` subdirectory
of the shared data directory. The directory is resolved from the
`SHARED_DATA_DIR` environment variable, falling back to
`~/Documents/funzioni/shared_data` when unset. The files contain
polygons for Lombardy municipalities and CPI (Centro Per l'Impiego)
areas.

## Usage

``` r
load_spatial_maps()
```

## Value

A named list with two `sf` objects:

- `comuni`: polygons of Lombardy municipalities, including the
  `PRO_COM_T`, `COMUNE`, `cpi`, and `denominazione` fields

- `cpi`: polygons of CPI areas, including the `cpi` and `denominazione`
  fields

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires comuni_lom_map.rds and cpi_lom_map.rds under SHARED_DATA_DIR/maps
maps <- load_spatial_maps()
names(maps)
nrow(maps$comuni)
} # }
```
