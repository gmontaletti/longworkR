# Add CPI information to a data.table

Joins CPI code and name to a data.table based on comune identifier. The
function looks for comune identifiers in various common column names and
performs a left join with the comune-CPI lookup table.

## Usage

``` r
add_cpi_to_data(dt, comune_col = "comune", lookup = NULL, add_name = TRUE)
```

## Arguments

- dt:

  A data.table containing comune identifiers

- comune_col:

  Character string specifying the column name containing comune codes.
  Common values include "PRO_COM_T", "PRO_COM", "comune_code", or
  "comune". Default is "comune".

- lookup:

  Optional pre-loaded lookup table. If NULL, will load from saved RDS
  file or create new one.

- add_name:

  Logical. If TRUE, adds both cpi_code and cpi_name columns. If FALSE,
  only adds cpi_code. Default is TRUE.

## Value

The input data.table with added columns:

- cpi_code: CPI identifier code

- cpi_name: CPI area name (if add_name = TRUE)

The data.table is modified by reference for efficiency.

## Details

The function automatically detects the comune identifier column if it
exists under common names. It standardizes 5-digit codes to 6-digit
format (with leading zero) for matching.

## Examples

``` r
if (FALSE) { # \dontrun{
# Add CPI to a data.table with comune codes
dt <- data.table::data.table(PRO_COM_T = c("015146", "016124", "018084"))
dt <- add_cpi_to_data(dt, comune_col = "PRO_COM_T")
print(dt)

# Add only CPI code without name
dt <- add_cpi_to_data(dt, comune_col = "PRO_COM_T", add_name = FALSE)
} # }
```
