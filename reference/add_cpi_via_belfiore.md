# Add CPI via Belfiore codes

Two-step conversion: Belfiore code → ISTAT PRO_COM_T → CPI. This
function is designed for employment data that uses Belfiore codes (e.g.,
COMUNE_LAVORATORE field in CO data).

## Usage

``` r
add_cpi_via_belfiore(dt, belfiore_col = "COMUNE_LAVORATORE", add_name = TRUE)
```

## Arguments

- dt:

  A data.table containing Belfiore codes

- belfiore_col:

  Character string specifying the column name containing Belfiore codes.
  Default is "COMUNE_LAVORATORE".

- add_name:

  Logical. If TRUE, adds both cpi_code and cpi_name columns. If FALSE,
  only adds cpi_code. Default is TRUE.

## Value

The input data.table with added columns:

- pro_com_t: ISTAT code (intermediate result)

- cpi_code: CPI identifier code

- cpi_name: CPI area name (if add_name = TRUE)

The data.table is modified by reference for efficiency.

## Details

This function performs two sequential joins:

1.  Join Belfiore codes to ISTAT PRO_COM_T codes

2.  Join ISTAT codes to CPI codes

## Examples

``` r
# Add CPI to employment data with Belfiore codes
dt <- data.table(COMUNE_LAVORATORE = c("F205", "B292", "D869"))
#> Error in data.table(COMUNE_LAVORATORE = c("F205", "B292", "D869")): could not find function "data.table"
dt <- add_cpi_via_belfiore(dt)
#> Error in add_cpi_via_belfiore(dt): Input must be a data.table
print(dt)
#> function (x, df, ncp, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_dt, x, df, log)
#>     else .Call(C_dnt, x, df, ncp, log)
#> }
#> <bytecode: 0x55e5a7a3af80>
#> <environment: namespace:stats>
```
