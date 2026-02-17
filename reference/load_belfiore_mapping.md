# Load Belfiore to ISTAT mapping

Loads the mapping table that converts Belfiore codes (cadastral codes)
to ISTAT PRO_COM_T codes. The mapping file was created from official
ISTAT data downloaded from www.istat.it.

## Usage

``` r
load_belfiore_mapping()
```

## Value

A data.table with columns:

- belfiore: 4-character Belfiore/cadastral code

- pro_com_t: 6-digit ISTAT PRO_COM_T code

- comune_name: Municipality name
