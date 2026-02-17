# Get Available (Free) RAM in GB

Detects available/free memory on the system (not just total). More
accurate than total RAM for determining safe sample sizes.

## Usage

``` r
.get_available_memory_gb()
```

## Value

Numeric. Available RAM in GB, or NULL if detection fails
