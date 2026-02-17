# Compute Cluster Quality Metrics (Internal)

Compute Cluster Quality Metrics (Internal)

## Usage

``` r
.compute_cluster_quality(
  features_matrix,
  clusters,
  method,
  use_sampling = FALSE,
  sample_size = 50000,
  memory_fraction = 0.33
)
```

## Arguments

- features_matrix:

  Numeric feature matrix

- clusters:

  Vector of cluster assignments

- method:

  Clustering method

- use_sampling:

  Logical. Use sampling for large datasets?

- sample_size:

  Integer. Sample size for quality metrics

- memory_fraction:

  Numeric. Fraction of available RAM to use (default 0.33)

## Value

List with quality metrics
