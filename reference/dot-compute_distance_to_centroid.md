# Compute Distance to Cluster Centroid (Internal)

Compute Distance to Cluster Centroid (Internal)

## Usage

``` r
.compute_distance_to_centroid(
  features_matrix,
  cluster_ids,
  cluster_result,
  batch_size = 1e+05
)
```

## Arguments

- features_matrix:

  Numeric feature matrix

- cluster_ids:

  Vector of cluster assignments

- cluster_result:

  Clustering result object with centers

- batch_size:

  Integer. Batch size for processing (reduces memory spikes)

## Value

Numeric vector of distances
