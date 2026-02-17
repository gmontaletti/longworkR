# Determine Optimal Number of Clusters (Hybrid Approach)

Uses hybrid approach: Elbow method (primary) + Silhouette validation
(micro-sample). This avoids memory overflow while maintaining clustering
quality.

## Usage

``` r
.determine_optimal_clusters(
  features_matrix,
  method = "kmeans",
  min_k = 3,
  max_k = 6,
  min_cluster_size = 10,
  nstart = 25,
  seed = 123,
  verbose = FALSE,
  use_sampling = FALSE,
  sample_size = 50000,
  memory_fraction = 0.33,
  k_selection_method = c("hybrid", "elbow", "silhouette")
)
```

## Arguments

- features_matrix:

  Numeric matrix

- method:

  Clustering method (for compatibility, currently only kmeans supported)

- min_k:

  Minimum number of clusters

- max_k:

  Maximum number of clusters

- min_cluster_size:

  Minimum cluster size

- nstart:

  Number of random starts for k-means

- seed:

  Random seed

- verbose:

  Print progress

- use_sampling:

  Whether to use sampling

- sample_size:

  Sample size for silhouette

- memory_fraction:

  Fraction of RAM for distance calculations

- k_selection_method:

  Selection method: "hybrid", "elbow", or "silhouette"

- k_range:

  Range of k to test

## Value

List with optimal_k, wss_values, silhouette_values, method_used
