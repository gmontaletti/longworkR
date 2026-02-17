# Reassign Small Clusters to Nearest Larger Cluster (Internal)

Reassign Small Clusters to Nearest Larger Cluster (Internal)

## Usage

``` r
.reassign_small_clusters(
  cluster_assignments,
  features_matrix,
  cluster_result,
  small_clusters,
  min_cluster_size
)
```

## Arguments

- cluster_assignments:

  data.table with cluster assignments

- features_matrix:

  Numeric feature matrix

- cluster_result:

  Clustering result object

- small_clusters:

  Vector of small cluster IDs

- min_cluster_size:

  Minimum size threshold

## Value

Updated cluster_assignments data.table
