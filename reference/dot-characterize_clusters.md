# Characterize Clusters and Generate Bilingual Labels (Internal)

Characterize Clusters and Generate Bilingual Labels (Internal)

## Usage

``` r
.characterize_clusters(
  career_metrics,
  cluster_assignments,
  clustering_features,
  id_column,
  verbose
)
```

## Arguments

- career_metrics:

  data.table with career metrics

- cluster_assignments:

  data.table with cluster IDs

- clustering_features:

  Character vector of feature names

- id_column:

  Character name of ID column

- verbose:

  Logical

## Value

List with profiles and labels data.tables
