# Prepare Data for Clustering (Internal)

Prepare Data for Clustering (Internal)

## Usage

``` r
.prepare_clustering_data(
  career_metrics,
  clustering_features,
  id_column,
  standardize,
  verbose
)
```

## Arguments

- career_metrics:

  data.table with career metrics

- clustering_features:

  Character vector of feature names

- id_column:

  Character name of ID column

- standardize:

  Logical

- verbose:

  Logical

## Value

List with features_matrix, features_scaled, ids
