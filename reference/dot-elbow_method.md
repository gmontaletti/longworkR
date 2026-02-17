# Elbow Method for Optimal k Selection

Identifies the optimal number of clusters by finding the "elbow" point
in the Within-cluster Sum of Squares (WSS) curve. The elbow represents
the k value where increasing k provides diminishing returns in reducing
WSS.

## Usage

``` r
.elbow_method(features_matrix, k_range = 3:6, seed = NULL, verbose = TRUE)
```

## Arguments

- features_matrix:

  Numeric matrix of features (n observations x p features)

- k_range:

  Integer vector of k values to test (default: 3:6)

- seed:

  Random seed for reproducibility (default: NULL)

- verbose:

  Logical, if TRUE prints progress messages (default: TRUE)

## Value

A list with three elements:

- optimal_k:

  Integer, the suggested optimal k value

- wss_values:

  Named numeric vector of WSS for each k tested

- knee_point:

  Integer, same as optimal_k (the detected elbow)

## Details

The function uses the gradient method to detect the elbow:

1.  Computes WSS for each k using k-means clustering

2.  Calculates first derivative (rate of WSS decrease)

3.  Calculates second derivative (rate of change of slope)

4.  Identifies the k where the second derivative is maximum (sharpest
    bend)

Memory complexity: O(n\*p) - linear in dataset size, suitable for large
data.
