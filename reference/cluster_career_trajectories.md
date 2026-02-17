# Cluster Career Trajectories into Meaningful Segments

Segments workers into 3-6 distinct career trajectory groups based on
employment stability, employment rates, and career progression patterns.
Automatically determines optimal number of clusters and provides
bilingual (English/Italian) labels for each segment.

## Usage

``` r
cluster_career_trajectories(
  career_metrics,
  n_clusters = NULL,
  method = "kmeans",
  features = c("stability", "employment", "progression"),
  k_selection_method = c("hybrid", "elbow", "silhouette"),
  id_column = "cf",
  min_cluster_size = 10,
  standardize = TRUE,
  nstart = 25,
  seed = 123,
  verbose = FALSE,
  use_sampling = NULL,
  sample_size_k = 50000,
  sample_size_quality = 50000,
  batch_size = 1e+05,
  memory_fraction = 0.33
)
```

## Arguments

- career_metrics:

  data.table. Output from calculate_comprehensive_career_metrics()
  containing career quality, stability, and progression metrics

- n_clusters:

  Integer or NULL. Number of clusters (3-6). If NULL, automatically
  selects optimal number using hybrid method (Elbow + Silhouette).
  Default: NULL

- method:

  Character. Clustering algorithm: "kmeans" (default), "pam", or
  "hierarchical"

- features:

  Character vector. Metric categories to use: "stability", "employment",
  "progression", "quality", "complexity". Default: c("stability",
  "employment", "progression")

- k_selection_method:

  Character string specifying the method for automatic k selection when
  n_clusters = NULL. Options:

  - `"hybrid"` (default): Uses Elbow method for initial estimate, then
    validates with Silhouette on micro-sample. Best balance of accuracy
    and memory efficiency.

  - `"elbow"`: Uses only Within-cluster Sum of Squares (WSS) to identify
    the "elbow" point. Most memory efficient, works on full dataset.

  - `"silhouette"`: Uses only Silhouette coefficient on micro-sample.
    Most statistically rigorous but slower.

  Ignored if n_clusters is specified manually.

- id_column:

  Character. Name of person identifier column. Default: "cf"

- min_cluster_size:

  Integer. Minimum workers per cluster (filters small clusters).
  Default: 10

- standardize:

  Logical. Standardize features before clustering? Default: TRUE

- nstart:

  Integer. Number of random starts for k-means. Default: 25

- seed:

  Integer. Random seed for reproducibility. Default: 123

- verbose:

  Logical. Print clustering progress? Default: FALSE

- use_sampling:

  Logical or NULL. Enable sampling for large datasets? If NULL
  (default), automatically enables for n \>= 100K. Set TRUE/FALSE to
  override auto-detection.

- sample_size_k:

  Integer. Sample size for optimal k determination. Default: 50,000.
  Note: Automatically capped based on available system memory (typically
  1K-100K).

- sample_size_quality:

  Integer. Sample size for cluster quality metrics. Default: 50,000.
  Note: Automatically capped based on available system memory (typically
  1K-100K).

- batch_size:

  Integer. Batch size for distance calculations. Default: 100,000.
  Reduces memory spikes for large datasets.

- memory_fraction:

  Numeric (0-1). Fraction of available RAM to use for distance matrix
  calculations. Default: 0.33 (33% of available RAM).

  **How it works:**

  - Function detects available (free) system RAM

  - Calculates maximum safe sample size: max_n = sqrt(available_RAM \*
    memory_fraction / 8 bytes)

  - Automatically limits silhouette computation to max_n observations

  - Prevents memory overflow for large datasets

  **Recommendations:**

  - Normal use: 0.25-0.33 (balanced)

  - Conservative: 0.10-0.20 (safer for limited RAM)

  - Aggressive: 0.40-0.50 (for systems with abundant RAM)

  Lower values reduce memory risk but may limit silhouette validation
  sample size. If you experience memory errors, reduce this value first.

## Value

A list with clustering results:

- cluster_assignments:

  data.table with worker-level cluster assignments and labels

- cluster_profiles:

  data.table with cluster-level summary statistics

- cluster_quality:

  list with validation metrics (silhouette, Dunn, Calinski-Harabasz)

- feature_importance:

  data.table with feature discrimination statistics

- cluster_labels:

  data.table mapping cluster IDs to bilingual labels

When n_clusters = NULL, the following diagnostic attributes are attached
to the result:

- wss_values:

  Named numeric vector of Within-cluster Sum of Squares for each k
  tested (if elbow or hybrid method used). Lower values indicate tighter
  clusters. Names are "k3", "k4", "k5", "k6".

- silhouette_values:

  Named numeric vector of average silhouette width for each k tested (if
  silhouette or hybrid method used). Range: -1 to 1. Values \> 0.5
  indicate strong cluster structure. Names are "k3", "k4", "k5", "k6".

- k_selection_method:

  Character string indicating which method was used: "hybrid", "elbow",
  or "silhouette".

- decision_rule:

  Character string describing how the final k was chosen. Possible
  values:

  - "agreement": Both elbow and silhouette agreed on the same k

  - "adjacent_prefer_silhouette": Methods differed by 1, silhouette
    chosen

  - "disagreement_prefer_elbow": Methods differed by \>1, elbow chosen

  - "elbow_only": Only elbow method was used

  - "silhouette_only": Only silhouette method was used

Access these attributes using `attr(result, "wss_values")`,
`attr(result, "silhouette_values")`,
`attr(result, "k_selection_method")`, and
`attr(result, "decision_rule")`.

## Details

**Feature Selection Strategy:**

The function uses different metric combinations based on the features
parameter:

- **stability**: employment_stability_index, avg_employment_spell,
  job_turnover_rate

- **employment**: employment_rate, days_employed, unemployment_spells

- **progression**: career_advancement_index, career_success_index

- **quality**: contract_quality_score, employment_intensity_score

- **complexity**: career_complexity_index, concurrent_employment_rate

**Cluster Archetypes (automatically detected):**

- **Stable Full Employment**: High stability + high employment rate -
  IT: "Occupazione Stabile Continuativa"

- **Precarious Intermittent Workers**: Low stability + low employment
  rate - IT: "Lavoratori Precari Intermittenti"

- **Upward Career Progression**: High advancement + increasing quality -
  IT: "Carriere in Ascesa"

- **Entry-Level Starters**: Moderate quality + low advancement - IT:
  "Lavoratori in Ingresso"

- **Multi-Employment Complex**: High complexity + multiple concurrent
  jobs - IT: "Carriere Multi-Impiego Complesse"

- **Declining Trajectories**: Decreasing quality + low progression - IT:
  "Traiettorie in Declino"

## Optimal k Selection Methods

When n_clusters = NULL, the function automatically determines the
optimal number of clusters using one of three methods:

**Hybrid Method (Recommended)**

The hybrid approach combines two complementary techniques:

1.  **Elbow Method**: Computes Within-cluster Sum of Squares (WSS) for k
    in 3:6 on the full dataset. Memory efficient (O(n\*p)). Identifies
    the "elbow" point where WSS decrease rate slows down.

2.  **Silhouette Validation**: Computes average silhouette width on a
    micro-sample (max 10,000 observations) to validate the elbow result.
    Memory bounded (O(n^2) but n \<= 10K).

3.  **Decision Rule**:

    - If both methods agree -\> use that k

    - If differ by 1 -\> prefer silhouette (more statistically rigorous)

    - If differ by \>1 -\> prefer elbow (computed on larger sample)

**Elbow Method Only**

Uses only WSS analysis. Fastest and most memory efficient. Suitable for
very large datasets (\>100K observations) or when silhouette computation
is too slow.

**Silhouette Method Only**

Uses only average silhouette width on micro-sample. Most accurate but
slower than hybrid. Suitable when you prioritize statistical rigor over
speed.

**Memory Management**

The function automatically limits the sample size for silhouette
calculations to prevent memory overflow:

- Maximum sample: 10,000 observations (hard cap)

- Safety factor: 0.5x the theoretical memory limit

- For 24GB RAM: typically limits to ~8,000 observations

This ensures the function can handle datasets with millions of
observations without exceeding available memory.

**Scalability for Large Datasets:**

The function automatically adapts to dataset size:

- **n \< 100K**: Exact methods, full dataset analysis

- **100K ≤ n \< 500K**: Sample-based k determination, full clustering

- **n ≥ 500K**: Full scalable pipeline with approximate metrics

**Important considerations for large datasets (n \> 100K):**

- Use `method = "kmeans"` only (scales to millions of observations)

- Avoid `method = "hierarchical"` (requires O(n²) memory, will fail at n
  \> 100K)

- Avoid `method = "pam"` for n \> 500K (use kmeans instead)

- Consider specifying `n_clusters` directly to skip k determination

- Increase `sample_size_k` for better k estimates with very large
  datasets

- Quality metrics are computed on stratified samples (still accurate)

**Memory requirements:**

- K-means: ~8 bytes × n × p (features) + overhead

- For 2.7M workers × 11 features: ~240MB peak usage (manageable)

- Hierarchical/PAM: ~8 bytes × n² (distance matrix) - fails for large n

## Memory Management and Troubleshooting

**Understanding Memory Requirements**

The clustering function uses different memory approaches for different
operations:

- **K-means clustering**: O(n × p) memory - scales linearly with dataset
  size

- **Elbow method**: O(n × p) memory - works efficiently on full datasets

- **Silhouette validation**: O(n²) memory - requires distance matrix,
  most memory-intensive

**Automatic Memory Management**

The function automatically manages memory through several mechanisms:

1.  **Available memory detection**: Detects actual free RAM (not just
    total)

2.  **Micro-sampling for silhouette**: Automatically limits silhouette
    computation to memory-safe sample sizes (typically 3K-10K
    observations)

3.  **Automatic fallback**: Switches to elbow-only method if memory
    insufficient

4.  **Graceful degradation**: Quality metrics return NA instead of
    crashing

**When Memory Errors Occur**

If you see "vector memory limit reached" or allocation errors, try these
solutions in order:

1.  **Reduce memory_fraction** (default 0.33):

        cluster_career_trajectories(
          career_metrics,
          memory_fraction = 0.15  # More conservative
        )

2.  **Use elbow-only method** (fastest, no silhouette):

        cluster_career_trajectories(
          career_metrics,
          k_selection_method = "elbow"  # Skip silhouette
        )

3.  **Manually specify n_clusters** (skip optimization):

        cluster_career_trajectories(
          career_metrics,
          n_clusters = 4  # Direct clustering, no k-selection
        )

4.  **Limit sample sizes** (for very large datasets):

        cluster_career_trajectories(
          career_metrics,
          sample_size_k = 10000,        # Limit k-selection sample
          sample_size_quality = 5000    # Limit quality metrics sample
        )

5.  **Close other applications** to free system RAM

6.  **Use a machine with more RAM** for very large datasets (\>500K
    observations)

**Dataset Size Guidelines**

- \< 10K observations:

  All methods work with default settings

- 10K - 100K observations:

  Hybrid method recommended, automatic micro-sampling active

- 100K - 500K observations:

  Hybrid or elbow method, conservative memory_fraction (0.15-0.2)

- \> 500K observations:

  Elbow-only method or manually specify n_clusters

**Verbose Mode for Debugging**

Enable verbose output to see memory decisions:

    result <- cluster_career_trajectories(
      career_metrics,
      verbose = TRUE  # Shows memory limits, sample sizes, fallback decisions
    )

This will display:

- Available system RAM

- Memory-aware sample limits

- Whether micro-sampling is triggered

- Automatic fallback decisions

## See also

[`calculate_comprehensive_career_metrics`](https://gmontaletti.github.io/longworkR/reference/calculate_comprehensive_career_metrics.md)
for computing career metrics,
[`plot_cluster_profiles`](https://gmontaletti.github.io/longworkR/reference/plot_cluster_profiles.md)
for visualizing cluster characteristics

## Examples
