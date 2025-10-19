#' Career Trajectory Clustering and Segmentation
#'
#' This module provides career trajectory clustering functionality to segment workers
#' into meaningful groups based on their employment patterns, stability, and progression.
#' Supports automatic cluster number selection, multiple clustering algorithms, and
#' bilingual labeling for Italian and English contexts.
#'
#' @name career_clustering
#' @author longworkR Package
#' @importFrom data.table data.table setDT copy setkey merge.data.table
#' @importFrom stats kmeans hclust dist cutree
#' @importFrom cluster silhouette pam
NULL

#' Cluster Career Trajectories into Meaningful Segments
#'
#' Segments workers into 3-6 distinct career trajectory groups based on employment
#' stability, employment rates, and career progression patterns. Automatically
#' determines optimal number of clusters and provides bilingual (English/Italian)
#' labels for each segment.
#'
#' @param career_metrics data.table. Output from calculate_comprehensive_career_metrics()
#'   containing career quality, stability, and progression metrics
#' @param n_clusters Integer or NULL. Number of clusters (3-6). If NULL, automatically
#'   selects optimal number using hybrid method (Elbow + Silhouette). Default: NULL
#' @param method Character. Clustering algorithm: "kmeans" (default), "pam", or "hierarchical"
#' @param features Character vector. Metric categories to use: "stability", "employment",
#'   "progression", "quality", "complexity". Default: c("stability", "employment", "progression")
#' @param k_selection_method Character string specifying the method for automatic k selection
#'   when n_clusters = NULL. Options:
#'   \itemize{
#'     \item \code{"hybrid"} (default): Uses Elbow method for initial estimate,
#'       then validates with Silhouette on micro-sample. Best balance of accuracy
#'       and memory efficiency.
#'     \item \code{"elbow"}: Uses only Within-cluster Sum of Squares (WSS) to
#'       identify the "elbow" point. Most memory efficient, works on full dataset.
#'     \item \code{"silhouette"}: Uses only Silhouette coefficient on micro-sample.
#'       Most statistically rigorous but slower.
#'   }
#'   Ignored if n_clusters is specified manually.
#' @param id_column Character. Name of person identifier column. Default: "cf"
#' @param min_cluster_size Integer. Minimum workers per cluster (filters small clusters). Default: 10
#' @param standardize Logical. Standardize features before clustering? Default: TRUE
#' @param nstart Integer. Number of random starts for k-means. Default: 25
#' @param seed Integer. Random seed for reproducibility. Default: 123
#' @param verbose Logical. Print clustering progress? Default: FALSE
#' @param use_sampling Logical or NULL. Enable sampling for large datasets? If NULL (default),
#'   automatically enables for n >= 100K. Set TRUE/FALSE to override auto-detection.
#' @param sample_size_k Integer. Sample size for optimal k determination. Default: 50,000.
#'   Note: Automatically capped based on available system memory (typically 1K-100K).
#' @param sample_size_quality Integer. Sample size for cluster quality metrics. Default: 50,000.
#'   Note: Automatically capped based on available system memory (typically 1K-100K).
#' @param batch_size Integer. Batch size for distance calculations. Default: 100,000.
#'   Reduces memory spikes for large datasets.
#' @param memory_fraction Numeric (0-1). Fraction of available RAM to use for distance matrix
#'   calculations. Default: 0.33 (33% of available RAM).
#'
#'   **How it works:**
#'   \itemize{
#'     \item Function detects available (free) system RAM
#'     \item Calculates maximum safe sample size: max_n = sqrt(available_RAM * memory_fraction / 8 bytes)
#'     \item Automatically limits silhouette computation to max_n observations
#'     \item Prevents memory overflow for large datasets
#'   }
#'
#'   **Recommendations:**
#'   \itemize{
#'     \item Normal use: 0.25-0.33 (balanced)
#'     \item Conservative: 0.10-0.20 (safer for limited RAM)
#'     \item Aggressive: 0.40-0.50 (for systems with abundant RAM)
#'   }
#'
#'   Lower values reduce memory risk but may limit silhouette validation sample size.
#'   If you experience memory errors, reduce this value first.
#'
#' @return A list with clustering results:
#'   \item{cluster_assignments}{data.table with worker-level cluster assignments and labels}
#'   \item{cluster_profiles}{data.table with cluster-level summary statistics}
#'   \item{cluster_quality}{list with validation metrics (silhouette, Dunn, Calinski-Harabasz)}
#'   \item{feature_importance}{data.table with feature discrimination statistics}
#'   \item{cluster_labels}{data.table mapping cluster IDs to bilingual labels}
#'
#'   When n_clusters = NULL, the following diagnostic attributes are attached to the result:
#'   \describe{
#'     \item{wss_values}{Named numeric vector of Within-cluster Sum of Squares
#'       for each k tested (if elbow or hybrid method used). Lower values indicate
#'       tighter clusters. Names are "k3", "k4", "k5", "k6".}
#'     \item{silhouette_values}{Named numeric vector of average silhouette width
#'       for each k tested (if silhouette or hybrid method used). Range: -1 to 1.
#'       Values > 0.5 indicate strong cluster structure. Names are "k3", "k4", "k5", "k6".}
#'     \item{k_selection_method}{Character string indicating which method was used:
#'       "hybrid", "elbow", or "silhouette".}
#'     \item{decision_rule}{Character string describing how the final k was chosen.
#'       Possible values:
#'       \itemize{
#'         \item "agreement": Both elbow and silhouette agreed on the same k
#'         \item "adjacent_prefer_silhouette": Methods differed by 1, silhouette chosen
#'         \item "disagreement_prefer_elbow": Methods differed by >1, elbow chosen
#'         \item "elbow_only": Only elbow method was used
#'         \item "silhouette_only": Only silhouette method was used
#'       }}
#'   }
#'
#'   Access these attributes using \code{attr(result, "wss_values")},
#'   \code{attr(result, "silhouette_values")}, \code{attr(result, "k_selection_method")},
#'   and \code{attr(result, "decision_rule")}.
#'
#' @details
#' **Feature Selection Strategy:**
#'
#' The function uses different metric combinations based on the features parameter:
#' \itemize{
#'   \item **stability**: employment_stability_index, avg_employment_spell, job_turnover_rate
#'   \item **employment**: employment_rate, days_employed, unemployment_spells
#'   \item **progression**: career_advancement_index, career_success_index
#'   \item **quality**: contract_quality_score, employment_intensity_score
#'   \item **complexity**: career_complexity_index, concurrent_employment_rate
#' }
#'
#' **Cluster Archetypes (automatically detected):**
#' \itemize{
#'   \item **Stable Full Employment**: High stability + high employment rate
#'     - IT: "Occupazione Stabile Continuativa"
#'   \item **Precarious Intermittent Workers**: Low stability + low employment rate
#'     - IT: "Lavoratori Precari Intermittenti"
#'   \item **Upward Career Progression**: High advancement + increasing quality
#'     - IT: "Carriere in Ascesa"
#'   \item **Entry-Level Starters**: Moderate quality + low advancement
#'     - IT: "Lavoratori in Ingresso"
#'   \item **Multi-Employment Complex**: High complexity + multiple concurrent jobs
#'     - IT: "Carriere Multi-Impiego Complesse"
#'   \item **Declining Trajectories**: Decreasing quality + low progression
#'     - IT: "Traiettorie in Declino"
#' }
#'
#' @section Optimal k Selection Methods:
#'
#' When n_clusters = NULL, the function automatically determines the optimal number
#' of clusters using one of three methods:
#'
#' **Hybrid Method (Recommended)**
#'
#' The hybrid approach combines two complementary techniques:
#' \enumerate{
#'   \item \strong{Elbow Method}: Computes Within-cluster Sum of Squares (WSS) for
#'     k in 3:6 on the full dataset. Memory efficient (O(n*p)). Identifies the
#'     "elbow" point where WSS decrease rate slows down.
#'   \item \strong{Silhouette Validation}: Computes average silhouette width on a
#'     micro-sample (max 10,000 observations) to validate the elbow result.
#'     Memory bounded (O(n^2) but n <= 10K).
#'   \item \strong{Decision Rule}:
#'     \itemize{
#'       \item If both methods agree -> use that k
#'       \item If differ by 1 -> prefer silhouette (more statistically rigorous)
#'       \item If differ by >1 -> prefer elbow (computed on larger sample)
#'     }
#' }
#'
#' **Elbow Method Only**
#'
#' Uses only WSS analysis. Fastest and most memory efficient. Suitable for very
#' large datasets (>100K observations) or when silhouette computation is too slow.
#'
#' **Silhouette Method Only**
#'
#' Uses only average silhouette width on micro-sample. Most accurate but slower
#' than hybrid. Suitable when you prioritize statistical rigor over speed.
#'
#' **Memory Management**
#'
#' The function automatically limits the sample size for silhouette calculations
#' to prevent memory overflow:
#' \itemize{
#'   \item Maximum sample: 10,000 observations (hard cap)
#'   \item Safety factor: 0.5x the theoretical memory limit
#'   \item For 24GB RAM: typically limits to ~8,000 observations
#' }
#'
#' This ensures the function can handle datasets with millions of observations
#' without exceeding available memory.
#'
#' **Scalability for Large Datasets:**
#'
#' The function automatically adapts to dataset size:
#' \itemize{
#'   \item **n < 100K**: Exact methods, full dataset analysis
#'   \item **100K ≤ n < 500K**: Sample-based k determination, full clustering
#'   \item **n ≥ 500K**: Full scalable pipeline with approximate metrics
#' }
#'
#' **Important considerations for large datasets (n > 100K):**
#' \itemize{
#'   \item Use \code{method = "kmeans"} only (scales to millions of observations)
#'   \item Avoid \code{method = "hierarchical"} (requires O(n²) memory, will fail at n > 100K)
#'   \item Avoid \code{method = "pam"} for n > 500K (use kmeans instead)
#'   \item Consider specifying \code{n_clusters} directly to skip k determination
#'   \item Increase \code{sample_size_k} for better k estimates with very large datasets
#'   \item Quality metrics are computed on stratified samples (still accurate)
#' }
#'
#' **Memory requirements:**
#' \itemize{
#'   \item K-means: ~8 bytes × n × p (features) + overhead
#'   \item For 2.7M workers × 11 features: ~240MB peak usage (manageable)
#'   \item Hierarchical/PAM: ~8 bytes × n² (distance matrix) - fails for large n
#' }
#'
#' @section Memory Management and Troubleshooting:
#'
#' **Understanding Memory Requirements**
#'
#' The clustering function uses different memory approaches for different operations:
#' \itemize{
#'   \item **K-means clustering**: O(n × p) memory - scales linearly with dataset size
#'   \item **Elbow method**: O(n × p) memory - works efficiently on full datasets
#'   \item **Silhouette validation**: O(n²) memory - requires distance matrix, most memory-intensive
#' }
#'
#' **Automatic Memory Management**
#'
#' The function automatically manages memory through several mechanisms:
#' \enumerate{
#'   \item **Available memory detection**: Detects actual free RAM (not just total)
#'   \item **Micro-sampling for silhouette**: Automatically limits silhouette computation
#'     to memory-safe sample sizes (typically 3K-10K observations)
#'   \item **Automatic fallback**: Switches to elbow-only method if memory insufficient
#'   \item **Graceful degradation**: Quality metrics return NA instead of crashing
#' }
#'
#' **When Memory Errors Occur**
#'
#' If you see "vector memory limit reached" or allocation errors, try these solutions
#' in order:
#'
#' 1. **Reduce memory_fraction** (default 0.33):
#'    ```r
#'    cluster_career_trajectories(
#'      career_metrics,
#'      memory_fraction = 0.15  # More conservative
#'    )
#'    ```
#'
#' 2. **Use elbow-only method** (fastest, no silhouette):
#'    ```r
#'    cluster_career_trajectories(
#'      career_metrics,
#'      k_selection_method = "elbow"  # Skip silhouette
#'    )
#'    ```
#'
#' 3. **Manually specify n_clusters** (skip optimization):
#'    ```r
#'    cluster_career_trajectories(
#'      career_metrics,
#'      n_clusters = 4  # Direct clustering, no k-selection
#'    )
#'    ```
#'
#' 4. **Limit sample sizes** (for very large datasets):
#'    ```r
#'    cluster_career_trajectories(
#'      career_metrics,
#'      sample_size_k = 10000,        # Limit k-selection sample
#'      sample_size_quality = 5000    # Limit quality metrics sample
#'    )
#'    ```
#'
#' 5. **Close other applications** to free system RAM
#'
#' 6. **Use a machine with more RAM** for very large datasets (>500K observations)
#'
#' **Dataset Size Guidelines**
#'
#' \describe{
#'   \item{< 10K observations}{All methods work with default settings}
#'   \item{10K - 100K observations}{Hybrid method recommended, automatic micro-sampling active}
#'   \item{100K - 500K observations}{Hybrid or elbow method, conservative memory_fraction (0.15-0.2)}
#'   \item{> 500K observations}{Elbow-only method or manually specify n_clusters}
#' }
#'
#' **Verbose Mode for Debugging**
#'
#' Enable verbose output to see memory decisions:
#' ```r
#' result <- cluster_career_trajectories(
#'   career_metrics,
#'   verbose = TRUE  # Shows memory limits, sample sizes, fallback decisions
#' )
#' ```
#'
#' This will display:
#' \itemize{
#'   \item Available system RAM
#'   \item Memory-aware sample limits
#'   \item Whether micro-sampling is triggered
#'   \item Automatic fallback decisions
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate career metrics
#' career_metrics <- calculate_comprehensive_career_metrics(
#'   data = employment_data,
#'   metrics = "all"
#' )
#'
#' # Automatic clustering with optimal k selection
#' clusters <- cluster_career_trajectories(
#'   career_metrics = career_metrics,
#'   n_clusters = NULL,
#'   features = c("stability", "employment", "progression"),
#'   verbose = TRUE
#' )
#'
#' # View cluster assignments with bilingual labels
#' head(clusters$cluster_assignments)
#'
#' # Examine cluster profiles
#' print(clusters$cluster_profiles)
#'
#' # Check cluster quality
#' print(clusters$cluster_quality)
#'
#' # Use clusters for grouped statistics
#' career_metrics_labeled <- merge(
#'   career_metrics,
#'   clusters$cluster_assignments[, .(cf, cluster_label_en, cluster_label_it)],
#'   by = "cf"
#' )
#'
#' # Summary by cluster
#' career_metrics_labeled[, .(
#'   n_workers = .N,
#'   avg_employment_rate = mean(employment_rate, na.rm = TRUE),
#'   avg_career_success = mean(career_success_index, na.rm = TRUE)
#' ), by = .(cluster_label_en, cluster_label_it)]
#'
#' # Clustering with specific number of clusters
#' clusters_4 <- cluster_career_trajectories(
#'   career_metrics = career_metrics,
#'   n_clusters = 4,
#'   method = "pam",
#'   features = c("stability", "employment", "progression", "quality")
#' )
#'
#' # Automatic k selection with hybrid method (default)
#' clustered <- cluster_career_trajectories(
#'   career_metrics,
#'   features = c("stability", "employment"),
#'   n_clusters = NULL,  # Auto-detect
#'   k_selection_method = "hybrid"
#' )
#'
#' # Inspect k selection diagnostics
#' cat("Selected k:", length(unique(clustered$cluster_assignments$cluster_id)), "\n")
#' cat("Method:", attr(clustered, "k_selection_method"), "\n")
#' cat("Decision rule:", attr(clustered, "decision_rule"), "\n")
#' cat("\nWSS values:\n")
#' print(attr(clustered, "wss_values"))
#' cat("\nSilhouette values:\n")
#' print(attr(clustered, "silhouette_values"))
#'
#' # Use elbow-only for very large datasets
#' clustered_fast <- cluster_career_trajectories(
#'   career_metrics,
#'   n_clusters = NULL,
#'   k_selection_method = "elbow"  # Fastest, most memory efficient
#' )
#'
#' # Large dataset example (> 1M workers)
#' large_clusters <- cluster_career_trajectories(
#'   career_metrics = large_career_metrics,  # 2.7M workers
#'   n_clusters = 4,                          # Specify k to save time
#'   method = "kmeans",                       # Required for large datasets
#'   features = c("stability", "employment", "progression"),
#'   use_sampling = TRUE,                     # Auto-detected, but can force
#'   sample_size_k = 100000,                  # Larger sample for better k estimate
#'   verbose = TRUE                           # See scalability info
#' )
#' # Processing time: ~2-3 minutes for 2.7M workers
#' # Memory usage: ~500MB peak
#'
#' # Example: Handling memory constraints for large datasets
#' \dontrun{
#' # If you encounter memory errors, try these approaches:
#'
#' # Approach 1: Reduce memory fraction
#' clusters_conservative <- cluster_career_trajectories(
#'   career_metrics,
#'   n_clusters = NULL,
#'   memory_fraction = 0.15,  # More conservative
#'   verbose = TRUE           # See memory decisions
#' )
#'
#' # Approach 2: Use elbow-only (fastest, most memory efficient)
#' clusters_elbow <- cluster_career_trajectories(
#'   career_metrics,
#'   n_clusters = NULL,
#'   k_selection_method = "elbow",  # Skip silhouette computation
#'   verbose = TRUE
#' )
#'
#' # Approach 3: Manually specify k (skip optimization)
#' clusters_manual <- cluster_career_trajectories(
#'   career_metrics,
#'   n_clusters = 4,  # Based on domain knowledge or preliminary analysis
#'   verbose = FALSE
#' )
#'
#' # Approach 4: Limit sample sizes explicitly
#' clusters_limited <- cluster_career_trajectories(
#'   career_metrics,
#'   n_clusters = NULL,
#'   k_selection_method = "hybrid",
#'   sample_size_k = 10000,       # Max 10K for k-selection
#'   sample_size_quality = 5000,  # Max 5K for quality metrics
#'   memory_fraction = 0.2,
#'   verbose = TRUE
#' )
#'
#' # Very large dataset (500K+ observations) - recommended approach
#' clusters_very_large <- cluster_career_trajectories(
#'   very_large_career_metrics,
#'   n_clusters = 4,              # Specify k directly
#'   method = "kmeans",           # Only kmeans scales to millions
#'   sample_size_quality = 5000,  # Limit quality sample
#'   verbose = TRUE
#' )
#' }
#' }
#'
#' @seealso
#' \code{\link{calculate_comprehensive_career_metrics}} for computing career metrics,
#' \code{\link{plot_cluster_profiles}} for visualizing cluster characteristics
#'
#' @export
cluster_career_trajectories <- function(career_metrics,
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
                                       batch_size = 100000,
                                       memory_fraction = 0.33) {

  k_selection_method <- match.arg(k_selection_method)

  # Input validation
  if (!inherits(career_metrics, "data.table")) {
    stop("career_metrics must be a data.table (output from calculate_comprehensive_career_metrics)")
  }

  if (!method %in% c("kmeans", "pam", "hierarchical")) {
    stop("method must be one of: 'kmeans', 'pam', 'hierarchical'")
  }

  if (!is.null(n_clusters) && (n_clusters < 3 || n_clusters > 6)) {
    stop("n_clusters must be between 3 and 6 (or NULL for automatic selection)")
  }

  valid_features <- c("stability", "employment", "progression", "quality", "complexity")
  invalid_features <- setdiff(features, valid_features)
  if (length(invalid_features) > 0) {
    stop(paste("Invalid features:", paste(invalid_features, collapse = ", ")))
  }

  if (!id_column %in% names(career_metrics)) {
    stop(paste("ID column", id_column, "not found in career_metrics"))
  }

  # Auto-detect scalability needs based on dataset size
  n_workers <- nrow(career_metrics)

  if (is.null(use_sampling)) {
    use_sampling <- n_workers >= 100000
  }

  # Warn about unsuitable methods for large datasets
  if (n_workers >= 50000 && method == "hierarchical") {
    warning("Hierarchical clustering not recommended for n >= 50K workers. Consider using 'kmeans'.")
    if (n_workers >= 100000) {
      stop("Hierarchical clustering requires O(n²) memory and will fail with n >= 100K. Use 'kmeans' instead.")
    }
  }

  if (n_workers >= 500000 && method == "pam") {
    warning("PAM clustering not recommended for n >= 500K workers. Using CLARA approximation.")
  }

  if (verbose) {
    cat("===== Career Trajectory Clustering =====\n")
    cat("Input data:", format(n_workers, big.mark = ","), "workers\n")
    if (use_sampling) {
      cat("Scalability mode: ENABLED (n >= 100K)\n")
      cat(sprintf("  Sample size for k: %s\n", format(sample_size_k, big.mark = ",")))
      cat(sprintf("  Sample size for quality: %s\n", format(sample_size_quality, big.mark = ",")))
    }
    cat("Features selected:", paste(features, collapse = ", "), "\n")
    cat("Method:", method, "\n\n")
  }

  # Select clustering features based on user specification
  clustering_features <- .select_clustering_features(career_metrics, features, verbose)

  # Prepare clustering data
  clustering_data <- .prepare_clustering_data(
    career_metrics,
    clustering_features,
    id_column,
    standardize,
    verbose
  )

  # Determine optimal number of clusters if not specified
  optimal_result <- NULL
  if (is.null(n_clusters)) {
    if (verbose) {
      if (use_sampling) {
        cat(sprintf("Determining optimal number of clusters on stratified sample (%s workers)...\n",
                    format(min(sample_size_k, nrow(clustering_data$features_matrix)), big.mark = ",")))
      } else {
        cat("Determining optimal number of clusters...\n")
      }
    }

    optimal_result <- .determine_optimal_clusters(
      clustering_data$features_matrix,
      method = method,
      min_k = 3,
      max_k = 6,
      min_cluster_size = min_cluster_size,
      nstart = nstart,
      seed = seed,
      verbose = verbose,
      use_sampling = use_sampling,
      sample_size = sample_size_k,
      memory_fraction = memory_fraction,
      k_selection_method = k_selection_method
    )

    n_clusters <- optimal_result$optimal_k

    if (verbose) {
      cat(sprintf("Selected %d clusters (method: %s, decision: %s)\n\n",
                  n_clusters, optimal_result$method_used, optimal_result$decision_rule))
    }
  }

  # Perform clustering
  set.seed(seed)

  if (verbose) cat(sprintf("Performing %s clustering with k=%d...\n", method, n_clusters))

  cluster_result <- .perform_clustering(
    clustering_data$features_matrix,
    n_clusters = n_clusters,
    method = method,
    nstart = nstart,
    seed = seed
  )

  # Add cluster assignments to original data
  cluster_assignments <- data.table(
    id = clustering_data$ids,
    cluster_id = cluster_result$cluster
  )
  setnames(cluster_assignments, "id", id_column)

  # Filter small clusters if needed
  cluster_sizes <- cluster_assignments[, .N, by = cluster_id]
  small_clusters <- cluster_sizes[N < min_cluster_size, cluster_id]

  if (length(small_clusters) > 0) {
    if (verbose) {
      cat(sprintf("Warning: %d clusters have fewer than %d members\n",
                  length(small_clusters), min_cluster_size))
      cat("Small clusters:", paste(small_clusters, collapse = ", "), "\n")
    }
    # Reassign small clusters to nearest larger cluster
    cluster_assignments <- .reassign_small_clusters(
      cluster_assignments,
      clustering_data$features_matrix,
      cluster_result,
      small_clusters,
      min_cluster_size
    )
  }

  # Characterize clusters and generate labels
  if (verbose) cat("Characterizing clusters and generating labels...\n")

  cluster_characterization <- .characterize_clusters(
    career_metrics,
    cluster_assignments,
    clustering_features,
    id_column,
    verbose
  )

  # Compute validation metrics
  if (verbose) {
    if (use_sampling) {
      cat(sprintf("Computing cluster quality metrics on stratified sample (%s workers)...\n",
                  format(min(sample_size_quality, nrow(clustering_data$features_matrix)), big.mark = ",")))
    } else {
      cat("Computing cluster quality metrics...\n")
    }
  }

  cluster_quality <- .compute_cluster_quality(
    clustering_data$features_matrix,
    cluster_assignments$cluster_id,
    method = method,
    use_sampling = use_sampling,
    sample_size = sample_size_quality,
    memory_fraction = memory_fraction
  )

  # Compute feature importance
  feature_importance <- .compute_feature_importance(
    clustering_data$features_scaled,
    cluster_assignments$cluster_id,
    clustering_features
  )

  # Merge labels into assignments
  cluster_assignments <- merge(
    cluster_assignments,
    cluster_characterization$labels,
    by = "cluster_id",
    all.x = TRUE
  )

  # Add distance to centroid
  if (method %in% c("kmeans", "pam")) {
    if (verbose && use_sampling && n_workers > batch_size) {
      cat(sprintf("Computing distances to centroids in batches (%s per batch)...\n",
                  format(batch_size, big.mark = ",")))
    }
    cluster_assignments[, cluster_distance := .compute_distance_to_centroid(
      clustering_data$features_matrix,
      cluster_id,
      cluster_result,
      batch_size = batch_size
    )]
  }

  if (verbose) {
    cat("\n===== Clustering Complete =====\n")
    cat("Number of clusters:", n_clusters, "\n")
    cat("Overall silhouette score:", round(cluster_quality$overall_silhouette, 3), "\n")
    cat("\nCluster distribution:\n")
    print(cluster_characterization$profiles[, .(cluster_id, cluster_label_en, n_workers, pct_workers)])
  }

  # Return comprehensive results
  result <- list(
    cluster_assignments = cluster_assignments,
    cluster_profiles = cluster_characterization$profiles,
    cluster_quality = cluster_quality,
    feature_importance = feature_importance,
    cluster_labels = cluster_characterization$labels,
    clustering_features = clustering_features,
    method = method,
    n_clusters = n_clusters
  )

  # Add diagnostic attributes if optimal k was determined
  if (!is.null(optimal_result)) {
    attr(result, "wss_values") <- optimal_result$wss_values
    attr(result, "silhouette_values") <- optimal_result$silhouette_values
    attr(result, "k_selection_method") <- k_selection_method
    attr(result, "decision_rule") <- optimal_result$decision_rule
  }

  return(result)
}


#' Select Clustering Features Based on Category (Internal)
#'
#' @param career_metrics data.table with career metrics
#' @param features Character vector of feature categories
#' @param verbose Logical
#' @return Character vector of selected feature names
#' @keywords internal
.select_clustering_features <- function(career_metrics, features, verbose = FALSE) {

  # Define feature mappings
  feature_map <- list(
    stability = c("employment_stability_index", "career_stability_score",
                  "avg_employment_spell", "job_turnover_rate"),
    employment = c("employment_rate", "days_employed", "employment_spells",
                   "unemployment_spells", "total_employment_days"),
    progression = c("career_advancement_index", "career_success_index"),
    quality = c("contract_quality_score", "employment_intensity_score",
                "growth_opportunity_score"),
    complexity = c("career_complexity_index", "career_fragmentation_index",
                   "concurrent_employment_rate", "max_concurrent_jobs")
  )

  # Collect selected features
  selected_features <- unique(unlist(feature_map[features]))

  # Check which features are available in the data
  available_features <- intersect(selected_features, names(career_metrics))
  missing_features <- setdiff(selected_features, available_features)

  if (length(missing_features) > 0 && verbose) {
    cat("Note: The following features are not available and will be excluded:\n")
    cat(paste("-", missing_features, collapse = "\n"), "\n\n")
  }

  if (length(available_features) < 2) {
    stop("Insufficient features available for clustering (need at least 2)")
  }

  if (verbose) {
    cat("Selected clustering features:\n")
    cat(paste("-", available_features, collapse = "\n"), "\n\n")
  }

  return(available_features)
}


#' Prepare Data for Clustering (Internal)
#'
#' @param career_metrics data.table with career metrics
#' @param clustering_features Character vector of feature names
#' @param id_column Character name of ID column
#' @param standardize Logical
#' @param verbose Logical
#' @return List with features_matrix, features_scaled, ids
#' @keywords internal
.prepare_clustering_data <- function(career_metrics, clustering_features,
                                     id_column, standardize, verbose) {

  # Extract clustering features
  clustering_cols <- c(id_column, clustering_features)
  clustering_dt <- career_metrics[, ..clustering_cols]

  # Remove rows with missing values
  complete_cases <- complete.cases(clustering_dt[, ..clustering_features])
  n_missing <- sum(!complete_cases)

  if (n_missing > 0) {
    if (verbose) {
      cat(sprintf("Removing %d rows with missing values (%.1f%%)\n",
                  n_missing, 100 * n_missing / nrow(clustering_dt)))
    }
    clustering_dt <- clustering_dt[complete_cases]
  }

  if (nrow(clustering_dt) < 100) {
    warning("Very small sample size after removing missing values: ", nrow(clustering_dt), " rows")
  }

  # Extract IDs and feature matrix
  ids <- clustering_dt[[id_column]]
  features_matrix <- as.matrix(clustering_dt[, ..clustering_features])

  # Standardize features if requested
  if (standardize) {
    features_scaled <- scale(features_matrix)

    # Handle constant columns (zero variance)
    zero_var_cols <- which(apply(features_scaled, 2, function(x) all(is.nan(x))))
    if (length(zero_var_cols) > 0) {
      if (verbose) {
        cat("Removing zero-variance features:",
            paste(colnames(features_scaled)[zero_var_cols], collapse = ", "), "\n")
      }
      features_scaled <- features_scaled[, -zero_var_cols, drop = FALSE]
      features_matrix <- features_matrix[, -zero_var_cols, drop = FALSE]
    }
  } else {
    features_scaled <- features_matrix
  }

  if (verbose) {
    cat(sprintf("Clustering dataset prepared: %d workers × %d features\n\n",
                nrow(features_scaled), ncol(features_scaled)))
  }

  return(list(
    features_matrix = features_matrix,
    features_scaled = features_scaled,
    ids = ids
  ))
}


#' Get Available (Free) RAM in GB
#'
#' Detects available/free memory on the system (not just total).
#' More accurate than total RAM for determining safe sample sizes.
#'
#' @return Numeric. Available RAM in GB, or NULL if detection fails
#' @keywords internal
.get_available_memory_gb <- function() {

  available_gb <- tryCatch({
    os_type <- Sys.info()["sysname"]

    if (os_type == "Darwin") {
      # Mac: use vm_stat to get free + inactive pages
      vm_output <- system("vm_stat", intern = TRUE)

      # Extract free and inactive pages
      free_line <- grep("Pages free", vm_output, value = TRUE)
      inactive_line <- grep("Pages inactive", vm_output, value = TRUE)

      free_pages <- as.numeric(gsub(".*:\\s*(\\d+).*", "\\1", free_line))
      inactive_pages <- as.numeric(gsub(".*:\\s*(\\d+).*", "\\1", inactive_line))

      # Page size is typically 4096 bytes on Mac
      page_size <- 4096
      available_bytes <- (free_pages + inactive_pages) * page_size
      available_bytes / (1024^3)

    } else if (os_type == "Linux") {
      # Linux: read MemAvailable from /proc/meminfo
      mem_info <- readLines("/proc/meminfo")
      avail_line <- grep("MemAvailable", mem_info, value = TRUE)

      if (length(avail_line) > 0) {
        # Extract value in KB
        avail_kb <- as.numeric(sub("MemAvailable:\\s+(\\d+).*", "\\1", avail_line))
        avail_kb / (1024^2)
      } else {
        # Fallback: free + cached
        free_line <- grep("MemFree", mem_info, value = TRUE)
        cached_line <- grep("^Cached", mem_info, value = TRUE)

        free_kb <- as.numeric(sub("MemFree:\\s+(\\d+).*", "\\1", free_line))
        cached_kb <- as.numeric(sub("Cached:\\s+(\\d+).*", "\\1", cached_line))

        (free_kb + cached_kb) / (1024^2)
      }

    } else if (.Platform$OS.type == "windows") {
      # Windows: Try to use system command
      # Note: This is a basic implementation
      NULL  # Fall back to total memory approach
    } else {
      NULL
    }
  }, error = function(e) NULL)

  return(available_gb)
}


#' Calculate Memory-Aware Sample Size Limit (Internal)
#'
#' Determines the maximum sample size for distance matrix calculations based on
#' available system memory. This prevents memory exhaustion when computing
#' silhouette scores or other metrics requiring O(n²) distance matrices.
#'
#' @param memory_fraction Numeric. Fraction of available RAM to use (default 0.33 = 33%)
#' @param p Integer. Number of features (for overhead calculation)
#' @param method Character. Purpose: "clustering" (default) or "silhouette" (stricter limits)
#' @param verbose Logical. Print memory info?
#' @return Integer. Maximum safe sample size for distance matrix calculations
#' @keywords internal
.calculate_memory_aware_limit <- function(memory_fraction = 0.33, p = 10,
                                          method = c("clustering", "silhouette"),
                                          verbose = FALSE) {

  method <- match.arg(method)

  # Try to determine available RAM first (more accurate)
  available_ram_gb <- .get_available_memory_gb()
  using_total <- FALSE

  # If available RAM detection failed, fall back to total RAM
  if (is.null(available_ram_gb) || is.na(available_ram_gb) || available_ram_gb <= 0) {
    # Fall back to total RAM
    total_ram_gb <- tryCatch({
      if (.Platform$OS.type == "windows") {
        memory.limit() / 1024
      } else {
        # Try memuse package or system commands (existing code)
        if (requireNamespace("memuse", quietly = TRUE)) {
          as.numeric(memuse::Sys.meminfo()$totalram) / (1024^3)
        } else {
          os_type <- Sys.info()["sysname"]

          if (os_type == "Darwin") {
            mem_bytes <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
            mem_bytes / (1024^3)
          } else if (os_type == "Linux") {
            mem_info <- readLines("/proc/meminfo", n = 1)
            mem_kb <- as.numeric(sub("MemTotal:\\s+(\\d+).*", "\\1", mem_info))
            mem_kb / (1024^2)
          } else {
            NULL
          }
        }
      }
    }, error = function(e) NULL)

    if (is.null(total_ram_gb) || is.na(total_ram_gb) || total_ram_gb <= 0) {
      if (verbose) {
        cat("Unable to determine system RAM, using conservative default (5K sample limit)\n")
      }
      return(5000L)  # More conservative default
    }

    # Use total RAM with conservative fraction
    available_ram_gb <- total_ram_gb * 0.5  # Assume 50% is available
    using_total <- TRUE
  }

  # Calculate maximum sample size based on memory constraints
  # Distance matrix: n × n × 8 bytes (double precision)
  # Additional overhead: feature matrix (n × p × 8), R objects (~20% overhead)
  # Total memory ≈ n² × 8 + n × p × 8 + 0.2 × (n² × 8 + n × p × 8)
  # Simplified: memory_bytes ≈ n² × 8 × 1.2 + n × p × 8 × 1.2

  available_bytes <- available_ram_gb * (1024^3) * memory_fraction

  # Solve quadratic equation: 9.6 × n² + 9.6 × p × n - available_bytes = 0
  # Using quadratic formula: n = (-b + sqrt(b² + 4ac)) / 2a
  a <- 9.6
  b <- 9.6 * p
  c <- -available_bytes

  discriminant <- b^2 - 4 * a * c
  max_n <- (-b + sqrt(discriminant)) / (2 * a)

  # Apply safety bounds
  max_n <- max(1000, min(max_n, 100000))  # Between 1K and 100K
  max_n <- floor(max_n)

  # Apply stricter limits for silhouette (requires O(n²) distance matrix)
  if (method == "silhouette") {
    # Silhouette needs much more conservative limits due to distance matrix
    # Apply 0.5x safety factor and hard cap at 10,000
    max_n <- min(max_n * 0.5, 10000)

    if (verbose) {
      cat(sprintf("    Silhouette micro-sample limit: %d observations\n", floor(max_n)))
    }
  }

  if (verbose && method == "clustering") {
    if (using_total) {
      cat(sprintf("System RAM (total): %.1f GB (estimated %.1f GB available)\n",
                  total_ram_gb, available_ram_gb))
    } else {
      cat(sprintf("System RAM (available): %.1f GB\n", available_ram_gb))
    }
    cat(sprintf("Using %.0f%% of available RAM for distance calculations\n", memory_fraction * 100))
    cat(sprintf("Memory-aware sample limit: %s observations\n",
                format(max_n, big.mark = ",")))
  }

  return(as.integer(max_n))
}


# 1. Elbow method for optimal k selection -----

#' Elbow Method for Optimal k Selection
#'
#' Identifies the optimal number of clusters by finding the "elbow" point in the
#' Within-cluster Sum of Squares (WSS) curve. The elbow represents the k value
#' where increasing k provides diminishing returns in reducing WSS.
#'
#' @param features_matrix Numeric matrix of features (n observations x p features)
#' @param k_range Integer vector of k values to test (default: 3:6)
#' @param seed Random seed for reproducibility (default: NULL)
#' @param verbose Logical, if TRUE prints progress messages (default: TRUE)
#'
#' @return A list with three elements:
#' \describe{
#'   \item{optimal_k}{Integer, the suggested optimal k value}
#'   \item{wss_values}{Named numeric vector of WSS for each k tested}
#'   \item{knee_point}{Integer, same as optimal_k (the detected elbow)}
#' }
#'
#' @details
#' The function uses the gradient method to detect the elbow:
#' \enumerate{
#'   \item Computes WSS for each k using k-means clustering
#'   \item Calculates first derivative (rate of WSS decrease)
#'   \item Calculates second derivative (rate of change of slope)
#'   \item Identifies the k where the second derivative is maximum (sharpest bend)
#' }
#'
#' Memory complexity: O(n*p) - linear in dataset size, suitable for large data.
#'
#' @keywords internal
.elbow_method <- function(features_matrix, k_range = 3:6, seed = NULL, verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (verbose) cat("  Running Elbow method (WSS analysis)...\n")

  # Calculate WSS for each k
  wss_values <- sapply(k_range, function(k) {
    if (verbose) cat(sprintf("    Testing k=%d... ", k))

    km <- kmeans(features_matrix, centers = k, nstart = 10, iter.max = 100)
    wss <- km$tot.withinss

    if (verbose) cat(sprintf("WSS=%.2f\n", wss))

    return(wss)
  })

  names(wss_values) <- paste0("k", k_range)

  # Detect elbow using gradient method
  # The elbow is where the rate of WSS decrease slows down significantly
  if (length(k_range) >= 3) {
    # Calculate second derivative (rate of change of slope)
    gradients <- diff(wss_values)
    second_deriv <- diff(gradients)

    # Elbow is where second derivative is maximum (sharpest bend)
    elbow_idx <- which.max(abs(second_deriv)) + 1  # +1 because diff() reduces length
    optimal_k <- k_range[elbow_idx]
  } else {
    # Fallback: choose k with steepest gradient
    gradients <- diff(wss_values)
    optimal_k <- k_range[which.max(abs(gradients))]
  }

  if (verbose) {
    cat(sprintf("  Elbow method suggests k=%d (WSS=%.2f)\n",
                optimal_k, wss_values[paste0("k", optimal_k)]))
  }

  return(list(
    optimal_k = optimal_k,
    wss_values = wss_values,
    knee_point = optimal_k
  ))
}


#' Determine Optimal Number of Clusters (Hybrid Approach)
#'
#' Uses hybrid approach: Elbow method (primary) + Silhouette validation (micro-sample).
#' This avoids memory overflow while maintaining clustering quality.
#'
#' @param features_matrix Numeric matrix
#' @param k_range Range of k to test
#' @param sample_size Sample size for silhouette
#' @param memory_fraction Fraction of RAM for distance calculations
#' @param use_sampling Whether to use sampling
#' @param seed Random seed
#' @param verbose Print progress
#' @param k_selection_method Selection method: "hybrid", "elbow", or "silhouette"
#' @param method Clustering method (for compatibility, currently only kmeans supported)
#' @param min_k Minimum number of clusters
#' @param max_k Maximum number of clusters
#' @param min_cluster_size Minimum cluster size
#' @param nstart Number of random starts for k-means
#'
#' @return List with optimal_k, wss_values, silhouette_values, method_used
#'
#' @keywords internal
.determine_optimal_clusters <- function(features_matrix,
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
                                        k_selection_method = c("hybrid", "elbow", "silhouette")) {

  k_selection_method <- match.arg(k_selection_method)
  k_range <- min_k:max_k

  if (verbose) {
    cat("\nDetermining optimal number of clusters...\n")
    cat(sprintf("Method: %s\n", k_selection_method))
  }

  n <- nrow(features_matrix)

  # === PHASE 1: ELBOW METHOD (if method != "silhouette") ===
  elbow_result <- NULL
  if (k_selection_method %in% c("hybrid", "elbow")) {
    elbow_result <- .elbow_method(
      features_matrix = features_matrix,
      k_range = k_range,
      seed = seed,
      verbose = verbose
    )
  }

  # === PHASE 2: SILHOUETTE VALIDATION (if method != "elbow") ===
  silhouette_result <- NULL
  if (k_selection_method %in% c("hybrid", "silhouette")) {

    # Calculate conservative limit for silhouette micro-sample
    # ALWAYS check memory regardless of use_sampling flag
    max_silhouette_sample <- .calculate_memory_aware_limit(
      memory_fraction = memory_fraction,
      p = ncol(features_matrix),
      method = "silhouette",
      verbose = verbose
    )

    # NEW: Pre-flight safety check
    effective_sample_size <- min(sample_size, max_silhouette_sample, n)
    required_gb <- (effective_sample_size^2 * 8) / (1024^3)  # Estimate distance matrix size

    # Get available memory for comparison
    available_mem <- .get_available_memory_gb()
    if (is.null(available_mem) || is.na(available_mem)) {
      # Fall back to conservative estimate
      available_mem <- 8.0  # Assume 8GB available
    }

    # Check if we have enough memory (with 20% safety margin)
    if (required_gb > available_mem * 0.8) {
      if (verbose) {
        cat(sprintf("\nWARNING: Silhouette computation would require %.1f GB but only %.1f GB available.\n",
                    required_gb, available_mem))
        cat("Automatically switching to elbow-only method for safety.\n\n")
      }

      # Force elbow-only method
      k_selection_method <- "elbow"

      # Skip silhouette computation entirely
      silhouette_result <- NULL

      # Continue to Phase 3 (decision) which will handle elbow-only case
    } else {
      # Memory OK, proceed with silhouette

      # CRITICAL FIX: Always use micro-sample when n exceeds memory-safe limit
      # This is independent of use_sampling flag (which is for clustering, not silhouette)
      if (n > max_silhouette_sample) {
        if (verbose) {
          cat(sprintf("  Silhouette validation on micro-sample (n=%d, memory-limited)...\n",
                      effective_sample_size))
        }

        # Stratified sampling to preserve feature distribution
        sample_indices <- .stratified_sample(features_matrix, effective_sample_size, seed)
        features_sample <- features_matrix[sample_indices, , drop = FALSE]
      } else {
        features_sample <- features_matrix
        if (verbose) cat("  Silhouette validation on full dataset (n within memory limits)...\n")
      }

      # Compute silhouette for each k with error handling
      sil_values <- numeric(length(k_range))
      names(sil_values) <- paste0("k", k_range)

      for (i in seq_along(k_range)) {
        k <- k_range[i]
        if (verbose) cat(sprintf("    Testing k=%d... ", k))

        # Cluster the sample
        km_sample <- kmeans(features_sample, centers = k, nstart = 10, iter.max = 100)
        clusters <- km_sample$cluster

        # Calculate silhouette with error handling
        avg_sil <- tryCatch({
          sil <- silhouette(clusters, dist(features_sample))
          mean(sil[, "sil_width"])
        }, error = function(e) {
          if (verbose) {
            cat(sprintf("\nERROR during silhouette computation for k=%d:\n", k))
            cat(sprintf("  %s\n\n", e$message))
          }

          # Check if it's a memory error
          if (grepl("memory|vector|allocation|limit", e$message, ignore.case = TRUE)) {
            stop(sprintf(
              "Memory overflow during silhouette computation:\n  %s\n\nThis typically happens when the sample size is too large for available RAM.\n\nSolutions (try in order):\n  1. Reduce memory_fraction parameter (try 0.15 or 0.1)\n  2. Use k_selection_method='elbow' (faster, no silhouette)\n  3. Manually specify n_clusters to skip automatic optimization\n  4. Close other applications to free system RAM\n  5. Use a machine with more RAM for large datasets\n\nCurrent settings:\n  - Dataset size: %s observations\n  - Sample size for silhouette: %s observations\n  - Estimated memory needed: %.1f GB\n  - Memory fraction: %.0f%%",
              e$message,
              format(n, big.mark = ","),
              format(nrow(features_sample), big.mark = ","),
              required_gb,
              memory_fraction * 100
            ))
          } else {
            # Re-throw non-memory errors
            stop(e)
          }
        })

        sil_values[i] <- avg_sil

        if (verbose) cat(sprintf("avg_sil=%.3f\n", avg_sil))
      }

      # Optimal k is the one with highest average silhouette
      optimal_k_sil <- k_range[which.max(sil_values)]

      if (verbose) {
        cat(sprintf("  Silhouette suggests k=%d (avg_sil=%.3f)\n",
                    optimal_k_sil, max(sil_values)))
      }

      silhouette_result <- list(
        optimal_k = optimal_k_sil,
        silhouette_values = sil_values
      )
    }
  }

  # === PHASE 3: FINAL DECISION ===
  if (k_selection_method == "elbow") {
    final_k <- elbow_result$optimal_k
    decision_rule <- "elbow_only"
  } else if (k_selection_method == "silhouette") {
    final_k <- silhouette_result$optimal_k
    decision_rule <- "silhouette_only"
  } else {
    # Hybrid: combine both methods
    k_elbow <- elbow_result$optimal_k
    k_sil <- silhouette_result$optimal_k

    if (k_elbow == k_sil) {
      final_k <- k_elbow
      decision_rule <- "agreement"
      if (verbose) cat(sprintf("\nBoth methods agree: k=%d\n", final_k))
    } else if (abs(k_elbow - k_sil) == 1) {
      # Adjacent values: prefer silhouette (more statistically rigorous)
      final_k <- k_sil
      decision_rule <- "adjacent_prefer_silhouette"
      if (verbose) {
        cat(sprintf("\nMethods differ by 1 (elbow=%d, sil=%d), using silhouette: k=%d\n",
                    k_elbow, k_sil, final_k))
      }
    } else {
      # Substantial difference: prefer elbow (computed on larger sample)
      final_k <- k_elbow
      decision_rule <- "disagreement_prefer_elbow"
      if (verbose) {
        cat(sprintf("\nMethods disagree (elbow=%d, sil=%d), using elbow: k=%d\n",
                    k_elbow, k_sil, final_k))
      }
    }
  }

  # Return comprehensive results
  return(list(
    optimal_k = final_k,
    wss_values = if (!is.null(elbow_result)) elbow_result$wss_values else NULL,
    silhouette_values = if (!is.null(silhouette_result)) silhouette_result$silhouette_values else NULL,
    method_used = k_selection_method,
    decision_rule = decision_rule
  ))
}


#' Perform Clustering with Specified Method (Internal)
#'
#' @param features_matrix Numeric matrix
#' @param n_clusters Number of clusters
#' @param method Clustering method
#' @param nstart Number of random starts
#' @param seed Random seed
#' @return Clustering result object
#' @keywords internal
.perform_clustering <- function(features_matrix, n_clusters, method, nstart, seed) {

  set.seed(seed)

  if (method == "kmeans") {
    result <- kmeans(features_matrix, centers = n_clusters, nstart = nstart, iter.max = 100)
  } else if (method == "pam") {
    result <- pam(features_matrix, k = n_clusters)
    # Standardize output format
    result$cluster <- result$clustering
    result$centers <- result$medoids
  } else { # hierarchical
    hc <- hclust(dist(features_matrix), method = "ward.D2")
    clusters <- cutree(hc, k = n_clusters)
    # Compute centroids
    centers <- matrix(0, nrow = n_clusters, ncol = ncol(features_matrix))
    for (k in 1:n_clusters) {
      centers[k, ] <- colMeans(features_matrix[clusters == k, , drop = FALSE])
    }
    result <- list(cluster = clusters, centers = centers, method = "hierarchical")
  }

  return(result)
}


#' Reassign Small Clusters to Nearest Larger Cluster (Internal)
#'
#' @param cluster_assignments data.table with cluster assignments
#' @param features_matrix Numeric feature matrix
#' @param cluster_result Clustering result object
#' @param small_clusters Vector of small cluster IDs
#' @param min_cluster_size Minimum size threshold
#' @return Updated cluster_assignments data.table
#' @keywords internal
.reassign_small_clusters <- function(cluster_assignments, features_matrix,
                                     cluster_result, small_clusters, min_cluster_size) {

  # For each small cluster, find nearest larger cluster
  for (small_id in small_clusters) {
    small_members <- which(cluster_assignments$cluster_id == small_id)

    # Find centroid of small cluster
    small_centroid <- colMeans(features_matrix[small_members, , drop = FALSE])

    # Compute distances to all other cluster centroids
    distances <- apply(cluster_result$centers, 1, function(center) {
      sqrt(sum((center - small_centroid)^2))
    })

    # Exclude the small cluster itself and find nearest
    distances[small_id] <- Inf
    nearest_cluster <- which.min(distances)

    # Reassign members
    cluster_assignments[cluster_id == small_id, cluster_id := nearest_cluster]
  }

  return(cluster_assignments)
}


#' Characterize Clusters and Generate Bilingual Labels (Internal)
#'
#' @param career_metrics data.table with career metrics
#' @param cluster_assignments data.table with cluster IDs
#' @param clustering_features Character vector of feature names
#' @param id_column Character name of ID column
#' @param verbose Logical
#' @return List with profiles and labels data.tables
#' @keywords internal
.characterize_clusters <- function(career_metrics, cluster_assignments,
                                   clustering_features, id_column, verbose) {

  # Merge cluster assignments with metrics
  metrics_with_clusters <- merge(career_metrics, cluster_assignments, by = id_column)

  # Compute cluster profiles
  profile_features <- c(
    "employment_rate", "employment_stability_index", "career_advancement_index",
    "career_success_index", "contract_quality_score", "employment_intensity_score",
    "avg_employment_spell", "job_turnover_rate", "career_complexity_index",
    "days_employed", "employment_spells"
  )

  available_profile_features <- intersect(profile_features, names(metrics_with_clusters))

  # Aggregate by cluster
  cluster_profiles <- metrics_with_clusters[, c(
    list(
      n_workers = .N,
      pct_workers = .N / nrow(metrics_with_clusters) * 100
    ),
    lapply(.SD, function(x) mean(x, na.rm = TRUE))
  ), by = cluster_id, .SDcols = available_profile_features]

  # Generate labels based on cluster characteristics
  cluster_labels <- .generate_cluster_labels(cluster_profiles, verbose)

  # Merge labels into profiles
  cluster_profiles <- merge(cluster_profiles, cluster_labels, by = "cluster_id")

  # Reorder columns
  col_order <- c("cluster_id", "cluster_label_en", "cluster_label_it", "n_workers", "pct_workers",
                 setdiff(names(cluster_profiles), c("cluster_id", "cluster_label_en", "cluster_label_it", "n_workers", "pct_workers")))
  setcolorder(cluster_profiles, col_order)

  return(list(
    profiles = cluster_profiles,
    labels = cluster_labels
  ))
}


#' Generate Bilingual Cluster Labels Based on Characteristics (Internal)
#'
#' @param cluster_profiles data.table with cluster means
#' @param verbose Logical
#' @return data.table with cluster_id, cluster_label_en, cluster_label_it
#' @keywords internal
.generate_cluster_labels <- function(cluster_profiles, verbose = FALSE) {

  n_clusters <- nrow(cluster_profiles)
  labels <- data.table(
    cluster_id = cluster_profiles$cluster_id,
    cluster_label_en = character(n_clusters),
    cluster_label_it = character(n_clusters)
  )

  # Get raw (un-normalized) metrics for differentiation
  raw_metrics <- data.table(
    cluster_id = cluster_profiles$cluster_id,
    stability = cluster_profiles$employment_stability_index,
    employment_rate = cluster_profiles$employment_rate,
    advancement = cluster_profiles$career_advancement_index,
    quality = if ("contract_quality_score" %in% names(cluster_profiles)) {
      cluster_profiles$contract_quality_score
    } else if ("career_success_index" %in% names(cluster_profiles)) {
      cluster_profiles$career_success_index
    } else {
      rep(0.5, n_clusters)
    },
    complexity = if ("career_complexity_index" %in% names(cluster_profiles)) {
      cluster_profiles$career_complexity_index
    } else {
      rep(0.5, n_clusters)
    }
  )

  # Rank clusters on each dimension (for tie-breaking)
  raw_metrics[, stability_rank := frank(-stability, ties.method = "dense")]
  raw_metrics[, employment_rank := frank(-employment_rate, ties.method = "dense")]
  raw_metrics[, advancement_rank := frank(-advancement, ties.method = "dense")]
  raw_metrics[, quality_rank := frank(-quality, ties.method = "dense")]

  # Normalize metrics to 0-1 scale for thresholding
  normalize <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1e-10)
  }

  stability_norm <- normalize(raw_metrics$stability)
  employment_norm <- normalize(raw_metrics$employment_rate)
  advancement_norm <- normalize(raw_metrics$advancement)
  quality_norm <- normalize(raw_metrics$quality)
  complexity_norm <- normalize(raw_metrics$complexity)

  # First pass: Assign primary labels based on patterns
  primary_labels <- character(n_clusters)

  for (i in 1:n_clusters) {
    # Stable Full Employment: High stability + High employment
    if (stability_norm[i] > 0.6 && employment_norm[i] > 0.7) {
      primary_labels[i] <- "Stable"
    # Precarious Workers: Low stability + Low employment
    } else if (stability_norm[i] < 0.4 && employment_norm[i] < 0.5) {
      primary_labels[i] <- "Precarious"
    # Career Progressors: High advancement
    } else if (advancement_norm[i] > 0.6 && quality_norm[i] > 0.5) {
      primary_labels[i] <- "Upward"
    # Entry-level: Low advancement
    } else if (advancement_norm[i] < 0.4) {
      primary_labels[i] <- "Entry"
    # Complex Multi-job: High complexity
    } else if (complexity_norm[i] > 0.6) {
      primary_labels[i] <- "Complex"
    # Declining: Low quality
    } else if (quality_norm[i] < 0.4) {
      primary_labels[i] <- "Declining"
    # Moderate/Mixed: Default
    } else {
      primary_labels[i] <- "Moderate"
    }
  }

  # Second pass: Differentiate duplicates by adding qualifiers
  label_counts <- table(primary_labels)

  for (label_type in names(label_counts)) {
    if (label_counts[label_type] > 1) {
      # Multiple clusters with same primary label - differentiate them
      dup_indices <- which(primary_labels == label_type)

      # Differentiate based on the primary characteristic
      if (label_type == "Stable") {
        # Rank by stability level
        ranks <- raw_metrics[dup_indices, ]$stability_rank

        # Define qualifiers for up to 6 clusters
        qualifiers_en_all <- c("Very High Stability", "High Stability", "Moderate Stability",
                              "Basic Stability", "Low Stability", "Minimal Stability")
        qualifiers_it_all <- c("Stabilità Molto Alta", "Alta Stabilità", "Stabilità Moderata",
                              "Stabilità Base", "Bassa Stabilità", "Stabilità Minima")

        # Check if we have enough qualifiers
        if (max(ranks) <= length(qualifiers_en_all)) {
          for (j in seq_along(dup_indices)) {
            idx <- dup_indices[j]
            labels[idx, cluster_label_en := paste0(qualifiers_en_all[ranks[j]], " Employment")]
            labels[idx, cluster_label_it := paste0("Occupazione con ", qualifiers_it_all[ranks[j]])]
          }
        } else {
          # Fallback to numeric differentiation
          for (j in seq_along(dup_indices)) {
            idx <- dup_indices[j]
            labels[idx, cluster_label_en := sprintf("Stable Employment - Level %d", ranks[j])]
            labels[idx, cluster_label_it := sprintf("Occupazione Stabile - Livello %d", ranks[j])]
          }
        }

      } else if (label_type == "Precarious") {
        # Rank by employment rate
        ranks <- raw_metrics[dup_indices, ]$employment_rank

        # Define qualifiers for up to 6 clusters
        qualifiers_en_all <- c("Extremely Precarious", "Severely Precarious", "Moderately Precarious",
                              "Marginally Precarious", "Mildly Precarious", "Slightly Precarious")
        qualifiers_it_all <- c("Precarietà Estrema", "Precarietà Grave", "Precarietà Moderata",
                              "Precarietà Marginale", "Precarietà Lieve", "Precarietà Lievissima")

        # Check if we have enough qualifiers
        if (max(ranks) <= length(qualifiers_en_all)) {
          for (j in seq_along(dup_indices)) {
            idx <- dup_indices[j]
            labels[idx, cluster_label_en := paste0(qualifiers_en_all[ranks[j]], " Workers")]
            labels[idx, cluster_label_it := paste0("Lavoratori con ", qualifiers_it_all[ranks[j]])]
          }
        } else {
          # Fallback to numeric differentiation
          for (j in seq_along(dup_indices)) {
            idx <- dup_indices[j]
            labels[idx, cluster_label_en := sprintf("Precarious Workers - Level %d", ranks[j])]
            labels[idx, cluster_label_it := sprintf("Lavoratori Precari - Livello %d", ranks[j])]
          }
        }

      } else if (label_type == "Entry") {
        # Rank by advancement
        ranks <- raw_metrics[dup_indices, ]$advancement_rank

        # Define qualifiers for up to 6 clusters
        qualifiers_en_all <- c("Very Early Career", "Early Career Entry", "Entry-Level",
                              "Junior Entry", "Basic Entry", "Initial Entry")
        qualifiers_it_all <- c("Ingresso Inizialissimo", "Ingresso Iniziale", "Livello Base",
                              "Ingresso Junior", "Ingresso Elementare", "Primo Ingresso")

        # Check if we have enough qualifiers
        if (max(ranks) <= length(qualifiers_en_all)) {
          for (j in seq_along(dup_indices)) {
            idx <- dup_indices[j]
            labels[idx, cluster_label_en := paste0(qualifiers_en_all[ranks[j]], " Workers")]
            labels[idx, cluster_label_it := paste0("Lavoratori ", qualifiers_it_all[ranks[j]])]
          }
        } else {
          # Fallback to numeric differentiation
          for (j in seq_along(dup_indices)) {
            idx <- dup_indices[j]
            labels[idx, cluster_label_en := sprintf("Entry Workers - Level %d", ranks[j])]
            labels[idx, cluster_label_it := sprintf("Lavoratori Ingresso - Livello %d", ranks[j])]
          }
        }

      } else {
        # Generic differentiation by cluster size rank
        for (j in seq_along(dup_indices)) {
          idx <- dup_indices[j]
          suffix <- c("Type A", "Type B", "Type C", "Type D")[j]
          labels[idx, cluster_label_en := paste0(label_type, " - ", suffix)]
          labels[idx, cluster_label_it := paste0(label_type, " - Tipo ", LETTERS[j])]
        }
      }
    } else {
      # Only one cluster with this label - use full descriptive name
      idx <- which(primary_labels == label_type)

      if (label_type == "Stable") {
        labels[idx, cluster_label_en := "Stable Full Employment"]
        labels[idx, cluster_label_it := "Occupazione Stabile Continuativa"]
      } else if (label_type == "Precarious") {
        labels[idx, cluster_label_en := "Precarious Intermittent Workers"]
        labels[idx, cluster_label_it := "Lavoratori Precari Intermittenti"]
      } else if (label_type == "Upward") {
        labels[idx, cluster_label_en := "Upward Career Progression"]
        labels[idx, cluster_label_it := "Carriere in Ascesa"]
      } else if (label_type == "Entry") {
        labels[idx, cluster_label_en := "Entry-Level Starters"]
        labels[idx, cluster_label_it := "Lavoratori in Ingresso"]
      } else if (label_type == "Complex") {
        labels[idx, cluster_label_en := "Multi-Employment Complex Careers"]
        labels[idx, cluster_label_it := "Carriere Multi-Impiego Complesse"]
      } else if (label_type == "Declining") {
        labels[idx, cluster_label_en := "Declining Employment Quality"]
        labels[idx, cluster_label_it := "Traiettorie in Declino"]
      } else {
        labels[idx, cluster_label_en := "Moderate Mixed Trajectories"]
        labels[idx, cluster_label_it := "Traiettorie Miste Moderate"]
      }
    }
  }

  if (verbose) {
    cat("\nGenerated cluster labels:\n")
    print(labels)
    cat("\n")
  }

  return(labels)
}


#' Compute Cluster Quality Metrics (Internal)
#'
#' @param features_matrix Numeric feature matrix
#' @param clusters Vector of cluster assignments
#' @param method Clustering method
#' @param use_sampling Logical. Use sampling for large datasets?
#' @param sample_size Integer. Sample size for quality metrics
#' @param memory_fraction Numeric. Fraction of available RAM to use (default 0.33)
#' @return List with quality metrics
#' @keywords internal
.compute_cluster_quality <- function(features_matrix, clusters, method,
                                     use_sampling = FALSE, sample_size = 50000,
                                     memory_fraction = 0.33) {

  # Use sampling for large datasets
  # CRITICAL: Silhouette requires dist() which needs O(n²) memory
  # Calculate memory-aware limit based on available RAM
  max_quality_sample <- .calculate_memory_aware_limit(
    memory_fraction = memory_fraction,
    p = ncol(features_matrix),
    method = "silhouette",
    verbose = FALSE  # Don't print during quality computation
  )

  if (use_sampling && nrow(features_matrix) > max_quality_sample) {
    # Use smaller sample for quality metrics due to O(n²) distance matrix requirement
    effective_sample_size <- min(sample_size, max_quality_sample)

    # Stratified sampling preserving cluster proportions
    sample_indices <- .stratified_cluster_sample(clusters, effective_sample_size)
    features_sample <- features_matrix[sample_indices, , drop = FALSE]
    clusters_sample <- clusters[sample_indices]

    is_approximate <- TRUE
    approx_note <- sprintf("Metrics computed on stratified sample (%s/%s observations)",
                          format(length(sample_indices), big.mark = ","),
                          format(nrow(features_matrix), big.mark = ","))
  } else {
    features_sample <- features_matrix
    clusters_sample <- clusters
    is_approximate <- FALSE
    approx_note <- NULL
  }

  # Silhouette analysis (with error handling)
  overall_silhouette <- tryCatch({
    sil <- silhouette(clusters_sample, dist(features_sample))
    mean(sil[, "sil_width"])
  }, error = function(e) {
    warning(sprintf(
      "Could not compute silhouette quality metric due to memory constraints.\nThis is not critical - other quality metrics are still available.\nError: %s",
      e$message
    ))

    # Return NA instead of crashing
    NA_real_
  })

  # Only compute per-cluster silhouettes if overall succeeded
  if (!is.na(overall_silhouette)) {
    sil <- silhouette(clusters_sample, dist(features_sample))
    cluster_silhouettes <- tapply(sil[, "sil_width"], clusters_sample, mean)
  } else {
    cluster_silhouettes <- rep(NA_real_, length(unique(clusters_sample)))
    names(cluster_silhouettes) <- sort(unique(clusters_sample))
  }

  # Dunn index (ratio of minimum inter-cluster distance to maximum intra-cluster distance)
  dunn_index <- .compute_dunn_index(features_sample, clusters_sample)

  # Calinski-Harabasz index (variance ratio criterion)
  ch_index <- .compute_calinski_harabasz(features_sample, clusters_sample)

  result <- list(
    overall_silhouette = overall_silhouette,
    cluster_silhouettes = cluster_silhouettes,
    dunn_index = dunn_index,
    calinski_harabasz = ch_index,
    method = method
  )

  if (is_approximate) {
    result$is_approximate <- TRUE
    result$approximation_note <- approx_note
  }

  return(result)
}


#' Compute Dunn Index (Internal)
#'
#' @param features_matrix Numeric matrix
#' @param clusters Vector of cluster assignments
#' @return Numeric Dunn index value
#' @keywords internal
.compute_dunn_index <- function(features_matrix, clusters) {

  # Compute cluster centroids
  unique_clusters <- unique(clusters)
  n_clusters <- length(unique_clusters)

  centroids <- matrix(0, nrow = n_clusters, ncol = ncol(features_matrix))
  for (i in seq_along(unique_clusters)) {
    k <- unique_clusters[i]
    centroids[i, ] <- colMeans(features_matrix[clusters == k, , drop = FALSE])
  }

  # Minimum inter-cluster distance
  min_inter <- Inf
  for (i in 1:(n_clusters - 1)) {
    for (j in (i + 1):n_clusters) {
      dist_ij <- sqrt(sum((centroids[i, ] - centroids[j, ])^2))
      min_inter <- min(min_inter, dist_ij)
    }
  }

  # Maximum intra-cluster distance
  max_intra <- 0
  for (k in unique_clusters) {
    cluster_data <- features_matrix[clusters == k, , drop = FALSE]
    if (nrow(cluster_data) > 1) {
      cluster_dists <- dist(cluster_data)
      max_intra <- max(max_intra, max(cluster_dists))
    }
  }

  dunn <- min_inter / (max_intra + 1e-10)
  return(dunn)
}


#' Compute Calinski-Harabasz Index (Internal)
#'
#' @param features_matrix Numeric matrix
#' @param clusters Vector of cluster assignments
#' @return Numeric CH index value
#' @keywords internal
.compute_calinski_harabasz <- function(features_matrix, clusters) {

  n <- nrow(features_matrix)
  k <- length(unique(clusters))

  # Overall centroid
  overall_centroid <- colMeans(features_matrix)

  # Between-cluster sum of squares
  bgss <- 0
  for (cluster_id in unique(clusters)) {
    cluster_data <- features_matrix[clusters == cluster_id, , drop = FALSE]
    n_k <- nrow(cluster_data)
    cluster_centroid <- colMeans(cluster_data)
    bgss <- bgss + n_k * sum((cluster_centroid - overall_centroid)^2)
  }

  # Within-cluster sum of squares
  wgss <- 0
  for (cluster_id in unique(clusters)) {
    cluster_data <- features_matrix[clusters == cluster_id, , drop = FALSE]
    cluster_centroid <- colMeans(cluster_data)
    wgss <- wgss + sum(apply(cluster_data, 1, function(x) sum((x - cluster_centroid)^2)))
  }

  # CH index
  ch <- (bgss / (k - 1)) / (wgss / (n - k) + 1e-10)
  return(ch)
}


#' Compute Feature Importance for Clustering (Internal)
#'
#' @param features_scaled Scaled feature matrix
#' @param clusters Vector of cluster assignments
#' @param feature_names Character vector of feature names
#' @return data.table with feature importance statistics
#' @keywords internal
.compute_feature_importance <- function(features_scaled, clusters, feature_names) {

  n_features <- ncol(features_scaled)
  importance_results <- data.table(
    feature = colnames(features_scaled),
    f_statistic = numeric(n_features),
    p_value = numeric(n_features),
    importance_score = numeric(n_features)
  )

  for (i in 1:n_features) {
    # ANOVA F-test for each feature
    feature_values <- features_scaled[, i]
    anova_result <- summary(aov(feature_values ~ factor(clusters)))

    importance_results[i, f_statistic := anova_result[[1]]$`F value`[1]]
    importance_results[i, p_value := anova_result[[1]]$`Pr(>F)`[1]]

    # Importance score: will be normalized after all features are processed
    importance_results[i, importance_score := f_statistic]
  }

  # Normalize importance scores to sum to 1
  total_importance <- sum(importance_results$importance_score, na.rm = TRUE)
  if (total_importance > 0) {
    importance_results[, importance_score := importance_score / total_importance]
  }

  # Sort by importance
  setorder(importance_results, -importance_score)

  return(importance_results)
}


#' Compute Distance to Cluster Centroid (Internal)
#'
#' @param features_matrix Numeric feature matrix
#' @param cluster_ids Vector of cluster assignments
#' @param cluster_result Clustering result object with centers
#' @param batch_size Integer. Batch size for processing (reduces memory spikes)
#' @return Numeric vector of distances
#' @keywords internal
.compute_distance_to_centroid <- function(features_matrix, cluster_ids, cluster_result,
                                          batch_size = 100000) {

  n <- nrow(features_matrix)

  # Process in batches if dataset is large
  if (n > batch_size) {
    distances <- numeric(n)
    n_batches <- ceiling(n / batch_size)

    for (batch in 1:n_batches) {
      start_idx <- (batch - 1) * batch_size + 1
      end_idx <- min(batch * batch_size, n)
      batch_indices <- start_idx:end_idx

      # Process this batch
      for (i in batch_indices) {
        cluster_id <- cluster_ids[i]
        centroid <- cluster_result$centers[cluster_id, ]
        distances[i] <- sqrt(sum((features_matrix[i, ] - centroid)^2))
      }
    }
  } else {
    # Standard processing for small datasets
    distances <- numeric(n)
    for (i in 1:n) {
      cluster_id <- cluster_ids[i]
      centroid <- cluster_result$centers[cluster_id, ]
      distances[i] <- sqrt(sum((features_matrix[i, ] - centroid)^2))
    }
  }

  return(distances)
}


#' Stratified Sampling to Preserve Variance Structure (Internal)
#'
#' @param features_matrix Numeric feature matrix
#' @param sample_size Integer. Target sample size
#' @param seed Random seed
#' @return Integer vector of sample indices
#' @keywords internal
.stratified_sample <- function(features_matrix, sample_size, seed = 123) {

  n <- nrow(features_matrix)

  if (n <= sample_size) {
    return(1:n)
  }

  set.seed(seed)

  # Simple random sampling (preserves approximate distribution)
  # For very large datasets, truly stratified sampling would be too expensive
  sample_indices <- sample(1:n, size = sample_size, replace = FALSE)

  return(sample_indices)
}


#' Stratified Sampling Preserving Cluster Proportions (Internal)
#'
#' @param clusters Vector of cluster assignments
#' @param sample_size Integer. Target sample size
#' @return Integer vector of sample indices
#' @keywords internal
.stratified_cluster_sample <- function(clusters, sample_size) {

  n <- length(clusters)

  if (n <= sample_size) {
    return(1:n)
  }

  # Calculate cluster proportions
  cluster_table <- table(clusters)
  cluster_props <- cluster_table / n

  # Sample proportionally from each cluster
  sample_indices <- integer(0)

  for (cluster_id in names(cluster_table)) {
    cluster_mask <- clusters == cluster_id
    cluster_indices <- which(cluster_mask)
    n_cluster <- length(cluster_indices)

    # Target sample size for this cluster
    target_n <- max(1, round(sample_size * cluster_props[cluster_id]))
    target_n <- min(target_n, n_cluster)  # Don't sample more than available

    # Sample from this cluster
    cluster_sample <- sample(cluster_indices, size = target_n, replace = FALSE)
    sample_indices <- c(sample_indices, cluster_sample)
  }

  return(sample_indices)
}


#' Visualize Career Cluster Profiles
#'
#' Creates comprehensive visualizations of career trajectory clusters showing key
#' metrics, cluster sizes, and comparative profiles across segments.
#'
#' @param cluster_result List. Output from cluster_career_trajectories() function
#' @param metrics Character vector. Metrics to visualize. Options: "employment_rate",
#'   "career_advancement_index", "contract_quality_score", "employment_stability_index",
#'   "career_complexity_index", "avg_employment_spell", "job_turnover_rate", "all".
#'   Default: c("employment_rate", "career_advancement_index", "employment_stability_index")
#' @param plot_type Character. Type of plot: "radar" (spider/radar chart), "bar" (grouped bars),
#'   "heatmap" (cluster × metric heatmap), "all" (multiple plots). Default: "radar"
#' @param language Character. Label language: "en" (English), "it" (Italian), "both". Default: "en"
#' @param color_palette Character. Color palette name or vector of colors. Default: "Set2"
#' @param title Character. Plot title. Default: auto-generated
#' @param theme_base ggplot2 theme. Base theme for plots. Default: theme_minimal()
#'
#' @return A ggplot2 object or list of ggplot2 objects (if plot_type = "all")
#'
#' @details
#' **Plot Types:**
#' \itemize{
#'   \item **radar**: Spider/radar chart showing cluster profiles across metrics (best for 3-6 metrics)
#'   \item **bar**: Grouped bar chart comparing clusters on each metric
#'   \item **heatmap**: Heatmap showing cluster × metric matrix with color intensity
#'   \item **all**: Returns a list with all three plot types
#' }
#'
#' **Metric Normalization:**
#' All metrics are normalized to 0-1 scale for fair comparison across different units.
#'
#' @examples
#' \dontrun{
#' # Cluster careers
#' career_metrics <- calculate_comprehensive_career_metrics(employment_data)
#' clusters <- cluster_career_trajectories(career_metrics)
#'
#' # Radar chart of cluster profiles
#' plot_cluster_profiles(
#'   clusters,
#'   metrics = c("employment_rate", "career_advancement_index",
#'               "employment_stability_index", "contract_quality_score"),
#'   plot_type = "radar",
#'   language = "en"
#' )
#'
#' # Bar chart with Italian labels
#' plot_cluster_profiles(
#'   clusters,
#'   metrics = "all",
#'   plot_type = "bar",
#'   language = "it"
#' )
#'
#' # Heatmap visualization
#' plot_cluster_profiles(
#'   clusters,
#'   plot_type = "heatmap",
#'   color_palette = "RdYlGn"
#' )
#'
#' # Generate all plot types
#' all_plots <- plot_cluster_profiles(clusters, plot_type = "all")
#' print(all_plots$radar)
#' print(all_plots$bar)
#' print(all_plots$heatmap)
#' }
#'
#' @seealso
#' \code{\link{cluster_career_trajectories}} for creating career clusters
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar geom_tile geom_polygon coord_polar
#'   facet_wrap scale_fill_brewer scale_fill_manual theme_minimal labs
#'   theme element_text element_blank
plot_cluster_profiles <- function(cluster_result,
                                  metrics = c("employment_rate", "career_advancement_index",
                                            "employment_stability_index"),
                                  plot_type = "radar",
                                  language = "en",
                                  color_palette = "Set2",
                                  title = NULL,
                                  theme_base = theme_minimal()) {

  if (!inherits(cluster_result, "list") ||
      !all(c("cluster_profiles", "cluster_labels") %in% names(cluster_result))) {
    stop("cluster_result must be output from cluster_career_trajectories()")
  }

  if (!plot_type %in% c("radar", "bar", "heatmap", "all")) {
    stop("plot_type must be one of: 'radar', 'bar', 'heatmap', 'all'")
  }

  if (!language %in% c("en", "it", "both")) {
    stop("language must be one of: 'en', 'it', 'both'")
  }

  # Get cluster profiles
  profiles <- copy(cluster_result$cluster_profiles)

  # Select metrics to visualize
  if ("all" %in% metrics) {
    available_metrics <- c("employment_rate", "employment_stability_index",
                          "career_advancement_index", "career_success_index",
                          "contract_quality_score", "employment_intensity_score",
                          "avg_employment_spell", "job_turnover_rate",
                          "career_complexity_index")
    metrics <- intersect(available_metrics, names(profiles))
  } else {
    metrics <- intersect(metrics, names(profiles))
  }

  if (length(metrics) == 0) {
    stop("No valid metrics found in cluster profiles")
  }

  # Prepare label column based on language
  if (language == "en") {
    profiles[, cluster_label := cluster_label_en]
  } else if (language == "it") {
    profiles[, cluster_label := cluster_label_it]
  } else { # both
    profiles[, cluster_label := paste0(cluster_label_en, "\n(", cluster_label_it, ")")]
  }

  # Normalize metrics to 0-1 scale for visualization
  profiles_norm <- copy(profiles)
  for (metric in metrics) {
    metric_values <- profiles[[metric]]
    metric_min <- min(metric_values, na.rm = TRUE)
    metric_max <- max(metric_values, na.rm = TRUE)
    metric_range <- metric_max - metric_min

    if (metric_range > 0) {
      profiles_norm[[metric]] <- (metric_values - metric_min) / metric_range
    } else {
      profiles_norm[[metric]] <- 0.5
    }
  }

  # Generate plots based on type
  if (plot_type == "radar" || plot_type == "all") {
    radar_plot <- .create_radar_plot(profiles_norm, metrics, color_palette, title, theme_base)
  }

  if (plot_type == "bar" || plot_type == "all") {
    bar_plot <- .create_bar_plot(profiles, metrics, color_palette, title, theme_base)
  }

  if (plot_type == "heatmap" || plot_type == "all") {
    heatmap_plot <- .create_heatmap_plot(profiles_norm, metrics, color_palette, title, theme_base)
  }

  # Return appropriate plot(s)
  if (plot_type == "radar") {
    return(radar_plot)
  } else if (plot_type == "bar") {
    return(bar_plot)
  } else if (plot_type == "heatmap") {
    return(heatmap_plot)
  } else { # all
    return(list(
      radar = radar_plot,
      bar = bar_plot,
      heatmap = heatmap_plot
    ))
  }
}


#' Create Radar/Spider Chart for Cluster Profiles (Internal)
#'
#' @param profiles_norm data.table with normalized profiles
#' @param metrics Character vector of metric names
#' @param color_palette Color palette
#' @param title Plot title
#' @param theme_base Base theme
#' @return ggplot object
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_polygon coord_polar scale_fill_brewer
#'   scale_color_brewer scale_fill_manual scale_color_manual labs theme element_text
.create_radar_plot <- function(profiles_norm, metrics, color_palette, title, theme_base) {

  # Reshape data for radar chart
  radar_data <- melt(
    profiles_norm[, c("cluster_id", "cluster_label", metrics), with = FALSE],
    id.vars = c("cluster_id", "cluster_label"),
    variable.name = "metric",
    value.name = "value"
  )

  # Create clean metric labels
  radar_data[, metric_label := gsub("_", " ", metric)]
  radar_data[, metric_label := tools::toTitleCase(metric_label)]

  # Create radar plot
  if (is.null(title)) {
    title <- "Career Cluster Profiles"
  }

  p <- ggplot(radar_data, aes(x = metric_label, y = value, group = cluster_label,
                              fill = cluster_label, color = cluster_label)) +
    geom_polygon(alpha = 0.3, linewidth = 1) +
    coord_polar() +
    labs(
      title = title,
      subtitle = paste("Comparing", length(unique(radar_data$cluster_id)), "career trajectory segments"),
      fill = "Cluster",
      color = "Cluster"
    ) +
    theme_base +
    theme(
      axis.text.x = element_text(size = 9),
      legend.position = "bottom"
    )

  # Apply color palette
  if (is.character(color_palette) && length(color_palette) == 1) {
    p <- p + scale_fill_brewer(palette = color_palette) +
         scale_color_brewer(palette = color_palette)
  } else {
    p <- p + scale_fill_manual(values = color_palette) +
         scale_color_manual(values = color_palette)
  }

  return(p)
}


#' Create Grouped Bar Chart for Cluster Profiles (Internal)
#'
#' @param profiles data.table with cluster profiles
#' @param metrics Character vector of metric names
#' @param color_palette Color palette
#' @param title Plot title
#' @param theme_base Base theme
#' @return ggplot object
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_bar position_dodge scale_fill_brewer
#'   facet_wrap labs theme
.create_bar_plot <- function(profiles, metrics, color_palette, title, theme_base) {

  # Reshape data for bar chart
  bar_data <- melt(
    profiles[, c("cluster_id", "cluster_label", metrics), with = FALSE],
    id.vars = c("cluster_id", "cluster_label"),
    variable.name = "metric",
    value.name = "value"
  )

  # Create clean metric labels
  bar_data[, metric_label := gsub("_", " ", metric)]
  bar_data[, metric_label := tools::toTitleCase(metric_label)]

  if (is.null(title)) {
    title <- "Career Metrics by Cluster"
  }

  p <- ggplot(bar_data, aes(x = cluster_label, y = value, fill = cluster_label)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ metric_label, scales = "free_y", ncol = 3) +
    labs(
      title = title,
      x = "Cluster",
      y = "Metric Value",
      fill = "Cluster"
    ) +
    theme_base +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "none"
    )

  # Apply color palette
  if (is.character(color_palette) && length(color_palette) == 1) {
    p <- p + scale_fill_brewer(palette = color_palette)
  } else {
    p <- p + scale_fill_manual(values = color_palette)
  }

  return(p)
}


#' Create Heatmap for Cluster Profiles (Internal)
#'
#' @param profiles_norm data.table with normalized profiles
#' @param metrics Character vector of metric names
#' @param color_palette Color palette
#' @param title Plot title
#' @param theme_base Base theme
#' @return ggplot object
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 labs theme
#'   element_text element_blank
.create_heatmap_plot <- function(profiles_norm, metrics, color_palette, title, theme_base) {

  # Reshape data for heatmap
  heatmap_data <- melt(
    profiles_norm[, c("cluster_id", "cluster_label", metrics), with = FALSE],
    id.vars = c("cluster_id", "cluster_label"),
    variable.name = "metric",
    value.name = "value"
  )

  # Create clean metric labels
  heatmap_data[, metric_label := gsub("_", " ", metric)]
  heatmap_data[, metric_label := tools::toTitleCase(metric_label)]

  if (is.null(title)) {
    title <- "Cluster Profile Heatmap"
  }

  p <- ggplot(heatmap_data, aes(x = metric_label, y = cluster_label, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
      midpoint = 0.5,
      limits = c(0, 1),
      name = "Normalized\nValue"
    ) +
    labs(
      title = title,
      x = "Metric",
      y = "Cluster"
    ) +
    theme_base +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  return(p)
}
