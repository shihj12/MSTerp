# =========================================================
# R/engines/stats/heatmap_cluster.R — Heatmap Cluster Cutting
#
# Functions for cutting heatmap dendrograms into clusters
# and preparing target lists for per-cluster GO-ORA analysis.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Cut heatmap dendrogram into k clusters
#'
#' @param results Heatmap engine results (list with $data containing dendrograms)
#' @param dendro_type Which dendrogram to cut: "zscore" or "abundance"
#' @param k Number of clusters
#' @return List with cluster_assignments, cluster_targets, cluster_sizes, dendro_type, k
heatmap_cut_clusters <- function(results, dendro_type, k) {
  data <- results$data

  # Select dendrogram

  dendro <- if (identical(dendro_type, "abundance")) {
    data$dendro_abundance
  } else {
    data$dendro_zscore
  }

  if (is.null(dendro) || !inherits(dendro, "hclust")) {
    stop("No valid dendrogram available for cutting")
  }

  # Get target order from dendrogram labels or fallback to data
  target_order <- dendro$labels
  if (is.null(target_order) || length(target_order) == 0) {
    target_order <- data$target_order %||% data$gene_order %||%
      data$matched_targets %||% data$matched_genes %||% rownames(data$mat_zscore)
  }

  if (is.null(target_order) || length(target_order) == 0) {
    stop("Cannot determine feature order for cluster cutting")
  }

  n_items <- length(target_order)

  # Validate k

  if (!is.numeric(k) || length(k) != 1 || is.na(k)) {
    stop("k must be a single numeric value")
  }
  k <- as.integer(k)
  if (k < 2) {
    stop("k must be at least 2")
  }
  if (k > n_items) {
    stop(sprintf("k (%d) cannot exceed number of features (%d)", k, n_items))
  }

  # Cut the tree
  clusters <- stats::cutree(dendro, k = k)

  # clusters is a named integer vector if dendro had labels,

  # otherwise it's just integers in dendrogram order
  if (is.null(names(clusters))) {
    names(clusters) <- target_order
  }

  # Build cluster target lists
  cluster_targets <- lapply(seq_len(k), function(i) {
    names(clusters)[clusters == i]
  })
  names(cluster_targets) <- paste0("Cluster ", seq_len(k))

  cluster_sizes <- vapply(cluster_targets, length, integer(1))
  names(cluster_sizes) <- names(cluster_targets)

  list(
    cluster_assignments = clusters,
    cluster_targets = cluster_targets,
    cluster_genes = cluster_targets,    # backward compat alias
    cluster_sizes = cluster_sizes,
    dendro_type = dendro_type,
    k = k
  )
}

#' Preview cluster sizes without running full analysis
#'
#' @param results Heatmap engine results
#' @param dendro_type Which dendrogram: "zscore" or "abundance"
#' @param k Number of clusters
#' @return Named integer vector of cluster sizes, or NULL on error
heatmap_cluster_preview <- function(results, dendro_type, k) {
  tryCatch({
    info <- heatmap_cut_clusters(results, dendro_type, k)
    info$cluster_sizes
  }, error = function(e) {
    NULL
  })
}

#' Get maximum valid k for a dendrogram
#'
#' @param results Heatmap engine results
#' @param dendro_type Which dendrogram: "zscore" or "abundance"
#' @return Maximum k value, or 0 if dendrogram unavailable
heatmap_cluster_max_k <- function(results, dendro_type) {
  data <- results$data %||% list()

  dendro <- if (identical(dendro_type, "abundance")) {
    data$dendro_abundance
  } else {
    data$dendro_zscore
  }

  if (is.null(dendro) || !inherits(dendro, "hclust")) {
    return(0L)
  }

  # Number of leaves in the dendrogram
  n <- length(dendro$order)
  if (n < 2) return(0L)

  # Cap at 20 for usability

  min(n, 20L)
}

#' Check which dendrograms are available
#'
#' @param results Heatmap engine results
#' @return Named logical vector with zscore and abundance availability
heatmap_dendro_available <- function(results) {
  data <- results$data %||% list()

  c(
    zscore = !is.null(data$dendro_zscore) && inherits(data$dendro_zscore, "hclust"),
    abundance = !is.null(data$dendro_abundance) && inherits(data$dendro_abundance, "hclust")
  )
}

#' Convert cluster target list to GO-ORA input (primary IDs)
#'
#' @param cluster_targets Character vector of display IDs (gene symbols or metabolite names)
#' @param ids_df Data frame with ID mappings
#' @param id_primary_col Column name for primary IDs (protein_id, metabolite_id)
#' @param id_display_col Column name for display IDs (gene_symbol, metabolite_name)
#' @return Character vector of primary IDs for GO-ORA query
heatmap_cluster_to_goora_input <- function(cluster_targets, ids_df, id_primary_col, id_display_col) {
  if (is.null(cluster_targets) || length(cluster_targets) == 0) {
    return(character(0))
  }

  # If no mapping available, return display IDs directly
  if (is.null(ids_df) || !is.data.frame(ids_df)) {
    return(cluster_targets)
  }

  if (!id_display_col %in% names(ids_df) || !id_primary_col %in% names(ids_df)) {
    return(cluster_targets)
  }

  # Build display -> primary ID mapping
  display_to_primary <- stats::setNames(
    as.character(ids_df[[id_primary_col]]),
    as.character(ids_df[[id_display_col]])
  )

  # Map display IDs to primary IDs
  primary_ids <- unname(display_to_primary[cluster_targets])
  primary_ids <- primary_ids[!is.na(primary_ids) & nzchar(primary_ids)]

  unique(primary_ids)
}
