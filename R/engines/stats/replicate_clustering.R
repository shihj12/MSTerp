# =========================================================
# R/engines/stats/replicate_clustering.R — Replicate Clustering Engine
#
# Hierarchical clustering of all replicates (samples) to
# assess reproducibility of sample groups.
#
# Contract v1.0: hclust object, group map, group colors
# =========================================================

#' Execute replicate_clustering engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
#'   - data$hc: hclust object (sample-level dendrogram)
#'   - data$group_map: named character vector (sample -> group)
#'   - data$group_colors: named character vector (group -> hex color)
#'   - data$cluster_info: data.frame with sample, group, merge_height
stats_replicate_clustering_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()
  params <- params %||% payload$params %||% list()

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries <<- c(log_entries, list(list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level,
      message = msg
    )))
  }

  if (!isTRUE(payload$ok)) {
    return(list(
      engine_id = "replicate_clustering",
      params = params,
      data = list(
        hc = NULL,
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Handle mixed-entity multi-dataset mode (same pattern as PCA)
  is_mixed_entity <- identical(payload$multi_subtype, "protein-metabolomics")
  if (is_mixed_entity) {
    mat <- payload$mat_combined
    samples <- payload$samples_combined
    add_log("INFO", sprintf("Mixed-entity mode: %d features x %d paired samples",
                            nrow(mat), ncol(mat)))
  } else {
    mat <- payload$mat
    samples <- payload$samples
    add_log("INFO", sprintf("Initiating replicate clustering: %d features x %d samples",
                            nrow(mat), ncol(mat)))
  }

  # Parameters

  distance_method <- params$distance_method %||% "pearson"
  linkage_method <- params$linkage_method %||% "complete"
  log_transform <- params$log_transform %||% "log10"

  # Apply log transform
  if (log_transform == "log10") {
    add_log("INFO", "Applying log10 transformation...")
    mat[mat <= 0] <- NA
    mat <- log10(mat)
  } else if (log_transform == "log2") {
    add_log("INFO", "Applying log2 transformation...")
    mat[mat <= 0] <- NA
    mat <- log2(mat)
  }

  # Remove rows with any NA (need complete cases for distance computation)
  complete_rows <- complete.cases(mat)
  mat_complete <- mat[complete_rows, , drop = FALSE]
  n_excluded <- sum(!complete_rows)

  add_log("INFO", sprintf("Complete cases: %d features (excluded %d with missing values)",
                          nrow(mat_complete), n_excluded))

  if (nrow(mat_complete) < 2 || ncol(mat_complete) < 2) {
    return(list(
      engine_id = "replicate_clustering",
      params = params,
      data = list(
        hc = NULL,
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "Insufficient data for clustering after removing NA values",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Compute distance matrix on samples (columns)
  add_log("INFO", sprintf("Computing %s distance...", distance_method))
  dist_mat <- if (distance_method == "euclidean") {
    dist(t(mat_complete))
  } else {
    cor_mat <- cor(mat_complete, method = distance_method, use = "pairwise.complete.obs")
    # Clamp to [-1, 1] to avoid floating-point issues
    cor_mat[cor_mat > 1] <- 1
    cor_mat[cor_mat < -1] <- -1
    as.dist(1 - cor_mat)
  }

  # Run hierarchical clustering
  add_log("INFO", sprintf("Running hclust (linkage = %s)...", linkage_method))
  hc <- hclust(dist_mat, method = linkage_method)

  # Build group map (sample -> group)
  group_map <- stats::setNames(
    as.character(samples$group_name),
    as.character(samples$sample_col)
  )

  # Extract group colors from metadata (same pattern as PCA)
  groups <- unique(as.character(samples$group_name))
  groups <- groups[nzchar(groups)]
  meta_groups <- payload$metadata$groups
  if (!is.null(meta_groups) && is.data.frame(meta_groups) &&
      "group_name" %in% names(meta_groups) && "color" %in% names(meta_groups)) {
    color_map <- stats::setNames(
      as.character(meta_groups$color),
      as.character(meta_groups$group_name)
    )
    group_colors <- color_map[groups]
    missing <- is.na(group_colors) | !nzchar(group_colors)
    if (any(missing)) {
      auto_colors <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
      group_colors[missing] <- auto_colors
    }
    names(group_colors) <- groups
  } else {
    group_colors <- grDevices::hcl.colors(max(1, length(groups)), palette = "Dark 3")
    group_colors <- group_colors[seq_along(groups)]
    names(group_colors) <- groups
  }

  # Build cluster_info table
  # For each sample: its group and the height at which it merges
  merge_heights <- numeric(length(hc$labels))
  names(merge_heights) <- hc$labels
  for (i in seq_len(nrow(hc$merge))) {
    h <- hc$height[i]
    for (j in 1:2) {
      idx <- hc$merge[i, j]
      if (idx < 0) {
        merge_heights[-idx] <- h
      }
    }
  }

  cluster_info <- data.frame(
    sample = hc$labels,
    group = as.character(group_map[hc$labels]),
    merge_height = merge_heights[hc$labels],
    stringsAsFactors = FALSE
  )
  cluster_info <- cluster_info[order(cluster_info$group, cluster_info$sample), ]
  rownames(cluster_info) <- NULL

  # Log runtime

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Replicate clustering completed in %.2f seconds (%d samples, %d features)",
                          engine_duration, ncol(mat_complete), nrow(mat_complete)))

  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(time = e$time, level = e$level, message = e$message,
               stringsAsFactors = FALSE)
  }))

  list(
    engine_id = "replicate_clustering",
    params = params,
    data = list(
      hc = hc,
      group_map = group_map,
      group_colors = group_colors,
      cluster_info = cluster_info,
      n_complete = nrow(mat_complete),
      n_excluded = n_excluded,
      log = log_df
    )
  )
}
