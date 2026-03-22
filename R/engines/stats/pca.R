# =========================================================
# R/engines/stats/pca.R — PCA Engine
#
# Computes proteinID PCA with outputs for viewer and paired
# children (loadings/ranks for GO-FCS).
#
# Contract v1.1: Scores, loadings, variance explained
# =========================================================

#' Execute pca engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
#'   - data$scores: data.frame with sample, group, PC1, PC2, ...
#'   - data$loadings: data.frame with protein_id, PC1, PC2, ...
#'   - data$variance: data.frame with PC, variance_explained, cumulative
#'   - data$sets: named list of protein ID vectors for paired children
stats_pca_run <- function(payload, params = NULL, context = NULL) {
  # Track engine start time
  engine_start <- Sys.time()

  params <- params %||% payload$params %||% list()

  # Initialize log entries
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
      engine_id = "pca",
      params = params,
      data = list(
        scores = data.frame(sample = character(0), group = character(0),
                            PC1 = numeric(0), PC2 = numeric(0),
                            stringsAsFactors = FALSE),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Detect mixed-entity multi-dataset mode (protein-metabolomics)
  # In this mode, use the combined matrix (proteins + metabolites x paired samples)
  is_mixed_entity <- identical(payload$multi_subtype, "protein-metabolomics")

  if (is_mixed_entity) {
    mat <- payload$mat_combined
    samples <- payload$samples_combined
    ids <- payload$ids_combined
    add_log("INFO", sprintf("Mixed-entity PCA mode: %d features (%d proteins + %d metabolites) x %d paired samples",
                            nrow(mat), length(payload$features_a), length(payload$features_b), ncol(mat)))
  } else {
    mat <- payload$mat
    samples <- payload$samples
    ids <- payload$ids
    add_log("INFO", sprintf("Initiating PCA: %d proteins x %d samples", nrow(mat), ncol(mat)))
  }

  # Get parameters
  log_transform <- params$log_transform %||% "log10"
  # n_pcs_paired: PCs used for paired analysis (GO-FCS children) - default 3
  # n_pcs_scree: PCs shown in scree plot - default 8
  n_pcs_paired <- min(as.integer(params$n_pcs %||% 3), ncol(mat) - 1, nrow(mat) - 1)
  n_pcs_scree <- min(as.integer(params$n_pcs_scree %||% 8), ncol(mat) - 1, nrow(mat) - 1)
  n_pcs <- max(n_pcs_paired, n_pcs_scree)  # Compute enough PCs for both uses
  top_n <- as.integer(params$top_n %||% 50)
  # Scaling method: "zscore" (default), "rank", or "none"
  # For multi-dataset mode, "rank" provides more robust normalization
  scale_method <- params$scale_method %||% "zscore"

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

  # Remove rows with any NA (PCA requires complete cases)
  complete_rows <- complete.cases(mat)
  mat_complete <- mat[complete_rows, , drop = FALSE]
  ids_complete <- ids[complete_rows, , drop = FALSE]

  add_log("INFO", sprintf("Complete cases: %d proteins (excluded %d with missing values)",
                          nrow(mat_complete), sum(!complete_rows)))

  if (nrow(mat_complete) < 3 || ncol(mat_complete) < 2) {
    return(list(
      engine_id = "pca",
      params = params,
      data = list(
        scores = data.frame(sample = character(0), group = character(0),
                            PC1 = numeric(0), PC2 = numeric(0),
                            stringsAsFactors = FALSE),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "Insufficient data for PCA after removing NA values",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", "Running PCA decomposition...")

  # Transpose: PCA on samples (columns become rows)
  mat_t <- t(mat_complete)

  # Apply scaling based on method
  if (scale_method == "rank") {
    # Rank-based scaling: convert each protein's values to ranks
    # More robust for multi-dataset with different distributions
    # Remove zero-variance columns first (all identical values → rank SD=0)
    col_vars <- apply(mat_t, 2, var, na.rm = TRUE)
    zero_var <- col_vars == 0 | is.na(col_vars)
    if (any(zero_var)) {
      add_log("INFO", sprintf("Removing %d zero-variance features before scaling", sum(zero_var)))
      mat_t <- mat_t[, !zero_var, drop = FALSE]
      mat_complete <- mat_complete[zero_var == FALSE, , drop = FALSE]
      ids_complete <- ids_complete[zero_var == FALSE, , drop = FALSE]
    }
    add_log("INFO", "Applying rank-based scaling...")
    mat_scaled <- apply(mat_t, 2, function(x) {
      r <- rank(x, ties.method = "average")
      # Standardize ranks to have mean 0, sd 1
      (r - mean(r)) / sd(r)
    })
  } else if (scale_method == "zscore") {
    # Standard Z-score scaling (per protein across samples)
    # Remove zero-variance columns first (scale() cannot divide by SD=0)
    col_vars <- apply(mat_t, 2, var, na.rm = TRUE)
    zero_var <- col_vars == 0 | is.na(col_vars)
    if (any(zero_var)) {
      add_log("INFO", sprintf("Removing %d zero-variance features before scaling", sum(zero_var)))
      mat_t <- mat_t[, !zero_var, drop = FALSE]
      mat_complete <- mat_complete[zero_var == FALSE, , drop = FALSE]
      ids_complete <- ids_complete[zero_var == FALSE, , drop = FALSE]
    }
    add_log("INFO", "Applying Z-score scaling...")
    mat_scaled <- scale(mat_t, center = TRUE, scale = TRUE)
  } else {
    # No scaling - just center
    add_log("INFO", "Applying centering only (no scaling)...")
    mat_scaled <- scale(mat_t, center = TRUE, scale = FALSE)
  }

  # Run PCA
  pca_result <- prcomp(mat_scaled, center = FALSE, scale. = FALSE)

  # Apply deterministic sign convention: for each PC, ensure the loading with
  # the maximum absolute value is positive. This prevents arbitrary sign flips
  # between runs while preserving the mathematical validity of the PCA.
  # Note: Eigenvectors are only defined up to a sign, so this is purely cosmetic
  # but improves reproducibility and user experience.
  n_pcs_available <- min(n_pcs, ncol(pca_result$rotation))
  for (pc_idx in seq_len(n_pcs_available)) {
    pc_loadings <- pca_result$rotation[, pc_idx]
    max_abs_idx <- which.max(abs(pc_loadings))
    if (pc_loadings[max_abs_idx] < 0) {
      # Flip both loadings and scores for this PC
      pca_result$rotation[, pc_idx] <- -pca_result$rotation[, pc_idx]
      pca_result$x[, pc_idx] <- -pca_result$x[, pc_idx]
    }
  }

  add_log("INFO", sprintf("PCA completed: extracting %d principal components (scale=%s, sign convention applied)",
                          n_pcs, scale_method))

  # Extract scores (sample projections) - use n_pcs_paired for scores plot
  scores <- as.data.frame(pca_result$x[, 1:min(n_pcs_paired, ncol(pca_result$x)), drop = FALSE])
  scores$sample <- rownames(scores)

  # Add group info
  scores$group <- samples$group_name[match(scores$sample, samples$sample_col)]

  # Reorder columns
  pc_cols <- grep("^PC", names(scores), value = TRUE)
  scores <- scores[, c("sample", "group", pc_cols), drop = FALSE]
  rownames(scores) <- NULL

  # Extract loadings (feature contributions) - use n_pcs_paired for paired analysis
  loadings <- as.data.frame(pca_result$rotation[, 1:min(n_pcs_paired, ncol(pca_result$rotation)), drop = FALSE])

  # Add feature IDs
  if (is_mixed_entity) {
    # Use row names from combined matrix (prefixed P: / M:)
    loadings$protein_id <- rownames(mat_complete)
    # Add entity_type column by parsing the prefix
    loadings$entity_type <- ifelse(grepl("^P:", loadings$protein_id), "protein", "metabolite")
    # Add clean display ID (strip prefix)
    if ("display_id" %in% names(ids_complete)) {
      loadings$display_id <- as.character(ids_complete$display_id)
    } else {
      loadings$display_id <- sub("^[PM]:", "", loadings$protein_id)
    }
  } else {
    protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
    if (nzchar(protein_col) && protein_col %in% names(ids_complete)) {
      loadings$protein_id <- as.character(ids_complete[[protein_col]])
    } else {
      loadings$protein_id <- rownames(mat_complete)
    }
  }

  # Reorder columns
  extra_cols <- intersect(c("entity_type", "display_id"), names(loadings))
  loadings <- loadings[, c("protein_id", extra_cols, pc_cols), drop = FALSE]
  rownames(loadings) <- NULL

  # Compute variance explained - use n_pcs_scree for scree plot (more PCs)
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  n_var <- min(n_pcs_scree, length(var_explained))
  variance_df <- data.frame(
    PC = paste0("PC", seq_len(n_var)),
    variance_explained = var_explained[1:n_var],
    cumulative = cumsum(var_explained[1:n_var]),
    stringsAsFactors = FALSE
  )

  # Log variance explained (handle case where fewer than 2 PCs)
  if (length(var_explained) >= 2) {
    add_log("INFO", sprintf("Variance explained: PC1=%.1f%%, PC2=%.1f%%",
                            var_explained[1] * 100, var_explained[2] * 100))
  } else if (length(var_explained) >= 1) {
    add_log("INFO", sprintf("Variance explained: PC1=%.1f%%", var_explained[1] * 100))
  }

  # Build frozen sets for paired children (top/bottom N by loading) - use n_pcs_paired
  add_log("INFO", sprintf("Building loading sets (top/bottom %d per PC)...", top_n))
  sets <- list()
  for (pc_idx in seq_len(min(n_pcs_paired, ncol(pca_result$rotation)))) {
    pc_name <- paste0("PC", pc_idx)
    pc_loadings <- loadings[[pc_name]]

    # Top N (positive loadings)
    top_idx <- order(pc_loadings, decreasing = TRUE)[1:min(top_n, length(pc_loadings))]
    sets[[paste0(pc_name, "_top")]] <- loadings$protein_id[top_idx]

    # Bottom N (negative loadings)
    bottom_idx <- order(pc_loadings, decreasing = FALSE)[1:min(top_n, length(pc_loadings))]
    sets[[paste0(pc_name, "_bottom")]] <- loadings$protein_id[bottom_idx]

    # Full ranked list for 1D-GOFCS
    ranked_idx <- order(pc_loadings, decreasing = TRUE)
    sets[[paste0(pc_name, "_loadings")]] <- setNames(
      pc_loadings[ranked_idx],
      loadings$protein_id[ranked_idx]
    )
  }

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("PCA completed in %.2f seconds (%d samples, %d features)",
                          engine_duration, ncol(mat_complete), nrow(mat_complete)))

  # Build log data.frame from accumulated entries
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  # Extract group colors from metadata (same pattern as heatmap/hor_dis)
  groups <- unique(as.character(scores$group %||% payload$groups %||% character()))
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

  list(
    engine_id = "pca",
    params = params,
    data = list(
      scores = scores,
      loadings = loadings,
      variance = variance_df,
      sets = sets,
      group_colors = group_colors,
      is_mixed_entity = is_mixed_entity,
      n_complete = nrow(mat_complete),
      n_excluded = sum(!complete_rows),
      log = log_df
    )
  )
}
