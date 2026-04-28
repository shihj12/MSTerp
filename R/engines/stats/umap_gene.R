# =========================================================
# R/engines/stats/umap_gene.R - Gene-level UMAP Engine
#
# Default mode: one point = one melted feature/sample observation,
# embedded from measurement-derived numeric features and colored by
# the observation's sample group.
#
# Feature-profile mode: one point = one feature (protein/metabolite).
# The matrix is kept as features x samples; each feature becomes a data
# point embedded by similarity of its profile across samples.
#
# Coloring modes:
#   - sample_group colour by dominant sample group profile
#   - none         single colour for all points
#   - target_list  highlight a user-provided list of IDs
#   - cluster      k-means on the UMAP layout
#
# Contract:
#   - data$scores: data.frame(feature_id, display_id, UMAP1, UMAP2,
#                             sample_group, is_target, cluster)
#   - data$color_mode, data$group_colors, data$cluster_colors, data$target_color
#   - data$n_complete, data$n_excluded, data$matched_targets,
#     data$unmatched_targets, data$log
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

stats_umap_gene_run <- function(payload, params = NULL, context = NULL) {
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

  empty_scores <- function() {
    data.frame(feature_id = character(0), display_id = character(0),
               UMAP1 = numeric(0), UMAP2 = numeric(0),
               sample_group = character(0),
               is_target = logical(0), cluster = integer(0),
               stringsAsFactors = FALSE)
  }
  build_log_df <- function() {
    if (length(log_entries) == 0) {
      return(data.frame(time = character(0), level = character(0),
                        message = character(0), stringsAsFactors = FALSE))
    }
    do.call(rbind, lapply(log_entries, function(e) {
      data.frame(time = e$time, level = e$level, message = e$message,
                 stringsAsFactors = FALSE)
    }))
  }
  early_return <- function() {
    list(engine_id = "umap_gene", params = params,
         data = list(scores = empty_scores(),
                     embedding_mode = params$embedding_mode %||% "melted_observation",
                     color_mode = params$color_mode %||% "sample_group",
                     group_colors = character(0),
                     log = build_log_df()))
  }

  if (!isTRUE(payload$ok)) {
    add_log("ERROR", payload$error %||% "Invalid payload")
    return(early_return())
  }

  if (!requireNamespace("uwot", quietly = TRUE)) {
    add_log("ERROR", "Package 'uwot' is required for UMAP")
    return(early_return())
  }

  mat <- payload$mat
  ids <- payload$ids
  samples <- payload$samples

  add_log("INFO", sprintf("Initiating gene-level UMAP: %d features x %d samples",
                          nrow(mat), ncol(mat)))

  # ---- Parameters ----
  embedding_mode <- params$embedding_mode %||% "melted_observation"
  if (!embedding_mode %in% c("melted_observation", "feature_profile")) {
    embedding_mode <- "melted_observation"
  }
  log_transform <- params$log_transform %||% "log2"
  scale_method <- params$scale_method %||% "zscore"
  missing_handling <- params$missing_handling %||% "drop_gene"
  min_valid_fraction <- as.numeric(params$min_valid_fraction %||% 0.7)
  variance_filter_top_n <- as.integer(params$variance_filter_top_n %||% 2000)
  n_neighbors <- as.integer(params$n_neighbors %||% 15)
  min_dist <- as.numeric(params$min_dist %||% 0.1)
  spread <- as.numeric(params$spread %||% 1)
  n_epochs_param <- as.integer(params$n_epochs %||% 0)
  init <- params$init %||% "spectral"
  metric <- params$metric %||% "euclidean"
  seed <- as.integer(params$seed %||% 42)
  color_mode <- params$color_mode %||% "sample_group"
  if (!color_mode %in% c("sample_group", "none", "target_list", "cluster")) color_mode <- "sample_group"
  n_clusters <- as.integer(params$n_clusters %||% 6)

  build_group_colors <- function(groups, smp) {
    groups <- unique(as.character(groups %||% character()))
    groups <- groups[!is.na(groups) & nzchar(groups)]
    if (length(groups) == 0) return(character(0))

    meta_groups <- payload$metadata$groups
    if (!is.null(meta_groups) && is.data.frame(meta_groups) &&
        "group_name" %in% names(meta_groups) && "color" %in% names(meta_groups)) {
      color_map <- stats::setNames(
        as.character(meta_groups$color),
        as.character(meta_groups$group_name)
      )
      group_colors <- color_map[groups]
    } else if (is.data.frame(smp) && "color" %in% names(smp)) {
      color_df <- unique(data.frame(
        group = as.character(smp$group_name),
        color = as.character(smp$color),
        stringsAsFactors = FALSE
      ))
      color_df <- color_df[!is.na(color_df$color) & nzchar(color_df$color), , drop = FALSE]
      color_map <- stats::setNames(color_df$color[!duplicated(color_df$group)],
                                   color_df$group[!duplicated(color_df$group)])
      group_colors <- color_map[groups]
    } else {
      group_colors <- rep(NA_character_, length(groups))
      names(group_colors) <- groups
    }

    missing <- is.na(group_colors) | !nzchar(group_colors)
    if (any(missing)) {
      auto_colors <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
      group_colors[missing] <- auto_colors
    }
    names(group_colors) <- groups
    group_colors
  }

  resolve_feature_ids <- function() {
    ids_kept <- if (!is.null(ids) && nrow(ids) >= max(keep_idx)) {
      ids[keep_idx, , drop = FALSE]
    } else {
      NULL
    }

    protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
    if (!is.null(ids_kept) && nzchar(protein_col) && protein_col %in% names(ids_kept)) {
      feature_id <- as.character(ids_kept[[protein_col]])
    } else {
      feature_id <- as.character(rownames(mat) %||% paste0("feat_", keep_idx))
    }
    if (!is.null(ids_kept) && "display_id" %in% names(ids_kept)) {
      display_id <- as.character(ids_kept$display_id)
    } else {
      gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
      if (!is.null(ids_kept) && nzchar(gene_col) && gene_col %in% names(ids_kept)) {
        display_id <- as.character(ids_kept[[gene_col]])
      } else {
        display_id <- feature_id
      }
    }

    list(feature_id = feature_id, display_id = display_id)
  }

  scale_observation_input <- function(x) {
    col_sds <- apply(x, 2, stats::sd, na.rm = TRUE)
    keep_cols <- is.finite(col_sds) & col_sds > 0
    if (any(!keep_cols)) {
      add_log("INFO", sprintf("Removing %d zero-variance observation feature(s)", sum(!keep_cols)))
      x <- x[, keep_cols, drop = FALSE]
      col_sds <- col_sds[keep_cols]
    }
    if (ncol(x) == 0) return(x)

    if (scale_method == "zscore") {
      add_log("INFO", "Applied observation-level z-score scaling")
      scale(x, center = TRUE, scale = TRUE)
    } else if (scale_method == "pareto") {
      add_log("INFO", "Applied observation-level Pareto scaling (centre / sqrt(SD))")
      scale(x, center = TRUE, scale = sqrt(col_sds))
    } else if (scale_method == "covariance") {
      add_log("INFO", "Applied observation-level mean-centering (covariance)")
      scale(x, center = TRUE, scale = FALSE)
    } else {
      add_log("INFO", "No observation-level scaling applied (raw values)")
      x
    }
  }

  apply_coloring <- function(scores, emb, n_points_eff) {
    matched_targets <- character(0)
    unmatched_targets <- character(0)
    cluster_colors <- character(0)
    target_color <- "#E74C3C"

    if (color_mode == "target_list") {
      raw_targets <- params$target_list %||% ""
      targets <- unlist(strsplit(as.character(raw_targets), "\\r?\\n", perl = TRUE),
                        use.names = FALSE)
      targets <- trimws(targets)
      targets <- targets[nzchar(targets)]
      targets <- targets[!duplicated(targets)]

      if (length(targets) == 0) {
        add_log("WARN", "color_mode='target_list' but no targets provided")
      } else if (length(targets) > 2000) {
        add_log("WARN", sprintf("Target list capped at 2000 (received %d)", length(targets)))
        targets <- targets[seq_len(2000)]
      }

      fid <- scores$feature_id
      did <- scores$display_id
      is_match_id <- fid %in% targets
      is_match_display <- toupper(did) %in% toupper(targets)
      scores$is_target <- is_match_id | is_match_display

      matched_set <- unique(c(fid[is_match_id], did[is_match_display]))
      matched_targets <- intersect(targets, matched_set)
      if (length(matched_targets) == 0 && length(targets) > 0) {
        matched_targets <- targets[toupper(targets) %in% toupper(matched_set)]
      }
      unmatched_targets <- setdiff(targets, matched_targets)

      add_log("INFO", sprintf("Target list: matched %d / %d",
                              length(matched_targets), length(targets)))
    } else if (color_mode == "cluster") {
      k <- max(2L, min(n_clusters, n_points_eff - 1L))
      if (k != n_clusters) {
        add_log("INFO", sprintf("Adjusted n_clusters from %d to %d", n_clusters, k))
      }
      set.seed(seed)
      km <- tryCatch(
        stats::kmeans(emb, centers = k, nstart = 10, iter.max = 50),
        error = function(e) {
          add_log("ERROR", sprintf("kmeans failed: %s", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(km)) {
        scores$cluster <- as.integer(km$cluster)
        cluster_levels <- as.character(seq_len(k))
        cluster_colors <- grDevices::hcl.colors(k, palette = "Dark 3")
        names(cluster_colors) <- cluster_levels
        add_log("INFO", sprintf("Assigned k-means clusters (k=%d)", k))
      }
    }

    list(
      scores = scores,
      target_color = target_color,
      cluster_colors = cluster_colors,
      matched_targets = matched_targets,
      unmatched_targets = unmatched_targets
    )
  }

  # ---- Log transform ----
  if (log_transform == "log10") {
    add_log("INFO", "Applying log10 transform")
    mat[mat <= 0] <- NA_real_
    mat <- log10(mat)
  } else if (log_transform == "log2") {
    add_log("INFO", "Applying log2 transform")
    mat[mat <= 0] <- NA_real_
    mat <- log2(mat)
  }

  # ---- Track row indices through filtering so ids stays aligned ----
  keep_idx <- seq_len(nrow(mat))

  # ---- Missing-value handling (row-wise) ----
  n_samples <- ncol(mat)
  valid_frac <- rowSums(!is.na(mat)) / max(n_samples, 1)
  keep_rows <- valid_frac >= min_valid_fraction
  n_dropped_valid <- sum(!keep_rows)
  if (n_dropped_valid > 0) {
    add_log("INFO", sprintf("Dropping %d feature(s) below min_valid_fraction=%.2f",
                            n_dropped_valid, min_valid_fraction))
    mat <- mat[keep_rows, , drop = FALSE]
    keep_idx <- keep_idx[keep_rows]
  }

  if (missing_handling == "drop_gene") {
    complete_rows <- complete.cases(mat)
    n_dropped_na <- sum(!complete_rows)
    if (n_dropped_na > 0) {
      add_log("INFO", sprintf("Dropping %d feature(s) with any remaining NA", n_dropped_na))
    }
    mat <- mat[complete_rows, , drop = FALSE]
    keep_idx <- keep_idx[complete_rows]
  } else if (missing_handling == "min_impute") {
    for (i in seq_len(nrow(mat))) {
      row_vals <- mat[i, ]
      na_idx <- is.na(row_vals)
      if (any(na_idx) && any(!na_idx)) {
        mat[i, na_idx] <- min(row_vals, na.rm = TRUE)
      }
    }
    mat[is.na(mat)] <- 0
  } else if (missing_handling == "zero_impute") {
    mat[is.na(mat)] <- 0
  }

  if (nrow(mat) < 10 || ncol(mat) < 3) {
    add_log("WARN", sprintf("Insufficient data after filtering: %d features x %d samples",
                            nrow(mat), ncol(mat)))
    return(early_return())
  }

  # ---- Variance filter (keep top-N features) ----
  if (variance_filter_top_n > 0 && nrow(mat) > variance_filter_top_n) {
    variance_vec <- apply(mat, 1, stats::var, na.rm = TRUE)
    variance_vec[!is.finite(variance_vec)] <- 0
    top_idx <- order(variance_vec, decreasing = TRUE)[seq_len(variance_filter_top_n)]
    top_idx <- sort(top_idx)
    mat <- mat[top_idx, , drop = FALSE]
    keep_idx <- keep_idx[top_idx]
    add_log("INFO", sprintf("Variance filter: kept top %d features", variance_filter_top_n))
  }

  if (embedding_mode == "melted_observation") {
    if (!is.data.frame(samples) || nrow(samples) == 0 ||
        !all(c("sample_col", "group_name") %in% names(samples))) {
      add_log("WARN", "Melted observation UMAP requires payload$samples with sample_col and group_name")
      return(early_return())
    }

    smp <- samples
    smp$sample_col <- as.character(smp$sample_col)
    smp$group_name <- as.character(smp$group_name)
    smp <- smp[!is.na(smp$sample_col) & nzchar(smp$sample_col) &
                 !is.na(smp$group_name) & nzchar(smp$group_name), , drop = FALSE]
    smp <- smp[smp$sample_col %in% colnames(mat), , drop = FALSE]
    if (nrow(smp) == 0) {
      add_log("WARN", "No sample metadata rows matched UMAP matrix columns")
      return(early_return())
    }

    sample_order <- intersect(as.character(smp$sample_col), colnames(mat))
    mat_obs <- mat[, sample_order, drop = FALSE]
    smp <- smp[match(sample_order, smp$sample_col), , drop = FALSE]

    obs_idx <- which(!is.na(mat_obs), arr.ind = TRUE)
    if (nrow(obs_idx) < 10) {
      add_log("WARN", sprintf("Insufficient melted observations for UMAP: %d", nrow(obs_idx)))
      return(early_return())
    }

    ids_resolved <- resolve_feature_ids()
    feature_mean <- rowMeans(mat_obs, na.rm = TRUE)
    feature_sd <- apply(mat_obs, 1, stats::sd, na.rm = TRUE)
    feature_sd[!is.finite(feature_sd)] <- 0

    groups <- unique(as.character(smp$group_name))
    groups <- groups[!is.na(groups) & nzchar(groups)]
    group_colors <- build_group_colors(groups, smp)

    group_mean_mat <- matrix(NA_real_, nrow = nrow(mat_obs), ncol = length(groups),
                             dimnames = list(NULL, groups))
    for (grp in groups) {
      cols <- smp$sample_col[smp$group_name == grp]
      cols <- intersect(cols, colnames(mat_obs))
      if (length(cols) > 0) {
        group_mean_mat[, grp] <- rowMeans(mat_obs[, cols, drop = FALSE], na.rm = TRUE)
      }
    }

    obs_feature_idx <- obs_idx[, 1]
    obs_sample_idx <- obs_idx[, 2]
    obs_sample <- colnames(mat_obs)[obs_sample_idx]
    obs_group <- as.character(smp$group_name[obs_sample_idx])
    obs_value <- mat_obs[obs_idx]
    obs_group_col <- match(obs_group, colnames(group_mean_mat))
    obs_group_mean <- group_mean_mat[cbind(obs_feature_idx, obs_group_col)]
    obs_feature_mean <- feature_mean[obs_feature_idx]
    obs_feature_sd <- feature_sd[obs_feature_idx]
    obs_group_delta <- obs_group_mean - obs_feature_mean
    obs_replicate_delta <- obs_value - obs_group_mean
    obs_value_rank <- rank(obs_value, ties.method = "average") / max(length(obs_value), 1)

    umap_input <- cbind(
      value = obs_value,
      feature_mean = obs_feature_mean,
      feature_sd = obs_feature_sd,
      group_mean = obs_group_mean,
      group_delta = obs_group_delta,
      replicate_delta = obs_replicate_delta,
      value_rank = obs_value_rank
    )
    keep_obs <- stats::complete.cases(umap_input) & !is.na(obs_group) & nzchar(obs_group)
    if (sum(keep_obs) < 10) {
      add_log("WARN", sprintf("Insufficient complete melted observations for UMAP: %d", sum(keep_obs)))
      return(early_return())
    }

    umap_input <- umap_input[keep_obs, , drop = FALSE]
    obs_feature_idx <- obs_feature_idx[keep_obs]
    obs_sample <- obs_sample[keep_obs]
    obs_group <- obs_group[keep_obs]
    obs_value <- obs_value[keep_obs]
    obs_group_mean <- obs_group_mean[keep_obs]
    obs_feature_mean <- obs_feature_mean[keep_obs]
    obs_group_delta <- obs_group_delta[keep_obs]
    obs_replicate_delta <- obs_replicate_delta[keep_obs]

    umap_input <- scale_observation_input(umap_input)
    if (ncol(umap_input) < 2) {
      add_log("WARN", "Melted observation UMAP needs at least two non-constant numeric inputs")
      return(early_return())
    }

    n_points_eff <- nrow(umap_input)
    n_neighbors_eff <- max(2L, min(n_neighbors, n_points_eff - 1L))
    if (n_neighbors_eff != n_neighbors) {
      add_log("INFO", sprintf("Adjusted n_neighbors from %d to %d (observations=%d)",
                              n_neighbors, n_neighbors_eff, n_points_eff))
    }

    n_epochs_arg <- if (n_epochs_param > 0) n_epochs_param else NULL
    add_log("INFO", sprintf(
      "Running uwot::umap in melted observation mode (n=%d observations, features=%d, neighbors=%d, metric=%s, min_dist=%.3g, spread=%.3g, init=%s, n_epochs=%s)",
      n_points_eff, ncol(umap_input), n_neighbors_eff, metric, min_dist, spread, init,
      if (is.null(n_epochs_arg)) "auto" else as.character(n_epochs_arg)
    ))
    set.seed(seed)
    emb <- tryCatch(
      uwot::umap(umap_input, n_neighbors = n_neighbors_eff, min_dist = min_dist,
                 spread = spread, init = init, n_epochs = n_epochs_arg,
                 metric = metric, n_components = 2L, verbose = FALSE),
      error = function(e) {
        add_log("ERROR", sprintf("uwot::umap failed: %s", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(emb)) return(early_return())

    scores <- data.frame(
      feature_id = ids_resolved$feature_id[obs_feature_idx],
      display_id = ids_resolved$display_id[obs_feature_idx],
      sample = obs_sample,
      sample_group = obs_group,
      value = obs_value,
      feature_mean = obs_feature_mean,
      sample_group_mean = obs_group_mean,
      group_delta = obs_group_delta,
      replicate_delta = obs_replicate_delta,
      UMAP1 = emb[, 1],
      UMAP2 = emb[, 2],
      is_target = FALSE,
      cluster = NA_integer_,
      stringsAsFactors = FALSE
    )

    coloring <- apply_coloring(scores, emb, n_points_eff)
    scores <- coloring$scores

    engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
    add_log("INFO", sprintf("Gene-level UMAP completed in %.2f seconds (%d observations, %d features, %d samples)",
                            engine_duration, nrow(scores), nrow(mat_obs), ncol(mat_obs)))

    return(list(
      engine_id = "umap_gene",
      params = params,
      data = list(
        scores = scores,
        embedding_mode = embedding_mode,
        color_mode = color_mode,
        group_colors = group_colors,
        target_color = coloring$target_color,
        cluster_colors = coloring$cluster_colors,
        matched_targets = coloring$matched_targets,
        unmatched_targets = coloring$unmatched_targets,
        n_complete = nrow(scores),
        n_features = nrow(mat_obs),
        n_samples = ncol(mat_obs),
        n_excluded = n_dropped_valid,
        log = build_log_df()
      )
    ))
  }

  # ---- Per-feature scaling across samples ----
  if (scale_method %in% c("zscore", "pareto")) {
    row_sds <- apply(mat, 1, stats::sd, na.rm = TRUE)
    nonzero <- is.finite(row_sds) & row_sds > 0
    if (any(!nonzero)) {
      add_log("INFO", sprintf("Removing %d zero-variance features", sum(!nonzero)))
      mat <- mat[nonzero, , drop = FALSE]
      keep_idx <- keep_idx[nonzero]
      row_sds <- row_sds[nonzero]
    }
    row_means <- rowMeans(mat, na.rm = TRUE)
    if (scale_method == "zscore") {
      mat <- (mat - row_means) / row_sds
      add_log("INFO", "Applied per-feature z-score scaling")
    } else {
      mat <- (mat - row_means) / sqrt(row_sds)
      add_log("INFO", "Applied per-feature Pareto scaling (centre / sqrt(SD))")
    }
  } else if (scale_method == "covariance") {
    row_means <- rowMeans(mat, na.rm = TRUE)
    mat <- mat - row_means
    add_log("INFO", "Applied per-feature mean-centering (covariance)")
  } else {
    add_log("INFO", "No scaling applied (raw values)")
  }

  # ---- Sample group assignment for feature points ----
  # A gene-level UMAP point is a feature, not a sample. For sample-group
  # colouring, assign each feature to the group with the highest processed
  # group mean across its replicate columns.
  group_colors <- character(0)
  sample_group <- rep(NA_character_, nrow(mat))
  sample_group_mean <- rep(NA_real_, nrow(mat))
  if (is.data.frame(samples) && nrow(samples) > 0 &&
      all(c("sample_col", "group_name") %in% names(samples))) {
    smp <- samples
    smp$sample_col <- as.character(smp$sample_col)
    smp$group_name <- as.character(smp$group_name)
    smp <- smp[!is.na(smp$sample_col) & nzchar(smp$sample_col) &
                 !is.na(smp$group_name) & nzchar(smp$group_name), , drop = FALSE]
    smp <- smp[smp$sample_col %in% colnames(mat), , drop = FALSE]

    groups <- unique(smp$group_name)
    groups <- groups[nzchar(groups)]
    if (length(groups) > 0) {
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
      } else if ("color" %in% names(smp)) {
        color_df <- unique(data.frame(
          group = smp$group_name,
          color = as.character(smp$color),
          stringsAsFactors = FALSE
        ))
        color_df <- color_df[!is.na(color_df$color) & nzchar(color_df$color), , drop = FALSE]
        color_map <- stats::setNames(color_df$color[!duplicated(color_df$group)],
                                     color_df$group[!duplicated(color_df$group)])
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

      group_means <- vapply(groups, function(grp) {
        cols <- smp$sample_col[smp$group_name == grp]
        cols <- intersect(cols, colnames(mat))
        if (length(cols) == 0) return(rep(NA_real_, nrow(mat)))
        rowMeans(mat[, cols, drop = FALSE], na.rm = TRUE)
      }, numeric(nrow(mat)))
      if (!is.matrix(group_means)) {
        group_means <- matrix(group_means, ncol = length(groups),
                              dimnames = list(NULL, groups))
      }
      group_means[!is.finite(group_means)] <- NA_real_
      row_has_group <- rowSums(!is.na(group_means)) > 0
      if (any(row_has_group)) {
        group_means_for_max <- group_means
        group_means_for_max[is.na(group_means_for_max)] <- -Inf
        max_idx <- max.col(group_means_for_max, ties.method = "first")
        sample_group[row_has_group] <- groups[max_idx[row_has_group]]
        sample_group_mean[row_has_group] <- group_means[cbind(which(row_has_group), max_idx[row_has_group])]
      }
      add_log("INFO", sprintf("Assigned dominant sample group for %d / %d feature(s)",
                              sum(!is.na(sample_group)), nrow(mat)))
    }
  }

  # ---- Resolve feature IDs aligned with surviving rows ----
  ids_kept <- if (!is.null(ids) && nrow(ids) >= max(keep_idx)) {
    ids[keep_idx, , drop = FALSE]
  } else {
    NULL
  }

  protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
  if (!is.null(ids_kept) && nzchar(protein_col) && protein_col %in% names(ids_kept)) {
    feature_id <- as.character(ids_kept[[protein_col]])
  } else {
    feature_id <- as.character(rownames(mat) %||% paste0("feat_", keep_idx))
  }
  if (!is.null(ids_kept) && "display_id" %in% names(ids_kept)) {
    display_id <- as.character(ids_kept$display_id)
  } else {
    gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
    if (!is.null(ids_kept) && nzchar(gene_col) && gene_col %in% names(ids_kept)) {
      display_id <- as.character(ids_kept[[gene_col]])
    } else {
      display_id <- feature_id
    }
  }

  # uwot's n_neighbors must be < n
  n_features_eff <- nrow(mat)
  n_neighbors_eff <- max(2L, min(n_neighbors, n_features_eff - 1L))
  if (n_neighbors_eff != n_neighbors) {
    add_log("INFO", sprintf("Adjusted n_neighbors from %d to %d (features=%d)",
                            n_neighbors, n_neighbors_eff, n_features_eff))
  }

  # ---- Run UMAP (no transpose; features are rows = data points) ----
  n_epochs_arg <- if (n_epochs_param > 0) n_epochs_param else NULL
  add_log("INFO", sprintf(
    "Running uwot::umap (n=%d features, neighbors=%d, metric=%s, min_dist=%.3g, spread=%.3g, init=%s, n_epochs=%s)",
    n_features_eff, n_neighbors_eff, metric, min_dist, spread, init,
    if (is.null(n_epochs_arg)) "auto" else as.character(n_epochs_arg)
  ))
  set.seed(seed)
  emb <- tryCatch(
    uwot::umap(mat, n_neighbors = n_neighbors_eff, min_dist = min_dist,
               spread = spread, init = init, n_epochs = n_epochs_arg,
               metric = metric, n_components = 2L, verbose = FALSE),
    error = function(e) {
      add_log("ERROR", sprintf("uwot::umap failed: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(emb)) return(early_return())

  scores <- data.frame(
    feature_id = feature_id,
    display_id = display_id,
    UMAP1 = emb[, 1],
    UMAP2 = emb[, 2],
    sample_group = sample_group,
    sample_group_mean = sample_group_mean,
    is_target = FALSE,
    cluster = NA_integer_,
    stringsAsFactors = FALSE
  )

  # ---- Coloring ----
  matched_targets <- character(0)
  unmatched_targets <- character(0)
  cluster_colors <- character(0)
  target_color <- "#E74C3C"

  if (color_mode == "target_list") {
    raw_targets <- params$target_list %||% ""
    targets <- unlist(strsplit(as.character(raw_targets), "\\r?\\n", perl = TRUE),
                      use.names = FALSE)
    targets <- trimws(targets)
    targets <- targets[nzchar(targets)]
    targets <- targets[!duplicated(targets)]

    if (length(targets) == 0) {
      add_log("WARN", "color_mode='target_list' but no targets provided")
    } else if (length(targets) > 2000) {
      add_log("WARN", sprintf("Target list capped at 2000 (received %d)", length(targets)))
      targets <- targets[seq_len(2000)]
    }

    # Match against feature_id and display_id (case-insensitive on display)
    fid <- scores$feature_id
    did <- scores$display_id
    is_match_id <- fid %in% targets
    is_match_display <- toupper(did) %in% toupper(targets)
    scores$is_target <- is_match_id | is_match_display

    matched_set <- unique(c(fid[is_match_id], did[is_match_display]))
    matched_targets <- intersect(targets, matched_set)
    if (length(matched_targets) == 0 && length(targets) > 0) {
      # try case-insensitive match against targets directly
      matched_targets <- targets[toupper(targets) %in% toupper(matched_set)]
    }
    unmatched_targets <- setdiff(targets, matched_targets)

    add_log("INFO", sprintf("Target list: matched %d / %d",
                            length(matched_targets), length(targets)))
  } else if (color_mode == "cluster") {
    k <- max(2L, min(n_clusters, n_features_eff - 1L))
    if (k != n_clusters) {
      add_log("INFO", sprintf("Adjusted n_clusters from %d to %d", n_clusters, k))
    }
    set.seed(seed)
    km <- tryCatch(
      stats::kmeans(emb, centers = k, nstart = 10, iter.max = 50),
      error = function(e) {
        add_log("ERROR", sprintf("kmeans failed: %s", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(km)) {
      scores$cluster <- as.integer(km$cluster)
      cluster_levels <- as.character(seq_len(k))
      cluster_colors <- grDevices::hcl.colors(k, palette = "Dark 3")
      names(cluster_colors) <- cluster_levels
      add_log("INFO", sprintf("Assigned k-means clusters (k=%d)", k))
    }
  }

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Gene-level UMAP completed in %.2f seconds (%d features, %d samples)",
                          engine_duration, n_features_eff, ncol(mat)))

  list(
    engine_id = "umap_gene",
    params = params,
    data = list(
      scores = scores,
      embedding_mode = embedding_mode,
      color_mode = color_mode,
      group_colors = group_colors,
      target_color = target_color,
      cluster_colors = cluster_colors,
      matched_targets = matched_targets,
      unmatched_targets = unmatched_targets,
      n_complete = n_features_eff,
      n_excluded = n_dropped_valid,
      log = build_log_df()
    )
  )
}
