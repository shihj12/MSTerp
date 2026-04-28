# =========================================================
# R/engines/stats/umap.R - Sample-level UMAP Engine
#
# One point = one sample/replicate. Mirrors PCA's paradigm:
#  - Transpose matrix to samples x features
#  - Run UMAP to embed each sample in 2D
#  - Color by group (legend), optional group ellipses/hulls
#
# Contract:
#  - data$scores: data.frame(sample, group, UMAP1, UMAP2)
#  - data$group_colors: named character vector (group -> hex)
#  - data$n_complete, data$n_excluded, data$log
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Execute umap engine (sample-level)
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_umap_run <- function(payload, params = NULL, context = NULL) {
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
    data.frame(sample = character(0), group = character(0),
               UMAP1 = numeric(0), UMAP2 = numeric(0),
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

  if (!isTRUE(payload$ok)) {
    add_log("ERROR", payload$error %||% "Invalid payload")
    return(list(engine_id = "umap", params = params,
                data = list(scores = empty_scores(), log = build_log_df())))
  }

  if (!requireNamespace("uwot", quietly = TRUE)) {
    add_log("ERROR", "Package 'uwot' is required for UMAP")
    return(list(engine_id = "umap", params = params,
                data = list(scores = empty_scores(), log = build_log_df())))
  }

  mat <- payload$mat
  samples <- payload$samples

  add_log("INFO", sprintf("Initiating UMAP: %d features x %d samples",
                          nrow(mat), ncol(mat)))

  # ---- Parameters ----
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
  metric <- params$metric %||% "correlation"
  seed <- as.integer(params$seed %||% 42)

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

  # ---- Missing-value handling (row-wise) ----
  n_samples <- ncol(mat)
  valid_frac <- rowSums(!is.na(mat)) / max(n_samples, 1)
  keep_rows <- valid_frac >= min_valid_fraction
  n_dropped_valid <- sum(!keep_rows)
  if (n_dropped_valid > 0) {
    add_log("INFO", sprintf("Dropping %d feature(s) below min_valid_fraction=%.2f",
                            n_dropped_valid, min_valid_fraction))
    mat <- mat[keep_rows, , drop = FALSE]
  }

  if (missing_handling == "drop_gene") {
    complete_rows <- complete.cases(mat)
    n_dropped_na <- sum(!complete_rows)
    if (n_dropped_na > 0) {
      add_log("INFO", sprintf("Dropping %d feature(s) with any remaining NA", n_dropped_na))
    }
    mat <- mat[complete_rows, , drop = FALSE]
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
    return(list(engine_id = "umap", params = params,
                data = list(scores = empty_scores(), log = build_log_df(),
                            n_complete = nrow(mat), n_excluded = n_dropped_valid)))
  }

  # ---- Variance filter (keep top-N features) ----
  if (variance_filter_top_n > 0 && nrow(mat) > variance_filter_top_n) {
    variance_vec <- apply(mat, 1, stats::var, na.rm = TRUE)
    variance_vec[!is.finite(variance_vec)] <- 0
    top_idx <- order(variance_vec, decreasing = TRUE)[seq_len(variance_filter_top_n)]
    mat <- mat[top_idx, , drop = FALSE]
    add_log("INFO", sprintf("Variance filter: kept top %d features",
                            variance_filter_top_n))
  }

  # ---- Per-feature scaling across samples ----
  if (scale_method %in% c("zscore", "pareto")) {
    row_sds <- apply(mat, 1, stats::sd, na.rm = TRUE)
    nonzero <- is.finite(row_sds) & row_sds > 0
    if (any(!nonzero)) {
      add_log("INFO", sprintf("Removing %d zero-variance features", sum(!nonzero)))
      mat <- mat[nonzero, , drop = FALSE]
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

  # ---- Transpose: samples become rows ----
  mat_t <- t(mat)
  sample_names <- rownames(mat_t)

  # Clamp n_neighbors to a valid range
  n_samples_eff <- nrow(mat_t)
  n_neighbors_eff <- max(2L, min(n_neighbors, n_samples_eff - 1L))
  if (n_neighbors_eff != n_neighbors) {
    add_log("INFO", sprintf("Adjusted n_neighbors from %d to %d (samples=%d)",
                            n_neighbors, n_neighbors_eff, n_samples_eff))
  }

  # ---- Run UMAP ----
  n_epochs_arg <- if (n_epochs_param > 0) n_epochs_param else NULL
  add_log("INFO", sprintf(
    "Running uwot::umap (n=%d samples, neighbors=%d, metric=%s, min_dist=%.3g, spread=%.3g, init=%s, n_epochs=%s)",
    n_samples_eff, n_neighbors_eff, metric, min_dist, spread, init,
    if (is.null(n_epochs_arg)) "auto" else as.character(n_epochs_arg)
  ))
  set.seed(seed)
  emb <- tryCatch(
    uwot::umap(mat_t, n_neighbors = n_neighbors_eff, min_dist = min_dist,
               spread = spread, init = init, n_epochs = n_epochs_arg,
               metric = metric, n_components = 2L, verbose = FALSE),
    error = function(e) {
      add_log("ERROR", sprintf("uwot::umap failed: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(emb)) {
    return(list(engine_id = "umap", params = params,
                data = list(scores = empty_scores(), log = build_log_df())))
  }

  # ---- Build scores data.frame (sample | group | UMAP1 | UMAP2) ----
  scores <- data.frame(
    sample = sample_names,
    UMAP1 = emb[, 1],
    UMAP2 = emb[, 2],
    stringsAsFactors = FALSE
  )
  scores$group <- samples$group_name[match(scores$sample, samples$sample_col)]
  scores <- scores[, c("sample", "group", "UMAP1", "UMAP2"), drop = FALSE]
  rownames(scores) <- NULL

  # ---- Group color map (reuse PCA's pattern) ----
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

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("UMAP completed in %.2f seconds (%d samples, %d features)",
                          engine_duration, n_samples_eff, nrow(mat)))

  list(
    engine_id = "umap",
    params = params,
    data = list(
      scores = scores,
      group_colors = group_colors,
      n_complete = nrow(mat),
      n_excluded = n_dropped_valid,
      log = build_log_df()
    )
  )
}
