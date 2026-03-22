# =========================================================
# R/engines/stats/idquant.R — ID Quantification engines
#
# Legacy:
#   - stats_idquant_run() produces a single-engine output with shared raw data
#
# Container child engines:
#   - stats_idquant_id_quant_run()
#   - stats_idquant_average_value_run()
#   - stats_idquant_cv_scatter_run()
#   - stats_idquant_cv_bar_run()
#   - stats_idquant_overlap_run()
# =========================================================

idquant_compute_shared <- function(payload, params) {
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
    log_df <- data.frame(
      time = format(Sys.time()),
      level = "ERROR",
      message = payload$error %||% "Invalid payload",
      stringsAsFactors = FALSE
    )

    return(list(
      ok = FALSE,
      params = params,
      data = list(
        counts = data.frame(group = character(), metric = character(), n = integer(), stringsAsFactors = FALSE),
        log = log_df
      )
    ))
  }

  mat <- payload$mat
  samples <- payload$samples
  groups <- payload$groups

  add_log("INFO", sprintf("Initiating idquant: %d proteins x %d groups", nrow(mat), length(groups)))

  counts_list <- list()
  replicate_counts_list <- list()

  for (grp in groups) {
    grp_samples <- samples[samples$group_name == grp, , drop = FALSE]
    grp_cols <- intersect(grp_samples$sample_col, colnames(mat))
    if (length(grp_cols) == 0) next

    grp_mat <- mat[, grp_cols, drop = FALSE]
    n_reps <- ncol(grp_mat)
    add_log("INFO", sprintf("Processing group '%s': %d replicates", grp, n_reps))

    # Identified: non-NA in at least 1 replicate
    has_any <- apply(grp_mat, 1, function(x) any(!is.na(x) & is.finite(x) & x > 0))
    n_identified <- sum(has_any)

    # Quantified: non-NA in ALL replicates
    has_all <- apply(grp_mat, 1, function(x) all(!is.na(x) & is.finite(x) & x > 0))
    n_quantified <- sum(has_all)

    add_log("INFO", sprintf("  Identified: %d, Quantified: %d", n_identified, n_quantified))

    counts_list <- c(counts_list, list(data.frame(
      group = grp,
      metric = c("identified", "quantified"),
      n = c(n_identified, n_quantified),
      stringsAsFactors = FALSE
    )))

    for (j in seq_along(grp_cols)) {
      col <- grp_cols[j]
      rep_num <- grp_samples$replicate[grp_samples$sample_col == col]
      if (length(rep_num) == 0 || is.na(rep_num)) rep_num <- j

      vals <- mat[, col]
      n_detected <- sum(!is.na(vals) & is.finite(vals) & vals > 0)

      replicate_counts_list <- c(replicate_counts_list, list(data.frame(
        group = grp,
        replicate = as.integer(rep_num),
        sample_col = col,
        n = n_detected,
        stringsAsFactors = FALSE
      )))
    }
  }

  counts_df <- if (length(counts_list) > 0) do.call(rbind, counts_list) else data.frame(
    group = character(0), metric = character(0), n = integer(0), stringsAsFactors = FALSE
  )

  replicate_counts_df <- if (length(replicate_counts_list) > 0) do.call(rbind, replicate_counts_list) else NULL

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("IDQuant core computed in %.2f seconds (%d groups)", engine_duration, length(groups)))

  log_df <- do.call(rbind, lapply(log_entries, function(e) data.frame(
    time = e$time, level = e$level, message = e$message, stringsAsFactors = FALSE
  )))

  # Viewer-time default used by overlap renderer.
  params$overlap_plot_type <- params$overlap_plot_type %||% if (length(groups) > 4) "upset" else "venn"

  # Extract group colors from metadata (same pattern as heatmap/hor_dis)
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

  data <- list(
    counts = counts_df,
    log = log_df,
    intensity_mat = mat,
    ids = payload$ids %||% NULL,
    sample_meta = samples,
    groups = groups,
    group_colors = group_colors
  )
  if (!is.null(replicate_counts_df)) data$replicate_counts <- replicate_counts_df

  list(ok = TRUE, params = params, data = data)
}

# ---- Engines -----------------------------------------------------------------

# Legacy single-engine output (kept for backward compatibility)
stats_idquant_run <- function(payload, params = NULL, context = NULL) {
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant", params = core$params, data = core$data)
}

stats_idquant_id_quant_run <- function(payload, params = NULL, context = NULL) {
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant_id_quant", params = core$params, data = core$data)
}

stats_idquant_average_value_run <- function(payload, params = NULL, context = NULL) {
  core <- idquant_compute_shared(payload, params = params)

  mat <- core$data$intensity_mat
  smeta <- core$data$sample_meta
  groups <- core$data$groups

  avg_list <- list()
  sample_avg_list <- list()
  for (grp in groups) {
    cols <- smeta$sample_col[smeta$group_name == grp]
    cols <- intersect(as.character(cols), colnames(mat))
    if (length(cols) == 0) next

    grp_mat <- mat[, cols, drop = FALSE]
    protein_means <- rowMeans(grp_mat, na.rm = TRUE)
    protein_means <- protein_means[is.finite(protein_means) & !is.na(protein_means) & protein_means > 0]

    avg_list[[length(avg_list) + 1L]] <- data.frame(
      group = grp,
      mean_value = mean(protein_means, na.rm = TRUE),
      median_value = stats::median(protein_means, na.rm = TRUE),
      n_proteins = length(protein_means),
      stringsAsFactors = FALSE
    )

    # Per-sample average intensities (for error bars and t-tests)
    for (col in cols) {
      vals <- mat[, col]
      vals <- vals[is.finite(vals) & !is.na(vals) & vals > 0]
      sample_avg_list[[length(sample_avg_list) + 1L]] <- data.frame(
        group = grp,
        sample_col = col,
        mean_value = if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  avg_df <- if (length(avg_list) > 0) do.call(rbind, avg_list) else data.frame(
    group = character(), mean_value = numeric(), median_value = numeric(), n_proteins = integer(),
    stringsAsFactors = FALSE
  )

  sample_avg_df <- if (length(sample_avg_list) > 0) do.call(rbind, sample_avg_list) else data.frame(
    group = character(), sample_col = character(), mean_value = numeric(),
    stringsAsFactors = FALSE
  )

  core$data$average_value <- avg_df
  core$data$sample_averages <- sample_avg_df
  list(engine_id = "idquant_average_value", params = core$params, data = core$data)
}

stats_idquant_cv_scatter_run <- function(payload, params = NULL, context = NULL) {
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant_cv_scatter", params = core$params, data = core$data)
}

stats_idquant_cv_bar_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant_cv_bar", params = core$params, data = core$data)
}

stats_idquant_overlap_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()
  if (is.null(params$overlap_metric) || !nzchar(as.character(params$overlap_metric))) {
    params$overlap_metric <- "detected"
  }
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant_overlap", params = core$params, data = core$data)
}

stats_idquant_overlap_detected_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()
  params$overlap_metric <- "detected"
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant_overlap_detected", params = core$params, data = core$data)
}

stats_idquant_overlap_quantified_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()
  params$overlap_metric <- "quantified"
  core <- idquant_compute_shared(payload, params = params)
  list(engine_id = "idquant_overlap_quantified", params = core$params, data = core$data)
}
