# =========================================================
# R/engines/stats/ma_plot.R — MA Plot Engine (FC-only)
#
# M (log2 fold change, y) vs A (average abundance, x) for each
# pairwise comparison. Significance is classified purely on FC cutoffs.
# No statistical testing (no p-values, no FDR).
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

.ma_pairwise <- function(mat, samples, grp_a, grp_b, primary_ids, display_ids,
                         mean_type, a_axis_transform, fc_threshold,
                         add_log, is_log_transformed = FALSE) {
  add_log("INFO", sprintf("--- MA comparison: %s vs %s ---", grp_a, grp_b))

  cols_a <- intersect(samples$sample_col[samples$group_name == grp_a], colnames(mat))
  cols_b <- intersect(samples$sample_col[samples$group_name == grp_b], colnames(mat))
  if (length(cols_a) == 0 || length(cols_b) == 0) {
    add_log("WARN", sprintf("Skipping %s vs %s: missing columns", grp_a, grp_b))
    return(NULL)
  }

  mat_a <- mat[, cols_a, drop = FALSE]
  mat_b <- mat[, cols_b, drop = FALSE]

  mean_a <- rowMeans(mat_a, na.rm = TRUE)
  mean_b <- rowMeans(mat_b, na.rm = TRUE)

  if (is_log_transformed) {
    log2fc <- mean_b - mean_a
  } else {
    log2fc <- log2(mean_b + 1) - log2(mean_a + 1)
  }

  # A = combined abundance
  if (identical(mean_type, "harmonic")) {
    safe_a <- ifelse(mean_a > 0, mean_a, NA_real_)
    safe_b <- ifelse(mean_b > 0, mean_b, NA_real_)
    a_val <- 2 / (1 / safe_a + 1 / safe_b)
  } else {
    a_val <- (mean_a + mean_b) / 2
  }

  # A-axis transform
  if (identical(a_axis_transform, "log2")) {
    a_val[!is.na(a_val) & a_val <= 0] <- NA_real_
    a_plot <- log2(a_val)
  } else if (identical(a_axis_transform, "log10")) {
    a_val[!is.na(a_val) & a_val <= 0] <- NA_real_
    a_plot <- log10(a_val)
  } else {
    a_plot <- a_val
  }

  results <- data.frame(
    primary_id = primary_ids,
    display_id = display_ids,
    protein_id = primary_ids,
    gene_symbol = display_ids,
    gene_id = display_ids,
    a_value = a_plot,
    log2fc = log2fc,
    mean_a = mean_a,
    mean_b = mean_b,
    stringsAsFactors = FALSE
  )

  valid <- is.finite(results$log2fc) & is.finite(results$a_value)
  results_valid <- results[valid, , drop = FALSE]

  fc_up <- fc_threshold[2]; fc_down <- fc_threshold[1]
  is_up <- results_valid$log2fc >  fc_up
  is_dn <- results_valid$log2fc <  fc_down
  results_valid$significance <- ifelse(is_up, "up", ifelse(is_dn, "down", "ns"))

  add_log("INFO", sprintf("  %d features, up=%d, down=%d", nrow(results_valid), sum(is_up), sum(is_dn)))

  list(
    points = results_valid[, c("primary_id", "display_id", "protein_id",
                               "gene_symbol", "gene_id",
                               "a_value", "log2fc", "mean_a", "mean_b",
                               "significance"), drop = FALSE],
    comparison = list(
      group_a = grp_a, group_b = grp_b,
      label = paste0(grp_b, "_vs_", grp_a),
      mean_type = mean_type, a_axis_transform = a_axis_transform,
      fc_threshold = fc_threshold,
      n_sig_up = sum(is_up), n_sig_down = sum(is_dn),
      n_total = nrow(results_valid)
    )
  )
}

stats_ma_plot_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()
  params <- params %||% payload$params %||% list()

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries[[length(log_entries) + 1]] <<- list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level, message = msg
    )
  }

  empty_result <- function(err) {
    list(
      engine_id = "ma_plot", params = params,
      data = list(
        comparisons = list(),
        summary = data.frame(),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = err, stringsAsFactors = FALSE)
      )
    )
  }

  if (!isTRUE(payload$ok)) return(empty_result(payload$error %||% "Invalid payload"))

  mat <- payload$mat
  samples <- payload$samples
  groups <- payload$groups
  ids <- payload$ids
  if (length(groups) < 2) return(empty_result("MA plot requires at least 2 groups."))

  ctx <- payload$data_type_context %||% msterp_data_type_context("proteomics")

  mean_type <- as.character(params$mean_type %||% "arithmetic")
  a_axis_transform <- as.character(params$a_axis_transform %||% "log2")
  is_log_transformed <- isTRUE(params$is_log_transformed)
  fc_threshold <- params$fc_threshold %||% c(-1, 1)
  if (length(fc_threshold) < 2) fc_threshold <- c(-1, 1)

  add_log("INFO", sprintf("MA plot: %d features, %d groups, mean_type=%s, A-transform=%s",
                          nrow(mat), length(groups), mean_type, a_axis_transform))

  primary_col <- as.character(payload$metadata$id_primary_col %||%
                              payload$metadata$id_protein_col %||% "")[1]
  display_col <- tryCatch(
    ctx_resolve_id_col(ctx, payload$metadata, names(ids)),
    error = function(e) NULL
  )
  if (is.null(display_col) || !nzchar(display_col)) {
    display_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
  }
  primary_ids <- if (nzchar(primary_col) && primary_col %in% names(ids)) {
    as.character(ids[[primary_col]])
  } else {
    rownames(mat)
  }
  display_ids <- if (nzchar(display_col) && display_col %in% names(ids)) {
    as.character(ids[[display_col]])
  } else {
    rep(NA_character_, nrow(mat))
  }

  # Control orientation (mirror volcano)
  meta_groups <- payload$metadata$groups
  is_control <- if (!is.null(meta_groups$is_control)) {
    tolower(as.character(meta_groups$is_control)) %in% c("true", "t", "1", "yes")
  } else {
    rep(FALSE, nrow(meta_groups %||% data.frame()))
  }
  group_names <- as.character(meta_groups$group_name %||% character(0))
  is_control_map <- rep(FALSE, length(groups))
  for (idx in seq_along(groups)) {
    match_idx <- match(groups[idx], group_names)
    if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
  }
  control_only <- isTRUE(params$control_only)
  control_idx <- which(is_control_map)
  has_control <- length(control_idx) == 1

  n_groups <- length(groups)
  comparisons <- list()

  for (i in seq_len(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      if (control_only && has_control) {
        if (!i %in% control_idx && !j %in% control_idx) next
      }
      grp_a <- groups[i]; grp_b <- groups[j]
      if (has_control && j %in% control_idx && !(i %in% control_idx)) {
        tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
      }
      res <- .ma_pairwise(mat, samples, grp_a, grp_b, primary_ids, display_ids,
                          mean_type, a_axis_transform, fc_threshold,
                          add_log, is_log_transformed)
      if (!is.null(res)) {
        comparisons[[paste0(grp_b, "_vs_", grp_a)]] <- res
      }
    }
  }

  summary_df <- if (length(comparisons) > 0) {
    data.frame(
      comparison = names(comparisons),
      n_total    = vapply(comparisons, function(x) x$comparison$n_total, integer(1)),
      n_sig_up   = vapply(comparisons, function(x) x$comparison$n_sig_up, integer(1)),
      n_sig_down = vapply(comparisons, function(x) x$comparison$n_sig_down, integer(1)),
      mean_type  = mean_type,
      a_transform = a_axis_transform,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }

  add_log("INFO", sprintf("MA plot finished in %.2fs",
                          as.numeric(difftime(Sys.time(), engine_start, units = "secs"))))

  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(time = e$time, level = e$level, message = e$message,
               stringsAsFactors = FALSE)
  }))

  list(
    engine_id = "ma_plot",
    params = params,
    data = list(
      comparisons = comparisons,
      summary = summary_df,
      log = log_df
    )
  )
}
