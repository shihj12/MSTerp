# =========================================================
# R/engines/stats/fc_histogram.R — Fold Change Histogram Engine
#
# Computes per-feature log2 fold-change for every pairwise group
# comparison. The renderer draws the distribution of those fold changes
# as a histogram (count on Y) with optional x=0, median, and mean
# reference lines. No statistical testing (no p-values, no padj).
#
# Shares the per-feature FC math with fc_rankplot; the only difference is
# the renderer (distribution vs ranked scatter). The `rank` column is not
# produced here. Median / mean reference values are computed at render
# time so flip_fc / min_replicates style changes stay live.
#
# Contract:
#  - data$comparisons: named list keyed by "B_vs_A"; each entry:
#    - points: primary_id, display_id, protein_id, gene_id,
#              log2fc, mean_a, mean_b, n_a, n_b
#    - group_a, group_b: scalars
#    - comparison: list(group_a, group_b, label, n_total)
#  - data$comparison_names: vector for viewer dropdown
#  - data$fc_vectors: named list of fc vectors keyed by comp name
#  - data$id_cols: list(primary, display, protein, gene)
#  - data$log: data.frame of log entries
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Compute log2 fold-change for a single pairwise group comparison
.fc_histogram_pairwise <- function(mat, samples, grp_a, grp_b,
                                   primary_ids, display_ids,
                                   fc_transform, is_log_transformed, add_log) {
  add_log("INFO", sprintf("--- FC Histogram: %s vs %s ---", grp_a, grp_b))

  cols_a <- samples$sample_col[samples$group_name == grp_a]
  cols_b <- samples$sample_col[samples$group_name == grp_b]
  cols_a <- intersect(cols_a, colnames(mat))
  cols_b <- intersect(cols_b, colnames(mat))

  add_log("INFO", sprintf("  Samples: %d in %s, %d in %s",
                          length(cols_a), grp_a, length(cols_b), grp_b))

  if (length(cols_a) == 0 || length(cols_b) == 0) {
    add_log("WARN", sprintf("  Skipping %s vs %s: no valid columns", grp_a, grp_b))
    return(NULL)
  }

  mat_a <- mat[, cols_a, drop = FALSE]
  mat_b <- mat[, cols_b, drop = FALSE]

  mean_a <- rowMeans(mat_a, na.rm = TRUE)
  mean_b <- rowMeans(mat_b, na.rm = TRUE)

  n_a <- if (length(cols_a) == 1) as.integer(!is.na(mat_a[, 1])) else rowSums(!is.na(mat_a))
  n_b <- if (length(cols_b) == 1) as.integer(!is.na(mat_b[, 1])) else rowSums(!is.na(mat_b))

  # Fold-change (B vs A, positive = higher in B)
  if (isTRUE(is_log_transformed)) {
    log2fc <- mean_b - mean_a
  } else if (identical(fc_transform, "log2")) {
    log2fc <- log2(mean_b + 1) - log2(mean_a + 1)
  } else if (identical(fc_transform, "log10")) {
    log2fc <- log2(10) * (log10(mean_b + 1) - log10(mean_a + 1))
  } else {
    # "none" — treat as direct ratio on natural scale (log2 of ratio)
    log2fc <- log2((mean_b + 1) / (mean_a + 1))
  }

  results <- data.frame(
    primary_id = primary_ids,
    display_id = display_ids,
    protein_id = primary_ids,
    gene_id = display_ids,
    log2fc = as.numeric(log2fc),
    mean_a = as.numeric(mean_a),
    mean_b = as.numeric(mean_b),
    n_a = as.integer(n_a),
    n_b = as.integer(n_b),
    stringsAsFactors = FALSE
  )

  # Filter rows with finite FC
  valid_idx <- is.finite(results$log2fc)
  n_excluded <- sum(!valid_idx)
  results <- results[valid_idx, , drop = FALSE]
  add_log("INFO", sprintf("  Valid features: %d (excluded %d)", nrow(results), n_excluded))

  fc_vector <- as.numeric(log2fc)
  names(fc_vector) <- primary_ids

  list(
    points = results,
    group_a = grp_a,
    group_b = grp_b,
    fc_vector = fc_vector,
    comparison = list(
      group_a = grp_a,
      group_b = grp_b,
      label = paste0(grp_b, "_vs_", grp_a),
      n_total = nrow(results)
    )
  )
}

#' Execute fc_histogram engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters (fc_transform, is_log_transformed, control_only)
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_fc_histogram_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()
  params <- params %||% payload$params %||% list()

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries[[length(log_entries) + 1]] <<- list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level, message = msg
    )
  }

  build_log_df <- function() {
    if (length(log_entries) == 0) {
      return(data.frame(time = character(), level = character(),
                        message = character(), stringsAsFactors = FALSE))
    }
    do.call(rbind, lapply(log_entries, function(e) data.frame(
      time = e$time, level = e$level, message = e$message,
      stringsAsFactors = FALSE
    )))
  }

  if (!isTRUE(payload$ok)) {
    return(list(
      engine_id = "fc_histogram",
      params = params,
      data = list(
        comparisons = list(),
        comparison_names = character(),
        fc_vectors = list(),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  mat <- payload$mat
  samples <- payload$samples
  groups <- payload$groups
  ids <- payload$ids

  ctx <- payload$data_type_context %||% msterp_data_type_context("proteomics")

  add_log("INFO", sprintf("Initiating fc_histogram: %d features, %d groups",
                          nrow(mat %||% matrix(0, 0, 0)), length(groups %||% character())))

  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    add_log("ERROR", "Payload matrix is missing or empty")
    return(list(
      engine_id = "fc_histogram",
      params = params,
      data = list(comparisons = list(), comparison_names = character(),
                  fc_vectors = list(), log = build_log_df(),
                  error = "Payload matrix is missing or empty")
    ))
  }

  if (length(groups) < 2) {
    add_log("ERROR", "fc_histogram requires at least 2 groups")
    return(list(
      engine_id = "fc_histogram",
      params = params,
      data = list(comparisons = list(), comparison_names = character(),
                  fc_vectors = list(), log = build_log_df(),
                  error = "fc_histogram requires at least 2 groups")
    ))
  }

  fc_transform <- params$fc_transform %||% "log2"
  if (!fc_transform %in% c("none", "log2", "log10")) fc_transform <- "log2"
  is_log_transformed <- isTRUE(params$is_log_transformed)
  control_only <- isTRUE(params$control_only)

  # Resolve ID columns (context-aware)
  primary_col <- as.character(payload$metadata$id_primary_col %||%
                              payload$metadata$id_protein_col %||% "")[1]
  display_col <- ctx_resolve_id_col(ctx, payload$metadata, names(ids)) %||%
                 as.character(payload$metadata$id_gene_col %||% "")[1]

  primary_ids <- if (nzchar(primary_col) && primary_col %in% names(ids)) {
    as.character(ids[[primary_col]])
  } else {
    rownames(mat) %||% paste0("row_", seq_len(nrow(mat)))
  }

  display_ids <- if (nzchar(display_col) && display_col %in% names(ids)) {
    as.character(ids[[display_col]])
  } else {
    rep(NA_character_, nrow(mat))
  }

  # Control detection (matches volcano's pattern)
  meta_groups <- payload$metadata$groups
  is_control_raw <- if (!is.null(meta_groups$is_control)) meta_groups$is_control else NULL
  is_control <- if (is.character(is_control_raw)) {
    tolower(as.character(is_control_raw)) %in% c("true", "t", "1", "yes")
  } else if (!is.null(is_control_raw)) {
    as.logical(is_control_raw)
  } else {
    rep(FALSE, nrow(meta_groups %||% data.frame()))
  }
  is_control[is.na(is_control)] <- FALSE
  group_names_meta <- as.character(meta_groups$group_name %||% character(0))

  is_control_map <- rep(FALSE, length(groups))
  for (idx in seq_along(groups)) {
    match_idx <- match(groups[idx], group_names_meta)
    if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
  }
  control_idx <- which(is_control_map)
  has_control <- length(control_idx) == 1

  if (control_only && has_control) {
    add_log("INFO", sprintf("Control-only mode: comparisons against control (%s)",
                            groups[control_idx]))
  }

  n_groups <- length(groups)
  comparisons <- list()
  fc_vectors <- list()

  for (i in seq_len(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      if (control_only && has_control) {
        if (!i %in% control_idx && !j %in% control_idx) next
      }

      grp_a <- groups[i]
      grp_b <- groups[j]

      # Control orientation: keep control as denominator (grp_a)
      if (has_control && j %in% control_idx && !(i %in% control_idx)) {
        tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
      }

      result <- .fc_histogram_pairwise(
        mat = mat, samples = samples,
        grp_a = grp_a, grp_b = grp_b,
        primary_ids = primary_ids, display_ids = display_ids,
        fc_transform = fc_transform,
        is_log_transformed = is_log_transformed,
        add_log = add_log
      )

      if (!is.null(result)) {
        comp_key <- paste0(grp_b, "_vs_", grp_a)
        comparisons[[comp_key]] <- result
        fc_vectors[[comp_key]] <- result$fc_vector
      }
    }
  }

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("fc_histogram completed in %.2f seconds (%d comparisons)",
                          engine_duration, length(comparisons)))

  list(
    engine_id = "fc_histogram",
    params = params,
    data = list(
      comparisons = comparisons,
      comparison_names = names(comparisons),
      fc_vectors = fc_vectors,
      id_cols = list(primary = primary_col, display = display_col,
                     protein = primary_col, gene = display_col),
      log = build_log_df()
    )
  )
}
