# =========================================================
# R/engines/stats/multi_cross_correlation.R - Cross-Entity Correlation Engine
#
# For mixed-entity multi-dataset analysis (protein-metabolomics):
# Computes per-entity correlations between proteins and metabolites
# across paired samples. Each protein gets its best-correlated
# metabolites, and vice versa.
#
# Contract v1.0:
#  - data$cross_correlations: data.frame with feature_a, feature_b,
#      display_a, display_b, correlation, p_value, method
#  - data$per_entity_a: data.frame with per-protein summary (max abs cor)
#  - data$per_entity_b: data.frame with per-metabolite summary (max abs cor)
#  - data$statistics: summary data.frame
#  - data$metadata: payload metadata
#  - data$log: execution log data.frame
# =========================================================

# Note: %||% operator is defined in R/utils/uniprot.R and sourced before this file

#' Execute multi_cross_correlation engine
#'
#' Computes pairwise correlations between entities from two different
#' omics layers (e.g., proteins vs metabolites) across paired samples.
#'
#' @param payload Mixed-entity multi-dataset payload
#' @param params Engine parameters (method, top_n, p_cutoff)
#' @param context Execution context
#' @return Contract-compliant results
stats_multi_cross_correlation_run <- function(payload, params = NULL, context = NULL) {
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

  # Build empty result helper
  empty_result <- function(error_msg = NULL) {
    log_df <- .xcor_build_log_df(log_entries)
    if (!is.null(error_msg)) {
      add_log("ERROR", error_msg)
      log_df <- .xcor_build_log_df(log_entries)
    }
    list(
      engine_id = "multi_cross_correlation",
      params = params,
      data = list(
        cross_correlations = data.frame(
          feature_a = character(0), feature_b = character(0),
          display_a = character(0), display_b = character(0),
          correlation = numeric(0), p_value = numeric(0),
          method = character(0), stringsAsFactors = FALSE
        ),
        per_entity_a = data.frame(
          feature_id = character(0), display_id = character(0),
          max_abs_cor = numeric(0), best_partner = character(0),
          best_partner_display = character(0), best_cor = numeric(0),
          best_p = numeric(0), stringsAsFactors = FALSE
        ),
        per_entity_b = data.frame(
          feature_id = character(0), display_id = character(0),
          max_abs_cor = numeric(0), best_partner = character(0),
          best_partner_display = character(0), best_cor = numeric(0),
          best_p = numeric(0), stringsAsFactors = FALSE
        ),
        statistics = data.frame(metric = character(0), value = character(0), stringsAsFactors = FALSE),
        metadata = list(),
        log = log_df
      )
    )
  }

  add_log("INFO", "Starting cross-entity correlation analysis")

  # Validate mixed-entity mode
  if (!identical(payload$multi_subtype, "protein-metabolomics")) {
    return(empty_result("multi_cross_correlation requires protein-metabolomics payload"))
  }

  mat_a <- payload$mat_a_paired  # proteins x paired_labels
  mat_b <- payload$mat_b_paired  # metabolites x paired_labels

  if (is.null(mat_a) || is.null(mat_b)) {
    return(empty_result("Missing paired matrices in payload"))
  }

  n_a <- nrow(mat_a)
  n_b <- nrow(mat_b)
  n_samples <- ncol(mat_a)

  if (n_samples < 3) {
    return(empty_result("Need at least 3 paired samples for correlation"))
  }

  # Parameters
  method <- params$method %||% "spearman"
  top_n <- as.integer(params$top_n %||% 10)
  p_cutoff <- as.numeric(params$p_cutoff %||% 0.05)

  add_log("INFO", sprintf("Computing %s correlations: %d proteins x %d metabolites across %d paired samples",
                          method, n_a, n_b, n_samples))

  # Get display names
  ids_a <- payload$ids_a
  ids_b <- payload$ids_b

  display_map_a <- if (!is.null(ids_a) && "display_id" %in% names(ids_a)) {
    stats::setNames(as.character(ids_a$display_id), as.character(ids_a$primary_id))
  } else {
    stats::setNames(rownames(mat_a), rownames(mat_a))
  }

  display_map_b <- if (!is.null(ids_b) && "display_id" %in% names(ids_b)) {
    stats::setNames(as.character(ids_b$display_id), as.character(ids_b$primary_id))
  } else {
    stats::setNames(rownames(mat_b), rownames(mat_b))
  }

  # Compute all pairwise correlations
  # For large matrices, use vectorized approach
  cor_results <- list()

  for (i in seq_len(n_a)) {
    feat_a <- rownames(mat_a)[i]
    vec_a <- as.numeric(mat_a[i, ])

    # Skip if too many NAs
    if (sum(!is.na(vec_a)) < 3) next

    for (j in seq_len(n_b)) {
      feat_b <- rownames(mat_b)[j]
      vec_b <- as.numeric(mat_b[j, ])

      # Skip if too many NAs
      valid <- !is.na(vec_a) & !is.na(vec_b)
      if (sum(valid) < 3) next

      ct <- tryCatch(
        cor.test(vec_a[valid], vec_b[valid], method = method),
        error = function(e) NULL
      )

      if (!is.null(ct)) {
        cor_results[[length(cor_results) + 1L]] <- list(
          feature_a = feat_a,
          feature_b = feat_b,
          display_a = display_map_a[feat_a] %||% feat_a,
          display_b = display_map_b[feat_b] %||% feat_b,
          correlation = as.numeric(ct$estimate),
          p_value = ct$p.value
        )
      }
    }
  }

  if (length(cor_results) == 0) {
    return(empty_result("No valid correlations computed"))
  }

  # Build cross-correlations data.frame
  cross_cor_df <- do.call(rbind, lapply(cor_results, function(r) {
    data.frame(
      feature_a = r$feature_a,
      feature_b = r$feature_b,
      display_a = r$display_a,
      display_b = r$display_b,
      correlation = r$correlation,
      p_value = r$p_value,
      method = method,
      stringsAsFactors = FALSE
    )
  }))

  add_log("INFO", sprintf("Computed %d pairwise correlations", nrow(cross_cor_df)))

  # Compute per-entity summaries
  # For each protein: find its best-correlated metabolite
  per_entity_a <- .xcor_per_entity_summary(cross_cor_df, "feature_a", "feature_b",
                                           "display_a", "display_b",
                                           display_map_a, rownames(mat_a))

  # For each metabolite: find its best-correlated protein
  per_entity_b <- .xcor_per_entity_summary(cross_cor_df, "feature_b", "feature_a",
                                           "display_b", "display_a",
                                           display_map_b, rownames(mat_b))

  # Filter cross_cor_df to significant results for output
  sig_cors <- cross_cor_df[cross_cor_df$p_value <= p_cutoff, , drop = FALSE]

  # Sort by absolute correlation and keep top N per entity
  sig_cors <- sig_cors[order(abs(sig_cors$correlation), decreasing = TRUE), , drop = FALSE]

  # Statistics summary
  n_sig <- nrow(sig_cors)
  n_total <- nrow(cross_cor_df)
  mean_abs_cor <- mean(abs(cross_cor_df$correlation), na.rm = TRUE)

  stats_summary <- data.frame(
    metric = c("Total pairs tested", "Significant pairs", "Mean |correlation|",
               "Method", "P-value cutoff", "N paired samples",
               "N proteins", "N metabolites"),
    value = c(
      as.character(n_total),
      as.character(n_sig),
      sprintf("%.4f", mean_abs_cor),
      method,
      sprintf("%.4f", p_cutoff),
      as.character(n_samples),
      as.character(n_a),
      as.character(n_b)
    ),
    stringsAsFactors = FALSE
  )

  add_log("INFO", sprintf("Significant correlations: %d / %d (p <= %.4f)", n_sig, n_total, p_cutoff))

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Cross-correlation completed in %.2f seconds", engine_duration))

  log_df <- .xcor_build_log_df(log_entries)

  list(
    engine_id = "multi_cross_correlation",
    params = params,
    data = list(
      cross_correlations = sig_cors,
      all_correlations = cross_cor_df,
      per_entity_a = per_entity_a,
      per_entity_b = per_entity_b,
      statistics = stats_summary,
      metadata = payload$metadata,
      log = log_df
    )
  )
}


# =========================================================
# Internal Helper Functions
# =========================================================

#' Build per-entity summary: for each entity, find best-correlated partner
#' @noRd
.xcor_per_entity_summary <- function(cor_df, entity_col, partner_col,
                                     entity_display_col, partner_display_col,
                                     display_map, all_ids) {
  if (nrow(cor_df) == 0) {
    return(data.frame(
      feature_id = character(0), display_id = character(0),
      max_abs_cor = numeric(0), best_partner = character(0),
      best_partner_display = character(0), best_cor = numeric(0),
      best_p = numeric(0), stringsAsFactors = FALSE
    ))
  }

  entities <- unique(cor_df[[entity_col]])

  summaries <- lapply(entities, function(eid) {
    sub <- cor_df[cor_df[[entity_col]] == eid, , drop = FALSE]
    best_idx <- which.max(abs(sub$correlation))
    data.frame(
      feature_id = eid,
      display_id = display_map[eid] %||% eid,
      max_abs_cor = abs(sub$correlation[best_idx]),
      best_partner = sub[[partner_col]][best_idx],
      best_partner_display = sub[[partner_display_col]][best_idx],
      best_cor = sub$correlation[best_idx],
      best_p = sub$p_value[best_idx],
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, summaries)
  result <- result[order(result$max_abs_cor, decreasing = TRUE), , drop = FALSE]
  rownames(result) <- NULL
  result
}


#' Build log data frame from log entries
#' @noRd
.xcor_build_log_df <- function(log_entries) {
  if (length(log_entries) == 0) {
    return(data.frame(
      time = character(0),
      level = character(0),
      message = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))
}


#' Get multi_cross_correlation engine info
multi_cross_correlation_engine_info <- function() {
  list(
    engine_id = "multi_cross_correlation",
    label = "Cross-Entity Correlation",
    description = "Per-entity correlation between proteins and metabolites across paired samples"
  )
}
