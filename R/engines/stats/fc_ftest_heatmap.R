# =========================================================
# R/engines/stats/fc_ftest_heatmap.R — FC F-test Heatmap Engine
#
# Performs multi-group statistical testing to identify significant
# features, then generates fold change heatmap for the top significant features.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Compute Welch's One-Way ANOVA p-values for each row
#' @param mat Numeric matrix (rows = features, cols = samples)
#' @param group_vec Character vector of group assignments (same order as columns)
#' @return Numeric vector of p-values (same length as nrow(mat))
compute_welch_anova <- function(mat, group_vec) {
  n_rows <- nrow(mat)
  groups <- factor(group_vec)

  vapply(seq_len(n_rows), function(i) {
    row <- mat[i, ]
    df <- data.frame(value = row, group = groups)
    df <- df[!is.na(df$value), ]

    groups_with_data <- unique(df$group[!is.na(df$value)])
    if (length(groups_with_data) < 2) return(NA_real_)

    group_counts <- table(df$group)
    if (any(group_counts[group_counts > 0] < 2)) return(NA_real_)

    tryCatch({
      res <- stats::oneway.test(value ~ group, data = df, var.equal = FALSE)
      res$p.value
    }, error = function(e) NA_real_)
  }, FUN.VALUE = numeric(1))
}

#' Compute Kruskal-Wallis p-values for each row
#' @param mat Numeric matrix (rows = features, cols = samples)
#' @param group_vec Character vector of group assignments
#' @return Numeric vector of p-values (same length as nrow(mat))
compute_kruskal_wallis <- function(mat, group_vec) {
  n_rows <- nrow(mat)
  groups <- factor(group_vec)

  vapply(seq_len(n_rows), function(i) {
    row <- mat[i, ]
    df <- data.frame(value = row, group = groups)
    df <- df[!is.na(df$value), ]

    groups_with_data <- unique(df$group)
    if (length(groups_with_data) < 2) return(NA_real_)

    tryCatch({
      res <- stats::kruskal.test(value ~ group, data = df)
      res$p.value
    }, error = function(e) NA_real_)
  }, FUN.VALUE = numeric(1))
}

#' Compute limma moderated F-test p-values for each row
#' @param mat Numeric matrix (rows = features, cols = samples)
#' @param group_vec Character vector of group assignments
#' @return Numeric vector of p-values
compute_limma_ftest <- function(mat, group_vec) {
  if (!requireNamespace("limma", quietly = TRUE)) {
    stop("Package 'limma' required for limma F-test. Install with:\n",
         "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')\n",
         "BiocManager::install('limma')")
  }

  n_rows <- nrow(mat)
  groups <- factor(group_vec)

  design <- stats::model.matrix(~0 + groups)
  colnames(design) <- levels(groups)

  tryCatch({
    fit <- limma::lmFit(mat, design)

    n_groups <- length(levels(groups))
    group_names <- levels(groups)

    contrast_strings <- character()
    for (i in seq_len(n_groups - 1)) {
      for (j in (i + 1):n_groups) {
        contrast_strings <- c(contrast_strings,
                              sprintf("%s - %s", group_names[j], group_names[i]))
      }
    }

    contrast_matrix <- limma::makeContrasts(
      contrasts = contrast_strings,
      levels = design
    )

    fit2 <- limma::contrasts.fit(fit, contrast_matrix)
    fit2 <- limma::eBayes(fit2)

    pvals <- if (!is.null(fit2$F.p.value) && length(fit2$F.p.value) == n_rows) {
      fit2$F.p.value
    } else if (!is.null(fit2$p.value)) {
      as.numeric(fit2$p.value[, 1])
    } else {
      rep(NA_real_, n_rows)
    }

    if (length(pvals) != n_rows) {
      warning(sprintf("limma returned %d p-values for %d rows, padding with NA", length(pvals), n_rows))
      pvals <- rep(NA_real_, n_rows)
    }

    return(pvals)

  }, error = function(e) {
    warning(sprintf("limma F-test failed: %s", e$message))
    return(rep(NA_real_, n_rows))
  })
}

#' Convert p-values to significance stars
#' @param p Numeric vector of p-values
#' @return Character vector of significance labels
p_to_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", ""))))
}

#' Row-wise z-score scaling
#' @param x Numeric vector
#' @return Scaled numeric vector
scale_row <- function(x) {
  if (all(is.na(x))) return(x)
  mu <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) {
    y <- x
    y[!is.na(y)] <- 0
    return(y)
  }
  (x - mu) / s
}

#' Detect control group from metadata
#' @param meta_groups Data frame with group_name and is_control columns
#' @return Control group name or NULL
detect_control_group <- function(meta_groups) {
  if (is.null(meta_groups) || !is.data.frame(meta_groups)) return(NULL)
  if (!"is_control" %in% names(meta_groups)) return(NULL)
  if (!"group_name" %in% names(meta_groups)) return(NULL)

  is_ctrl <- tolower(as.character(meta_groups$is_control)) %in% c("true", "t", "1", "yes")
  ctrl_groups <- as.character(meta_groups$group_name[is_ctrl])

  if (length(ctrl_groups) == 1) return(ctrl_groups)
  return(NULL)
}

#' Compute fold change value
#' @param mean_a Mean intensity of group A (denominator/baseline)
#' @param mean_b Mean intensity of group B (numerator)
#' @param log_transform "log2", "log10", or "none"
#' @param is_log_transformed If TRUE, data is already log-transformed; use subtraction directly
#' @return Fold change value
compute_fc <- function(mean_a, mean_b, log_transform = "log2", is_log_transformed = FALSE) {
  if (is_log_transformed) {
    mean_b - mean_a
  } else if (log_transform == "log2") {
    log2(mean_b + 1) - log2(mean_a + 1)
  } else if (log_transform == "log10") {
    log10(mean_b + 1) - log10(mean_a + 1)
  } else {
    (mean_b + 1) / (mean_a + 1)
  }
}

#' Compute z-score dendrogram using correlation distance
#' @param m Numeric matrix
#' @return hclust object or NULL
compute_z_dendro <- function(m) {
  if (nrow(m) < 2) return(NULL)

  clust_rows <- which(rowSums(!is.na(m)) >= 2)
  if (length(clust_rows) < 2) return(NULL)

  m_sub <- m[clust_rows, , drop = FALSE]
  cmat <- suppressWarnings(stats::cor(t(m_sub), use = "pairwise.complete.obs"))
  cmat[is.na(cmat)] <- 0
  diag(cmat) <- 1

  d <- stats::as.dist(1 - cmat)
  hc <- stats::hclust(d, method = "average")
  if (is.null(hc$labels) && !is.null(rownames(m_sub))) {
    hc$labels <- rownames(m_sub)
  }
  hc
}

#' Compute FC dendrogram using Euclidean distance
#' @param m Numeric matrix (FC values)
#' @return hclust object or NULL
compute_fc_dendro <- function(m) {
  if (nrow(m) < 2) return(NULL)

  clust_rows <- which(rowSums(is.na(m)) == 0)
  if (length(clust_rows) < 2) return(NULL)

  m_sub <- m[clust_rows, , drop = FALSE]
  d <- stats::dist(m_sub)
  hc <- stats::hclust(d, method = "ward.D2")
  if (is.null(hc$labels) && !is.null(rownames(m_sub))) {
    hc$labels <- rownames(m_sub)
  }
  hc
}

#' Execute FC F-test Heatmap Engine
#'
#' Performs multi-group statistical testing to identify significant features,
#' then generates fold change heatmap data for visualization.
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_fc_ftest_heatmap_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()
  ctx <- payload$data_type_context %||% msterp_data_type_context("proteomics")

  # =========================================================
  # 1. VALIDATE PAYLOAD
  # =========================================================
  if (!isTRUE(payload$ok)) {
    stop(payload$error %||% "Invalid payload")
  }

  mat <- payload$mat
  samples <- payload$samples
  ids <- payload$ids

  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    stop("FC F-test Heatmap: payload matrix is missing or empty")
  }
  if (is.null(samples) || !is.data.frame(samples) || nrow(samples) == 0) {
    stop("FC F-test Heatmap: payload samples are missing or empty")
  }

  # Get display name column (gene name for proteomics, metabolite name for metabolomics)
  id_gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
  display_names_lookup <- NULL
  # Try context-aware ID column first
  id_display_col <- ctx_resolve_id_col(ctx, payload$metadata, names(ids %||% data.frame()))
  if (!is.null(ids) && is.data.frame(ids) && nrow(ids) == nrow(mat)) {
    if (!is.null(id_display_col) && id_display_col %in% names(ids)) {
      display_names_lookup <- as.character(ids[[id_display_col]])
    } else if (nzchar(id_gene_col) && id_gene_col %in% names(ids)) {
      display_names_lookup <- as.character(ids[[id_gene_col]])
    }
  }

  # =========================================================
  # 2. PARSE AND VALIDATE PARAMS
  # =========================================================
  max_missingness <- as.numeric(params$max_missingness %||% 60)
  if (!is.finite(max_missingness) || max_missingness < 0 || max_missingness > 100) {
    max_missingness <- 60
  }

  normalize <- isTRUE(params$normalize %||% FALSE)

  stat_test <- as.character(params$stat_test %||% "welch_anova")[1]
  if (!stat_test %in% c("welch_anova", "kruskal_wallis", "limma")) {
    stat_test <- "welch_anova"
  }

  padj_threshold <- as.numeric(params$padj_threshold %||% 0.05)
  if (!is.finite(padj_threshold) || padj_threshold <= 0 || padj_threshold > 1) {
    padj_threshold <- 0.05
  }

  top_n <- as.integer(params$top_n %||% 100)
  if (!is.finite(top_n) || top_n < 1) top_n <- 100
  top_n <- min(top_n, 300)

  cluster_rows <- isTRUE(params$cluster_rows %||% TRUE)

  log_transform <- as.character(params$log_transform %||% "log2")[1]
  if (!log_transform %in% c("log2", "log10", "none")) {
    log_transform <- "log2"
  }
  is_log_transformed <- isTRUE(params$is_log_transformed)

  control_only <- isTRUE(params$control_only %||% FALSE)

  # =========================================================
  # 3. BUILD GROUP MAPPING
  # =========================================================
  sample_col <- as.character(payload$metadata$sample_col %||% "sample_col")[1]
  if (!nzchar(sample_col) || !sample_col %in% names(samples)) {
    sample_col <- "sample_col"
  }
  if (!sample_col %in% names(samples)) {
    stop("FC F-test Heatmap: payload$samples must include `sample_col`")
  }
  if (!"group_name" %in% names(samples)) {
    stop("FC F-test Heatmap: payload$samples must include `group_name`")
  }

  # Order samples by group then replicate
  rep_num <- suppressWarnings(as.numeric(samples$replicate %||% seq_len(nrow(samples))))
  if (all(!is.finite(rep_num))) rep_num <- seq_len(nrow(samples))
  ord <- order(as.character(samples$group_name), rep_num, na.last = TRUE)
  sample_order <- as.character(samples[[sample_col]][ord])
  sample_order <- sample_order[sample_order %in% colnames(mat)]

  if (length(sample_order) == 0) {
    stop("FC F-test Heatmap: no sample columns matched between payload$mat and payload$samples")
  }

  # Subset and reorder matrix columns
  mat <- mat[, sample_order, drop = FALSE]

  # Build group vector
  group_vec <- as.character(samples$group_name[match(sample_order, samples[[sample_col]])])
  groups <- unique(group_vec)

  if (length(groups) < 2) {
    stop("FC F-test Heatmap requires at least 2 groups")
  }

  # Detect control group
  control_group <- detect_control_group(payload$metadata$groups)
  has_control <- !is.null(control_group)

  # =========================================================
  # 4. MISSINGNESS FILTER
  # =========================================================
  n_total <- ncol(mat)
  n_valid <- rowSums(!is.na(mat) & is.finite(mat) & mat > 0)
  valid_frac <- n_valid / n_total
  min_valid_frac <- 1 - (max_missingness / 100)
  keep_rows <- valid_frac >= min_valid_frac

  if (sum(keep_rows) == 0) {
    stop(sprintf("FC F-test Heatmap: no rows passed missingness filter (max_missingness=%d%%)", max_missingness))
  }

  mat_filtered <- mat[keep_rows, , drop = FALSE]
  kept_row_indices <- which(keep_rows)

  # Get display names for features
  if (!is.null(display_names_lookup)) {
    original_display_names <- display_names_lookup[kept_row_indices]
  } else {
    original_display_names <- rownames(mat_filtered)
  }

  if (is.null(original_display_names) || length(original_display_names) == 0) {
    original_display_names <- paste0("Row_", kept_row_indices)
  }

  original_rownames <- rownames(mat_filtered)
  if (is.null(original_rownames) || length(original_rownames) == 0) {
    original_rownames <- paste0("Row_", kept_row_indices)
    rownames(mat_filtered) <- original_rownames
  }

  # =========================================================
  # 5. LOG TRANSFORM FOR STATISTICAL TESTING
  # =========================================================
  mat_log <- mat_filtered
  mat_log[!is.finite(mat_log) | mat_log <= 0] <- NA_real_
  mat_log <- log10(mat_log)

  # =========================================================
  # 6. OPTIONAL NORMALIZATION (median centering)
  # =========================================================
  if (normalize) {
    col_medians <- apply(mat_log, 2, stats::median, na.rm = TRUE)
    global_median <- stats::median(col_medians, na.rm = TRUE)
    mat_log <- sweep(mat_log, 2, col_medians - global_median)
  }

  # =========================================================
  # 7. STATISTICAL TESTING
  # =========================================================
  n_rows_filtered <- nrow(mat_log)

  pvals <- switch(stat_test,
                  "welch_anova" = compute_welch_anova(mat_log, group_vec),
                  "kruskal_wallis" = compute_kruskal_wallis(mat_log, group_vec),
                  "limma" = compute_limma_ftest(mat_log, group_vec))

  if (is.null(pvals) || length(pvals) != n_rows_filtered) {
    warning(sprintf("Statistical test returned %d p-values for %d rows, using all NA",
                    length(pvals %||% 0), n_rows_filtered))
    pvals <- rep(NA_real_, n_rows_filtered)
  }

  # =========================================================
  # 8. FDR ADJUSTMENT
  # =========================================================
  padj <- stats::p.adjust(pvals, method = "BH")

  # =========================================================
  # 9. FILTER SIGNIFICANT + TOP N
  # =========================================================
  sig_idx <- which(padj < padj_threshold & !is.na(padj))

  if (length(sig_idx) == 0) {
    # No significant features - return empty result
    return(list(
      engine_id = "fc_ftest_heatmap",
      params = params,
      data = list(
        mat_fc = matrix(nrow = 0, ncol = 0),
        mat_zscore = matrix(nrow = 0, ncol = 0),
        dendro_zscore = NULL,
        dendro_fc = NULL,
        target_order = character(0),
        gene_order = character(0),     # backward compat alias
        comparison_order = character(0),
        stats_table = data.frame(
          feature = character(0),
          gene = character(0),
          pval = numeric(0),
          padj = numeric(0),
          sig_label = character(0),
          stringsAsFactors = FALSE
        ),
        n_tested = nrow(mat_filtered),
        n_significant = 0,
        message = sprintf("No significant %s found (FDR < %.3f)", ctx$entity_plural, padj_threshold),
        log_transform = log_transform,
        control_only = control_only,
        control_group = control_group,
        ids = ids,
        id_cols = list(
          protein = as.character(payload$metadata$id_protein_col %||% "protein_id"),
          gene = id_display_col %||% id_gene_col
        )
      )
    ))
  }

  # Sort by significance and take top N
  if (length(sig_idx) > top_n) {
    sig_idx <- sig_idx[order(padj[sig_idx])][seq_len(top_n)]
  }

  # =========================================================
  # 10. BUILD COMPARISON PAIRS FOR FC
  # =========================================================
  if (control_only && has_control) {
    non_control <- setdiff(groups, control_group)
    pairs <- lapply(non_control, function(g) list(a = control_group, b = g))
  } else {
    pair_combos <- utils::combn(groups, 2, simplify = FALSE)
    pairs <- lapply(pair_combos, function(x) {
      if (has_control) {
        if (x[1] == control_group) {
          list(a = x[1], b = x[2])
        } else if (x[2] == control_group) {
          list(a = x[2], b = x[1])
        } else {
          list(a = x[1], b = x[2])
        }
      } else {
        list(a = x[1], b = x[2])
      }
    })
  }

  comparison_names <- vapply(pairs, function(p) paste0(p$b, "/", p$a), character(1))

  # =========================================================
  # 11. COMPUTE FC MATRIX FOR SIGNIFICANT GENES
  # =========================================================
  # Use original (non-log-transformed) abundance for FC calculation
  mat_sig_abundance <- mat_filtered[sig_idx, , drop = FALSE]
  display_names <- original_display_names[sig_idx]
  rownames(mat_sig_abundance) <- display_names

  fc_mat <- matrix(NA_real_, nrow = length(sig_idx), ncol = length(pairs))
  rownames(fc_mat) <- display_names
  colnames(fc_mat) <- comparison_names

  for (i in seq_along(pairs)) {
    p <- pairs[[i]]
    cols_a <- sample_order[group_vec == p$a]
    cols_b <- sample_order[group_vec == p$b]
    cols_a <- intersect(cols_a, colnames(mat_sig_abundance))
    cols_b <- intersect(cols_b, colnames(mat_sig_abundance))

    if (length(cols_a) == 0 || length(cols_b) == 0) next

    if (!is_log_transformed && stat_test == "limma") {
      # Geometric mean for consistency with limma (log-transform first, then average)
      log_mean_a <- rowMeans(log2(mat_sig_abundance[, cols_a, drop = FALSE] + 1), na.rm = TRUE)
      log_mean_b <- rowMeans(log2(mat_sig_abundance[, cols_b, drop = FALSE] + 1), na.rm = TRUE)
      if (log_transform == "log10") {
        fc_mat[, i] <- (log_mean_b - log_mean_a) / log2(10)
      } else {
        fc_mat[, i] <- log_mean_b - log_mean_a
      }
    } else {
      mean_a <- rowMeans(mat_sig_abundance[, cols_a, drop = FALSE], na.rm = TRUE)
      mean_b <- rowMeans(mat_sig_abundance[, cols_b, drop = FALSE], na.rm = TRUE)
      fc_mat[, i] <- compute_fc(mean_a, mean_b, log_transform, is_log_transformed)
    }
  }

  # =========================================================
  # 12. Z-SCORE COMPUTATION
  # =========================================================
  mat_zscore <- t(apply(fc_mat, 1, scale_row))
  dimnames(mat_zscore) <- dimnames(fc_mat)

  # =========================================================
  # 13. CLUSTERING
  # =========================================================
  dendro_zscore <- NULL
  dendro_fc <- NULL

  if (cluster_rows && nrow(fc_mat) >= 2) {
    dendro_zscore <- compute_z_dendro(mat_zscore)
    dendro_fc <- compute_fc_dendro(fc_mat)
  }

  # =========================================================
  # 14. BUILD STATS TABLE
  # =========================================================
  stats_table <- data.frame(
    feature = display_names,
    gene = display_names,        # backward compat alias
    pval = pvals[sig_idx],
    padj = padj[sig_idx],
    sig_label = p_to_stars(padj[sig_idx]),
    stringsAsFactors = FALSE
  )

  stats_table <- stats_table[order(stats_table$padj), ]

  # =========================================================
  # 15. RETURN RESULT
  # =========================================================
  list(
    engine_id = "fc_ftest_heatmap",
    params = params,
    data = list(
      mat_fc = fc_mat,
      mat_zscore = mat_zscore,
      dendro_zscore = dendro_zscore,
      dendro_fc = dendro_fc,
      target_order = display_names,
      gene_order = display_names,      # backward compat alias
      comparison_order = comparison_names,
      stats_table = stats_table,
      n_tested = nrow(mat_filtered),
      n_significant = length(sig_idx),
      log_transform = log_transform,
      control_only = control_only,
      control_group = control_group,
      ids = ids,
      id_cols = list(
        protein = as.character(payload$metadata$id_protein_col %||% "protein_id"),
        gene = id_display_col %||% id_gene_col
      )
    )
  )
}
