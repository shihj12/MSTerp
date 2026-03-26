# =========================================================
# R/engines/stats/ftest_heatmap.R - F-test Heatmap Engine
#
# Computes multi-group F-tests (Welch ANOVA, Kruskal-Wallis,
# or limma) to identify significant features, then generates a
# heatmap of the top significant features.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Compute Welch's One-Way ANOVA p-values for each row
#' @param mat Numeric matrix (rows = features, cols = samples)
#' @param group_vec Character vector of group assignments (same order as columns)
#' @return Numeric vector of p-values (same length as nrow(mat))
compute_welch_anova <- function(mat, group_vec) {
  n_rows <- nrow(mat)
  groups <- factor(group_vec)

  # Use vapply for guaranteed output length
  pvals <- vapply(seq_len(n_rows), function(i) {
    row <- mat[i, ]
    df <- data.frame(value = row, group = groups)
    df <- df[!is.na(df$value), ]

    # Keep only groups with at least 2 observations (needed for variance estimation)
    group_counts <- table(df$group)
    valid_groups <- names(group_counts[group_counts >= 2])
    if (length(valid_groups) < 2) return(NA_real_)

    # Subset to valid groups and drop unused factor levels
    df <- df[df$group %in% valid_groups, , drop = FALSE]
    df$group <- factor(df$group)

    tryCatch({
      res <- stats::oneway.test(value ~ group, data = df, var.equal = FALSE)
      res$p.value
    }, error = function(e) NA_real_)
  }, FUN.VALUE = numeric(1))

  pvals
}

#' Compute Kruskal-Wallis p-values for each row
#' @param mat Numeric matrix (rows = features, cols = samples)
#' @param group_vec Character vector of group assignments
#' @return Numeric vector of p-values (same length as nrow(mat))
compute_kruskal_wallis <- function(mat, group_vec) {
  n_rows <- nrow(mat)
  groups <- factor(group_vec)

  # Use vapply for guaranteed output length
  pvals <- vapply(seq_len(n_rows), function(i) {
    row <- mat[i, ]
    df <- data.frame(value = row, group = groups)
    df <- df[!is.na(df$value), ]

    # Need at least 2 groups with data
    groups_with_data <- unique(df$group)
    if (length(groups_with_data) < 2) return(NA_real_)

    tryCatch({
      res <- stats::kruskal.test(value ~ group, data = df)
      res$p.value
    }, error = function(e) NA_real_)
  }, FUN.VALUE = numeric(1))

  pvals
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

  # Design matrix: one column per group (means parameterization)
  design <- stats::model.matrix(~0 + groups)
  colnames(design) <- levels(groups)

  # limma requires complete cases - handle NAs by running row-by-row or imputing
  # For now, we'll run limma and handle the output carefully
  tryCatch({
    # Fit linear model
    fit <- limma::lmFit(mat, design)

    # Build pairwise contrasts for F-test
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

    # Return moderated F-test p-values
    # For single contrast (2 groups), F.p.value may not exist - use p.value instead
    pvals <- if (!is.null(fit2$F.p.value) && length(fit2$F.p.value) == n_rows) {
      fit2$F.p.value
    } else if (!is.null(fit2$p.value)) {
      # Single contrast case - p.value is a matrix, extract first column
      as.numeric(fit2$p.value[, 1])
    } else {
      rep(NA_real_, n_rows)
    }

    # Ensure correct length
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
  # Ensure labels are set from row names
  if (is.null(hc$labels) && !is.null(rownames(m_sub))) {
    hc$labels <- rownames(m_sub)
  }
  hc
}

#' Compute abundance dendrogram using Euclidean distance
#' @param m Numeric matrix
#' @return hclust object or NULL
compute_abund_dendro <- function(m) {
  if (nrow(m) < 2) return(NULL)

  clust_rows <- which(rowSums(is.na(m)) == 0)
  if (length(clust_rows) < 2) return(NULL)

  m_sub <- m[clust_rows, , drop = FALSE]
  d <- stats::dist(m_sub)
  hc <- stats::hclust(d, method = "ward.D2")
  # Ensure labels are set from row names
  if (is.null(hc$labels) && !is.null(rownames(m_sub))) {
    hc$labels <- rownames(m_sub)
  }
  hc
}

#' Execute F-test Heatmap Engine
#'
#' Performs multi-group statistical testing to identify significant genes,
#' then generates heatmap data for visualization.
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_ftest_heatmap_run <- function(payload, params = NULL, context = NULL) {
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
    stop("F-test Heatmap: payload matrix is missing or empty")
  }
  if (is.null(samples) || !is.data.frame(samples) || nrow(samples) == 0) {
    stop("F-test Heatmap: payload samples are missing or empty")
  }

  # Get display name column (context-aware: metabolite name for metabolomics, gene for proteomics)
  id_display_col <- ctx_resolve_id_col(ctx, payload$metadata, names(ids %||% data.frame()))
  id_gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
  display_names_lookup <- NULL
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

  log_transform <- as.character(params$log_transform %||% "log10")[1]
  if (!log_transform %in% c("log10", "none")) {
    log_transform <- "log10"
  }

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
  top_n <- min(top_n, 300)  # Cap at 300

  cluster_rows <- isTRUE(params$cluster_rows %||% TRUE)

  # =========================================================
  # 3. BUILD GROUP MAPPING
  # =========================================================
  sample_col <- as.character(payload$metadata$sample_col %||% "sample_col")[1]
  if (!nzchar(sample_col) || !sample_col %in% names(samples)) {
    sample_col <- "sample_col"
  }
  if (!sample_col %in% names(samples)) {
    stop("F-test Heatmap: payload$samples must include `sample_col`")
  }
  if (!"group_name" %in% names(samples)) {
    stop("F-test Heatmap: payload$samples must include `group_name`")
  }

  # Order samples by group then replicate
  rep_num <- suppressWarnings(as.numeric(samples$replicate %||% seq_len(nrow(samples))))
  if (all(!is.finite(rep_num))) rep_num <- seq_len(nrow(samples))
  ord <- order(as.character(samples$group_name), rep_num, na.last = TRUE)
  sample_order <- as.character(samples[[sample_col]][ord])
  sample_order <- sample_order[sample_order %in% colnames(mat)]

  if (length(sample_order) == 0) {
    stop("F-test Heatmap: no sample columns matched between payload$mat and payload$samples")
  }

  # Subset and reorder matrix columns
  mat <- mat[, sample_order, drop = FALSE]

  # Build group vector
  group_vec <- as.character(samples$group_name[match(sample_order, samples[[sample_col]])])
  groups <- unique(group_vec)

  if (length(groups) < 2) {
    stop("F-test Heatmap requires at least 2 groups")
  }

  # =========================================================
  # 4. MISSINGNESS FILTER
  # =========================================================
  n_total <- ncol(mat)
  n_valid <- rowSums(!is.na(mat) & is.finite(mat) & mat > 0)
  valid_frac <- n_valid / n_total
  min_valid_frac <- 1 - (max_missingness / 100)
  keep_rows <- valid_frac >= min_valid_frac

  if (sum(keep_rows) == 0) {
    stop(sprintf("F-test Heatmap: no rows passed missingness filter (max_missingness=%d%%)", max_missingness))
  }

  mat_filtered <- mat[keep_rows, , drop = FALSE]

  # Track which original row indices we kept (for display name lookup)
  kept_row_indices <- which(keep_rows)

  # Get display names for display - prefer names from ids table over rownames
  if (!is.null(display_names_lookup)) {
    original_display_names <- display_names_lookup[kept_row_indices]
  } else {
    original_display_names <- rownames(mat_filtered)
  }

  # Ensure we have display names (use row indices if NULL)
  if (is.null(original_display_names) || length(original_display_names) == 0) {
    original_display_names <- paste0("Row_", kept_row_indices)
  }

  # Also keep track of original rownames for internal indexing
  original_rownames <- rownames(mat_filtered)
  if (is.null(original_rownames) || length(original_rownames) == 0) {
    original_rownames <- paste0("Row_", kept_row_indices)
    rownames(mat_filtered) <- original_rownames
  }

  # =========================================================

  # 5. LOG TRANSFORM
  # =========================================================
  mat_log <- mat_filtered
  if (identical(log_transform, "log10")) {
    mat_log[!is.finite(mat_log) | mat_log <= 0] <- NA_real_
    mat_log <- log10(mat_log)
  } else {
    mat_log[!is.finite(mat_log)] <- NA_real_
  }

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

  # Validate pvals length matches matrix rows

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
    # No significant features - return empty result with message
    return(list(
      engine_id = "ftest_heatmap",
      params = params,
      data = list(
        mat_log = matrix(nrow = 0, ncol = ncol(mat_log)),
        mat_zscore = matrix(nrow = 0, ncol = ncol(mat_log)),
        dendro_zscore = NULL,
        dendro_abundance = NULL,
        target_order = character(0),
        gene_order = character(0),
        sample_order = sample_order,
        group_annotations = data.frame(
          sample = sample_order,
          group = group_vec,
          stringsAsFactors = FALSE
        ),
        group_colors = character(0),
        stats_table = data.frame(
          gene = character(0),
          feature = character(0),
          pval = numeric(0),
          padj = numeric(0),
          sig_label = character(0),
          stringsAsFactors = FALSE
        ),
        n_tested = nrow(mat_filtered),
        n_significant = 0,
        message = sprintf("No significant %s found (FDR < %.3f)", ctx$entity_plural, padj_threshold),
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
  # 10. SUBSET MATRIX
  # =========================================================
  mat_sig <- mat_log[sig_idx, , drop = FALSE]
  display_names <- original_display_names[sig_idx]
  rownames(mat_sig) <- display_names

  # =========================================================
  # 11. Z-SCORE COMPUTATION
  # =========================================================
  mat_zscore <- t(apply(mat_sig, 1, scale_row))
  dimnames(mat_zscore) <- dimnames(mat_sig)

  # =========================================================
  # 12. CLUSTERING
  # =========================================================
  dendro_zscore <- NULL
  dendro_abundance <- NULL

  if (cluster_rows && nrow(mat_sig) >= 2) {
    dendro_zscore <- compute_z_dendro(mat_zscore)
    dendro_abundance <- compute_abund_dendro(mat_sig)
  }

  # =========================================================
  # 13. GROUP ANNOTATIONS & COLORS
  # =========================================================
  group_annotations <- data.frame(
    sample = sample_order,
    group = group_vec,
    stringsAsFactors = FALSE
  )

  # Try to use colors from formatted Excel file
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

  # =========================================================
  # 14. BUILD STATS TABLE
  # =========================================================
  stats_table <- data.frame(
    gene = display_names,
    feature = display_names,
    pval = pvals[sig_idx],
    padj = padj[sig_idx],
    sig_label = p_to_stars(padj[sig_idx]),
    stringsAsFactors = FALSE
  )

  # Sort stats_table by padj
  stats_table <- stats_table[order(stats_table$padj), ]

  # =========================================================
  # 15. RETURN RESULT
  # =========================================================
  list(
    engine_id = "ftest_heatmap",
    params = params,
    data = list(
      mat_log = mat_sig,
      mat_zscore = mat_zscore,
      dendro_zscore = dendro_zscore,
      dendro_abundance = dendro_abundance,
      target_order = display_names,
      gene_order = display_names,
      sample_order = sample_order,
      group_annotations = group_annotations,
      group_colors = group_colors,
      stats_table = stats_table,
      n_tested = nrow(mat_filtered),
      n_significant = length(sig_idx),
      ids = ids,
      id_cols = list(
        protein = as.character(payload$metadata$id_protein_col %||% "protein_id"),
        gene = id_display_col %||% id_gene_col
      )
    )
  )
}
