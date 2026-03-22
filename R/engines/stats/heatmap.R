# =========================================================
# R/engines/stats/heatmap.R — Heatmap Engine
#
# Computes log-scale and z-score matrices for a user-provided
# target list, with optional row clustering and group annotations.
# =========================================================

stats_heatmap_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()
  ctx <- payload$data_type_context %||% msterp_data_type_context("proteomics")

  if (!isTRUE(payload$ok)) {
    stop(payload$error %||% "Invalid payload")
  }

  mat <- payload$mat
  samples <- payload$samples
  ids <- payload$ids

  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    stop("Heatmap: payload matrix is missing or empty")
  }
  if (is.null(samples) || !is.data.frame(samples) || nrow(samples) == 0) {
    stop("Heatmap: payload samples are missing or empty")
  }
  if (is.null(ids) || !is.data.frame(ids) || nrow(ids) == 0) {
    stop("Heatmap: payload ids are missing or empty")
  }

  parse_target_list <- function(x) {
    x <- as.character(x %||% "")[1]
    x <- trimws(x)
    if (!nzchar(x)) {
      stop(sprintf("Heatmap: %s is empty (paste one %s per line)", ctx$list_label, ctx$entity_label))
    }
    targets <- unlist(strsplit(x, "\\r?\\n", perl = TRUE), use.names = FALSE)
    targets <- trimws(targets)
    targets <- targets[nzchar(targets)]
    targets <- targets[!duplicated(targets)]
    if (length(targets) == 0) {
      stop(sprintf("Heatmap: %s is empty (paste one %s per line)", ctx$list_label, ctx$entity_label))
    }
    if (length(targets) > 2000) {
      stop(sprintf("Heatmap: %s too large (%d). Maximum is 2000 %s.", ctx$list_label, length(targets), ctx$entity_plural))
    }
    targets
  }

  # Support both new key (target_list) and legacy key (gene_list)
  target_list <- parse_target_list(params$target_list %||% params$gene_list)

  id_lookup_col <- ctx_resolve_id_col(ctx, payload$metadata, names(ids))
  # Fallback: try gene col then primary col directly
  if (is.null(id_lookup_col)) {
    id_gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
    id_primary_col <- as.character(payload$metadata$id_primary_col %||% "")[1]
    if (nzchar(id_gene_col) && id_gene_col %in% names(ids)) {
      id_lookup_col <- id_gene_col
    } else if (nzchar(id_primary_col) && id_primary_col %in% names(ids)) {
      id_lookup_col <- id_primary_col
    }
  }
  if (is.null(id_lookup_col)) {
    stop("Heatmap: cannot match targets (no suitable ID column found in payload$ids)")
  }

  id_ref <- as.character(ids[[id_lookup_col]])
  row_idx <- match(target_list, id_ref)

  matched_targets <- target_list[!is.na(row_idx)]
  unmatched_targets <- target_list[is.na(row_idx)]

  if (length(matched_targets) == 0) {
    stop(sprintf(
      "Heatmap: none of the provided %s matched (n=%d).",
      ctx$entity_plural, length(target_list)
    ))
  }

  mat_sub <- mat[row_idx[!is.na(row_idx)], , drop = FALSE]
  rownames(mat_sub) <- matched_targets

  sample_col <- as.character(payload$metadata$sample_col %||% "sample_col")[1]
  if (!nzchar(sample_col) || !sample_col %in% names(samples)) {
    sample_col <- "sample_col"
  }
  if (!sample_col %in% names(samples)) {
    stop("Heatmap: payload$samples must include `sample_col`")
  }
  if (!"group_name" %in% names(samples)) {
    stop("Heatmap: payload$samples must include `group_name`")
  }
  if (!"replicate" %in% names(samples)) {
    stop("Heatmap: payload$samples must include `replicate`")
  }

  rep_num <- suppressWarnings(as.numeric(samples$replicate))
  if (all(!is.finite(rep_num))) rep_num <- seq_len(nrow(samples))
  ord <- order(as.character(samples$group_name), rep_num, na.last = TRUE)
  sample_order <- as.character(samples[[sample_col]][ord])
  sample_order <- sample_order[sample_order %in% colnames(mat_sub)]
  if (length(sample_order) == 0) {
    stop("Heatmap: no sample columns matched between payload$mat and payload$samples")
  }
  mat_sub <- mat_sub[, sample_order, drop = FALSE]

  log_transform <- params$log_transform %||% "log10"
  if (!log_transform %in% c("log10", "none")) {
    log_transform <- "log10"
  }

  mat_log <- mat_sub
  if (identical(log_transform, "log10")) {
    mat_log[!is.finite(mat_log) | mat_log <= 0] <- NA_real_
    mat_log <- log10(mat_log)
  } else {
    mat_log[!is.finite(mat_log)] <- NA_real_
  }

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

  mat_zscore <- t(apply(mat_log, 1, scale_row))
  dimnames(mat_zscore) <- dimnames(mat_log)

  exclude_na_rows <- isTRUE(params$exclude_na_rows %||% FALSE)
  if (exclude_na_rows) {
    keep <- rowSums(is.na(mat_log)) == 0
    mat_log <- mat_log[keep, , drop = FALSE]
    mat_zscore <- mat_zscore[keep, , drop = FALSE]
    matched_targets <- rownames(mat_log)
  }

  # Legacy functions for backward compatibility
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

  # Unified clustering function with configurable options
  # Returns: list(dendro, clusters, cluster_order, method, clust_rows)
  compute_heatmap_clustering <- function(m, style, mode = "zscore") {
    cluster_method <- style$cluster_method %||% "hierarchical"
    k <- as.integer(style$cluster_k %||% 2)
    distance_method <- style$distance_method %||% "correlation"
    corr_type <- style$correlation_type %||% "spearman"
    linkage <- style$linkage_method %||% "average"

    if (nrow(m) < 2) return(list(dendro = NULL, clusters = NULL, method = cluster_method))

    # Filter rows suitable for clustering
    if (distance_method == "correlation") {
      # Correlation-based: need at least 2 non-NA values
      clust_rows <- which(rowSums(!is.na(m)) >= 2)
    } else {
      # Euclidean/Manhattan: need complete cases for reliable distances
      clust_rows <- which(rowSums(is.na(m)) == 0)
    }
    if (length(clust_rows) < 2) return(list(dendro = NULL, clusters = NULL, method = cluster_method))

    m_sub <- m[clust_rows, , drop = FALSE]

    # Compute distance matrix
    if (distance_method == "correlation") {
      cmat <- suppressWarnings(stats::cor(t(m_sub), use = "pairwise.complete.obs", method = corr_type))
      cmat[is.na(cmat)] <- 0
      diag(cmat) <- 1
      d <- stats::as.dist(1 - cmat)
    } else if (distance_method == "euclidean") {
      d <- stats::dist(m_sub, method = "euclidean")
    } else {
      d <- stats::dist(m_sub, method = "manhattan")
    }

    if (cluster_method == "hierarchical") {
      hc <- stats::hclust(d, method = linkage)
      if (is.null(hc$labels)) hc$labels <- rownames(m_sub)
      return(list(dendro = hc, clusters = NULL, method = "hierarchical", clust_rows = clust_rows))
    }

    # K-means / K-medians
    k <- min(k, nrow(m_sub))
    if (k < 2) k <- 2

    if (cluster_method == "kmeans") {
      set.seed(42)
      km <- stats::kmeans(m_sub, centers = k, nstart = 25)
      clusters <- km$cluster
    } else {
      # K-medians using cluster::pam
      if (requireNamespace("cluster", quietly = TRUE)) {
        set.seed(42)
        pam_result <- cluster::pam(m_sub, k = k)
        clusters <- pam_result$clustering
      } else {
        # Fallback to k-means if cluster package not available
        set.seed(42)
        km <- stats::kmeans(m_sub, centers = k, nstart = 25)
        clusters <- km$cluster
      }
    }

    # Order rows by cluster membership for display
    cluster_order <- order(clusters)
    names(clusters) <- rownames(m_sub)

    list(
      dendro = NULL,  # No dendrogram for k-means/k-medians
      clusters = clusters,
      cluster_order = cluster_order,
      method = cluster_method,
      clust_rows = clust_rows
    )
  }

  dendro_zscore <- compute_z_dendro(mat_zscore)
  dendro_abundance <- compute_abund_dendro(mat_log)

  group_vec <- samples$group_name[match(sample_order, samples[[sample_col]])]
  group_annotations <- data.frame(
    sample = sample_order,
    group = as.character(group_vec),
    stringsAsFactors = FALSE
  )

  groups <- unique(group_annotations$group)

  # Try to use colors from formatted Excel file (metadata$groups$color)
  # Fall back to auto-generated colors if not available
  meta_groups <- payload$metadata$groups
  if (!is.null(meta_groups) && is.data.frame(meta_groups) &&
      "group_name" %in% names(meta_groups) && "color" %in% names(meta_groups)) {
    # Use colors from formatted Excel
    color_map <- stats::setNames(
      as.character(meta_groups$color),
      as.character(meta_groups$group_name)
    )
    group_colors <- color_map[groups]
    # Fill in any missing colors with auto-generated ones
    missing <- is.na(group_colors) | !nzchar(group_colors)
    if (any(missing)) {
      auto_colors <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
      group_colors[missing] <- auto_colors
    }
    names(group_colors) <- groups
  } else {
    # Fall back to auto-generated colors
    group_colors <- grDevices::hcl.colors(max(1, length(groups)), palette = "Dark 3")
    group_colors <- group_colors[seq_along(groups)]
    names(group_colors) <- groups
  }

  # Context-aware display column (metabolite name for metabolomics, gene for proteomics)
  id_display_col <- ctx_resolve_id_col(ctx, payload$metadata, names(ids)) %||%
                    as.character(payload$metadata$id_gene_col %||% "")[1]

  list(
    engine_id = "heatmap",
    params = params,
    data = list(
      mat_log = mat_log,
      mat_zscore = mat_zscore,
      dendro_zscore = dendro_zscore,
      dendro_abundance = dendro_abundance,
      target_order = matched_targets,
      gene_order = matched_targets,       # backward compat alias
      sample_order = sample_order,
      group_annotations = group_annotations,
      group_colors = group_colors,
      matched_targets = matched_targets,
      matched_genes = matched_targets,     # backward compat alias
      unmatched_targets = unmatched_targets,
      unmatched_genes = unmatched_targets,  # backward compat alias
      ids = ids,
      id_cols = list(
        protein = as.character(payload$metadata$id_protein_col %||% "protein_id"),
        gene = id_display_col
      )
    )
  )
}
