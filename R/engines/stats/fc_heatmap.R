# =========================================================
# R/engines/stats/fc_heatmap.R — Fold Change Targeted Heatmap Engine
#
# Computes fold change matrix for a user-provided target list,
# with columns representing pairwise group comparisons.
# Supports control-only or all pairwise comparisons.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
    # Data already log-transformed: subtraction = log-scale ratio
    mean_b - mean_a
  } else if (log_transform == "log2") {
    log2(mean_b + 1) - log2(mean_a + 1)
  } else if (log_transform == "log10") {
    log10(mean_b + 1) - log10(mean_a + 1)
  } else {
    # Raw ratio
    (mean_b + 1) / (mean_a + 1)
  }
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

#' Execute FC Targeted Heatmap Engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_fc_heatmap_run <- function(payload, params = NULL, context = NULL) {
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
    stop("FC Heatmap: payload matrix is missing or empty")
  }
  if (is.null(samples) || !is.data.frame(samples) || nrow(samples) == 0) {
    stop("FC Heatmap: payload samples are missing or empty")
  }
  if (is.null(ids) || !is.data.frame(ids) || nrow(ids) == 0) {
    stop("FC Heatmap: payload ids are missing or empty")
  }

  # =========================================================
  # 2. PARSE TARGET LIST
  # =========================================================
  #' Parse target list from newline-separated string
  #' @param x Input string
  #' @return Character vector of unique target identifiers
  parse_target_list <- function(x) {
    x <- as.character(x %||% "")[1]
    x <- trimws(x)
    if (!nzchar(x)) {
      stop(sprintf("FC Heatmap: %s is empty (paste one %s per line)", ctx$list_label, ctx$entity_label))
    }
    targets <- unlist(strsplit(x, "\\r?\\n", perl = TRUE), use.names = FALSE)
    targets <- trimws(targets)
    targets <- targets[nzchar(targets)]
    targets <- targets[!duplicated(targets)]
    if (length(targets) == 0) {
      stop(sprintf("FC Heatmap: %s is empty (paste one %s per line)", ctx$list_label, ctx$entity_label))
    }
    if (length(targets) > 2000) {
      stop(sprintf("FC Heatmap: %s too large (%d). Maximum is 2000 %s.", ctx$list_label, length(targets), ctx$entity_plural))
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
    stop("FC Heatmap: cannot match targets (no suitable ID column found in payload$ids)")
  }

  id_ref <- as.character(ids[[id_lookup_col]])
  row_idx <- match(target_list, id_ref)

  matched_targets <- target_list[!is.na(row_idx)]
  unmatched_targets <- target_list[is.na(row_idx)]

  if (length(matched_targets) == 0) {
    stop(sprintf(
      "FC Heatmap: none of the provided %s matched (n=%d).",
      ctx$entity_plural, length(target_list)
    ))
  }

  mat_sub <- mat[row_idx[!is.na(row_idx)], , drop = FALSE]
  rownames(mat_sub) <- matched_targets

  # =========================================================
  # 3. GET PARAMETERS
  # =========================================================
  log_transform <- params$log_transform %||% "log2"
  if (!log_transform %in% c("log2", "log10", "none")) {
    log_transform <- "log2"
  }
  is_log_transformed <- isTRUE(params$is_log_transformed)

  control_only <- isTRUE(params$control_only %||% FALSE)
  cluster_rows <- isTRUE(params$cluster_rows %||% TRUE)

  # =========================================================
  # 4. BUILD GROUP MAPPING
  # =========================================================
  sample_col <- as.character(payload$metadata$sample_col %||% "sample_col")[1]
  if (!nzchar(sample_col) || !sample_col %in% names(samples)) {
    sample_col <- "sample_col"
  }
  if (!sample_col %in% names(samples)) {
    stop("FC Heatmap: payload$samples must include `sample_col`")
  }
  if (!"group_name" %in% names(samples)) {
    stop("FC Heatmap: payload$samples must include `group_name`")
  }

  groups <- unique(as.character(samples$group_name))
  if (length(groups) < 2) {
    stop("FC Heatmap requires at least 2 groups")
  }

  # Detect control group
  control_group <- detect_control_group(payload$metadata$groups)
  has_control <- !is.null(control_group)

  # =========================================================
  # 5. BUILD COMPARISON PAIRS
  # =========================================================
  if (control_only && has_control) {
    # Only comparisons against control: KO1/Control, KO2/Control, etc.
    non_control <- setdiff(groups, control_group)
    pairs <- lapply(non_control, function(g) list(a = control_group, b = g))
  } else {
    # All pairwise comparisons
    pair_combos <- utils::combn(groups, 2, simplify = FALSE)
    pairs <- lapply(pair_combos, function(x) {
      # If control is present, always make it the denominator (a)
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

  if (length(pairs) == 0) {
    stop("FC Heatmap: no comparison pairs could be generated")
  }

  # =========================================================
  # 6. COMPUTE FC MATRIX
  # =========================================================
  comparison_names <- vapply(pairs, function(p) paste0(p$b, "/", p$a), character(1))

  fc_mat <- matrix(NA_real_, nrow = length(matched_targets), ncol = length(pairs))
  rownames(fc_mat) <- matched_targets
  colnames(fc_mat) <- comparison_names

  for (i in seq_along(pairs)) {
    p <- pairs[[i]]
    cols_a <- as.character(samples[[sample_col]][samples$group_name == p$a])
    cols_b <- as.character(samples[[sample_col]][samples$group_name == p$b])
    cols_a <- intersect(cols_a, colnames(mat_sub))
    cols_b <- intersect(cols_b, colnames(mat_sub))

    if (length(cols_a) == 0 || length(cols_b) == 0) next

    mean_a <- rowMeans(mat_sub[, cols_a, drop = FALSE], na.rm = TRUE)
    mean_b <- rowMeans(mat_sub[, cols_b, drop = FALSE], na.rm = TRUE)

    fc_mat[, i] <- compute_fc(mean_a, mean_b, log_transform, is_log_transformed)
  }

  # =========================================================
  # 7. COMPUTE Z-SCORES
  # =========================================================
  mat_zscore <- t(apply(fc_mat, 1, scale_row))
  dimnames(mat_zscore) <- dimnames(fc_mat)

  # =========================================================
  # 8. COMPUTE DENDROGRAMS
  # =========================================================
  dendro_zscore <- NULL
  dendro_fc <- NULL

  if (cluster_rows && nrow(fc_mat) >= 2) {
    dendro_zscore <- compute_z_dendro(mat_zscore)
    dendro_fc <- compute_fc_dendro(fc_mat)
  }

  # =========================================================
  # 9. RETURN RESULT
  # =========================================================
  list(
    engine_id = "fc_heatmap",
    params = params,
    data = list(
      mat_fc = fc_mat,
      mat_zscore = mat_zscore,
      dendro_zscore = dendro_zscore,
      dendro_fc = dendro_fc,
      target_order = matched_targets,
      gene_order = matched_targets,           # backward compat alias
      comparison_order = comparison_names,
      matched_targets = matched_targets,
      matched_genes = matched_targets,         # backward compat alias
      unmatched_targets = unmatched_targets,
      unmatched_genes = unmatched_targets,     # backward compat alias
      log_transform = log_transform,
      control_only = control_only,
      control_group = control_group,
      ids = ids,
      id_cols = list(
        protein = as.character(payload$metadata$id_protein_col %||% "protein_id"),
        gene = id_lookup_col %||% as.character(payload$metadata$id_gene_col %||% "")
      )
    )
  )
}
