# =========================================================
# R/engines/stats/gene_barchart.R — Feature Bar Chart Engine
#
# Creates bar charts for selected features from heatmap results.
# Shows averaged group values (non-FC) or FC values per comparison.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Convert p-value to significance stars
#' @param pval Numeric p-value
#' @return Character string with stars or "ns"
pval_to_stars <- function(pval) {
  if (is.na(pval) || !is.finite(pval)) return("")

  if (pval < 0.001) return("***")
  if (pval < 0.01) return("**")
  if (pval < 0.05) return("*")
  return("ns")
}

#' Format p-value for display (2 significant figures)
#' @param pval Numeric p-value
#' @return Formatted string like "p = 0.051"
format_pval <- function(pval) {
  if (is.na(pval) || !is.finite(pval)) return("")
  if (pval < 0.001) return("p < 0.001")
  sprintf("p = %s", signif(pval, 2))
}

#' Compute group means from sample matrix
#' @param mat Matrix with samples as columns, features as rows
#' @param group_annotations Data frame with sample and group columns
#' @return List with means matrix and sem matrix (Standard Error of Mean)
compute_group_stats <- function(mat, group_annotations) {
  groups <- unique(group_annotations$group)
  features <- rownames(mat)

  means_mat <- matrix(NA_real_, nrow = length(features), ncol = length(groups))
  rownames(means_mat) <- features
  colnames(means_mat) <- groups

  sem_mat <- matrix(NA_real_, nrow = length(features), ncol = length(groups))
  rownames(sem_mat) <- features
  colnames(sem_mat) <- groups

  for (grp in groups) {
    samples_in_grp <- group_annotations$sample[group_annotations$group == grp]
    samples_in_grp <- intersect(samples_in_grp, colnames(mat))

    if (length(samples_in_grp) == 0) next

    n <- length(samples_in_grp)
    if (n == 1) {
      means_mat[, grp] <- mat[, samples_in_grp]
      sem_mat[, grp] <- 0
    } else {
      means_mat[, grp] <- rowMeans(mat[, samples_in_grp, drop = FALSE], na.rm = TRUE)
      sd_vals <- apply(mat[, samples_in_grp, drop = FALSE], 1, stats::sd, na.rm = TRUE)
      sem_mat[, grp] <- sd_vals / sqrt(n)  # SEM = SD / sqrt(n)
    }
  }

  list(means = means_mat, sem = sem_mat)
}

#' Compute pairwise significance using t-tests
#' @param mat Matrix with samples as columns, features as rows
#' @param group_annotations Data frame with sample and group columns
#' @param targets Character vector of features to test
#' @return List of significance results per feature
compute_pairwise_significance <- function(mat, group_annotations, targets) {
  groups <- unique(group_annotations$group)
  sig <- list()

  if (length(groups) < 2) return(sig)

  # All pairwise combinations
  pairs <- utils::combn(groups, 2, simplify = FALSE)

  for (target in targets) {
    if (!target %in% rownames(mat)) next

    row_vals <- mat[target, ]
    sig[[target]] <- list()

    for (pair in pairs) {
      grp_a <- pair[1]
      grp_b <- pair[2]

      samples_a <- group_annotations$sample[group_annotations$group == grp_a]
      samples_b <- group_annotations$sample[group_annotations$group == grp_b]

      samples_a <- intersect(samples_a, names(row_vals))
      samples_b <- intersect(samples_b, names(row_vals))

      vals_a <- row_vals[samples_a]
      vals_b <- row_vals[samples_b]

      # Remove NAs
      vals_a <- vals_a[!is.na(vals_a)]
      vals_b <- vals_b[!is.na(vals_b)]

      # Need at least 2 values in each group for t-test
      if (length(vals_a) < 2 || length(vals_b) < 2) {
        pval <- NA_real_
      } else {
        tt <- tryCatch(
          stats::t.test(vals_a, vals_b),
          error = function(e) NULL
        )
        pval <- if (!is.null(tt)) tt$p.value else NA_real_
      }

      comp_name <- paste0(grp_b, " vs ", grp_a)
      sig[[target]][[comp_name]] <- list(
        group_a = grp_a,
        group_b = grp_b,
        pval = pval,
        stars = pval_to_stars(pval),
        pval_formatted = format_pval(pval)
      )
    }
  }

  sig
}

#' Execute Feature Bar Chart Engine
#'
#' @param payload Payload (minimal, data comes from params)
#' @param params Engine-specific parameters including parent data
#' @param context Execution context
#' @return Contract-compliant results
stats_gene_barchart_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% list()
  ctx <- payload$data_type_context %||% msterp_data_type_context("proteomics")

  # Support both new key (targets) and legacy key (genes)
  targets <- params$targets %||% params$genes
  if (is.null(targets) || length(targets) == 0) {
    stop(sprintf("Feature Bar Chart: no %s specified", ctx$entity_plural))
  }

  data_type <- params$data_type %||% "zscore"
  if (!data_type %in% c("zscore", "raw")) {
    data_type <- "zscore"
  }

  parent_engine_id <- params$parent_engine_id %||% ""
  parent_data <- params$parent_data

  if (is.null(parent_data)) {
    stop("Feature Bar Chart: parent_data is required")
  }

  # Compute group means and significance
  mat <- parent_data$mat
    if (is.null(mat)) {
      stop("Feature Bar Chart: matrix not found in parent data")
    }

    group_annotations <- parent_data$group_annotations
    if (is.null(group_annotations)) {
      stop("Feature Bar Chart: group_annotations not found in parent data")
    }

    # Filter to selected targets
    targets <- intersect(targets, rownames(mat))
    if (length(targets) == 0) {
      stop(sprintf("Feature Bar Chart: none of the selected %s found in matrix", ctx$entity_plural))
    }

    mat_sub <- mat[targets, , drop = FALSE]

    # Compute group statistics
    group_stats <- compute_group_stats(mat_sub, group_annotations)

    # Compute significance
    significance <- compute_pairwise_significance(mat_sub, group_annotations, targets)

    # Determine group order (preserve order from annotations)
    group_order <- unique(group_annotations$group)
    # Filter to groups that exist in the computed means
    group_order <- intersect(group_order, colnames(group_stats$means))

  list(
    engine_id = "gene_barchart",
    params = params,
    data = list(
      targets = targets,
      genes = targets,           # backward compat alias
      data_type = data_type,
      mat_sub = mat_sub,         # sample-level matrix for render-time recomputation
      group_means = group_stats$means,
      group_sem = group_stats$sem,
      group_order = group_order,
      group_colors = parent_data$group_colors %||% list(),
      group_annotations = group_annotations,
      significance = significance,
      parent_engine_id = parent_engine_id,
      entity_label = ctx$entity_label
    )
  )
}
