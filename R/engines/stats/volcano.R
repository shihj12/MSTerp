# =========================================================
# R/engines/stats/volcano.R — Volcano Plot Engine
#
# Computes differential expression for ALL pairwise group
# comparisons with ID inference, optional display ID
# annotation, and frozen sets for paired GO children.
#
# Contract v1.1:
#  - data$comparisons: list of comparison results, each containing:
#    - points: primary_id, display_id, log2fc, pval, padj, significance
#    - sets: sig_up, sig_down
#    - comparison: group_a, group_b, apply_fdr, sig_threshold, fc_threshold
#  - data$fc_vectors: named list of log2fc vectors keyed by "grpA_vs_grpB"
#  - data$sets: combined sig_up, sig_down across all comparisons
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Compute a single pairwise comparison
#' @param mat Data matrix
#' @param samples Sample info data.frame
#' @param grp_a First group name
#' @param grp_b Second group name
#' @param primary_ids Primary ID vector
#' @param display_ids Display ID vector
#' @param stat_mode Statistical test mode
#' @param fc_transform Fold-change transform
#' @param apply_fdr Whether to use FDR-adjusted p-values (TRUE) or raw p-values (FALSE)
#' @param fc_threshold FC threshold c(down, up)
#' @param p_threshold P-value threshold
#' @param add_log Logging function
#' @return List with points, sets, comparison info
.volcano_pairwise <- function(mat, samples, grp_a, grp_b, primary_ids, display_ids,
                               stat_mode, fc_transform, apply_fdr, fc_threshold,
                               p_threshold, add_log, is_log_transformed = FALSE) {

  add_log("INFO", sprintf("--- Comparing: %s vs %s ---", grp_a, grp_b))

  cols_a <- samples$sample_col[samples$group_name == grp_a]
  cols_b <- samples$sample_col[samples$group_name == grp_b]
  cols_a <- intersect(cols_a, colnames(mat))
  cols_b <- intersect(cols_b, colnames(mat))

  add_log("INFO", sprintf("  Samples: %d in %s, %d in %s", length(cols_a), grp_a, length(cols_b), grp_b))

  if (length(cols_a) == 0 || length(cols_b) == 0) {
    add_log("WARN", sprintf("  Skipping %s vs %s: no valid columns", grp_a, grp_b))
    return(NULL)
  }

  # Build results data.frame
  results <- data.frame(
    primary_id = primary_ids,
    display_id = display_ids,
    protein_id = primary_ids,    # backward compat
    gene_symbol = display_ids,   # backward compat
    gene_id = display_ids,       # backward compat
    stringsAsFactors = FALSE
  )

  mat_a <- mat[, cols_a, drop = FALSE]
  mat_b <- mat[, cols_b, drop = FALSE]

  # Normalize stat mode early (needed for mean type decision)
  stat_mode_norm <- tolower(as.character(stat_mode %||% "ttest"))
  if (stat_mode_norm == "t_test") stat_mode_norm <- "ttest"

  # Compute mean values
  # limma operates on log-scale data, so use geometric mean for consistency
  if (!is_log_transformed && stat_mode_norm == "limma") {
    log_mean_a <- rowMeans(log2(mat_a + 1), na.rm = TRUE)
    log_mean_b <- rowMeans(log2(mat_b + 1), na.rm = TRUE)
    mean_a <- pmax(0, 2^log_mean_a - 1)
    mean_b <- pmax(0, 2^log_mean_b - 1)
  } else {
    mean_a <- rowMeans(mat_a, na.rm = TRUE)
    mean_b <- rowMeans(mat_b, na.rm = TRUE)
  }

  # Compute fold change (B vs A, so positive = higher in B)
  if (is_log_transformed) {
    # Data already log-transformed: subtraction = log-scale ratio
    results$log2fc <- mean_b - mean_a
  } else if (fc_transform == "log2") {
    results$log2fc <- log2(mean_b + 1) - log2(mean_a + 1)
  } else if (fc_transform == "log10") {
    results$log2fc <- log2(10) * (log10(mean_b + 1) - log10(mean_a + 1))
  } else {
    results$log2fc <- log2((mean_b + 1) / (mean_a + 1))
  }

  if (stat_mode_norm == "limma") {
    if (!requireNamespace("limma", quietly = TRUE)) {
      stop("Missing package 'limma'. Please install with:\\nif (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')\\nBiocManager::install('limma')")
    }

    # limma expects approximately log-scale values
    mat_sub <- cbind(mat_a, mat_b)
    y <- if (is_log_transformed) mat_sub else log2(mat_sub + 1)

    grp <- factor(
      c(rep("A", ncol(mat_a)), rep("B", ncol(mat_b))),
      levels = c("A", "B")
    )
    design <- stats::model.matrix(~0 + grp)
    colnames(design) <- c("A", "B")

    fit <- limma::lmFit(y, design)
    contrast <- limma::makeContrasts(BvsA = B - A, levels = design)
    fit2 <- limma::contrasts.fit(fit, contrast)
    fit2 <- limma::eBayes(fit2)

    results$log2fc <- as.numeric(fit2$coefficients[, 1])
    results$pval <- as.numeric(fit2$p.value[, 1])
    results$padj <- p.adjust(results$pval, method = "BH")

    # Extra limma statistics (opt-in columns)
    results$t <- as.numeric(fit2$t[, 1])
    results$AveExpr <- as.numeric(fit2$Amean)
    results$B <- as.numeric(fit2$lods[, 1])
  } else {
    pvals <- numeric(nrow(mat))

    for (i in seq_len(nrow(mat))) {
      vals_a <- mat_a[i, ]
      vals_b <- mat_b[i, ]

      vals_a <- vals_a[!is.na(vals_a) & is.finite(vals_a)]
      vals_b <- vals_b[!is.na(vals_b) & is.finite(vals_b)]

      if (length(vals_a) >= 2 && length(vals_b) >= 2) {
        tryCatch({
          tt <- t.test(vals_a, vals_b, var.equal = FALSE)
          pvals[i] <- tt$p.value
        }, error = function(e) {
          pvals[i] <<- NA_real_
        })
      } else {
        pvals[i] <- NA_real_
      }
    }

    results$pval <- pvals
    results$padj <- p.adjust(pvals, method = "BH")
  }

  results$mean_a <- mean_a
  results$mean_b <- mean_b

  # Track uniquely quantified proteins (quantified in ALL replicates of one group,
  # but missing in ALL replicates of the other group).
  #
  # Definition for comparison A vs B:
  # - unique_to_a: quantified in all A replicates AND missing in all B replicates
  # - unique_to_b: quantified in all B replicates AND missing in all A replicates
  #
  # This is used for downstream paired analysis sets (GO) and must NOT be counted
  # as "significant up/down" on the volcano.
  is_valid_val <- function(x) !is.na(x) & is.finite(x) & x > 0
  has_any_a <- rowSums(is_valid_val(mat_a)) > 0
  has_any_b <- rowSums(is_valid_val(mat_b)) > 0
  has_all_a <- rowSums(is_valid_val(mat_a)) == ncol(mat_a)
  has_all_b <- rowSums(is_valid_val(mat_b)) == ncol(mat_b)
  unique_to_a <- has_all_a & !has_any_b
  unique_to_b <- has_all_b & !has_any_a
  n_unique_a <- sum(unique_to_a)
  n_unique_b <- sum(unique_to_b)

  add_log("INFO", sprintf("  Unique to %s: %d, Unique to %s: %d", grp_a, n_unique_a, grp_b, n_unique_b))

  # Filter valid results
  valid_idx <- !is.na(results$pval) & is.finite(results$log2fc)
  n_excluded <- sum(!valid_idx)
  results_valid <- results[valid_idx, , drop = FALSE]

  add_log("INFO", sprintf("  Valid features: %d (excluded %d)", nrow(results_valid), n_excluded))

  # Determine significance: apply_fdr=TRUE uses padj, FALSE uses raw pval
  sig_values <- if (isTRUE(apply_fdr)) results_valid$padj else results_valid$pval
  fc_up <- fc_threshold[2]
  fc_down <- fc_threshold[1]

  unique_ids <- union(primary_ids[unique_to_a], primary_ids[unique_to_b])
  is_unique_valid <- results_valid$primary_id %in% unique_ids

  is_sig_up <- results_valid$log2fc > fc_up & sig_values < p_threshold & !is.na(sig_values) & !is_unique_valid
  is_sig_down <- results_valid$log2fc < fc_down & sig_values < p_threshold & !is.na(sig_values) & !is_unique_valid

  results_valid$significance <- ifelse(is_sig_up, "up", ifelse(is_sig_down, "down", "ns"))

  add_log("INFO", sprintf("  Significant up: %d, down: %d", sum(is_sig_up), sum(is_sig_down)))

  # Build sets (including uniquely quantified proteins for GO-ORA)
  sets <- list(
    sig_up = results_valid$primary_id[is_sig_up],
    sig_down = results_valid$primary_id[is_sig_down],
    # FIX: Include protein IDs for uniquely quantified proteins (for paired GO-ORA)
    unique_quantified_A = primary_ids[unique_to_a],
    unique_quantified_B = primary_ids[unique_to_b]
  )

  # Build FC vector (all proteins, including those with NA - for 2dgofcs ranking)
  fc_vector <- results$log2fc
  names(fc_vector) <- primary_ids

  list(
    points = {
      cols <- c("primary_id", "display_id", "protein_id", "gene_symbol", "gene_id", "log2fc", "pval", "padj", "mean_a", "mean_b", "significance")
      if (tolower(as.character(stat_mode %||% "ttest")) == "limma") {
        cols <- c(cols, "t", "AveExpr", "B")
      }
      results_valid[, cols, drop = FALSE]
    },
    sets = sets,
    fc_vector = fc_vector,
    comparison = list(
      group_a = grp_a,
      group_b = grp_b,
      label = paste0(grp_b, "_vs_", grp_a),
      apply_fdr = apply_fdr,
      sig_threshold = p_threshold,
      fc_threshold = fc_threshold,
      n_sig_up = sum(is_sig_up),
      n_sig_down = sum(is_sig_down),
      n_total = nrow(results_valid),
      # FIX: Add unique quantification counts
      n_unique_a = n_unique_a,
      n_unique_b = n_unique_b
    )
  )
}

#' Execute volcano engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_volcano_run <- function(payload, params = NULL, context = NULL) {
  # Track engine start time
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

  if (!isTRUE(payload$ok)) {
    return(list(
      engine_id = "volcano",
      params = params,
      data = list(
        comparisons = list(),
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

  # Resolve data-type context for terminology
  ctx <- payload$data_type_context %||% msterp_data_type_context("proteomics")

  add_log("INFO", sprintf("Initiating volcano analysis: %d features, %d groups", nrow(mat), length(groups)))

  if (length(groups) < 2) {
    return(list(
      engine_id = "volcano",
      params = params,
      data = list(
        comparisons = list(),
        fc_vectors = list(),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "Volcano requires at least 2 groups",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Get parameters
  stat_mode <- tolower(as.character(params$stat_mode %||% "ttest"))
  if (stat_mode == "t_test") stat_mode <- "ttest"
  fc_transform <- params$fc_transform %||% "log2"
  is_log_transformed <- isTRUE(params$is_log_transformed)
  # apply_fdr: TRUE uses FDR-adjusted p-values (padj), FALSE uses raw p-values (pval)
  apply_fdr <- params$apply_fdr %||% TRUE
  fc_threshold <- params$fc_threshold %||% c(-1, 1)
  p_threshold <- as.numeric(params$p_threshold %||% 0.05)

  # Ensure fc_threshold has 2 values
  if (length(fc_threshold) < 2) {
    fc_threshold <- c(-1, 1)
    add_log("WARN", "fc_threshold malformed, using default c(-1, 1)")
  }

  # Get ID columns (context-aware: metabolite name for metabolomics, gene for proteomics)
  primary_col <- as.character(payload$metadata$id_primary_col %||% payload$metadata$id_protein_col %||% "")[1]
  display_col <- ctx_resolve_id_col(ctx, payload$metadata, names(ids)) %||%
                 as.character(payload$metadata$id_gene_col %||% "")[1]

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

  # =========================================================
  # Generate pairwise comparisons (with control-only filtering)
  # =========================================================
  n_groups <- length(groups)
  n_all_comparisons <- n_groups * (n_groups - 1) / 2

  # Get control info from metadata
  meta_groups <- payload$metadata$groups
  is_control <- if (!is.null(meta_groups$is_control)) {
    tolower(as.character(meta_groups$is_control)) %in% c("true", "t", "1", "yes")
  } else {
    rep(FALSE, nrow(meta_groups %||% data.frame()))
  }
  group_names <- as.character(meta_groups$group_name %||% character(0))

  # Match is_control to our groups vector (in case order differs)
  is_control_map <- rep(FALSE, length(groups))
  for (idx in seq_along(groups)) {
    match_idx <- match(groups[idx], group_names)
    if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
  }

  control_only <- isTRUE(params$control_only)
  control_idx <- which(is_control_map)
  has_control <- length(control_idx) == 1

  if (control_only && has_control) {
    # Filter to only control comparisons (will generate n_groups - 1 comparisons)
    add_log("INFO", sprintf("Control-only mode: generating %d comparisons against control (%s)",
                            n_groups - 1, groups[control_idx]))
  } else {
    add_log("INFO", sprintf("Generating %d pairwise comparisons", n_all_comparisons))
  }

  comparisons <- list()
  fc_vectors <- list()
  all_sig_up <- character(0)
  all_sig_down <- character(0)
  # FIX: Track uniquely quantified proteins across all comparisons
  all_unique_A <- character(0)
  all_unique_B <- character(0)

  # Generate all pairs (i < j to avoid duplicates)
  for (i in seq_len(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      # Skip non-control comparisons if control_only is enabled
      if (control_only && has_control) {
        if (!i %in% control_idx && !j %in% control_idx) {
          next  # Skip comparison not involving control
        }
      }

      grp_a <- groups[i]
      grp_b <- groups[j]

      # Control orientation: control should always be the denominator/baseline (grp_a)
      # Fold change is calculated as B/A, so positive log2fc means higher in B than A
      # For ko1_vs_control, we want ko1/control, so control must be grp_a
      # Only swap when control is in j position (needs to move to grp_a)
      if (has_control && j %in% control_idx && !(i %in% control_idx)) {
        # grp_b is control but should be grp_a (baseline) - swap needed
        tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
      }
      # If i is control, grp_a is already control (baseline) - no swap needed

      result <- .volcano_pairwise(
        mat = mat,
        samples = samples,
        grp_a = grp_a,
        grp_b = grp_b,
        primary_ids = primary_ids,
        display_ids = display_ids,
        stat_mode = stat_mode,
        fc_transform = fc_transform,
        apply_fdr = apply_fdr,
        fc_threshold = fc_threshold,
        p_threshold = p_threshold,
        add_log = add_log,
        is_log_transformed = is_log_transformed
      )

      if (!is.null(result)) {
        comp_key <- paste0(grp_b, "_vs_", grp_a)
        comparisons[[comp_key]] <- result
        fc_vectors[[comp_key]] <- result$fc_vector

        # Accumulate significant proteins across all comparisons
        all_sig_up <- union(all_sig_up, result$sets$sig_up)
        all_sig_down <- union(all_sig_down, result$sets$sig_down)
        # FIX: Accumulate uniquely quantified proteins
        all_unique_A <- union(all_unique_A, result$sets$unique_quantified_A)
        all_unique_B <- union(all_unique_B, result$sets$unique_quantified_B)
      }
    }
  }

  # Build combined summary with unique quantification counts
  summary_df <- data.frame(
    comparison = names(comparisons),
    n_total = vapply(comparisons, function(x) x$comparison$n_total, integer(1)),
    n_sig_up = vapply(comparisons, function(x) x$comparison$n_sig_up, integer(1)),
    n_sig_down = vapply(comparisons, function(x) x$comparison$n_sig_down, integer(1)),
    # FIX: Add unique quantification counts to summary table
    n_unique_a = vapply(comparisons, function(x) x$comparison$n_unique_a %||% 0L, integer(1)),
    n_unique_b = vapply(comparisons, function(x) x$comparison$n_unique_b %||% 0L, integer(1)),
    stringsAsFactors = FALSE
  )
  # FIX: Add more descriptive column names for unique counts (group names)
  # Use group_a/group_b from each comparison to create labels
  for (i in seq_along(comparisons)) {
    comp <- comparisons[[i]]$comparison
    colnames(summary_df)[5] <- paste0("unique_", comp$group_a)
    colnames(summary_df)[6] <- paste0("unique_", comp$group_b)
    break  # Column names set from first comparison
  }

  # Combined sets across all comparisons (including unique quantified for GO-ORA)
  combined_sets <- list(
    sig_up = all_sig_up,
    sig_down = all_sig_down,
    # FIX: Include unique quantified sets for paired GO-ORA with include_unique_in_sig
    unique_quantified_A = all_unique_A,
    unique_quantified_B = all_unique_B
  )

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Volcano completed in %.2f seconds (%d comparisons)",
                          engine_duration, length(comparisons)))

  # Build log data.frame
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  # For backward compatibility, also include first comparison as "points"
  first_comp <- if (length(comparisons) > 0) comparisons[[1]] else NULL

  list(
    engine_id = "volcano",
    params = params,
    data = list(
      # All pairwise comparisons
      comparisons = comparisons,
      # FC vectors for 2dgofcs
      fc_vectors = fc_vectors,
      # Combined sets
      sets = combined_sets,
      # Summary table
      summary = summary_df,
      # Backward compatibility: first comparison as "points"
      points = if (!is.null(first_comp)) first_comp$points else data.frame(),
      comparison = if (!is.null(first_comp)) first_comp$comparison else list(),
      # Log
      log = log_df
    )
  )
}
