# =========================================================
# R/engines/stats/pathway_fcs.R - 1D Pathway Functional Class Scoring Engine
#
# Performs rank-based functional class scoring on metabolite pathways
# (similar to 1D GO-FCS but for KEGG/Reactome pathways).
#
# Uses the Perseus method: Wilcoxon rank-sum test on ranked metabolites.
# Score = 2 * (mean_rank_in - mean_rank_out) / N, range [-1, 1]
#
# Contract:
#  - data$terms: pathway_id, pathway_name, pathway_type, fdr, score, n_metabolites, metabolite_ids
#  - data$analyses: (multi-comparison mode) named list of per-comparison results
#  - Input is ranked metabolite scores (e.g., from volcano fold-change or PCA loadings)
#  - Standalone mode: computes FC vectors from data matrix when no scores provided
# =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------
# Core FCS computation helper
# Runs pathway FCS on a single named score vector against metabobase.
# Returns a list(terms = data.frame, n_scored = int) or NULL on failure.
# ---------------------------------------------------------
pathway_fcs_core <- function(scores, metabobase, params, add_log) {
  fdr_cutoff <- params$fdr_cutoff %||% 0.03
  min_pathway_size <- params$min_pathway_size %||% 5
  min_overlap <- params$min_overlap %||% 3
  max_terms <- params$max_terms %||% 20

  # Ensure scores is a named numeric vector
  if (!is.numeric(scores) || is.null(names(scores))) {
    add_log("ERROR", "Scores must be a named numeric vector")
    return(NULL)
  }

  # Remove NA scores
  scores <- scores[!is.na(scores)]
  n_scores <- length(scores)

  if (n_scores < 10) {
    add_log("WARN", sprintf("Too few scored metabolites: %d", n_scores))
    return(NULL)
  }

  add_log("INFO", sprintf("Input: %d metabolites with scores", n_scores))

  # Extract pathway mappings from metabobase
  annot_long <- metabobase$annot_long
  terms_by_id <- metabobase$terms_by_id

  if (is.null(annot_long) || !is.data.frame(annot_long) || nrow(annot_long) == 0) {
    add_log("ERROR", "MetaboBase missing pathway annotations")
    return(NULL)
  }

  # Build term-to-metabolite mapping
  term_metabolites <- split(annot_long$metabolite_id, annot_long$pathway_id)

  # Multi-layer ID reconciliation: remap score names to metabobase IDs
  reconciled <- metabobase_reconcile_ids(
    query_ids = names(scores),
    metabobase = metabobase,
    use_pubchem_resolver = TRUE,
    log_callback = add_log
  )
  id_map <- reconciled$id_map

  # Remap: replace names(scores) with resolved metabobase metabolite_ids
  if (length(id_map) > 0) {
    mapped_names <- id_map[names(scores)]
    has_mapping <- !is.na(mapped_names)
    if (any(has_mapping)) {
      new_scores <- scores[has_mapping]
      names(new_scores) <- as.character(mapped_names[has_mapping])
      new_scores <- new_scores[!duplicated(names(new_scores))]
      n_before <- length(scores)
      scores <- new_scores
      add_log("INFO", sprintf("ID reconciliation remapped %d -> %d scored metabolites",
                               n_before, length(scores)))
    }
  }

  # Get pathway names and types
  pathway_names <- stats::setNames(rep(NA_character_, length(term_metabolites)), names(term_metabolites))
  pathway_types <- stats::setNames(rep(NA_character_, length(term_metabolites)), names(term_metabolites))

  if (!is.null(terms_by_id) && is.data.frame(terms_by_id)) {
    if ("pathway_name" %in% names(terms_by_id) && "pathway_id" %in% names(terms_by_id)) {
      idx <- match(names(term_metabolites), terms_by_id$pathway_id)
      pathway_names[!is.na(idx)] <- terms_by_id$pathway_name[idx[!is.na(idx)]]
    }
    if ("pathway_type" %in% names(terms_by_id) && "pathway_id" %in% names(terms_by_id)) {
      idx <- match(names(term_metabolites), terms_by_id$pathway_id)
      pathway_types[!is.na(idx)] <- terms_by_id$pathway_type[idx[!is.na(idx)]]
    }
  }

  # Vectorized fallback: fill missing pathway types/names from annot_long
  missing_types <- is.na(pathway_types)
  if (any(missing_types)) {
    pw_ids <- names(pathway_types)[missing_types]
    idx <- match(pw_ids, annot_long$pathway_id)
    found <- !is.na(idx)
    pathway_types[pw_ids[found]] <- annot_long$pathway_type[idx[found]]
  }
  missing_names <- is.na(pathway_names)
  if (any(missing_names)) {
    pw_ids <- names(pathway_names)[missing_names]
    idx <- match(pw_ids, annot_long$pathway_id)
    found <- !is.na(idx)
    pathway_names[pw_ids[found]] <- annot_long$pathway_name[idx[found]]
  }

  # Compute ranks (Perseus method: rank by score descending, ties averaged)
  ranks <- rank(-scores, ties.method = "average")
  names(ranks) <- names(scores)
  N <- length(ranks)

  # Pre-compute tie correction factor for Wilcoxon normal approximation
  # (same for all pathways since overall ranks don't change)
  rank_freq <- tabulate(match(ranks, sort(unique(ranks))))
  rank_freq <- rank_freq[rank_freq > 0]
  tie_correction <- sum(rank_freq^3 - rank_freq) / (N * (N - 1))

  # Perform Wilcoxon rank-sum test for each pathway
  # Uses inline normal approximation (identical to wilcox.test(exact=FALSE))
  results <- list()

  for (pathway_id in names(term_metabolites)) {
    pathway_mets <- unique(term_metabolites[[pathway_id]])

    # Filter to metabolites with scores
    mets_in <- intersect(pathway_mets, names(ranks))
    n_in <- length(mets_in)

    # Skip small pathways or insufficient overlap
    if (n_in < min_overlap) next
    if (length(pathway_mets) < min_pathway_size) next

    n_out <- N - n_in
    if (n_out == 0) next

    # Inline Wilcoxon rank-sum normal approximation
    ranks_in <- ranks[mets_in]
    W <- sum(ranks_in)
    E_W <- n_in * (N + 1) / 2
    V_W <- (n_in * n_out / 12) * ((N + 1) - tie_correction)

    if (V_W <= 0) next

    z <- (W - E_W) / sqrt(V_W)
    pval <- 2 * stats::pnorm(-abs(z))

    # Calculate FCS score: 2 * (mean_rank_out - mean_rank_in) / N
    # Range: [-1, 1], positive = enriched at top of ranked list
    mean_rank_in <- W / n_in
    mean_rank_out <- (sum(ranks) - W) / n_out
    fcs_score <- 2 * (mean_rank_out - mean_rank_in) / N

    results <- c(results, list(data.frame(
      pathway_id = pathway_id,
      pathway_name = pathway_names[pathway_id] %||% pathway_id,
      pathway_type = pathway_types[pathway_id] %||% "unknown",
      pval = pval,
      score = fcs_score,
      n_metabolites = n_in,
      pathway_size = length(pathway_mets),
      metabolite_ids = paste(mets_in, collapse = ";"),
      stringsAsFactors = FALSE
    )))
  }

  if (length(results) == 0) {
    add_log("WARN", "No pathways passed filtering criteria")
    return(NULL)
  }

  # Combine results
  results_df <- do.call(rbind, results)

  # FDR correction
  results_df$fdr <- stats::p.adjust(results_df$pval, method = "BH")

  # Filter by FDR
  results_df <- results_df[results_df$fdr <= fdr_cutoff, , drop = FALSE]

  if (nrow(results_df) == 0) {
    add_log("INFO", sprintf("No pathways significant at FDR <= %.3f", fdr_cutoff))
    return(NULL)
  }

  # Sort by absolute score (most enriched/depleted first), then by FDR
  results_df <- results_df[order(-abs(results_df$score), results_df$fdr), , drop = FALSE]

  # Limit per pathway_type to keep balance across databases
  final_results <- list()
  for (pt in unique(results_df$pathway_type)) {
    pt_df <- results_df[results_df$pathway_type == pt, , drop = FALSE]
    pt_df <- head(pt_df, max_terms)
    final_results <- c(final_results, list(pt_df))
  }
  results_df <- do.call(rbind, final_results)

  add_log("INFO", sprintf("Pathway FCS: %d significant pathways found", nrow(results_df)))

  # Build output terms
  terms_out <- data.frame(
    pathway_id = results_df$pathway_id,
    pathway_name = results_df$pathway_name,
    pathway_type = results_df$pathway_type,
    fdr = results_df$fdr,
    score = results_df$score,
    n_metabolites = results_df$n_metabolites,
    metabolite_ids = results_df$metabolite_ids,
    stringsAsFactors = FALSE
  )

  list(terms = terms_out, n_scored = length(scores))
}


#' Execute 1D Pathway FCS engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with metabobase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_pathway_fcs_run <- function(payload, params = NULL, context = NULL) {
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

  # Empty result helper
  empty_result <- function(error_msg = NULL, warn_msg = NULL) {
    log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
    if (is.null(log_df) || nrow(log_df) == 0) {
      log_df <- data.frame(
        time = format(Sys.time()),
        level = if (!is.null(error_msg)) "ERROR" else "WARN",
        message = error_msg %||% warn_msg %||% "No results",
        stringsAsFactors = FALSE
      )
    }
    list(
      engine_id = "pathway_fcs",
      params = params,
      data = list(
        terms = data.frame(
          pathway_id = character(0),
          pathway_name = character(0),
          pathway_type = character(0),
          fdr = numeric(0),
          score = numeric(0),
          n_metabolites = integer(0),
          metabolite_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = log_df
      )
    )
  }

  if (!isTRUE(payload$ok)) {
    return(empty_result(error_msg = payload$error %||% "Invalid payload"))
  }

  add_log("INFO", "Initiating 1D Pathway Functional Class Scoring")

  # Get parameters
  fdr_cutoff <- params$fdr_cutoff %||% 0.03
  min_pathway_size <- params$min_pathway_size %||% 5
  min_overlap <- params$min_overlap %||% 3
  max_terms <- params$max_terms %||% 20
  score_label <- params$score_label %||% payload$score_label %||% context$score_label %||% "Score"

  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, min_overlap=%d",
                          fdr_cutoff, min_pathway_size, min_overlap))

  # Get metabolite scores
  # Priority: payload$scores > params$scores > context$scores
  scores <- payload$scores %||% params$scores %||% context$scores %||% NULL
  fc_vectors <- list()

  # If no scores, try to compute from fold-change vectors
  if (is.null(scores)) {
    fc_vectors <- payload$fc_vectors %||% context$fc_vectors %||% list()
    if (length(fc_vectors) > 0) {
      if (length(fc_vectors) == 1) {
        scores <- fc_vectors[[1]]
        score_label <- paste0("log2(", gsub("_vs_", "/", names(fc_vectors)[1]), ")")
        add_log("INFO", sprintf("Using fold-change vector: %s", names(fc_vectors)[1]))
        fc_vectors <- list()  # consumed
      }
      # If multiple, handled in multi-comparison mode below
    }
  }

  # Standalone FC mode: compute FC vectors from data matrix
  if (is.null(scores) && length(fc_vectors) == 0) {
    mat <- payload$mat
    samples <- payload$samples
    groups <- payload$groups

    if (!is.null(mat) && !is.null(groups) && length(groups) >= 2) {
      add_log("INFO", "Standalone FC mode: computing fold-change vectors")

      # Get metabolite IDs
      id_col <- as.character(payload$metadata$id_primary_col %||% "")[1]
      metabolite_ids <- if (nzchar(id_col) && !is.null(payload$ids) &&
                            id_col %in% names(payload$ids)) {
        as.character(payload$ids[[id_col]])
      } else {
        rownames(mat)
      }

      # Get control info from metadata
      meta_groups <- payload$metadata$groups
      is_control <- if (!is.null(meta_groups$is_control)) {
        tolower(as.character(meta_groups$is_control)) %in% c("true", "t", "1", "yes")
      } else {
        rep(FALSE, nrow(meta_groups %||% data.frame()))
      }
      group_names <- as.character(meta_groups$group_name %||% character(0))

      # Match is_control to groups vector
      is_control_map <- rep(FALSE, length(groups))
      for (idx in seq_along(groups)) {
        match_idx <- match(groups[idx], group_names)
        if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
      }

      control_only <- isTRUE(params$control_only)
      control_idx <- which(is_control_map)
      has_control <- length(control_idx) == 1
      is_log_transformed <- isTRUE(params$is_log_transformed)

      if (control_only && has_control) {
        add_log("INFO", sprintf("Control-only mode: generating FC vectors against control (%s)",
                                groups[control_idx]))
      }

      # Compute FC vectors for each comparison
      n_groups <- length(groups)
      for (i in seq_len(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          if (control_only && has_control) {
            if (!i %in% control_idx && !j %in% control_idx) next
          }
          grp_a <- groups[i]
          grp_b <- groups[j]
          # Control orientation: control as baseline (grp_a)
          if (has_control && j %in% control_idx && !(i %in% control_idx)) {
            tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
          }

          cols_a <- samples$sample_col[samples$group_name == grp_a]
          cols_b <- samples$sample_col[samples$group_name == grp_b]
          cols_a <- intersect(cols_a, colnames(mat))
          cols_b <- intersect(cols_b, colnames(mat))

          if (length(cols_a) > 0 && length(cols_b) > 0) {
            mean_a <- rowMeans(mat[, cols_a, drop = FALSE], na.rm = TRUE)
            mean_b <- rowMeans(mat[, cols_b, drop = FALSE], na.rm = TRUE)
            fc <- if (is_log_transformed) mean_b - mean_a
                  else log2(mean_b + 1) - log2(mean_a + 1)
            names(fc) <- metabolite_ids

            comp_key <- paste0(grp_b, "_vs_", grp_a)
            fc_vectors[[comp_key]] <- fc
            add_log("INFO", sprintf("  %s: computed", comp_key))
          }
        }
      }

      if (length(fc_vectors) == 1) {
        scores <- fc_vectors[[1]]
        score_label <- paste0("log2(", gsub("_vs_", "/", names(fc_vectors)[1]), ")")
        add_log("INFO", sprintf("Using %s as primary score vector", score_label))
        fc_vectors <- list()  # consumed
      }
    }
  }

  # No scores and no FC vectors -> error
  if ((is.null(scores) || length(scores) == 0) && length(fc_vectors) == 0) {
    add_log("ERROR", "No scores provided for pathway FCS")
    return(empty_result(error_msg = "No metabolite scores provided. Requires ranked scores from parent analysis or standalone FC computation."))
  }

  # Get metabobase mappings
  metabobase <- payload$metabobase %||% context$metabobase

  if (is.null(metabobase)) {
    add_log("ERROR", "Pathway FCS requires metabobase with pathway annotations")
    return(empty_result(error_msg = "Pathway FCS requires metabobase with pathway annotations"))
  }

  # Log available pathway types
  annot_long <- metabobase$annot_long
  if (!is.null(annot_long) && is.data.frame(annot_long) && nrow(annot_long) > 0) {
    pw_types <- unique(annot_long$pathway_type)
    add_log("INFO", sprintf("Pathway types in MetaboBase: %s (%d total annotations)",
                            paste(pw_types, collapse = ", "), nrow(annot_long)))
  }

  # -------------------------------------------------------
  # Multi-comparison mode: run FCS for each FC vector
  # -------------------------------------------------------
  if (length(fc_vectors) > 1) {
    add_log("INFO", sprintf("Running Pathway FCS for %d pairwise comparisons", length(fc_vectors)))

    analyses <- list()
    for (comp_name in names(fc_vectors)) {
      comp_scores <- fc_vectors[[comp_name]]
      comp_label <- paste0("log2(", gsub("_vs_", "/", comp_name), ")")
      add_log("INFO", sprintf("  Analyzing %s", comp_name))

      core_result <- pathway_fcs_core(comp_scores, metabobase, params, add_log)

      if (!is.null(core_result)) {
        analyses[[comp_name]] <- list(
          terms = core_result$terms,
          score_label = comp_label,
          comparison = comp_name
        )
      }
    }

    # Log engine runtime
    engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
    add_log("INFO", sprintf("Pathway FCS multi-comparison completed in %.2f seconds (%d comparisons)",
                            engine_duration, length(analyses)))

    log_df <- do.call(rbind, lapply(log_entries, function(e) {
      data.frame(time = e$time, level = e$level, message = e$message, stringsAsFactors = FALSE)
    }))

    return(list(
      engine_id = "pathway_fcs",
      params = params,
      data = list(
        analyses = analyses,
        log = log_df
      )
    ))
  }

  # -------------------------------------------------------
  # Single comparison mode
  # -------------------------------------------------------
  core_result <- pathway_fcs_core(scores, metabobase, params, add_log)

  if (is.null(core_result)) {
    return(empty_result(warn_msg = "No significant pathways found"))
  }

  # Record runtime
  runtime_sec <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Pathway FCS engine completed in %.2f seconds", runtime_sec))
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  list(
    engine_id = "pathway_fcs",
    params = params,
    data = list(
      terms = core_result$terms,
      n_scored = core_result$n_scored,
      score_label = score_label,
      log = log_df
    )
  )
}
