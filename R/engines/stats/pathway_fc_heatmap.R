# =========================================================
# R/engines/stats/pathway_fc_heatmap.R — Pathway FC Heatmap Engine
#
# Computes GO/complex enrichment scores (Perseus-style 1D FCS)
# for each group-vs-control comparison, producing a heatmap matrix.
# Rows = user-specified terms, columns = comparisons.
# Supports single-dataset and multi-dataset (protein-protein) mode.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Execute Pathway FC Heatmap Engine
#'
#' @param payload Payload from nr_build_step_payload or nr_build_multi_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_pathway_fc_heatmap_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries[[length(log_entries) + 1]] <<- list(
      time = format(Sys.time()), level = level, message = msg
    )
  }

  make_error <- function(msg) {
    add_log("ERROR", msg)
    list(
      engine_id = "pathway_fc_heatmap",
      params = params,
      data = list(
        mat_score = matrix(nrow = 0, ncol = 0),
        term_info = data.frame(term_id = character(0), term_name = character(0),
                               ontology = character(0), stringsAsFactors = FALSE),
        log = do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
      )
    )
  }

  # =========================================================
  # 1. VALIDATE PAYLOAD
  # =========================================================
  if (!isTRUE(payload$ok)) {
    return(make_error(payload$error %||% "Invalid payload"))
  }

  # =========================================================
  # 2. PARSE TERM LIST
  # =========================================================
  term_list_raw <- params$term_list %||% ""
  term_ids <- unique(trimws(unlist(strsplit(term_list_raw, "\n"))))
  term_ids <- term_ids[nzchar(term_ids)]

  if (length(term_ids) == 0) {
    return(make_error("No term IDs provided. Paste GO term IDs (e.g., GO:0006915) one per line."))
  }
  if (length(term_ids) > 500) {
    term_ids <- term_ids[1:500]
    add_log("WARN", "Term list truncated to 500 entries")
  }

  add_log("INFO", sprintf("Parsed %d term IDs", length(term_ids)))

  database <- params$database %||% "go"
  is_log_transformed <- isTRUE(params$is_log_transformed)
  min_overlap <- as.integer(params$min_overlap %||% 3)

  # =========================================================
  # 3. LOAD TERM_PROTEINS MAPPING
  # =========================================================
  if (!exists("build_term_proteins", mode = "function")) {
    source("R/engines/enrichment_utils.R", local = TRUE)
  }

  term_proteins <- list()
  term_info <- NULL

  if (identical(database, "complex")) {
    complexbase <- payload$complexbase %||% context$complexbase
    if (is.null(complexbase)) return(make_error("Complex enrichment requires complexbase"))

    if (!exists("complexbase_build_term_proteins", mode = "function")) {
      source("R/engines/complexbase.R", local = TRUE)
    }

    term_proteins <- if (!is.null(complexbase$term_proteins)) {
      complexbase$term_proteins
    } else {
      complexbase_build_term_proteins(complexbase)
    }
    term_info <- complexbase_get_term_info(complexbase)
    add_log("INFO", sprintf("Loaded %d protein complex mappings", length(term_proteins)))

  } else {
    # GO mode (default)
    terpbase <- payload$terpbase %||% context$terpbase
    if (is.null(terpbase)) return(make_error("GO enrichment requires terpbase with GO annotations"))

    protein_to_go <- terpbase$protein_to_go %||% terpbase$protein_terms %||% NULL
    if (is.null(protein_to_go)) {
      annotations <- terpbase$annotations %||% NULL
      if (!is.null(annotations) && "go_ids" %in% names(annotations)) {
        protein_to_go <- annotations
      }
    }
    if (is.null(protein_to_go)) return(make_error("Terpbase missing protein_to_go mapping"))

    term_proteins <- if (!is.null(terpbase$term_proteins)) {
      terpbase$term_proteins
    } else {
      build_term_proteins(protein_to_go)
    }
    term_info <- terpbase$go_terms %||% terpbase$terms %||% NULL
    add_log("INFO", sprintf("Loaded %d GO term mappings", length(term_proteins)))
  }

  if (length(term_proteins) == 0) return(make_error("No term-protein mappings found in database"))

  # =========================================================
  # 4. FILTER TO REQUESTED TERMS
  # =========================================================
  matched_term_ids <- intersect(term_ids, names(term_proteins))
  unmatched_terms <- setdiff(term_ids, matched_term_ids)

  if (length(unmatched_terms) > 0) {
    add_log("WARN", sprintf("%d term IDs not found in database: %s",
                            length(unmatched_terms),
                            paste(head(unmatched_terms, 10), collapse = ", ")))
  }
  if (length(matched_term_ids) == 0) {
    return(make_error("None of the provided term IDs were found in the database"))
  }

  add_log("INFO", sprintf("%d of %d term IDs matched", length(matched_term_ids), length(term_ids)))

  # Subset term_proteins to requested terms only
  term_proteins <- term_proteins[matched_term_ids]

  # =========================================================
  # 5. RESOLVE TERM INFO (names, ontologies)
  # =========================================================
  resolve_term_info <- function(tid) {
    tname <- tid
    tont <- "unknown"
    if (!is.null(term_info)) {
      if (is.data.frame(term_info)) {
        row <- term_info[term_info$term_id == tid |
                         (if ("go_id" %in% names(term_info)) term_info$go_id == tid else FALSE), , drop = FALSE]
        if (nrow(row) > 0) {
          name_cols <- intersect(c("term_name", "name", "description"), names(row))
          ont_cols <- intersect(c("ontology", "namespace", "ont"), names(row))
          if (length(name_cols) > 0) tname <- as.character(row[[name_cols[1]]][1])
          if (length(ont_cols) > 0) tont <- as.character(row[[ont_cols[1]]][1])
        }
      } else if (is.list(term_info) && !is.null(term_info[[tid]])) {
        ti <- term_info[[tid]]
        tname <- ti$name %||% ti$term_name %||% tid
        tont <- ti$ontology %||% ti$namespace %||% "unknown"
      }
    }
    list(term_name = tname, ontology = tont)
  }

  term_info_df <- do.call(rbind, lapply(matched_term_ids, function(tid) {
    info <- resolve_term_info(tid)
    data.frame(term_id = tid, term_name = info$term_name, ontology = info$ontology,
               stringsAsFactors = FALSE)
  }))

  # =========================================================
  # 6. COMPUTE SCORES
  # =========================================================
  is_multi <- identical(payload$mode, "multi_dataset")

  # Helper: detect control group from samples df
  detect_control_from_samples <- function(samples_df) {
    if (is.null(samples_df) || !"is_control" %in% names(samples_df)) return(NULL)
    ctrl_groups <- unique(samples_df$group_name[
      tolower(as.character(samples_df$is_control)) %in% c("true", "t", "1", "yes")
    ])
    if (length(ctrl_groups) == 1) return(ctrl_groups)
    NULL
  }

  # Helper: compute FC vectors for all non-control groups vs control
  compute_fc_vectors <- function(mat, samples_df, protein_ids, control_group) {
    all_groups <- unique(samples_df$group_name)
    test_groups <- setdiff(all_groups, control_group)

    if (length(test_groups) == 0) return(list())

    fc_vectors <- list()
    for (tg in test_groups) {
      cols_ctrl <- samples_df$sample_col[samples_df$group_name == control_group]
      cols_test <- samples_df$sample_col[samples_df$group_name == tg]
      cols_ctrl <- intersect(cols_ctrl, colnames(mat))
      cols_test <- intersect(cols_test, colnames(mat))

      if (length(cols_ctrl) > 0 && length(cols_test) > 0) {
        mean_ctrl <- rowMeans(mat[, cols_ctrl, drop = FALSE], na.rm = TRUE)
        mean_test <- rowMeans(mat[, cols_test, drop = FALSE], na.rm = TRUE)
        fc <- if (is_log_transformed) mean_test - mean_ctrl else log2(mean_test + 1) - log2(mean_ctrl + 1)
        names(fc) <- protein_ids
        comp_key <- paste0(tg, "/", control_group)
        fc_vectors[[comp_key]] <- fc
      }
    }
    fc_vectors
  }

  # Helper: compute Perseus score for a single FC vector against filtered terms
  compute_term_scores <- function(fc_vec) {
    valid_mask <- !is.na(fc_vec)
    valid_scores <- fc_vec[valid_mask]
    valid_pids <- names(fc_vec)[valid_mask]
    N <- length(valid_scores)

    if (N < 2) return(rep(NA_real_, length(matched_term_ids)))

    ranks <- rank(valid_scores, ties.method = "average")
    names(ranks) <- valid_pids

    scores <- rep(NA_real_, length(matched_term_ids))
    pvals <- rep(NA_real_, length(matched_term_ids))

    for (k in seq_along(matched_term_ids)) {
      tid <- matched_term_ids[k]
      tp <- unique(term_proteins[[tid]])
      tp_in <- intersect(tp, valid_pids)

      if (length(tp_in) < min_overlap) next

      ranks_in <- ranks[tp_in]
      other_pids <- setdiff(valid_pids, tp_in)
      ranks_out <- ranks[other_pids]

      if (length(ranks_out) < 2) next

      # Perseus-style score
      mean_rank_in <- mean(ranks_in, na.rm = TRUE)
      mean_rank_out <- mean(ranks_out, na.rm = TRUE)
      scores[k] <- 2 * (mean_rank_in - mean_rank_out) / N

      # Wilcoxon p-value
      pvals[k] <- tryCatch({
        wt <- suppressWarnings(wilcox.test(ranks_in, ranks_out,
                                           alternative = "two.sided", exact = FALSE))
        wt$p.value
      }, error = function(e) NA_real_)
    }

    attr(scores, "pvals") <- pvals
    scores
  }

  # --- SINGLE DATASET MODE ---
  if (!is_multi) {
    mat <- payload$mat
    samples <- payload$samples
    ids <- payload$ids

    if (is.null(mat) || is.null(samples)) {
      return(make_error("Payload missing mat or samples"))
    }

    # Get protein IDs
    protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
    protein_ids <- if (nzchar(protein_col) && !is.null(ids) && protein_col %in% names(ids)) {
      as.character(ids[[protein_col]])
    } else {
      rownames(mat)
    }

    # Detect control
    control_group <- if (exists("detect_control_group", mode = "function")) {
      detect_control_group(payload$metadata$groups)
    } else {
      detect_control_from_samples(samples)
    }

    if (is.null(control_group)) {
      return(make_error("No control group found. Mark one group as control in the experimental design."))
    }

    add_log("INFO", sprintf("Control group: %s", control_group))

    fc_vectors <- compute_fc_vectors(mat, samples, protein_ids, control_group)

    if (length(fc_vectors) == 0) {
      return(make_error("No valid comparisons could be computed (check group assignments)"))
    }

    add_log("INFO", sprintf("Computing scores for %d comparisons", length(fc_vectors)))

  } else {
    # --- MULTI DATASET MODE ---
    mat_a <- payload$mat_a
    mat_b <- payload$mat_b
    samples_a <- payload$samples_a
    samples_b <- payload$samples_b
    ds_a_name <- payload$metadata$dataset_a_name %||% "Dataset A"
    ds_b_name <- payload$metadata$dataset_b_name %||% "Dataset B"

    # Protein IDs (aligned across datasets)
    protein_ids <- payload$aligned_proteins %||% rownames(mat_a)

    # Detect controls per dataset
    ctrl_a <- detect_control_from_samples(samples_a)
    ctrl_b <- detect_control_from_samples(samples_b)

    fc_vectors <- list()

    if (!is.null(ctrl_a)) {
      add_log("INFO", sprintf("%s control: %s", ds_a_name, ctrl_a))
      fc_a <- compute_fc_vectors(mat_a, samples_a, protein_ids, ctrl_a)
      # Prefix with dataset name
      names(fc_a) <- paste0(ds_a_name, ": ", names(fc_a))
      fc_vectors <- c(fc_vectors, fc_a)
    } else {
      add_log("WARN", sprintf("%s has no control group defined — skipping", ds_a_name))
    }

    if (!is.null(ctrl_b)) {
      add_log("INFO", sprintf("%s control: %s", ds_b_name, ctrl_b))
      fc_b <- compute_fc_vectors(mat_b, samples_b, protein_ids, ctrl_b)
      names(fc_b) <- paste0(ds_b_name, ": ", names(fc_b))
      fc_vectors <- c(fc_vectors, fc_b)
    } else {
      add_log("WARN", sprintf("%s has no control group defined — skipping", ds_b_name))
    }

    if (length(fc_vectors) == 0) {
      return(make_error("No control groups found in either dataset. Mark one group as control per dataset."))
    }

    add_log("INFO", sprintf("Multi-dataset: %d total comparisons across datasets", length(fc_vectors)))
  }

  # =========================================================
  # 7. BUILD SCORE MATRIX
  # =========================================================
  comparison_names <- names(fc_vectors)
  n_terms <- length(matched_term_ids)
  n_comps <- length(comparison_names)

  mat_score <- matrix(NA_real_, nrow = n_terms, ncol = n_comps)
  rownames(mat_score) <- matched_term_ids
  colnames(mat_score) <- comparison_names

  mat_pval <- matrix(NA_real_, nrow = n_terms, ncol = n_comps)
  rownames(mat_pval) <- matched_term_ids
  colnames(mat_pval) <- comparison_names

  for (ci in seq_along(fc_vectors)) {
    result <- compute_term_scores(fc_vectors[[ci]])
    mat_score[, ci] <- result
    mat_pval[, ci] <- attr(result, "pvals")
  }

  # FDR correction per column
  mat_fdr <- mat_pval
  for (ci in seq_len(ncol(mat_pval))) {
    pvals_col <- mat_pval[, ci]
    valid <- !is.na(pvals_col)
    if (any(valid)) {
      mat_fdr[valid, ci] <- stats::p.adjust(pvals_col[valid], method = "BH")
    }
  }

  add_log("INFO", sprintf("Score matrix: %d terms x %d comparisons", n_terms, n_comps))

  scored_count <- sum(!is.na(mat_score))
  add_log("INFO", sprintf("Scored cells: %d / %d (%.0f%%)",
                           scored_count, n_terms * n_comps,
                           100 * scored_count / max(1, n_terms * n_comps)))

  # =========================================================
  # 8. Z-SCORE AND DENDROGRAMS
  # =========================================================
  mat_zscore <- t(apply(mat_score, 1, function(x) {
    if (all(is.na(x))) return(x)
    mu <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) { y <- x; y[!is.na(y)] <- 0; return(y) }
    (x - mu) / s
  }))
  rownames(mat_zscore) <- matched_term_ids
  colnames(mat_zscore) <- comparison_names

  # Dendrograms
  dendro_zscore <- NULL
  dendro_score <- NULL

  if (n_terms >= 2) {
    # Z-score dendrogram (correlation-based)
    clust_rows <- which(rowSums(!is.na(mat_zscore)) >= 2)
    if (length(clust_rows) >= 2) {
      m_sub <- mat_zscore[clust_rows, , drop = FALSE]
      cmat <- suppressWarnings(stats::cor(t(m_sub), use = "pairwise.complete.obs"))
      cmat[is.na(cmat)] <- 0
      diag(cmat) <- 1
      d <- stats::as.dist(1 - cmat)
      dendro_zscore <- tryCatch(stats::hclust(d, method = "average"), error = function(e) NULL)
      if (!is.null(dendro_zscore) && is.null(dendro_zscore$labels)) {
        dendro_zscore$labels <- rownames(m_sub)
      }
    }

    # Score dendrogram (Euclidean)
    complete_rows <- which(rowSums(is.na(mat_score)) == 0)
    if (length(complete_rows) >= 2) {
      m_sub <- mat_score[complete_rows, , drop = FALSE]
      d <- stats::dist(m_sub)
      dendro_score <- tryCatch(stats::hclust(d, method = "ward.D2"), error = function(e) NULL)
      if (!is.null(dendro_score) && is.null(dendro_score$labels)) {
        dendro_score$labels <- rownames(m_sub)
      }
    }
  }

  # =========================================================
  # 9. RETURN
  # =========================================================
  log_df <- if (length(log_entries) > 0) {
    do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(time = character(0), level = character(0), message = character(0),
               stringsAsFactors = FALSE)
  }

  list(
    engine_id = "pathway_fc_heatmap",
    params = params,
    data = list(
      mat_score = mat_score,
      mat_zscore = mat_zscore,
      mat_fdr = mat_fdr,
      dendro_zscore = dendro_zscore,
      dendro_score = dendro_score,
      term_order = matched_term_ids,
      term_info = term_info_df,
      comparison_order = comparison_names,
      matched_terms = matched_term_ids,
      unmatched_terms = unmatched_terms,
      log = log_df
    )
  )
}
