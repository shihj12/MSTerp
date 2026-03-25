# =========================================================
# R/engines/stats/2dgofcs.R — 2D GO Functional Class Scoring Engine
#
# Perseus-style 2D FCS on two named score vectors (proteinID).
# Uses rank-based scoring (Perseus method):
#   1. Convert raw scores to ranks (1 to N) for both dimensions
#   2. MANOVA test on ranks (Wilks' Lambda) instead of product of p-values
#   3. Score_x = 2 * (mean_rank_in_x - mean_rank_out_x) / N
#      Score_y = 2 * (mean_rank_in_y - mean_rank_out_y) / N
#      -> Normalized to [-1, 1] range for both dimensions
#
# Supports two modes:
#  1. Child of PCA: receives scores_x/scores_y from payload (PC loadings)
#  2. Standalone with 3+ groups: computes all pairwise FC vectors
#     and runs 2D FCS on each pair of comparisons
#
# Contract v1.2:
#  - data$analyses: list of 2D analyses, each containing terms
#  - data$terms: (backward compat) first analysis terms
#  - Output scores are in range [-1, 1] (rank-normalized)
#  - neglog10_fdr column added for visualization
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Compute fold-change vector for a group pair
#' @param mat Data matrix
#' @param samples Sample info
#' @param grp_a First group
#' @param grp_b Second group
#' @param protein_ids Protein ID vector
#' @param is_log_transformed If TRUE, data is already log-transformed
#' @return Named numeric vector of log2FC values
.compute_fc_vector <- function(mat, samples, grp_a, grp_b, protein_ids, is_log_transformed = FALSE) {
  cols_a <- samples$sample_col[samples$group_name == grp_a]
  cols_b <- samples$sample_col[samples$group_name == grp_b]
  cols_a <- intersect(cols_a, colnames(mat))
  cols_b <- intersect(cols_b, colnames(mat))

  if (length(cols_a) == 0 || length(cols_b) == 0) {
    return(NULL)
  }

  mat_a <- mat[, cols_a, drop = FALSE]
  mat_b <- mat[, cols_b, drop = FALSE]

  mean_a <- rowMeans(mat_a, na.rm = TRUE)
  mean_b <- rowMeans(mat_b, na.rm = TRUE)

  # log2 fold change (B vs A)
  fc <- if (is_log_transformed) mean_b - mean_a else log2(mean_b + 1) - log2(mean_a + 1)
  names(fc) <- protein_ids

  fc
}

#' Run 2D GO-FCS on a pair of score vectors (Perseus-style rank-based)
#' @param scores_x First score vector (named)
#' @param scores_y Second score vector (named)
#' @param term_proteins List of term -> protein mappings
#' @param term_info Term info data.frame (works for both GO and complexbase)
#' @param ontology Ontology filter
#' @param fdr_cutoff FDR threshold
#' @param min_term_size Minimum term size
#' @param max_terms Maximum terms to return
#' @param add_log Logging function
#' @return data.frame of enriched terms with rank-normalized scores
.run_2d_fcs <- function(scores_x, scores_y, term_proteins, term_info,
                        ontology, fdr_cutoff, min_term_size, max_terms,
                        add_log) {

  # Get common proteins
  common_pids <- intersect(names(scores_x), names(scores_y))

  if (length(common_pids) == 0) {
    add_log("WARN", "No common proteins between score vectors")
    return(data.frame(
      term_id = character(0),
      term_name = character(0),
      ontology = character(0),
      fdr = numeric(0),
      neglog10_fdr = numeric(0),
      score_x = numeric(0),
      score_y = numeric(0),
      protein_ids = character(0),
      n_genes = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Extract aligned score vectors for common proteins
  vals_x <- as.numeric(scores_x[common_pids])
  vals_y <- as.numeric(scores_y[common_pids])

  # Filter out proteins with NA in either dimension
  valid_mask <- !is.na(vals_x) & !is.na(vals_y)
  valid_pids <- common_pids[valid_mask]
  valid_x <- vals_x[valid_mask]
  valid_y <- vals_y[valid_mask]

  N <- length(valid_pids)
  if (N < 2 * min_term_size) {
    add_log("WARN", sprintf("Insufficient valid proteins (%d) for 2D analysis", N))
    return(data.frame(
      term_id = character(0),
      term_name = character(0),
      ontology = character(0),
      fdr = numeric(0),
      neglog10_fdr = numeric(0),
      score_x = numeric(0),
      score_y = numeric(0),
      protein_ids = character(0),
      n_genes = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Convert scores to ranks (Perseus-style, done once before term loop)
  ranks_x <- rank(valid_x, ties.method = "average")
  ranks_y <- rank(valid_y, ties.method = "average")
  names(ranks_x) <- valid_pids
  names(ranks_y) <- valid_pids

  # Build rank matrix for MANOVA (rows = proteins, cols = dimensions)
  R <- cbind(ranks_x, ranks_y)

  add_log("INFO", sprintf("Rank conversion: %d proteins, X range [%.1f, %.1f], Y range [%.1f, %.1f]",
                          N, min(ranks_x), max(ranks_x), min(ranks_y), max(ranks_y)))

  # NOTE: As of v1.5, GO engines always compute ALL ontologies (BP, CC, MF).
  # The params$ontology setting is now deprecated in favor of style$ontology_filter
  # which filters at render/viewer time. This allows users to change ontology filter
  # without re-running the analysis.
  filter_ontology <- NULL

  # 2D FCS (Perseus-style): For each term, MANOVA on ranks + rank-based scores
  results <- list()

  for (term_id in names(term_proteins)) {
    term_pids <- unique(term_proteins[[term_id]])
    term_pids_in_data <- intersect(term_pids, valid_pids)

    if (length(term_pids_in_data) < min_term_size) next

    # Get term info
    term_name <- term_id
    term_ont <- "unknown"

    if (!is.null(term_info)) {
      if (is.data.frame(term_info)) {
        # Works for both complexbase and GO
        term_row <- term_info[term_info$term_id == term_id |
                              (if ("go_id" %in% names(term_info)) term_info$go_id == term_id else FALSE), , drop = FALSE]
        if (nrow(term_row) > 0) {
          name_col <- intersect(
            c("term_name", "name", "description"), names(term_row))[1]
          ont_col <- intersect(
            c("ontology", "namespace", "ont"), names(term_row))[1]
          if (!is.na(name_col)) {
            term_name <- as.character(term_row[[name_col]][1])
          }
          if (!is.na(ont_col)) {
            term_ont <- as.character(term_row[[ont_col]][1])
          }
        }
      } else if (is.list(term_info) && !is.null(term_info[[term_id]])) {
        ti <- term_info[[term_id]]
        term_name <- ti$name %||% ti$term_name %||% term_id
        term_ont <- ti$ontology %||% ti$namespace %||% "unknown"
      }
    }

    # Filter by ontology
    if (!is.null(filter_ontology)) {
      if (!grepl(filter_ontology, term_ont, ignore.case = TRUE)) next
    }

    # Get indices for term proteins vs others
    in_term <- valid_pids %in% term_pids_in_data
    other_pids <- setdiff(valid_pids, term_pids_in_data)

    # Edge case: need enough proteins in "out" group
    if (length(other_pids) < min_term_size) next

    # MANOVA test on ranks (Perseus-style, Wilks' Lambda)
    pval <- tryCatch({
      group <- factor(ifelse(in_term, "in", "out"))
      fit <- manova(R ~ group)
      summ <- summary(fit, test = "Wilks")
      summ$stats[1, "Pr(>F)"]
    }, error = function(e) {
      # Fallback: product of Wilcoxon tests if MANOVA fails
      # (can happen with very small groups or collinearity)
      pval_x <- tryCatch({
        wt <- suppressWarnings(wilcox.test(ranks_x[in_term], ranks_x[!in_term],
                                            alternative = "two.sided", exact = FALSE))
        wt$p.value
      }, error = function(e2) 1)
      pval_y <- tryCatch({
        wt <- suppressWarnings(wilcox.test(ranks_y[in_term], ranks_y[!in_term],
                                            alternative = "two.sided", exact = FALSE))
        wt$p.value
      }, error = function(e2) 1)
      pval_x * pval_y
    })

    if (is.na(pval) || is.null(pval)) pval <- 1

    # Perseus-style scores: 2 * (mean_rank_in - mean_rank_out) / N
    # Normalized to [-1, 1] range
    mean_rank_in_x <- mean(ranks_x[in_term], na.rm = TRUE)
    mean_rank_out_x <- mean(ranks_x[!in_term], na.rm = TRUE)
    score_x <- 2 * (mean_rank_in_x - mean_rank_out_x) / N

    mean_rank_in_y <- mean(ranks_y[in_term], na.rm = TRUE)
    mean_rank_out_y <- mean(ranks_y[!in_term], na.rm = TRUE)
    score_y <- 2 * (mean_rank_in_y - mean_rank_out_y) / N

    results <- c(results, list(list(
      term_id = term_id,
      term_name = term_name,
      ontology = term_ont,
      pval = pval,
      score_x = score_x,
      score_y = score_y,
      protein_ids = paste(sort(term_pids_in_data), collapse = ";"),
      n_genes = length(term_pids_in_data)
    )))
  }

  if (length(results) == 0) {
    return(data.frame(
      term_id = character(0),
      term_name = character(0),
      ontology = character(0),
      fdr = numeric(0),
      neglog10_fdr = numeric(0),
      score_x = numeric(0),
      score_y = numeric(0),
      protein_ids = character(0),
      n_genes = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Build data.frame via direct vector extraction (memory-efficient)
  terms_df <- data.frame(
    term_id = vapply(results, `[[`, "", "term_id"),
    term_name = vapply(results, `[[`, "", "term_name"),
    ontology = vapply(results, `[[`, "", "ontology"),
    pval = vapply(results, `[[`, 0, "pval"),
    score_x = vapply(results, `[[`, 0, "score_x"),
    score_y = vapply(results, `[[`, 0, "score_y"),
    protein_ids = vapply(results, `[[`, "", "protein_ids"),
    n_genes = vapply(results, `[[`, 0L, "n_genes"),
    stringsAsFactors = FALSE
  )

  # Compute FDR
  terms_df$fdr <- p.adjust(terms_df$pval, method = "BH")

  # Add neglog10_fdr for visualization
  terms_df$neglog10_fdr <- -log10(pmax(terms_df$fdr, 1e-300))

  # Filter and sort
  terms_df <- terms_df[terms_df$fdr <= fdr_cutoff, , drop = FALSE]

  # Sort by distance from origin (in rank-normalized score space)
  terms_df$distance <- sqrt(terms_df$score_x^2 + terms_df$score_y^2)
  terms_df <- terms_df[order(terms_df$fdr, -terms_df$distance), , drop = FALSE]

  # Limit to max_terms PER ONTOLOGY (not globally)
  if ("ontology" %in% names(terms_df) && nrow(terms_df) > 0) {
    ontologies <- unique(terms_df$ontology)
    terms_list <- lapply(ontologies, function(ont) {
      ont_df <- terms_df[terms_df$ontology == ont, , drop = FALSE]
      if (nrow(ont_df) > max_terms) {
        ont_df <- ont_df[1:max_terms, , drop = FALSE]
      }
      ont_df
    })
    terms_df <- do.call(rbind, terms_list)
    terms_df <- terms_df[order(terms_df$fdr, -terms_df$distance), , drop = FALSE]
  } else if (nrow(terms_df) > max_terms) {
    terms_df <- terms_df[1:max_terms, , drop = FALSE]
  }

  terms_df <- terms_df[, c("term_id", "term_name", "ontology", "fdr", "neglog10_fdr",
                           "score_x", "score_y", "protein_ids", "n_genes"), drop = FALSE]
  rownames(terms_df) <- NULL

  terms_df
}

#' Execute 2dgofcs engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with terpbase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_2dgofcs_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "2dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          neglog10_fdr = numeric(0),
          score_x = numeric(0),
          score_y = numeric(0),
          protein_ids = character(0),
          n_genes = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", "Initiating 2D GO-FCS analysis")

  # =========================================================
  # Detect multi-dataset mode
  # =========================================================
  is_multi_dataset <- !is.null(payload$mode) && payload$mode == "multi_dataset"

  if (is_multi_dataset) {
    add_log("INFO", "Multi-dataset mode detected - using cross-dataset scores")
  }

  # Detect data type for database selection (ensure scalar)
  data_type <- as.character(payload$data_type %||% payload$metadata$data_type %||% context$data_type %||% "proteomics")[1]
  add_log("INFO", sprintf("Data type: %s", data_type))

  # Get parameters (ensure scalar)
  database <- as.character(params$database %||% "go")[1]
  ontology <- as.character(params$ontology %||% "all")[1]
  fdr_cutoff <- as.numeric(params$fdr_cutoff %||% 0.03)[1]
  min_term_size <- as.integer(params$min_term_size %||% 5)[1]
  max_terms <- as.integer(params$max_terms %||% 20)[1]
  is_log_transformed <- isTRUE(params$is_log_transformed)

  add_log("INFO", sprintf("Database mode: %s", database))
  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, ontology=%s",
                          fdr_cutoff, min_term_size, ontology))

  # Guard: protein complex enrichment is not applicable to metabolomics data
  if (identical(database, "complex") && identical(data_type, "metabolomics")) {
    add_log("ERROR", "Protein complex enrichment is not applicable to metabolomics data")
    return(list(
      engine_id = "2dgofcs",
      params = params,
      data = list(
        terms = data.frame(term_id = character(0), term_name = character(0),
                           ontology = character(0), fdr = numeric(0),
                           mean_x = numeric(0), mean_y = numeric(0),
                           n_genes = integer(0), stringsAsFactors = FALSE),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "Protein complex enrichment is not applicable to metabolomics data.",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # =========================================================
  # Score extraction (multi-dataset, grandchild, or standalone)
  # =========================================================

  # Default axis labels
  x_score_label <- NULL
  y_score_label <- NULL
  protein_ids <- NULL

  if (is_multi_dataset) {
    # Multi-dataset mode: use pre-computed axis values from payload
    scores_x <- payload$scores_x
    scores_y <- payload$scores_y

    if (is.null(scores_x) || is.null(scores_y)) {
      add_log("ERROR", "Multi-dataset mode requires scores_x and scores_y in payload")
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Missing axis values - configure axes in New Run",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Get axis labels from payload metadata
    x_score_label <- payload$x_label %||% payload$metadata$dataset_a_name %||% "Dataset A"
    y_score_label <- payload$y_label %||% payload$metadata$dataset_b_name %||% "Dataset B"

    add_log("INFO", sprintf("X-axis: %s (%d scores)", x_score_label, length(scores_x)))
    add_log("INFO", sprintf("Y-axis: %s (%d scores)", y_score_label, length(scores_y)))

    # Get protein IDs from aligned_proteins or names intersection
    protein_ids <- payload$aligned_proteins
    if (is.null(protein_ids)) {
      protein_ids <- intersect(names(scores_x), names(scores_y))
    }

    add_log("INFO", sprintf("Aligned proteins: %d", length(protein_ids)))

    # Not standalone mode - we have pre-computed scores
    standalone_mode <- FALSE

  } else {
    # Check for score vectors from payload, params, or context (grandchild mode)
    # Priority: payload > params > context (matches 1dgofcs pattern)
    scores_x <- payload$scores_x %||% params$scores_x %||% context$scores_x %||% NULL
    scores_y <- payload$scores_y %||% params$scores_y %||% context$scores_y %||% NULL

    # Extract axis labels from payload (for PCA-paired mode)
    x_score_label <- payload$x_score_label %||% params$x_score_label %||% NULL
    y_score_label <- payload$y_score_label %||% params$y_score_label %||% NULL

    # If no score vectors provided, try standalone mode with multiple groups
    standalone_mode <- is.null(scores_x) || is.null(scores_y)
  }

  if (standalone_mode) {
    # Standalone mode: compute FC vectors from raw data
    mat <- payload$mat
    samples <- payload$samples
    groups <- payload$groups
    ids <- payload$ids

    if (length(groups) < 3) {
      add_log("WARN", sprintf(
        paste0("Standalone 2D GO-FCS requires 3+ groups (found %d). ",
               "Use as PCA grandchild for 2-group data."), length(groups)))
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          analyses = list(),
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "WARN",
                           message = "Standalone 2D GO-FCS requires 3+ groups",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    add_log("INFO", sprintf("Standalone mode: %d groups detected", length(groups)))

    # Get protein IDs
    protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
    protein_ids <- if (nzchar(protein_col) && protein_col %in% names(ids)) {
      as.character(ids[[protein_col]])
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

    # Match is_control to our groups vector (in case order differs)
    is_control_map <- rep(FALSE, length(groups))
    for (idx in seq_along(groups)) {
      match_idx <- match(groups[idx], group_names)
      if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
    }

    control_only <- isTRUE(params$control_only)
    control_idx <- which(is_control_map)
    has_control <- length(control_idx) == 1

    # Compute all pairwise FC vectors (with control_only filtering)
    n_groups <- length(groups)
    fc_vectors <- list()

    if (control_only && has_control) {
      add_log("INFO", sprintf("Control-only mode: computing FC vectors against control (%s)",
                              groups[control_idx]))
    } else {
      add_log("INFO", "Computing pairwise fold-change vectors...")
    }

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
        # FC = log2(grp_b/grp_a), so positive FC means higher in grp_b
        # Only swap when control is at j position (grp_b) and i is not control
        if (has_control && j %in% control_idx && !(i %in% control_idx)) {
          tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
        }
        # If i is control, grp_a is already control (baseline) - no swap needed

        comp_key <- paste0(grp_b, "_vs_", grp_a)

        fc <- .compute_fc_vector(mat, samples, grp_a, grp_b, protein_ids, is_log_transformed)
        if (!is.null(fc)) {
          fc_vectors[[comp_key]] <- fc
          add_log("INFO", sprintf("  %s: computed", comp_key))
        }
      }
    }

    if (length(fc_vectors) < 2) {
      add_log("ERROR", "Need at least 2 valid FC comparisons for 2D analysis")
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          analyses = list(),
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = do.call(rbind, lapply(log_entries, as.data.frame,
                                       stringsAsFactors = FALSE))
        )
      ))
    }
  }

  # Source shared enrichment utilities
  if (!exists("build_term_proteins", mode = "function")) {
    source("R/engines/enrichment_utils.R", local = TRUE)
  }

  # Initialize term_proteins and term_info based on database mode
  term_proteins <- list()
  term_info <- NULL

  # Allow callers to pre-build term_proteins (e.g., cluster loop optimization)
  if (!is.null(payload$term_proteins) && length(payload$term_proteins) > 0) {
    term_proteins <- payload$term_proteins
    term_info <- payload$term_info %||% NULL
    add_log("INFO", sprintf("Using pre-built term_proteins (%d terms)", length(term_proteins)))
  } else if (identical(database, "complex")) {
    # =========================================================
    # Complex enrichment mode: use complexbase
    # =========================================================
    complexbase <- payload$complexbase %||% context$complexbase

    if (is.null(complexbase)) {
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Complex enrichment requires complexbase",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Source complexbase utilities if not already loaded
    if (!exists("complexbase_build_term_proteins", mode = "function")) {
      source("R/engines/complexbase.R", local = TRUE)
    }

    # Use pre-computed mapping if available, otherwise build on demand
    term_proteins <- if (!is.null(complexbase$term_proteins)) {
      complexbase$term_proteins
    } else {
      complexbase_build_term_proteins(complexbase)
    }
    term_info <- complexbase_get_term_info(complexbase)

    add_log("INFO", sprintf("Loaded %d protein complex mappings", length(term_proteins)))

  } else if (identical(data_type, "metabolomics")) {
    # =========================================================
    # KEGG pathway enrichment mode: use metabobase
    # =========================================================
    metabobase <- payload$metabobase %||% context$metabobase

    if (is.null(metabobase)) {
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Metabolomics enrichment requires metabobase",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Extract KEGG pathway mappings from metabobase
    compound_to_pathway <- metabobase$compound_to_pathway %||% metabobase$compound_terms %||% NULL
    pathway_info <- metabobase$pathway_info %||% metabobase$terms %||% NULL

    # Use pre-computed mapping if available, otherwise build on demand
    if (!is.null(metabobase$term_proteins)) {
      term_proteins <- metabobase$term_proteins
    } else if (!is.null(compound_to_pathway)) {
      term_proteins <- build_term_proteins(compound_to_pathway)
    }

    term_info <- pathway_info
    add_log("INFO", sprintf("Loaded %d KEGG pathway mappings", length(term_proteins)))

  } else {
    # =========================================================
    # GO enrichment mode: use terpbase (default)
    # =========================================================
    terpbase <- payload$terpbase %||% context$terpbase

    if (is.null(terpbase)) {
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Proteomics enrichment requires terpbase with GO annotations",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Extract GO mappings (same logic as goora/1dgofcs)
    protein_to_go <- terpbase$protein_to_go %||% terpbase$protein_terms %||% NULL
    go_terms <- terpbase$go_terms %||% terpbase$terms %||% NULL

    if (is.null(protein_to_go)) {
      annotations <- terpbase$annotations %||% NULL
      if (!is.null(annotations) && "go_ids" %in% names(annotations)) {
        protein_to_go <- annotations
      }
    }

    if (is.null(protein_to_go)) {
      return(list(
        engine_id = "2dgofcs",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            neglog10_fdr = numeric(0),
            score_x = numeric(0),
            score_y = numeric(0),
            protein_ids = character(0),
            n_genes = integer(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Terpbase missing protein_to_go mapping",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Use pre-computed mapping from pipeline context if available,
    # otherwise build on demand (e.g. tool-based enrichment).
    term_proteins <- if (!is.null(terpbase$term_proteins)) {
      terpbase$term_proteins
    } else {
      build_term_proteins(protein_to_go)
    }

    # Store go_terms for term info lookup
    term_info <- go_terms

    add_log("INFO", sprintf("Loaded %d GO term mappings", length(term_proteins)))
  }

  if (length(term_proteins) == 0) {
    return(list(
      engine_id = "2dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          neglog10_fdr = numeric(0),
          score_x = numeric(0),
          score_y = numeric(0),
          protein_ids = character(0),
          n_genes = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = sprintf("No valid %s term mappings found",
                                           if (identical(database, "complex")) "complex" else "GO"),
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # =========================================================
  # Run 2D FCS analyses
  # =========================================================

  analyses <- list()
  all_terms <- list()

  if (standalone_mode) {
    # Standalone mode: run 2D FCS on all pairs of FC vectors
    fc_keys <- names(fc_vectors)
    n_fc <- length(fc_keys)
    n_pairs <- n_fc * (n_fc - 1) / 2

    add_log("INFO", sprintf("Running 2D GO-FCS on %d FC vector pairs", n_pairs))

    for (i in seq_len(n_fc - 1)) {
      for (j in (i + 1):n_fc) {
        key_x <- fc_keys[i]
        key_y <- fc_keys[j]
        pair_label <- paste0(key_x, "_x_", key_y)

        add_log("INFO", sprintf("  Analyzing: %s", pair_label))

        terms_df <- .run_2d_fcs(
          scores_x = fc_vectors[[key_x]],
          scores_y = fc_vectors[[key_y]],
          term_proteins = term_proteins,
          term_info = term_info,
          ontology = ontology,
          fdr_cutoff = fdr_cutoff,
          min_term_size = min_term_size,
          max_terms = max_terms,
          add_log = add_log
        )

        analyses[[pair_label]] <- list(
          x_comparison = key_x,
          y_comparison = key_y,
          terms = terms_df,
          n_terms = nrow(terms_df)
        )

        add_log("INFO", sprintf("    Found %d enriched terms", nrow(terms_df)))

        # Collect all terms
        if (nrow(terms_df) > 0) {
          terms_df$analysis <- pair_label
          all_terms <- c(all_terms, list(terms_df))
        }
      }
    }

  } else {
    # Single analysis mode: multi-dataset or grandchild (PCA-paired)
    if (is_multi_dataset) {
      add_log("INFO", "Running 2D GO-FCS on multi-dataset cross-comparison")
    } else {
      add_log("INFO", "Running single 2D GO-FCS analysis (grandchild mode)")
    }

    terms_df <- .run_2d_fcs(
      scores_x = scores_x,
      scores_y = scores_y,
      term_proteins = term_proteins,
      term_info = term_info,
      ontology = ontology,
      fdr_cutoff = fdr_cutoff,
      min_term_size = min_term_size,
      max_terms = max_terms,
      add_log = add_log
    )

    # Use axis labels from payload if available
    # Multi-dataset: dataset names, PCA-paired: "PC1 Top N", else generic
    analysis_key <- if (is_multi_dataset) "multi_dataset" else "default"
    analyses[[analysis_key]] <- list(
      x_comparison = x_score_label %||% "Score X",
      y_comparison = y_score_label %||% "Score Y",
      terms = terms_df,
      n_terms = nrow(terms_df),
      metadata = if (is_multi_dataset) list(
        mode = "multi_dataset",
        dataset_a = payload$metadata$dataset_a_name,
        dataset_b = payload$metadata$dataset_b_name,
        n_proteins = length(protein_ids)
      ) else NULL
    )

    if (nrow(terms_df) > 0) {
      all_terms <- list(terms_df)
    }

    add_log("INFO", sprintf("Found %d enriched terms", nrow(terms_df)))
  }

  # Combine all terms for backward compatibility
  combined_terms <- if (length(all_terms) > 0) {
    do.call(rbind, all_terms)
  } else {
    data.frame(
      term_id = character(0),
      term_name = character(0),
      ontology = character(0),
      fdr = numeric(0),
      neglog10_fdr = numeric(0),
      score_x = numeric(0),
      score_y = numeric(0),
      protein_ids = character(0),
      n_genes = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # Build summary
  summary_df <- data.frame(
    analysis = names(analyses),
    x_comparison = vapply(analyses, function(a) a$x_comparison, character(1)),
    y_comparison = vapply(analyses, function(a) a$y_comparison, character(1)),
    n_terms = vapply(analyses, function(a) a$n_terms, integer(1)),
    stringsAsFactors = FALSE
  )

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("2D GO-FCS completed in %.2f seconds (%d analyses, %d total terms)",
                          engine_duration, length(analyses), nrow(combined_terms)))

  # Build log data.frame
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  # Build output data with optional axis labels and multi-dataset metadata
  output_data <- list(
    analyses = analyses,
    terms = combined_terms,  # Backward compatibility
    summary = summary_df,
    log = log_df
  )

  # Include axis labels if provided (e.g., "PC1 Top 1234" for PCA-paired mode)
  if (!is.null(x_score_label) && nzchar(x_score_label)) {
    output_data$x_score_label <- x_score_label
  }
  if (!is.null(y_score_label) && nzchar(y_score_label)) {
    output_data$y_score_label <- y_score_label
  }

  # Include multi-dataset metadata for downstream use
  if (is_multi_dataset) {
    output_data$mode <- "multi_dataset"
    output_data$metadata <- list(
      mode = "multi_dataset",
      dataset_a = payload$metadata$dataset_a_name,
      dataset_b = payload$metadata$dataset_b_name,
      x_label = x_score_label,
      y_label = y_score_label
    )
  }

  list(
    engine_id = "2dgofcs",
    params = params,
    data = output_data
  )
}
