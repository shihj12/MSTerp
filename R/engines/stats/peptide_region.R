# =========================================================
# R/engines/stats/peptide_region.R — Peptide Region Engine
#
# Overview mode: scores all proteins by peptide heterogeneity
# and FC disparity across multiple comparisons.
# Detail mode: per-residue coverage plot with UniProt annotations.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Helpers ---------------------------------------------------------------

#' Clean a modified peptide sequence for protein mapping
pr_clean_peptide_sequence <- function(mod_seq) {
  s <- gsub("^_|_$", "", mod_seq)
  s <- gsub("\\[.*?\\]", "", s)
  s <- gsub("[^A-Z]", "", s)
  s
}

#' Fetch protein data from UniProt REST API
pr_fetch_uniprot_data <- function(uniprot_id) {
  url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
  resp <- httr::GET(url, httr::add_headers(Accept = "application/json"))
  httr::stop_for_status(resp, task = paste("fetch UniProt entry for", uniprot_id))

  json_data <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  protein_seq    <- json_data$sequence$value
  protein_length <- json_data$sequence$length

  gene_symbol <- NULL
  if (!is.null(json_data$genes) && length(json_data$genes) > 0) {
    gene_symbol <- json_data$genes[[1]]$geneName$value
  }

  features_list <- json_data$features
  if (length(features_list) == 0) {
    features_df <- data.frame(
      type = character(), start = integer(), end = integer(),
      description = character(), stringsAsFactors = FALSE
    )
  } else {
    features_df <- do.call(rbind, lapply(features_list, function(ft) {
      data.frame(
        type        = ft$type %||% NA_character_,
        start       = ft$location$start$value %||% NA_integer_,
        end         = ft$location$end$value   %||% NA_integer_,
        description = ft$description %||% "",
        stringsAsFactors = FALSE
      )
    }))
  }

  list(
    sequence    = protein_seq,
    seq_length  = protein_length,
    features    = features_df,
    gene_symbol = gene_symbol
  )
}

#' Map cleaned peptide sequences to positions in a protein
pr_map_peptides <- function(clean_seqs, protein_sequence) {
  n <- length(clean_seqs)
  starts <- integer(n)
  ends   <- integer(n)
  valid  <- logical(n)

  for (i in seq_len(n)) {
    pos <- regexpr(clean_seqs[i], protein_sequence, fixed = TRUE)
    if (pos == -1L) {
      valid[i] <- FALSE
    } else {
      starts[i] <- as.integer(pos)
      ends[i]   <- starts[i] + nchar(clean_seqs[i]) - 1L
      valid[i]  <- TRUE
    }
  }

  list(starts = starts, ends = ends, valid = valid)
}

#' Resolve aggregation function by name
pr_agg_fn <- function(method) {
  switch(
    method %||% "harmonic_mean",
    "harmonic_mean" = function(x) {
      x <- x[x > 0 & !is.na(x)]
      if (length(x) == 0L) NA_real_ else length(x) / sum(1 / x)
    },
    "arithmetic_mean" = function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) NA_real_ else mean(x)
    },
    "median" = function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) NA_real_ else stats::median(x)
    },
    "sum" = function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) NA_real_ else sum(x)
    },
    function(x) {
      x <- x[x > 0 & !is.na(x)]
      if (length(x) == 0L) NA_real_ else length(x) / sum(1 / x)
    }
  )
}

#' Build per-residue coverage from peptide stats
pr_build_coverage <- function(pep_stats, protein_length, method = "harmonic_mean",
                              value_cols = c("ctrl_mean", "treatment_mean"),
                              compute_cv = FALSE, replicate_lists = NULL,
                              pvalue_col = NULL) {
  agg <- pr_agg_fn(method)

  per_res <- lapply(value_cols, function(col) vector("list", protein_length))
  names(per_res) <- value_cols

  if (compute_cv && !is.null(replicate_lists)) {
    rep_per_res <- lapply(names(replicate_lists), function(nm) vector("list", protein_length))
    names(rep_per_res) <- names(replicate_lists)
  }

  # Per-residue p-value accumulation
  pval_per_res <- if (!is.null(pvalue_col) && pvalue_col %in% names(pep_stats))
                    vector("list", protein_length)
                  else NULL

  for (i in seq_len(nrow(pep_stats))) {
    s <- pep_stats$prot_start[i]
    e <- pep_stats$prot_end[i]
    for (pos in s:e) {
      for (col in value_cols) {
        v <- pep_stats[[col]][i]
        if (!is.na(v)) per_res[[col]][[pos]] <- c(per_res[[col]][[pos]], v)
      }
      if (compute_cv && !is.null(replicate_lists)) {
        for (nm in names(replicate_lists)) {
          reps <- replicate_lists[[nm]][[i]]
          if (length(reps) > 0) rep_per_res[[nm]][[pos]] <- c(rep_per_res[[nm]][[pos]], reps)
        }
      }
      if (!is.null(pval_per_res)) {
        pv <- pep_stats[[pvalue_col]][i]
        if (!is.na(pv)) pval_per_res[[pos]] <- c(pval_per_res[[pos]], pv)
      }
    }
  }

  out <- data.frame(residue = seq_len(protein_length), stringsAsFactors = FALSE)
  for (col in value_cols) {
    out[[col]] <- vapply(per_res[[col]], agg, numeric(1))
  }
  out$depth <- vapply(per_res[[value_cols[1]]], function(x) sum(!is.na(x)), integer(1))

  if (all(c("ctrl_mean", "treatment_mean") %in% value_cols)) {
    out$fc <- out$treatment_mean / out$ctrl_mean
  }

  if (compute_cv && !is.null(replicate_lists)) {
    for (nm in names(rep_per_res)) {
      cv_col <- paste0(nm, "_cv")
      out[[cv_col]] <- vapply(rep_per_res[[nm]], function(x) {
        x <- x[!is.na(x)]
        if (length(x) < 2L) return(NA_real_)
        m <- mean(x); if (m == 0) NA_real_ else stats::sd(x) / abs(m)
      }, numeric(1))
    }
  }

  # Min-p per residue (most significant overlapping peptide)
  if (!is.null(pval_per_res)) {
    out$min_pvalue <- vapply(pval_per_res, function(x) {
      if (length(x) == 0) NA_real_ else min(x, na.rm = TRUE)
    }, numeric(1))
  }

  out
}

#' Safe coefficient of variation
pr_cv <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) return(NA_real_)
  m <- mean(x)
  if (m == 0) return(NA_real_)
  stats::sd(x) / abs(m)
}

#' Detect control group from metadata (same logic as volcano engine)
pr_detect_control <- function(groups, metadata) {
  meta_groups <- metadata$groups
  if (is.null(meta_groups) || is.null(meta_groups$is_control)) {
    return(list(has_control = FALSE, control_idx = integer(0)))
  }

  is_control <- tolower(as.character(meta_groups$is_control)) %in% c("true", "t", "1", "yes")
  group_names <- as.character(meta_groups$group_name %||% character(0))

  is_control_map <- rep(FALSE, length(groups))
  for (idx in seq_along(groups)) {
    match_idx <- match(groups[idx], group_names)
    if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
  }

  control_idx <- which(is_control_map)
  list(has_control = length(control_idx) == 1, control_idx = control_idx)
}

#' Build pairwise comparisons (control always as denominator)
pr_build_comparisons <- function(groups, samples, control_info, control_only) {
  n_groups <- length(groups)
  comps <- list()

  for (i in seq_len(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      if (control_only && control_info$has_control) {
        if (!i %in% control_info$control_idx && !j %in% control_info$control_idx) next
      }

      grp_a <- groups[i]
      grp_b <- groups[j]

      # Control should be denominator (grp_a)
      if (control_info$has_control && j %in% control_info$control_idx && !(i %in% control_info$control_idx)) {
        tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
      }

      comp_key <- paste0(grp_b, "_vs_", grp_a)
      comps[[comp_key]] <- list(
        ctrl_group      = grp_a,
        treatment_group = grp_b,
        ctrl_samples    = samples$sample_col[samples$group_name == grp_a],
        treatment_samples = samples$sample_col[samples$group_name == grp_b]
      )
    }
  }

  comps
}

#' Resolve target input: accepts both protein IDs and gene symbols
pr_resolve_targets <- function(target_strings, prot_to_rows, gene_to_prot) {
  resolved <- character(0)
  for (t in target_strings) {
    if (t %in% names(prot_to_rows)) {
      resolved <- c(resolved, t)
    } else if (t %in% names(gene_to_prot)) {
      resolved <- c(resolved, gene_to_prot[[t]])
    }
  }
  unique(resolved)
}

#' Biology category tag from GO terms
#'
#' Looks up the protein/gene in the supplied protein_to_go map, joins to
#' go_terms for human-readable names, then runs an ordered first-match pattern
#' classification. Falls back to "Other" when no GO data is available.
#'
#' @return list(tag = character(1), go_names = character(1))
pr_biology_tag <- function(gene_sym, protein_id, protein_to_go, go_terms,
                           max_terms = 15L) {
  result <- list(tag = "Other", go_names = "")

  if (length(protein_to_go) == 0L) return(result)

  go_ids <- protein_to_go[[gene_sym]]
  if (is.null(go_ids)) go_ids <- protein_to_go[[protein_id]]
  if (is.null(go_ids) || length(go_ids) == 0L) return(result)

  # Look up names if a go_terms table is available
  go_names <- character(0)
  if (!is.null(go_terms) && is.data.frame(go_terms) && nrow(go_terms) > 0L) {
    id_col <- if ("go_id" %in% names(go_terms)) "go_id"
              else if ("term_id" %in% names(go_terms)) "term_id"
              else if ("ID" %in% names(go_terms)) "ID"
              else NULL
    name_col <- if ("name" %in% names(go_terms)) "name"
                else if ("term_name" %in% names(go_terms)) "term_name"
                else if ("Description" %in% names(go_terms)) "Description"
                else NULL
    if (!is.null(id_col) && !is.null(name_col)) {
      idx <- match(go_ids, go_terms[[id_col]])
      go_names <- as.character(go_terms[[name_col]][idx[!is.na(idx)]])
      go_names <- go_names[!is.na(go_names) & nzchar(go_names)]
    }
  }

  # If no name lookup, fall back to bare GO IDs
  if (length(go_names) == 0L) go_names <- as.character(go_ids)
  go_names <- unique(go_names)

  result$go_names <- paste(utils::head(go_names, max_terms), collapse = "; ")

  # Ordered first-match classification
  combined <- tolower(paste(go_names, collapse = " | "))
  if (grepl("proteasom", combined)) {
    result$tag <- "Proteasome"
  } else if (grepl("integral component of membrane|transmembrane", combined)) {
    result$tag <- "Transmembrane"
  } else if (grepl("lysosom", combined)) {
    result$tag <- "Lysosomal"
  } else if (grepl("extracellular|secret", combined)) {
    result$tag <- "Secreted"
  } else if (grepl("mitochond", combined)) {
    result$tag <- "Mitochondrial"
  } else if (grepl("peptidase|protease|proteolysis", combined)) {
    result$tag <- "Protease"
  } else if (grepl("nucle", combined)) {
    result$tag <- "Nuclear"
  } else if (grepl("cytopl", combined)) {
    result$tag <- "Cytoplasmic"
  } else if (grepl("\\bmembrane\\b", combined)) {
    result$tag <- "Membrane"
  }

  result
}

#' Sanitize a comparison key into a valid R column suffix
pr_sanitize_comp_name <- function(x) {
  gsub("[^A-Za-z0-9_]+", "_", x)
}

# ---- Overview / Scoring Engine ---------------------------------------------

stats_peptide_region_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% list()

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries[[length(log_entries) + 1L]] <<- list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level, message = msg
    )
  }

  # ---- Identify columns ----
  id_primary_col <- as.character(payload$metadata$id_primary_col %||% "")[1]
  id_protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
  id_gene_col    <- as.character(payload$metadata$id_gene_col %||% "")[1]

  ids     <- payload$ids
  mat     <- payload$mat
  samples <- payload$samples

  # ---- Optional GO context for biology tagging ----
  terpbase_obj  <- context$terpbase %||% payload$terpbase %||% NULL
  protein_to_go <- terpbase_obj$protein_to_go %||% list()
  go_terms      <- terpbase_obj$go_terms      %||% NULL
  has_go        <- length(protein_to_go) > 0L
  if (has_go) {
    add_log("INFO", sprintf("GO annotation available (%d entries) — biology tags enabled",
                             length(protein_to_go)))
  } else {
    add_log("INFO", "No GO annotation available — biology tags will be 'Other'")
  }

  if (!nzchar(id_primary_col) || !id_primary_col %in% names(ids)) {
    stop("Peptide Region: peptide ID column '", id_primary_col, "' not found in data")
  }
  if (!nzchar(id_protein_col) || !id_protein_col %in% names(ids)) {
    stop("Peptide Region: protein ID column '", id_protein_col, "' not found in data")
  }

  # ---- Groups & comparisons ----
  groups   <- unique(samples$group_name)
  n_groups <- length(groups)
  if (n_groups < 1) stop("Peptide Region: no sample groups found")

  control_only <- isTRUE(params$control_only %||% TRUE)
  control_info <- pr_detect_control(groups, payload$metadata)

  if (n_groups >= 2) {
    comparisons_info <- pr_build_comparisons(groups, samples, control_info, control_only)
    add_log("INFO", sprintf("Generated %d comparison(s): %s",
                             length(comparisons_info),
                             paste(names(comparisons_info), collapse = ", ")))
  } else {
    comparisons_info <- list()
    add_log("INFO", sprintf("Single-group mode: %s (n=%d)",
                             groups[1], sum(samples$group_name == groups[1])))
  }
  is_single_group <- length(comparisons_info) == 0L

  aggregation_method <- params$aggregation_method %||% "harmonic_mean"
  min_peptides <- as.integer(params$min_peptides %||% 3L)

  # ---- Per-comparison column scaffolding (cap at 10) ----
  MAX_COMP_COLS <- 10L
  comp_keys_all <- names(comparisons_info)
  comp_keys_kept <- if (length(comp_keys_all) > MAX_COMP_COLS) {
    add_log("WARN", sprintf(
      "More than %d comparisons (%d total) — only the first %d will get per-comparison columns",
      MAX_COMP_COLS, length(comp_keys_all), MAX_COMP_COLS))
    comp_keys_all[seq_len(MAX_COMP_COLS)]
  } else {
    comp_keys_all
  }
  comp_col_suffix <- vapply(comp_keys_kept, pr_sanitize_comp_name, character(1))
  fc_mean_cols <- if (length(comp_col_suffix) > 0L) paste0("fc_mean_", comp_col_suffix) else character(0)
  fc_disp_cols <- if (length(comp_col_suffix) > 0L) paste0("fc_disp_", comp_col_suffix) else character(0)

  # ---- Build protein → row index mapping ----
  protein_col_values <- as.character(ids[[id_protein_col]])
  prot_to_rows <- list()
  for (i in seq_along(protein_col_values)) {
    prots <- trimws(unlist(strsplit(protein_col_values[i], ";")))
    for (p in prots) {
      if (nzchar(p)) prot_to_rows[[p]] <- c(prot_to_rows[[p]], i)
    }
  }

  add_log("INFO", sprintf("Found %d unique proteins across %d peptide rows",
                           length(prot_to_rows), nrow(mat)))

  # ---- Gene symbol lookup + reverse map ----
  has_gene_col <- nzchar(id_gene_col) && id_gene_col %in% names(ids)
  gene_to_prot <- list()

  gene_lookup <- function(protein_id) {
    if (!has_gene_col) return(protein_id)
    rows <- prot_to_rows[[protein_id]]
    if (is.null(rows) || length(rows) == 0) return(protein_id)
    genes <- unique(as.character(ids[[id_gene_col]][rows]))
    genes <- genes[nzchar(genes) & !is.na(genes)]
    if (length(genes) == 0) return(protein_id)
    # Build reverse map
    for (g in genes) gene_to_prot[[g]] <<- c(gene_to_prot[[g]], protein_id)
    genes[1]
  }

  # Pre-populate gene_to_prot
  for (pid in names(prot_to_rows)) gene_lookup(pid)
  # Deduplicate reverse map
  gene_to_prot <- lapply(gene_to_prot, unique)

  # ---- Resolve target list (protein IDs + gene symbols) ----
  raw_targets <- params$target_proteins %||% ""
  target_strings <- trimws(unlist(strsplit(raw_targets, "[,;\n\r]+")))
  target_strings <- target_strings[nzchar(target_strings)]
  target_strings <- unique(target_strings)

  resolved_targets <- if (length(target_strings) > 0) {
    pr_resolve_targets(target_strings, prot_to_rows, gene_to_prot)
  } else { character(0) }

  if (length(target_strings) > 0) {
    add_log("INFO", sprintf("Resolved %d target(s) from %d input(s): %s",
                             length(resolved_targets), length(target_strings),
                             paste(resolved_targets, collapse = ", ")))
  }

  # ---- Score each protein (across all comparisons) ----
  score_rows <- list()
  peptide_index <- list()

  for (prot_id in names(prot_to_rows)) {
    rows <- prot_to_rows[[prot_id]]
    n_pep <- length(rows)
    if (n_pep < min_peptides) next

    peptide_index[[prot_id]] <- rows
    gene_sym <- gene_lookup(prot_id)

    pep_seqs <- as.character(ids[[id_primary_col]][rows])
    clean_seqs <- vapply(pep_seqs, pr_clean_peptide_sequence, character(1), USE.NAMES = FALSE)
    n_unique <- length(unique(clean_seqs))

    # Per-peptide means for first group (always present)
    first_group <- groups[1]
    first_samples <- samples$sample_col[samples$group_name == first_group]
    first_mat <- mat[rows, first_samples, drop = FALSE]
    pep_first_means <- rowMeans(first_mat, na.rm = TRUE)
    pep_first_means[is.nan(pep_first_means)] <- NA_real_

    peptide_cv <- pr_cv(pep_first_means[!is.na(pep_first_means)])

    rep_cvs <- apply(first_mat, 1, function(x) {
      x <- x[!is.na(x)]
      if (length(x) < 2) return(NA_real_)
      m <- mean(x); if (m == 0) NA_real_ else stats::sd(x) / abs(m)
    })
    mean_rep_cv <- mean(rep_cvs, na.rm = TRUE)

    # Score across all comparisons
    best_fc_disparity <- NA_real_
    max_abs_fc <- NA_real_

    # Per-comparison value buffers (NA when no data)
    per_comp_means <- setNames(rep(NA_real_, length(comp_keys_kept)), comp_keys_kept)
    per_comp_disps <- setNames(rep(NA_real_, length(comp_keys_kept)), comp_keys_kept)

    if (length(comparisons_info) > 0) {
      for (comp_key in names(comparisons_info)) {
        comp <- comparisons_info[[comp_key]]
        ctrl_s <- comp$ctrl_samples
        treat_s <- comp$treatment_samples

        pep_ctrl <- rowMeans(mat[rows, ctrl_s, drop = FALSE], na.rm = TRUE)
        pep_treat <- rowMeans(mat[rows, treat_s, drop = FALSE], na.rm = TRUE)
        pep_ctrl[is.nan(pep_ctrl)] <- NA_real_
        pep_treat[is.nan(pep_treat)] <- NA_real_

        both <- !is.na(pep_ctrl) & !is.na(pep_treat) & pep_ctrl > 0 & pep_treat > 0
        pep_fc <- rep(NA_real_, n_pep)
        pep_fc[both] <- log2(pep_treat[both] / pep_ctrl[both])

        valid_fc <- pep_fc[!is.na(pep_fc)]
        disp <- if (length(valid_fc) >= 2) stats::sd(valid_fc) else NA_real_
        overall <- if (length(valid_fc) > 0) mean(valid_fc) else NA_real_

        if (!is.na(disp) && (is.na(best_fc_disparity) || disp > best_fc_disparity)) {
          best_fc_disparity <- disp
        }
        if (!is.na(overall) && (is.na(max_abs_fc) || abs(overall) > abs(max_abs_fc))) {
          max_abs_fc <- overall
        }

        # Store per-comparison values if this comparison is in the kept set
        if (comp_key %in% comp_keys_kept) {
          per_comp_means[comp_key] <- overall
          per_comp_disps[comp_key] <- disp
        }
      }
    }

    # Composite score
    # Single-group: just peptide CV (no FC info available)
    # Multi-group: mean(peptide CV, best FC disparity * 2)
    composite <- if (is_single_group) {
      if (is.na(peptide_cv)) 0 else peptide_cv
    } else {
      score_parts <- c()
      if (!is.na(peptide_cv)) score_parts <- c(score_parts, peptide_cv)
      if (!is.na(best_fc_disparity)) score_parts <- c(score_parts, best_fc_disparity * 2)
      if (length(score_parts) > 0) mean(score_parts) else 0
    }

    # Biology tag from GO (or "Other" when GO unavailable)
    bio <- pr_biology_tag(gene_sym, prot_id, protein_to_go, go_terms)

    row_static <- list(
      protein_id        = prot_id,
      gene_symbol       = gene_sym,
      n_peptides        = n_pep,
      n_unique_peptides = n_unique,
      peptide_cv        = peptide_cv,
      mean_replicate_cv = mean_rep_cv,
      best_fc_disparity = best_fc_disparity,
      max_log2fc        = max_abs_fc,
      composite_score   = composite,
      is_target         = prot_id %in% resolved_targets,
      biology_tag       = bio$tag,
      go_terms_str      = bio$go_names
    )

    # Append per-comparison FC columns (kept set only)
    if (length(comp_keys_kept) > 0L) {
      per_comp_list <- c(
        setNames(as.list(per_comp_means), fc_mean_cols),
        setNames(as.list(per_comp_disps), fc_disp_cols)
      )
      row_static <- c(row_static, per_comp_list)
    }

    row <- as.data.frame(row_static, stringsAsFactors = FALSE)
    score_rows[[length(score_rows) + 1L]] <- row
  }

  if (length(score_rows) == 0) {
    stop("Peptide Region: no proteins with >= ", min_peptides, " peptides found")
  }

  protein_scores <- do.call(rbind, score_rows)
  # Targets first, then by composite score
  protein_scores <- protein_scores[order(-protein_scores$is_target, -protein_scores$composite_score), ]
  rownames(protein_scores) <- NULL

  add_log("INFO", sprintf("Scored %d proteins (min_peptides=%d)", nrow(protein_scores), min_peptides))

  log_df <- if (length(log_entries) > 0) {
    do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(time = character(), level = character(), message = character(),
               stringsAsFactors = FALSE)
  }

  # ---- Build group_colors map from samples (set by nr_build_samples) ----
  group_colors <- NULL
  if ("color" %in% names(samples) && "group_name" %in% names(samples)) {
    gn <- as.character(samples$group_name)
    gc <- as.character(samples$color)
    keep <- !is.na(gn) & nzchar(gn) & !is.na(gc) & nzchar(gc)
    if (any(keep)) {
      df_gc <- unique(data.frame(g = gn[keep], c = gc[keep], stringsAsFactors = FALSE))
      # First color wins for each group name
      group_colors <- stats::setNames(df_gc$c[!duplicated(df_gc$g)],
                                      df_gc$g[!duplicated(df_gc$g)])
    }
  }

  list(
    engine_id = "peptide_region",
    params    = params,
    data      = list(
      mode               = "overview",
      protein_scores     = protein_scores,
      peptide_index      = peptide_index,
      comparisons_info   = comparisons_info,
      n_groups           = n_groups,
      single_group       = is_single_group,
      fc_mean_cols       = fc_mean_cols,
      fc_disp_cols       = fc_disp_cols,
      comp_keys_kept     = comp_keys_kept,
      aggregation_method = aggregation_method,
      gene_to_prot       = gene_to_prot,
      group_colors       = group_colors,
      mat                = mat,
      ids                = ids,
      samples            = samples,
      id_primary_col     = id_primary_col,
      id_protein_col     = id_protein_col,
      log                = log_df
    )
  )
}

# ---- Detail Computation (for child views) ----------------------------------

#' Compute detail-mode results for a single protein.
#' Builds one coverage per comparison (multi-comparison support).
pr_compute_detail <- function(protein_id, parent_data) {
  rows <- parent_data$peptide_index[[protein_id]]
  if (is.null(rows) || length(rows) == 0) {
    stop("No peptide data found for protein: ", protein_id)
  }

  mat     <- parent_data$mat
  ids     <- parent_data$ids
  samples <- parent_data$samples
  id_primary_col     <- parent_data$id_primary_col
  comparisons_info   <- parent_data$comparisons_info %||% list()
  n_groups           <- parent_data$n_groups %||% 1L
  aggregation_method <- parent_data$aggregation_method %||% "harmonic_mean"

  # Fetch UniProt
  uniprot <- pr_fetch_uniprot_data(protein_id)
  gene_symbol <- uniprot$gene_symbol %||% protein_id

  # Map peptides
  peptide_seqs <- as.character(ids[[id_primary_col]][rows])
  clean_seqs   <- vapply(peptide_seqs, pr_clean_peptide_sequence, character(1), USE.NAMES = FALSE)
  mapped       <- pr_map_peptides(clean_seqs, uniprot$sequence)

  valid_idx <- which(mapped$valid)
  if (length(valid_idx) == 0) {
    return(list(
      engine_id = "peptide_region",
      params = list(mode = "detail", detail_protein = protein_id),
      data = list(
        mode = "detail",
        protein = list(
          uniprot_id = protein_id, gene_symbol = gene_symbol,
          sequence = uniprot$sequence, seq_length = uniprot$seq_length,
          features = uniprot$features, coverage = NULL,
          peptide_stats = NULL, has_coverage = FALSE
        ),
        comparisons = list(), n_groups = n_groups,
        aggregation_method = aggregation_method
      )
    ))
  }

  # Base peptide stats (positions only)
  pep_stats_base <- data.frame(
    peptide_seq = peptide_seqs[valid_idx],
    clean_seq   = clean_seqs[valid_idx],
    prot_start  = mapped$starts[valid_idx],
    prot_end    = mapped$ends[valid_idx],
    stringsAsFactors = FALSE
  )

  # Tryptic classification (K/R terminal residues)
  pep_stats_base$c_term_residue <- substr(
    pep_stats_base$clean_seq,
    nchar(pep_stats_base$clean_seq),
    nchar(pep_stats_base$clean_seq)
  )
  c_term_is_kr <- pep_stats_base$c_term_residue %in% c("K", "R")

  # N-terminal: residue before prot_start in protein sequence
  # prot_start == 1 means protein N-terminus → always tryptic at that end
  n_term_preceding <- ifelse(
    pep_stats_base$prot_start > 1L,
    substr(uniprot$sequence,
           pep_stats_base$prot_start - 1L,
           pep_stats_base$prot_start - 1L),
    "-"
  )
  n_term_is_kr <- n_term_preceding %in% c("K", "R", "-")

  pep_stats_base$tryptic_class <- ifelse(
    c_term_is_kr & n_term_is_kr, "fully_tryptic",
    ifelse(c_term_is_kr | n_term_is_kr, "semi_tryptic", "non_tryptic")
  )

  # ---- Build per-group coverage ----
  groups <- unique(samples$group_name)
  group_coverages <- list()

  for (grp in groups) {
    grp_samples <- samples$sample_col[samples$group_name == grp]
    pep_stats <- pep_stats_base

    grp_mat_sub <- mat[rows[valid_idx], grp_samples, drop = FALSE]
    pep_stats$group_mean <- rowMeans(grp_mat_sub, na.rm = TRUE)
    pep_stats$group_mean[is.nan(pep_stats$group_mean)] <- NA_real_

    # Per-replicate lists for CV
    n_samp <- length(grp_samples)
    rep_lists <- lapply(seq_len(nrow(grp_mat_sub)), function(i) {
      x <- as.numeric(grp_mat_sub[i, ]); x[!is.na(x)]
    })

    keep <- !is.na(pep_stats$group_mean)
    pep_stats <- pep_stats[keep, , drop = FALSE]
    rep_lists <- rep_lists[keep]

    cov <- pr_build_coverage(
      pep_stats, uniprot$seq_length, aggregation_method,
      value_cols = "group_mean",
      compute_cv = (n_samp >= 2L),
      replicate_lists = if (n_samp >= 2L) list(group = rep_lists) else NULL
    )

    group_coverages[[grp]] <- list(
      coverage      = cov,
      peptide_stats = pep_stats,
      group_name    = grp,
      n_samples     = n_samp
    )
  }

  # ---- Build pairwise stats (FC + t-test) for multi-group ----
  pairwise <- list()

  if (n_groups >= 2L && length(comparisons_info) > 0) {
    for (comp_key in names(comparisons_info)) {
      comp <- comparisons_info[[comp_key]]
      grp_a <- comp$ctrl_group
      grp_b <- comp$treatment_group

      cov_a <- group_coverages[[grp_a]]$coverage
      cov_b <- group_coverages[[grp_b]]$coverage

      # Residue-level FC: group_b / group_a
      fc_cov <- data.frame(
        residue = cov_a$residue,
        fc      = cov_b$group_mean / cov_a$group_mean,
        stringsAsFactors = FALSE
      )

      # Per-peptide t-test
      n_a <- length(comp$ctrl_samples)
      n_b <- length(comp$treatment_samples)
      has_stats <- n_a >= 2L && n_b >= 2L

      if (has_stats) {
        mat_a <- mat[rows[valid_idx], comp$ctrl_samples, drop = FALSE]
        mat_b <- mat[rows[valid_idx], comp$treatment_samples, drop = FALSE]
        pep_pvals <- numeric(nrow(mat_a))
        for (pi in seq_len(nrow(mat_a))) {
          va <- as.numeric(mat_a[pi, ])
          vb <- as.numeric(mat_b[pi, ])
          va <- va[!is.na(va) & is.finite(va)]
          vb <- vb[!is.na(vb) & is.finite(vb)]
          if (length(va) >= 2L && length(vb) >= 2L) {
            tt <- tryCatch(stats::t.test(va, vb, var.equal = FALSE), error = function(e) NULL)
            pep_pvals[pi] <- if (!is.null(tt)) tt$p.value else NA_real_
          } else {
            pep_pvals[pi] <- NA_real_
          }
        }
        padj <- stats::p.adjust(pep_pvals, method = "BH")

        # Propagate min padj to residues
        ps_tmp <- pep_stats_base[, c("prot_start", "prot_end"), drop = FALSE]
        ps_tmp$padj <- padj
        pval_per_res <- vector("list", uniprot$seq_length)
        for (i in seq_len(nrow(ps_tmp))) {
          pv <- ps_tmp$padj[i]
          if (!is.na(pv)) {
            for (pos in ps_tmp$prot_start[i]:ps_tmp$prot_end[i]) {
              pval_per_res[[pos]] <- c(pval_per_res[[pos]], pv)
            }
          }
        }
        fc_cov$min_pvalue <- vapply(pval_per_res, function(x) {
          if (length(x) == 0) NA_real_ else min(x, na.rm = TRUE)
        }, numeric(1))
      }

      pairwise[[comp_key]] <- list(
        fc_coverage    = fc_cov,
        group_a        = grp_a,
        group_b        = grp_b,
        has_statistics = has_stats
      )
    }
  }

  list(
    engine_id = "peptide_region",
    params = list(mode = "detail", detail_protein = protein_id),
    data = list(
      mode = "detail",
      protein = list(
        uniprot_id    = protein_id,
        gene_symbol   = gene_symbol,
        sequence      = uniprot$sequence,
        seq_length    = uniprot$seq_length,
        features      = uniprot$features,
        peptide_positions = pep_stats_base[, c("prot_start", "prot_end", "c_term_residue", "tryptic_class")],
        has_coverage  = TRUE
      ),
      group_coverages    = group_coverages,
      pairwise           = pairwise,
      comparisons        = list(),
      n_groups           = n_groups,
      group_names        = groups,
      group_colors       = parent_data$group_colors %||% NULL,
      aggregation_method = aggregation_method
    )
  )
}
