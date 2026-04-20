# =========================================================
# R/engines/stats/umap_gene.R — Gene-level UMAP Engine
#
# Each point = one protein/gene. Two modes:
#  - combined (default): one UMAP over all samples; the 3 sample
#    groups become features per gene, not separating factors.
#  - per_group: one UMAP per group, computed only on that group's
#    samples. Embedding has a `group` column so the renderer can
#    produce one plot per group.
#
# Annotation: subcellular localization bucket via terpbase gene_meta
# and .map_subloc_bucket() from stats/subloc.R.
#
# Contract:
#  - data$embedding: data.frame(group, protein_id, gene_symbol,
#      UMAP1, UMAP2, subloc, cluster, mean_abundance, variance)
#  - data$groups: character vector of group labels present
#  - data$subloc_levels / data$log / data$clustering_enabled
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------
# Helper: run a single UMAP pipeline on a matrix slice.
# Returns a per-group embedding data.frame (or NULL on failure).
# ---------------------------------------------------------
.umap_gene_pipeline <- function(mat, protein_ids, group_label, params,
                                terpbase, add_log) {
  if (nrow(mat) < 10) {
    add_log("WARN", sprintf("[%s] <10 proteins after filtering; skipped", group_label))
    return(NULL)
  }
  if (ncol(mat) < 2) {
    add_log("WARN", sprintf("[%s] <2 samples; cannot run UMAP", group_label))
    return(NULL)
  }

  scale_method <- params$scale_method %||% "zscore"
  variance_filter_top_n <- as.integer(params$variance_filter_top_n %||% 2000)
  n_neighbors <- as.integer(params$n_neighbors %||% 15)
  min_dist    <- as.numeric(params$min_dist %||% 0.1)
  metric      <- params$metric %||% "correlation"
  seed        <- as.integer(params$seed %||% 42)
  clustering_enabled <- isTRUE(params$clustering_enabled %||% TRUE)
  cluster_min_size   <- as.integer(params$cluster_min_size %||% 15)
  cluster_on         <- params$cluster_on %||% "embedding"
  id_type            <- params$id_type %||% "uniprot"

  # Per-gene stats BEFORE variance filter
  mean_abundance <- rowMeans(mat, na.rm = TRUE)
  variance_vec   <- apply(mat, 1, var, na.rm = TRUE)
  variance_vec[!is.finite(variance_vec)] <- 0

  # Variance filter
  if (variance_filter_top_n > 0 && nrow(mat) > variance_filter_top_n) {
    top_idx <- order(variance_vec, decreasing = TRUE)[seq_len(variance_filter_top_n)]
    mat <- mat[top_idx, , drop = FALSE]
    protein_ids <- protein_ids[top_idx]
    mean_abundance <- mean_abundance[top_idx]
    variance_vec <- variance_vec[top_idx]
    add_log("INFO", sprintf("[%s] kept top %d proteins by variance",
                            group_label, variance_filter_top_n))
  }

  # Scale per-gene across samples
  if (scale_method == "zscore") {
    mat_scaled <- t(scale(t(mat), center = TRUE, scale = TRUE))
    bad <- apply(mat_scaled, 1, function(x) any(!is.finite(x)))
    if (any(bad)) {
      add_log("INFO", sprintf("[%s] dropped %d zero-variance proteins after scaling",
                              group_label, sum(bad)))
      mat_scaled <- mat_scaled[!bad, , drop = FALSE]
      protein_ids <- protein_ids[!bad]
      mean_abundance <- mean_abundance[!bad]
      variance_vec <- variance_vec[!bad]
    }
  } else {
    mat_scaled <- as.matrix(mat)
  }

  n_neighbors_eff <- min(n_neighbors, nrow(mat_scaled) - 1, ncol(mat_scaled) * 10)
  n_neighbors_eff <- max(2L, as.integer(n_neighbors_eff))
  if (nrow(mat_scaled) < n_neighbors_eff + 1) {
    add_log("WARN", sprintf("[%s] need >=%d proteins for UMAP; have %d — skipped",
                            group_label, n_neighbors_eff + 1, nrow(mat_scaled)))
    return(NULL)
  }

  add_log("INFO", sprintf("[%s] UMAP (n_neighbors=%d, min_dist=%.2f, metric=%s) on %d x %d",
                          group_label, n_neighbors_eff, min_dist, metric,
                          nrow(mat_scaled), ncol(mat_scaled)))
  set.seed(seed)
  umap_res <- tryCatch(
    uwot::umap(
      mat_scaled,
      n_neighbors = n_neighbors_eff,
      min_dist = min_dist,
      metric = metric,
      n_components = 2,
      verbose = FALSE
    ),
    error = function(e) {
      add_log("ERROR", sprintf("[%s] uwot::umap failed: %s",
                               group_label, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(umap_res)) return(NULL)

  # HDBSCAN
  cluster_vec <- rep(0L, nrow(umap_res))
  if (clustering_enabled) {
    cluster_input <- if (cluster_on == "features") mat_scaled else umap_res
    min_pts_eff <- max(2L, min(cluster_min_size, floor(nrow(cluster_input) / 2)))
    cl <- tryCatch(
      dbscan::hdbscan(cluster_input, minPts = min_pts_eff),
      error = function(e) {
        add_log("WARN", sprintf("[%s] hdbscan failed: %s",
                                group_label, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(cl)) {
      cluster_vec <- as.integer(cl$cluster)
      add_log("INFO", sprintf("[%s] HDBSCAN: %d clusters, %d noise (minPts=%d)",
                              group_label,
                              length(unique(cluster_vec[cluster_vec > 0])),
                              sum(cluster_vec == 0), min_pts_eff))
    }
  }

  # Subloc annotation
  subloc_vec <- rep("Other/Unknown", nrow(umap_res))
  gene_symbol_vec <- protein_ids

  if (!is.null(terpbase)) {
    gene_meta <- terpbase$gene_meta %||% NULL
    if (!is.null(gene_meta) && is.data.frame(gene_meta)) {
      if (!"entry" %in% names(gene_meta) && "Entry" %in% names(gene_meta)) {
        names(gene_meta)[names(gene_meta) == "Entry"] <- "entry"
      }
      if (!"gene_symbol" %in% names(gene_meta) && "gene" %in% names(gene_meta)) {
        names(gene_meta)[names(gene_meta) == "gene"] <- "gene_symbol"
      }
      if (!"subcell_location" %in% names(gene_meta)) {
        alt <- intersect(c("Subcell", "subcellular_location", "localization", "location"),
                         names(gene_meta))
        if (length(alt) > 0) {
          names(gene_meta)[names(gene_meta) == alt[1]] <- "subcell_location"
        }
      }

      lookup_key <- if (id_type == "uniprot" && "entry" %in% names(gene_meta)) {
        toupper(trimws(as.character(gene_meta$entry)))
      } else if ("gene_symbol" %in% names(gene_meta)) {
        toupper(trimws(as.character(gene_meta$gene_symbol)))
      } else if ("entry" %in% names(gene_meta)) {
        toupper(trimws(as.character(gene_meta$entry)))
      } else {
        NULL
      }

      if (!is.null(lookup_key) && "subcell_location" %in% names(gene_meta)) {
        query <- toupper(trimws(as.character(protein_ids)))
        if (id_type == "uniprot") {
          query <- sub("-\\d+$", "", query, perl = TRUE)
        }
        idx <- match(query, lookup_key)
        raw_loc <- as.character(gene_meta$subcell_location)[idx]
        mapped <- .map_subloc_bucket(raw_loc)
        mapped[is.na(mapped)] <- "Other/Unknown"
        subloc_vec <- mapped

        if ("gene_symbol" %in% names(gene_meta)) {
          gs <- as.character(gene_meta$gene_symbol)[idx]
          miss <- is.na(gs) | !nzchar(gs)
          gs[miss] <- protein_ids[miss]
          gene_symbol_vec <- gs
        }
      }
    }
  }

  data.frame(
    group = group_label,
    protein_id = protein_ids,
    gene_symbol = gene_symbol_vec,
    UMAP1 = as.numeric(umap_res[, 1]),
    UMAP2 = as.numeric(umap_res[, 2]),
    subloc = subloc_vec,
    cluster = cluster_vec,
    mean_abundance = mean_abundance,
    variance = variance_vec,
    stringsAsFactors = FALSE
  )
}

#' Execute umap_gene engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with terpbase
#' @return Contract-compliant list(engine_id, params, data)
stats_umap_gene_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()

  params <- params %||% payload$params %||% list()

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries <<- c(log_entries, list(list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level,
      message = msg
    )))
  }

  empty_embedding <- function() data.frame(
    group = character(0),
    protein_id = character(0),
    gene_symbol = character(0),
    UMAP1 = numeric(0),
    UMAP2 = numeric(0),
    subloc = character(0),
    cluster = integer(0),
    mean_abundance = numeric(0),
    variance = numeric(0),
    stringsAsFactors = FALSE
  )

  fail <- function(msg, level = "ERROR") {
    add_log(level, msg)
    log_df <- do.call(rbind, lapply(log_entries, function(e) {
      data.frame(time = e$time, level = e$level, message = e$message,
                 stringsAsFactors = FALSE)
    }))
    list(
      engine_id = "umap_gene",
      params = params,
      data = list(
        embedding = empty_embedding(),
        groups = character(0),
        subloc_levels = character(0),
        clustering_enabled = isTRUE(params$clustering_enabled %||% TRUE),
        log = log_df
      )
    )
  }

  if (!isTRUE(payload$ok)) {
    return(fail(payload$error %||% "Invalid payload"))
  }

  mat <- payload$mat
  ids <- payload$ids
  samples <- payload$samples
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) < 2) {
    return(fail("umap_gene requires a matrix with >=1 protein and >=2 samples"))
  }

  n_input <- nrow(mat)
  add_log("INFO", sprintf("Initiating gene-UMAP: %d proteins x %d samples",
                          n_input, ncol(mat)))

  log_transform    <- params$log_transform %||% "log2"
  missing_handling <- params$missing_handling %||% "drop_gene"
  min_valid_fraction <- as.numeric(params$min_valid_fraction %||% 0.7)
  group_mode       <- params$group_mode %||% "combined"

  # Protein IDs (mirror subloc.R convention)
  protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
  if (nzchar(protein_col) && protein_col %in% names(ids)) {
    protein_ids <- as.character(ids[[protein_col]])
  } else {
    protein_ids <- rownames(mat)
    if (is.null(protein_ids)) protein_ids <- as.character(seq_len(n_input))
  }

  # Log transform (applied globally — pure per-value op)
  if (log_transform == "log10") {
    mat[mat <= 0] <- NA
    mat <- log10(mat)
    add_log("INFO", "Applied log10 transform")
  } else if (log_transform == "log2") {
    mat[mat <= 0] <- NA
    mat <- log2(mat)
    add_log("INFO", "Applied log2 transform")
  }

  # Shared helper for missing-value filtering on an arbitrary slice
  apply_missing <- function(m, pids, tag) {
    valid_frac <- rowSums(!is.na(m)) / ncol(m)
    keep <- valid_frac >= min_valid_fraction
    if (sum(!keep) > 0) {
      add_log("INFO", sprintf("[%s] dropped %d proteins below %.0f%% valid threshold",
                              tag, sum(!keep), min_valid_fraction * 100))
    }
    m <- m[keep, , drop = FALSE]
    pids <- pids[keep]
    if (missing_handling == "drop_gene") {
      cc <- complete.cases(m)
      if (sum(!cc) > 0) {
        add_log("INFO", sprintf("[%s] dropped %d proteins with remaining NA",
                                tag, sum(!cc)))
      }
      m <- m[cc, , drop = FALSE]
      pids <- pids[cc]
    } else if (missing_handling == "min_impute") {
      row_mins <- apply(m, 1, min, na.rm = TRUE)
      row_mins[!is.finite(row_mins)] <- 0
      for (i in seq_len(nrow(m))) {
        na_mask <- is.na(m[i, ])
        if (any(na_mask)) m[i, na_mask] <- row_mins[i]
      }
    } else if (missing_handling == "zero_impute") {
      m[is.na(m)] <- 0
    }
    list(mat = m, pids = pids)
  }

  terpbase <- payload$terpbase %||% context$terpbase

  # =========================================================
  # Branch on group_mode
  # =========================================================
  if (group_mode == "per_group") {
    if (is.null(samples) || !is.data.frame(samples) ||
        !all(c("sample_col", "group_name") %in% names(samples))) {
      return(fail("per_group mode requires payload$samples with sample_col + group_name"))
    }

    group_names <- unique(as.character(samples$group_name))
    group_names <- group_names[nzchar(group_names)]
    add_log("INFO", sprintf("Per-group mode: %d groups (%s)",
                            length(group_names), paste(group_names, collapse = ", ")))

    per_group_emb <- list()
    for (grp in group_names) {
      grp_cols <- as.character(samples$sample_col[samples$group_name == grp])
      grp_cols <- intersect(grp_cols, colnames(mat))
      if (length(grp_cols) < 2) {
        add_log("WARN", sprintf("[%s] <2 samples in this group; skipped", grp))
        next
      }
      mat_g <- mat[, grp_cols, drop = FALSE]
      mh <- apply_missing(mat_g, protein_ids, grp)
      emb_g <- .umap_gene_pipeline(mh$mat, mh$pids, grp, params, terpbase, add_log)
      if (!is.null(emb_g)) per_group_emb[[grp]] <- emb_g
    }

    if (length(per_group_emb) == 0) {
      return(fail("No group produced a valid UMAP; see log"))
    }
    embedding <- do.call(rbind, per_group_emb)
    groups_out <- names(per_group_emb)
  } else {
    # combined mode
    mh <- apply_missing(mat, protein_ids, "combined")
    emb <- .umap_gene_pipeline(mh$mat, mh$pids, "combined", params, terpbase, add_log)
    if (is.null(emb)) return(fail("UMAP failed in combined mode; see log"))
    embedding <- emb
    groups_out <- "combined"
  }

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("umap_gene completed in %.2f seconds (%d rows, %d groups)",
                          engine_duration, nrow(embedding), length(groups_out)))

  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(time = e$time, level = e$level, message = e$message,
               stringsAsFactors = FALSE)
  }))

  subloc_levels_present <- if (exists("SUBLOC_LEVELS")) {
    intersect(SUBLOC_LEVELS, unique(embedding$subloc))
  } else {
    unique(embedding$subloc)
  }

  list(
    engine_id = "umap_gene",
    params = params,
    data = list(
      embedding = embedding,
      groups = groups_out,
      subloc_levels = subloc_levels_present,
      clustering_enabled = isTRUE(params$clustering_enabled %||% TRUE),
      log = log_df
    )
  )
}
