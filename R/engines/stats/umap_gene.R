# =========================================================
# R/engines/stats/umap_gene.R — Gene-level UMAP Engine (v1)
#
# Each point = one protein/gene, positioned by its log-abundance
# pattern across samples. Annotated with subcellular localization
# (from terpbase$gene_meta via .map_subloc_bucket in subloc.R).
# Optionally clustered post-embedding via HDBSCAN.
#
# Contract v1.1:
#  - data$embedding: data.frame(protein_id, gene_symbol, UMAP1, UMAP2,
#      subloc, cluster, mean_abundance, variance)
#  - data$n_input / data$n_used / data$n_dropped
#  - data$subloc_levels: character vector of all subloc buckets used
#  - data$log
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Execute umap_gene engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters (UMAP hyperparameters, preprocessing)
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
        n_input = 0L, n_used = 0L, n_dropped = 0L,
        subloc_levels = character(0),
        log = log_df
      )
    )
  }

  if (!isTRUE(payload$ok)) {
    return(fail(payload$error %||% "Invalid payload"))
  }

  mat <- payload$mat
  ids <- payload$ids
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) < 2) {
    return(fail("umap_gene requires a matrix with >=1 protein and >=2 samples"))
  }

  n_input <- nrow(mat)
  add_log("INFO", sprintf("Initiating gene-UMAP: %d proteins x %d samples",
                          n_input, ncol(mat)))

  # -----------------------------------------------------------
  # Parameters
  # -----------------------------------------------------------
  log_transform <- params$log_transform %||% "log2"
  scale_method  <- params$scale_method %||% "zscore"
  missing_handling <- params$missing_handling %||% "drop_gene"
  min_valid_fraction <- as.numeric(params$min_valid_fraction %||% 0.7)
  variance_filter_top_n <- as.integer(params$variance_filter_top_n %||% 2000)

  n_neighbors <- as.integer(params$n_neighbors %||% 15)
  min_dist    <- as.numeric(params$min_dist %||% 0.1)
  metric      <- params$metric %||% "correlation"
  seed        <- as.integer(params$seed %||% 42)

  clustering_enabled <- isTRUE(params$clustering_enabled %||% TRUE)
  cluster_min_size   <- as.integer(params$cluster_min_size %||% 15)
  cluster_on         <- params$cluster_on %||% "embedding"
  id_type            <- params$id_type %||% "uniprot"

  # -----------------------------------------------------------
  # Extract per-row protein IDs (mirror subloc.R convention)
  # -----------------------------------------------------------
  protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
  if (nzchar(protein_col) && protein_col %in% names(ids)) {
    protein_ids <- as.character(ids[[protein_col]])
  } else {
    protein_ids <- rownames(mat)
    if (is.null(protein_ids)) protein_ids <- as.character(seq_len(n_input))
  }

  # -----------------------------------------------------------
  # Log transform
  # -----------------------------------------------------------
  if (log_transform == "log10") {
    mat[mat <= 0] <- NA
    mat <- log10(mat)
    add_log("INFO", "Applied log10 transform")
  } else if (log_transform == "log2") {
    mat[mat <= 0] <- NA
    mat <- log2(mat)
    add_log("INFO", "Applied log2 transform")
  }

  # -----------------------------------------------------------
  # Missing-value filtering: drop rows with too few valid values
  # -----------------------------------------------------------
  valid_frac <- rowSums(!is.na(mat)) / ncol(mat)
  keep <- valid_frac >= min_valid_fraction
  n_too_sparse <- sum(!keep)
  if (n_too_sparse > 0) {
    add_log("INFO", sprintf("Dropped %d proteins with <%.0f%% valid values",
                            n_too_sparse, min_valid_fraction * 100))
  }
  mat <- mat[keep, , drop = FALSE]
  protein_ids <- protein_ids[keep]

  if (nrow(mat) < 10) {
    return(fail("Fewer than 10 proteins survived missing-value filter; cannot run UMAP"))
  }

  # -----------------------------------------------------------
  # Missing value handling
  # -----------------------------------------------------------
  if (missing_handling == "drop_gene") {
    complete <- complete.cases(mat)
    n_incomplete <- sum(!complete)
    if (n_incomplete > 0) {
      add_log("INFO", sprintf("Dropped %d proteins with any remaining NA",
                              n_incomplete))
    }
    mat <- mat[complete, , drop = FALSE]
    protein_ids <- protein_ids[complete]
  } else if (missing_handling == "min_impute") {
    row_mins <- apply(mat, 1, min, na.rm = TRUE)
    row_mins[!is.finite(row_mins)] <- 0
    for (i in seq_len(nrow(mat))) {
      na_mask <- is.na(mat[i, ])
      if (any(na_mask)) mat[i, na_mask] <- row_mins[i]
    }
    add_log("INFO", "Imputed NA values with per-gene minimum")
  } else if (missing_handling == "zero_impute") {
    mat[is.na(mat)] <- 0
    add_log("INFO", "Imputed NA values with zero")
  }

  if (nrow(mat) < 10) {
    return(fail("Fewer than 10 proteins after missing-value handling"))
  }

  # -----------------------------------------------------------
  # Compute per-gene summary stats BEFORE scaling (for annotation)
  # -----------------------------------------------------------
  mean_abundance <- rowMeans(mat, na.rm = TRUE)
  variance_vec   <- apply(mat, 1, var, na.rm = TRUE)
  variance_vec[!is.finite(variance_vec)] <- 0

  # -----------------------------------------------------------
  # Variance filter: keep top-N most variable proteins
  # -----------------------------------------------------------
  if (variance_filter_top_n > 0 && nrow(mat) > variance_filter_top_n) {
    top_idx <- order(variance_vec, decreasing = TRUE)[seq_len(variance_filter_top_n)]
    mat <- mat[top_idx, , drop = FALSE]
    protein_ids <- protein_ids[top_idx]
    mean_abundance <- mean_abundance[top_idx]
    variance_vec <- variance_vec[top_idx]
    add_log("INFO", sprintf("Kept top %d proteins by variance", variance_filter_top_n))
  }

  # -----------------------------------------------------------
  # Scale (per-gene across samples)
  # -----------------------------------------------------------
  if (scale_method == "zscore") {
    mat_scaled <- t(scale(t(mat), center = TRUE, scale = TRUE))
    # Rows where scale() produced NaN (sd=0) — drop
    bad <- apply(mat_scaled, 1, function(x) any(!is.finite(x)))
    if (any(bad)) {
      add_log("INFO", sprintf("Dropped %d zero-variance proteins after scaling", sum(bad)))
      mat_scaled <- mat_scaled[!bad, , drop = FALSE]
      protein_ids <- protein_ids[!bad]
      mean_abundance <- mean_abundance[!bad]
      variance_vec <- variance_vec[!bad]
    }
  } else {
    mat_scaled <- as.matrix(mat)
  }

  if (nrow(mat_scaled) < max(10, n_neighbors + 1)) {
    return(fail(sprintf(
      "Need >=%d proteins for n_neighbors=%d; have %d",
      n_neighbors + 1, n_neighbors, nrow(mat_scaled))))
  }

  # -----------------------------------------------------------
  # Run UMAP
  # -----------------------------------------------------------
  add_log("INFO", sprintf("Running UMAP (n_neighbors=%d, min_dist=%.2f, metric=%s, seed=%d)",
                          n_neighbors, min_dist, metric, seed))
  set.seed(seed)
  n_neighbors_eff <- min(n_neighbors, nrow(mat_scaled) - 1)
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
      add_log("ERROR", sprintf("uwot::umap failed: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(umap_res)) {
    return(fail("UMAP embedding failed; see log", level = "ERROR"))
  }

  # -----------------------------------------------------------
  # Post-hoc clustering (HDBSCAN)
  # -----------------------------------------------------------
  cluster_vec <- rep(0L, nrow(umap_res))
  if (clustering_enabled) {
    cluster_input <- if (cluster_on == "features") mat_scaled else umap_res
    min_pts_eff <- max(2L, min(cluster_min_size, floor(nrow(cluster_input) / 2)))
    cl <- tryCatch(
      dbscan::hdbscan(cluster_input, minPts = min_pts_eff),
      error = function(e) {
        add_log("WARN", sprintf("hdbscan failed: %s", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(cl)) {
      cluster_vec <- as.integer(cl$cluster)
      n_clusters <- length(unique(cluster_vec[cluster_vec > 0]))
      n_noise <- sum(cluster_vec == 0)
      add_log("INFO", sprintf("HDBSCAN: %d clusters, %d noise points (minPts=%d, on=%s)",
                              n_clusters, n_noise, min_pts_eff, cluster_on))
    }
  }

  # -----------------------------------------------------------
  # Annotate with subcellular localization from terpbase
  # Reuses .map_subloc_bucket() + SUBLOC_LEVELS from stats/subloc.R
  # -----------------------------------------------------------
  terpbase <- payload$terpbase %||% context$terpbase
  subloc_vec <- rep("Other/Unknown", nrow(umap_res))
  gene_symbol_vec <- protein_ids  # fallback to protein_id

  if (!is.null(terpbase)) {
    gene_meta <- terpbase$gene_meta %||% NULL
    if (!is.null(gene_meta) && is.data.frame(gene_meta)) {
      # Normalize column names (mirror subloc.R)
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

      # Lookup key by id_type
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
        # Strip isoform suffix from query IDs if UniProt
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
          gs[is.na(gs) | !nzchar(gs)] <- protein_ids[is.na(gs) | !nzchar(gs)]
          gene_symbol_vec <- gs
        }
        add_log("INFO", sprintf("Annotated subcellular localization: %d / %d matched",
                                sum(!is.na(idx)), length(idx)))
      } else {
        add_log("WARN", "gene_meta missing subcell_location or id columns; subloc=Other/Unknown")
      }
    } else {
      add_log("WARN", "terpbase$gene_meta absent; subloc=Other/Unknown")
    }
  } else {
    add_log("WARN", "No terpbase available; subloc=Other/Unknown")
  }

  # -----------------------------------------------------------
  # Assemble embedding data.frame
  # -----------------------------------------------------------
  embedding <- data.frame(
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

  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("umap_gene completed in %.2f seconds (%d points embedded)",
                          engine_duration, nrow(embedding)))

  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(time = e$time, level = e$level, message = e$message,
               stringsAsFactors = FALSE)
  }))

  # Subloc levels actually present (for legend ordering)
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
      n_input = n_input,
      n_used = nrow(embedding),
      n_dropped = n_input - nrow(embedding),
      subloc_levels = subloc_levels_present,
      clustering_enabled = clustering_enabled,
      log = log_df
    )
  )
}
