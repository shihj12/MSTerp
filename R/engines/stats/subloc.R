# =========================================================
# R/engines/stats/subloc.R — Subcellular Localization Engine
#
# Maps proteinIDs to subcellular bins from terpbase and
# produces distribution data per group.
#
# Contract v1.1:
#  - data$points: bin, group, value, protein_id
#  - data$counts: bin, count
#  - data$summary: bin, group, n, mean, median
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =========================================================
# Canonical subcellular location buckets and vectorized mapper
# =========================================================

SUBLOC_LEVELS <- c(
  "Nucleus", "Cytoplasm", "Cell membrane", "Mitochondria",
  "Endoplasmic reticulum", "Golgi", "Lysosome", "Endosome",
  "Peroxisome", "Cytoskeleton", "Synapse",
  "Extracellular membrane", "Secreted",
  "Other/Unknown"
)

#' Map raw subcellular location strings to canonical buckets (vectorized)
#' @param x Character vector of raw subcellular location strings
#' @return Character vector of canonical bucket names
.map_subloc_bucket <- function(x) {
  x <- as.character(x)
  s <- tolower(x)
  s[is.na(s)] <- ""
  out <- rep(NA_character_, length(s))

  pick <- function(pattern, label) {
    mask <- is.na(out) & grepl(pattern, s)
    out[mask] <<- label
  }

  # Order matters: more specific patterns first
  pick("mitochond", "Mitochondria")
  pick("nucleus|nucleol|nucleoplasm|chromatin|chromosome|centromere|kinetochore|pml body|cajal body|nuclear speck|nuclear body|nucleus membrane|nucleus envelope|nucleus lamina|nucleus matrix", "Nucleus")
  pick("synap|presynap|postsynap|synaptosome|postsynaptic density", "Synapse")
  pick("lysosom", "Lysosome")
  pick("endosom|phagosome|multivesicular body", "Endosome")
  pick("cytoskeleton|microtubule|actin cytoskeleton|intermediate filament|sarcomere|myofibril|stress fiber|spindle|centrosome|centriole|axoneme|cilium|flagellum|microvillus|stereocilium", "Cytoskeleton")
  pick("endoplasmic reticulum|sarcoplasmic reticulum|microsome", "Endoplasmic reticulum")
  pick("golgi", "Golgi")
  pick("exosome|extracellular vesicle|blood microparticle|virion", "Extracellular membrane")
  pick("secreted|extracellular space|extracellular region|extracellular matrix|basement membrane|collagen-containing extracellular matrix", "Secreted")
  pick("plasma membrane|cell membrane|sarcolemma|cell surface|apical plasma membrane|basolateral plasma membrane|lateral plasma membrane|membrane raft|postsynaptic density membrane|presynaptic membrane|synaptic membrane|apical membrane|basal membrane|basolateral cell membrane|lateral cell membrane|apical cell membrane|basal cell membrane", "Cell membrane")
  pick("cytoplasm|cytosol|p-body|p body|stress granule|ribonucleoprotein granule|perinuclear region|cell cortex|perikaryon", "Cytoplasm")
  pick("peroxisom", "Peroxisome")

  out
}

#' Normalize UniProt IDs (strip isoform suffixes)
#' @param x Character vector of UniProt IDs
#' @return Character vector of canonical UniProt IDs
.canonical_uniprot <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- ifelse(nzchar(x), sub("-\\d+$", "", x, perl = TRUE), x)
  x[x == ""] <- NA_character_
  x
}

#' Execute subloc engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with terpbase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_subloc_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "subloc",
      params = params,
      data = list(
        points = data.frame(
          bin = character(0),
          group = character(0),
          value = numeric(0),
          protein_id = character(0),
          stringsAsFactors = FALSE
        ),
        counts = data.frame(
          bin = character(0),
          count = integer(0),
          stringsAsFactors = FALSE
        ),
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

  add_log("INFO", sprintf("Initiating subloc: %d proteins x %d groups",
                          nrow(mat), length(groups)))

  # Get parameters
  log_transform <- params$log_transform %||% "log10"
  id_type <- params$id_type %||% "uniprot"  # "uniprot" or "gene"

  # Get terpbase mappings
  terpbase <- payload$terpbase %||% context$terpbase

  if (is.null(terpbase)) {
    return(list(
      engine_id = "subloc",
      params = params,
      data = list(
        points = data.frame(
          bin = character(0),
          group = character(0),
          value = numeric(0),
          protein_id = character(0),
          stringsAsFactors = FALSE
        ),
        counts = data.frame(
          bin = character(0),
          count = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "subloc requires terpbase with localization annotations",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # =========================================================
  # Build lookup table from gene_meta (vectorized)
  # =========================================================
  gene_meta <- terpbase$gene_meta %||% NULL

  if (is.null(gene_meta)) {
    return(list(
      engine_id = "subloc",
      params = params,
      data = list(
        points = data.frame(
          bin = character(0),
          group = character(0),
          value = numeric(0),
          protein_id = character(0),
          stringsAsFactors = FALSE
        ),
        counts = data.frame(
          bin = character(0),
          count = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "Terpbase missing gene_meta for subloc",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Normalize column names
  gm_names <- names(gene_meta)
  if (!"entry" %in% gm_names && "Entry" %in% gm_names) {
    names(gene_meta)[names(gene_meta) == "Entry"] <- "entry"
  }
  if (!"gene_symbol" %in% names(gene_meta) && "gene" %in% names(gene_meta)) {
    names(gene_meta)[names(gene_meta) == "gene"] <- "gene_symbol"
  }
  if (!"subcell_location" %in% names(gene_meta)) {
    # Try common alternatives
    loc_col_candidates <- intersect(c("Subcell", "subcellular_location", "localization", "location"),
                                    names(gene_meta))
    if (length(loc_col_candidates) > 0) {
      names(gene_meta)[names(gene_meta) == loc_col_candidates[1]] <- "subcell_location"
    }
  }

  # Check required columns
  if (!"subcell_location" %in% names(gene_meta)) {
    return(list(
      engine_id = "subloc",
      params = params,
      data = list(
        points = data.frame(
          bin = character(0),
          group = character(0),
          value = numeric(0),
          protein_id = character(0),
          stringsAsFactors = FALSE
        ),
        counts = data.frame(
          bin = character(0),
          count = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "gene_meta missing subcell_location column",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Build lookup vectors (vectorized, no loops)
  if (id_type == "uniprot" && "entry" %in% names(gene_meta)) {
    lookup_key <- toupper(trimws(as.character(gene_meta$entry)))
    add_log("INFO", "Using UniProt entry for lookup")
  } else if ("gene_symbol" %in% names(gene_meta)) {
    lookup_key <- toupper(trimws(as.character(gene_meta$gene_symbol)))
    add_log("INFO", "Using gene symbol for lookup")
  } else if ("entry" %in% names(gene_meta)) {
    lookup_key <- toupper(trimws(as.character(gene_meta$entry)))
    add_log("INFO", "Falling back to entry for lookup")
  } else {
    return(list(
      engine_id = "subloc",
      params = params,
      data = list(
        points = data.frame(
          bin = character(0),
          group = character(0),
          value = numeric(0),
          protein_id = character(0),
          stringsAsFactors = FALSE
        ),
        counts = data.frame(
          bin = character(0),
          count = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "gene_meta missing entry and gene_symbol columns",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  lookup_loc <- as.character(gene_meta$subcell_location)

  add_log("INFO", sprintf("Loaded %d entries from gene_meta", length(lookup_key)))

  # =========================================================
  # Get protein IDs from data
  # =========================================================
  protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
  protein_ids <- if (nzchar(protein_col) && protein_col %in% names(ids)) {
    as.character(ids[[protein_col]])
  } else {
    rownames(mat)
  }

  # Normalize protein IDs for matching
  if (id_type == "uniprot") {
    protein_ids_norm <- .canonical_uniprot(protein_ids)
  } else {
    protein_ids_norm <- toupper(trimws(protein_ids))
    protein_ids_norm[protein_ids_norm == ""] <- NA_character_
  }

  # =========================================================
  # Vectorized bucket assignment
  # =========================================================
  add_log("INFO", "Mapping proteins to subcellular buckets...")

  # Match protein IDs to lookup table
  match_idx <- match(protein_ids_norm, lookup_key)
  subloc_raw <- ifelse(is.na(match_idx), NA_character_, lookup_loc[match_idx])

  # Map raw locations to canonical buckets (vectorized)
  buckets <- .map_subloc_bucket(subloc_raw)
  buckets[is.na(buckets) | !nzchar(buckets)] <- "Other/Unknown"

  n_mapped <- sum(!is.na(match_idx))
  n_unmapped <- sum(is.na(match_idx))
  add_log("INFO", sprintf("Mapped %d proteins, %d unmapped (assigned to Other/Unknown)",
                          n_mapped, n_unmapped))

  # =========================================================
  # Apply log transform
  # =========================================================
  if (log_transform == "log10") {
    mat[mat <= 0] <- NA
    mat <- log10(mat)
  } else if (log_transform == "log2") {
    mat[mat <= 0] <- NA
    mat <- log2(mat)
  }

  # =========================================================
  # Build points data (vectorized per group)
  # =========================================================
  add_log("INFO", "Building points data...")

  points_list <- vector("list", length(groups))

  for (g_idx in seq_along(groups)) {
    grp <- groups[g_idx]
    grp_samples <- samples[samples$group_name == grp, , drop = FALSE]
    grp_cols <- intersect(grp_samples$sample_col, colnames(mat))

    if (length(grp_cols) == 0) next

    # Compute mean per protein (vectorized)
    grp_mean <- rowMeans(mat[, grp_cols, drop = FALSE], na.rm = TRUE)

    # Filter valid values
    valid <- !is.na(grp_mean) & is.finite(grp_mean)

    if (sum(valid) == 0) next

    points_list[[g_idx]] <- data.frame(
      bin = buckets[valid],
      group = rep(grp, sum(valid)),
      value = grp_mean[valid],
      protein_id = protein_ids[valid],
      stringsAsFactors = FALSE
    )
  }

  # Combine all groups
  points_df <- do.call(rbind, points_list[!vapply(points_list, is.null, logical(1))])

  if (is.null(points_df) || nrow(points_df) == 0) {
    points_df <- data.frame(
      bin = character(0),
      group = character(0),
      value = numeric(0),
      protein_id = character(0),
      stringsAsFactors = FALSE
    )
  }

  # =========================================================
  # Compute counts per bin (vectorized)
  # =========================================================
  if (nrow(points_df) > 0) {
    # Count unique proteins per bin
    bin_protein <- unique(points_df[, c("bin", "protein_id")])
    counts_table <- table(bin_protein$bin)
    counts_df <- data.frame(
      bin = names(counts_table),
      count = as.integer(counts_table),
      stringsAsFactors = FALSE
    )
  } else {
    counts_df <- data.frame(
      bin = character(0),
      count = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # =========================================================
  # Compute summary stats per bin per group (vectorized)
  # =========================================================
  if (nrow(points_df) > 0) {
    # Use aggregate for summary stats
    summary_n <- aggregate(value ~ bin + group, data = points_df, FUN = length)
    names(summary_n)[3] <- "n"

    summary_mean <- aggregate(value ~ bin + group, data = points_df, FUN = mean, na.rm = TRUE)
    names(summary_mean)[3] <- "mean"

    summary_median <- aggregate(value ~ bin + group, data = points_df, FUN = median, na.rm = TRUE)
    names(summary_median)[3] <- "median"

    summary_df <- merge(summary_n, summary_mean, by = c("bin", "group"))
    summary_df <- merge(summary_df, summary_median, by = c("bin", "group"))
  } else {
    summary_df <- data.frame(
      bin = character(0),
      group = character(0),
      n = integer(0),
      mean = numeric(0),
      median = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Subloc completed in %.2f seconds (%d proteins, %d bins)",
                          engine_duration,
                          length(unique(points_df$protein_id)),
                          length(unique(points_df$bin))))

  # Build log data.frame from accumulated entries
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  # Extract group colors from metadata (same pattern as heatmap/hor_dis)
  groups <- unique(as.character(points_df$group %||% payload$groups %||% character()))
  groups <- groups[nzchar(groups)]
  meta_groups <- payload$metadata$groups
  if (!is.null(meta_groups) && is.data.frame(meta_groups) &&
      "group_name" %in% names(meta_groups) && "color" %in% names(meta_groups)) {
    color_map <- stats::setNames(
      as.character(meta_groups$color),
      as.character(meta_groups$group_name)
    )
    group_colors <- color_map[groups]
    missing <- is.na(group_colors) | !nzchar(group_colors)
    if (any(missing)) {
      auto_colors <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
      group_colors[missing] <- auto_colors
    }
    names(group_colors) <- groups
  } else {
    group_colors <- grDevices::hcl.colors(max(1, length(groups)), palette = "Dark 3")
    group_colors <- group_colors[seq_along(groups)]
    names(group_colors) <- groups
  }

  list(
    engine_id = "subloc",
    params = params,
    data = list(
      points = points_df,
      counts = counts_df,
      summary = summary_df,
      bucket_levels = SUBLOC_LEVELS,
      group_colors = group_colors,
      log = log_df
    )
  )
}
