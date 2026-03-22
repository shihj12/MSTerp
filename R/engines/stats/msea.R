# =========================================================
# R/engines/stats/msea.R - Metabolite Set Enrichment Analysis Engine
#
# Performs pathway over-representation analysis on metabolite sets
# using KEGG and/or Reactome pathways from MetaboBase.
#
# Contract:
#  - data$terms: pathway_id, pathway_name, pathway_type, fdr, fold_enrichment, n_metabolites, metabolite_ids
#  - Input is metabolite ID sets from volcano/pca or user-provided list
# =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Execute MSEA engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with metabobase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_msea_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "msea",
      params = params,
      data = list(
        terms = data.frame(
          pathway_id = character(0),
          pathway_name = character(0),
          pathway_type = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
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

  add_log("INFO", "Initiating MSEA (Metabolite Set Enrichment Analysis)")

  # Get parameters
  fdr_cutoff <- params$fdr_cutoff %||% 0.05
  min_pathway_size <- params$min_pathway_size %||% 3
  min_overlap <- params$min_overlap %||% 2
  max_terms <- params$max_terms %||% 20

  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, min_overlap=%d",
                          fdr_cutoff, min_pathway_size, min_overlap))

  # Get query metabolites (from frozen parent sets)
  # Priority: payload$query_metabolites > params > context
  query_metabolites <- payload$query_metabolites %||% params$query_metabolites %||% context$query_metabolites %||% character(0)

  # Also try protein-style naming for compatibility
  if (length(query_metabolites) == 0) {
    query_metabolites <- payload$query_proteins %||% params$query_proteins %||% context$query_proteins %||% character(0)
  }

  if (length(query_metabolites) == 0) {
    add_log("WARN", "No query metabolites provided for MSEA")
    return(empty_result(warn_msg = "No query metabolites provided for MSEA"))
  }

  query_metabolites <- unique(as.character(query_metabolites))
  add_log("INFO", sprintf("Query set contains %d unique metabolites", length(query_metabolites)))

  # Get metabobase mappings
  metabobase <- payload$metabobase %||% context$metabobase

  if (is.null(metabobase)) {
    add_log("ERROR", "MSEA requires metabobase with pathway annotations")
    return(empty_result(error_msg = "MSEA requires metabobase with pathway annotations"))
  }

  # Extract pathway mappings from metabobase
  annot_long <- metabobase$annot_long
  terms_by_id <- metabobase$terms_by_id

  if (is.null(annot_long) || !is.data.frame(annot_long) || nrow(annot_long) == 0) {
    add_log("ERROR", "MetaboBase missing pathway annotations (annot_long)")
    return(empty_result(error_msg = "MetaboBase missing pathway annotations"))
  }

  # Log available pathway types
  pw_types <- unique(annot_long$pathway_type)
  add_log("INFO", sprintf("Pathway types in MetaboBase: %s (%d total annotations)",
                          paste(pw_types, collapse = ", "), nrow(annot_long)))

  # Build term-to-metabolite mapping
  term_metabolites <- split(annot_long$metabolite_id, annot_long$pathway_id)

  # Get background (all metabolites with annotations)
  background <- unique(annot_long$metabolite_id)
  n_background <- length(background)
  add_log("INFO", sprintf("Background universe: %d metabolites with pathway annotations", n_background))

  # Multi-layer ID reconciliation (shared helper from metabobase.R)
  reconciled <- metabobase_reconcile_ids(
    query_ids = query_metabolites,
    metabobase = metabobase,
    use_pubchem_resolver = TRUE,
    log_callback = add_log
  )
  query_id_map <- reconciled$id_map

  # Replace query_metabolites with resolved IDs
  resolved_ids <- unique(as.character(query_id_map))
  resolved_ids <- resolved_ids[resolved_ids %in% background]

  # Filter query to those in background
  query_in_bg <- resolved_ids
  n_query <- length(query_in_bg)

  if (n_query == 0) {
    add_log("WARN", "No query metabolites matched MetaboBase background after ID reconciliation")
    add_log("DEBUG", sprintf("Query sample: %s", paste(head(query_metabolites, 5), collapse = ", ")))
    add_log("DEBUG", sprintf("Background sample: %s", paste(head(background, 5), collapse = ", ")))
    return(empty_result(warn_msg = "No query metabolites found in pathway database"))
  }

  add_log("INFO", sprintf("Query metabolites in background: %d of %d (%.1f%%)",
                          n_query, length(query_metabolites),
                          100 * n_query / max(1, length(query_metabolites))))

  # Get pathway names from terms_by_id if available
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

  # Also try to get pathway type from annot_long
  if (all(is.na(pathway_types))) {
    for (pw_id in names(term_metabolites)) {
      pt <- annot_long$pathway_type[annot_long$pathway_id == pw_id][1]
      if (!is.na(pt)) pathway_types[pw_id] <- pt
    }
  }

  # Perform hypergeometric test for each pathway
  results <- list()

  for (pathway_id in names(term_metabolites)) {
    pathway_mets <- unique(term_metabolites[[pathway_id]])
    n_pathway <- length(pathway_mets)

    # Skip small pathways
    if (n_pathway < min_pathway_size) next

    # Count overlap
    overlap <- intersect(query_in_bg, pathway_mets)
    n_overlap <- length(overlap)

    # Skip if overlap too small
    if (n_overlap < min_overlap) next

    # Hypergeometric test
    # phyper(q, m, n, k, lower.tail = FALSE)
    # q: number of white balls drawn (overlap - 1 for P(X > q))
    # m: number of white balls in urn (pathway size)
    # n: number of black balls in urn (background - pathway)
    # k: number of balls drawn (query size)
    pval <- stats::phyper(
      n_overlap - 1,
      n_pathway,
      n_background - n_pathway,
      n_query,
      lower.tail = FALSE
    )

    # Calculate fold enrichment
    expected <- (n_query * n_pathway) / n_background
    fold_enrichment <- n_overlap / max(expected, 1e-10)

    results <- c(results, list(data.frame(
      pathway_id = pathway_id,
      pathway_name = pathway_names[pathway_id] %||% pathway_id,
      pathway_type = pathway_types[pathway_id] %||% "unknown",
      pval = pval,
      n_metabolites = n_overlap,
      pathway_size = n_pathway,
      fold_enrichment = fold_enrichment,
      metabolite_ids = paste(overlap, collapse = ";"),
      stringsAsFactors = FALSE
    )))
  }

  if (length(results) == 0) {
    add_log("WARN", "No pathways passed filtering criteria")
    return(empty_result(warn_msg = "No pathways passed filtering criteria"))
  }

  # Combine results
  results_df <- do.call(rbind, results)

  # FDR correction
  results_df$fdr <- stats::p.adjust(results_df$pval, method = "BH")

  # Filter by FDR
  results_df <- results_df[results_df$fdr <= fdr_cutoff, , drop = FALSE]

  if (nrow(results_df) == 0) {
    add_log("INFO", sprintf("No pathways significant at FDR <= %.3f", fdr_cutoff))
    return(empty_result(warn_msg = sprintf("No pathways significant at FDR <= %.3f", fdr_cutoff)))
  }

  # Sort by FDR, then by fold_enrichment
  results_df <- results_df[order(results_df$fdr, -results_df$fold_enrichment), , drop = FALSE]

  # Limit results per pathway type if "all" databases selected
  # Limit per pathway_type to keep balance across databases
  final_results <- list()
  for (pt in unique(results_df$pathway_type)) {
    pt_df <- results_df[results_df$pathway_type == pt, , drop = FALSE]
    pt_df <- head(pt_df, max_terms)
    final_results <- c(final_results, list(pt_df))
  }
  results_df <- do.call(rbind, final_results)

  add_log("INFO", sprintf("MSEA complete: %d significant pathways found", nrow(results_df)))

  # Build reverse lookup: metabolite_id -> display name
  id_to_name <- character(0)
  if (!is.null(metabobase$metabolite_meta) && is.data.frame(metabobase$metabolite_meta) &&
      "metabolite_id" %in% names(metabobase$metabolite_meta) && "name" %in% names(metabobase$metabolite_meta)) {
    id_to_name <- stats::setNames(metabobase$metabolite_meta$name, metabobase$metabolite_meta$metabolite_id)
  }

  # Add metabolite_names column (human-readable names for the KEGG IDs)
  metabolite_names_col <- vapply(results_df$metabolite_ids, function(ids_str) {
    ids <- trimws(strsplit(ids_str, ";")[[1]])
    names_vec <- ifelse(ids %in% names(id_to_name), id_to_name[ids], ids)
    paste(names_vec, collapse = ";")
  }, character(1), USE.NAMES = FALSE)

  # Build output
  terms_out <- data.frame(
    pathway_id = results_df$pathway_id,
    pathway_name = results_df$pathway_name,
    pathway_type = results_df$pathway_type,
    fdr = results_df$fdr,
    fold_enrichment = results_df$fold_enrichment,
    n_metabolites = results_df$n_metabolites,
    metabolite_ids = results_df$metabolite_ids,
    metabolite_names = metabolite_names_col,
    stringsAsFactors = FALSE
  )

  # Build log dataframe
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  # Record runtime
  runtime_sec <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("MSEA engine completed in %.2f seconds", runtime_sec))
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  list(
    engine_id = "msea",
    params = params,
    data = list(
      terms = terms_out,
      query_count = n_query,
      background_count = n_background,
      log = log_df
    )
  )
}
