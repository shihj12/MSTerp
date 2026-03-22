# =========================================================
# R/engines/stats/class_enrichment.R - Chemical Class Enrichment Engine
#
# Performs over-representation analysis on chemical classes
# (lipids, amino acids, carbohydrates, etc.) using MetaboBase.
#
# Contract:
#  - data$terms: class_name, class_level, fdr, fold_enrichment, n_metabolites, metabolite_ids
#  - Input is metabolite ID sets from volcano/pca or user-provided list
# =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Execute chemical class enrichment engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with metabobase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_class_enrichment_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "class_enrichment",
      params = params,
      data = list(
        terms = data.frame(
          class_name = character(0),
          class_level = character(0),
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

  add_log("INFO", "Initiating Chemical Class Enrichment Analysis")

  # Get parameters
  class_level <- params$class_level %||% "class"  # superclass, class, or subclass
  fdr_cutoff <- params$fdr_cutoff %||% 0.05
  min_class_size <- params$min_class_size %||% 3
  max_terms <- params$max_terms %||% 20

  add_log("INFO", sprintf("Parameters: class_level=%s, FDR <= %.3f, min_size=%d",
                          class_level, fdr_cutoff, min_class_size))

  # Get query metabolites
  query_metabolites <- payload$query_metabolites %||% params$query_metabolites %||% context$query_metabolites %||% character(0)

  # Also try protein-style naming for compatibility
  if (length(query_metabolites) == 0) {
    query_metabolites <- payload$query_proteins %||% params$query_proteins %||% context$query_proteins %||% character(0)
  }

  if (length(query_metabolites) == 0) {
    add_log("WARN", "No query metabolites provided for class enrichment")
    return(empty_result(warn_msg = "No query metabolites provided for class enrichment"))
  }

  query_metabolites <- unique(as.character(query_metabolites))
  add_log("INFO", sprintf("Query set contains %d unique metabolites", length(query_metabolites)))

  # Get metabobase mappings
  metabobase <- payload$metabobase %||% context$metabobase

  if (is.null(metabobase)) {
    add_log("ERROR", "Class enrichment requires metabobase with class mappings")
    return(empty_result(error_msg = "Class enrichment requires metabobase with class mappings"))
  }

  # Extract class mappings from metabobase
  class_mappings <- metabobase$class_mappings

  if (is.null(class_mappings) || !is.data.frame(class_mappings) || nrow(class_mappings) == 0) {
    add_log("ERROR", "MetaboBase missing class mappings")
    return(empty_result(error_msg = "MetaboBase missing chemical class annotations"))
  }

  # Check class level column exists
  if (!class_level %in% names(class_mappings)) {
    add_log("ERROR", sprintf("Class level '%s' not found in MetaboBase", class_level))
    return(empty_result(error_msg = sprintf("Class level '%s' not available in MetaboBase", class_level)))
  }

  # Filter out rows with missing class values
  class_mappings <- class_mappings[!is.na(class_mappings[[class_level]]) &
                                     nzchar(as.character(class_mappings[[class_level]])), , drop = FALSE]

  if (nrow(class_mappings) == 0) {
    add_log("WARN", sprintf("No metabolites have '%s' annotations", class_level))
    return(empty_result(warn_msg = sprintf("No metabolites have '%s' annotations", class_level)))
  }

  # Build class-to-metabolite mapping
  class_metabolites <- split(class_mappings$metabolite_id, class_mappings[[class_level]])

  # Get background (all metabolites with class annotations)
  background <- unique(class_mappings$metabolite_id)
  n_background <- length(background)
  add_log("INFO", sprintf("Background universe: %d metabolites with class annotations", n_background))

  # Filter query to those in background
  query_in_bg <- intersect(query_metabolites, background)
  n_query <- length(query_in_bg)

  if (n_query == 0) {
    add_log("WARN", "No query metabolites found in MetaboBase class annotations")
    return(empty_result(warn_msg = "No query metabolites found with class annotations"))
  }

  add_log("INFO", sprintf("Query metabolites in background: %d of %d (%.1f%%)",
                          n_query, length(query_metabolites),
                          100 * n_query / max(1, length(query_metabolites))))

  # Perform hypergeometric test for each class
  results <- list()

  for (class_name in names(class_metabolites)) {
    class_mets <- unique(class_metabolites[[class_name]])
    n_class <- length(class_mets)

    # Skip small classes
    if (n_class < min_class_size) next

    # Count overlap
    overlap <- intersect(query_in_bg, class_mets)
    n_overlap <- length(overlap)

    # Skip if no overlap
    if (n_overlap == 0) next

    # Hypergeometric test
    pval <- stats::phyper(
      n_overlap - 1,
      n_class,
      n_background - n_class,
      n_query,
      lower.tail = FALSE
    )

    # Calculate fold enrichment
    expected <- (n_query * n_class) / n_background
    fold_enrichment <- n_overlap / max(expected, 1e-10)

    results <- c(results, list(data.frame(
      class_name = class_name,
      class_level = class_level,
      pval = pval,
      n_metabolites = n_overlap,
      class_size = n_class,
      fold_enrichment = fold_enrichment,
      metabolite_ids = paste(overlap, collapse = ";"),
      stringsAsFactors = FALSE
    )))
  }

  if (length(results) == 0) {
    add_log("WARN", "No classes had overlap with query metabolites")
    return(empty_result(warn_msg = "No classes had overlap with query metabolites"))
  }

  # Combine results
  results_df <- do.call(rbind, results)

  # FDR correction
  results_df$fdr <- stats::p.adjust(results_df$pval, method = "BH")

  # Filter by FDR
  results_df <- results_df[results_df$fdr <= fdr_cutoff, , drop = FALSE]

  if (nrow(results_df) == 0) {
    add_log("INFO", sprintf("No classes significant at FDR <= %.3f", fdr_cutoff))
    return(empty_result(warn_msg = sprintf("No classes significant at FDR <= %.3f", fdr_cutoff)))
  }

  # Sort by FDR, then by fold_enrichment
  results_df <- results_df[order(results_df$fdr, -results_df$fold_enrichment), , drop = FALSE]

  # Limit results
  results_df <- head(results_df, max_terms)

  add_log("INFO", sprintf("Class enrichment complete: %d significant classes found", nrow(results_df)))

  # Build output
  terms_out <- data.frame(
    class_name = results_df$class_name,
    class_level = results_df$class_level,
    fdr = results_df$fdr,
    fold_enrichment = results_df$fold_enrichment,
    n_metabolites = results_df$n_metabolites,
    metabolite_ids = results_df$metabolite_ids,
    stringsAsFactors = FALSE
  )

  # Build log dataframe
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  # Record runtime
  runtime_sec <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Class enrichment engine completed in %.2f seconds", runtime_sec))
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  list(
    engine_id = "class_enrichment",
    params = params,
    data = list(
      terms = terms_out,
      query_count = n_query,
      background_count = n_background,
      log = log_df
    )
  )
}
