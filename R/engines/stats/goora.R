# =========================================================
# R/engines/stats/goora.R — GO Over-Representation Analysis Engine
#
# Performs GO-ORA on proteinID sets with required fold_enrichment.
#
# Contract v1.1:
#  - data$terms: term_id, term_name, fdr, fold_enrichment, n_genes
#  - Input is proteinID sets from volcano/pca
# =========================================================

#' Execute goora engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with terpbase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_goora_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", "Initiating GO-ORA analysis")

  # Detect data type for database selection (ensure scalar)
  data_type <- as.character(payload$data_type %||% payload$metadata$data_type %||% context$data_type %||% "proteomics")[1]

  # Get parameters (ensure scalar)
  database <- as.character(params$database %||% "go")[1]
  ontology <- as.character(params$ontology %||% "all")[1]
  fdr_cutoff <- as.numeric(params$fdr_cutoff %||% 0.03)[1]
  min_term_size <- as.integer(params$min_term_size %||% 5)[1]
  min_overlap <- as.integer(params$min_overlap %||% 3)[1]
  max_terms <- as.integer(params$max_terms %||% 20)[1]

  add_log("INFO", sprintf("Database mode: %s", database))
  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, min_overlap=%d, ontology=%s",
                          fdr_cutoff, min_term_size, min_overlap, ontology))

  # Guard: protein complex enrichment is not applicable to metabolomics data
  if (identical(database, "complex") && identical(data_type, "metabolomics")) {
    add_log("ERROR", "Protein complex enrichment is not applicable to metabolomics data")
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0), term_name = character(0), ontology = character(0),
          fdr = numeric(0), fold_enrichment = numeric(0), n_genes = integer(0),
          protein_ids = character(0), stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "Protein complex enrichment is not applicable to metabolomics data. Use GO or pathway databases instead.",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Get query proteins (from frozen parent sets)
  # Priority: payload$query_proteins > params > context
  query_proteins <- payload$query_proteins %||% params$query_proteins %||% context$query_proteins %||% character(0)

  if (length(query_proteins) == 0) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No query proteins provided for GO-ORA",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Source shared enrichment utilities
  if (!exists("build_term_proteins", mode = "function")) {
    source("R/engines/enrichment_utils.R", local = TRUE)
  }

  # Initialize term_proteins and term_info based on database mode
  term_proteins <- list()
  term_info <- NULL
  background <- NULL

  # Allow callers to pre-build term_proteins (e.g., cluster loop optimization)
  if (!is.null(payload$term_proteins) && length(payload$term_proteins) > 0) {
    term_proteins <- payload$term_proteins
    term_info <- payload$term_info %||% NULL
    background <- payload$background %||% NULL
    add_log("INFO", sprintf("Using pre-built term_proteins (%d terms)", length(term_proteins)))
  } else if (identical(database, "complex")) {
    # =========================================================
    # Complex enrichment mode: use complexbase
    # =========================================================
    complexbase <- payload$complexbase %||% context$complexbase

    if (is.null(complexbase)) {
      return(list(
        engine_id = "goora",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            fold_enrichment = numeric(0),
            n_genes = integer(0),
            protein_ids = character(0),
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

    # Build term-to-protein mapping (gene symbols from embedded gene_symbol column)
    term_proteins <- complexbase_build_term_proteins(complexbase)
    term_info <- complexbase_get_term_info(complexbase)

    add_log("INFO", sprintf("Loaded %d protein complex mappings", length(term_proteins)))

  } else if (identical(data_type, "metabolomics")) {
    # =========================================================
    # KEGG pathway enrichment mode: use metabobase
    # =========================================================
    metabobase <- payload$metabobase %||% context$metabobase

    if (is.null(metabobase)) {
      return(list(
        engine_id = "goora",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            fold_enrichment = numeric(0),
            n_genes = integer(0),
            protein_ids = character(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Metabolomics enrichment requires metabobase",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Extract KEGG pathway mappings from metabobase
    # Expected structure: metabobase$compound_to_pathway (compound to KEGG mapping)
    compound_to_pathway <- metabobase$compound_to_pathway %||% metabobase$compound_terms %||% NULL
    pathway_info <- metabobase$pathway_info %||% metabobase$terms %||% NULL

    # Build term-to-compound mapping (vectorized)
    if (!is.null(compound_to_pathway)) {
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
        engine_id = "goora",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            fold_enrichment = numeric(0),
            n_genes = integer(0),
            protein_ids = character(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Proteomics enrichment requires terpbase with GO annotations",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Extract GO mappings from terpbase
    # Expected structure: terpbase$go_terms (term annotations) and
    # terpbase$protein_to_go (protein to term mapping)

    go_terms <- terpbase$go_terms %||% terpbase$terms %||% NULL
    protein_to_go <- terpbase$protein_to_go %||% terpbase$protein_terms %||% NULL
    background <- terpbase$background %||% terpbase$universe %||% NULL

    if (is.null(protein_to_go)) {
      # Try to build from annotations
      annotations <- terpbase$annotations %||% NULL
      if (!is.null(annotations) && "go_ids" %in% names(annotations)) {
        protein_to_go <- annotations
      }
    }

    if (is.null(protein_to_go)) {
      return(list(
        engine_id = "goora",
        params = params,
        data = list(
          terms = data.frame(
            term_id = character(0),
            term_name = character(0),
            ontology = character(0),
            fdr = numeric(0),
            fold_enrichment = numeric(0),
            n_genes = integer(0),
            protein_ids = character(0),
            stringsAsFactors = FALSE
          ),
          log = data.frame(time = format(Sys.time()), level = "ERROR",
                           message = "Terpbase missing protein_to_go mapping",
                           stringsAsFactors = FALSE)
        )
      ))
    }

    # Build term-to-protein mapping (vectorized)
    term_proteins <- build_term_proteins(protein_to_go)

    # Store go_terms for term info lookup
    term_info <- go_terms

    add_log("INFO", sprintf("Loaded %d GO term mappings", length(term_proteins)))
  }

  if (length(term_proteins) == 0) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = sprintf("No valid %s term mappings found",
                                           if (identical(database, "complex")) "complex" else "GO"),
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Define background universe
  if (is.null(background)) {
    background <- unique(unlist(term_proteins))
  }
  N <- length(background)  # Total proteins in universe

  # Query set
  query_set <- intersect(query_proteins, background)
  n <- length(query_set)  # Proteins in query set

  add_log("INFO", sprintf("Query: %d proteins, Background: %d proteins",
                          n, N))

  if (n == 0) {
    # Log diagnostic info to help debug ID format mismatches
    sample_query <- head(query_proteins, 3)
    sample_bg <- head(background, 3)
    add_log("DEBUG", sprintf(
      "ID mismatch? Query sample: [%s] | Background sample: [%s]",
      paste(sample_query, collapse = ", "),
      paste(sample_bg, collapse = ", ")))

    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No query proteins found in background universe",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # NOTE: As of v1.5, GO engines always compute ALL ontologies (BP, CC, MF).
  # The params$ontology setting is now deprecated in favor of style$ontology_filter
  # which filters at render/viewer time. This allows users to change ontology filter
  # without re-running the analysis.
  # Keeping filter_ontology = NULL to ensure all terms are computed.
  filter_ontology <- NULL

  # Perform hypergeometric test for each term
  results <- list()

  for (term_id in names(term_proteins)) {
    term_pids <- unique(term_proteins[[term_id]])
    M <- length(term_pids)  # Proteins annotated to this term

    # Skip small terms
    if (M < min_term_size) next

    # Overlap with query
    k <- length(intersect(query_set, term_pids))
    if (k == 0) next

    # Hypergeometric test (over-representation)
    # P(X >= k) where X ~ Hypergeometric(N, M, n)
    pval <- phyper(k - 1, M, N - M, n, lower.tail = FALSE)

    # Fold enrichment = (k/n) / (M/N)
    expected <- (n * M) / N
    fold_enrichment <- k / expected

    # Get term name and ontology
    term_name <- term_id
    term_ont <- "unknown"

    if (!is.null(term_info)) {
      if (is.data.frame(term_info)) {
        # Works for both complexbase (term_id, term_name, ontology) and GO (go_id/term_id, term_name/name, ontology/namespace)
        term_row <- term_info[term_info$term_id == term_id |
                              (if ("go_id" %in% names(term_info)) term_info$go_id == term_id else FALSE), , drop = FALSE]
        if (nrow(term_row) > 0) {
          name_col_candidates <- intersect(c("term_name", "name", "description"), names(term_row))
          ont_col_candidates <- intersect(c("ontology", "namespace", "ont"), names(term_row))
          name_col <- if (length(name_col_candidates) > 0) name_col_candidates[1] else NA_character_
          ont_col <- if (length(ont_col_candidates) > 0) ont_col_candidates[1] else NA_character_
          if (!is.na(name_col)) term_name <- as.character(term_row[[name_col]][1])
          if (!is.na(ont_col)) term_ont <- as.character(term_row[[ont_col]][1])
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

    results <- c(results, list(list(
      term_id = term_id,
      term_name = term_name,
      ontology = term_ont,
      pval = pval,
      fold_enrichment = fold_enrichment,
      n_genes = k,
      n_term = M,
      protein_ids = paste(intersect(query_set, term_pids), collapse = ",")
    )))
  }

  if (length(results) == 0) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "INFO",
                         message = sprintf("%s-ORA: No enriched terms found (query: %d proteins)",
                                           if (identical(database, "complex")) "Complex" else "GO", n),
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Build results data.frame via direct vector extraction (avoids 2-3x peak
  # memory overhead from do.call(rbind, lapply(...)) with intermediate copies)
  terms_df <- data.frame(
    term_id = vapply(results, `[[`, "", "term_id"),
    term_name = vapply(results, `[[`, "", "term_name"),
    ontology = vapply(results, `[[`, "", "ontology"),
    pval = vapply(results, `[[`, 0, "pval"),
    fold_enrichment = vapply(results, `[[`, 0, "fold_enrichment"),
    n_genes = vapply(results, `[[`, 0L, "n_genes"),
    n_term = vapply(results, `[[`, 0L, "n_term"),
    protein_ids = vapply(results, `[[`, "", "protein_ids"),
    stringsAsFactors = FALSE
  )

  # Compute FDR (BH adjustment)
  terms_df$fdr <- p.adjust(terms_df$pval, method = "BH")

  # Filter by FDR cutoff
  terms_df <- terms_df[terms_df$fdr <= fdr_cutoff, , drop = FALSE]

  # Filter by min_overlap (minimum query proteins overlapping with term)
  if (min_overlap > 1 && "n_genes" %in% names(terms_df) && nrow(terms_df) > 0) {
    before_count <- nrow(terms_df)
    terms_df <- terms_df[terms_df$n_genes >= min_overlap, , drop = FALSE]
    add_log("INFO", sprintf("Min overlap filter (n_genes >= %d): %d -> %d terms",
                            min_overlap, before_count, nrow(terms_df)))
  }

  # Sort by FDR, then fold_enrichment
  terms_df <- terms_df[order(terms_df$fdr, -terms_df$fold_enrichment), , drop = FALSE]

  # Limit to max_terms PER ONTOLOGY (not globally)
  # This ensures we get up to max_terms for BP, MF, and CC each
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
    # Re-sort after combining
    terms_df <- terms_df[order(terms_df$fdr, -terms_df$fold_enrichment), , drop = FALSE]
  } else if (nrow(terms_df) > max_terms) {
    # Fallback for data without ontology column
    terms_df <- terms_df[1:max_terms, , drop = FALSE]
  }

  # Final column order
  terms_df <- terms_df[, c("term_id", "term_name", "ontology", "fdr",
                           "fold_enrichment", "n_genes", "n_term", "protein_ids"), drop = FALSE]
  rownames(terms_df) <- NULL

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("%s-ORA completed in %.2f seconds: %d enriched terms (FDR <= %.3f)",
                          if (identical(database, "complex")) "Complex" else "GO",
                          engine_duration,
                          nrow(terms_df),
                          fdr_cutoff))

  # Build log data.frame from accumulated entries
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  list(
    engine_id = "goora",
    params = params,
    data = list(
      terms = terms_df,
      query_info = list(
        n_query = n,
        n_background = N,
        direction = params$direction %||% "unknown"
      ),
      log = log_df
    )
  )
}
