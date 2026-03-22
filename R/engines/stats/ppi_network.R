# =========================================================
# R/engines/stats/ppi_network.R - PPI Network Engine
#
# Protein-protein interaction network visualization using
# STRING-DB. Supports clustering, centrality analysis, and
# interactive visNetwork rendering.
#
# Contract:
#  - data$nodes: visNetwork nodes data.frame
#  - data$edges: visNetwork edges data.frame
#  - data$graph: igraph object (for layout calculations)
#  - data$stats: network statistics list
#  - data$hubs: hub proteins data.frame
#  - data$centrality: full centrality metrics data.frame
# =========================================================

# Note: %||% operator is defined in R/utils/uniprot.R and sourced before this file

#' Execute PPI Network engine
#'
#' Fetches protein-protein interactions from STRING-DB, builds an igraph
#' network, applies clustering, and prepares visNetwork visualization data.
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_ppi_network_run <- function(payload, params = NULL, context = NULL) {
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

  # Build empty result helper
  empty_result <- function(error_msg = NULL) {
    log_df <- .ppi_build_log_df(log_entries)
    if (!is.null(error_msg)) {
      add_log("ERROR", error_msg)
      log_df <- .ppi_build_log_df(log_entries)
    }
    list(
      engine_id = "ppi_network",
      params = params,
      data = list(
        nodes = data.frame(id = character(0), label = character(0), stringsAsFactors = FALSE),
        edges = data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE),
        graph = NULL,
        stats = list(n_nodes = 0, n_edges = 0, density = 0, avg_degree = 0),
        hubs = data.frame(name = character(0), degree = integer(0), stringsAsFactors = FALSE),
        centrality = data.frame(name = character(0), degree = integer(0), stringsAsFactors = FALSE),
        edges_raw = data.frame(protein1 = character(0), protein2 = character(0), stringsAsFactors = FALSE),
        log = log_df
      )
    )
  }

  # Validate payload
  if (!isTRUE(payload$ok)) {
    return(empty_result(payload$error %||% "Invalid payload"))
  }

  add_log("INFO", "Starting PPI Network analysis")

  # =========================================================
  # Extract parameters with defaults
  # =========================================================
  protein_source <- params$protein_source %||% "de_proteins"
  score_threshold <- as.integer(params$score_threshold %||% 400)
  species <- params$species %||% "9606"
  network_type <- params$network_type %||% "functional"
  clustering_method <- params$clustering_method %||% "louvain"
  max_proteins <- as.integer(params$max_proteins %||% 500)

  add_log("INFO", sprintf("Parameters: source=%s, species=%s, score=%d, network=%s, clustering=%s",
                          protein_source, species, score_threshold, network_type, clustering_method))

  # =========================================================
  # Get protein list based on source
  # =========================================================
  proteins <- .ppi_get_protein_list(payload, params, protein_source, add_log)

  if (length(proteins) == 0) {
    add_log("WARN", "No proteins found for network analysis")
    return(empty_result("No proteins found for network analysis"))
  }

  add_log("INFO", sprintf("Initial protein list: %d proteins", length(proteins)))

  # =========================================================
  # Limit to max_proteins (prioritize by |log2fc| if available)
  # =========================================================
  if (length(proteins) > max_proteins) {
    proteins <- .ppi_limit_proteins(proteins, payload, max_proteins, add_log)
  }

  add_log("INFO", sprintf("Final protein list: %d proteins (max=%d)", length(proteins), max_proteins))

  # =========================================================
  # Source PPI utilities (if not already loaded)
  # =========================================================
  ppi_utils_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "../ppi_utils.R")
  if (!exists("ppi_fetch_string_network", mode = "function")) {
    if (file.exists(ppi_utils_path)) {
      source(ppi_utils_path, local = FALSE)
    } else {
      # Try alternative path for Shiny context
      alt_path <- "R/engines/ppi_utils.R"
      if (file.exists(alt_path)) {
        source(alt_path, local = FALSE)
      }
    }
  }

  # =========================================================
  # Fetch STRING network
  # =========================================================
  add_log("INFO", "Fetching interactions from STRING-DB...")

  edges_raw <- tryCatch({
    ppi_fetch_string_network(
      protein_ids = proteins,
      species = as.integer(species),
      score_threshold = score_threshold,
      network_type = network_type
    )
  }, error = function(e) {
    add_log("ERROR", sprintf("STRING API error: %s", e$message))
    NULL
  })

  if (is.null(edges_raw) || nrow(edges_raw) == 0) {
    add_log("WARN", "No interactions found from STRING-DB")
    return(empty_result("No interactions found from STRING-DB for the given proteins"))
  }

  add_log("INFO", sprintf("Retrieved %d interactions from STRING-DB", nrow(edges_raw)))

  # =========================================================
  # Build node data with attributes from payload
  # =========================================================
  node_data <- .ppi_build_node_data(payload, proteins, add_log)

  # =========================================================
  # Build igraph network
  # =========================================================
  add_log("INFO", "Building igraph network...")

  graph <- ppi_build_network(edges_raw, node_data)

  if (is.null(graph)) {
    add_log("ERROR", "Failed to build network")
    return(empty_result("Failed to build network from STRING edges"))
  }

  n_nodes <- igraph::vcount(graph)
  n_edges <- igraph::ecount(graph)
  add_log("INFO", sprintf("Network built: %d nodes, %d edges", n_nodes, n_edges))

  # =========================================================
  # Apply clustering
  # =========================================================
  add_log("INFO", sprintf("Applying %s clustering...", clustering_method))

  cluster_result <- ppi_cluster_network(graph, clustering_method)
  graph <- cluster_result$graph

  add_log("INFO", sprintf("Clustering complete: %d clusters, modularity=%.3f",
                          cluster_result$n_clusters, cluster_result$modularity %||% NA))

  # =========================================================
  # Compute statistics, hubs, and centrality
  # =========================================================
  add_log("INFO", "Computing network statistics...")

  stats <- ppi_get_network_stats(graph)
  stats$n_clusters <- cluster_result$n_clusters
  stats$modularity <- round(cluster_result$modularity %||% NA, 4)

  hubs <- ppi_find_hubs(graph, top_n = 20)
  centrality <- ppi_compute_centrality(graph)

  add_log("INFO", sprintf("Stats: density=%.4f, avg_degree=%.2f, components=%d",
                          stats$density, stats$avg_degree, stats$n_components))

  # =========================================================
  # Prepare visNetwork data
  # =========================================================
  add_log("INFO", "Preparing visNetwork visualization data...")

  # Get style preferences (defaults for now, viewer can override)
  color_by <- params$color_by %||% "cluster"
  size_by <- params$size_by %||% "degree"
  show_confidence <- params$show_confidence %||% TRUE

  vis_nodes <- ppi_create_vis_nodes(graph, color_by, size_by, node_data)
  vis_edges <- ppi_create_vis_edges(graph, show_confidence)

  add_log("INFO", sprintf("visNetwork data prepared: %d nodes, %d edges",
                          nrow(vis_nodes), nrow(vis_edges)))

  # =========================================================
  # Log completion
  # =========================================================
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("PPI Network analysis completed in %.2f seconds", engine_duration))

  # Build log data.frame
  log_df <- .ppi_build_log_df(log_entries)

  # =========================================================
  # Return contract-compliant result
  # =========================================================
  list(
    engine_id = "ppi_network",
    params = params,
    data = list(
      nodes = vis_nodes,
      edges = vis_edges,
      graph = graph,
      stats = stats,
      hubs = hubs,
      centrality = centrality,
      edges_raw = edges_raw,
      cluster_membership = cluster_result$membership,
      log = log_df
    )
  )
}


# =========================================================
# Internal Helper Functions
# =========================================================

#' Get protein list based on source parameter
#' @noRd
.ppi_get_protein_list <- function(payload, params, protein_source, add_log) {
  proteins <- character(0)

  if (protein_source == "de_proteins") {
    # Get from upstream volcano results or payload sets
    upstream_sets <- payload$upstream$sets %||% payload$sets %||% list()

    sig_up <- upstream_sets$sig_up %||% character(0)
    sig_down <- upstream_sets$sig_down %||% character(0)

    proteins <- unique(c(sig_up, sig_down))
    add_log("INFO", sprintf("DE proteins source: %d up, %d down", length(sig_up), length(sig_down)))

  } else if (protein_source == "go_term") {
    # From GO term selection (future feature)
    # Would come from viewer_state selected GO term members
    go_proteins <- params$go_term_proteins %||% character(0)
    if (length(go_proteins) > 0) {
      proteins <- go_proteins
      add_log("INFO", sprintf("GO term source: %d proteins", length(proteins)))
    }

  } else if (protein_source == "custom") {
    # Parse custom protein list (comma or newline separated)
    custom_str <- params$custom_proteins %||% ""
    if (nzchar(custom_str)) {
      proteins <- trimws(unlist(strsplit(custom_str, "[,\n\r]+")))
      proteins <- proteins[nzchar(proteins)]
      add_log("INFO", sprintf("Custom list source: %d proteins", length(proteins)))
    }
  }

  # Fallback: use all protein IDs from payload
  if (length(proteins) == 0) {
    ids <- payload$ids

    # Try gene_symbol first, then protein column
    gene_col <- payload$metadata$id_gene_col %||% ""
    protein_col <- payload$metadata$id_protein_col %||% ""

    if (nzchar(gene_col) && gene_col %in% names(ids)) {
      proteins <- unique(as.character(ids[[gene_col]]))
      add_log("INFO", sprintf("Fallback to gene column '%s': %d proteins", gene_col, length(proteins)))
    } else if (nzchar(protein_col) && protein_col %in% names(ids)) {
      proteins <- unique(as.character(ids[[protein_col]]))
      add_log("INFO", sprintf("Fallback to protein column '%s': %d proteins", protein_col, length(proteins)))
    } else if (!is.null(ids) && ncol(ids) > 0) {
      # Use first column
      proteins <- unique(as.character(ids[[1]]))
      add_log("INFO", sprintf("Fallback to first ID column: %d proteins", length(proteins)))
    }
  }

  # Remove NA and empty strings
  proteins <- proteins[!is.na(proteins) & nzchar(proteins)]

  proteins
}


#' Limit protein list to max_proteins, prioritizing by |log2fc|
#' @noRd
.ppi_limit_proteins <- function(proteins, payload, max_proteins, add_log) {
  # Try to get fold change data from upstream
  upstream_data <- payload$upstream$comparisons %||% list()

  fc_data <- NULL

  # Check for first comparison's points data
  if (length(upstream_data) > 0) {
    first_comp <- upstream_data[[1]]
    if (!is.null(first_comp$points) && "log2fc" %in% names(first_comp$points)) {
      fc_data <- first_comp$points[, c("gene_symbol", "log2fc"), drop = FALSE]
    }
  }

  if (!is.null(fc_data) && nrow(fc_data) > 0) {
    # Sort by absolute log2fc
    fc_data$abs_fc <- abs(fc_data$log2fc)
    fc_data <- fc_data[order(fc_data$abs_fc, decreasing = TRUE), ]

    # Filter to proteins in our list and take top N
    fc_data <- fc_data[fc_data$gene_symbol %in% proteins, ]
    proteins <- fc_data$gene_symbol[1:min(max_proteins, nrow(fc_data))]
    add_log("INFO", sprintf("Limited to top %d proteins by |log2fc|", length(proteins)))
  } else {
    # No FC data, just take first max_proteins
    proteins <- proteins[1:max_proteins]
    add_log("INFO", sprintf("Limited to first %d proteins (no FC data for ranking)", max_proteins))
  }

  proteins
}


#' Build node data frame with attributes from payload
#' @noRd
.ppi_build_node_data <- function(payload, proteins, add_log) {
  # Start with basic gene_symbol column
  node_data <- data.frame(
    gene_symbol = proteins,
    stringsAsFactors = FALSE
  )

  # Try to get additional attributes from upstream volcano results
  upstream_data <- payload$upstream$comparisons %||% list()

  if (length(upstream_data) > 0) {
    first_comp <- upstream_data[[1]]
    if (!is.null(first_comp$points)) {
      points <- first_comp$points

      # Match proteins to points data
      idx <- match(proteins, points$gene_symbol)

      if ("log2fc" %in% names(points)) {
        node_data$log2fc <- points$log2fc[idx]
      }
      if ("padj" %in% names(points)) {
        node_data$padj <- points$padj[idx]
      }
      if ("significance" %in% names(points)) {
        node_data$significance <- points$significance[idx]
      }

      n_matched <- sum(!is.na(idx))
      add_log("INFO", sprintf("Node attributes: %d/%d proteins matched to upstream data",
                              n_matched, length(proteins)))
    }
  }

  node_data
}


#' Build log data frame from log entries
#' @noRd
.ppi_build_log_df <- function(log_entries) {
  if (length(log_entries) == 0) {
    return(data.frame(
      time = character(0),
      level = character(0),
      message = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))
}


#' Run PPI Network from a bare protein/gene list (viewer-triggered)
#'
#' Convenience wrapper for triggering PPI network analysis from the result
#' viewer (e.g., heatmap clusters, volcano DE proteins, GO term gene lists).
#' Constructs a minimal payload and delegates to stats_ppi_network_run().
#'
#' @param proteins Character vector of protein/gene identifiers
#' @param species STRING-DB species ID (default "9606" for human)
#' @param score_threshold Minimum STRING confidence score (default 400)
#' @param network_type "functional" or "physical" (default "functional")
#' @param clustering_method Community detection method (default "louvain")
#' @param max_proteins Maximum proteins to include (default 500)
#' @return Contract-compliant PPI network results
stats_ppi_network_run_from_list <- function(proteins,
                                             species = "9606",
                                             score_threshold = 400,
                                             network_type = "functional",
                                             clustering_method = "louvain",
                                             max_proteins = 500) {
  # Remove NA and empty strings
  proteins <- unique(proteins[!is.na(proteins) & nzchar(proteins)])

  if (length(proteins) == 0) {
    return(list(
      engine_id = "ppi_network",
      params = list(),
      data = list(
        nodes = data.frame(id = character(0), label = character(0), stringsAsFactors = FALSE),
        edges = data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE),
        graph = NULL,
        stats = list(n_nodes = 0, n_edges = 0, density = 0, avg_degree = 0),
        hubs = data.frame(name = character(0), degree = integer(0), stringsAsFactors = FALSE),
        centrality = data.frame(name = character(0), degree = integer(0), stringsAsFactors = FALSE),
        edges_raw = data.frame(protein1 = character(0), protein2 = character(0), stringsAsFactors = FALSE),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No proteins provided", stringsAsFactors = FALSE)
      )
    ))
  }

  # Build minimal payload
  payload <- list(
    ok = TRUE,
    ids = data.frame(gene_symbol = proteins, stringsAsFactors = FALSE),
    metadata = list(id_gene_col = "gene_symbol"),
    upstream = list(sets = list(sig_up = proteins, sig_down = character(0)))
  )

  params <- list(
    protein_source = "de_proteins",
    species = as.character(species),
    score_threshold = as.integer(score_threshold),
    network_type = network_type,
    clustering_method = clustering_method,
    max_proteins = as.integer(max_proteins)
  )

  stats_ppi_network_run(payload, params)
}
