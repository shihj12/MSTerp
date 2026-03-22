# =========================================================
# R/engines/ppi_utils.R - PPI Network Utilities
#
# STRING-DB API functions, igraph network utilities, and
# visNetwork visualization helpers for the PPI Network engine.
#
# Functions are prefixed with ppi_ to avoid namespace collisions.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =========================================================
# STRING-DB API Functions
# =========================================================

#' Fetch protein network from STRING-DB
#'
#' Retrieves protein-protein interactions from STRING database for a set of
#' protein identifiers. Handles batching for large queries (>400 proteins)
#' with rate limiting between batches.
#'
#' @param protein_ids Character vector of protein identifiers (gene symbols or UniProt IDs)
#' @param species Numeric species taxonomy ID (9606 for human, 10090 for mouse)
#' @param score_threshold Minimum combined score (0-1000, default 400 = medium confidence)
#' @param network_type Type of network: "functional" (default) or "physical"
#'
#' @return Data frame with columns: protein1, protein2, combined_score, stringId_A, stringId_B,
#'         and optional evidence score columns (neighborhood, fusion, cooccurrence, etc.)
ppi_fetch_string_network <- function(protein_ids,
                                      species = 9606,
                                      score_threshold = 400,
                                      network_type = "functional") {

  if (length(protein_ids) == 0) {
    return(data.frame(
      protein1 = character(0),
      protein2 = character(0),
      combined_score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # STRING API has a limit of ~400 proteins per request
  batch_size <- 400
  n_proteins <- length(protein_ids)

  if (n_proteins > batch_size) {
    message(sprintf("Batching %d proteins into %d requests...",
                    n_proteins, ceiling(n_proteins / batch_size)))

    batches <- split(protein_ids, ceiling(seq_along(protein_ids) / batch_size))
    all_results <- lapply(batches, function(batch) {
      Sys.sleep(0.5)  # Rate limiting between batches
      ppi_fetch_string_batch(batch, species, score_threshold, network_type)
    })

    result <- do.call(rbind, all_results)
    if (!is.null(result) && nrow(result) > 0) {
      result <- unique(result)
    }
    return(result)
  }

  ppi_fetch_string_batch(protein_ids, species, score_threshold, network_type)
}


#' Internal batch helper for STRING API fetch
#'
#' @param protein_ids Character vector of protein identifiers (max ~400)
#' @param species Species taxonomy ID
#' @param score_threshold Minimum combined score
#' @param network_type Network type
#'
#' @return Data frame with interaction data or empty data frame on error
#' @noRd
ppi_fetch_string_batch <- function(protein_ids, species, score_threshold, network_type) {

  base_url <- "https://string-db.org/api/json/network"

  # Prepare identifiers - URL encode the newline separator
  identifiers <- paste(protein_ids, collapse = "%0d")

  # Build query parameters
  query_params <- list(
    identifiers = identifiers,
    species = species,
    required_score = score_threshold,
    network_type = network_type,
    caller_identity = "MSTerp_PPI"
  )

  tryCatch({
    response <- httr::GET(
      base_url,
      query = query_params,
      httr::timeout(30)
    )

    if (httr::status_code(response) != 200) {
      warning(sprintf("STRING API returned status %d", httr::status_code(response)))
      return(data.frame(
        protein1 = character(0),
        protein2 = character(0),
        combined_score = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)

    if (length(data) == 0 || nrow(data) == 0) {
      message("No interactions found for the given proteins")
      return(data.frame(
        protein1 = character(0),
        protein2 = character(0),
        combined_score = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    # Extract relevant columns
    result <- data.frame(
      protein1 = data$preferredName_A,
      protein2 = data$preferredName_B,
      stringId_A = data$stringId_A,
      stringId_B = data$stringId_B,
      combined_score = data$score,
      stringsAsFactors = FALSE
    )

    # Add individual evidence scores if available
    if ("nscore" %in% names(data)) result$neighborhood_score <- data$nscore
    if ("fscore" %in% names(data)) result$fusion_score <- data$fscore
    if ("pscore" %in% names(data)) result$cooccurrence_score <- data$pscore
    if ("ascore" %in% names(data)) result$coexpression_score <- data$ascore
    if ("escore" %in% names(data)) result$experimental_score <- data$escore
    if ("dscore" %in% names(data)) result$database_score <- data$dscore
    if ("tscore" %in% names(data)) result$textmining_score <- data$tscore

    return(result)

  }, error = function(e) {
    warning(sprintf("Error fetching STRING data: %s", e$message))
    return(data.frame(
      protein1 = character(0),
      protein2 = character(0),
      combined_score = numeric(0),
      stringsAsFactors = FALSE
    ))
  })
}


#' Map identifiers using STRING API
#'
#' Maps protein identifiers (gene symbols, UniProt IDs, etc.) to STRING internal IDs.
#' Useful for resolving ambiguous identifiers before network fetch.
#'
#' @param identifiers Character vector of protein identifiers
#' @param species Species taxonomy ID (default 9606 for human)
#'
#' @return Data frame with columns: query, stringId, preferredName, annotation,
#'         or NULL if mapping fails
ppi_map_string_identifiers <- function(identifiers, species = 9606) {

  base_url <- "https://string-db.org/api/json/get_string_ids"

  query_params <- list(
    identifiers = paste(identifiers, collapse = "%0d"),
    species = species,
    caller_identity = "MSTerp_PPI"
  )

  tryCatch({
    response <- httr::GET(
      base_url,
      query = query_params,
      httr::timeout(30)
    )

    if (httr::status_code(response) != 200) {
      warning(sprintf("STRING ID mapping returned status %d", httr::status_code(response)))
      return(NULL)
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)

    if (length(data) == 0) {
      return(NULL)
    }

    return(data.frame(
      query = data$queryItem,
      stringId = data$stringId,
      preferredName = data$preferredName,
      annotation = data$annotation,
      stringsAsFactors = FALSE
    ))

  }, error = function(e) {
    warning(sprintf("Error mapping STRING IDs: %s", e$message))
    return(NULL)
  })
}


#' Fetch STRING functional enrichment
#'
#' Retrieves functional enrichment analysis (GO terms, KEGG pathways, etc.)
#' for a set of proteins from STRING-DB.
#'
#' @param protein_ids Character vector of protein identifiers
#' @param species Species taxonomy ID (default 9606 for human)
#'
#' @return Data frame with enrichment results, or NULL if fetch fails
ppi_fetch_string_enrichment <- function(protein_ids, species = 9606) {

  base_url <- "https://string-db.org/api/json/enrichment"

  query_params <- list(
    identifiers = paste(protein_ids, collapse = "%0d"),
    species = species,
    caller_identity = "MSTerp_PPI"
  )

  tryCatch({
    response <- httr::GET(
      base_url,
      query = query_params,
      httr::timeout(60)
    )

    if (httr::status_code(response) != 200) {
      return(NULL)
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)

    if (length(data) == 0) {
      return(NULL)
    }

    return(data)

  }, error = function(e) {
    warning(sprintf("Error fetching STRING enrichment: %s", e$message))
    return(NULL)
  })
}


# =========================================================
# igraph Network Utilities
# =========================================================

#' Build igraph object from STRING edges
#'
#' Creates an igraph network from STRING interaction data.
#' Edge weights are normalized to 0-1 range from STRING scores.
#'
#' @param edges Data frame with protein1, protein2, combined_score columns (from ppi_fetch_string_network)
#' @param node_data Optional data frame with node attributes. Must have 'gene_symbol' column
#'        for matching. Other columns (log2fc, padj, etc.) will be added as vertex attributes.
#'
#' @return igraph object with edge weights and optional node attributes, or NULL if edges empty
ppi_build_network <- function(edges, node_data = NULL) {

  if (is.null(edges) || nrow(edges) == 0) {
    return(NULL)
  }

  # Create edge list with normalized weights
  edge_df <- data.frame(
    from = edges$protein1,
    to = edges$protein2,
    weight = edges$combined_score / 1000,  # Normalize to 0-1
    stringsAsFactors = FALSE
  )

  # Get all unique nodes
  all_nodes <- unique(c(edge_df$from, edge_df$to))

  # Create undirected graph
  g <- igraph::graph_from_data_frame(
    d = edge_df,
    directed = FALSE,
    vertices = data.frame(name = all_nodes, stringsAsFactors = FALSE)
  )

  # Add node attributes if provided
  if (!is.null(node_data) && "gene_symbol" %in% names(node_data)) {
    # Match node data to graph vertices
    node_idx <- match(igraph::V(g)$name, node_data$gene_symbol)

    for (col in names(node_data)) {
      if (col != "gene_symbol") {
        g <- igraph::set_vertex_attr(g, col, value = node_data[[col]][node_idx])
      }
    }
  }

  return(g)
}


#' Cluster network using various algorithms
#'
#' Applies community detection algorithms to identify clusters/modules in the network.
#' Supports multiple algorithms with automatic fallback to Louvain if selected method fails.
#'
#' @param g igraph object
#' @param method Clustering method: "louvain" (default), "walktrap", "leiden", "fast_greedy", "label_prop"
#'
#' @return List with:
#'   - graph: igraph with 'cluster' vertex attribute added
#'   - membership: Named integer vector of cluster assignments
#'   - n_clusters: Number of clusters found
#'   - modularity: Network modularity score
ppi_cluster_network <- function(g, method = "louvain") {

  if (is.null(g)) {
    return(list(graph = NULL, membership = NULL, n_clusters = 0, modularity = NA))
  }

  clusters <- tryCatch({
    switch(method,
      "louvain" = igraph::cluster_louvain(g, weights = igraph::E(g)$weight),
      "walktrap" = igraph::cluster_walktrap(g, weights = igraph::E(g)$weight),
      "leiden" = igraph::cluster_leiden(g, weights = igraph::E(g)$weight,
                                         objective_function = "modularity"),
      "fast_greedy" = igraph::cluster_fast_greedy(g, weights = igraph::E(g)$weight),
      "label_prop" = igraph::cluster_label_prop(g, weights = igraph::E(g)$weight),
      # Default to louvain
      igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
    )
  }, error = function(e) {
    warning(sprintf("Clustering failed with method '%s': %s. Using louvain instead.",
                    method, e$message))
    igraph::cluster_louvain(g)
  })

  membership <- igraph::membership(clusters)
  n_clusters <- length(unique(membership))

  # Add cluster membership to graph vertices
  g <- igraph::set_vertex_attr(g, "cluster", value = as.integer(membership))

  list(
    graph = g,
    membership = membership,
    n_clusters = n_clusters,
    modularity = igraph::modularity(clusters)
  )
}


#' Compute network centrality metrics
#'
#' Calculates various centrality measures for each node in the network.
#' Handles errors gracefully by returning NA for failed metrics.
#'
#' @param g igraph object
#'
#' @return Data frame with columns: name, degree, betweenness, closeness, eigenvector
ppi_compute_centrality <- function(g) {

  if (is.null(g)) {
    return(data.frame(
      name = character(0),
      degree = integer(0),
      betweenness = numeric(0),
      closeness = numeric(0),
      eigenvector = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  node_names <- igraph::V(g)$name

  # Compute various centrality measures
  degree <- igraph::degree(g)
  betweenness <- igraph::betweenness(g, weights = igraph::E(g)$weight, normalized = TRUE)

  closeness <- tryCatch(
    igraph::closeness(g, weights = igraph::E(g)$weight, normalized = TRUE),
    error = function(e) rep(NA_real_, length(node_names))
  )

  eigenvector <- tryCatch(
    igraph::eigen_centrality(g, weights = igraph::E(g)$weight)$vector,
    error = function(e) rep(NA_real_, length(node_names))
  )

  data.frame(
    name = node_names,
    degree = degree,
    betweenness = round(betweenness, 4),
    closeness = round(closeness, 4),
    eigenvector = round(eigenvector, 4),
    stringsAsFactors = FALSE
  )
}


#' Get network-level statistics
#'
#' Computes summary statistics for the entire network including size,
#' density, path lengths (computed on largest component), and component count.
#'
#' @param g igraph object
#'
#' @return Named list with: n_nodes, n_edges, density, avg_degree,
#'         avg_path_length, diameter, n_components
ppi_get_network_stats <- function(g) {

  if (is.null(g)) {
    return(list(
      n_nodes = 0,
      n_edges = 0,
      density = 0,
      avg_degree = 0,
      avg_path_length = NA,
      diameter = NA,
      n_components = 0
    ))
  }

  n_nodes <- igraph::vcount(g)
  n_edges <- igraph::ecount(g)

  # Density
  density <- igraph::edge_density(g)

  # Average degree
  avg_degree <- mean(igraph::degree(g))

  # Average path length and diameter (computed on largest component)
  components <- igraph::components(g)
  largest_comp <- which.max(components$csize)
  subg <- igraph::induced_subgraph(g, which(components$membership == largest_comp))

  avg_path_length <- tryCatch(
    igraph::mean_distance(subg, directed = FALSE),
    error = function(e) NA_real_
  )

  diameter <- tryCatch(
    igraph::diameter(subg, directed = FALSE),
    error = function(e) NA_integer_
  )

  list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    density = round(density, 4),
    avg_degree = round(avg_degree, 2),
    avg_path_length = round(avg_path_length, 2),
    diameter = diameter,
    n_components = components$no
  )
}


#' Find hub proteins (top nodes by connectivity)
#'
#' Identifies the most highly connected proteins in the network
#' using centrality metrics.
#'
#' @param g igraph object
#' @param top_n Number of top hubs to return (default 10)
#'
#' @return Data frame sorted by degree (descending) with centrality metrics
ppi_find_hubs <- function(g, top_n = 10) {

  if (is.null(g)) {
    return(data.frame(
      name = character(0),
      degree = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  centrality <- ppi_compute_centrality(g)
  centrality <- centrality[order(centrality$degree, decreasing = TRUE), ]
  head(centrality, top_n)
}


#' Extract subnetwork around specific nodes
#'
#' Creates a subgraph containing specified nodes and their neighbors
#' up to a given order (number of hops).
#'
#' @param g igraph object
#' @param nodes Character vector of node names to include
#' @param order Number of neighborhood steps (default 1 = immediate neighbors)
#'
#' @return igraph subgraph, or NULL if no matching nodes found
ppi_extract_subnetwork <- function(g, nodes, order = 1) {

  if (is.null(g) || length(nodes) == 0) {
    return(NULL)
  }

  # Find matching nodes
  node_ids <- which(igraph::V(g)$name %in% nodes)

  if (length(node_ids) == 0) {
    return(NULL)
  }

  # Get neighborhood
  neighborhood <- igraph::ego(g, order = order, nodes = node_ids)
  all_nodes <- unique(unlist(lapply(neighborhood, function(x) x$name)))

  igraph::induced_subgraph(g, which(igraph::V(g)$name %in% all_nodes))
}


#' Compute graph layout
#'
#' Applies layout algorithms to position nodes for visualization.
#'
#' @param g igraph object
#' @param layout Layout algorithm: "fr" (Fruchterman-Reingold, default), "kk" (Kamada-Kawai),
#'        "circle", "grid", "drl", "graphopt", "lgl", "mds"
#'
#' @return Layout matrix (n x 2) with x, y coordinates
ppi_compute_layout <- function(g, layout = "fr") {

  if (is.null(g)) {
    return(matrix(ncol = 2, nrow = 0))
  }

  layout_func <- switch(layout,
    "fr" = igraph::layout_with_fr,
    "kk" = igraph::layout_with_kk,
    "circle" = igraph::layout_in_circle,
    "grid" = igraph::layout_on_grid,
    "drl" = igraph::layout_with_drl,
    "graphopt" = igraph::layout_with_graphopt,
    "lgl" = igraph::layout_with_lgl,
    "mds" = igraph::layout_with_mds,
    # Default to Fruchterman-Reingold
    igraph::layout_with_fr
  )

  tryCatch(
    layout_func(g),
    error = function(e) {
      warning(sprintf("Layout failed: %s. Using circle layout.", e$message))
      igraph::layout_in_circle(g)
    }
  )
}


# =========================================================
# visNetwork Visualization Helpers
# =========================================================

#' Create visNetwork nodes data frame from igraph
#'
#' Transforms an igraph object into a nodes data frame for visNetwork,
#' with customizable coloring and sizing based on node attributes.
#'
#' @param g igraph object
#' @param color_by Attribute to color nodes by: "cluster" (default), "log2fc", "pval", "degree"
#' @param size_by Attribute to size nodes by: "degree" (default), "pval", "log2fc", "fixed"
#' @param node_data Optional data frame with additional node attributes (must have 'gene_symbol' column)
#'
#' @return Data frame suitable for visNetwork::visNetwork(nodes = ...) with columns:
#'         id, label, degree, cluster, color, size, title, borderWidth, borderWidthSelected
ppi_create_vis_nodes <- function(g, color_by = "cluster", size_by = "degree", node_data = NULL) {

  if (is.null(g)) {
    return(data.frame(
      id = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  node_names <- igraph::V(g)$name
  n_nodes <- length(node_names)

  # Get degree for sizing/coloring
  degrees <- igraph::degree(g)
  names(degrees) <- node_names

  # Get cluster membership if available
  clusters <- if (!is.null(igraph::V(g)$cluster)) {
    igraph::V(g)$cluster
  } else {
    rep(1, n_nodes)
  }
  names(clusters) <- node_names

  # Base node data with degree and cluster
  nodes <- data.frame(
    id = node_names,
    label = node_names,
    degree = as.integer(degrees),
    cluster = as.integer(clusters),
    stringsAsFactors = FALSE
  )

  # Merge with additional node data if provided
  if (!is.null(node_data) && "gene_symbol" %in% names(node_data)) {
    # Only merge columns that don't already exist
    merge_cols <- setdiff(names(node_data), c("gene_symbol", "degree", "cluster"))
    if (length(merge_cols) > 0) {
      # Ensure unique gene symbols - take first occurrence
      node_subset <- node_data[!duplicated(node_data$gene_symbol),
                                c("gene_symbol", merge_cols), drop = FALSE]
      # Use match for safer attribute assignment (no row duplication)
      idx <- match(nodes$id, node_subset$gene_symbol)
      for (col in merge_cols) {
        nodes[[col]] <- node_subset[[col]][idx]
      }
    }
  }

  # Apply coloring
  nodes$color <- ppi_apply_node_colors(nodes, color_by)

  # Apply sizing
  nodes$size <- ppi_apply_node_sizes(nodes, size_by)

  # Add titles (hover tooltips)
  nodes$title <- ppi_create_node_tooltips(nodes)

  # Border styling
  nodes$borderWidth <- 2
  nodes$borderWidthSelected <- 4

  nodes
}


#' Apply colors to nodes based on attribute
#'
#' Internal helper that maps node attributes to colors.
#'
#' @param nodes Data frame with node attributes
#' @param color_by Coloring method: "cluster", "log2fc", "pval", "degree"
#'
#' @return Character vector of colors (hex codes)
#' @noRd
ppi_apply_node_colors <- function(nodes, color_by) {

  n <- nrow(nodes)

  if (color_by == "cluster") {
    # Use cluster-based colors with safe indexing
    # Cluster IDs may not be contiguous (e.g., after filtering), so map by unique values
    clusters <- nodes$cluster
    unique_clusters <- sort(unique(clusters))
    n_clusters <- length(unique_clusters)
    cluster_colors <- ppi_generate_cluster_palette(n_clusters)
    color_map <- stats::setNames(cluster_colors, as.character(unique_clusters))
    return(unname(color_map[as.character(clusters)]))
  }

  if (color_by == "log2fc" && "log2fc" %in% names(nodes)) {
    # Red-blue diverging scale for fold change
    fc <- nodes$log2fc
    fc[is.na(fc)] <- 0
    return(ppi_fc_to_color(fc))
  }

  if (color_by == "pval" && "padj" %in% names(nodes)) {
    # Yellow for significant, gray for not
    padj <- nodes$padj
    padj[is.na(padj)] <- 1
    return(ppi_pval_to_color(padj))
  }

  if (color_by == "degree") {
    # Size-based gradient
    deg <- nodes$degree
    deg_norm <- (deg - min(deg)) / (max(deg) - min(deg) + 0.001)
    return(ppi_degree_to_color(deg_norm))
  }

  # Default: visNetwork blue

  rep("#97C2FC", n)
}


#' Apply sizes to nodes based on attribute
#'
#' Internal helper that maps node attributes to sizes.
#'
#' @param nodes Data frame with node attributes
#' @param size_by Sizing method: "degree", "pval", "log2fc", "fixed"
#'
#' @return Numeric vector of sizes
#' @noRd
ppi_apply_node_sizes <- function(nodes, size_by) {

  n <- nrow(nodes)
  base_size <- 20
  max_size <- 50
  min_size <- 10

  if (size_by == "degree") {
    deg <- nodes$degree
    deg_norm <- (deg - min(deg)) / (max(deg) - min(deg) + 0.001)
    return(min_size + deg_norm * (max_size - min_size))
  }

  if (size_by == "pval" && "padj" %in% names(nodes)) {
    # More significant = larger
    padj <- nodes$padj
    padj[is.na(padj)] <- 1
    neglog <- -log10(pmax(padj, 1e-10))
    neglog_norm <- pmin(neglog / 5, 1)  # Cap at -log10(1e-5)
    return(min_size + neglog_norm * (max_size - min_size))
  }

  if (size_by == "log2fc" && "log2fc" %in% names(nodes)) {
    fc <- abs(nodes$log2fc)
    fc[is.na(fc)] <- 0
    fc_norm <- pmin(fc / 3, 1)  # Cap at |log2fc| = 3
    return(min_size + fc_norm * (max_size - min_size))
  }

  # Fixed size
  rep(base_size, n)
}


#' Create hover tooltips for nodes
#'
#' Generates HTML tooltip content for visNetwork node hover display.
#'
#' @param nodes Data frame with node attributes
#'
#' @return Character vector of HTML tooltip strings
#' @noRd
ppi_create_node_tooltips <- function(nodes) {

  tooltips <- paste0("<b>", nodes$id, "</b>")

  if ("log2fc" %in% names(nodes)) {
    fc_text <- ifelse(is.na(nodes$log2fc), "N/A", sprintf("%.2f", nodes$log2fc))
    tooltips <- paste0(tooltips, "<br>log2FC: ", fc_text)
  }

  if ("padj" %in% names(nodes)) {
    pval_text <- ifelse(is.na(nodes$padj), "N/A", sprintf("%.2e", nodes$padj))
    tooltips <- paste0(tooltips, "<br>FDR: ", pval_text)
  }

  if ("degree" %in% names(nodes)) {
    tooltips <- paste0(tooltips, "<br>Connections: ", nodes$degree)
  }

  if ("cluster" %in% names(nodes)) {
    tooltips <- paste0(tooltips, "<br>Cluster: ", nodes$cluster)
  }

  if ("pathway" %in% names(nodes)) {
    tooltips <- paste0(tooltips, "<br>Pathway: ", nodes$pathway)
  }

  tooltips
}


#' Create visNetwork edges data frame from igraph
#'
#' Transforms igraph edges into a data frame for visNetwork,
#' with optional confidence-based coloring and width.
#'
#' @param g igraph object
#' @param show_confidence Logical; if TRUE, color and width edges by STRING confidence score
#'
#' @return Data frame suitable for visNetwork::visNetwork(edges = ...) with columns:
#'         from, to, weight, color, width, smooth
ppi_create_vis_edges <- function(g, show_confidence = TRUE) {

  if (is.null(g) || igraph::ecount(g) == 0) {
    return(data.frame(
      from = character(0),
      to = character(0),
      stringsAsFactors = FALSE
    ))
  }

  edge_list <- igraph::as_data_frame(g, what = "edges")

  edges <- data.frame(
    from = edge_list$from,
    to = edge_list$to,
    weight = edge_list$weight,
    stringsAsFactors = FALSE
  )

  if (show_confidence) {
    # Color and width by STRING score
    edges$color <- ppi_confidence_to_color(edges$weight)
    edges$width <- 1 + edges$weight * 4  # 1-5 range
  } else {
    edges$color <- "#848484"
    edges$width <- 1
  }

  # Smooth edges (curved)
  edges$smooth <- FALSE

  edges
}


#' Generate cluster color palette
#'
#' Creates a color palette for cluster visualization using ColorBrewer Set1
#' extended for larger numbers of clusters.
#'
#' @param n Number of clusters
#'
#' @return Character vector of n hex color codes
ppi_generate_cluster_palette <- function(n) {
  if (n <= 8) {
    colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
  } else {
    colors <- grDevices::colorRampPalette(
      c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
        "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
    )(n)
  }
  colors[1:n]
}


#' Convert fold change to color (red-blue diverging)
#'
#' Maps log2 fold change values to a red-white-blue diverging color scale.
#' Red indicates upregulation, blue indicates downregulation.
#'
#' @param fc Numeric vector of log2 fold change values
#'
#' @return Character vector of hex color codes
ppi_fc_to_color <- function(fc) {
  # Red for up, blue for down
  fc_clamped <- pmax(pmin(fc, 3), -3)  # Clamp to [-3, 3]
  fc_norm <- (fc_clamped + 3) / 6  # Normalize to [0, 1]

  # Use colorRamp for smooth gradient
  pal <- grDevices::colorRamp(c("#2166AC", "#F7F7F7", "#B2182B"))
  rgb_vals <- pal(fc_norm)
  grDevices::rgb(rgb_vals[, 1], rgb_vals[, 2], rgb_vals[, 3], maxColorValue = 255)
}


#' Convert p-value to color
#'
#' Maps adjusted p-values to a gray-yellow-red gradient.
#' More significant values (lower p) are more red.
#'
#' @param pval Numeric vector of adjusted p-values
#'
#' @return Character vector of hex color codes
ppi_pval_to_color <- function(pval) {
  # Yellow for significant, gray for not
  neglog <- -log10(pmax(pval, 1e-10))
  neglog_norm <- pmin(neglog / 5, 1)  # Cap at -log10(1e-5)

  pal <- grDevices::colorRamp(c("#CCCCCC", "#FEE08B", "#D73027"))
  rgb_vals <- pal(neglog_norm)
  grDevices::rgb(rgb_vals[, 1], rgb_vals[, 2], rgb_vals[, 3], maxColorValue = 255)
}


#' Convert degree to color
#'
#' Maps normalized degree values to a blue gradient.
#' Higher connectivity = darker blue.
#'
#' @param deg_norm Numeric vector of normalized degree values (0-1)
#'
#' @return Character vector of hex color codes
ppi_degree_to_color <- function(deg_norm) {
  pal <- grDevices::colorRamp(c("#EFF3FF", "#6BAED6", "#08519C"))
  rgb_vals <- pal(deg_norm)
  grDevices::rgb(rgb_vals[, 1], rgb_vals[, 2], rgb_vals[, 3], maxColorValue = 255)
}


#' Convert STRING confidence score to edge color
#'
#' Maps STRING interaction confidence (0-1) to a gray gradient.
#' Higher confidence = darker edge.
#'
#' @param score Numeric vector of confidence scores (0-1)
#'
#' @return Character vector of hex color codes
ppi_confidence_to_color <- function(score) {
  # Light gray to dark gray based on confidence
  pal <- grDevices::colorRamp(c("#CCCCCC", "#666666", "#333333"))
  rgb_vals <- pal(score)
  grDevices::rgb(rgb_vals[, 1], rgb_vals[, 2], rgb_vals[, 3], maxColorValue = 255)
}


#' Configure visNetwork options for interactive exploration
#'
#' Applies standard interactivity options to a visNetwork object including
#' highlighting, node selection, physics simulation, and navigation controls.
#'
#' @param network A visNetwork object (from visNetwork::visNetwork)
#'
#' @return visNetwork object with configured options
ppi_configure_vis_options <- function(network) {
  network %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = TRUE, style = "width: 200px;"),
      selectedBy = list(variable = "cluster", main = "Select by Cluster")
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      navigationButtons = TRUE,
      keyboard = TRUE,
      hover = TRUE,
      hoverConnectedEdges = TRUE,
      tooltipDelay = 100
    ) %>%
    visNetwork::visPhysics(
      enabled = TRUE,
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01,
        springLength = 100,
        springConstant = 0.08
      ),
      stabilization = list(
        enabled = TRUE,
        iterations = 200,
        updateInterval = 25
      )
    ) %>%
    visNetwork::visLayout(randomSeed = 42)
}


#' Create color legend metadata
#'
#' Returns legend information for the specified coloring scheme,
#' useful for adding legends to the visualization UI.
#'
#' @param color_by Color scheme: "log2fc", "pval", "degree", or "cluster"
#'
#' @return List with: title, colors, labels
ppi_create_color_legend <- function(color_by) {

  if (color_by == "log2fc") {
    return(list(
      title = "log2 Fold Change",
      colors = c("#2166AC", "#F7F7F7", "#B2182B"),
      labels = c("Down (-3)", "0", "Up (+3)")
    ))
  }

  if (color_by == "pval") {
    return(list(
      title = "-log10(FDR)",
      colors = c("#CCCCCC", "#FEE08B", "#D73027"),
      labels = c("Not sig", "p<0.05", "Highly sig")
    ))
  }

  if (color_by == "degree") {
    return(list(
      title = "Connectivity",
      colors = c("#EFF3FF", "#6BAED6", "#08519C"),
      labels = c("Low", "Medium", "High")
    ))
  }

  list(title = "Cluster", colors = NULL, labels = NULL)
}
