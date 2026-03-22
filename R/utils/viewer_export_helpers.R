# R/utils/viewer_export_helpers.R
# Shared pure-logic helpers for Result Viewer export workflows.
# Extracted from page_results.R to eliminate duplication between
# bulk-download and PDF-prefilter modals.

#' Collect plot keys for a given engine result
#'
#' Pure function — no Shiny reactivity, no side effects.
#'
#' @param res       Result list from tb_load_results() (must have $data).
#' @param eng_id    Engine identifier string (e.g. "volcano", "goora").
#' @param registry  Engine registry list (must have $engines).
#' @return Character vector of plot keys, possibly length-0.
res_collect_plot_keys <- function(res, eng_id, registry) {
  eng_id <- tolower(as.character(eng_id %||% ""))
  data_obj <- res$data %||% list()

  if (eng_id == "volcano") {
    comps <- data_obj$comparisons %||% list()
    if (is.list(comps) && length(comps) > 0) return(names(comps))
    return("volcano_plot")
  }

  if (eng_id %in% c("goora", "1dgofcs")) {
    if (eng_id == "1dgofcs") {
      analyses <- data_obj$analyses %||% NULL
      if (is.list(analyses) && length(analyses) > 0) {
        return(paste0(names(analyses), "_plot"))
      }
    }
    tabs <- character()
    for (tab in c("BP", "MF", "CC")) {
      if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
        tabs <- c(tabs, tab)
      }
    }
    if (length(tabs) == 0 && !is.null(data_obj$terms) && is.data.frame(data_obj$terms) &&
        "ontology" %in% names(data_obj$terms)) {
      ontologies_present <- unique(as.character(data_obj$terms$ontology))
      for (tab in c("BP", "MF", "CC")) {
        if (tab %in% ontologies_present) tabs <- c(tabs, tab)
      }
    }
    if (length(tabs) > 0) return(paste0(tolower(tabs), "_plot"))
    return(if (eng_id == "goora") "goora_plot" else "1dgofcs_plot")
  }

  if (eng_id == "2dgofcs") {
    analyses <- data_obj$analyses %||% NULL
    if (is.list(analyses) && length(analyses) > 0) {
      return(names(analyses))
    }
    tabs <- character()
    for (tab in c("BP", "MF", "CC")) {
      if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
        tabs <- c(tabs, tab)
      }
    }
    if (length(tabs) > 0) return(paste0(tolower(tabs), "_plot"))
    return("2dgofcs_plot")
  }

  spec_plots <- registry$engines[[eng_id]]$render_spec$plots %||% character()
  spec_plots <- spec_plots[nzchar(spec_plots)]
  if (length(spec_plots) > 0) return(spec_plots)

  character()
}

#' Build named list of graph choices for modal checkboxes
#'
#' Iterates visible nodes, collects plot keys, and builds a named list
#' suitable for checkboxGroupInput choices.
#'
#' @param nodes_df              Data frame of pipeline nodes (node_id, engine_id, node_dir, ...).
#' @param registry              Engine registry list.
#' @param build_effective_state_fn  Function(node_id, node_dir) -> list(style = ...).
#' @param load_results_fn       Function(node_dir) -> result list or NULL.
#' @param export_graph_name_fn  Function matching res_export_graph_name signature.
#' @param node_numbers          Named list mapping node_id -> display number.
#' @return Named list: label -> "node_id::plot_key".
res_build_graph_choices <- function(nodes_df, registry, build_effective_state_fn,
                                    load_results_fn, export_graph_name_fn,
                                    node_numbers) {
  graph_choices <- list()
  should_hide <- res_should_hide_nodes(nodes_df, registry)

  for (i in seq_len(nrow(nodes_df))) {
    node_id <- nodes_df$node_id[i]
    eng_id  <- tolower(nodes_df$engine_id[i] %||% "")
    node_dir <- nodes_df$node_dir[i]

    # Skip overview, dataprocessor, and hidden nodes (same logic as sidebar)
    if (eng_id == "dataprocessor" || node_id == "overview") next
    if (should_hide[i]) next

    res <- load_results_fn(node_dir)
    if (is.null(res)) next

    effective_state <- build_effective_state_fn(node_id, node_dir)
    style <- effective_state$style %||% list()

    plot_keys <- res_collect_plot_keys(res, eng_id, registry)
    if (length(plot_keys) == 0) next

    force_plot_label <- length(plot_keys) > 1
    for (plot_key in plot_keys) {
      base <- export_graph_name_fn(node_id, eng_id, plot_key, registry, res, style, node_numbers,
                                   force_plot_label = force_plot_label)
      label <- base
      if (!nzchar(label)) label <- paste0(node_id, "-", plot_key)

      value <- paste(node_id, plot_key, sep = "::")
      if (label %in% names(graph_choices)) {
        label <- paste0(label, " (", plot_key, ")")
      }
      graph_choices[[label]] <- value
    }
  }

  graph_choices
}
