# R/utils/plotly_state.R
# Shared helpers for building plotly state data used by both the Results
# Viewer (page_results.R) and the Tools pages (e.g. tool_2dgofcs.R).

# Build the per-plot state used by tb_2dgofcs_plotly. Returns NULL when no
# data is available. The returned list is a stable contract:
#   $df_plot : data.frame with term, term_id, score_x, score_y, fdr, n, ontology
#   $xlim    : numeric(2)
#   $ylim    : numeric(2)
#   $labs    : character vector of term labels to render (by design, all
#              visible points get labels in 2dgofcs)
#   $saved   : named list of saved label positions keyed by term_id
#
# `plot_key` selects the correct analysis for multi-comparison results; it
# may be "2dgofcs_plot" for single-plot results.
tb_build_2dgofcs_plot_state <- function(res, style, visibility = NULL, plot_key = "2dgofcs_plot",
                                        plotly_state = NULL) {
  data_obj <- res$data %||% list()
  df <- NULL

  analyses <- data_obj$analyses %||% NULL
  if (!is.null(analyses) && is.list(analyses) && plot_key %in% names(analyses)) {
    df <- analyses[[plot_key]]$terms
  }

  if (is.null(df) || !is.data.frame(df)) {
    tab_key <- toupper(sub("_plot$", "", plot_key))
    if (tab_key %in% c("BP", "MF", "CC")) {
      tab_data <- data_obj[[tab_key]]
      df <- tab_data$terms %||% tab_data$data %||% tab_data
    }
  }

  if (is.null(df) || !is.data.frame(df)) {
    df <- data_obj$terms %||% data_obj
  }

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  if (!"term_id" %in% names(df)) {
    for (col in c("TermID", "termID", "go_id", "GO", "ID")) {
      if (col %in% names(df)) { df$term_id <- df[[col]]; break }
    }
  }
  if (!"term" %in% names(df)) {
    for (col in c("term_name", "Term", "pathway", "Pathway", "term_id")) {
      if (col %in% names(df)) { df$term <- df[[col]]; break }
    }
  }
  if ("term_id" %in% names(df)) df$term_id <- as.character(df$term_id)
  if ("term" %in% names(df)) df$term <- as.character(df$term)
  if (!"fdr" %in% names(df)) {
    for (col in c("FDR", "p.adjust", "padj", "pval")) {
      if (col %in% names(df)) { df$fdr <- df[[col]]; break }
    }
    if (!"fdr" %in% names(df)) df$fdr <- 0.05
  }
  if (!"n" %in% names(df)) {
    for (col in c("n_genes", "count", "Count", "GeneCount", "size")) {
      if (col %in% names(df)) { df$n <- df[[col]]; break }
    }
    if (!"n" %in% names(df)) df$n <- 5
  }
  if (!"score_x" %in% names(df)) df$score_x <- 0
  if (!"score_y" %in% names(df)) df$score_y <- 0
  if (!"ontology" %in% names(df)) df$ontology <- NA_character_

  ontology_filter <- style$ontology_filter %||% "all"
  if (!identical(ontology_filter, "all") && "ontology" %in% names(df)) {
    df <- df[toupper(as.character(df$ontology)) == toupper(ontology_filter), , drop = FALSE]
  }

  df_all <- df
  hidden_terms <- (visibility %||% list())$hidden_terms %||% character(0)
  term_labels <- (visibility %||% list())$term_labels %||% list()
  df_plot <- df_all[!(df_all$term %in% hidden_terms), , drop = FALSE]

  # Render-time max_terms slice (mirrors tb_render_2dgofcs_scatter_xy logic).
  max_terms_view <- suppressWarnings(as.integer(style$max_terms %||% NA_integer_))
  if (length(max_terms_view) == 1 && is.finite(max_terms_view) && max_terms_view > 0 &&
      nrow(df_plot) > max_terms_view) {
    df_plot <- df_plot[seq_len(max_terms_view), , drop = FALSE]
  }

  if (length(term_labels) > 0 && nrow(df_plot) > 0) {
    df_plot$term_original <- df_plot$term
    for (i in seq_len(nrow(df_plot))) {
      orig <- df_plot$term[i]
      if (!is.null(term_labels[[orig]]) && nzchar(term_labels[[orig]])) {
        df_plot$term[i] <- term_labels[[orig]]
      }
    }
  } else if (nrow(df_plot) > 0) {
    df_plot$term_original <- df_plot$term
  }

  xlim <- suppressWarnings(as.numeric(c(style$x_min %||% -1, style$x_max %||% 1)))
  ylim <- suppressWarnings(as.numeric(c(style$y_min %||% -1, style$y_max %||% 1)))
  if (length(xlim) != 2 || any(!is.finite(xlim))) xlim <- c(-1, 1)
  if (length(ylim) != 2 || any(!is.finite(ylim))) ylim <- c(-1, 1)
  xlim <- sort(xlim)
  ylim <- sort(ylim)

  plotly_state <- plotly_state %||% list()
  saved <- plotly_state$labels_by_plot[[plot_key]] %||%
    plotly_state$labels_by_plot$default %||%
    plotly_state$labels %||% list()

  labs <- if (nrow(df_plot) > 0) df_plot$term else character(0)

  list(df_plot = df_plot, xlim = xlim, ylim = ylim, labs = labs, saved = saved)
}
