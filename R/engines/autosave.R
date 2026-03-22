# =========================================================
# R/autosave.R — Debounced autosave for Results Viewer
# - Keeps in-memory override state per active node
# - Writes render_state.json sparsely + atomically
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

tb_atomic_write_json <- function(x, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp_", sprintf("%08d", sample.int(1e8, 1)))
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  ok <- file.rename(tmp, path)
  if (!ok) {
    # Windows fallback
    if (file.exists(path)) file.remove(path)
    ok2 <- file.rename(tmp, path)
    if (!ok2) {
      if (file.exists(tmp)) file.remove(tmp)
      stop("Failed to atomically write: ", path)
    }
  }
  invisible(TRUE)
}

#' Prune term_labels to cap at MAX_TERM_LABELS entries (LRU eviction)
#'
#' Maintains sparse semantics - only non-empty overrides are stored.
#' When cap is exceeded, oldest entries are removed.
#'
#' @param term_labels Named list of term label overrides
#' @param max_entries Maximum entries to keep (default 500)
#' @return Pruned term_labels list
tb_prune_term_labels <- function(term_labels, max_entries = 500L) {

  if (!is.list(term_labels) || length(term_labels) == 0) return(list())

  # Remove NULL/empty entries (sparse semantics)
  term_labels <- term_labels[vapply(term_labels, function(x) !is.null(x) && nzchar(x), logical(1))]

  # If under cap, return as-is
  if (length(term_labels) <= max_entries) return(term_labels)

  # LRU eviction: keep most recent entries (end of list)
  # R lists preserve insertion order, so tail entries are most recent
  keep_idx <- seq(length(term_labels) - max_entries + 1L, length(term_labels))
  term_labels[keep_idx]
}

tb_plotly_migrate_legacy_labels <- function(plotly_state) {
  # Backward compatibility: older terpbooks stored dragged labels as a flat map
  # at `plotly$labels`. The enhanced schema stores labels per plot key under
  # `plotly$labels_by_plot[[plot_key]]`.
  #
  # At this layer we don't know the plot_key (comparison/analysis), so we migrate
  # legacy labels into a conservative default bucket to ensure:
  # - old terpbooks load without error, and
  # - the first save after load writes `labels_by_plot` (migration-on-save).
  if (!is.list(plotly_state)) return(plotly_state)
  if (!is.null(plotly_state$labels) && is.null(plotly_state$labels_by_plot)) {
    plotly_state$labels_by_plot <- list(default = plotly_state$labels)
  }
  plotly_state
}

tb_save_render_state_atomic <- function(node_dir, patch) {
  patch <- patch %||% list()
  
  # Load existing override-only state so we don't overwrite other sections
  old <- tb_load_render_state_override(node_dir)  # style=list(), plotly=NULL, visibility=NULL
  
  new <- list(
    style = old$style %||% list(),
    plotly = old$plotly %||% NULL,
    visibility = old$visibility %||% NULL
  )
  
  # Apply patch components (NULL means "leave unchanged")
  if (!is.null(patch$style)) new$style <- patch$style %||% list()
  if (!is.null(patch$plotly)) new$plotly <- patch$plotly
  if (!is.null(patch$visibility)) new$visibility <- patch$visibility

  # Ensure legacy plotly label state is migrated on save.
  if (!is.null(new$plotly)) new$plotly <- tb_plotly_migrate_legacy_labels(new$plotly)

  # Prune term_labels to prevent unbounded growth (500 entry cap with LRU eviction)
  if (!is.null(new$visibility) && is.list(new$visibility$term_labels)) {
    new$visibility$term_labels <- tb_prune_term_labels(new$visibility$term_labels)
  }

  out <- list(style = new$style %||% list())
  if (!is.null(new$plotly)) out$plotly <- new$plotly
  if (!is.null(new$visibility)) out$visibility <- new$visibility
  
  out$updated_at <- as.character(Sys.time())

  p <- tb_node_paths(node_dir)
  tb_atomic_write_json(tb_json_safe(out), p$render_state)

  # Invalidate caches after saving to ensure fresh reads
  if (exists("tb_cache_invalidate_node", mode = "function")) {
    tb_cache_invalidate_node(node_dir)
  }

  invisible(TRUE)
}


tb_style_sparse <- function(cur, baseline, schema_names = NULL) {
  cur <- cur %||% list()
  baseline <- baseline %||% list()
  nms <- schema_names %||% unique(c(names(cur), names(baseline)))
  sparse <- list()
  for (nm in nms) {
    if (!tb_is_equal(cur[[nm]], baseline[[nm]])) sparse[[nm]] <- cur[[nm]]
  }
  sparse
}

tb_style_merge_preserve_extras <- function(prev_style, sparse_style, schema_names) {
  prev_style <- prev_style %||% list()
  sparse_style <- sparse_style %||% list()
  schema_names <- as.character(schema_names %||% character())

  prev_names <- names(prev_style) %||% character()
  extra_names <- setdiff(prev_names, schema_names)

  c(prev_style[extra_names], sparse_style)
}

tb_debounce_ms <- function(ms, func) {
  force(ms); force(func)
  token <- 0L
  
  function(...) {
    token <<- token + 1L
    my_token <- token
    args <- list(...)
    
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(function() {
        if (!identical(my_token, token)) return(invisible(NULL))
        tryCatch(do.call(func, args), error = function(e) {
          message("[tb_debounce_ms] Error in debounced callback: ", conditionMessage(e))
        })
      }, delay = ms / 1000)
    } else {
      do.call(func, args)
    }
    
    invisible(TRUE)
  }
}

