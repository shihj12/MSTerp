# R/utils/compose_helpers.R
# Pure helpers for the patchwork composition canvas (Result Viewer "Layouts").
# Layouts let users place already-rendered ggplots into a grid and export the
# composition as PNG / PDF / SVG for downstream editing in Illustrator etc.
#
# All functions in this file are pure (no Shiny reactivity, no global state).
# Persistence path: <run_root>/layouts.json. The .terpbook zip already round-
# trips run_root, so writing layouts.json there is enough.

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Defaults --------------------------------------------------------------

#' Default empty layout skeleton.
#'
#' @param id     Stable identifier (e.g. "layout_1").
#' @param name   Display name shown in the layout picker.
#' @param ncol   Columns (>=1).
#' @param nrow   Rows (>=1).
#' @return Named list (see compose_validate_layout for the canonical shape).
compose_default_layout <- function(id = "layout_1", name = "Layout 1",
                                   ncol = 2L, nrow = 2L) {
  ncol <- max(1L, suppressWarnings(as.integer(ncol)))
  nrow <- max(1L, suppressWarnings(as.integer(nrow)))
  if (!is.finite(ncol)) ncol <- 2L
  if (!is.finite(nrow)) nrow <- 2L

  cells <- vector("list", ncol * nrow)  # NULL = empty cell

  list(
    id           = as.character(id),
    name         = as.character(name),
    ncol         = ncol,
    nrow         = nrow,
    widths       = rep(1, ncol),
    heights      = rep(1, nrow),
    cells        = cells,
    annotation   = list(title = "", tag_levels = "A"),
    export       = list(width_in = 8.5, height_in = 6, units = "in"),
    hide_legends = FALSE
  )
}

# ---- Validation / coercion -------------------------------------------------

#' Coerce an arbitrary list (e.g. from JSON) to a valid layout. Missing fields
#' get sensible defaults; oversized cell vectors are truncated; undersized are
#' padded with NULLs.
compose_validate_layout <- function(layout) {
  if (!is.list(layout)) layout <- list()

  ncol <- suppressWarnings(as.integer(layout$ncol %||% 2L))
  nrow <- suppressWarnings(as.integer(layout$nrow %||% 2L))
  if (!is.finite(ncol) || ncol < 1L) ncol <- 2L
  if (!is.finite(nrow) || nrow < 1L) nrow <- 2L
  if (ncol > 8L) ncol <- 8L
  if (nrow > 8L) nrow <- 8L

  widths <- suppressWarnings(as.numeric(layout$widths %||% rep(1, ncol)))
  if (length(widths) != ncol || any(!is.finite(widths)) || any(widths <= 0)) {
    widths <- rep(1, ncol)
  }

  heights <- suppressWarnings(as.numeric(layout$heights %||% rep(1, nrow)))
  if (length(heights) != nrow || any(!is.finite(heights)) || any(heights <= 0)) {
    heights <- rep(1, nrow)
  }

  cells <- layout$cells %||% list()
  if (!is.list(cells)) cells <- list()
  total <- ncol * nrow
  if (length(cells) > total) {
    cells <- cells[seq_len(total)]
  } else if (length(cells) < total) {
    cells <- c(cells, vector("list", total - length(cells)))
  }
  cells <- lapply(cells, function(cell) {
    if (is.null(cell)) return(NULL)
    if (!is.list(cell)) return(NULL)
    nid <- as.character(cell$node_id %||% "")
    pkey <- as.character(cell$plot_key %||% "")
    if (!nzchar(nid) || !nzchar(pkey)) return(NULL)
    list(
      node_id  = nid,
      plot_key = pkey,
      tag      = if (is.null(cell$tag)) NULL else as.character(cell$tag),
      title    = if (is.null(cell$title)) NULL else as.character(cell$title)
    )
  })

  ann <- layout$annotation %||% list()
  if (!is.list(ann)) ann <- list()
  ann$title <- as.character(ann$title %||% "")
  ann$tag_levels <- as.character(ann$tag_levels %||% "A")
  if (!ann$tag_levels %in% c("A", "a", "1", "I", "i", "none")) {
    ann$tag_levels <- "A"
  }

  exp <- layout$export %||% list()
  if (!is.list(exp)) exp <- list()
  w <- suppressWarnings(as.numeric(exp$width_in %||% 8.5))
  h <- suppressWarnings(as.numeric(exp$height_in %||% 6))
  if (!is.finite(w) || w <= 0) w <- 8.5
  if (!is.finite(h) || h <= 0) h <- 6
  exp <- list(width_in = w, height_in = h, units = "in")

  hide_legends <- suppressWarnings(as.logical(layout$hide_legends %||% FALSE))
  if (length(hide_legends) != 1L || is.na(hide_legends)) hide_legends <- FALSE

  list(
    id           = as.character(layout$id %||% "layout_1"),
    name         = as.character(layout$name %||% "Layout"),
    ncol         = ncol,
    nrow         = nrow,
    widths       = widths,
    heights      = heights,
    cells        = cells,
    annotation   = ann,
    export       = exp,
    hide_legends = hide_legends
  )
}

# ---- Patchwork build -------------------------------------------------------

#' Build a patchwork composition from a layout.
#'
#' @param layout         Validated layout list (see compose_validate_layout).
#' @param plot_lookup_fn function(node_id, plot_key) -> ggplot/patchwork or NULL.
#'                       NULL return → cell rendered as a blank spacer.
#' @return A patchwork object ready to print/ggsave, or NULL if all cells empty.
compose_build_patchwork <- function(layout, plot_lookup_fn) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork is required for compose_build_patchwork().")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for compose_build_patchwork().")
  }
  layout <- compose_validate_layout(layout)

  placeholder <- function(label) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = label,
                        size = 3, colour = "grey50") +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme_void()
  }

  cells <- layout$cells
  any_filled <- FALSE
  plots <- lapply(cells, function(cell) {
    if (is.null(cell)) {
      return(patchwork::plot_spacer())
    }
    p <- tryCatch(
      plot_lookup_fn(cell$node_id, cell$plot_key),
      error = function(e) NULL
    )
    if (is.null(p)) {
      return(placeholder("Plot unavailable"))
    }
    if (!is.null(cell$title) && nzchar(cell$title)) {
      p <- p + ggplot2::labs(title = cell$title)
    }
    any_filled <<- TRUE
    p
  })

  if (!any_filled) return(NULL)

  combined <- patchwork::wrap_plots(
    plots,
    ncol    = layout$ncol,
    nrow    = layout$nrow,
    widths  = layout$widths,
    heights = layout$heights
  )

  ann <- layout$annotation
  tag_levels <- if (identical(ann$tag_levels, "none")) NULL else ann$tag_levels
  combined <- combined + patchwork::plot_annotation(
    title      = if (nzchar(ann$title %||% "")) ann$title else NULL,
    tag_levels = tag_levels
  )

  if (isTRUE(layout$hide_legends)) {
    combined <- combined & ggplot2::theme(legend.position = "none")
  }

  combined
}

# ---- Persistence -----------------------------------------------------------

compose_layouts_path <- function(run_root) {
  file.path(run_root, "layouts.json")
}

#' Load layouts from <run_root>/layouts.json. Missing/corrupt file → empty list.
compose_load_layouts <- function(run_root) {
  if (is.null(run_root) || !nzchar(run_root)) return(list())
  path <- compose_layouts_path(run_root)
  if (!file.exists(path)) return(list())
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(list())
  raw <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw)) return(list())
  layouts <- raw$layouts %||% raw  # tolerate either {layouts: [...]} or [...]
  if (!is.list(layouts)) return(list())
  out <- list()
  for (lay in layouts) {
    v <- compose_validate_layout(lay)
    out[[v$id]] <- v
  }
  out
}

#' Persist layouts to <run_root>/layouts.json. No-op if layouts is empty AND
#' the file does not yet exist (avoids polluting unmodified terpbooks).
compose_save_layouts <- function(run_root, layouts) {
  if (is.null(run_root) || !nzchar(run_root)) return(invisible(FALSE))
  path <- compose_layouts_path(run_root)
  layouts <- layouts %||% list()
  if (length(layouts) == 0 && !file.exists(path)) return(invisible(FALSE))

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("jsonlite required to save layouts.json")
    return(invisible(FALSE))
  }

  validated <- lapply(layouts, compose_validate_layout)
  validated <- unname(validated)

  payload <- list(
    version    = 1L,
    updated_at = as.character(Sys.time()),
    layouts    = validated
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp_", sprintf("%08d", sample.int(1e8, 1)))
  jsonlite::write_json(payload, tmp, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  ok <- file.rename(tmp, path)
  if (!ok) {
    if (file.exists(path)) file.remove(path)
    ok <- file.rename(tmp, path)
    if (!ok) {
      if (file.exists(tmp)) file.remove(tmp)
      stop("Failed to atomically write: ", path)
    }
  }
  invisible(TRUE)
}

# ---- Misc helpers ----------------------------------------------------------

#' Generate a fresh layout id not already in `existing` (named list).
compose_next_layout_id <- function(existing) {
  if (!is.list(existing)) existing <- list()
  i <- length(existing) + 1L
  repeat {
    cand <- paste0("layout_", i)
    if (!cand %in% names(existing)) return(cand)
    i <- i + 1L
  }
}
