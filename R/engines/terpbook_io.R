# =========================================================
# R/terpbook_io.R - terpbook loader + tree/index utilities
#
# Supports:
#  - manifest.json (flat steps)
#  - step.json with `views` list (children)
#  - views/<id>/view.json with optional nested `views`
#  - render_state.json at any node (master + overrides)
#  - effective style = merged ancestor style (parent -> child)
#
# PDF alignment:
#  - defaults come from terpbook descriptors (step.json / view.json)
#  - render_state.json should be override-only (sparse)
#
# Render state schema notes (plot label persistence):
#  - `render_state.json` stores sparse user overrides under:
#     - `style`: scalar plot styling overrides
#     - `visibility`: per-item show/hide overrides
#     - `plotly`: interactive editor state (including dragged label positions)
#  - New label schema (per-plot persistence):
#     - `plotly$labels_by_plot[[plot_key]][[label_id]] = list(...)`
#       where `plot_key` is derived by the results viewer (e.g. volcano
#       comparison name or GO analysis name) and `label_id` is the label's name.
#     - Each label stores axis-normalized coordinates for robust replay:
#         - `x`, `y`: normalized (0..1)
#         - `x_range`, `y_range`: axis ranges used at save time (length-2 numeric)
#       Normalization/denormalization is implemented in `tb_normalize_coords()`
#       and `tb_denormalize_coords()`.
#  - Backward compatibility (legacy terpbooks):
#     - Older terpbooks may store a flat label map at `plotly$labels`.
#     - Readers treat `plotly$labels` as a fallback/default; writers migrate the
#       legacy map into `plotly$labels_by_plot$default` on first save (see
#       `tb_save_render_state_atomic()` in `R/engines/autosave.R`).
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Session-level cache for terpbook I/O ------------------------------------
# Reduces filesystem reads on cloud deployments (Posit Connect) where I/O latency
# is higher than local SSDs. Cache is cleared when loading a new terpbook and
# invalidated selectively when render_state is saved.

.tb_cache <- new.env(parent = emptyenv())
.tb_cache$descriptors <- list()    # node_dir|kind -> descriptor
.tb_cache$results <- list()        # node_dir -> results RDS
.tb_cache$render_state <- list()   # node_dir -> render_state override
.tb_cache$defaults <- list()       # node_dir -> node defaults
.tb_cache$effective <- list()      # node_id -> effective render state

tb_cache_clear <- function() {
  .tb_cache$descriptors <- list()
  .tb_cache$results <- list()
  .tb_cache$render_state <- list()
  .tb_cache$defaults <- list()
  .tb_cache$effective <- list()
  invisible(TRUE)
}

tb_cache_invalidate_node <- function(node_dir) {
  key <- tb_norm(node_dir)
  .tb_cache$render_state[[key]] <- NULL
  .tb_cache$defaults[[key]] <- NULL
  .tb_cache$effective <- list()  # Clear all effective states on any change
  invisible(TRUE)
}

tb_require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required.")
  }
}

tb_norm <- function(p) {
  # Vector-safe normalizePath; empty stays empty.
  p <- as.character(p %||% "")
  p[is.na(p)] <- ""
  p <- trimws(p)
  
  if (length(p) == 0) return(character())
  
  ok <- nzchar(p)
  out <- p
  out[!ok] <- ""
  
  if (any(ok)) {
    out[ok] <- normalizePath(path.expand(out[ok]), winslash = "/", mustWork = FALSE)
  }
  out
}

tb_read_json <- function(path) {
  tb_require_jsonlite()
  jsonlite::read_json(path, simplifyVector = TRUE)
}

tb_json_safe <- function(x) {
  if (is.atomic(x) && !is.null(names(x)) && length(names(x)) > 0) return(as.list(x))
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(tb_json_safe(x[[i]]))
    }
  }
  x
}

tb_write_json <- function(x, path) {
  tb_require_jsonlite()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(tb_json_safe(x), path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

tb_manifest_path <- function(run_root) file.path(run_root, "manifest.json")

tb_manifest_engine_ids <- function(manifest) {
  st <- manifest$steps %||% list()
  if (is.null(st) || length(st) == 0) return(character())

  if (is.data.frame(st)) {
    eids <- st$engine_id %||% st$engine %||% character()
    return(tolower(as.character(eids)))
  }

  if (is.list(st)) {
    eids <- vapply(st, function(x) {
      as.character((x %||% list())$engine_id %||% (x %||% list())$engine %||% "")
    }, character(1))
    eids <- eids[nzchar(eids)]
    return(tolower(eids))
  }

  character()
}

tb_legacy_warnings <- function(manifest) {
  eids <- unique(tb_manifest_engine_ids(manifest))
  warnings <- character()

  warnings
}

tb_find_run_root <- function(exdir) {
  exdir <- tb_norm(exdir)
  if (!dir.exists(exdir)) stop("Extraction dir not found: ", exdir)
  
  direct <- file.path(exdir, "manifest.json")
  if (file.exists(direct)) return(exdir)
  
  subs <- list.dirs(exdir, full.names = TRUE, recursive = FALSE)
  for (d in subs) {
    if (file.exists(file.path(d, "manifest.json"))) return(tb_norm(d))
  }
  
  cand <- list.files(exdir, pattern = "^manifest\\.json$", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) return(tb_norm(dirname(cand[[1]])))
  
  stop("Could not locate manifest.json in extracted terpbook.")
}

tb_unzip_terpbook <- function(zip_path, exdir = tempfile("terpbook_")) {
  zip_path <- tb_norm(zip_path)
  if (!file.exists(zip_path)) stop("File not found: ", zip_path)
  
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  
  # Preflight: ensure archive is readable and contains something
  zlist <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(zlist) || nrow(zlist) == 0) {
    stop("Failed to read zip index (archive may be invalid): ", zip_path)
  }
  
  ok <- tryCatch({
    utils::unzip(zip_path, exdir = exdir, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    stop("Failed to unzip terpbook: ", conditionMessage(e))
  })
  
  if (!isTRUE(ok)) stop("Failed to unzip terpbook: ", zip_path)
  
  run_root <- tb_find_run_root(exdir)
  list(exdir = tb_norm(exdir), run_root = run_root)
}


tb_load_manifest <- function(run_root) {
  run_root <- tb_norm(run_root)
  mp <- tb_manifest_path(run_root)
  if (!file.exists(mp)) stop("manifest.json missing in run_root: ", run_root)
  manifest <- tb_read_json(mp)
  warns <- tb_legacy_warnings(manifest)
  if (length(warns) > 0) {
    for (w in warns) warning(w, call. = FALSE)
  }
  list(run_root = run_root, manifest = manifest, warnings = warns)
}

tb_steps_df <- function(run_root, manifest) {
  st <- manifest$steps %||% list()
  
  if (is.null(st) || length(st) == 0) {
    return(data.frame(
      step_index = integer(),
      step_id = character(),
      engine_id = character(),
      step_dir = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  if (is.data.frame(st)) {
    st_list <- lapply(seq_len(nrow(st)), function(i) as.list(st[i, , drop = FALSE]))
  } else if (is.list(st)) {
    st_list <- st
  } else {
    st_list <- list()
  }
  
  out <- lapply(st_list, function(x) {
    step_dir_raw <- as.character(x$step_dir %||% x$dir %||% x$path %||% "")
    data.frame(
      step_index = suppressWarnings(as.integer(x$step_index %||% NA_integer_)),
      step_id = as.character(x$step_id %||% x$name %||% ""),
      engine_id = as.character(x$engine_id %||% x$engine %||% ""),
      step_dir = tb_norm(file.path(run_root, step_dir_raw)),
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, out)
  df <- df[order(df$step_index, na.last = TRUE), , drop = FALSE]
  df
}

tb_node_paths <- function(node_dir) {
  node_dir <- tb_norm(node_dir)
  list(
    node_dir = node_dir,
    step_json = file.path(node_dir, "step.json"),
    view_json = file.path(node_dir, "view.json"),
    results_rds = file.path(node_dir, "results.rds"),
    render_state = file.path(node_dir, "render_state.json"),
    artifacts_json = file.path(node_dir, "artifacts.json")
  )
}

tb_load_results <- function(node_dir, use_cache = TRUE) {
  key <- tb_norm(node_dir)
  if (isTRUE(use_cache) && !is.null(.tb_cache$results[[key]])) {
    return(.tb_cache$results[[key]])
  }
  p <- tb_node_paths(node_dir)
  if (!file.exists(p$results_rds)) return(NULL)
  res <- tryCatch(readRDS(p$results_rds), error = function(e) NULL)
  if (isTRUE(use_cache)) .tb_cache$results[[key]] <- res
  res
}

# ---- Node descriptors + defaults --------------------------------------------

tb_as_list_of_rows <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  if (is.list(x)) return(x)
  list()
}

tb_read_node_descriptor <- function(node_dir, kind = c("step", "view"), use_cache = TRUE) {
  kind <- match.arg(kind)
  key <- paste0(tb_norm(node_dir), "|", kind)
  if (isTRUE(use_cache) && !is.null(.tb_cache$descriptors[[key]])) {
    return(.tb_cache$descriptors[[key]])
  }
  p <- tb_node_paths(node_dir)
  json_path <- if (kind == "step") p$step_json else p$view_json
  desc <- if (file.exists(json_path)) tb_read_json(json_path) else list()
  if (isTRUE(use_cache)) .tb_cache$descriptors[[key]] <- desc
  desc
}

tb_read_descriptor_any <- function(node_dir) {
  p <- tb_node_paths(node_dir)
  if (file.exists(p$view_json)) return(tb_read_json(p$view_json))
  if (file.exists(p$step_json)) return(tb_read_json(p$step_json))
  list()
}

tb_node_defaults <- function(node_dir, use_cache = TRUE) {
  # Defaults that come WITH the terpbook (NOT user overrides).
  key <- tb_norm(node_dir)
  if (isTRUE(use_cache) && !is.null(.tb_cache$defaults[[key]])) {
    return(.tb_cache$defaults[[key]])
  }
  d <- tb_read_descriptor_any(node_dir)
  defaults <- list(
    style = d$style %||% list(),
    plotly = d$plotly %||% list(),
    visibility = d$visibility %||% list()
  )
  if (isTRUE(use_cache)) .tb_cache$defaults[[key]] <- defaults
  defaults
}

# ---- State merge helpers -----------------------------------------------------

tb_has_nonempty <- function(x) {
  is.list(x) && length(x) > 0
}

# ---- Label coordinate helpers -------------------------------------------------
#
# Label drag positions are stored as axis-normalized coordinates (0..1) with the
# axis range used at the time of saving. Normalization is used so that labels
# can be replayed in both plotly and ggplot renderers even when the plot is
# resized or the axis direction is reversed.
#
# - Reversed ranges are supported (e.g., c(10, 0)); the mapping preserves the
#   axis direction such that range[[1]] maps to 0 and range[[2]] maps to 1.
# - Non-finite x/y values become NA and should be omitted by callers.
# - Values outside the range are clamped to [0, 1] during normalization and
#   denormalization to avoid runaway positions.

tb_normalize_coords <- function(x, y, x_range, y_range) {
  clamp01 <- function(v) pmin(1, pmax(0, v))
  range_ok <- function(r) is.numeric(r) && length(r) == 2L && all(is.finite(r)) && !isTRUE(all(r[1] == r[2]))

  x_out <- rep(NA_real_, length(x %||% numeric()))
  y_out <- rep(NA_real_, length(y %||% numeric()))
  if (!range_ok(x_range) || !range_ok(y_range)) return(list(x = x_out, y = y_out))

  x <- as.numeric(x)
  y <- as.numeric(y)

  x_ok <- is.finite(x)
  y_ok <- is.finite(y)

  x_den <- (x_range[[2]] - x_range[[1]])
  y_den <- (y_range[[2]] - y_range[[1]])

  x_tmp <- rep(NA_real_, length(x))
  y_tmp <- rep(NA_real_, length(y))

  if (is.finite(x_den) && x_den != 0) x_tmp[x_ok] <- (x[x_ok] - x_range[[1]]) / x_den
  if (is.finite(y_den) && y_den != 0) y_tmp[y_ok] <- (y[y_ok] - y_range[[1]]) / y_den

  list(x = clamp01(x_tmp), y = clamp01(y_tmp))
}

tb_denormalize_coords <- function(x_norm, y_norm, x_range, y_range) {
  clamp01 <- function(v) pmin(1, pmax(0, v))
  range_ok <- function(r) is.numeric(r) && length(r) == 2L && all(is.finite(r)) && !isTRUE(all(r[1] == r[2]))

  x_out <- rep(NA_real_, length(x_norm %||% numeric()))
  y_out <- rep(NA_real_, length(y_norm %||% numeric()))
  if (!range_ok(x_range) || !range_ok(y_range)) return(list(x = x_out, y = y_out))

  x_norm <- as.numeric(x_norm)
  y_norm <- as.numeric(y_norm)

  x_ok <- is.finite(x_norm)
  y_ok <- is.finite(y_norm)

  x_den <- (x_range[[2]] - x_range[[1]])
  y_den <- (y_range[[2]] - y_range[[1]])

  x_tmp <- rep(NA_real_, length(x_norm))
  y_tmp <- rep(NA_real_, length(y_norm))

  if (is.finite(x_den) && x_den != 0) x_tmp[x_ok] <- x_range[[1]] + clamp01(x_norm[x_ok]) * x_den
  if (is.finite(y_den) && y_den != 0) y_tmp[y_ok] <- y_range[[1]] + clamp01(y_norm[y_ok]) * y_den

  list(x = x_tmp, y = y_tmp)
}

tb_publication_export_defaults <- function(style = NULL, aspect_ratio = 4 / 3) {
  style <- style %||% list()

  width_in <- suppressWarnings(as.numeric(style$export_width %||% style$width %||% 7.5))
  if (!is.finite(width_in) || width_in <= 0) width_in <- 7.5

  height_in <- suppressWarnings(as.numeric(style$export_height %||% style$height))
  if (!is.finite(height_in) || height_in <= 0) {
    ar <- suppressWarnings(as.numeric(aspect_ratio))
    if (!is.finite(ar) || ar <= 0) ar <- 4 / 3
    height_in <- width_in / ar
  }

  dpi <- suppressWarnings(as.integer(style$export_dpi %||% 300))
  if (!is.finite(dpi) || dpi <= 0) dpi <- 300L

  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  svg_device <- if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else "svg"

  list(
    width = width_in,
    height = height_in,
    units = "in",
    dpi = dpi,
    pdf_device = pdf_device,
    svg_device = svg_device
  )
}

tb_merge_states <- function(base, override) {
  base <- base %||% list(style = list(), plotly = list(), visibility = list())
  override <- override %||% list(style = list(), plotly = NULL, visibility = NULL)
  
  out <- base
  out$style <- modifyList(base$style %||% list(), override$style %||% list())
  
  # plotly/visibility: override iff explicitly provided (non-NULL). An empty list
  # is meaningful here (it represents a cleared override and must win).
  if (!is.null(override$plotly)) out$plotly <- override$plotly
  if (!is.null(override$visibility)) out$visibility <- override$visibility
  
  out
}

# ---- Render state (override-only + merged) ----------------------------------

tb_load_render_state_override <- function(node_dir, use_cache = TRUE) {
  key <- tb_norm(node_dir)
  if (isTRUE(use_cache) && !is.null(.tb_cache$render_state[[key]])) {
    return(.tb_cache$render_state[[key]])
  }
  p <- tb_node_paths(node_dir)
  if (!file.exists(p$render_state)) {
    result <- list(style = list(), plotly = NULL, visibility = NULL)
  } else {
    rs <- tryCatch(tb_read_json(p$render_state), error = function(e) list())
    result <- list(
      style = rs$style %||% list(),
      plotly = rs$plotly %||% NULL,
      visibility = rs$visibility %||% NULL
    )
  }
  if (isTRUE(use_cache)) .tb_cache$render_state[[key]] <- result
  result
}

tb_load_render_state <- function(node_dir) {
  # Merged terpbook defaults + sparse overrides.
  defaults <- tb_node_defaults(node_dir)
  override <- tb_load_render_state_override(node_dir)
  tb_merge_states(defaults, override)
}

tb_save_render_state <- function(node_dir, rs_override_only) {
  # Save sparse override-only object.
  rs_override_only$updated_at <- as.character(Sys.time())
  p <- tb_node_paths(node_dir)
  tb_write_json(rs_override_only, p$render_state)
  invisible(TRUE)
}

tb_delete_render_state <- function(node_dir) {
  p <- tb_node_paths(node_dir)
  if (file.exists(p$render_state)) file.remove(p$render_state)
  invisible(TRUE)
}

# ---- Tree parsing ------------------------------------------------------------

tb_build_step_tree <- function(step_row) {
  step_dir <- tb_norm(step_row$step_dir)
  step_desc <- tb_read_node_descriptor(step_dir, "step")
  

  # Compute engine_id first since we need it for label fallback
  engine_id <- tolower(as.character(step_desc$engine_id %||% step_row$engine_id %||% ""))

  # Determine display label: prefer explicit label, but if it looks like an
  # auto-generated step_id (contains "step_" with timestamp or hex suffix), use engine_id instead
  raw_label <- as.character(step_desc$label %||% step_row$step_id %||% "")
  is_auto_id <- grepl("^step_\\d{14}|_[0-9a-f]{8}$", raw_label, perl = TRUE)
  display_label <- if (is_auto_id && nzchar(engine_id)) engine_id else (raw_label %||% engine_id %||% "")
  step_node <- list(
    node_id = paste0("step:", step_row$step_index %||% "NA"),
    parent_id = NA_character_,
    depth = 0L,
    kind = "step",
    step_index = step_row$step_index %||% NA_integer_,
    step_id = as.character(step_row$step_id %||% ""),
    engine_id = engine_id,
    label = display_label,
    node_dir = step_dir,
    meta = step_desc$meta %||% list()
  )
  
  nodes <- list(step_node)
  
  views <- tb_as_list_of_rows(step_desc$views %||% list())

  # Legacy IDQuant view-id mapping:
  # - Old terpbooks wrote views: group/replicate/cv/overlap
  # - New terpbooks write container substeps: substep_001..substep_005
  # Map legacy IDs to the new substep IDs while keeping paths pointed at the
  # legacy view directories so results + render_state.json overrides are preserved.
  if (identical(engine_id, "idquant") && length(views) > 0) {
    vids <- vapply(views, function(v) as.character((v %||% list())$view_id %||% (v %||% list())$id %||% ""), character(1))
    has_substep_ids <- any(grepl("^substep_\\d+$", vids))
    has_legacy_ids <- any(vids %in% c("group", "replicate", "cv", "overlap"))

    if (isTRUE(has_legacy_ids) && !isTRUE(has_substep_ids)) {
      get_view_path <- function(id) {
        idx <- which(vids == id)
        if (length(idx) == 0) return(NULL)
        as.character((views[[idx[[1]]]] %||% list())$path %||% "")
      }

      group_path <- get_view_path("group")
      if (!nzchar(group_path %||% "")) group_path <- get_view_path("replicate")
      cv_path <- get_view_path("cv")
      overlap_path <- get_view_path("overlap")

      if (nzchar(group_path %||% "") && nzchar(cv_path %||% "") && nzchar(overlap_path %||% "")) {
        views <- list(
          list(view_id = "substep_001", label = "ID Quantification", engine_id = "idquant_id_quant", path = group_path),
          list(view_id = "substep_002", label = "Average Value", engine_id = "idquant_average_value", path = group_path),
          list(view_id = "substep_003", label = "CV% (scatter)", engine_id = "idquant_cv_scatter", path = cv_path),
          list(view_id = "substep_004", label = "CV% (bar)", engine_id = "idquant_cv_bar", path = cv_path),
          list(view_id = "substep_005", label = "Overlap", engine_id = "idquant_overlap", path = overlap_path)
        )
      }
    }
  }
  
  tb_walk_views <- function(parent_node, parent_dir, views_list, depth) {
    if (length(views_list) == 0) return()
    
    for (v in views_list) {
      vid <- as.character(v$view_id %||% v$id %||% v$name %||% "")
      vlab <- as.character(v$label %||% vid %||% "")
      vpath <- as.character(v$path %||% "")
      veng <- tolower(as.character(v$engine_id %||% parent_node$engine_id %||% ""))
      
      vdir <- tb_norm(file.path(parent_dir, vpath))
      vdesc <- tb_read_node_descriptor(vdir, "view")
      
      node_id <- paste0(parent_node$node_id, "/", vid)
      
      this_node <- list(
        node_id = node_id,
        parent_id = parent_node$node_id,
        depth = as.integer(depth),
        kind = "view",
        step_index = parent_node$step_index,
        step_id = parent_node$step_id,
        engine_id = tolower(as.character(veng %||% vdesc$engine_id %||% "")),
        label = as.character(vdesc$label %||% vlab %||% vid),
        node_dir = vdir,
        meta = vdesc$meta %||% v$meta %||% list()
      )
      
      nodes[[length(nodes) + 1L]] <<- this_node
      
      kids <- tb_as_list_of_rows(vdesc$views %||% v$views %||% list())
      if (length(kids) > 0) {
        tb_walk_views(this_node, vdir, kids, depth + 1L)
      }
    }
  }
  
  tb_walk_views(step_node, step_dir, views, depth = 1L)
  nodes
}

tb_nodes_df <- function(run_root, manifest, registry = NULL) {
  df_steps <- tb_steps_df(run_root, manifest)
  if (nrow(df_steps) == 0) {
    return(data.frame(
      node_id = character(), parent_id = character(), depth = integer(),
      kind = character(), step_index = integer(), engine_id = character(),
      label = character(), node_dir = character(), stringsAsFactors = FALSE
    ))
  }
  
  all_nodes <- list()
  for (i in seq_len(nrow(df_steps))) {
    step_nodes <- tb_build_step_tree(df_steps[i, , drop = FALSE])
    all_nodes <- c(all_nodes, step_nodes)
  }
  
  df <- do.call(rbind, lapply(all_nodes, function(n) {
    data.frame(
      node_id = as.character(n$node_id),
      parent_id = as.character(n$parent_id %||% NA_character_),
      depth = as.integer(n$depth %||% 0L),
      kind = as.character(n$kind %||% "view"),
      step_index = suppressWarnings(as.integer(n$step_index %||% NA_integer_)),
      engine_id = as.character(n$engine_id %||% ""),
      label = as.character(n$label %||% ""),
      node_dir = as.character(n$node_dir %||% ""),
      stringsAsFactors = FALSE
    )
  }))
  
  df$node_dir <- tb_norm(df$node_dir)
  if (exists("migrate_legacy_engine_name", mode = "function")) {
    df$engine_id <- migrate_legacy_engine_name(df$engine_id)
  }

  # Resolve display labels from the engine registry so that sidebar, exports,

  # and other downstream consumers show human-friendly names instead of raw
  # engine IDs (e.g. "GO Over-Representation Analysis" instead of "goora").
  if (!is.null(registry)) {
    engines <- registry$engines %||% list()
    for (i in seq_len(nrow(df))) {
      eid <- df$engine_id[i]
      if (!nzchar(eid)) next
      edef <- engines[[eid]]
      if (is.null(edef$label)) next
      current_label <- df$label[i]
      prettified_eid <- trimws(gsub("_", " ", eid))
      if (!nzchar(current_label) ||
          identical(tolower(current_label), eid) ||
          identical(tolower(current_label), prettified_eid)) {
        df$label[i] <- edef$label
      }
    }
  }

  df
}

tb_node_meta <- function(node_dir) {
  d <- tb_read_descriptor_any(node_dir)
  d$meta %||% list()
}

# ---- Style inheritance -------------------------------------------------------

tb_ancestor_chain <- function(nodes_df, node_id) {
  if (is.null(node_id) || !nzchar(node_id)) return(character())
  idx <- match(node_id, nodes_df$node_id)
  if (!is.finite(idx)) return(character())
  
  chain <- character()
  cur <- node_id
  seen <- character()
  
  while (nzchar(cur) && !(cur %in% seen)) {
    seen <- c(seen, cur)
    chain <- c(cur, chain)
    j <- match(cur, nodes_df$node_id)
    if (!is.finite(j)) break
    pid <- nodes_df$parent_id[[j]]
    if (is.na(pid) || !nzchar(pid)) break
    cur <- pid
  }
  
  chain
}

tb_effective_render_state <- function(nodes_df, node_id, use_cache = TRUE) {
  cache_key <- as.character(node_id)
  if (isTRUE(use_cache) && !is.null(.tb_cache$effective[[cache_key]])) {
    return(.tb_cache$effective[[cache_key]])
  }

  chain <- tb_ancestor_chain(nodes_df, node_id)
  if (length(chain) == 0) return(list(style = list(), plotly = list(), visibility = list()))

  state <- list(style = list(), plotly = list(), visibility = list())
  for (nid in chain) {
    j <- match(nid, nodes_df$node_id)
    nd <- nodes_df$node_dir[[j]]
    rs_node <- tb_load_render_state(nd) # defaults + overrides at this node
    state <- tb_merge_states(state, rs_node)

    # IDQuant legacy support:
    # Merge replicate-view overrides into the new "ID Quantification" child node.
    # This preserves legacy customizations when the replicate node is no longer a standalone child.
    eng <- tolower(as.character(nodes_df$engine_id[[j]] %||% ""))
    if (identical(eng, "idquant_id_quant")) {
      step_dir <- tb_norm(file.path(nd, "..", ".."))
      rep_dir <- tb_norm(file.path(step_dir, "views", "replicate"))
      if (dir.exists(rep_dir)) {
        rs_rep <- tb_load_render_state(rep_dir)
        state <- tb_merge_states(state, rs_rep)
      }
    }
  }

  if (isTRUE(use_cache)) .tb_cache$effective[[cache_key]] <- state
  state
}

# ---- Child View Creation (for cluster GO-ORA, etc.) -------------------------

#' Create a child view directory under a parent step
#'
#' @param parent_dir Parent node directory (step or view)
#' @param view_id Unique ID for the child view (e.g., "cluster_1_goora")
#' @param engine_id Engine type for the child (e.g., "goora")
#' @param label Display label for the child
#' @param params Engine parameters list
#' @param style Style defaults list (optional)
#' @return Path to created view directory
tb_create_child_view <- function(parent_dir, view_id, engine_id, label, params, style = list()) {
  tb_require_jsonlite()

  parent_dir <- tb_norm(parent_dir)
  if (!dir.exists(parent_dir)) {
    stop("Parent directory does not exist: ", parent_dir)
  }

  views_dir <- file.path(parent_dir, "views")
  dir.create(views_dir, showWarnings = FALSE, recursive = TRUE)

  view_dir <- file.path(views_dir, view_id)
  dir.create(view_dir, showWarnings = FALSE, recursive = TRUE)

  # Build view descriptor
  view_desc <- list(
    view_id = view_id,
    engine_id = engine_id,
    label = label,
    params = params,
    style = style
  )

  # Write view.json
  view_json_path <- file.path(view_dir, "view.json")
  jsonlite::write_json(
    tb_json_safe(view_desc),
    view_json_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  # Register the new view in the parent's descriptor (step.json or view.json)
  # so that tb_build_step_tree can discover it
  parent_step_json <- file.path(parent_dir, "step.json")
  parent_view_json <- file.path(parent_dir, "view.json")

  parent_desc_path <- if (file.exists(parent_step_json)) {
    parent_step_json
  } else if (file.exists(parent_view_json)) {
    parent_view_json
  } else {
    NULL
  }

  if (!is.null(parent_desc_path)) {
    parent_desc <- tryCatch(tb_read_json(parent_desc_path), error = function(e) list())

    # Get existing views list
    existing_views <- tb_as_list_of_rows(parent_desc$views %||% list())

    # Check if this view_id already exists
    existing_ids <- vapply(existing_views, function(v) {
      as.character((v %||% list())$view_id %||% (v %||% list())$id %||% "")
    }, character(1))

    if (!(view_id %in% existing_ids)) {
      # Add new view reference (path is relative: views/<view_id>)
      # Include engine_id so tb_walk_views picks it up correctly
      new_view_ref <- list(
        view_id = view_id,
        engine_id = engine_id,
        path = file.path("views", view_id)
      )
      parent_desc$views <- c(existing_views, list(new_view_ref))

      # Write updated parent descriptor
      jsonlite::write_json(
        tb_json_safe(parent_desc),
        parent_desc_path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )

      # Invalidate cache for parent node
      cache_key <- paste0(parent_dir, "|", if (file.exists(parent_step_json)) "step" else "view")
      .tb_cache$descriptors[[cache_key]] <- NULL
    }
  }

  tb_norm(view_dir)
}

#' Save results to a child view directory
#'
#' @param view_dir Child view directory
#' @param results Engine results list
#' @return TRUE on success
tb_save_child_results <- function(view_dir, results) {
  view_dir <- tb_norm(view_dir)
  if (!dir.exists(view_dir)) {
    stop("View directory does not exist: ", view_dir)
  }

  results_path <- file.path(view_dir, "results.rds")
  saveRDS(results, results_path)

  # Invalidate cache for this node
  tb_cache_invalidate_node(view_dir)

  TRUE
}
