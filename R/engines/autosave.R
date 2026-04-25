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

# =========================================================
# Workspace helpers — stable per-file workspace dir under app cache.
#
# Replaces the per-session tempfile("terpbook_") pattern with a content-addressed
# workspace dir under tools::R_user_dir("MSTerp","cache")/workspaces. This makes
# in-progress edits survive Return-clicks, new file loads, and crashes. The
# debounced render_state.json writes are unchanged — only the parent dir moves.
# =========================================================

tb_workspace_root <- function() {
  root <- tryCatch(
    tools::R_user_dir("MSTerp", which = "cache"),
    error = function(e) file.path(tempdir(), "MSTerp")
  )
  ws <- file.path(root, "workspaces")
  if (!dir.exists(ws)) dir.create(ws, recursive = TRUE, showWarnings = FALSE)
  ws
}

# Short stable ASCII hash of a string (uses tools::md5sum via a tempfile because
# md5sum operates on files; falls back to a simple xor-based hash if anything
# fails). Returns 12 hex chars.
.tb_str_hash12 <- function(s) {
  s <- as.character(s %||% "")
  out <- tryCatch({
    tf <- tempfile("tbhash_")
    on.exit(try(file.remove(tf), silent = TRUE), add = TRUE)
    writeBin(charToRaw(enc2utf8(s)), tf)
    h <- unname(tools::md5sum(tf))
    if (is.na(h) || !nzchar(h)) stop("md5sum failed")
    substr(h, 1, 12)
  }, error = function(e) NA_character_)
  if (is.na(out)) {
    # Fallback: deterministic but weaker
    bytes <- as.integer(charToRaw(enc2utf8(s)))
    if (length(bytes) == 0) return(strrep("0", 12))
    acc <- 5381L
    for (b in bytes) acc <- bitwXor(bitwShiftL(acc, 5L) + acc, b)
    out <- sprintf("%012x", abs(as.numeric(acc)) %% 16^12)
  }
  out
}

# Fingerprint of a file's content based on size + first/last 64KB.
# Cheap, but distinguishes nearly all real-world re-saves.
.tb_file_content_hash <- function(path) {
  if (is.null(path) || !file.exists(path)) return("missing")
  info <- file.info(path)
  size <- as.numeric(info$size %||% 0)
  if (!is.finite(size) || size <= 0) return("empty")
  con <- tryCatch(file(path, open = "rb"), error = function(e) NULL)
  if (is.null(con)) return("unreadable")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  head_n <- as.integer(min(size, 65536))
  head_bytes <- tryCatch(readBin(con, what = "raw", n = head_n), error = function(e) raw())
  tail_bytes <- raw()
  if (size > 131072) {
    tryCatch(seek(con, where = size - 65536, origin = "start"), error = function(e) NULL)
    tail_bytes <- tryCatch(readBin(con, what = "raw", n = 65536L), error = function(e) raw())
  }
  combined <- paste0(
    "s=", format(size, scientific = FALSE),
    "|h=", paste(as.integer(head_bytes), collapse = ","),
    "|t=", paste(as.integer(tail_bytes), collapse = ",")
  )
  .tb_str_hash12(combined)
}

# Sanitize a basename for use in a directory name.
.tb_safe_basename <- function(name) {
  s <- as.character(name %||% "untitled")
  s <- sub("\\.terpbook$", "", s, ignore.case = TRUE)
  s <- gsub("[^A-Za-z0-9._-]+", "_", s)
  s <- substr(s, 1, 40)
  if (!nzchar(s)) s <- "untitled"
  s
}

# Build a deterministic workspace ID for a file. If path is a real source path
# on disk, key on absolute path + content fingerprint. If path is missing
# (browser upload), key on content hash + basename only.
tb_workspace_id <- function(path, display_name = NULL) {
  base <- .tb_safe_basename(display_name %||% basename(path %||% "untitled"))
  if (!is.null(path) && nzchar(path) && file.exists(path)) {
    abs <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
                    error = function(e) path)
    info <- file.info(abs)
    mtime_size <- paste0("m=", as.numeric(info$mtime %||% 0),
                         "|sz=", as.numeric(info$size %||% 0))
    path_h <- .tb_str_hash12(abs)
    content_h <- .tb_str_hash12(paste0(mtime_size, "|", .tb_file_content_hash(abs)))
    paste0(path_h, "__", content_h, "__", base)
  } else {
    content_h <- if (!is.null(path) && file.exists(path)) {
      .tb_file_content_hash(path)
    } else {
      .tb_str_hash12(paste0("noPath|", base, "|", as.numeric(Sys.time())))
    }
    paste0("upload__", content_h, "__", base)
  }
}

.tb_workspace_meta_path <- function(dir) file.path(dir, "workspace.json")
.tb_workspace_lock_path <- function(dir) file.path(dir, "workspace.lock")
.tb_workspace_clean_path <- function(dir) file.path(dir, ".clean")

# Read the workspace metadata file, returning an empty list on failure.
tb_workspace_meta_read <- function(dir) {
  p <- .tb_workspace_meta_path(dir)
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::read_json(p, simplifyVector = TRUE), error = function(e) list())
}

tb_workspace_meta_write <- function(dir, meta) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tb_atomic_write_json(meta, .tb_workspace_meta_path(dir))
}

# Inspect a lock file. Returns list(present=, fresh=, foreign=, pid=, session_id=, age_s=).
# A lock is "fresh" if its timestamp is within 5 min. "foreign" means it does
# not belong to the calling session_id.
tb_workspace_lock_status <- function(dir, session_id = NULL) {
  p <- .tb_workspace_lock_path(dir)
  if (!file.exists(p)) {
    return(list(present = FALSE, fresh = FALSE, foreign = FALSE,
                pid = NA_integer_, session_id = NA_character_, age_s = NA_real_))
  }
  info <- tryCatch(jsonlite::read_json(p, simplifyVector = TRUE), error = function(e) list())
  ts <- suppressWarnings(as.numeric(info$ts %||% NA))
  age <- if (is.finite(ts)) as.numeric(Sys.time()) - ts else NA_real_
  fresh <- is.finite(age) && age >= 0 && age <= 300
  pid <- suppressWarnings(as.integer(info$pid %||% NA))
  sid <- as.character(info$session_id %||% NA_character_)
  foreign <- !identical(sid, as.character(session_id %||% ""))
  list(present = TRUE, fresh = fresh, foreign = foreign,
       pid = pid, session_id = sid, age_s = age)
}

tb_workspace_lock <- function(dir, session_id) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tb_atomic_write_json(list(
    pid = Sys.getpid(),
    session_id = as.character(session_id %||% ""),
    ts = as.numeric(Sys.time())
  ), .tb_workspace_lock_path(dir))
  invisible(TRUE)
}

tb_workspace_unlock <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(invisible(FALSE))
  p <- .tb_workspace_lock_path(dir)
  if (file.exists(p)) try(file.remove(p), silent = TRUE)
  invisible(TRUE)
}

# Mark workspace as clean: write a .clean sentinel containing the source mtime
# at save time. tb_workspace_is_dirty() compares against this when scanning.
tb_workspace_mark_clean <- function(dir, source_mtime = NA) {
  if (is.null(dir) || !dir.exists(dir)) return(invisible(FALSE))
  payload <- list(
    cleaned_at = as.numeric(Sys.time()),
    source_mtime = if (is.numeric(source_mtime)) as.numeric(source_mtime) else NA_real_
  )
  tb_atomic_write_json(payload, .tb_workspace_clean_path(dir))
  invisible(TRUE)
}

# Find the most recent render_state.json mtime in a workspace dir; NA if none.
.tb_workspace_last_edit <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(NA_real_)
  files <- list.files(dir, pattern = "^render_state\\.json$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(NA_real_)
  mt <- file.info(files)$mtime
  if (length(mt) == 0) return(NA_real_)
  as.numeric(max(mt, na.rm = TRUE))
}

# A workspace is "dirty" if it has render_state.json files newer than the
# .clean sentinel (or no .clean sentinel at all).
tb_workspace_is_dirty <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(FALSE)
  last_edit <- .tb_workspace_last_edit(dir)
  if (!is.finite(last_edit)) return(FALSE)
  cp <- .tb_workspace_clean_path(dir)
  if (!file.exists(cp)) return(TRUE)
  cleaned_at <- tryCatch({
    info <- jsonlite::read_json(cp, simplifyVector = TRUE)
    as.numeric(info$cleaned_at %||% file.info(cp)$mtime)
  }, error = function(e) as.numeric(file.info(cp)$mtime %||% 0))
  isTRUE(last_edit > cleaned_at + 0.5)  # 0.5s slop for filesystem rounding
}

# Resolve where a file's workspace lives. Performs lock-aware sibling forking
# when another live session holds the canonical workspace.
#
# Returns:
#   dir                  — workspace directory path
#   workspace_id         — final ID (may differ from canonical if forked)
#   canonical_id         — the unforked ID
#   is_new               — TRUE if dir was just created (no prior content)
#   has_dirty_overrides  — TRUE if existing dir contains edits newer than source
#   last_edit_at         — POSIXct of last render_state.json write (or NA)
#   forked               — TRUE if foreign-lock forced a sibling workspace
#   meta                 — current workspace meta (read from disk)
tb_workspace_resolve <- function(path, display_name = NULL, session_id = NULL) {
  root <- tb_workspace_root()
  canonical_id <- tb_workspace_id(path, display_name)

  # If canonical workspace is held by a live foreign session, fork.
  canonical_dir <- file.path(root, canonical_id)
  forked <- FALSE
  workspace_id <- canonical_id
  dir <- canonical_dir
  if (dir.exists(canonical_dir)) {
    st <- tb_workspace_lock_status(canonical_dir, session_id = session_id)
    if (isTRUE(st$present) && isTRUE(st$fresh) && isTRUE(st$foreign)) {
      sess_short <- substr(.tb_str_hash12(as.character(session_id %||% Sys.time())), 1, 6)
      workspace_id <- paste0(canonical_id, "__sess", sess_short)
      dir <- file.path(root, workspace_id)
      forked <- TRUE
    }
  }

  is_new <- !dir.exists(dir)
  if (is_new) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  has_dirty <- tb_workspace_is_dirty(dir)
  last_edit <- .tb_workspace_last_edit(dir)
  last_edit_at <- if (is.finite(last_edit)) as.POSIXct(last_edit, origin = "1970-01-01") else as.POSIXct(NA)

  # Update or seed metadata.
  meta <- tb_workspace_meta_read(dir)
  abs_path <- if (!is.null(path) && nzchar(path) && file.exists(path)) {
    tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path)
  } else NA_character_
  meta$source_path <- abs_path
  meta$source_basename <- as.character(display_name %||% basename(path %||% "untitled"))
  meta$canonical_id <- canonical_id
  meta$workspace_id <- workspace_id
  meta$forked <- forked
  if (is.null(meta$created_at)) meta$created_at <- as.numeric(Sys.time())
  meta$last_opened_at <- as.numeric(Sys.time())
  if (!is.na(abs_path)) {
    info <- file.info(abs_path)
    meta$source_mtime <- as.numeric(info$mtime %||% NA)
    meta$source_size <- as.numeric(info$size %||% NA)
  }
  tryCatch(tb_workspace_meta_write(dir, meta), error = function(e) NULL)

  list(
    dir = dir,
    workspace_id = workspace_id,
    canonical_id = canonical_id,
    is_new = is_new,
    has_dirty_overrides = has_dirty,
    last_edit_at = last_edit_at,
    forked = forked,
    meta = meta
  )
}

# Open an isolated sibling workspace ("Open Fresh Copy"). Always creates a new
# dir suffixed with a timestamp; the caller is responsible for unzipping into it.
tb_workspace_fresh_copy <- function(path, display_name = NULL, session_id = NULL) {
  root <- tb_workspace_root()
  canonical_id <- tb_workspace_id(path, display_name)
  suffix <- format(Sys.time(), "%Y%m%d%H%M%S")
  workspace_id <- paste0(canonical_id, "__fresh", suffix)
  dir <- file.path(root, workspace_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  meta <- list(
    source_path = if (!is.null(path) && file.exists(path))
      tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path) else NA_character_,
    source_basename = as.character(display_name %||% basename(path %||% "untitled")),
    canonical_id = canonical_id,
    workspace_id = workspace_id,
    forked = TRUE,
    fresh_copy = TRUE,
    created_at = as.numeric(Sys.time()),
    last_opened_at = as.numeric(Sys.time())
  )
  tryCatch(tb_workspace_meta_write(dir, meta), error = function(e) NULL)
  list(dir = dir, workspace_id = workspace_id, canonical_id = canonical_id,
       is_new = TRUE, has_dirty_overrides = FALSE,
       last_edit_at = as.POSIXct(NA), forked = TRUE, meta = meta)
}

# Discard a workspace (delete its contents) and recreate it empty. Used by
# the "Discard" branch of the recovery modal.
tb_workspace_reset <- function(dir) {
  if (is.null(dir) || !nzchar(dir)) return(invisible(FALSE))
  if (dir.exists(dir)) try(unlink(dir, recursive = TRUE, force = TRUE), silent = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

# Sweep old workspaces: clean ones beyond ttl_clean_days, anything beyond
# ttl_dirty_days, and cap dirty workspaces at `cap` (oldest-evicted).
tb_workspace_sweep <- function(ttl_clean_days = NULL, ttl_dirty_days = NULL, cap = NULL) {
  ttl_clean <- as.numeric(getOption("MSTERP_WORKSPACE_TTL_DAYS", ttl_clean_days %||% 14))
  ttl_dirty <- as.numeric(getOption("MSTERP_WORKSPACE_DIRTY_TTL_DAYS", ttl_dirty_days %||% 90))
  workspace_cap <- as.integer(getOption("MSTERP_WORKSPACE_CAP", cap %||% 20L))

  root <- tb_workspace_root()
  if (!dir.exists(root)) return(invisible(list(deleted = character(), warnings = character())))

  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  if (length(dirs) == 0) return(invisible(list(deleted = character(), warnings = character())))

  now <- as.numeric(Sys.time())
  warns <- character()
  deleted <- character()

  rows <- lapply(dirs, function(d) {
    lock_st <- tb_workspace_lock_status(d)
    if (isTRUE(lock_st$present) && isTRUE(lock_st$fresh)) return(NULL)  # in use
    last_edit <- .tb_workspace_last_edit(d)
    age <- (now - max(c(last_edit, file.info(d)$mtime, 0), na.rm = TRUE)) / 86400
    is_dirty <- tb_workspace_is_dirty(d)
    list(dir = d, age_days = age, dirty = is_dirty, last_edit = last_edit)
  })
  rows <- Filter(Negate(is.null), rows)

  for (r in rows) {
    if (!isTRUE(r$dirty) && r$age_days > ttl_clean) {
      try(unlink(r$dir, recursive = TRUE, force = TRUE), silent = TRUE)
      deleted <- c(deleted, r$dir); next
    }
    if (r$age_days > ttl_dirty) {
      try(unlink(r$dir, recursive = TRUE, force = TRUE), silent = TRUE)
      deleted <- c(deleted, r$dir); next
    }
  }

  rows <- Filter(function(r) !(r$dir %in% deleted), rows)
  dirty_rows <- Filter(function(r) isTRUE(r$dirty), rows)
  if (length(dirty_rows) > workspace_cap) {
    ord <- order(vapply(dirty_rows, function(r) r$age_days, numeric(1)), decreasing = TRUE)
    drop_n <- length(dirty_rows) - workspace_cap
    drop_dirs <- vapply(dirty_rows[ord[seq_len(drop_n)]], function(r) r$dir, character(1))
    for (d in drop_dirs) {
      try(unlink(d, recursive = TRUE, force = TRUE), silent = TRUE)
      deleted <- c(deleted, d)
    }
    warns <- c(warns, sprintf("Evicted %d oldest dirty workspace(s) over cap of %d.",
                              drop_n, workspace_cap))
  }

  invisible(list(deleted = deleted, warnings = warns))
}

# Return a list of dirty workspaces suitable for a recovery banner.
tb_workspace_scan_dirty <- function() {
  root <- tb_workspace_root()
  if (!dir.exists(root)) return(list())
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  out <- list()
  for (d in dirs) {
    if (!tb_workspace_is_dirty(d)) next
    lock_st <- tb_workspace_lock_status(d)
    if (isTRUE(lock_st$present) && isTRUE(lock_st$fresh)) next  # actively held
    meta <- tb_workspace_meta_read(d)
    last_edit <- .tb_workspace_last_edit(d)
    out[[length(out) + 1L]] <- list(
      dir = d,
      workspace_id = meta$workspace_id %||% basename(d),
      source_path = meta$source_path %||% NA_character_,
      source_basename = meta$source_basename %||% basename(d),
      last_edit_at = if (is.finite(last_edit)) as.POSIXct(last_edit, origin = "1970-01-01") else as.POSIXct(NA),
      forked = isTRUE(meta$forked)
    )
  }
  out[order(vapply(out, function(x) {
    t <- x$last_edit_at
    if (is.null(t) || all(is.na(t))) -Inf else as.numeric(t)
  }, numeric(1)), decreasing = TRUE)]
}

# Total disk size of workspace cache (bytes).
tb_workspace_total_size <- function() {
  root <- tb_workspace_root()
  if (!dir.exists(root)) return(0)
  fs <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(fs) == 0) return(0)
  sum(file.info(fs)$size, na.rm = TRUE)
}

# Wipe the entire workspace cache. Used by the "Clear recovery data" button.
# Skips actively-locked workspaces.
tb_workspace_clear_all <- function() {
  root <- tb_workspace_root()
  if (!dir.exists(root)) return(invisible(0L))
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  removed <- 0L
  for (d in dirs) {
    st <- tb_workspace_lock_status(d)
    if (isTRUE(st$present) && isTRUE(st$fresh)) next
    if (isTRUE(try(unlink(d, recursive = TRUE, force = TRUE) == 0, silent = TRUE))) {
      removed <- removed + 1L
    }
  }
  invisible(removed)
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

