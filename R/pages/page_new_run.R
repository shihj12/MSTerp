# =========================================================
# R/pages/page_new_run.R — New Run (terpbook runner UI)
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

nr_norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)


nr_sanitize <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  if (!nzchar(x)) return("run")
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

nr_fileinput_path <- function(f) {
  if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return(NULL)
  f$datapath
}

nr_ext <- function(path) tolower(tools::file_ext(path %||% ""))

nr_ui_validate_terpbase <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("terpbase", "rds")) return(list(ok = FALSE, msg = "Expected .terpbase/.rds"))
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) return(list(ok = FALSE, msg = "Cannot readRDS"))
  list(ok = TRUE, msg = "OK")
}

nr_ui_validate_metabobase <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("metabobase", "rds")) return(list(ok = FALSE, msg = "Expected .metabobase/.rds"))
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) return(list(ok = FALSE, msg = "Cannot readRDS"))
  list(ok = TRUE, msg = "OK")
}

nr_ui_validate_complexbase <- function(path) {
  # Empty path is OK for complexbase (optional)
  if (is.null(path) || !nzchar(path)) return(list(ok = TRUE, msg = "None selected"))
  if (!file.exists(path)) return(list(ok = FALSE, msg = "File not found"))
  ex <- nr_ext(path)
  if (!ex %in% c("complexbase", "rds")) return(list(ok = FALSE, msg = "Expected .complexbase/.rds"))
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) return(list(ok = FALSE, msg = "Cannot readRDS"))
  list(ok = TRUE, msg = "OK")
}

nr_ui_validate_formatted <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("xlsx", "xls")) return(list(ok = FALSE, msg = "Expected .xlsx (data+design sheets)"))

  if (!requireNamespace("readxl", quietly = TRUE)) return(list(ok = FALSE, msg = "readxl not installed"))

  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
  if (is.null(sh)) return(list(ok = FALSE, msg = "Cannot read sheets"))
  sh_l <- tolower(sh)

  # Check for single-dataset (data + design) or multi-dataset (data_a + data_b + design) format
  has_single_data <- "data" %in% sh_l
  has_multi_data <- all(c("data_a", "data_b") %in% sh_l)
  has_design <- "design" %in% sh_l

  if (!has_design) {
    return(list(ok = FALSE, msg = "Missing required sheet: design"))
  }
  if (!has_single_data && !has_multi_data) {
    return(list(ok = FALSE, msg = "Missing required sheets: data (or data_a + data_b for multi-dataset)"))
  }

  is_multi_dataset <- has_multi_data && !has_single_data

  # Read design (small)
  design <- tryCatch(
    as.data.frame(readxl::read_excel(path, sheet = sh[match("design", sh_l)], n_max = 5000)),
    error = function(e) e
  )
  if (inherits(design, "error") || !is.data.frame(design)) return(list(ok = FALSE, msg = "Cannot read design sheet"))

  nms <- tolower(names(design))
  if (!("record_type" %in% nms)) return(list(ok = FALSE, msg = "design missing record_type"))

  # Required for parsing meta/groups/cols
  need_any <- c("record_type")
  if (!all(need_any %in% nms)) return(list(ok = FALSE, msg = "design missing required columns"))

  # Meta checks
  meta_ok <- all(c("key", "value") %in% nms)
  if (!meta_ok) return(list(ok = FALSE, msg = "design meta requires key,value"))

  meta <- design[tolower(design$record_type) == "meta", , drop = FALSE]
  keys <- tolower(as.character(meta$key %||% character()))
  if (!any(keys == "analysis_level")) return(list(ok = FALSE, msg = "meta missing analysis_level"))
  if (!any(keys == "id_primary_col")) return(list(ok = FALSE, msg = "meta missing id_primary_col"))

  # Column mapping checks
  col_need <- c("include", "data_col", "group_name", "color", "replicate")
  if (!all(col_need %in% nms)) return(list(ok = FALSE, msg = paste0("design column records require: ", paste(col_need, collapse = ", "))))

  cols <- design[tolower(design$record_type) == "column", , drop = FALSE]
  if (nrow(cols) == 0) return(list(ok = FALSE, msg = "No column records in design"))

  include <- cols$include
  include <- if (is.logical(include)) include else tolower(as.character(include)) %in% c("true","t","1","yes")
  cols_in <- cols[include %in% TRUE, , drop = FALSE]
  if (nrow(cols_in) == 0) return(list(ok = FALSE, msg = "No included measurement columns"))

  # Validate measurement columns exist in data sheet(s)
  if (is_multi_dataset) {
    # Multi-dataset: validate columns against data_a and data_b based on source_dataset
    data_head_a <- tryCatch(
      as.data.frame(readxl::read_excel(path, sheet = sh[match("data_a", sh_l)], n_max = 1)),
      error = function(e) e
    )
    data_head_b <- tryCatch(
      as.data.frame(readxl::read_excel(path, sheet = sh[match("data_b", sh_l)], n_max = 1)),
      error = function(e) e
    )
    if (inherits(data_head_a, "error") || !is.data.frame(data_head_a)) return(list(ok = FALSE, msg = "Cannot read data_a sheet"))
    if (inherits(data_head_b, "error") || !is.data.frame(data_head_b)) return(list(ok = FALSE, msg = "Cannot read data_b sheet"))

    data_cols_a <- names(data_head_a)
    data_cols_b <- names(data_head_b)

    # Check columns based on source_dataset if present
    if ("source_dataset" %in% names(cols_in)) {
      cols_a <- cols_in[tolower(cols_in$source_dataset) == "a", , drop = FALSE]
      cols_b <- cols_in[tolower(cols_in$source_dataset) == "b", , drop = FALSE]

      meas_a <- as.character(cols_a$data_col %||% character())
      meas_b <- as.character(cols_b$data_col %||% character())

      missing_a <- setdiff(meas_a, data_cols_a)
      missing_b <- setdiff(meas_b, data_cols_b)

      if (length(missing_a) > 0) return(list(ok = FALSE, msg = paste0("Missing columns in data_a: ", missing_a[[1]])))
      if (length(missing_b) > 0) return(list(ok = FALSE, msg = paste0("Missing columns in data_b: ", missing_b[[1]])))
    } else {
      # No source_dataset column - check against combined columns
      all_data_cols <- union(data_cols_a, data_cols_b)
      meas <- as.character(cols_in$data_col %||% character())
      missing_meas <- setdiff(meas, all_data_cols)
      if (length(missing_meas) > 0) return(list(ok = FALSE, msg = paste0("Missing measurement columns: ", missing_meas[[1]])))
    }

    total_cols <- nrow(cols_in)
    list(ok = TRUE, msg = paste0("OK (multi-dataset, ", total_cols, " measurement cols)"))
  } else {
    # Single-dataset: validate columns against data sheet
    data_head <- tryCatch(
      as.data.frame(readxl::read_excel(path, sheet = sh[match("data", sh_l)], n_max = 1)),
      error = function(e) e
    )
    if (inherits(data_head, "error") || !is.data.frame(data_head)) return(list(ok = FALSE, msg = "Cannot read data sheet"))

    data_cols <- names(data_head)
    meas <- as.character(cols_in$data_col %||% character())
    missing_meas <- setdiff(meas, data_cols)
    if (length(missing_meas) > 0) return(list(ok = FALSE, msg = paste0("Missing measurement columns in data: ", missing_meas[[1]])))

    list(ok = TRUE, msg = paste0("OK (", length(meas), " measurement cols)"))
  }
}


nr_ui_validate_terpflow <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("terpflow", "rds")) return(list(ok = FALSE, msg = "Expected .terpflow"))
  flow <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(flow, "error")) return(list(ok = FALSE, msg = "Cannot readRDS"))
  if (!is.list(flow)) return(list(ok = FALSE, msg = "Invalid object"))
  steps <- flow$steps %||% NULL
  if (!is.list(steps) || length(steps) == 0) return(list(ok = FALSE, msg = "No steps"))
  ok_engine <- all(vapply(steps, function(s) is.list(s) && nzchar(s$engine_id %||% ""), logical(1)))
  if (!isTRUE(ok_engine)) return(list(ok = FALSE, msg = "Step missing engine_id"))
  list(ok = TRUE, msg = "OK")
}

page_new_run_ui <- function() {
  msterp_page(
    title = NULL,
    actions = NULL,
    div(
      class = "nr-wrap",
      tags$head(
        tags$style(HTML("
        .nr-wrap { height: 100%; min-height: 0; display: flex; flex-direction: column; }
        .nr-main { flex: 1; min-height: 0; display: flex; padding: 20px 0; justify-content: center; }
        .nr-main .msterp-gate { flex: 1 1 auto; }

        /* Running panel: full width */
        .nr-running-panel {
          flex: 1 1 auto; min-width: 600px; max-width: 800px; width: 100%;
          background: var(--md-card-bg); border: 1px solid var(--md-card-border); border-radius: 12px;
          padding: 16px; overflow: auto; min-height: 0;
          box-shadow: var(--md-card-shadow);
        }

        .nr-title { font-weight: 800; font-size: 18px; margin-bottom: 12px; text-align: center; }

        .nr-row { display: flex; align-items: center; justify-content: space-between; gap: 10px; }
        .nr-light { display: inline-flex; align-items: center; gap: 8px; font-weight: 700; }
        .nr-dot { width: 10px; height: 10px; border-radius: 999px; display: inline-block; }
        .nr-dot.red { background: #d11; }
        .nr-dot.green { background: #1a8f3a; }
        .nr-dot.gray { background: #999; }
        .nr-msg { color: var(--text-primary); font-size: 12px; font-weight: 700; }

        .nr-section { border-top: 1px solid var(--md-border); margin-top: 10px; padding-top: 10px; }

        .nr-progress-wrap { margin-top: 10px; }
        .nr-progress-bar {
          height: 10px; background: var(--bg-muted); border-radius: 999px; overflow: hidden; border: 1px solid var(--border-light);
        }
        .nr-progress-fill {
          height: 10px; background: var(--progress-fill); width: 0%;
          transition: width 0.3s ease;
        }
        .nr-progress-fill.active {
          background: repeating-linear-gradient(
            -45deg,
            var(--progress-fill),
            var(--progress-fill) 10px,
            var(--progress-fill-dark) 10px,
            var(--progress-fill-dark) 20px
          );
          background-size: 28px 28px;
          animation: progress-stripe 0.6s linear infinite;
        }
        @keyframes progress-stripe {
          0% { background-position: 0 0; }
          100% { background-position: 28px 0; }
        }

        .nr-elapsed {
          font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
          font-size: 13px;
          font-weight: 700;
          color: var(--text-secondary);
        }

        .nr-status-pill {
          display: inline-block; padding: 6px 10px; border-radius: 999px;
          border: 1px solid var(--md-border); background: var(--bg-card); font-weight: 700; font-size: 12px;
        }
        .nr-status-pill .shiny-text-output {
          background: transparent; border: none; border-radius: 0; padding: 0;
          font-family: inherit; font-size: inherit; white-space: normal;
        }

        /* Flickering effect for running state */
        .nr-status-pill.running {
          background-color: var(--status-warn-bg);
          border-color: var(--status-warn-border);
          animation: pulse-running 1.5s infinite;
        }
        @keyframes pulse-running {
          0%, 100% { opacity: 1; box-shadow: 0 0 4px rgba(255, 193, 7, 0.4); }
          50% { opacity: 0.7; box-shadow: 0 0 8px rgba(255, 193, 7, 0.8); }
        }

        .nr-log {
          white-space: pre-wrap;
          overflow-wrap: break-word;
          word-break: normal;
          font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;
          font-size: 12.5px;
          line-height: 1.5;
          tab-size: 2;
          padding: 12px 14px;
          border: 1px solid var(--logs-border);
          border-radius: 12px;
          background: var(--logs-bg);
          color: var(--logs-text);
          min-height: 240px;
          max-height: 520px;
          overflow-x: auto;
          overflow-y: auto;
        }

        .nr-actions { margin-top: 16px; display: flex; gap: 10px; flex-wrap: wrap; }

        /* Queue Mode Styles */
        .nr-mode-toggle {
          display: flex;
          justify-content: center;
          margin-bottom: 16px;
        }
        .nr-mode-toggle .btn-group-toggle .btn {
          padding: 8px 20px;
          font-weight: 600;
        }
        .nr-queue-entry {
          margin-bottom: 16px;
          padding-bottom: 16px;
          border-bottom: 1px solid var(--md-border);
        }
        /* Prevent bouncing from terpbase conditional panel height changes */
        .nr-terpbase-conditional-wrapper {
          min-height: 70px;
        }
        .nr-queue-list {
          margin-top: 16px;
        }
        .nr-queue-item {
          background: var(--md-card-bg);
          border: 1px solid var(--md-card-border);
          border-radius: 8px;
          padding: 12px;
          margin-bottom: 10px;
          display: flex;
          align-items: center;
          gap: 12px;
        }
        .nr-queue-item.running {
          border-color: var(--status-warn-border);
          background: var(--status-warn-bg);
        }
        .nr-queue-item.done {
          border-color: var(--status-success-text);
          background: var(--status-success-bg);
        }
        .nr-queue-item.error {
          border-color: var(--status-error-text);
          background: var(--status-error-bg);
        }
        .nr-queue-item-num {
          font-weight: 800;
          font-size: 16px;
          min-width: 28px;
          text-align: center;
          color: var(--text-secondary);
        }
        .nr-queue-item-info {
          flex: 1;
          min-width: 0;
        }
        .nr-queue-item-name {
          font-weight: 700;
          font-size: 14px;
          margin-bottom: 4px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .nr-queue-item-files {
          font-size: 11px;
          color: var(--text-muted);
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .nr-queue-item-status {
          display: flex;
          align-items: center;
          gap: 8px;
          min-width: 100px;
        }
        .nr-queue-item-progress {
          flex: 0 0 80px;
        }
        .nr-queue-item-progress .nr-progress-bar {
          height: 6px;
        }
        .nr-queue-item-actions {
          display: flex;
          gap: 6px;
        }
        .nr-queue-actions-bar {
          display: flex;
          gap: 10px;
          margin-top: 16px;
          padding-top: 16px;
          border-top: 1px solid var(--md-border);
          flex-wrap: wrap;
        }
        .nr-queue-overall {
          margin-bottom: 12px;
          padding: 12px;
          background: var(--bg-card);
          border-radius: 8px;
        }
        .nr-queue-overall-text {
          font-weight: 700;
          font-size: 14px;
          margin-bottom: 8px;
        }
        .nr-queue-empty {
          text-align: center;
          padding: 40px 20px;
          color: var(--text-muted);
          font-style: italic;
        }

        /* Multi-dataset axis configuration */
        .nr-axis-config {
          background: var(--bg-card);
          border: 1px solid var(--md-border);
          border-radius: 12px;
          padding: 16px;
          margin-bottom: 16px;
        }

        .nr-axis-config-title {
          font-size: 15px;
          font-weight: 700;
          margin-bottom: 12px;
          color: var(--text-primary);
        }

        .nr-axis-row {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 16px;
          margin-bottom: 12px;
        }

        .nr-axis-panel {
          background: var(--bg-card);
          border: 1px solid var(--md-border);
          border-radius: 10px;
          padding: 14px;
        }

        .nr-axis-panel h6 {
          font-size: 13px;
          font-weight: 700;
          margin: 0 0 10px 0;
          color: var(--text-primary);
        }

        .nr-axis-panel .form-group {
          margin-bottom: 10px;
        }

        .nr-axis-panel label {
          font-size: 12px;
          font-weight: 600;
          color: var(--text-secondary);
        }

        .nr-multi-badge {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          background: var(--status-info-bg);
          border: 1px solid var(--status-info-text);
          border-radius: 6px;
          padding: 6px 10px;
          font-size: 12px;
          font-weight: 600;
          color: var(--status-info-text);
          margin-bottom: 12px;
        }

        @media (max-width: 600px) {
          .nr-axis-row {
            grid-template-columns: 1fr;
          }
        }
      "))
      ,
        tags$script(HTML("
        (function() {
          // Track scroll state per log element by a unique key
          var logScrollState = {};
          // Flag to prevent MutationObserver from firing during programmatic updates
          var isUpdating = false;

          function initNrLogAutoScroll() {
            var els = document.querySelectorAll('.nr-log');
            var foundNew = false;
            els.forEach(function(el) {
              if (el.dataset.nrAutoScrollInit === '1') return;
              foundNew = true;
              el.dataset.nrAutoScrollInit = '1';

              // Use a unique key based on parent structure
              var key = el.closest('.nr-running-panel') ? 'running' : 'gate';
              if (!logScrollState[key]) {
                logScrollState[key] = { paused: false };
              }
              var state = logScrollState[key];

              function isAtBottom() {
                return (el.scrollHeight - el.scrollTop - el.clientHeight) < 20;
              }
              function updatePaused() {
                state.paused = !isAtBottom();
                state.scrollTop = el.scrollTop;
              }

              el.addEventListener('scroll', function() {
                if (!isUpdating) {
                  updatePaused();
                }
              }, { passive: true });

              // Restore scroll position or scroll to bottom
              if (state.scrollTop !== undefined && state.paused) {
                el.scrollTop = state.scrollTop;
              } else {
                el.scrollTop = el.scrollHeight;
              }

              var obs = new MutationObserver(function() {
                // Skip if we're in the middle of a programmatic update
                if (isUpdating) return;
                if (!state.paused) {
                  el.scrollTop = el.scrollHeight;
                }
                state.scrollTop = el.scrollTop;
              });

              obs.observe(el, { childList: true, subtree: true, characterData: true });
            });
            // Fresh log elements found (DOM was recreated after in-app navigation)
            // Signal R to clear log caches so next poll sends full content
            if (foundNew && typeof Shiny !== 'undefined' && Shiny.setInputValue) {
              Shiny.setInputValue('nr_page_visible', Date.now(), {priority: 'event'});
            }
          }

          document.addEventListener('DOMContentLoaded', initNrLogAutoScroll);
          document.addEventListener('shiny:connected', initNrLogAutoScroll);
          // Check for new log elements periodically
          setInterval(initNrLogAutoScroll, 300);

          // Custom handler to update log content without replacing element
          Shiny.addCustomMessageHandler('nr_update_log', function(msg) {
            var el = document.getElementById(msg.id);
            if (!el) return;

            var logEl = el.querySelector('.nr-log') || el.closest('.nr-log') || el;
            if (!logEl) return;

            // Check if user has scrolled up (paused auto-scroll)
            var wasAtBottom = (logEl.scrollHeight - logEl.scrollTop - logEl.clientHeight) < 20;
            var oldScrollTop = logEl.scrollTop;
            var oldScrollLeft = logEl.scrollLeft;

            // Prevent MutationObserver from interfering during update
            isUpdating = true;
            var replace = (msg.replace === undefined) ? true : !!msg.replace;
            var html = msg.html || '';
            if (replace) {
              logEl.innerHTML = html;
            } else if (html) {
              if (logEl.innerHTML && logEl.innerHTML.trim() !== '') {
                logEl.insertAdjacentHTML('beforeend', '<br/>' + html);
              } else {
                logEl.innerHTML = html;
              }
            } else {
              isUpdating = false;
              return;
            }

            // Defer scroll restoration to after browser layout
            requestAnimationFrame(function() {
              if (wasAtBottom) {
                logEl.scrollTop = logEl.scrollHeight;
              } else {
                logEl.scrollTop = oldScrollTop;
              }
              // Always preserve horizontal scroll position
              logEl.scrollLeft = oldScrollLeft;
              isUpdating = false;
            });
          });

          // Handle page visibility changes - clear cache when page becomes visible
          // to force full log re-render (DOM elements are fresh after navigation)
          document.addEventListener('visibilitychange', function() {
            if (document.visibilityState === 'visible') {
              Shiny.setInputValue('nr_page_visible', Date.now(), {priority: 'event'});
            }
          });

          // Also detect when Shiny reconnects or tab navigation occurs
          $(document).on('shiny:connected shiny:sessioninitialized', function() {
            Shiny.setInputValue('nr_page_visible', Date.now(), {priority: 'event'});
          });
        })();
      "))
      ),

      div(
        class = "nr-main",
        uiOutput("nr_main_panel")
      )
    )
  )
}

# Helper to persist queue item files to durable temp location
nr_persist_queue_item_files <- function(item) {
  queue_files_dir <- file.path(tempdir(), "msterp_queue_files")
  dir.create(queue_files_dir, recursive = TRUE, showWarnings = FALSE)

  # Persist formatted data
  if (!is.null(item$formatted$path) && file.exists(item$formatted$path)) {
    new_path <- file.path(queue_files_dir, paste0(item$id, "_formatted.xlsx"))
    file.copy(item$formatted$path, new_path, overwrite = TRUE)
    item$formatted$path <- new_path
  }

  # Persist terpflow
  if (!is.null(item$terpflow$path) && file.exists(item$terpflow$path)) {
    new_path <- file.path(queue_files_dir, paste0(item$id, "_pipeline.terpflow"))
    file.copy(item$terpflow$path, new_path, overwrite = TRUE)
    item$terpflow$path <- new_path
  }

  # Persist terpbase (only if uploaded, not default)
  if (identical(item$terpbase$mode, "upload") &&
      !is.null(item$terpbase$path) && file.exists(item$terpbase$path)) {
    new_path <- file.path(queue_files_dir, paste0(item$id, "_terpbase.terpbase"))
    file.copy(item$terpbase$path, new_path, overwrite = TRUE)
    item$terpbase$path <- new_path
  }

  # Persist metabobase (only if uploaded, not default)
  if (identical(item$metabobase$mode, "upload") &&
      !is.null(item$metabobase$path) && file.exists(item$metabobase$path)) {
    new_path <- file.path(queue_files_dir, paste0(item$id, "_metabobase.metabobase"))
    file.copy(item$metabobase$path, new_path, overwrite = TRUE)
    item$metabobase$path <- new_path
  }

  item
}

page_new_run_server <- function(input, output, session, app_state = NULL, state = NULL) {

  rv <- reactiveValues(
    run_root = NULL,
    manifest_path = NULL,
    progress_path = NULL,
    log_path = NULL,
    last_progress = NULL,
    last_status = "idle",
    last_msg = "",
    last_err = NULL,
    ui_log = character(0),
    bg_process = NULL,  # Track background process
    run_start_time = NULL,  # Track when run started for elapsed time
    # Store original file names for display in log
    formatted_filename = NULL,
    terpflow_filename = NULL,
    terpbase_filename = NULL,
    metabobase_filename = NULL,
    # Queue mode state
    queue_items = list(),
    queue_running = FALSE,
    queue_current_index = 0,
    queue_bg_process = NULL,
    queue_results_dir = NULL,
    queue_start_time = NULL,
    run_log_cache = character(0),
    queue_log_cache = character(0),
    run_log_waiting = FALSE,
    queue_log_waiting = FALSE
  )

  # Multi-dataset axis configuration state
  nr_axis_config <- reactiveValues(
    x_dataset = "A",
    x_value_type = "log2fc",
    x_comparison = NULL,
    y_dataset = "B",
    y_value_type = "log2fc",
    y_comparison = NULL
  )

  set_busy <- function(active, message = "", percent = NULL) {
    if (exists("msterp_set_busy", mode = "function", inherits = TRUE)) {
      msterp_set_busy(session, active = active, message = message, percent = percent)
      try(session$flushReact(), silent = TRUE)
      try(shiny::flushReact(), silent = TRUE)
    }
  }

  reset_file_input <- function(id) {
    session$sendCustomMessage("msterp_reset_file_input", list(id = id))
  }

  # Clear log caches when DOM is recreated (in-app navigation or browser tab switch)
  # Forces the log observer to do a full replace on the next poll cycle
  observeEvent(input$nr_page_visible, {
    rv$run_log_cache <- character(0)
    rv$queue_log_cache <- character(0)
  }, ignoreInit = TRUE)

  # Helper to append log messages and update UI
  ui_log <- function(msg, level = "INFO") {
    # First line gets full date, subsequent lines get time only
    is_first <- length(rv$ui_log) == 0
    if (is_first) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    } else {
      timestamp <- format(Sys.time(), "%H:%M:%S")
    }
    line <- sprintf("[%s] %s", timestamp, msg)
    rv$ui_log <- c(rv$ui_log, line)
  }

  format_log_line <- function(line) {
    line <- gsub("&", "&amp;", line, fixed = TRUE)
    line <- gsub("<", "&lt;", line, fixed = TRUE)
    line <- gsub(">", "&gt;", line, fixed = TRUE)
    if (grepl("^\\[", line)) {
      line <- gsub("^(\\[[^\\]]+\\])", '<span style="color:var(--color-text-hint);">\\1</span>', line)
      line <- gsub('(<\\/span> )(\\[[^\\]]+\\])', '\\1<span style="color:var(--text-muted);">\\2</span>', line)
    }
    if (grepl("===", line)) {
      line <- gsub("(=== .+ ===)", '<span style="font-weight:700;color:#f0f0f0;">\\1</span>', line)
    }
    line
  }

  format_log_lines <- function(lines) {
    if (length(lines) == 0) return("")
    formatted_lines <- vapply(lines, format_log_line, character(1), USE.NAMES = FALSE)
    paste(formatted_lines, collapse = "<br/>")
  }

  log_delta <- function(lines, cache) {
    cache <- cache %||% character(0)
    if (length(lines) == 0) {
      return(list(mode = "replace", lines = character(0)))
    }
    if (length(cache) == 0) {
      return(list(mode = "replace", lines = lines))
    }
    if (length(lines) < length(cache)) {
      return(list(mode = "replace", lines = lines))
    }
    if (!identical(lines[seq_len(length(cache))], cache)) {
      return(list(mode = "replace", lines = lines))
    }
    if (length(lines) == length(cache)) {
      return(list(mode = "noop", lines = character(0)))
    }
    list(mode = "append", lines = lines[(length(cache) + 1):length(lines)])
  }

  # Helper to write progress.json (used by background process)
  write_progress <- function(progress_path, status, message, pct = NULL) {
    obj <- list(
      status = status,
      message = message,
      pct = pct,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    jsonlite::write_json(obj, progress_path, auto_unbox = TRUE, pretty = TRUE)
  }

  terpbase_path_rx <- reactive({
    if (identical(input$nr_terpbase_mode, "upload")) {
      nr_fileinput_path(input$nr_terpbase_upload)
    } else {
      input$nr_terpbase_default_path %||% ""
    }
  })

  metabobase_path_rx <- reactive({
    if (identical(input$nr_metabobase_mode, "upload")) {
      nr_fileinput_path(input$nr_metabobase_upload)
    } else {
      input$nr_metabobase_default_path %||% ""
    }
  })

  complexbase_path_rx <- reactive({
    mode <- input$nr_complexbase_mode %||% "default"
    if (identical(mode, "none")) {
      return("")
    } else if (identical(mode, "upload")) {
      nr_fileinput_path(input$nr_complexbase_upload)
    } else {
      input$nr_complexbase_default_path %||% ""
    }
  })

  formatted_path_rx <- reactive(nr_fileinput_path(input$nr_formatted_upload))
  terpflow_path_rx  <- reactive({
    nr_fileinput_path(input$nr_terpflow_upload)
  })

  # Capture original file names when files are uploaded
  observeEvent(input$nr_formatted_upload, {
    if (!is.null(input$nr_formatted_upload$name)) {
      rv$formatted_filename <- input$nr_formatted_upload$name
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nr_terpflow_upload, {
    if (!is.null(input$nr_terpflow_upload$name)) {
      rv$terpflow_filename <- input$nr_terpflow_upload$name
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nr_terpbase_upload, {
    if (!is.null(input$nr_terpbase_upload$name)) {
      rv$terpbase_filename <- input$nr_terpbase_upload$name
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nr_metabobase_upload, {
    if (!is.null(input$nr_metabobase_upload$name)) {
      rv$metabobase_filename <- input$nr_metabobase_upload$name
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nr_complexbase_upload, {
    if (!is.null(input$nr_complexbase_upload$name)) {
      rv$complexbase_filename <- input$nr_complexbase_upload$name
    }
  }, ignoreInit = TRUE)

  v_terpbase <- reactive(nr_ui_validate_terpbase(terpbase_path_rx()))
  v_metabobase <- reactive(nr_ui_validate_metabobase(metabobase_path_rx()))
  v_complexbase <- reactive(nr_ui_validate_complexbase(complexbase_path_rx()))
  v_formatted <- reactive(nr_ui_validate_formatted(formatted_path_rx()))
  v_terpflow <- reactive(nr_ui_validate_terpflow(terpflow_path_rx()))

  # Extract data type from formatted file
  formatted_data_type_rx <- reactive({
    fp <- formatted_path_rx()
    if (is.null(fp) || !nzchar(fp)) return(NULL)

    formatted <- tryCatch({
      if (exists("nr_load_formatted", mode = "function")) {
        nr_load_formatted(fp)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (is.null(formatted) || !isTRUE(formatted$ok)) return(NULL)

    # Check if data_type was inferred (look for warning in validation result)
    data_type <- formatted$metadata$data_type %||% NULL
    if (is.null(data_type)) return(NULL)

    # Detect if data_type was inferred from validation warnings
    inferred <- FALSE
    if (!is.null(formatted$validation) && !is.null(formatted$validation$warnings)) {
      inferred <- any(grepl("inferred.*from analysis_level", formatted$validation$warnings, ignore.case = TRUE))
    }

    list(value = data_type, inferred = inferred)
  })

  # Extract multi_subtype from formatted file (for multi-dataset mode)
  formatted_multi_subtype_rx <- reactive({
    fp <- formatted_path_rx()
    if (is.null(fp) || !nzchar(fp)) return(NULL)

    formatted <- tryCatch({
      if (exists("nr_load_formatted", mode = "function")) {
        nr_load_formatted(fp)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (is.null(formatted) || !isTRUE(formatted$ok)) return(NULL)
    formatted$metadata$multi_subtype %||% NULL
  })

  # Extract pipeline type from terpflow
  pipeline_type_rx <- reactive({
    tp <- terpflow_path_rx()
    if (is.null(tp) || !nzchar(tp)) return(NULL)

    terpflow <- tryCatch({
      if (exists("nr_load_terpflow", mode = "function")) {
        nr_load_terpflow(tp)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (is.null(terpflow) || !isTRUE(terpflow$ok)) return(NULL)
    terpflow$pipeline_type %||% "proteomics"
  })

  # Check for data type / pipeline type mismatch
  data_pipeline_mismatch_rx <- reactive({
    data_type_info <- formatted_data_type_rx()
    pipeline_type <- pipeline_type_rx()

    if (is.null(data_type_info) || is.null(pipeline_type)) return(NULL)

    # Extract value from list structure
    data_type <- if (is.list(data_type_info)) data_type_info$value else data_type_info
    if (is.null(data_type)) return(NULL)

    # Check for incompatibility
    is_mismatch <- FALSE
    if (data_type == "proteomics" && pipeline_type == "metabolomics") {
      is_mismatch <- TRUE
    } else if (data_type == "metabolomics" && pipeline_type == "proteomics") {
      is_mismatch <- TRUE
    }

    if (is_mismatch) {
      sprintf("Data type (%s) is incompatible with pipeline type (%s)", data_type, pipeline_type)
    } else {
      NULL
    }
  })

  all_ok <- reactive({
    # Always check formatted and terpflow
    ok <- isTRUE(v_formatted()$ok) && isTRUE(v_terpflow()$ok)

    # Check no mismatch
    ok <- ok && is.null(data_pipeline_mismatch_rx())

    # Conditionally check annotation databases based on pipeline type and multi_subtype
    pipeline_type <- pipeline_type_rx() %||% "proteomics"
    multi_subtype <- formatted_multi_subtype_rx()

    if (pipeline_type == "multi" && !is.null(multi_subtype)) {
      # For multi-dataset, check based on subtype
      if (multi_subtype == "protein-protein") {
        # Protein-protein only needs TerpBase
        ok <- ok && isTRUE(v_terpbase()$ok)
      } else {
        # metabolomics-metabolomics or protein-metabolomics need MetaboBase
        ok <- ok && isTRUE(v_metabobase()$ok)
      }
    } else if (pipeline_type == "proteomics") {
      ok <- ok && isTRUE(v_terpbase()$ok)
    } else if (pipeline_type == "metabolomics") {
      ok <- ok && isTRUE(v_metabobase()$ok)
    } else if (pipeline_type == "multi") {
      # Fallback for multi without subtype - require both
      ok <- ok && isTRUE(v_terpbase()$ok) && isTRUE(v_metabobase()$ok)
    }

    ok
  })

  nr_missing_items <- reactive({
    missing <- character(0)
    if (!isTRUE(v_terpbase()$ok)) missing <- c(missing, "Terpbase")
    if (!isTRUE(v_formatted()$ok)) missing <- c(missing, "Formatted data")
    if (!isTRUE(v_terpflow()$ok)) missing <- c(missing, "Pipeline")
    missing
  })

  light_ui <- function(v, label) {
    dot <- if (!isTRUE(v$ok)) "red" else "green"
    div(
      class = "nr-row",
      div(class = "nr-light", span(class = paste("nr-dot", dot)), label),
      div(class = "nr-msg", v$msg %||% "")
    )
  }

  output$nr_light_terpbase <- renderUI(light_ui(v_terpbase(), "Terpbase"))
  output$nr_light_metabobase <- renderUI(light_ui(v_metabobase(), "MetaboBase"))
  output$nr_light_complexbase <- renderUI(light_ui(v_complexbase(), "ComplexBase"))
  output$nr_light_formatted <- renderUI(light_ui(v_formatted(), "Formatted"))
  output$nr_light_terpflow <- renderUI(light_ui(v_terpflow(), "Pipeline"))

  # Data type badge
  output$nr_data_type_badge <- renderUI({
    data_type_info <- formatted_data_type_rx()
    if (is.null(data_type_info)) return(NULL)

    # Extract value and inferred flag
    if (is.list(data_type_info)) {
      data_type <- data_type_info$value
      inferred <- isTRUE(data_type_info$inferred)
    } else {
      data_type <- data_type_info
      inferred <- FALSE
    }

    if (is.null(data_type)) return(NULL)

    badge_color <- switch(data_type,
      "proteomics" = "#0066cc",
      "metabolomics" = "#1a8f3a",
      "multi" = "#7b2cbf",
      "#666"
    )

    badge_bg <- switch(data_type,
      "proteomics" = "#e7f3ff",
      "metabolomics" = "#e8f5e9",
      "multi" = "#f3e5ff",
      "#f0f0f0"
    )

    badge_text <- toupper(data_type)

    # Add "(auto-detected)" suffix if inferred
    if (inferred) {
      badge_text <- paste0(badge_text, " (auto-detected)")
    }

    # Use dashed border if inferred
    border_style <- if (inferred) "dashed" else "solid"

    div(
      style = sprintf(
        "display: inline-block; padding: 4px 10px; border-radius: 4px; font-size: 11px; font-weight: 700; color: %s; background: %s; border: 1px %s %s; margin-top: 6px;",
        badge_color, badge_bg, border_style, badge_color
      ),
      badge_text
    )
  })

  # Mismatch warning
  output$nr_mismatch_warning <- renderUI({
    msg <- data_pipeline_mismatch_rx()
    if (is.null(msg)) return(NULL)

    div(
      style = "color: var(--status-error-text); font-weight: 700; font-size: 13px; margin-top: 8px; padding: 8px; background: var(--notification-error-bg); border: 1px solid var(--status-error-text); border-radius: 6px;",
      icon("exclamation-triangle"),
      " ",
      msg
    )
  })

  # Conditional annotation database inputs
  output$nr_annotation_databases_ui <- renderUI({
    pipeline_type <- pipeline_type_rx()
    if (is.null(pipeline_type)) return(NULL)

    multi_subtype <- formatted_multi_subtype_rx()

    # Determine which databases to show based on pipeline type and multi_subtype
    show_terpbase <- FALSE
    show_metabobase <- FALSE

    if (pipeline_type == "multi" && !is.null(multi_subtype)) {
      # For multi-dataset, show based on subtype
      show_terpbase <- multi_subtype == "protein-protein"
      show_metabobase <- multi_subtype %in% c("metabolomics-metabolomics", "protein-metabolomics")
    } else if (pipeline_type == "multi") {
      # Fallback for multi without subtype - show both
      show_terpbase <- TRUE
      show_metabobase <- TRUE
    } else {
      show_terpbase <- pipeline_type == "proteomics"
      show_metabobase <- pipeline_type == "metabolomics"
    }

    sections <- tagList()

    # Show TerpBase if needed
    if (show_terpbase) {
      terpbase_choices <- tools_default_terpbase_choices()
      default_terpbase <- if (length(terpbase_choices) > 0 && nzchar(terpbase_choices[1])) terpbase_choices[1] else ""
      sections <- tagList(
        sections,
        div(class = "nr-section",
            strong("TerpBase (proteomics annotation)"),
            radioButtons(
              "nr_terpbase_mode",
              label = NULL,
              choices = c("Use default" = "default", "Upload" = "upload"),
              selected = "default",
              inline = TRUE
            ),
            div(class = "nr-terpbase-conditional-wrapper",
              conditionalPanel(
                "input.nr_terpbase_mode == 'default'",
                radioButtons(
                  "nr_terpbase_default_path",
                  label = NULL,
                  choices = terpbase_choices,
                  selected = default_terpbase,
                  inline = TRUE
                )
              ),
              conditionalPanel(
                "input.nr_terpbase_mode == 'upload'",
                fileInput("nr_terpbase_upload", label = NULL, accept = c(".terpbase", ".rds"))
              )
            ),
            uiOutput("nr_light_terpbase")
        )
      )

      # Also show ComplexBase for proteomics
      complexbase_choices <- tools_default_complexbase_choices()
      default_complexbase <- if (length(complexbase_choices) > 0 && nzchar(complexbase_choices[1])) complexbase_choices[1] else ""
      sections <- tagList(
        sections,
        div(class = "nr-section",
            strong("ComplexBase (protein complex annotation)"),
            radioButtons(
              "nr_complexbase_mode",
              label = NULL,
              choices = c("Use default" = "default", "Upload" = "upload", "None" = "none"),
              selected = "default",
              inline = TRUE
            ),
            div(class = "nr-terpbase-conditional-wrapper",
              conditionalPanel(
                "input.nr_complexbase_mode == 'default'",
                selectInput(
                  "nr_complexbase_default_path",
                  label = NULL,
                  choices = complexbase_choices,
                  selected = default_complexbase
                )
              ),
              conditionalPanel(
                "input.nr_complexbase_mode == 'upload'",
                fileInput("nr_complexbase_upload", label = NULL, accept = c(".complexbase", ".rds"))
              )
            ),
            uiOutput("nr_light_complexbase")
        )
      )
    }

    # Show MetaboBase if needed
    if (show_metabobase) {
      metabobase_choices <- tools_default_metabobase_choices()
      default_mb <- if (length(metabobase_choices) > 0 && nzchar(metabobase_choices[1])) metabobase_choices[1] else ""
      sections <- tagList(
        sections,
        div(class = "nr-section",
            strong("MetaboBase (metabolomics annotation)"),
            radioButtons(
              "nr_metabobase_mode",
              label = NULL,
              choices = c("Use default" = "default", "Upload" = "upload"),
              selected = "default",
              inline = TRUE
            ),
            div(class = "nr-terpbase-conditional-wrapper",  # Reuse same wrapper style
              conditionalPanel(
                "input.nr_metabobase_mode == 'default'",
                radioButtons(
                  "nr_metabobase_default_path",
                  label = NULL,
                  choices = metabobase_choices,
                  selected = default_mb,
                  inline = TRUE
                )
              ),
              conditionalPanel(
                "input.nr_metabobase_mode == 'upload'",
                fileInput("nr_metabobase_upload", label = NULL, accept = c(".metabobase", ".rds"))
              )
            ),
            uiOutput("nr_light_metabobase")
        )
      )
    }

    sections
  })

  # Shared input form UI (used in both single and queue modes)
  nr_input_form_ui <- function(for_queue = FALSE) {
    tagList(
      div(class = "nr-section",
          strong("1) Formatted data (.xlsx)"),
          fileInput("nr_formatted_upload", label = NULL, accept = c(".xlsx", ".xls")),
          uiOutput("nr_light_formatted"),
          uiOutput("nr_data_type_badge")
      ),

      div(class = "nr-section",
          strong("2) Pipeline (.terpflow)"),
          fileInput("nr_terpflow_upload", label = NULL, accept = c(".terpflow", ".rds")),
          uiOutput("nr_light_terpflow")
      ),

      # Mismatch warning (shown if data type conflicts with pipeline type)
      uiOutput("nr_mismatch_warning"),

      # Annotation databases (conditional on pipeline type)
      uiOutput("nr_annotation_databases_ui"),

      div(class = "nr-section",
          strong("Run Name"),
          textInput("nr_run_name", label = NULL, placeholder = "e.g., my_analysis", width = "70%")
      )
    )
  }

  #' Render axis configuration UI for multi-dataset mode
  #' @param ns Shiny namespace function
  #' @param multi_session Multi-dataset session object
  #' @param axis_state Reactive values for axis configuration
  nr_render_axis_config_ui <- function(ns, multi_session, axis_state) {
    if (is.null(multi_session) || multi_session$mode != "multi_dataset") {
      return(NULL)
    }

    ds_a <- multi_session$dataset_a
    ds_b <- multi_session$dataset_b

    # Get value type choices
    value_types <- nr_get_value_types()
    value_labels <- nr_get_value_type_labels()

    # Build comparison choices for each dataset
    comparisons_a <- nr_build_comparison_choices(ds_a$groups)
    comparisons_b <- nr_build_comparison_choices(ds_b$groups)

    tags$div(
      class = "nr-axis-config",
      tags$div(class = "nr-axis-config-title", "2D Analysis Configuration"),

      # Multi-dataset badge
      tags$div(
        class = "nr-multi-badge",
        icon("link"),
        sprintf("Linked: %s / %s (%d proteins)",
                ds_a$label, ds_b$label, length(multi_session$aligned_proteins))
      ),

      # Two-column axis configuration
      tags$div(
        class = "nr-axis-row",

        # X-axis panel
        tags$div(
          class = "nr-axis-panel",
          tags$h6(sprintf("X-Axis (%s)", ds_a$label)),

          selectInput(
            ns("nr_axis_x_value_type"), "Value type",
            choices = stats::setNames(value_types, value_labels[value_types]),
            selected = isolate(axis_state$x_value_type),
            width = "100%"
          ),

          # Comparison dropdown (shown for FC/pval types)
          conditionalPanel(
            condition = "input.nr_axis_x_value_type == 'log2fc' || input.nr_axis_x_value_type == 'neglog10p'",
            ns = ns,
            selectInput(
              ns("nr_axis_x_comparison"), "Comparison",
              choices = if (length(comparisons_a) > 0) comparisons_a else c("(no groups)" = ""),
              selected = isolate(axis_state$x_comparison) %||% (if (length(comparisons_a) > 0) comparisons_a[1] else ""),
              width = "100%"
            )
          )
        ),

        # Y-axis panel
        tags$div(
          class = "nr-axis-panel",
          tags$h6(sprintf("Y-Axis (%s)", ds_b$label)),

          selectInput(
            ns("nr_axis_y_value_type"), "Value type",
            choices = stats::setNames(value_types, value_labels[value_types]),
            selected = isolate(axis_state$y_value_type),
            width = "100%"
          ),

          # Comparison dropdown (shown for FC/pval types)
          conditionalPanel(
            condition = "input.nr_axis_y_value_type == 'log2fc' || input.nr_axis_y_value_type == 'neglog10p'",
            ns = ns,
            selectInput(
              ns("nr_axis_y_comparison"), "Comparison",
              choices = if (length(comparisons_b) > 0) comparisons_b else c("(no groups)" = ""),
              selected = isolate(axis_state$y_comparison) %||% (if (length(comparisons_b) > 0) comparisons_b[1] else ""),
              width = "100%"
            )
          )
        )
      )
    )
  }

  # Multi-dataset axis configuration (shown if multi_dataset_session exists)
  output$nr_axis_config_ui <- renderUI({
    multi_session <- app_state$multi_dataset_session
    if (is.null(multi_session)) return(NULL)

    nr_render_axis_config_ui(session$ns, multi_session, nr_axis_config)
  })

  nr_gate_panel_ui <- function() {
    mode <- input$nr_run_mode %||% "single"

    div(
      class = "msterp-gate",
      div(
        class = "msterp-gate-card",
        div(
          class = "msterp-gate-body",
          # Mode toggle
          div(class = "nr-mode-toggle",
              radioButtons(
                "nr_run_mode",
                label = NULL,
                choices = c("Single Run" = "single", "Queue Mode" = "queue"),
                selected = mode,
                inline = TRUE
              )
          ),

          # Single Run Mode
          if (identical(mode, "single")) {
            tagList(
              div(class = "msterp-gate-title", "New Run"),
              nr_input_form_ui(for_queue = FALSE),
              # Multi-dataset axis configuration (conditionally rendered)
              uiOutput("nr_axis_config_ui"),
              div(class = "nr-section", style = "text-align: center;",
                  uiOutput("nr_run_btn"),
                  uiOutput("nr_run_status_ui")
              )
            )
          } else {
            # Queue Mode
            tagList(
              div(class = "msterp-gate-title", "Queue Mode"),
              div(class = "nr-queue-entry",
                  nr_input_form_ui(for_queue = TRUE),
                  div(class = "nr-section", style = "text-align: center;",
                      uiOutput("nr_queue_add_btn")
                  )
              ),
              # Queue items list
              div(class = "nr-queue-list",
                  uiOutput("nr_queue_items_ui")
              ),
              # Queue actions bar
              uiOutput("nr_queue_actions_ui")
            )
          }
        )
      )
    )
  }

  output$nr_run_btn <- renderUI({
    if (isTRUE(all_ok())) {
      actionButton("nr_run", "Run Pipeline", class = "btn btn-primary btn-lg")
    } else {
      tags$button(
        type = "button",
        class = "btn btn-primary btn-lg",
        disabled = "disabled",
        "Run Pipeline"
      )
    }
  })

  # =========================================================
  # Axis Configuration Input Observers
  # =========================================================

  # Axis configuration input observers
  observeEvent(input$nr_axis_x_value_type, {
    nr_axis_config$x_value_type <- input$nr_axis_x_value_type
  }, ignoreInit = TRUE)

  observeEvent(input$nr_axis_x_comparison, {
    nr_axis_config$x_comparison <- input$nr_axis_x_comparison
  }, ignoreInit = TRUE)

  observeEvent(input$nr_axis_y_value_type, {
    nr_axis_config$y_value_type <- input$nr_axis_y_value_type
  }, ignoreInit = TRUE)

  observeEvent(input$nr_axis_y_comparison, {
    nr_axis_config$y_comparison <- input$nr_axis_y_comparison
  }, ignoreInit = TRUE)

  #' Build axis configuration object from reactive state
  nr_get_axis_config <- reactive({
    multi_session <- app_state$multi_dataset_session
    if (is.null(multi_session)) return(NULL)

    list(
      x = list(
        dataset = "A",  # X-axis always uses Dataset A
        value_type = nr_axis_config$x_value_type %||% "log2fc",
        comparison = nr_axis_config$x_comparison
      ),
      y = list(
        dataset = "B",  # Y-axis always uses Dataset B
        value_type = nr_axis_config$y_value_type %||% "log2fc",
        comparison = nr_axis_config$y_comparison
      )
    )
  })

  nr_running_panel_ui <- function() {
    pill_class <- if (rv$last_status == "running") "nr-status-pill running" else "nr-status-pill"

    # Pre-render log content for completed runs to avoid empty log box
    log_html <- NULL
    if (rv$last_status %in% c("done", "error") && !is.null(rv$run_root) && !is.null(rv$log_path) && file.exists(rv$log_path)) {
      ui_lines <- rv$ui_log
      file_lines <- tryCatch(
        readLines(rv$log_path, warn = FALSE, encoding = "UTF-8"),
        error = function(e) character(0)
      )
      all_lines <- c(ui_lines, file_lines)
      if (length(all_lines) > 0) {
        all_lines <- utils::tail(all_lines, 200)
        format_log_line <- function(line) {
          line <- gsub("&", "&amp;", line, fixed = TRUE)
          line <- gsub("<", "&lt;", line, fixed = TRUE)
          line <- gsub(">", "&gt;", line, fixed = TRUE)
          if (grepl("^\\[", line)) {
            line <- gsub("^(\\[[^\\]]+\\])", '<span style="color:var(--color-text-hint);">\\1</span>', line)
            line <- gsub('(<\\/span> )(\\[[^\\]]+\\])', '\\1<span style="color:var(--text-muted);">\\2</span>', line)
          }
          if (grepl("===", line)) {
            line <- gsub("(=== .+ ===)", '<span style="font-weight:700;color:#f0f0f0;">\\1</span>', line)
          }
          line
        }
        formatted_lines <- vapply(all_lines, format_log_line, character(1), USE.NAMES = FALSE)
        log_html <- paste(formatted_lines, collapse = "<br/>")
      }
    }

    div(
      class = "nr-running-panel",
      div(class = "nr-row",
          div(class = pill_class, textOutput("nr_status_text")),
          div(class = "nr-elapsed", uiOutput("nr_elapsed_time")),
          div(class = "nr-msg", textOutput("nr_status_msg"))
      ),
      div(class = "nr-progress-wrap",
          div(class = "nr-progress-bar", uiOutput("nr_progress_fill"))
      ),
      # Stop button - only show while running
      if (rv$last_status == "running") {
        div(class = "nr-actions", style = "margin-top: 10px;",
            actionButton("nr_stop_run", "Stop Run", class = "btn btn-danger btn-sm")
        )
      },
      tags$hr(),
      div(strong(""), tags$br(),
          div(id = "nr_single_log_container", class = "nr-log",
              if (!is.null(log_html)) HTML(log_html) else NULL
          )
      ),

      # Actions section - show after run completes
      if (rv$last_status %in% c("done", "error")) {
        div(class = "nr-actions",
            uiOutput("nr_download_ui"),
            actionButton("nr_new_run", "Start New Run", class = "btn btn-default")
        )
      }
    )
  }

  # Queue running panel UI (shows during queue execution)
  # Static shell — dynamic parts use uiOutput() placeholders to avoid full re-renders
  nr_queue_running_panel_ui <- function() {
    div(
      class = "nr-running-panel",
      # Overall progress (updated by separate renderUI)
      uiOutput("nr_queue_overall_progress"),
      # Current item progress (updated by separate renderUI)
      uiOutput("nr_queue_current_item"),
      # Stop button
      div(class = "nr-actions", style = "margin-top: 10px;",
          actionButton("nr_queue_stop", "Stop Queue", class = "btn btn-danger btn-sm")
      ),
      tags$hr(),
      # Log output - use ID for custom message updates to preserve scroll
      div(strong("Queue Log"), tags$br(), div(id = "nr_queue_log_container", class = "nr-log"))
    )
  }

  output$nr_main_panel <- renderUI({
    # Check for queue running state first
    if (rv$queue_running) {
      nr_queue_running_panel_ui()
    } else if (rv$last_status %in% c("running", "done", "error")) {
      nr_running_panel_ui()
    } else {
      nr_gate_panel_ui()
    }
  })

  # Queue elapsed time display
  output$nr_queue_elapsed_time <- renderUI({
    if (!rv$queue_running) return(NULL)
    invalidateLater(1000, session)

    if (is.null(rv$queue_start_time)) return(NULL)

    elapsed <- as.numeric(difftime(Sys.time(), rv$queue_start_time, units = "secs"))
    mins <- floor(elapsed / 60)
    secs <- floor(elapsed %% 60)
    sprintf("%d:%02d", mins, secs)
  })

  # Queue overall progress (polled independently to avoid rebuilding the whole panel)
  output$nr_queue_overall_progress <- renderUI({
    if (!rv$queue_running) return(NULL)
    invalidateLater(500, session)

    n_items <- length(rv$queue_items)
    n_done <- sum(vapply(rv$queue_items, function(x) x$status %in% c("done", "error"), logical(1)))
    overall_pct <- if (n_items > 0) round((n_done / n_items) * 100) else 0

    div(class = "nr-queue-overall",
        div(class = "nr-queue-overall-text",
            sprintf("Queue Progress: %d of %d items (%d%%)", n_done, n_items, overall_pct)
        ),
        div(class = "nr-progress-bar",
            div(class = "nr-progress-fill", style = paste0("width:", overall_pct, "%;"))
        )
    )
  })

  # Queue current item progress (polled independently)
  output$nr_queue_current_item <- renderUI({
    if (!rv$queue_running) return(NULL)
    invalidateLater(500, session)

    current_idx <- rv$queue_current_index
    n_items <- length(rv$queue_items)
    current_item <- if (current_idx >= 1 && current_idx <= n_items) rv$queue_items[[current_idx]] else NULL

    if (is.null(current_item) || !identical(current_item$status, "running")) return(NULL)

    current_pct <- current_item$progress$pct %||% 0
    current_msg <- current_item$progress$msg %||% ""
    current_name <- current_item$name

    div(style = "margin-bottom: 12px;",
        div(class = "nr-row",
            div(class = "nr-status-pill running", sprintf("Running: %s", current_name)),
            div(class = "nr-elapsed", uiOutput("nr_queue_elapsed_time")),
            div(class = "nr-msg", current_msg)
        ),
        div(class = "nr-progress-wrap",
            div(class = "nr-progress-bar",
                div(class = "nr-progress-fill active", style = paste0("width:", current_pct, "%;"))
            )
        )
    )
  })

  # Queue log output - use observer with custom message to preserve scroll position
  observe({
    if (!rv$queue_running) return()
    invalidateLater(500, session)

    # Combine UI log with current item's log
    ui_lines <- rv$ui_log
    file_lines <- character(0)

    # Get current item's log if running
    idx <- rv$queue_current_index
    if (idx >= 1 && idx <= length(rv$queue_items)) {
      item <- rv$queue_items[[idx]]
      log_path <- item$log_path
      if (!is.null(log_path) && file.exists(log_path)) {
        file_lines <- tryCatch(
          readLines(log_path, warn = FALSE, encoding = "UTF-8"),
          error = function(e) character(0)
        )
      }
    }

    all_lines <- c(ui_lines, file_lines)
    if (length(all_lines) == 0) {
      if (!isTRUE(rv$queue_log_waiting)) {
        rv$queue_log_cache <- character(0)
        rv$queue_log_waiting <- TRUE
        session$sendCustomMessage("nr_update_log", list(
          id = "nr_queue_log_container",
          html = '<span style="color: var(--text-secondary);">(waiting for run...)</span>',
          replace = TRUE
        ))
      }
      return()
    }

    rv$queue_log_waiting <- FALSE
    all_lines <- utils::tail(all_lines, 200)
    delta <- log_delta(all_lines, rv$queue_log_cache)
    if (identical(delta$mode, "noop")) return()

    html_chunk <- format_log_lines(delta$lines)
    session$sendCustomMessage("nr_update_log", list(
      id = "nr_queue_log_container",
      html = html_chunk,
      replace = identical(delta$mode, "replace")
    ))

    rv$queue_log_cache <- all_lines
  })

  output$nr_run_status_ui <- renderUI({
    if (!isTRUE(all_ok())) {
      return(div(class = "nr-msg", style = "color: var(--status-error-text); margin-top: 6px;",
                 "Needs valid inputs above"))
    }
    # Show if a run is in progress
    if (identical(rv$last_status, "running")) {
      return(div(class = "nr-msg", style = "color: var(--status-success-text); margin-top: 6px;",
                 "Pipeline running in background..."))
    }
    NULL
  })

  observeEvent(input$nr_run, {
    # Clear previous log
    rv$ui_log <- character(0)
    rv$run_log_cache <- character(0)
    rv$run_log_waiting <- FALSE
    session$sendCustomMessage("nr_update_log", list(
      id = "nr_single_log_container",
      html = "",
      replace = TRUE
    ))

    ui_log("Initiating run...")

    # Prevent multiple concurrent runs
    if (identical(rv$last_status, "running")) {
      ui_log("A run is already in progress", "WARN")
      return()
    }

    ui_log("Validating inputs...")

    if (!isTRUE(all_ok())) {
      ui_log("Validation failed", "ERROR")
      if (!isTRUE(v_terpbase()$ok)) ui_log("  - Terpbase: invalid or missing", "ERROR")
      if (!isTRUE(v_formatted()$ok)) ui_log("  - Formatted data: invalid or missing", "ERROR")
      if (!isTRUE(v_terpflow()$ok)) ui_log("  - Pipeline: invalid or missing", "ERROR")
      return()
    }

    ui_log("Validation passed")

    # Check if callr is available for async execution
    has_callr <- requireNamespace("callr", quietly = TRUE)
    if (!has_callr) {
      ui_log("Note: callr not available, running synchronously", "WARN")
    }

    rv$last_err <- NULL
    rv$last_status <- "running"
    rv$last_msg <- "Initiating run..."
    rv$run_start_time <- Sys.time()

    # Use temp directory for output (works on both local and server deployments)
    out_parent <- tempdir()
    dir.create(out_parent, recursive = TRUE, showWarnings = FALSE)

    # Get input paths (normalize to absolute paths for subprocess)
    tb <- terpbase_path_rx()
    mb <- metabobase_path_rx()
    cb <- complexbase_path_rx()
    ff <- formatted_path_rx()
    tf <- terpflow_path_rx()

    tb_norm <- if (!is.null(tb) && nzchar(tb)) nr_norm(tb) else NULL
    mb_norm <- if (!is.null(mb) && nzchar(mb)) nr_norm(mb) else NULL
    cb_norm <- if (!is.null(cb) && nzchar(cb)) nr_norm(cb) else NULL
    ff_norm <- nr_norm(ff)
    tf_norm <- nr_norm(tf)
    out_norm <- nr_norm(out_parent)

    # Create a unique run directory with progress.json upfront
    # Use user-provided run name if given, otherwise auto-generate
    user_run_name <- trimws(input$nr_run_name %||% "")
    if (nzchar(user_run_name)) {
      run_name <- paste0(nr_sanitize(user_run_name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    } else {
      run_name <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    run_root <- file.path(out_norm, paste0(run_name, ".terpbook"))
    dir.create(run_root, recursive = TRUE, showWarnings = FALSE)

    # Set up paths for polling
    rv$run_root <- run_root
    rv$progress_path <- file.path(run_root, "progress.json")
    rv$log_path <- file.path(run_root, "log.txt")
    rv$manifest_path <- file.path(run_root, "manifest.json")

    # Write initial progress
    write_progress(rv$progress_path, "running", "Initializing...", 5)

    ui_log("Loading input files...")
    # Use original file names if available, otherwise fall back to temp path basename
    formatted_display <- rv$formatted_filename %||% basename(ff_norm)
    terpflow_display <- rv$terpflow_filename %||% basename(tf_norm)
    terpbase_display <- rv$terpbase_filename %||% (if (!is.null(tb_norm)) basename(tb_norm) else NULL)

    ui_log(sprintf("  Formatted: %s", formatted_display))
    ui_log(sprintf("  Pipeline:  %s", terpflow_display))
    if (!is.null(tb_norm)) {
      # For default terpbase path, show the actual path
      if (identical(input$nr_terpbase_mode, "default")) {
        ui_log(sprintf("  Terpbase:  %s", basename(tb_norm)))
      } else {
        ui_log(sprintf("  Terpbase:  %s", terpbase_display %||% basename(tb_norm)))
      }
    }
    ui_log(sprintf("  Output:    %s", out_norm))

    # Check for multi-dataset mode and capture axis configuration
    multi_session <- app_state$multi_dataset_session
    axis_config <- NULL
    if (!is.null(multi_session) && multi_session$mode == "multi_dataset") {
      ui_log("Multi-dataset mode detected")
      axis_config <- nr_get_axis_config()
      ui_log(sprintf("  X-axis: %s (%s)", axis_config$x$value_type,
                     axis_config$x$comparison %||% "N/A"))
      ui_log(sprintf("  Y-axis: %s (%s)", axis_config$y$value_type,
                     axis_config$y$comparison %||% "N/A"))
    }

    # Get app root directory for sourcing in subprocess
    app_root <- getwd()

    if (has_callr) {
      # =========================================================
      # ASYNC EXECUTION via callr::r_bg()
      # =========================================================
      ui_log("Launching background process...")

      # The runner function executes in a separate R process
      # It sources the app files and runs nr_execute_run with a file-based progress callback
      runner_fn <- function(app_root, formatted_path, terpflow_path, terpbase_path,
                            metabobase_path = NULL, complexbase_path = NULL,
                            out_dir, run_name, progress_path, log_path,
                            multi_session = NULL, axis_config = NULL) {
        # Set working directory
        setwd(app_root)

        # Helper to write progress
        write_prog <- function(status, message, pct = NULL) {
          obj <- list(
            status = status,
            message = message,
            pct = pct,
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )
          jsonlite::write_json(obj, progress_path, auto_unbox = TRUE, pretty = TRUE)
        }

        # Helper to append to log
        write_log <- function(msg, level = "INFO") {
          # First line gets full date, subsequent lines get time only
          is_first <- !file.exists(log_path) || file.info(log_path)$size == 0
          if (is_first) {
            time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          } else {
            time_str <- format(Sys.time(), "%H:%M:%S")
          }
          line <- sprintf("[%s] %s\n", time_str, msg)
          cat(line, file = log_path, append = TRUE)
        }

        write_prog("running", "Loading engine code...", 8)
        write_log("Loading engine code...")

        # Source all required files
        tryCatch({
          source(file.path(app_root, "R", "00_init.R"), local = FALSE)

          # Source engines directory
          engine_files <- list.files(file.path(app_root, "R", "engines"),
                                      pattern = "\\.R$", full.names = TRUE)
          for (f in engine_files) source(f, local = FALSE)

          # Source stats engines
          stats_files <- list.files(file.path(app_root, "R", "engines", "stats"),
                                     pattern = "\\.R$", full.names = TRUE)
          for (f in stats_files) source(f, local = FALSE)

          # Source utils
          utils_files <- list.files(file.path(app_root, "R", "utils"),
                                     pattern = "\\.R$", full.names = TRUE)
          for (f in utils_files) source(f, local = FALSE)

          write_log("Engine code loaded")
        }, error = function(e) {
          write_prog("error", paste("Failed to load engine code:", conditionMessage(e)), 0)
          write_log(paste("ERROR loading code:", conditionMessage(e)), "ERROR")
          stop(e)
        })

        # Create progress callback that writes to file
        # This callback receives messages from nr_execute_run
        progress_callback <- function(status, msg, pct) {
          write_prog("running", msg, pct)
          write_log(msg)
        }

        # Run the pipeline
        result <- tryCatch({
          nr_execute_run(
            formatted_path = formatted_path,
            terpflow_path = terpflow_path,
            terpbase_path = terpbase_path,
            metabobase_path = metabobase_path,
            out_dir = out_dir,
            run_name = run_name,
            progress_callback = progress_callback,
            complexbase_path = complexbase_path,
            multi_session = multi_session,
            axis_config = axis_config
          )
        }, error = function(e) {
          write_prog("error", paste("Pipeline failed:", conditionMessage(e)), 0)
          write_log(paste("ERROR:", conditionMessage(e)), "ERROR")
          list(ok = FALSE, error = conditionMessage(e))
        })

        if (isTRUE(result$ok)) {
          write_prog("done", "Pipeline complete!", 100)
          write_log("Pipeline completed successfully")
        } else {
          write_prog("error", result$error %||% "Unknown error", 0)
          write_log(paste("Pipeline failed:", result$error %||% "Unknown error"), "ERROR")
        }

        result
      }

      # Launch background process
      rv$bg_process <- callr::r_bg(
        func = runner_fn,
        args = list(
          app_root = app_root,
          formatted_path = ff_norm,
          terpflow_path = tf_norm,
          terpbase_path = tb_norm,
          metabobase_path = mb_norm,
          complexbase_path = cb_norm,
          out_dir = out_norm,
          run_name = run_name,
          progress_path = rv$progress_path,
          log_path = rv$log_path,
          multi_session = multi_session,
          axis_config = axis_config
        ),
        package = TRUE,
        supervise = TRUE
      )

      ui_log("Background process launched")
      # Return immediately - polling will handle updates

    } else {
      # =========================================================
      # SYNCHRONOUS FALLBACK (no callr)
      # =========================================================
      ui_log("Running synchronously...")
      set_busy(TRUE, "Running pipeline...", 5)
      on.exit(set_busy(FALSE, "", NULL), add = TRUE)

      # Progress callback writes to file for consistency
      progress_callback <- function(status, msg, pct) {
        write_progress(rv$progress_path, status, msg, pct)
        set_busy(TRUE, msg, pct)
        ui_log(sprintf("[%s] %s", status, msg))
      }

      res <- tryCatch({
        nr_execute_run(
          formatted_path = ff_norm,
          terpflow_path = tf_norm,
          terpbase_path = tb_norm,
          metabobase_path = mb_norm,
          out_dir = out_norm,
          run_name = run_name,
          progress_callback = progress_callback,
          complexbase_path = cb_norm,
          multi_session = multi_session,
          axis_config = axis_config
        )
      }, error = function(e) {
        list(ok = FALSE, error = conditionMessage(e))
      })

      if (inherits(res, "error") || !isTRUE(res$ok)) {
        err_msg <- if (inherits(res, "error")) conditionMessage(res) else res$error %||% "Unknown error"
        ui_log(sprintf("Run failed: %s", err_msg), "ERROR")
        rv$last_status <- "error"
        rv$last_err <- err_msg
        rv$last_msg <- "Run failed."
        write_progress(rv$progress_path, "error", err_msg, 0)
        return()
      }

      rv$run_root <- nr_norm(res$run_root %||% run_root)
      rv$manifest_path <- nr_norm(res$manifest_path %||% file.path(rv$run_root, "manifest.json"))
      rv$log_path <- nr_norm(res$log_path %||% file.path(rv$run_root, "log.txt"))

      rv$last_status <- "done"
      rv$last_msg <- "Run complete!"
      ui_log(sprintf("Run complete! Output: %s", rv$run_root))
    }
  }, ignoreInit = TRUE)

  # =========================================================
  # Polling observer for background process completion
  # =========================================================
  observe({
    # Only poll if we have a running background process
    if (is.null(rv$bg_process) || !identical(rv$last_status, "running")) {
      return()
    }

    # Check process status
    if (!rv$bg_process$is_alive()) {
      # Process finished - get result
      result <- tryCatch({
        rv$bg_process$get_result()
      }, error = function(e) {
        list(ok = FALSE, error = conditionMessage(e))
      })

      if (isTRUE(result$ok)) {
        rv$last_status <- "done"
        rv$last_msg <- "Run complete!"
        ui_log(sprintf("Background run complete! Output: %s", rv$run_root))

        # Update paths from result if available
        if (!is.null(result$run_root)) {
          rv$run_root <- nr_norm(result$run_root)
          rv$manifest_path <- file.path(rv$run_root, "manifest.json")
          rv$log_path <- file.path(rv$run_root, "log.txt")
        }
      } else {
        rv$last_status <- "error"
        rv$last_err <- result$error %||% "Unknown error"
        rv$last_msg <- "Run failed."
        ui_log(sprintf("Background run failed: %s", rv$last_err), "ERROR")
      }

      rv$bg_process <- NULL
    } else {
      # Still running - poll again
      invalidateLater(500, session)
    }
  })
  
  # Poll progress + status
  output$nr_progress_json <- renderText({
    if (is.null(rv$run_root)) return("")
    if (!file.exists(rv$progress_path %||% "")) {
      invalidateLater(400, session)
      return("{ \"status\": \"starting\" }")
    }
    
    invalidateLater(400, session)
    txt <- paste(readLines(rv$progress_path, warn = FALSE), collapse = "\n")
    if (nzchar(txt)) txt else "{ }"
  })
  
  progress_obj_rx <- reactive({
    # Poll while running
    if (identical(rv$last_status, "running")) {
      invalidateLater(500, session)
    }

    if (is.null(rv$run_root) || is.null(rv$progress_path) || !file.exists(rv$progress_path)) return(NULL)
    if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
    tryCatch(jsonlite::read_json(rv$progress_path, simplifyVector = TRUE), error = function(e) NULL)
  })
  
  output$nr_status_text <- renderText({
    # First check rv$last_status for immediate feedback
    status <- rv$last_status %||% "idle"

    # Override with progress.json status if available
    p <- progress_obj_rx()
    if (!is.null(p) && !is.null(p$status)) {
      status <- as.character(p$status)
    }

    # Capitalize for display
    tools::toTitleCase(status)
  })
  
  output$nr_status_msg <- renderText({
    p <- progress_obj_rx()
    if (is.null(rv$run_root)) return("")
    if (is.null(p)) return(rv$last_msg %||% "")
    as.character(p$message %||% "")
  })
  
  output$nr_progress_fill <- renderUI({
    p <- progress_obj_rx()
    pct <- suppressWarnings(as.numeric(p$pct %||% 0))
    if (is.na(pct)) pct <- 0
    pct <- max(0, min(100, pct))
    # Add "active" class for animated stripes when running
    fill_class <- if (identical(rv$last_status, "running")) "nr-progress-fill active" else "nr-progress-fill"
    div(class = fill_class, style = paste0("width:", pct, "%;"))
  })

  # Elapsed time display
  output$nr_elapsed_time <- renderUI({
    # Poll while running to update timer
    if (identical(rv$last_status, "running")) {
      invalidateLater(1000, session)
    }

    if (is.null(rv$run_start_time)) return(NULL)

    elapsed <- as.numeric(difftime(Sys.time(), rv$run_start_time, units = "secs"))
    mins <- floor(elapsed / 60)
    secs <- floor(elapsed %% 60)
    sprintf("%d:%02d", mins, secs)
  })
  
  # Single-run log output - use observer with custom message to preserve scroll position
  observe({
    # Poll while running or when we have a run_root to show completed log
    if (!identical(rv$last_status, "running") && is.null(rv$run_root)) return()
    if (identical(rv$last_status, "running")) {
      invalidateLater(500, session)
    }

    # Show UI log first, then append file log if available
    ui_lines <- rv$ui_log
    file_lines <- character(0)

    if (!is.null(rv$run_root) && !is.null(rv$log_path) && file.exists(rv$log_path)) {
      file_lines <- tryCatch(
        readLines(rv$log_path, warn = FALSE, encoding = "UTF-8"),
        error = function(e) character(0)
      )
    }

    all_lines <- c(ui_lines, file_lines)
    if (length(all_lines) == 0) {
      if (!isTRUE(rv$run_log_waiting)) {
        rv$run_log_cache <- character(0)
        rv$run_log_waiting <- TRUE
        session$sendCustomMessage("nr_update_log", list(
          id = "nr_single_log_container",
          html = '<span style="color: var(--text-secondary);">(waiting for run...)</span>',
          replace = TRUE
        ))
      }
      return()
    }

    rv$run_log_waiting <- FALSE
    all_lines <- utils::tail(all_lines, 200)
    delta <- log_delta(all_lines, rv$run_log_cache)
    if (identical(delta$mode, "noop")) return()

    html_chunk <- format_log_lines(delta$lines)
    session$sendCustomMessage("nr_update_log", list(
      id = "nr_single_log_container",
      html = html_chunk,
      replace = identical(delta$mode, "replace")
    ))

    rv$run_log_cache <- all_lines
  })
  
  output$nr_download_ui <- renderUI({
    # Check if run completed (either via progress.json or rv$last_status)
    p <- progress_obj_rx()
    done_progress <- !is.null(p) && identical(as.character(p$status %||% ""), "done")
    done_status <- identical(rv$last_status, "done")
    if ((!done_progress && !done_status) || is.null(rv$run_root)) return(NULL)
    downloadButton("nr_download_terpbook", "Download .terpbook", class = "btn btn-default")
  })
  
  output$nr_download_terpbook <- downloadHandler(
    filename = function() {
      nm <- "terpbook"
      if (!is.null(rv$manifest_path) && file.exists(rv$manifest_path) && requireNamespace("jsonlite", quietly = TRUE)) {
        man <- tryCatch(jsonlite::read_json(rv$manifest_path, simplifyVector = TRUE), error = function(e) NULL)
        nm <- nr_sanitize((man$pipeline$pipeline_name %||% man$pipeline_name %||% "terpbook"))
      }
      paste0(nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".terpbook")
    },
    content = function(file) {
      run_root <- rv$run_root
      if (is.null(run_root) || !dir.exists(run_root)) stop("run_root missing")
      
      # Zip entire run folder into a single file (works on Windows via utils::zip)
      old <- setwd(dirname(run_root))
      on.exit(setwd(old), add = TRUE)
      
      base <- basename(run_root)
      
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = file, files = base)
      } else {
        # utils::zip requires 'zip' on PATH on some systems; Windows usually OK
        utils::zip(zipfile = file, files = base, flags = "-r9X")
      }
    }
  )
  
  # Stop running process
  observeEvent(input$nr_stop_run, {
    if (!is.null(rv$bg_process) && rv$bg_process$is_alive()) {
      ui_log("Stopping run...")
      tryCatch({
        rv$bg_process$kill()
      }, error = function(e) {
        ui_log(sprintf("Error stopping process: %s", conditionMessage(e)), "WARN")
      })
      rv$bg_process <- NULL
      rv$last_status <- "error"
      rv$last_msg <- "Run stopped by user"
      rv$last_err <- "Stopped by user"
      ui_log("Run stopped by user")

      # Update progress.json to reflect stopped state
      if (!is.null(rv$progress_path) && file.exists(dirname(rv$progress_path))) {
        write_progress(rv$progress_path, "error", "Stopped by user", 0)
      }
    }
  }, ignoreInit = TRUE)

  # Reset to start a new run
  observeEvent(input$nr_new_run, {
    # Reset all reactive state
    rv$last_status <- "idle"
    rv$last_msg <- ""
    rv$run_root <- NULL
    rv$progress_path <- NULL
    rv$log_path <- NULL
    rv$manifest_path <- NULL
    rv$ui_log <- character(0)
    rv$bg_process <- NULL
    rv$run_start_time <- NULL
    rv$formatted_filename <- NULL
    rv$terpflow_filename <- NULL
    rv$terpbase_filename <- NULL
    rv$metabobase_filename <- NULL

    # Reset run name input
    updateTextInput(session, "nr_run_name", value = "")

    # Reset file inputs after the gate panel is re-rendered
    session$onFlushed(function() {
      reset_file_input("nr_formatted_upload")
      reset_file_input("nr_terpflow_upload")
      reset_file_input("nr_terpbase_upload")
      reset_file_input("nr_metabobase_upload")
    }, once = TRUE)
  }, ignoreInit = TRUE)

  # =========================================================
  # QUEUE MODE OBSERVERS AND UI RENDERERS
  # =========================================================

  # Add to Queue button UI
  output$nr_queue_add_btn <- renderUI({
    if (isTRUE(all_ok())) {
      actionButton("nr_queue_add", "Add to Queue", class = "btn btn-primary")
    } else {
      tags$button(
        type = "button",
        class = "btn btn-primary",
        disabled = "disabled",
        "Add to Queue"
      )
    }
  })

  # Add item to queue
  observeEvent(input$nr_queue_add, {
    if (!isTRUE(all_ok())) {
      return()
    }

    # Create queue item
    item_id <- paste0("item_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
    run_name <- trimws(input$nr_run_name %||% "")
    if (!nzchar(run_name)) {
      run_name <- paste0("Run_", length(rv$queue_items) + 1)
    }

    item <- list(
      id = item_id,
      name = run_name,
      status = "pending",
      terpbase = list(
        mode = input$nr_terpbase_mode %||% "default",
        path = terpbase_path_rx(),
        display_name = rv$terpbase_filename %||% basename(terpbase_path_rx() %||% "default")
      ),
      metabobase = list(
        mode = input$nr_metabobase_mode %||% "default",
        path = metabobase_path_rx(),
        display_name = rv$metabobase_filename %||% basename(metabobase_path_rx() %||% "default")
      ),
      formatted = list(
        path = formatted_path_rx(),
        display_name = rv$formatted_filename %||% basename(formatted_path_rx() %||% "")
      ),
      terpflow = list(
        path = terpflow_path_rx(),
        display_name = rv$terpflow_filename %||% basename(terpflow_path_rx() %||% "")
      ),
      run_root = NULL,
      progress = list(pct = 0, msg = "Pending"),
      error = NULL,
      start_time = NULL,
      end_time = NULL
    )

    # Persist files to durable temp location
    item <- nr_persist_queue_item_files(item)

    # Add to queue
    rv$queue_items <- c(rv$queue_items, list(item))

    # Reset form for next entry
    updateTextInput(session, "nr_run_name", value = "")
    session$onFlushed(function() {
      reset_file_input("nr_formatted_upload")
      reset_file_input("nr_terpflow_upload")
      reset_file_input("nr_terpbase_upload")
      reset_file_input("nr_metabobase_upload")
    }, once = TRUE)

    # Reset cached filenames
    rv$formatted_filename <- NULL
    rv$terpflow_filename <- NULL
    rv$terpbase_filename <- NULL
    rv$metabobase_filename <- NULL
  }, ignoreInit = TRUE)

  # Queue items list UI
  output$nr_queue_items_ui <- renderUI({
    items <- rv$queue_items
    if (length(items) == 0) {
      return(div(class = "nr-queue-empty", "No items in queue. Add runs above."))
    }

    # Force re-render when queue is running
    if (rv$queue_running) {
      invalidateLater(500, session)
    }

    item_cards <- lapply(seq_along(items), function(i) {
      item <- items[[i]]
      status_class <- switch(item$status,
        "running" = "running",
        "done" = "done",
        "error" = "error",
        ""
      )

      status_dot <- switch(item$status,
        "pending" = span(class = "nr-dot gray"),
        "running" = span(class = "nr-dot", style = "background: var(--status-warn-border); animation: pulse-running 1.5s infinite;"),
        "done" = span(class = "nr-dot green"),
        "error" = span(class = "nr-dot red"),
        span(class = "nr-dot gray")
      )

      progress_ui <- NULL
      if (identical(item$status, "running")) {
        pct <- item$progress$pct %||% 0
        progress_ui <- div(class = "nr-queue-item-progress",
          div(class = "nr-progress-bar",
            div(class = "nr-progress-fill active", style = paste0("width:", pct, "%;"))
          )
        )
      }

      files_text <- paste0(
        item$formatted$display_name %||% "data",
        " + ",
        item$terpflow$display_name %||% "pipeline"
      )

      remove_btn <- NULL
      if (identical(item$status, "pending") && !rv$queue_running) {
        remove_btn <- actionButton(
          paste0("nr_queue_remove_", item$id),
          "",
          icon = icon("times"),
          class = "btn btn-sm btn-default",
          onclick = sprintf("Shiny.setInputValue('nr_queue_remove', '%s', {priority: 'event'})", item$id)
        )
      }

      div(class = paste("nr-queue-item", status_class),
        div(class = "nr-queue-item-num", i),
        div(class = "nr-queue-item-info",
          div(class = "nr-queue-item-name", item$name),
          div(class = "nr-queue-item-files", files_text)
        ),
        div(class = "nr-queue-item-status",
          status_dot,
          span(style = "font-size: 12px; font-weight: 600;",
            if (identical(item$status, "running")) item$progress$msg %||% "Running..."
            else if (identical(item$status, "done")) "Complete"
            else if (identical(item$status, "error")) item$error %||% "Failed"
            else "Pending"
          )
        ),
        progress_ui,
        div(class = "nr-queue-item-actions", remove_btn)
      )
    })

    do.call(tagList, item_cards)
  })

  # Remove item from queue
  observeEvent(input$nr_queue_remove, {
    item_id <- input$nr_queue_remove
    rv$queue_items <- rv$queue_items[
      !vapply(rv$queue_items, function(x) identical(x$id, item_id), logical(1))
    ]
  }, ignoreInit = TRUE)

  # Queue actions bar UI
  output$nr_queue_actions_ui <- renderUI({
    items <- rv$queue_items
    if (length(items) == 0) return(NULL)

    # Check completion status
    all_complete <- all(vapply(items, function(x) x$status %in% c("done", "error"), logical(1)))
    n_done <- sum(vapply(items, function(x) identical(x$status, "done"), logical(1)))

    div(class = "nr-queue-actions-bar",
      if (rv$queue_running) {
        tagList(
          actionButton("nr_queue_stop", "Stop Queue", class = "btn btn-danger"),
          span(style = "font-weight: 600; margin-left: 10px;",
            sprintf("Running %d of %d...", rv$queue_current_index, length(items))
          )
        )
      } else if (all_complete) {
        tagList(
          if (n_done > 0) {
            downloadButton("nr_queue_download_all", sprintf("Download All Results (%d)", n_done), class = "btn btn-primary")
          },
          actionButton("nr_queue_clear", "Clear Queue", class = "btn btn-default"),
          actionButton("nr_queue_new", "New Queue", class = "btn btn-default")
        )
      } else {
        tagList(
          actionButton("nr_queue_run_all", "Run All", class = "btn btn-primary btn-lg"),
          actionButton("nr_queue_clear", "Clear Queue", class = "btn btn-default")
        )
      }
    )
  })

  # Clear queue
  observeEvent(input$nr_queue_clear, {
    if (rv$queue_running) return()
    rv$queue_items <- list()
  }, ignoreInit = TRUE)

  # New queue (reset completed queue)
  observeEvent(input$nr_queue_new, {
    rv$queue_items <- list()
    rv$queue_running <- FALSE
    rv$queue_current_index <- 0
    rv$queue_bg_process <- NULL
    rv$queue_results_dir <- NULL
    rv$ui_log <- character(0)
  }, ignoreInit = TRUE)

  # Helper to start next queue item
  start_next_queue_item <- function() {
    rv$queue_current_index <- rv$queue_current_index + 1
    idx <- rv$queue_current_index

    if (idx > length(rv$queue_items)) {
      # All items complete
      rv$queue_running <- FALSE
      rv$queue_bg_process <- NULL
      n_done <- sum(vapply(rv$queue_items, function(x) identical(x$status, "done"), logical(1)))
      n_error <- sum(vapply(rv$queue_items, function(x) identical(x$status, "error"), logical(1)))
      ui_log(sprintf("=== Queue Complete: %d succeeded, %d failed ===", n_done, n_error))
      return()
    }

    item <- rv$queue_items[[idx]]
    ui_log(sprintf("=== Starting Item %d/%d: %s ===", idx, length(rv$queue_items), item$name))

    # Update item status
    rv$queue_items[[idx]]$status <- "running"
    rv$queue_items[[idx]]$start_time <- Sys.time()
    rv$queue_items[[idx]]$progress <- list(pct = 0, msg = "Initiating...")

    # Create item-specific output directory
    item_out_dir <- file.path(rv$queue_results_dir, sprintf("item_%03d_%s", idx, item$id))
    dir.create(item_out_dir, recursive = TRUE, showWarnings = FALSE)

    # Create run directory
    run_name <- paste0(nr_sanitize(item$name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    run_root <- file.path(item_out_dir, paste0(run_name, ".terpbook"))
    dir.create(run_root, recursive = TRUE, showWarnings = FALSE)

    progress_path <- file.path(run_root, "progress.json")
    log_path <- file.path(run_root, "log.txt")

    # Write initial progress
    write_progress(progress_path, "running", "Initializing...", 5)

    # Store paths in item
    rv$queue_items[[idx]]$run_root <- run_root
    rv$queue_items[[idx]]$progress_path <- progress_path
    rv$queue_items[[idx]]$log_path <- log_path

    # Get paths
    tb_norm <- if (!is.null(item$terpbase$path) && nzchar(item$terpbase$path)) nr_norm(item$terpbase$path) else NULL
    mb_norm <- if (!is.null(item$metabobase$path) && nzchar(item$metabobase$path)) nr_norm(item$metabobase$path) else NULL
    cb_norm <- if (!is.null(item$complexbase$path) && nzchar(item$complexbase$path)) nr_norm(item$complexbase$path) else NULL
    ff_norm <- nr_norm(item$formatted$path)
    tf_norm <- nr_norm(item$terpflow$path)
    out_norm <- nr_norm(item_out_dir)
    app_root <- getwd()

    # Queue runner function (same as single run but simpler)
    queue_runner_fn <- function(app_root, formatted_path, terpflow_path, terpbase_path,
                                metabobase_path = NULL, complexbase_path = NULL,
                                out_dir, run_name, progress_path, log_path) {
      setwd(app_root)

      write_prog <- function(status, message, pct = NULL) {
        obj <- list(status = status, message = message, pct = pct,
                    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        jsonlite::write_json(obj, progress_path, auto_unbox = TRUE, pretty = TRUE)
      }

      write_log <- function(msg) {
        is_first <- !file.exists(log_path) || file.info(log_path)$size == 0
        time_str <- format(Sys.time(), if (is_first) "%Y-%m-%d %H:%M:%S" else "%H:%M:%S")
        cat(sprintf("[%s] %s\n", time_str, msg), file = log_path, append = TRUE)
      }

      write_prog("running", "Loading engine code...", 8)
      write_log("Loading engine code...")

      tryCatch({
        source(file.path(app_root, "R", "00_init.R"), local = FALSE)
        engine_files <- list.files(file.path(app_root, "R", "engines"), pattern = "\\.R$", full.names = TRUE)
        for (f in engine_files) source(f, local = FALSE)
        stats_files <- list.files(file.path(app_root, "R", "engines", "stats"), pattern = "\\.R$", full.names = TRUE)
        for (f in stats_files) source(f, local = FALSE)
        utils_files <- list.files(file.path(app_root, "R", "utils"), pattern = "\\.R$", full.names = TRUE)
        for (f in utils_files) source(f, local = FALSE)
        write_log("Engine code loaded")
      }, error = function(e) {
        write_prog("error", paste("Failed to load engine code:", conditionMessage(e)), 0)
        write_log(paste("ERROR:", conditionMessage(e)))
        stop(e)
      })

      progress_callback <- function(status, msg, pct) {
        write_prog("running", msg, pct)
        write_log(msg)
      }

      result <- tryCatch({
        nr_execute_run(
          formatted_path = formatted_path,
          terpflow_path = terpflow_path,
          terpbase_path = terpbase_path,
          metabobase_path = metabobase_path,
          out_dir = out_dir,
          run_name = run_name,
          progress_callback = progress_callback,
          complexbase_path = complexbase_path
        )
      }, error = function(e) {
        write_prog("error", paste("Pipeline failed:", conditionMessage(e)), 0)
        write_log(paste("ERROR:", conditionMessage(e)))
        list(ok = FALSE, error = conditionMessage(e))
      })

      if (isTRUE(result$ok)) {
        write_prog("done", "Complete!", 100)
        write_log("Pipeline completed successfully")
      }

      result
    }

    # Launch background process
    rv$queue_bg_process <- callr::r_bg(
      func = queue_runner_fn,
      args = list(
        app_root = app_root,
        formatted_path = ff_norm,
        terpflow_path = tf_norm,
        terpbase_path = tb_norm,
        metabobase_path = mb_norm,
        complexbase_path = cb_norm,
        out_dir = out_norm,
        run_name = run_name,
        progress_path = progress_path,
        log_path = log_path
      ),
      package = TRUE,
      supervise = TRUE
    )
  }

  # Run All - start queue execution
  observeEvent(input$nr_queue_run_all, {
    if (length(rv$queue_items) == 0) return()
    if (rv$queue_running) return()

    rv$queue_log_cache <- character(0)
    rv$queue_log_waiting <- FALSE
    session$sendCustomMessage("nr_update_log", list(
      id = "nr_queue_log_container",
      html = "",
      replace = TRUE
    ))

    rv$queue_running <- TRUE
    rv$queue_current_index <- 0
    rv$queue_start_time <- Sys.time()
    rv$queue_results_dir <- tempfile("queue_results_")
    dir.create(rv$queue_results_dir, recursive = TRUE)
    rv$ui_log <- character(0)

    ui_log("=== Starting Queue Execution ===")
    ui_log(sprintf("Items in queue: %d", length(rv$queue_items)))

    # Reset all items to pending
    for (i in seq_along(rv$queue_items)) {
      rv$queue_items[[i]]$status <- "pending"
      rv$queue_items[[i]]$progress <- list(pct = 0, msg = "Waiting...")
      rv$queue_items[[i]]$error <- NULL
      rv$queue_items[[i]]$run_root <- NULL
    }

    # Start first item
    start_next_queue_item()
  }, ignoreInit = TRUE)

  # Queue progress polling observer
  observe({
    if (!rv$queue_running || is.null(rv$queue_bg_process)) return()

    idx <- rv$queue_current_index
    if (idx < 1 || idx > length(rv$queue_items)) return()

    item <- rv$queue_items[[idx]]
    progress_path <- item$progress_path

    # Poll progress file
    if (!is.null(progress_path) && file.exists(progress_path)) {
      prog <- tryCatch(jsonlite::read_json(progress_path), error = function(e) NULL)
      if (!is.null(prog)) {
        rv$queue_items[[idx]]$progress <- list(
          pct = as.numeric(prog$pct %||% 0),
          msg = as.character(prog$message %||% "")
        )
      }
    }

    # Check if process completed
    if (!rv$queue_bg_process$is_alive()) {
      result <- tryCatch(rv$queue_bg_process$get_result(), error = function(e) {
        list(ok = FALSE, error = conditionMessage(e))
      })

      # Update item status
      if (isTRUE(result$ok)) {
        rv$queue_items[[idx]]$status <- "done"
        # Use result$run_root if available, otherwise keep the pre-set run_root
        final_run_root <- if (!is.null(result$run_root)) {
          nr_norm(result$run_root)
        } else {
          rv$queue_items[[idx]]$run_root  # Keep existing
        }
        rv$queue_items[[idx]]$run_root <- final_run_root
        ui_log(sprintf("Item %d complete: %s (output: %s)", idx, item$name, final_run_root %||% "unknown"))
      } else {
        rv$queue_items[[idx]]$status <- "error"
        rv$queue_items[[idx]]$error <- result$error %||% "Unknown error"
        ui_log(sprintf("Item %d FAILED: %s - %s", idx, item$name, result$error %||% "Unknown error"))
      }

      rv$queue_items[[idx]]$end_time <- Sys.time()
      rv$queue_bg_process <- NULL

      # Continue to next item
      start_next_queue_item()
    } else {
      # Still running - poll again
      invalidateLater(500, session)
    }
  })

  # Stop queue execution
  observeEvent(input$nr_queue_stop, {
    if (!rv$queue_running) return()

    # Kill current background process
    if (!is.null(rv$queue_bg_process) && rv$queue_bg_process$is_alive()) {
      tryCatch(rv$queue_bg_process$kill(), error = function(e) NULL)
    }

    # Mark current item as error
    idx <- rv$queue_current_index
    if (idx >= 1 && idx <= length(rv$queue_items)) {
      rv$queue_items[[idx]]$status <- "error"
      rv$queue_items[[idx]]$error <- "Stopped by user"
    }

    rv$queue_running <- FALSE
    rv$queue_bg_process <- NULL

    ui_log("Queue stopped by user")
  }, ignoreInit = TRUE)

  # Download all queue results
  output$nr_queue_download_all <- downloadHandler(
    filename = function() {
      paste0("queue_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # Collect all successful terpbooks
      # Note: isolate() ensures we get current values in non-reactive context
      items <- isolate(rv$queue_items)
      terpbooks <- list()
      skipped_items <- character()
      for (item in items) {
        if (identical(item$status, "done")) {
          if (is.null(item$run_root)) {
            skipped_items <- c(skipped_items, sprintf("%s: run_root is NULL", item$name))
          } else if (!dir.exists(item$run_root)) {
            skipped_items <- c(skipped_items, sprintf("%s: dir not found at %s", item$name, item$run_root))
          } else {
            # Check if directory has content (manifest.json indicates a complete terpbook)
            manifest_path <- file.path(item$run_root, "manifest.json")
            if (!file.exists(manifest_path)) {
              skipped_items <- c(skipped_items, sprintf("%s: dir exists but no manifest.json at %s", item$name, item$run_root))
            } else {
              terpbooks[[item$name]] <- item$run_root
            }
          }
        }
      }

      if (length(terpbooks) == 0) {
        msg <- "No completed runs to download"
        if (length(skipped_items) > 0) {
          msg <- paste(msg, "Skipped:", paste(skipped_items, collapse = "; "))
        }
        stop(msg)
      }

      # Create combined zip directory
      temp_zip_dir <- tempfile("combined_")
      dir.create(temp_zip_dir, recursive = TRUE)

      for (name in names(terpbooks)) {
        src <- terpbooks[[name]]
        safe_name <- nr_sanitize(name)
        terpbook_file <- file.path(temp_zip_dir, paste0(safe_name, ".terpbook"))
        # Create a proper .terpbook zip file (not a folder)
        src_folder <- basename(src)
        src_parent <- dirname(src)
        res_safe_zip(zipfile = terpbook_file, files = src_folder, root = src_parent)
      }

      # Add queue summary CSV with run_root info for debugging
      summary_df <- data.frame(
        Name = vapply(items, function(x) x$name %||% "", character(1)),
        Status = vapply(items, function(x) x$status %||% "", character(1)),
        Error = vapply(items, function(x) x$error %||% "", character(1)),
        RunRoot = vapply(items, function(x) x$run_root %||% "(not set)", character(1)),
        RunRootExists = vapply(items, function(x) {
          if (is.null(x$run_root)) return("N/A")
          if (dir.exists(x$run_root)) "YES" else "NO"
        }, character(1)),
        stringsAsFactors = FALSE
      )
      write.csv(summary_df, file.path(temp_zip_dir, "queue_summary.csv"), row.names = FALSE)

      # Zip everything
      old_wd <- setwd(temp_zip_dir)
      on.exit(setwd(old_wd), add = TRUE)

      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = file, files = list.files("."))
      } else {
        utils::zip(zipfile = file, files = list.files("."), flags = "-r9X")
      }
    }
  )
}
