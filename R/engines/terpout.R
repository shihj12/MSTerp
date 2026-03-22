# =========================================================
# engines/terpbook.R — Run output writer (.terpbook)
# Creates a portable run folder:
#   <run_id>.terpbook/manifest.json, log.txt, steps/*/...
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

terpbook_sanitize <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  if (!nzchar(x)) return("x")
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

terpbook_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

terpbook_read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")
  jsonlite::read_json(path, simplifyVector = TRUE)
}

terpbook_write_json <- function(x, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

terpbook_manifest_path <- function(run_root) file.path(run_root, "manifest.json")
terpbook_log_path <- function(run_root) file.path(run_root, "log.txt")

terpbook_log <- function(run_root, msg, level = "INFO") {
  line <- sprintf("[%s] [%s] %s\n", terpbook_now(), level, msg)
  cat(line, file = terpbook_log_path(run_root), append = TRUE)
  invisible(TRUE)
}

terpbook_create_run <- function(base_dir,
                               run_id = NULL,
                               pipeline = NULL,
                               inputs = NULL,
                               overwrite = FALSE) {
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  run_id <- run_id %||% paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  run_id <- terpbook_sanitize(run_id)
  
  run_root <- file.path(base_dir, paste0(run_id, ".terpbook"))
  if (dir.exists(run_root)) {
    if (!isTRUE(overwrite)) {
      # ensure unique
      run_root <- file.path(base_dir, paste0(run_id, "_", format(Sys.time(), "%H%M%S"), ".terpbook"))
    } else {
      unlink(run_root, recursive = TRUE, force = TRUE)
    }
  }
  
  dir.create(run_root, recursive = TRUE, showWarnings = FALSE)

  # Contract-compliant manifest: steps array with step_index, step_id, engine_id, step_dir
  manifest <- list(
    steps = list()
  )

  terpbook_write_json(manifest, terpbook_manifest_path(run_root))
  terpbook_log(run_root, paste0("Created run: ", run_root))

  run_root
}

terpbook_copy_pipeline_file <- function(run_root, terpflow_path) {
  if (is.null(terpflow_path) || !nzchar(terpflow_path) || !file.exists(terpflow_path)) return(FALSE)
  dst <- file.path(run_root, "pipeline.terpflow.json")
  ok <- tryCatch(file.copy(terpflow_path, dst, overwrite = TRUE), error = function(e) FALSE)
  if (isTRUE(ok)) terpbook_log(run_root, paste0("Copied pipeline to: ", dst))
  isTRUE(ok)
}

terpbook_step_dir_rel <- function(step_order, step_index = NULL, engine_id = NULL, step_id = NULL) {
  # Contract-compliant format: step_001, step_002, etc.
  sprintf("step_%03d", as.integer(step_order))
}


terpbook_step_paths <- function(run_root, step_dir_rel) {
  step_dir_abs <- file.path(run_root, step_dir_rel)
  list(
    step_dir_abs = step_dir_abs,
    step_json = file.path(step_dir_abs, "step.json"),
    results_rds = file.path(step_dir_abs, "results.rds"),
    artifacts_json = file.path(step_dir_abs, "artifacts.json"),
    render_state = file.path(step_dir_abs, "render_state.json")
  )
}

terpbook_load_manifest <- function(run_root) {
  terpbook_read_json(terpbook_manifest_path(run_root))
}

terpbook_save_manifest <- function(run_root, manifest) {
  terpbook_write_json(manifest, terpbook_manifest_path(run_root))
  invisible(TRUE)
}

terpbook_step_start <- function(run_root,
                                step_index,
                                engine_id,
                                step_id,
                                label = NULL,
                                params = NULL,
                                style = NULL,
                                extra = list()) {
  man <- terpbook_load_manifest(run_root)

  # Assign monotonic order (1-based)
  steps <- man$steps %||% list()
  step_order <- length(steps) + 1

  step_index_int <- as.integer(step_index %||% step_order)
  step_dir_rel <- terpbook_step_dir_rel(step_order)
  p <- terpbook_step_paths(run_root, step_dir_rel)

  dir.create(p$step_dir_abs, recursive = TRUE, showWarnings = FALSE)

  # Contract-compliant manifest entry: step_index, step_id, engine_id, step_dir
  manifest_entry <- list(
    step_index = step_index_int,
    step_id = as.character(step_id %||% sprintf("step_%d", step_order)),
    engine_id = tolower(as.character(engine_id)),
    step_dir = step_dir_rel
  )

  # Step descriptor (step.json) with additional metadata
  step_descriptor <- list(
    engine_id = tolower(as.character(engine_id)),
    label = as.character(label %||% step_id %||% engine_id),
    meta = list(
      step_index = step_index_int,
      started_at = terpbook_now()
    )
  )
  if (!is.null(style) && length(style) > 0) {
    step_descriptor$style <- style
  }

  # Write step.json descriptor

  terpbook_write_json(step_descriptor, p$step_json)

  # Add entry to manifest
  steps <- c(steps, list(manifest_entry))
  man$steps <- steps
  terpbook_save_manifest(run_root, man)

  terpbook_log(run_root, sprintf(
    "Step start: [%d] %s (%s) -> %s",
    step_index_int, engine_id, step_id, step_dir_rel
  ))

  list(
    step_index = step_index_int,
    step_dir_rel = step_dir_rel,
    paths = p
  )
}

terpbook_step_finish <- function(run_root, step_dir_rel, status = "ok", summary = NULL, error = NULL) {
  p <- terpbook_step_paths(run_root, step_dir_rel)

  # Update step.json metadata
  step_desc <- if (file.exists(p$step_json)) terpbook_read_json(p$step_json) else list()
  step_desc$meta <- step_desc$meta %||% list()
  step_desc$meta$status <- status
  step_desc$meta$finished_at <- terpbook_now()
  if (!is.null(summary)) step_desc$meta$summary <- summary
  if (!is.null(error)) step_desc$meta$error <- error
  terpbook_write_json(step_desc, p$step_json)

  terpbook_log(
    run_root,
    sprintf("Step finish: %s (%s)", step_dir_rel, status),
    level = if (identical(status, "ok")) "INFO" else "ERROR"
  )

  invisible(TRUE)
}

terpbook_write_results <- function(run_root, step_dir_rel, results_obj) {
  p <- terpbook_step_paths(run_root, step_dir_rel)
  saveRDS(results_obj, p$results_rds)
  terpbook_log(run_root, paste0("Wrote results: ", step_dir_rel, "/results.rds"))
  invisible(TRUE)
}

terpbook_add_artifact_files <- function(run_root, step_dir_rel, files_named) {
  # files_named: named list or named character vector of absolute paths
  p <- terpbook_step_paths(run_root, step_dir_rel)
  art <- if (file.exists(p$artifacts_json)) terpbook_read_json(p$artifacts_json) else list()
  art$files <- art$files %||% list()
  
  nms <- names(files_named) %||% rep("", length(files_named))
  for (i in seq_along(files_named)) {
    nm <- nms[[i]]
    fp <- files_named[[i]]
    if (!nzchar(nm)) nm <- basename(fp)
    art$files[[nm]] <- fp
  }
  terpbook_write_json(art, p$artifacts_json)
  invisible(TRUE)
}

terpbook_finish_run <- function(run_root, status = "ok") {
  man <- terpbook_load_manifest(run_root)
  man$status <- status
  man$finished_at <- terpbook_now()
  terpbook_save_manifest(run_root, man)
  terpbook_log(run_root, paste0("Run finished: ", status))
  invisible(TRUE)
}

# =========================================================
# Progress Tracking
# =========================================================

msterp_update_progress <- function(run_root, status, message, pct, current_step = NULL, total_steps = NULL) {
  progress_path <- file.path(run_root, "progress.json")
  prog <- list(
    status = status,
    message = message,
    pct = max(0, min(100, as.numeric(pct))),
    current_step = current_step,
    total_steps = total_steps,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  terpbook_write_json(prog, progress_path)
}

# =========================================================
# Data Splitting and Metadata Extraction
# =========================================================

msterp_extract_design_metadata <- function(design_df) {
  # Extract metadata from design sheet
  meta_rows <- design_df[design_df$record_type == "meta", c("key", "value"), drop = FALSE]
  meta_map <- stats::setNames(as.character(meta_rows$value), as.character(meta_rows$key))

  # Extract groups
  group_rows <- design_df[design_df$record_type == "group", , drop = FALSE]
  groups <- if (nrow(group_rows) > 0) {
    group_rows[, intersect(c("group_id", "group_name", "color"), names(group_rows)), drop = FALSE]
  } else {
    data.frame(group_id = character(), group_name = character(), color = character())
  }

  # Extract columns
  col_rows <- design_df[design_df$record_type == "column", , drop = FALSE]

  # Parse include column
  if ("include" %in% names(col_rows)) {
    if (is.logical(col_rows$include)) {
      col_rows$include <- col_rows$include
    } else {
      col_rows$include <- tolower(as.character(col_rows$include)) %in% c("true", "t", "1", "yes", "y")
    }
    col_rows <- col_rows[!is.na(col_rows$include) & col_rows$include, , drop = FALSE]
  }

  list(
    meta = meta_map,
    groups = groups,
    columns = col_rows,
    analysis_level = tolower(trimws(meta_map[["analysis_level"]] %||% "protein")),
    id_primary_col = meta_map[["id_primary_col"]] %||% "",
    id_gene_col = meta_map[["id_gene_col"]] %||% "",
    id_protein_col = meta_map[["id_protein_col"]] %||% "",
    id_peptide_col = meta_map[["id_peptide_col"]] %||% ""
  )
}

msterp_get_id_columns <- function(metadata) {
  # Return all ID columns in stable order (protein-only)
  cols <- c(
    metadata$id_gene_col,
    metadata$id_protein_col
  )
  cols <- cols[!is.na(cols) & nzchar(as.character(cols))]
  unique(as.character(cols))
}

msterp_split_data_for_step <- function(data_full, step_params, metadata) {
  # Extract measurement column names from step params
  # Different engines use different param structures
  meas_cols <- character()

  # Two-group comparison (volcano, etc.)
  if (!is.null(step_params$group_a)) {
    meas_cols <- c(meas_cols, step_params$group_a)
  }
  if (!is.null(step_params$group_b)) {
    meas_cols <- c(meas_cols, step_params$group_b)
  }

  # Multi-group (heatmap, pca, etc.)
  if (!is.null(step_params$groups)) {
    for (grp in step_params$groups) {
      if (is.list(grp) && !is.null(grp$columns)) {
        meas_cols <- c(meas_cols, grp$columns)
      }
    }
  }

  # If no measurement columns found, use all measurement columns from metadata
  if (length(meas_cols) == 0) {
    meas_cols <- as.character(metadata$columns$data_col)
  }

  meas_cols <- unique(meas_cols)

  # Get ID columns
  id_cols <- msterp_get_id_columns(metadata)

  # Subset data
  all_cols <- c(id_cols, meas_cols)
  missing_cols <- setdiff(all_cols, names(data_full))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "Missing columns in data: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  present_cols <- intersect(all_cols, names(data_full))
  data_subset <- data_full[, present_cols, drop = FALSE]

  list(
    data = data_subset,
    id_cols = id_cols,
    meas_cols = intersect(meas_cols, names(data_full)),
    metadata = metadata
  )
}

# =========================================================
# Engine Execution
# =========================================================

msterp_execute_engine <- function(engine_id, step_params, data_subset, metadata, terpbase = NULL, registry = NULL) {
  # Get engine definition from registry
  if (is.null(registry)) {
    if (exists("msterp_engine_registry", mode = "function")) {
      registry <- msterp_engine_registry()
    } else {
      stop("Registry not available")
    }
  }

  engine_def <- registry$engines[[engine_id]]
  if (is.null(engine_def)) {
    stop(sprintf("Unknown engine: %s", engine_id))
  }

  # Validate requirements
  req <- engine_def$requirements %||% list()

  # Check terpbase requirement
  if (isTRUE(req$requires_terpbase)) {
    if (is.null(terpbase)) {
      stop(sprintf(
        "Engine '%s' requires a terpbase but none was provided",
        engine_id
      ))
    }
  }

  # Check required IDs
  required_ids <- req$required_ids %||% character()
  for (req_id in required_ids) {
    id_col_key <- paste0("id_", req_id, "_col")
    id_col <- metadata[[id_col_key]] %||% ""
    if (!nzchar(id_col)) {
      stop(sprintf(
        "Engine '%s' requires ID column for '%s' but it's not defined in metadata",
        engine_id, req_id
      ))
    }
    if (!id_col %in% names(data_subset)) {
      stop(sprintf(
        "Engine '%s' requires column '%s' but it's missing from data",
        engine_id, id_col
      ))
    }
  }

  # Dispatch to engine-specific function
  # For now, return a placeholder - actual engine implementations would go here
  result <- list(
    engine_id = engine_id,
    status = "not_implemented",
    message = sprintf("Engine '%s' execution not yet implemented", engine_id),
    data_modified = NULL
  )

  # Check if engine function exists
  engine_fn_name <- paste0("msterp_engine_", engine_id)
  if (exists(engine_fn_name, mode = "function")) {
    engine_fn <- get(engine_fn_name)

    # Call engine function
    result <- tryCatch({
      if (isTRUE(req$requires_terpbase)) {
        engine_fn(data = data_subset, params = step_params, metadata = metadata, terpbase = terpbase)
      } else {
        engine_fn(data = data_subset, params = step_params, metadata = metadata)
      }
    }, error = function(e) {
      list(
        engine_id = engine_id,
        status = "error",
        message = conditionMessage(e),
        error = conditionMessage(e),
        data_modified = NULL
      )
    })
  }

  result
}

# =========================================================
# Worker Function (runs in background or sync)
# =========================================================

msterp_terpbook_worker <- function(run_root, terpbase, formatted_obj, pipeline, registry = NULL) {
  # Initialize progress
  msterp_update_progress(run_root, "starting", "Initializing...", 0)
  terpbook_log(run_root, "Worker started")

  # Extract metadata from design sheet
  metadata <- msterp_extract_design_metadata(formatted_obj$design)
  terpbook_log(run_root, sprintf(
    "Analysis level: %s, Primary ID: %s",
    metadata$analysis_level,
    metadata$id_primary_col
  ))

  # Get full data
  data_current <- formatted_obj$data
  terpbook_log(run_root, sprintf(
    "Loaded data: %d rows, %d columns",
    nrow(data_current), ncol(data_current)
  ))

  # Get pipeline steps
  steps <- pipeline$steps %||% list()
  total_steps <- length(steps)

  if (total_steps == 0) {
    msterp_update_progress(run_root, "error", "No steps in pipeline", 0)
    terpbook_finish_run(run_root, "error")
    stop("Pipeline has no steps")
  }

  terpbook_log(run_root, sprintf("Processing %d steps", total_steps))

  # Process each step
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    engine_id <- step$engine_id %||% ""
    step_id <- step$step_id %||% sprintf("step_%d", i)
    step_params <- step$params %||% list()
    step_style <- step$style %||% list()

    # Update progress
    pct <- ((i - 1) / total_steps) * 95  # Leave 5% for finalization
    msterp_update_progress(
      run_root,
      status = "running",
      message = sprintf("Step %d/%d: %s", i, total_steps, engine_id),
      pct = pct,
      current_step = i,
      total_steps = total_steps
    )

    terpbook_log(run_root, sprintf(
      "=== Step %d/%d: %s (%s) ===",
      i, total_steps, engine_id, step_id
    ))

    # Start step tracking
    step_info <- terpbook_step_start(
      run_root = run_root,
      step_index = i,
      engine_id = engine_id,
      step_id = step_id,
      label = step$label %||% step_id,
      style = step_style
    )

    # Split data for this step
    data_split <- tryCatch({
      msterp_split_data_for_step(data_current, step_params, metadata)
    }, error = function(e) {
      terpbook_log(run_root, sprintf("Data split error: %s", conditionMessage(e)), level = "ERROR")
      terpbook_step_finish(run_root, step_info$step_dir_rel, status = "error", error = conditionMessage(e))
      stop(e)
    })

    terpbook_log(run_root, sprintf(
      "Data subset: %d rows, %d measurement columns",
      nrow(data_split$data), length(data_split$meas_cols)
    ))

    # Execute engine
    result <- tryCatch({
      msterp_execute_engine(
        engine_id = engine_id,
        step_params = step_params,
        data_subset = data_split$data,
        metadata = metadata,
        terpbase = terpbase,
        registry = registry
      )
    }, error = function(e) {
      terpbook_log(run_root, sprintf("Engine execution error: %s", conditionMessage(e)), level = "ERROR")
      list(
        status = "error",
        error = conditionMessage(e),
        data_modified = NULL
      )
    })

    # Check for errors
    if (!is.null(result$error) || identical(result$status, "error")) {
      error_msg <- result$error %||% result$message %||% "Unknown error"
      terpbook_step_finish(run_root, step_info$step_dir_rel, status = "error", error = error_msg)
      terpbook_finish_run(run_root, "error")
      msterp_update_progress(run_root, "error", sprintf("Step %d failed: %s", i, error_msg), pct)
      stop(sprintf("Step %d failed: %s", i, error_msg))
    }

    # Special handling for dataprocessor: update data_current
    if (engine_id == "dataprocessor" && !is.null(result$data_modified)) {
      data_current <- result$data_modified
      terpbook_log(run_root, sprintf(
        "Dataprocessor modified data: %d rows, %d cols",
        nrow(data_current), ncol(data_current)
      ))
    }

    # Save results
    terpbook_write_results(run_root, step_info$step_dir_rel, result)

    # Finish step
    summary_msg <- result$message %||% sprintf("Completed %s", engine_id)
    terpbook_step_finish(run_root, step_info$step_dir_rel, status = "ok", summary = summary_msg)

    terpbook_log(run_root, sprintf("Step %d completed successfully", i))
  }

  # Finalize
  msterp_update_progress(run_root, "done", "All steps completed", 100, total_steps, total_steps)
  terpbook_finish_run(run_root, "done")
  terpbook_log(run_root, "Worker completed successfully")

  invisible(TRUE)
}

# =========================================================
# Main Runner Function
# =========================================================

msterp_terpbook_run <- function(terpbase_path,
                                formatted_path,
                                terpflow_path,
                                out_parent,
                                run_label = NULL,
                                use_callr = TRUE,
                                wait = FALSE,
                                progress_callback = NULL) {

  # Normalize paths
  terpbase_path <- normalizePath(terpbase_path, winslash = "/", mustWork = FALSE)
  formatted_path <- normalizePath(formatted_path, winslash = "/", mustWork = FALSE)
  terpflow_path <- normalizePath(terpflow_path, winslash = "/", mustWork = FALSE)
  out_parent <- normalizePath(out_parent, winslash = "/", mustWork = FALSE)

  # Validate inputs
  if (!file.exists(terpbase_path)) stop("Terpbase file not found: ", terpbase_path)
  if (!file.exists(formatted_path)) stop("Formatted file not found: ", formatted_path)
  if (!file.exists(terpflow_path)) stop("Terpflow file not found: ", terpflow_path)

  # Load inputs
  terpbase <- tryCatch(readRDS(terpbase_path), error = function(e) {
    stop("Failed to load terpbase: ", conditionMessage(e))
  })

  formatted_obj <- tryCatch({
    if (!exists("msterp_read_formatted_xlsx", mode = "function")) {
      stop("msterp_read_formatted_xlsx not available")
    }
    msterp_read_formatted_xlsx(formatted_path)
  }, error = function(e) {
    stop("Failed to load formatted data: ", conditionMessage(e))
  })

  pipeline <- tryCatch(readRDS(terpflow_path), error = function(e) {
    stop("Failed to load pipeline: ", conditionMessage(e))
  })

  # Validate formatted data
  if (exists("msterp_validate_formatted", mode = "function")) {
    val <- msterp_validate_formatted(formatted_obj)
    if (!isTRUE(val$ok)) {
      stop("Formatted data validation failed: ", paste(val$errors, collapse = "; "))
    }
  }

  # Create run directory
  run_id <- run_label %||% pipeline$pipeline_name %||% "run"
  run_id <- terpbook_sanitize(run_id)

  run_root <- terpbook_create_run(
    base_dir = out_parent,
    run_id = run_id
  )

  # Copy pipeline file
  terpbook_copy_pipeline_file(run_root, terpflow_path)

  # Initialize progress file
  msterp_update_progress(run_root, "starting", "Preparing to run...", 0)

  manifest_path <- terpbook_manifest_path(run_root)
  progress_path <- file.path(run_root, "progress.json")
  log_path <- terpbook_log_path(run_root)

  # Get registry
  registry <- if (exists("msterp_engine_registry", mode = "function")) {
    msterp_engine_registry()
  } else {
    NULL
  }

  # Run worker
  if (isTRUE(use_callr) && requireNamespace("callr", quietly = TRUE)) {
    # Background execution
    terpbook_log(run_root, "Starting background worker (callr)")

    callr_handle <- callr::r_bg(
      func = function(run_root, terpbase, formatted_obj, pipeline, registry) {
        # Source required functions in background process
        # This assumes all functions are available in the package/environment
        msterp_terpbook_worker(run_root, terpbase, formatted_obj, pipeline, registry)
      },
      args = list(
        run_root = run_root,
        terpbase = terpbase,
        formatted_obj = formatted_obj,
        pipeline = pipeline,
        registry = registry
      ),
      supervise = TRUE
    )

    result <- list(
      run_root = run_root,
      manifest = manifest_path,
      progress = progress_path,
      log = log_path,
      callr_handle = callr_handle
    )

    if (isTRUE(wait)) {
      # Wait for completion
      callr_handle$wait()
      if (callr_handle$get_exit_status() != 0) {
        stop("Worker process failed")
      }
    }

  } else {
    # Synchronous execution
    terpbook_log(run_root, "Starting synchronous worker")

    msterp_terpbook_worker(run_root, terpbase, formatted_obj, pipeline, registry)

    result <- list(
      run_root = run_root,
      manifest = manifest_path,
      progress = progress_path,
      log = log_path,
      callr_handle = NULL
    )
  }

  result
}
