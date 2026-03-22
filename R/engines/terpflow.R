# R/engines/terpflow.R
# ============================================================
# .terpflow schema + helper functions
# ============================================================
#   Engine registry is defined ONLY in R/engines/registry.R
#   (msterp_engine_registry / msterp_engine_get / msterp_engine_ids)
# ============================================================
#
# STYLE STORAGE POLICY:
#   - .terpflow stores compute-time (params_schema) and style defaults (style_schema + viewer_schema)
#   - Style fields are "Pipeline Advanced Options" + Results Viewer defaults
#   - At run time, style is written to step.json as the run's viewing defaults
#   - Result Viewer reads step.json style as defaults, render_state.json for user overrides
#
# This enables:
#   - Pipeline Advanced Options to persist with each run output
#   - Result Viewer to show Pipeline-configured defaults for that run
#   - User can still override in Result Viewer (saved to render_state.json)
#   - Backward compatibility: old runs without style use registry defaults
#
# ============================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

msterp_make_id <- function(prefix = "id") {
  ts <- format(Sys.time(), "%Y%m%d%H%M%OS3")
  rnd <- sprintf("%08x", as.integer(stats::runif(1, 0, 2^31 - 1)))
  paste0(prefix, "_", ts, "_", rnd)
}

# Resolve registry lazily (keeps terpflow independent of source order until runtime)
msterp_resolve_registry <- function(registry = NULL) {
  if (!is.null(registry)) return(registry)
  if (!exists("msterp_engine_registry", mode = "function")) {
    stop("msterp_engine_registry() not found. Source R/engines/registry.R before using terpflow helpers.")
  }
  msterp_engine_registry()
}

# Registry-safe helpers
msterp_engine_ids_safe <- function(registry) {
  if (exists("msterp_engine_ids", mode = "function")) return(msterp_engine_ids(registry))
  names(registry$engines %||% list()) %||% character()
}

msterp_engine_get_safe <- function(engine_id, registry) {
  if (exists("msterp_engine_get", mode = "function")) return(msterp_engine_get(engine_id, registry))
  (registry$engines %||% list())[[engine_id]]
}

# Container defaults
msterp_idquant_default_substep_engine_ids <- function() {
  c(
    "idquant_id_quant",
    "idquant_average_value",
    "idquant_cv_scatter",
    "idquant_cv_bar",
    "idquant_overlap_detected",
    "idquant_overlap_quantified"
  )
}

# -----------------------------
# Schema helpers
# -----------------------------

# Accept schema as:
# 1) list(fields = list(field, field, ...))
# 2) list(field, field, ...)  (registry style)
# 3) JSON string of either of the above
msterp_schema_fields <- function(schema) {
  if (is.null(schema)) return(list())
  
  # JSON string support
  if (is.character(schema) && length(schema) == 1) {
    s <- trimws(schema)
    if (nzchar(s) && (startsWith(s, "{") || startsWith(s, "[")) &&
        requireNamespace("jsonlite", quietly = TRUE)) {
      parsed <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
      if (is.list(parsed)) schema <- parsed
    }
  }
  
  if (is.list(schema) && !is.null(schema$fields) && is.list(schema$fields)) {
    schema <- schema$fields
  }
  
  if (!is.list(schema) || length(schema) == 0) return(list())
  
  out <- list()
  
  for (el in schema) {
    # Field object
    if (is.list(el) && !is.null(el$name) && !is.null(el$type)) {
      out[[length(out) + 1L]] <- el
      next
    }
    
    # Nested list of field objects (defensive)
    if (is.list(el) && length(el) > 0) {
      looks_like_fields <- all(vapply(el, function(z) {
        is.list(z) && !is.null(z$name) && !is.null(z$type)
      }, logical(1)))
      
      if (isTRUE(looks_like_fields)) {
        out <- c(out, el)
      }
    }
  }
  
  out
}

msterp_engine_style_schema_all <- function(eng) {
  c(eng$style_schema %||% list(), eng$viewer_schema %||% list())
}

msterp_schema_defaults <- function(schema) {
  fields <- msterp_schema_fields(schema)
  if (length(fields) == 0) return(list())

  out <- list()
  for (f in fields) {
    if (!is.list(f) || is.null(f$name)) next
    if (!is.null(f$default)) out[[f$name]] <- f$default
  }
  out
}

msterp_merge_defaults <- function(defaults, values) {

  # Merge defaults with user values: defaults provide missing keys, values override
  if (is.null(defaults) || length(defaults) == 0) return(values %||% list())
  if (is.null(values) || length(values) == 0) return(defaults)
  modifyList(defaults, values)
}

#' Combined sanitize + validate in single pass (performance optimization)
#'
#' Reduces schema field iterations from 4+ passes to 1 pass per schema.
#' Handles deprecated param migration, unknown param removal, invalid choice
#' correction, and type validation all in one loop.
#'
#' @param schema Schema to validate against
#' @param values User-provided values
#' @param engine_id Engine ID for logging context
#' @return List with sanitized values, warnings, errors, and ok flag
msterp_schema_sanitize_and_validate <- function(schema, values, engine_id = NULL) {
  fields <- msterp_schema_fields(schema)
  warnings <- character()
  errs <- character()

  if (is.null(values)) values <- list()
  if (!is.list(values)) values <- list()

  ctx <- if (!is.null(engine_id)) paste0("[", engine_id, "] ") else ""


  # MIGRATION: sig_field -> apply_fdr (Volcano engine)
  if ("sig_field" %in% names(values) && !"apply_fdr" %in% names(values)) {
    sig_val <- values[["sig_field"]]
    if (identical(sig_val, "padj")) {
      values[["apply_fdr"]] <- TRUE
      warnings <- c(warnings, sprintf("%sMigrated deprecated 'sig_field: padj' to 'apply_fdr: TRUE'", ctx))
    } else if (identical(sig_val, "pval")) {
      values[["apply_fdr"]] <- FALSE
      warnings <- c(warnings, sprintf("%sMigrated deprecated 'sig_field: pval' to 'apply_fdr: FALSE'", ctx))
    } else {
      values[["apply_fdr"]] <- TRUE
      warnings <- c(warnings, sprintf("%sDeprecated 'sig_field' has invalid value '%s'; defaulting to 'apply_fdr: TRUE'", ctx, as.character(sig_val)))
    }
    values[["sig_field"]] <- NULL
  }

  # Build field lookup once (single pass to build lookup)
  schema_names <- character(length(fields))
  field_lookup <- vector("list", length(fields))
  for (i in seq_along(fields)) {
    f <- fields[[i]]
    if (is.list(f) && !is.null(f$name)) {
      nm <- f$name
      schema_names[i] <- nm
      field_lookup[[i]] <- f
      names(field_lookup)[i] <- nm
    }
  }
  schema_names <- schema_names[nzchar(schema_names)]

  # Remove unknown params
  unknown_params <- setdiff(names(values), schema_names)
  if (length(unknown_params) > 0) {
    warnings <- c(warnings, sprintf("%sIgnoring deprecated/unknown param(s): %s", ctx, paste(unknown_params, collapse = ", ")))
    values <- values[!names(values) %in% unknown_params]
  }

  # Single pass: sanitize choices AND validate types
  for (nm in names(values)) {
    f <- field_lookup[[nm]]
    if (is.null(f)) next

    v <- values[[nm]]
    if (is.null(v)) next

    type <- as.character(f$type)

    # Choice validation + sanitization
    if (identical(type, "choice")) {
      ch <- f$choices %||% NULL
      if (!is.null(ch) && length(ch) > 0 && !is.na(v)) {
        vv <- as.character(v)
        if (!vv %in% as.character(ch)) {
          default_val <- f$default %||% ch[[1]]
          warnings <- c(warnings, sprintf("%sParam '%s' has invalid value '%s' (not in choices); reverting to default '%s'", ctx, nm, vv, default_val))
          values[[nm]] <- default_val
        }
      }
    }

    # Bool validation
    else if (identical(type, "bool")) {
      if (!is.logical(v) || length(v) != 1) {
        errs <- c(errs, sprintf("'%s' must be logical.", nm))
      }
    }

    # Numeric validation (int/num)
    # Note: Allow single NA values (e.g., from JSON null) - they represent "no value"
    else if (type %in% c("int", "num")) {
      if (!is.numeric(v) && !(length(v) == 1 && is.na(v))) {
        errs <- c(errs, sprintf("'%s' must be numeric.", nm))
      } else {
        if (identical(type, "int")) {
          bad_int <- any(!is.na(v) & (v != as.integer(v)))
          if (isTRUE(bad_int)) errs <- c(errs, sprintf("'%s' must be integer.", nm))
        }
        if (!is.null(f$min)) {
          bad_min <- any(!is.na(v) & (v < f$min))
          if (isTRUE(bad_min)) errs <- c(errs, sprintf("'%s' must be >= %s.", nm, f$min))
        }
        if (!is.null(f$max)) {
          bad_max <- any(!is.na(v) & (v > f$max))
          if (isTRUE(bad_max)) errs <- c(errs, sprintf("'%s' must be <= %s.", nm, f$max))
        }
      }
    }

    # Range validation
    else if (identical(type, "range")) {
      if (!is.numeric(v) || length(v) != 2) {
        errs <- c(errs, sprintf("'%s' must be a numeric vector of length 2.", nm))
      } else {
        if (any(is.na(v))) errs <- c(errs, sprintf("'%s' cannot contain NA.", nm))
        if (!is.null(f$min) && any(v < f$min, na.rm = TRUE)) {
          errs <- c(errs, sprintf("'%s' must be >= %s.", nm, f$min))
        }
        if (!is.null(f$max) && any(v > f$max, na.rm = TRUE)) {
          errs <- c(errs, sprintf("'%s' must be <= %s.", nm, f$max))
        }
        if (!any(is.na(v)) && v[[1]] > v[[2]]) {
          errs <- c(errs, sprintf("'%s' must be ordered (min <= max).", nm))
        }
      }
    }

    # String validation
    else if (identical(type, "string")) {
      if (!is.character(v)) errs <- c(errs, sprintf("'%s' must be character.", nm))
    }
  }

  list(values = values, warnings = warnings, errors = errs, ok = length(errs) == 0)
}

msterp_schema_validate <- function(schema, values) {
  fields <- msterp_schema_fields(schema)
  errs <- character()
  
  if (is.null(values)) values <- list()
  if (!is.list(values)) values <- list()
  
  for (f in fields) {
    if (!is.list(f) || is.null(f$name) || is.null(f$type)) next
    nm <- f$name
    if (!nm %in% names(values)) next
    
    v <- values[[nm]]
    if (is.null(v)) next
    
    type <- as.character(f$type)
    
    if (identical(type, "choice")) {
      ch <- f$choices %||% NULL
      if (!is.null(ch) && length(ch) > 0 && !is.na(v)) {
        vv <- as.character(v)
        if (!vv %in% as.character(ch)) {
          errs <- c(errs, sprintf("'%s' must be one of: %s", nm, paste(ch, collapse = ", ")))
        }
      }
    }
    
    if (identical(type, "bool")) {
      if (!is.logical(v) || length(v) != 1) {
        errs <- c(errs, sprintf("'%s' must be logical.", nm))
      }
    }
    
    # Note: Allow single NA values (e.g., from JSON null) - they represent "no value"
    if (type %in% c("int", "num")) {
      if (!is.numeric(v) && !(length(v) == 1 && is.na(v))) {
        errs <- c(errs, sprintf("'%s' must be numeric.", nm))
      } else if (is.numeric(v)) {
        if (identical(type, "int")) {
          bad_int <- any(!is.na(v) & (v != as.integer(v)))
          if (isTRUE(bad_int)) errs <- c(errs, sprintf("'%s' must be integer.", nm))
        }
        if (!is.null(f$min)) {
          bad_min <- any(!is.na(v) & (v < f$min))
          if (isTRUE(bad_min)) errs <- c(errs, sprintf("'%s' must be >= %s.", nm, f$min))
        }
        if (!is.null(f$max)) {
          bad_max <- any(!is.na(v) & (v > f$max))
          if (isTRUE(bad_max)) errs <- c(errs, sprintf("'%s' must be <= %s.", nm, f$max))
        }
      }
    }

    # range validation (numeric length-2 vector)
    if (identical(type, "range")) {
      if (!is.numeric(v) || length(v) != 2) {
        errs <- c(errs, sprintf("'%s' must be a numeric vector of length 2.", nm))
      } else {
        if (any(is.na(v))) errs <- c(errs, sprintf("'%s' cannot contain NA.", nm))
        if (!is.null(f$min) && any(v < f$min, na.rm = TRUE)) {
          errs <- c(errs, sprintf("'%s' must be >= %s.", nm, f$min))
        }
        if (!is.null(f$max) && any(v > f$max, na.rm = TRUE)) {
          errs <- c(errs, sprintf("'%s' must be <= %s.", nm, f$max))
        }
        if (!any(is.na(v)) && v[[1]] > v[[2]]) {
          errs <- c(errs, sprintf("'%s' must be ordered (min <= max).", nm))
        }
      }
    }
    
    if (identical(type, "string")) {
      if (!is.character(v)) errs <- c(errs, sprintf("'%s' must be character.", nm))
    }
  }
  
  list(ok = length(errs) == 0, errors = errs)
}

#' Sanitize params by removing deprecated/unknown values
#'
#' Removes params not in schema and fixes invalid choice values by reverting to defaults.
#' Logs warnings for any sanitized values.
#'
#' @param schema Schema to validate against
#' @param values User-provided values
#' @param engine_id Engine ID for logging context
#' @return List with sanitized values and warnings
msterp_schema_sanitize_deprecated <- function(schema, values, engine_id = NULL) {
  fields <- msterp_schema_fields(schema)
  warnings <- character()

  if (is.null(values)) values <- list()
  if (!is.list(values)) values <- list()

  # -------------------------------------------------------------------

  # MIGRATION: sig_field → apply_fdr (Volcano engine)
  # sig_field="padj" → apply_fdr=TRUE; sig_field="pval" → apply_fdr=FALSE
  # Missing/invalid sig_field defaults to apply_fdr=TRUE with warning
  # -------------------------------------------------------------------
  if ("sig_field" %in% names(values) && !"apply_fdr" %in% names(values)) {
    sig_val <- values[["sig_field"]]
    ctx <- if (!is.null(engine_id)) paste0("[", engine_id, "] ") else ""
    if (identical(sig_val, "padj")) {
      values[["apply_fdr"]] <- TRUE
      warnings <- c(warnings, sprintf(
        "%sMigrated deprecated 'sig_field: padj' to 'apply_fdr: TRUE'", ctx
      ))
    } else if (identical(sig_val, "pval")) {
      values[["apply_fdr"]] <- FALSE
      warnings <- c(warnings, sprintf(
        "%sMigrated deprecated 'sig_field: pval' to 'apply_fdr: FALSE'", ctx
      ))
    } else {
      # Invalid or missing value: default to TRUE with warning
      values[["apply_fdr"]] <- TRUE
      warnings <- c(warnings, sprintf(
        "%sDeprecated 'sig_field' has invalid value '%s'; defaulting to 'apply_fdr: TRUE'",
        ctx, as.character(sig_val)
      ))
    }
    # Remove sig_field so it doesn't trigger "unknown param" warning
    values[["sig_field"]] <- NULL
  }
  # -------------------------------------------------------------------

  # Build lookup of valid field names and their properties
  schema_names <- vapply(fields, function(f) f$name %||% "", character(1))
  schema_names <- schema_names[nzchar(schema_names)]

  # Find and remove unknown params
  unknown_params <- setdiff(names(values), schema_names)
  if (length(unknown_params) > 0) {
    ctx <- if (!is.null(engine_id)) paste0("[", engine_id, "] ") else ""
    warnings <- c(warnings, sprintf(
      "%sIgnoring deprecated/unknown param(s): %s",
      ctx, paste(unknown_params, collapse = ", ")
    ))
    values <- values[!names(values) %in% unknown_params]
  }

  # Check for invalid choice values and revert to defaults
  for (f in fields) {
    if (!is.list(f) || is.null(f$name) || is.null(f$type)) next
    nm <- f$name
    if (!nm %in% names(values)) next

    v <- values[[nm]]
    if (is.null(v)) next

    type <- as.character(f$type)

    if (identical(type, "choice")) {
      ch <- f$choices %||% NULL
      if (!is.null(ch) && length(ch) > 0 && !is.na(v)) {
        vv <- as.character(v)
        if (!vv %in% as.character(ch)) {
          # Invalid choice: revert to default
          default_val <- f$default %||% ch[[1]]
          ctx <- if (!is.null(engine_id)) paste0("[", engine_id, "] ") else ""
          warnings <- c(warnings, sprintf(
            "%sParam '%s' has invalid value '%s' (not in choices); reverting to default '%s'",
            ctx, nm, vv, default_val
          ))
          values[[nm]] <- default_val
        }
      }
    }
  }

  list(values = values, warnings = warnings)
}

msterp_valid_analysis_levels_safe <- function() {
  if (exists("nr_valid_analysis_levels", mode = "function")) {
    lvls <- tryCatch(nr_valid_analysis_levels(), error = function(e) NULL)
    if (is.character(lvls) && length(lvls) > 0) return(unique(tolower(trimws(lvls))))
  }
  c("protein", "peptide")
}

msterp_normalize_analysis_level_safe <- function(level) {
  if (is.null(level) || length(level) == 0) return(NULL)
  lvl <- tolower(trimws(as.character(level)[1]))
  if (!nzchar(lvl)) return(NULL)
  if (!lvl %in% msterp_valid_analysis_levels_safe()) return(NULL)
  lvl
}


# ---------- Param key migration for data-type refactor ----------

#' Migrate legacy param keys in all steps of a terpflow object
#' Maps: gene_list → target_list, genes → targets,
#'        label_genes_map → label_targets_map, selected_gene → selected_target
#' @noRd
.terpflow_migrate_param_keys <- function(obj) {
  if (is.null(obj$steps) || !is.list(obj$steps) || length(obj$steps) == 0) return(obj)

  key_map <- list(
    gene_list        = "target_list",
    genes            = "targets",
    label_genes_map  = "label_targets_map",
    selected_target  = "selected_gene"
  )

  migrate_params <- function(params) {
    if (is.null(params) || !is.list(params)) return(params)
    for (old_key in names(key_map)) {
      new_key <- key_map[[old_key]]
      if (!is.null(params[[old_key]]) && is.null(params[[new_key]])) {
        params[[new_key]] <- params[[old_key]]
      }
    }
    params
  }

  for (i in seq_along(obj$steps)) {
    s <- obj$steps[[i]]
    if (!is.null(s$params)) s$params <- migrate_params(s$params)
    if (!is.null(s$paired) && !is.null(s$paired$params)) {
      s$paired$params <- migrate_params(s$paired$params)
    }
    if (!is.null(s$substeps) && is.list(s$substeps)) {
      for (j in seq_along(s$substeps)) {
        if (!is.null(s$substeps[[j]]$params)) {
          s$substeps[[j]]$params <- migrate_params(s$substeps[[j]]$params)
        }
      }
    }
    obj$steps[[i]] <- s
  }

  obj
}


# ---------- .terpflow create/edit ----------

msterp_terpflow_new <- function(pipeline_name = "New Pipeline", pipeline_type = "proteomics", registry = NULL) {
  registry <- msterp_resolve_registry(registry)

  # Validate pipeline_type (ensure single value)
  valid_types <- c("proteomics", "metabolomics", "multi")
  pipeline_type <- pipeline_type[[1]]  # Take first element if vector

  if (!pipeline_type %in% valid_types) pipeline_type <- "proteomics"

  list(
    schema_version = 2L,
    pipeline_id = msterp_make_id("terpflow"),
    pipeline_name = pipeline_name,
    pipeline_type = pipeline_type,
    created = Sys.time(),
    registry_version = registry$registry_version %||% registry$version %||% NA,
    sections = list(
      list(
        section_id = msterp_make_id("sec"),
        title = "Section 1",
        description = "",
        order = 1L
      )
    ),
    steps = list()
  )
}

msterp_terpflow_add_section <- function(flow, title, description = "") {
  stopifnot(is.list(flow), !is.null(flow$sections))
  next_order <- if (length(flow$sections) == 0) 1L else max(vapply(flow$sections, `[[`, integer(1), "order")) + 1L
  flow$sections[[length(flow$sections) + 1L]] <- list(
    section_id = msterp_make_id("sec"),
    title = title,
    description = description,
    order = next_order
  )
  flow
}

msterp_terpflow_add_step <- function(flow, section_id, engine_id, registry = NULL) {
  registry <- msterp_resolve_registry(registry)

  eng <- msterp_engine_get_safe(engine_id, registry)
  if (is.null(eng)) stop("Unknown engine_id: ", engine_id)

  next_order <- if (length(flow$steps) == 0) 1L else max(vapply(flow$steps, `[[`, integer(1), "order")) + 1L

  # COMPUTE-ONLY: Only store params_schema defaults, NOT style_schema
  # Style defaults are applied at render time from registry
  params <- msterp_schema_defaults(eng$params_schema)

  step <- list(
    step_id = msterp_make_id("step"),
    section_id = section_id,
    order = next_order,
    engine_id = engine_id,
    enabled = TRUE,
    params = params,
    # NOTE: style intentionally omitted - comes from registry at render time
    paired = NULL,
    sequential = NULL
  )
  if (as.integer(flow$schema_version %||% 1L) >= 2L) {
    step$type <- eng$type %||% "engine"
    if (identical(step$type, "container")) {
      step$substeps <- list()

      if (identical(engine_id, "idquant")) {
        child_eids <- msterp_idquant_default_substep_engine_ids()
        lock_substeps <- isTRUE(eng$locked_parent %||% FALSE)
        for (k in seq_along(child_eids)) {
          child_eng <- msterp_engine_get_safe(child_eids[[k]], registry)
          if (is.null(child_eng)) stop("Unknown child engine_id: ", child_eids[[k]])
          step$substeps[[k]] <- list(
            step_id = msterp_make_id("substep"),
            order = as.integer(k),
            engine_id = child_eids[[k]],
            type = "engine",
            enabled = TRUE,
            params = msterp_schema_defaults(child_eng$params_schema),
            paired = NULL,
            sequential = NULL,
            system_generated = lock_substeps
          )
        }
      }

      forced_eid <- eng$forced_final_substep_engine_id %||% NULL
      if (!is.null(forced_eid) && nzchar(forced_eid)) {
        forced_eng <- msterp_engine_get_safe(forced_eid, registry)
        if (!is.null(forced_eng)) {
          step$substeps[[1]] <- list(
            step_id = msterp_make_id("substep"),
            order = 1L,
            engine_id = forced_eid,
            type = "engine",
            enabled = TRUE,
            params = msterp_schema_defaults(forced_eng$params_schema),
            paired = NULL,
            sequential = NULL,
            system_generated = TRUE
          )
        }
      }

    }
  }
  flow$steps[[length(flow$steps) + 1L]] <- step
  flow
}

msterp_terpflow_add_substep <- function(flow, parent_step_id, engine_id, registry = NULL) {
  registry <- msterp_resolve_registry(registry)
  if (as.integer(flow$schema_version %||% 1L) < 2L) stop("Substeps require schema_version >= 2.")

  pi <- which(vapply(flow$steps, function(s) identical(s$step_id, parent_step_id), logical(1)))
  if (length(pi) == 0) stop("Parent step not found: ", parent_step_id)

  parent <- flow$steps[[pi]]
  if (!identical(parent$type %||% "engine", "container")) stop("Parent step is not a container: ", parent_step_id)
  if (is.null(parent$substeps) || !is.list(parent$substeps)) parent$substeps <- list()

  # Enforce fixed substeps for specific locked containers (currently: idquant).
  if (identical(parent$engine_id, "idquant")) return(flow)

  eng <- msterp_engine_get_safe(engine_id, registry)
  if (is.null(eng)) stop("Unknown engine_id: ", engine_id)
  if (identical(eng$type %||% "engine", "container")) stop("Nested containers are not supported.")

  sub <- list(
    step_id = msterp_make_id("substep"),
    engine_id = engine_id,
    type = "engine",
    enabled = TRUE,
    params = msterp_schema_defaults(eng$params_schema),
    paired = NULL,
    sequential = NULL
  )

  # Insert before trailing system_generated substeps (e.g., forced aggregation).
  substeps <- parent$substeps %||% list()
  is_sys <- vapply(substeps, function(x) isTRUE((x %||% list())$system_generated), logical(1))
  insert_at <- if (length(substeps) == 0) 1L else (max(which(!is_sys), 0L) + 1L)
  if (insert_at < 1L) insert_at <- 1L
  if (insert_at > length(substeps) + 1L) insert_at <- length(substeps) + 1L

  parent$substeps <- append(substeps, list(sub), after = insert_at - 1L)
  for (k in seq_along(parent$substeps)) parent$substeps[[k]]$order <- k
  flow$steps[[pi]] <- parent
  flow
}

msterp_terpflow_remove_substep <- function(flow, parent_step_id, substep_id) {
  pi <- which(vapply(flow$steps, function(s) identical(s$step_id, parent_step_id), logical(1)))
  if (length(pi) == 0) return(flow)

  parent <- flow$steps[[pi]]
  if (!identical(parent$type %||% "engine", "container")) return(flow)
  subs <- parent$substeps %||% list()
  if (length(subs) == 0) return(flow)

  si <- which(vapply(subs, function(s) identical(s$step_id, substep_id), logical(1)))
  if (length(si) == 0) return(flow)
  if (isTRUE((subs[[si]]$system_generated %||% FALSE))) return(flow)

  parent$substeps <- subs[-si]
  for (k in seq_along(parent$substeps)) parent$substeps[[k]]$order <- k
  flow$steps[[pi]] <- parent
  flow
}

msterp_terpflow_move_substep <- function(flow, parent_step_id, substep_id, direction = c("up", "down")) {
  direction <- match.arg(direction)
  pi <- which(vapply(flow$steps, function(s) identical(s$step_id, parent_step_id), logical(1)))
  if (length(pi) == 0) return(flow)

  parent <- flow$steps[[pi]]
  if (!identical(parent$type %||% "engine", "container")) return(flow)
  subs <- parent$substeps %||% list()
  if (length(subs) < 2) return(flow)

  subs <- subs[order(vapply(subs, `[[`, integer(1), "order"))]
  si <- which(vapply(subs, function(s) identical(s$step_id, substep_id), logical(1)))
  if (length(si) == 0) return(flow)

  if (isTRUE((subs[[si]]$system_generated %||% FALSE))) return(flow)

  sj <- if (direction == "up") si - 1 else si + 1
  if (sj < 1 || sj > length(subs)) return(flow)
  if (isTRUE((subs[[sj]]$system_generated %||% FALSE))) return(flow)

  tmp <- subs[[si]]
  subs[[si]] <- subs[[sj]]
  subs[[sj]] <- tmp
  for (k in seq_along(subs)) subs[[k]]$order <- k

  parent$substeps <- subs
  flow$steps[[pi]] <- parent
  flow
}

msterp_terpflow_remove_step <- function(flow, step_id) {
  idx <- which(vapply(flow$steps, function(s) identical(s$step_id, step_id), logical(1)))
  if (length(idx) == 0) return(flow)
  
  flow$steps <- flow$steps[-idx]
  
  if (length(flow$steps) > 0) {
    ord <- order(vapply(flow$steps, `[[`, integer(1), "order"))
    flow$steps <- flow$steps[ord]
    for (k in seq_along(flow$steps)) flow$steps[[k]]$order <- k
  }
  
  flow
}

msterp_terpflow_move_step <- function(flow, step_id, direction = c("up", "down")) {
  direction <- match.arg(direction)
  if (length(flow$steps) < 2) return(flow)
  
  ord <- order(vapply(flow$steps, `[[`, integer(1), "order"))
  flow$steps <- flow$steps[ord]
  
  i <- which(vapply(flow$steps, function(s) identical(s$step_id, step_id), logical(1)))
  if (length(i) == 0) return(flow)
  
  j <- if (direction == "up") i - 1 else i + 1
  if (j < 1 || j > length(flow$steps)) return(flow)
  
  tmp <- flow$steps[[i]]
  flow$steps[[i]] <- flow$steps[[j]]
  flow$steps[[j]] <- tmp
  
  for (k in seq_along(flow$steps)) flow$steps[[k]]$order <- k
  flow
}

# IMPORTANT FIX:
# - Use missing() so calling update_step(..., paired=...) doesn't auto-clear sequential
# NOTE: style is now stored in .terpflow to support Pipeline Advanced Options
# that persist as Result Viewer defaults (written to step.json at run time)
msterp_terpflow_update_step <- function(flow, step_id,
                                        enabled = NULL, params = NULL, style = NULL,
                                        paired = NULL, sequential = NULL) {
  i <- which(vapply(flow$steps, function(s) identical(s$step_id, step_id), logical(1)))
  if (length(i) == 0) return(flow)

  if (!is.null(enabled)) flow$steps[[i]]$enabled <- isTRUE(enabled)
  if (!is.null(params))  flow$steps[[i]]$params <- params
  if (!is.null(style))   flow$steps[[i]]$style <- style

  if (!missing(paired))     flow$steps[[i]]$paired     <- paired
  if (!missing(sequential)) flow$steps[[i]]$sequential <- sequential

  flow
}

msterp_terpflow_validate <- function(flow, registry = NULL) {
  registry <- msterp_resolve_registry(registry)

  errs <- character()
  warns <- character()

  if (!is.list(flow)) errs <- c(errs, "Flow is not a list.")
  if (is.null(flow$schema_version) || !is.numeric(flow$schema_version)) errs <- c(errs, "Missing schema_version.")
  if (is.null(flow$pipeline_id) || !nzchar(flow$pipeline_id)) errs <- c(errs, "Missing pipeline_id.")
  if (is.null(flow$sections) || !is.list(flow$sections)) errs <- c(errs, "Missing sections list.")
  if (is.null(flow$steps) || !is.list(flow$steps)) errs <- c(errs, "Missing steps list.")

  if (length(errs) > 0) return(list(ok = FALSE, errors = errs, warnings = warns))

  schema_version <- as.integer(flow$schema_version[1])
  if (!schema_version %in% c(1L, 2L)) {
    errs <- c(errs, sprintf("Unsupported schema_version: %s (expected 1 or 2).", as.character(flow$schema_version)))
    return(list(ok = FALSE, errors = errs, warnings = warns))
  }

  # Validate pipeline_type (backwards compatibility: default to proteomics)
  valid_types <- c("proteomics", "metabolomics", "multi")
  pt <- flow$pipeline_type[[1]] %||% NULL  # Ensure single value
  if (is.null(pt) || !pt %in% valid_types) {
    if (!is.null(flow$pipeline_type)) {
      warns <- c(warns, sprintf("Invalid pipeline_type: %s. Defaulting to 'proteomics'.", as.character(flow$pipeline_type)))
    } else {
      # Missing pipeline_type: warn about legacy file
      warns <- c(warns, "Missing pipeline_type; defaulting to 'proteomics'")
    }
    flow$pipeline_type <- "proteomics"
  }

  engine_ids <- msterp_engine_ids_safe(registry)

  validate_engine_step <- function(s, prefix) {
    if (!is.list(s)) {
      errs <<- c(errs, sprintf("%s is not a list.", prefix))
      return(invisible(FALSE))
    }

    if (schema_version < 2L) {
      if (!is.null(s$type) || !is.null(s$analysis_level) || !is.null(s$substeps) || isTRUE(s$system_generated)) {
        errs <<- c(errs, sprintf("%s uses v2-only fields but schema_version is %d.", prefix, schema_version))
      }
    }

    step_type <- s$type %||% "engine"
    if (!step_type %in% c("engine", "container")) {
      errs <<- c(errs, sprintf("%s has invalid type: %s", prefix, as.character(step_type)))
      step_type <- "engine"
    }

    if (!is.null(s$analysis_level)) {
      lvl <- msterp_normalize_analysis_level_safe(s$analysis_level)
      if (is.null(lvl)) {
        errs <<- c(errs, sprintf("%s has invalid analysis_level: %s", prefix, as.character(s$analysis_level %||% "")))
      }
    }

    if (!is.null(s$system_generated) && !is.logical(s$system_generated)) {
      errs <<- c(errs, sprintf("%s system_generated must be logical.", prefix))
    }

    if (is.null(s$engine_id) || !nzchar(as.character(s$engine_id))) {
      errs <<- c(errs, sprintf("%s missing engine_id.", prefix))
      return(invisible(FALSE))
    }
    if (!s$engine_id %in% engine_ids) {
      errs <<- c(errs, sprintf("%s has unknown engine_id: %s", prefix, s$engine_id))
      return(invisible(FALSE))
    }

    eng <- msterp_engine_get_safe(s$engine_id, registry)
    if (is.null(eng)) {
      errs <<- c(errs, sprintf("%s missing engine definition: %s", prefix, s$engine_id))
      return(invisible(FALSE))
    }

    # Combined sanitize + validate in single pass (performance optimization)
    p_result <- msterp_schema_sanitize_and_validate(eng$params_schema, s$params %||% list(), s$engine_id)
    if (length(p_result$warnings) > 0) warns <<- c(warns, p_result$warnings)
    if (!p_result$ok) errs <<- c(errs, paste0(prefix, " params: ", p_result$errors))

    style_schema <- msterp_engine_style_schema_all(eng)
    s_result <- msterp_schema_sanitize_and_validate(style_schema, s$style %||% list(), s$engine_id)
    if (length(s_result$warnings) > 0) warns <<- c(warns, s_result$warnings)
    if (!s_result$ok) errs <<- c(errs, paste0(prefix, " style: ", s_result$errors))

    if (!is.null(s$paired)) {
      if (!is.list(s$paired)) {
        errs <<- c(errs, sprintf("%s paired must be a list or NULL.", prefix))
      } else {
        if (is.list(s$paired$engines)) {
          for (peid in names(s$paired$engines)) {
            if (!nzchar(peid) || !peid %in% engine_ids) {
              errs <<- c(errs, sprintf("%s has invalid paired engine id in engines: %s", prefix, peid))
              next
            }
            peng <- msterp_engine_get_safe(peid, registry)
            if (is.null(peng)) {
              errs <<- c(errs, sprintf("%s missing paired engine definition: %s", prefix, peid))
              next
            }
            cfg <- s$paired$engines[[peid]] %||% list()
            pp_result <- msterp_schema_sanitize_and_validate(peng$params_schema, cfg$params %||% list(), peid)
            if (length(pp_result$warnings) > 0) warns <<- c(warns, pp_result$warnings)
            if (!pp_result$ok) errs <<- c(errs, paste0(prefix, " paired(", peid, ") params: ", pp_result$errors))

            paired_style_schema <- msterp_engine_style_schema_all(peng)
            ps_result <- msterp_schema_sanitize_and_validate(paired_style_schema, cfg$style %||% list(), peid)
            if (length(ps_result$warnings) > 0) warns <<- c(warns, ps_result$warnings)
            if (!ps_result$ok) errs <<- c(errs, paste0(prefix, " paired(", peid, ") style: ", ps_result$errors))
          }

          peid0 <- s$paired$engine_id %||% NULL
          if (!is.null(peid0) && nzchar(peid0) && !peid0 %in% engine_ids) {
            errs <<- c(errs, sprintf("%s has invalid paired engine_id: %s", prefix, peid0))
          }
        } else {
          peid <- s$paired$engine_id %||% NULL
          if (is.null(peid) || !nzchar(peid) || !peid %in% engine_ids) {
            errs <<- c(errs, sprintf("%s has invalid paired engine_id: %s", prefix, as.character(peid %||% "")))
          } else {
            peng <- msterp_engine_get_safe(peid, registry)
            if (is.null(peng)) {
              errs <<- c(errs, sprintf("%s missing paired engine definition: %s", prefix, peid))
            } else {
              pp_result <- msterp_schema_sanitize_and_validate(peng$params_schema, s$paired$params %||% list(), peid)
              if (length(pp_result$warnings) > 0) warns <<- c(warns, pp_result$warnings)
              if (!pp_result$ok) errs <<- c(errs, paste0(prefix, " paired params: ", pp_result$errors))

              paired_style_schema <- msterp_engine_style_schema_all(peng)
              ps_result <- msterp_schema_sanitize_and_validate(paired_style_schema, s$paired$style %||% list(), peid)
              if (length(ps_result$warnings) > 0) warns <<- c(warns, ps_result$warnings)
              if (!ps_result$ok) errs <<- c(errs, paste0(prefix, " paired style: ", ps_result$errors))
            }
          }
        }
      }
    }

    if (!is.null(s$sequential) && !isTRUE(eng$supports_sequential)) {
      warns <<- c(warns, sprintf(
        "%s has sequential config but engine '%s' does not support sequential.",
        prefix, s$engine_id
      ))
    }

    step_type
  }

  for (i in seq_along(flow$steps)) {
    s <- flow$steps[[i]]
    prefix <- sprintf("Step[%d] %s", i, s$step_id %||% "")
    step_type <- validate_engine_step(s, prefix)

    if (identical(step_type, "container")) {
      if (is.null(s$substeps) || !is.list(s$substeps) || length(s$substeps) == 0) {
        errs <- c(errs, sprintf("%s container step must have non-empty substeps list.", prefix))
        next
      }
      for (j in seq_along(s$substeps)) {
        ss <- s$substeps[[j]]
        sub_prefix <- sprintf("%s substep[%d] %s", prefix, j, ss$step_id %||% "")
        if (!is.null(ss$substeps) && length(ss$substeps) > 0) {
          errs <- c(errs, sprintf("%s cannot itself have substeps (nested containers not supported).", sub_prefix))
          next
        }
        if (!is.null(ss$type) && !identical(ss$type, "engine")) {
          errs <- c(errs, sprintf("%s has invalid type: %s (substeps must be type='engine').", sub_prefix, as.character(ss$type)))
        }
        validate_engine_step(modifyList(ss, list(type = "engine")), sub_prefix)
      }
    } else {
      if (!is.null(s$substeps) && length(s$substeps) > 0) {
        errs <- c(errs, sprintf("%s is not a container but has substeps.", prefix))
      }
    }
  }

  list(ok = length(errs) == 0, errors = errs, warnings = warns)
}

msterp_terpflow_apply_step_defaults <- function(step, registry) {
  step_type <- step$type %||% "engine"

  eng <- msterp_engine_get_safe(step$engine_id, registry)
  if (!is.null(eng)) {
    step$params <- msterp_merge_defaults(msterp_schema_defaults(eng$params_schema), step$params %||% list())
    step$style <- msterp_merge_defaults(msterp_schema_defaults(msterp_engine_style_schema_all(eng)), step$style %||% list())
  }

  if (!is.null(step$paired) && is.list(step$paired)) {
    if (is.list(step$paired$engines)) {
      for (peid in names(step$paired$engines)) {
        peng <- msterp_engine_get_safe(peid, registry)
        if (is.null(peng)) next

        cfg <- step$paired$engines[[peid]] %||% list()
        cfg$enabled <- isTRUE(cfg$enabled %||% FALSE)
        cfg$params <- msterp_merge_defaults(msterp_schema_defaults(peng$params_schema), cfg$params %||% list())
        cfg$style <- msterp_merge_defaults(msterp_schema_defaults(msterp_engine_style_schema_all(peng)), cfg$style %||% list())
        step$paired$engines[[peid]] <- cfg
      }

      if (is.null(step$paired$engine_id) || !nzchar(step$paired$engine_id)) {
        enabled_ids <- names(Filter(function(x) isTRUE(x$enabled), step$paired$engines))
        step$paired$engine_id <- if (length(enabled_ids) > 0) enabled_ids[[1]] else names(step$paired$engines)[[1]] %||% NULL
      }
    } else {
      peid <- step$paired$engine_id %||% NULL
      peng <- if (!is.null(peid)) msterp_engine_get_safe(peid, registry) else NULL
      if (!is.null(peng)) {
        step$paired$enabled <- isTRUE(step$paired$enabled %||% FALSE)
        step$paired$params <- msterp_merge_defaults(msterp_schema_defaults(peng$params_schema), step$paired$params %||% list())
        step$paired$style <- msterp_merge_defaults(msterp_schema_defaults(msterp_engine_style_schema_all(peng)), step$paired$style %||% list())
      }
    }
  }

  if (identical(step_type, "container") && is.list(step$substeps) && length(step$substeps) > 0) {
    for (j in seq_along(step$substeps)) {
      step$substeps[[j]] <- msterp_terpflow_apply_step_defaults(step$substeps[[j]], registry)
    }
  }

  step
}

msterp_terpflow_save <- function(flow, path) {
  if (!grepl("\\.terpflow$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".terpflow")
  }
  saveRDS(flow, path)
  invisible(path)
}

msterp_terpflow_migrate_legacy_idquant_container <- function(flow, registry) {
  if (is.null(flow$steps) || !is.list(flow$steps) || length(flow$steps) == 0) return(flow)

  schema_version <- as.integer(flow$schema_version %||% 1L)
  if (schema_version < 2L) {
    has_idquant <- any(vapply(flow$steps, function(s) identical((s %||% list())$engine_id, "idquant"), logical(1)))
    if (isTRUE(has_idquant)) {
      flow$schema_version <- 2L
      schema_version <- 2L
    }
  }

  if (schema_version < 2L) return(flow)

  for (i in seq_along(flow$steps)) {
    step <- flow$steps[[i]] %||% list()
    if (!identical(step$engine_id %||% "", "idquant")) next

    step_type <- step$type %||% "engine"
    needs_container <- !identical(step_type, "container") ||
      is.null(step$substeps) || !is.list(step$substeps) || length(step$substeps) == 0

    if (!isTRUE(needs_container)) next

    child_eids <- msterp_idquant_default_substep_engine_ids()
    substeps <- vector("list", length(child_eids))
    for (k in seq_along(child_eids)) {
      child_eng <- msterp_engine_get_safe(child_eids[[k]], registry)
      if (is.null(child_eng)) stop("Unknown child engine_id: ", child_eids[[k]])
      substeps[[k]] <- list(
        step_id = msterp_make_id("substep"),
        order = as.integer(k),
        engine_id = child_eids[[k]],
        type = "engine",
        enabled = TRUE,
        params = msterp_schema_defaults(child_eng$params_schema),
        paired = NULL,
        sequential = NULL
      )
    }

    step$type <- "container"
    step$substeps <- substeps
    flow$steps[[i]] <- step
  }

  flow
}

msterp_terpflow_migrate_legacy_idquant_cv_split <- function(flow, registry) {
  if (is.null(flow$steps) || !is.list(flow$steps) || length(flow$steps) == 0) return(flow)

  for (i in seq_along(flow$steps)) {
    step <- flow$steps[[i]] %||% list()
    if (!identical(tolower(step$engine_id %||% ""), "idquant")) next
    if (!identical(tolower(step$type %||% "engine"), "container")) next

    subs <- step$substeps %||% list()
    if (!is.list(subs) || length(subs) == 0) next

    out <- list()
    for (j in seq_along(subs)) {
      ss <- subs[[j]] %||% list()
      seid <- tolower(ss$engine_id %||% "")
      if (!identical(seid, "idquant_cv")) {
        out[[length(out) + 1L]] <- ss
        next
      }

      scatter <- ss
      scatter$engine_id <- "idquant_cv_scatter"
      out[[length(out) + 1L]] <- scatter

      bar <- ss
      bar$engine_id <- "idquant_cv_bar"
      out[[length(out) + 1L]] <- bar
    }

    for (k in seq_along(out)) out[[k]]$order <- as.integer(k)
    step$substeps <- out
    flow$steps[[i]] <- step
  }

  flow
}

msterp_terpflow_load <- function(path, registry = NULL) {
  registry <- msterp_resolve_registry(registry)
  obj <- readRDS(path)

  # Schema evolution: normalize legacy engine IDs before validation.
  if (exists("migrate_legacy_engine_name", mode = "function")) {
    if (!is.null(obj$steps) && is.list(obj$steps) && length(obj$steps) > 0) {
      for (i in seq_along(obj$steps)) {
        s <- obj$steps[[i]] %||% list()
        if (!is.null(s$engine_id)) s$engine_id <- migrate_legacy_engine_name(s$engine_id)

        if (!is.null(s$paired) && is.list(s$paired) && !is.null(s$paired$engine_id)) {
          s$paired$engine_id <- migrate_legacy_engine_name(s$paired$engine_id)
        }

        if (!is.null(s$substeps) && is.list(s$substeps)) {
          for (j in seq_along(s$substeps)) {
            ss <- s$substeps[[j]] %||% list()
            if (!is.null(ss$engine_id)) ss$engine_id <- migrate_legacy_engine_name(ss$engine_id)
            s$substeps[[j]] <- ss
          }
        }

        obj$steps[[i]] <- s
      }
    }
  }

  obj <- msterp_terpflow_migrate_legacy_idquant_container(obj, registry = registry)
  obj <- msterp_terpflow_migrate_legacy_idquant_cv_split(obj, registry = registry)

  # Migrate legacy param keys (gene_list → target_list, etc.)
  obj <- .terpflow_migrate_param_keys(obj)

  v <- msterp_terpflow_validate(obj, registry)
  if (!v$ok) stop("Invalid .terpflow:\n", paste(v$errors, collapse = "\n"))

  if (!is.null(obj$steps) && length(obj$steps) > 0) {
    for (i in seq_along(obj$steps)) {
      obj$steps[[i]] <- msterp_terpflow_apply_step_defaults(obj$steps[[i]], registry)
    }
  }

  obj
}

msterp_terpflow_summary <- function(flow) {
  if (is.null(flow$steps) || length(flow$steps) == 0) return("(No steps)")
  
  paste0(
    vapply(flow$steps, function(s) {
      eng <- s$engine_id %||% "?"
      nm  <- s$name %||% eng
      paste0("- ", nm, " (", eng, ")")
    }, character(1)),
    collapse = "\n"
  )
}
