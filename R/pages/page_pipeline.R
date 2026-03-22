# =========================================================
# R/pages/page_pipeline.R — Pipeline Builder (Home + Editor)
#
# Changes implemented:
# - New Home view with 3 choices (Create / Edit / Duplicate)
# - Edit/Duplicate load a .terpflow (RDS) and open the same editor UI
# - Duplicate mode: pipeline_name gets "_copy", pipeline_id regenerated,
#   and default download filename becomes "<original>_copy.terpflow"
# - Single editor layout (no tabsetPanel):
#     Name, Add step, Build, single-line Status, Download (after build)
# - Exit button (top-right, black/white) returns to Home (does NOT stopApp)
# =========================================================

.has_colourpicker <- requireNamespace("colourpicker", quietly = TRUE)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- Registry helpers ---
tf_engine_ids <- function(registry, pipeline_type = NULL) {
  ids <- names(registry$engines %||% list()) %||% character()

  # Hide system/internal engines from pickers.
  ids <- ids[!vapply(ids, function(eid) {
    eng <- tf_engine_get_safe(eid, registry) %||% list()
    isTRUE(eng$picker_hidden %||% FALSE)
  }, logical(1))]

  # Filter by pipeline_type if provided
  if (!is.null(pipeline_type) && nzchar(pipeline_type)) {
    ids <- Filter(function(eid) {
      eng <- tf_engine_get_safe(eid, registry) %||% list()
      supported <- eng$supported_data_types %||% c("proteomics")
      pipeline_type %in% supported
    }, ids)
  }

  ids
}

tf_engine_get_safe <- function(engine_id, registry) {
  (registry$engines %||% list())[[engine_id]]
}


# -----------------------------
# Constants
# -----------------------------
TF_PAIRED_ONLY_ENGINES <- c("goora", "msea")  # Enrichment engines cannot be added as parent cards

# -----------------------------
# Pipeline step ordering rules
# -----------------------------

#' Enforce rule: peptide_analysis must be first step if present
#' Moves peptide_analysis to position 1 and renumbers all steps
#' @param flow A terpflow object
#' @return Modified terpflow with peptide_analysis as first step (if present)
tf_enforce_peptide_analysis_first <- function(flow) {
  if (is.null(flow$steps) || length(flow$steps) == 0) return(flow)

  # Find peptide_analysis step index
  pep_idx <- which(vapply(flow$steps, function(s) {
    tolower(s$engine_id %||% "") %in% c("peptide_analysis", "dsilac_peptide_analysis")
  }, logical(1)))

  # If not found or already first, return unchanged
  if (length(pep_idx) == 0 || pep_idx[1] == 1L) return(flow)

  # Move peptide_analysis to first position
  pep_step <- flow$steps[[pep_idx[1]]]
  other_steps <- flow$steps[-pep_idx[1]]
  flow$steps <- c(list(pep_step), other_steps)


  # Renumber order field for all steps
  for (i in seq_along(flow$steps)) {
    flow$steps[[i]]$order <- i
  }

  flow
}

# -----------------------------
# Helpers (UI + schema)
# -----------------------------
tf_color_input <- function(id, label, value) {
  if (.has_colourpicker) {
    colourpicker::colourInput(id, label, value = value)
  } else {
    textInput(id, label, value = value, placeholder = "#RRGGBB")
  }
}

tf_is_color_field <- function(f) {
  nm <- tolower(f$name %||% "")
  lb <- tolower(f$label %||% "")
  grepl("color|colour|palette|fill", nm) || grepl("color|colour|palette|fill", lb)
}

tf_find_choice_value <- function(field, pattern) {
  ch <- field$choices %||% character()
  if (length(ch) == 0) return(NULL)
  hit <- ch[grepl(pattern, ch, ignore.case = TRUE)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

tf_engine_kind <- function(engine_id, registry) {
  e <- tf_engine_get_safe(engine_id, registry) %||% list()
  x <- tolower(paste(engine_id, e$label %||% e$name %||% "", sep = " :: "))
  
  
  # ---- Explicit engine_id mappings (matches your registry) ----
  if (engine_id %in% c("hor_dis")) return("histdens")
  if (engine_id %in% c("vert_dis")) return("boxviolin")
  if (engine_id %in% c("1dgofcs")) return("gofcs1d")
  if (engine_id %in% c("2dgofcs")) return("gofcs2d")
  
  if (engine_id %in% c("half_life")) return("processing")
  if (engine_id %in% c("idquant")) return("idquant")
  if (engine_id %in% c("spearman", "scatter_correlation")) return("spearman")
  if (engine_id %in% c("pca")) return("pca")
  if (engine_id %in% c("volcano")) return("volcano")
  if (engine_id %in% c("goora")) return("goora")
  if (engine_id %in% c("subloc")) return("subloc")
  
  # ---- Fallback heuristics ----
  if (grepl("\\bpca\\b", x)) return("pca")
  if (grepl("volcano", x)) return("volcano")
  if (grepl("spearman", x)) return("spearman")
  if (grepl("id\\s*quant|id_quant|idquant", x)) return("idquant")
  if (grepl("density|hist", x)) return("histdens")
  if (grepl("box|violin", x)) return("boxviolin")
  if (grepl("\\bgo\\b.*ora|go-ora|goora", x)) return("goora")
  if (grepl("1d", x) && grepl("go", x) && grepl("fcs", x)) return("gofcs1d")
  if (grepl("2d", x) && grepl("go", x) && grepl("fcs", x)) return("gofcs2d")
  if (grepl("subloc|subcell", x)) return("subloc")
  
  "other"
}

tf_plot_defaults <- function(kind) {
  # Only applied when plot_width/plot_height exist and user hasn't set values.
  switch(
    kind,
    "idquant"   = c(width = 7, height = 5),
    "spearman"  = c(width = 6, height = 6),
    "histdens"  = c(width = 7, height = 14),
    "boxviolin" = c(width = 8, height = 6),
    "pca"       = c(width = 7, height = 6),
    "goora"     = c(width = 8, height = 6),
    "gofcs1d"   = c(width = 8, height = 6),
    "gofcs2d"   = c(width = 8, height = 6),
    "subloc"    = c(width = 7, height = 6),
    c(width = NA, height = NA)
  )
}


tf_override_label <- function(kind, f) {
  ff <- f
  nm <- tolower(ff$name %||% "")
  lb <- ff$label %||% ""
  
  # Only rename "group" selectors to "Sample group"
  if (kind %in% c("histdens", "boxviolin")) {
    if (nm %in% c("group", "colour_by", "color_by", "fill_by") || tolower(lb) == "group") {
      if (identical(ff$type, "choice")) ff$label <- "Sample group"
    }
  }
  
  ff
}

tf_schema_fields <- function(schema) {
  if (is.null(schema)) return(list())
  if (!is.list(schema)) return(list())
  unname(schema)
}

tf_partition_schema <- function(kind, schema, force_all_adv = FALSE) {
  fields <- tf_schema_fields(schema)
  if (length(fields) == 0) return(list(basic = list(), adv = list(), all = list()))
  
  kind <- tolower(kind %||% "")
  
  fields <- Filter(function(f) !isTRUE(f$viewer_only), fields)
  if (length(fields) == 0) return(list(basic = list(), adv = list(), all = list()))

  force_adv <- vapply(fields, function(f) {
    nm <- tolower(f$name %||% "")
    lb <- tolower(f$label %||% "")
    nmlb <- paste(nm, lb)
    
    # Axis title -> Advanced for these engines
    if (kind %in% c("spearman", "histdens", "boxviolin", "subloc")) {
      is_axis_title <- grepl("axis.*title", nmlb) ||
        grepl("^(x|y)(_?axis)?_?title$", nm) ||
        grepl("^(x|y)lab$", nm) ||
        grepl("x.*axis.*title|y.*axis.*title", nmlb)
      if (is_axis_title) return(TRUE)
    }
    
    # Subcellular localization: specific fields should be Advanced
    if (kind == "subloc") {
      if (nm %in% c("mean_type", "orientation")) return(TRUE)
    }
    
    FALSE
  }, logical(1))
  
  is_adv <- if (isTRUE(force_all_adv)) {
    rep(TRUE, length(fields))
  } else {
    vapply(fields, function(f) isTRUE(f$advanced), logical(1)) | force_adv
  }

  list(
    basic = fields[!is_adv],
    adv   = fields[ is_adv],
    all   = fields
  )
}

tf_color_input_wrapper <- function(id, label, value) {
  tf_color_input(id, label, value)
}

tf_field_input <- function(id, field, value_override = NULL) {
  val <- value_override %||% field$default %||% NULL
  
  # numericInput expects NA for unset min/max
  minv <- field$min %||% NA
  maxv <- field$max %||% NA
  
  switch(
    field$type,
    
    "choice" = {
      ch <- field$choices %||% character()
      # Use choice_labels if provided, otherwise use choices as labels
      ch_labels <- field$choice_labels %||% ch
      if (length(ch_labels) != length(ch)) ch_labels <- ch  # Fallback if lengths don't match
      choices_vec <- setNames(ch, ch_labels)

      is_colorish <- grepl("color|colour|palette|fill|coloring", tolower(field$name %||% "")) ||
        grepl("color|colour|palette|fill|coloring", tolower(field$label %||% ""))

      if (is_colorish && "group" %in% ch) {
        names(choices_vec)[choices_vec == "group"] <- "Sample group"
      }

      selectInput(id, field$label, choices = choices_vec, selected = val)
    },
    
    "bool" = checkboxInput(id, field$label, value = isTRUE(val)),
    
    "int"  = numericInput(
      id, field$label,
      value = if (is.null(val)) NA else val,
      min = minv, max = maxv, step = 1
    ),
    
    "num"  = numericInput(
      id, field$label,
      value = if (is.null(val)) NA else val,
      min = minv, max = maxv, step = "any"
    ),
    
    # range slider support
    "range" = {
      mn <- field$min %||% -10
      mx <- field$max %||%  10
      v <- val
      if (is.null(v) || !is.numeric(v) || length(v) != 2) v <- c(-1, 1)
      sliderInput(id, field$label, min = mn, max = mx, value = v)
    },
    
    "string" = {
      # ONLY treat as multiline when explicitly intended
      if (identical(field$name %||% "", "label_genes") ||
          grepl("one per line|per line", tolower(field$label %||% ""))) {
        return(textAreaInput(id, field$label, value = val %||% "", rows = 5))
      }
      
      # Use colour picker for any color-like string field (name or label)
      if (tf_is_color_field(field) || grepl("color|colour|hex", tolower(field$label %||% ""))) {
        tf_color_input_wrapper(id, field$label, val %||% "")
      } else {
        textInput(id, field$label, value = val %||% "")
      }
    },
    
    textInput(id, field$label, value = as.character(val %||% ""))
  )
}


tf_field_id <- function(step_id, prefix, f) sprintf("%s__%s_%s", step_id, prefix, f$name)

tf_render_fields <- function(step_id, kind, schema, values_list, prefix, schema_all) {
  by_name <- list()
  for (ff in (schema_all %||% list())) by_name[[ff$name]] <- ff
  
  find_controller_choice <- function(name_pat, choice_pat) {
    cand <- Filter(function(ff) identical(ff$type, "choice") && (
      grepl(name_pat, ff$name %||% "", ignore.case = TRUE) ||
        grepl(name_pat, ff$label %||% "", ignore.case = TRUE)
    ), schema_all %||% list())
    if (length(cand) == 0) return(NULL)
    for (cc in cand) {
      v <- tf_find_choice_value(cc, choice_pat)
      if (!is.null(v)) return(list(field = cc, val = v))
    }
    NULL
  }
  
  find_controller_bool <- function(name_pat) {
    cand <- Filter(function(ff) identical(ff$type, "bool") && (
      grepl(name_pat, ff$name %||% "", ignore.case = TRUE) ||
        grepl(name_pat, ff$label %||% "", ignore.case = TRUE)
    ), schema_all %||% list())
    if (length(cand) == 0) return(NULL)
    cand[[1]]
  }
  
  out <- list()
  
  spearman_xy_ctrl <- NULL
  if (kind == "spearman") {
    spearman_xy_ctrl <- find_controller_choice("xy.*range.*mode|x\\/y.*range.*mode|x\\s*\\/\\s*y.*range.*mode|x/y range mode|x\\s*y\\s*range mode|range.*mode", "manual")
    if (is.null(spearman_xy_ctrl)) {
      if (!is.null(by_name[["xy_range_mode"]]) && identical(by_name[["xy_range_mode"]]$type, "choice")) {
        v <- tf_find_choice_value(by_name[["xy_range_mode"]], "manual")
        if (!is.null(v)) spearman_xy_ctrl <- list(field = by_name[["xy_range_mode"]], val = v)
      }
    }
  }
  
  for (f in (schema %||% list())) {
    # Skip hidden fields in Terpflow (pipeline editor)
    # NOTE: viewer_only fields are excluded from the Pipeline UI (viewer_schema is Results Viewer only).
    # They persist to step.json as viewer defaults without affecting computation
    if (isTRUE(f$hidden)) next

    ff <- tf_override_label(kind, f)

    if (kind == "idquant") {
      if (tolower(ff$name %||% "") == "mode" || tolower(ff$label %||% "") == "mode") next
    }
    
    id <- tf_field_id(step_id, prefix, ff)
    
    value_override <- values_list[[ff$name]] %||% ff$default
    pd <- tf_plot_defaults(kind)
    
    fnm <- ff$name %||% ""
    
    if (fnm %in% c("width", "plot_width") && is.null(values_list[[ff$name]]) && !is.na(pd[["width"]])) {
      value_override <- unname(pd[["width"]])
    }
    if (fnm %in% c("height", "plot_height") && is.null(values_list[[ff$name]]) && !is.na(pd[["height"]])) {
      value_override <- unname(pd[["height"]])
    }
    
    ui <- tf_field_input(id, ff, value_override = value_override)
    
    nm <- tolower(ff$name %||% "")
    lb <- tolower(ff$label %||% "")
    nmlb <- paste(nm, lb, sep = " ")
    
    if (kind == "volcano" && nm == "seq_include_unique") {
      ctrlb <- by_name[["sequential_goora"]] %||% find_controller_bool("sequential.*goora|sequential.*enrich")
      if (!is.null(ctrlb)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrlb)
        ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
      }
    }
    
    if (kind == "pca") {
      ctrl_load <- by_name[["loadings_corr"]] %||% find_controller_bool("loadings.*corr|loadings correlation")
      if (!is.null(ctrl_load) && nm %in% c("n_pcs", "top_n")) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl_load)
        ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
      }
    }
    
    if (grepl("flat", nm) && tf_is_color_field(ff)) {
      ctrl <- find_controller_choice("color|colour|palette|fill", "flat")
      if (!is.null(ctrl)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl$field)
        ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, ctrl$val), ui)
      }
    }
    
    if (kind == "subloc" && nm == "mean_type") {
      ctrl_id <- sprintf("%s__s_show_global_mean", step_id)
      ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
    }
    
    
    if (kind == "idquant" && grepl("^ymax", nm)) {
      ctrl <- find_controller_choice("y.*limit.*mode|y_limit_mode", "manual")
      if (!is.null(ctrl)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl$field)
        ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, ctrl$val), ui)
      }
    }
    
    if (kind == "spearman" && nm %in% c("xy_min", "xy_max") && !is.null(spearman_xy_ctrl)) {
      ctrl_id <- tf_field_id(step_id, prefix, spearman_xy_ctrl$field)
      ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, spearman_xy_ctrl$val), ui)
    }
    
    if (kind != "spearman" && grepl("^x(_|)min$|^x(_|)max$|xlim(_|)min|xlim(_|)max", nm)) {
      ctrl <- find_controller_choice("x.*(range|limit).*mode|xlim.*mode", "manual")
      if (!is.null(ctrl)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl$field)
        ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, ctrl$val), ui)
      }
    }
    
    if (grepl("^y(_|)min$|^y(_|)max$|ylim(_|)min|ylim(_|)max", nm)) {
      ctrl <- find_controller_choice("y.*(range|limit).*mode|ylim.*mode", "manual")
      if (!is.null(ctrl)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl$field)
        ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, ctrl$val), ui)
      }
    }
    
    if (kind == "histdens" && nm == "pool_value") {
      ctrlb <- by_name[["pool_above"]] %||% find_controller_bool("pool_above|pool values above|pool")
      if (!is.null(ctrlb)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrlb)
        ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
      }
    }
    
    if (grepl("mean_type|mean_line_size|mean.*thick|mean.*width|mean_line_width", nm)) {
      ctrlb <- find_controller_bool("show.*mean|mean.*line")
      if (!is.null(ctrlb)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrlb)
        ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
      }
    }
    
    if (grepl("guide_alpha|guide_size|guideline", nm)) {
      ctrlb <- by_name[["show_guides"]] %||% find_controller_bool("show_guides|guide|guideline")
      if (!is.null(ctrlb)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrlb)
        ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
      }
    }
    
    if (grepl("ellipse", nmlb) && grepl("opacity|alpha", nmlb)) {
      ctrlb <- by_name[["show_ellipse"]] %||% find_controller_bool("show_ellipse|ellipse")
      if (!is.null(ctrlb)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrlb)
        ui <- conditionalPanel(sprintf("input['%s'] == true", ctrl_id), ui)
      }
    }

    # CV bar: show cv_bin_N thresholds only when num_bins >= N+1
    if (grepl("^cv_bin_[1-5]$", nm)) {
      bin_index <- suppressWarnings(as.integer(sub("^cv_bin_", "", nm)))
      ctrl_num_bins <- by_name[["num_bins"]]
      if (!is.null(ctrl_num_bins) && is.finite(bin_index)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl_num_bins)
        ui <- conditionalPanel(sprintf("input['%s'] >= %d", ctrl_id, bin_index + 1L), ui)
      }
    }

    # Rankplot: threshold fields shown when highlight_mode == "threshold"
    if (grepl("^threshold_", nm)) {
      ctrl <- find_controller_choice("highlight.*mode", "threshold")
      if (!is.null(ctrl)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl$field)
        ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, ctrl$val), ui)
      }
    }

    # Rankplot: topn fields shown when highlight_mode == "topn"
    if (grepl("^topn_", nm)) {
      ctrl <- find_controller_choice("highlight.*mode", "topn")
      if (!is.null(ctrl)) {
        ctrl_id <- tf_field_id(step_id, prefix, ctrl$field)
        ui <- conditionalPanel(sprintf("input['%s'] == '%s'", ctrl_id, ctrl$val), ui)
      }
    }

    out[[length(out) + 1L]] <- ui
  }
  
  tagList(out)
}

tf_letter <- function(i) if (i >= 1 && i <= length(LETTERS)) LETTERS[i] else paste0("S", i)

# -----------------------------
# Data Processor helpers
# -----------------------------
tf_dp_default_plan <- function() {
  list(
    n = 1,
    substeps = list(list(
      op = "filter_threshold",
      opts = list(threshold = 0, direction = "below", all_data_columns = TRUE)
    )),
    save_processed_excel = TRUE
  )
}

tf_dp_ops <- function() {
  c(
    "Filter value by threshold (cell -> N/A)" = "filter_threshold",
    "Filter by prefix (row delete)" = "filter_prefix",
    "Filter by keyword (row delete)" = "filter_keyword",
    "Filter non-numeric (cell -> N/A)" = "filter_non_numeric",
    "Aggregate rows by identifier" = "aggregate_rows",
    "Average rows by identifier" = "average_rows",
    "Contaminants tag and remove" = "tag_remove_contaminants",
    "Imputation" = "impute",
    "Filter by % missing (group \u2192 N/A)" = "filter_pct_missing"
  )
}

tf_id_cols_prefix <- function() { c("Gene symbol" = "gene", "Protein ID" = "protein") }
tf_id_cols_no_precursor <- function() { c("Gene symbol" = "gene", "Protein ID" = "protein") }

# Context-aware ID column options for aggregate_rows operation
# Inside Peptide Analysis container: only "Peptide ID" available
# Outside containers (or other containers): only "Gene symbol" and "Protein ID" available
tf_id_cols_aggregate <- function(parent_container_engine_id = NULL) {
  if (isTRUE(parent_container_engine_id %in% c("peptide_analysis", "dsilac_peptide_analysis"))) {
    c("Peptide ID" = "peptide")
  } else {
    c("Gene symbol" = "gene", "Protein ID" = "protein")
  }
}

# -----------------------------
# UI (Home + Editor)
# -----------------------------
page_pipeline_ui <- function() {
  div(
    class = "tf-wrap",
    tags$head(
      tags$style(HTML("
        .tf-wrap {
          position: relative; height: 100%; min-height: 0;
          display: flex; flex-direction: column; gap: 14px;
          padding-top: 10px; /* top breathing room so header isn't jammed */
        }
        .tf-main { height: 100%; min-height: 0; display: flex; gap: 14px; }
        
        .tf-sidebar {
          width: 270px; min-width: 270px;
          background: var(--color-white); border: 1px solid var(--md-card-border); border-radius: 12px;
          padding: 12px; overflow: auto; min-height: 0;
        }
        .tf-canvas {
          flex: 1;
          background: var(--color-white); border: 1px solid var(--md-card-border); border-radius: 12px;
          padding: 12px; overflow: auto; min-height: 0;
        }

        .tf-sidebar .form-group { margin-bottom: 10px; }
        .tf-sidebar label { display:none; }

        /* Top bar (Editor) */
        .tf-topbar {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 12px 12px;       /* symmetric padding keeps Exit vertically centered */
          min-height: 56px;         /* stable header height */
          background: var(--color-white);
          border: 1px solid var(--md-card-border);
          border-radius: 12px;
        }
        .tf-topbar-title {
          font-weight: 800;
          font-size: 18px;
          color: var(--text-primary);
          letter-spacing: 0.2px;
          line-height: 1;
        }

        /* Exit button (in top bar) */
        .tf-exit-btn {
          background: #000 !important;
          color: #fff !important;
          border: 1px solid #000 !important;
          border-radius: 10px !important;
          padding: 8px 12px !important;
          font-weight: 800 !important;
        }
        .tf-exit-btn:hover { background: #111 !important; border-color: #111 !important; }
        [data-theme='dark'] .tf-exit-btn {
          background: var(--bg-hover) !important;
          color: var(--text-primary) !important;
          border-color: var(--border-medium) !important;
        }
        [data-theme='dark'] .tf-exit-btn:hover {
          background: var(--bg-muted) !important;
          border-color: var(--border-medium) !important;
        }

        /* Home view */
        .tf-home {
          flex: 1;
          min-height: 0;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 18px;
        }
        .tf-home-card {
          width: min(520px, 95vw);
          background: var(--md-card-bg);
          border: 1px solid var(--md-card-border);
          border-radius: 14px;
          padding: 18px;
          box-shadow: var(--md-card-shadow);
        }
        .tf-home-actions {
          display: flex;
          flex-direction: row;     /* horizontal */
          gap: 10px;
          flex-wrap: wrap;         /* wrap on narrow screens */
        }
        .tf-home-actions .btn {
          width: auto;             /* no longer full width */
          flex: 1 1 160px;          /* distribute evenly, but allow wrapping */
          border-radius: 12px;
          padding: 12px 14px;
          font-weight: 700;
        }
        .tf-home-file {
          margin-top: 12px;
          border-top: 1px solid var(--md-border);
          padding-top: 12px;
        }
        .tf-home-note {
          color: var(--text-secondary);
          font-size: 12px;
          margin-top: 6px;
        }
        .tf-type-badge {
          display: inline-block;
          background: var(--bg-hover, #f0f0f0);
          color: var(--text-primary, #333);
          border: 1px solid var(--md-card-border, #ddd);
          border-radius: 8px;
          padding: 6px 12px;
          font-size: 13px;
          font-weight: 600;
          margin-bottom: 10px;
        }

        /* Status line */
        .tf-status {
          font-weight: 700;
          padding: 8px 10px;
          border: 1px solid var(--md-border);
          border-radius: 10px;
          background: var(--bg-card);
        }
        .tf-status-line { display: flex; flex-direction: column; gap: 4px; }
        .tf-status-flag { font-weight: 700; }
        .tf-status-flag.dirty { color: var(--status-warn-text); }
        .tf-status-meta { font-size: 11px; color: var(--text-secondary); font-weight: 600; }

        /* Cards (unchanged) */
        .tf-card {
          border: 1px solid var(--md-card-border); border-radius: 12px;
          margin-bottom: 10px; background: var(--md-card-bg);
          box-shadow: var(--md-card-shadow);
        }
      
        .tf-card-hd {
          list-style: none;
          display: flex; align-items: center; gap: 10px;
          padding: 10px 12px;
          border-bottom: 1px solid var(--md-border);
          cursor: pointer;
          border-radius: 12px 12px 0 0;
        }
        .tf-card summary::-webkit-details-marker { display: none; }

        .tf-card.tf-parent > summary.tf-card-hd { background: var(--primary); color: #fff; }
        .tf-card.tf-secondary > summary.tf-card-hd { background: var(--accent-gold); color: #fff; }
        .tf-card.tf-secondary { margin-left: 18px; }

        .tf-card-hd .tf-step-title { color: #fff; }

        .tf-step-badge {
          width: 44px; min-width: 44px; height: 34px;
          display: flex; align-items: center; justify-content: center;
          background: var(--charcoal); color: #fff; border-radius: 10px;
          font-weight: 700;
        }
        .tf-step-title { font-weight: 700; }
        .tf-step-spacer { flex: 1; }
        .tf-step-actions { display: flex; gap: 6px; align-items: center; }

        .tf-iconbtn {
          width: 34px; height: 28px; padding: 0;
          background: var(--charcoal) !important;
          color: #fff !important;
          border: 1px solid var(--charcoal) !important;
          border-radius: 8px !important;
          line-height: 26px;
        }
        .tf-iconbtn:hover { background: var(--charcoal-light) !important; border-color: var(--charcoal-light) !important; }

        .tf-card-bd { padding: 12px; }
        .tf-section-label { font-weight: 700; margin: 4px 0 10px 0; color: var(--text-primary); }
        .tf-note { color: var(--text-secondary); padding: 8px 10px; background: var(--color-white); border: 1px solid var(--md-border); border-radius: 10px; }
        .tf-aes-wrap { margin-top: 10px; padding: 10px; background: var(--color-white); border: 1px dashed var(--md-border); border-radius: 10px; }
        .tf-info-box { color: var(--text-secondary); font-size: 11px; background: var(--status-info-bg); border: 1px solid var(--status-info-text); border-radius: 8px; padding: 8px 10px; margin: 8px 0; line-height: 1.4; }
        .tf-info-box strong { color: var(--text-primary); }

        /* Dataprocessor substeps as <details> */
        .tf-subdetails {
          border: 1px solid var(--md-card-border); border-radius: 12px;
          margin: 10px 0; background: var(--md-card-bg);
          box-shadow: var(--md-card-shadow);
        }
        .tf-subdetails > summary {
          list-style: none;
          display: flex; gap: 10px; align-items: center;
          padding: 10px 12px; border-bottom: 1px solid var(--md-border);
          background: var(--accent-gold);
          border-radius: 12px 12px 0 0;
          cursor: pointer;
        }
        .tf-subdetails summary::-webkit-details-marker { display: none; }
        .tf-substep-badge {
          width: 34px; min-width: 34px; height: 28px;
          display: flex; align-items: center; justify-content: center;
          background: var(--charcoal); color: #fff; border-radius: 10px;
          font-weight: 800;
        }
        .tf-substep-title { font-weight: 700; color: #fff; }
        .tf-substep-spacer { flex: 1; }
        .tf-substep-actions { display:flex; gap:6px; align-items:center; }
        .tf-subcard-bd { padding: 12px; }

        /* Container substeps */
        .tf-container-substeps { margin-top: 8px; }
        .tf-container-controls { display:flex; gap:10px; align-items:center; flex-wrap:wrap; margin: 6px 0 10px 0; }
        .tf-container-controls .btn { border-radius: 10px; font-weight: 700; }
        .tf-subdetails.tf-locked { opacity: 0.85; }
        .tf-subdetails.tf-locked > summary { background: var(--border-medium); }
        .tf-subdetails.tf-locked .tf-substep-title { color: var(--text-secondary); }
        .tf-subdetails.tf-locked .tf-substep-badge { background: #444; }

        /* Data Processor substeps inside Peptide Analysis: grey background for better visibility */
        /* Badge keeps black background with white text */
        .tf-subdetails.tf-dp-sub-in-peptide > summary { background: #444444; }
        .tf-subdetails.tf-dp-sub-in-peptide .tf-substep-title { color: #fff; }

        .tf-lock-badge {
          font-weight: 700;
          font-size: 11px;
          color: var(--text-primary);
          background: rgba(0,0,0,0.08);
          border: 1px solid rgba(0,0,0,0.10);
          padding: 4px 8px;
          border-radius: 999px;
        }

        /* Add-step modal layout */
        .tf-addstep-wrap { display: flex; gap: 12px; }
        .tf-addstep-col { flex: 1; min-width: 0; }
        .tf-addstep-box {
          border: 1px solid var(--md-card-border); border-radius: 12px;
          padding: 10px; background: var(--md-card-bg);
          max-height: 420px; overflow: auto;
          box-shadow: var(--md-card-shadow);
        }
        .tf-addrow { display: flex; align-items: center; gap: 10px; padding: 8px 6px; border-bottom: 1px solid var(--border-light); }
        .tf-addrow:last-child { border-bottom: none; }
        .tf-addname { font-weight: 700; }
        .tf-silac-badge {
          display: inline-block; font-size: 10px; font-weight: 700;
          background: #e8f0fe; color: #1a73e8; border-radius: 4px;
          padding: 1px 5px; margin-left: 6px; vertical-align: middle;
        }
        .tf-addspacer { flex: 1; }
        .tf-mini { width: 34px; height: 28px; padding: 0; border-radius: 8px; }

        @media (max-width: 1100px) {
          .tf-main { flex-direction: column; }
          .tf-sidebar, .tf-canvas {
            width: 100%;
            min-width: 0;
          }
          .tf-sidebar { max-height: 320px; }
        }
      ")),
      tags$script(HTML("
        window.TerpFlow = window.TerpFlow || {};

        TerpFlow.toggle = function(id){
          var el = document.getElementById(id);
          if(!el) return;
          var cur = el.style.display;
          if(cur === 'none' || cur === '') el.style.display = 'block';
          else el.style.display = 'none';
        };

        TerpFlow.bindDetails = function(){
          var cards = document.querySelectorAll('details[data-tf-details-id]');
          for (var i=0; i<cards.length; i++){
            var el = cards[i];
            if (el.__tf_bound) continue;
            el.__tf_bound = true;
            el.addEventListener('toggle', function(e){
              var did = this.getAttribute('data-tf-details-id');
              var isOpen = this.hasAttribute('open');
              if (window.Shiny && Shiny.setInputValue){
                Shiny.setInputValue('tf_details_state',
                  { id: did, open: isOpen, nonce: Math.random() },
                  { priority: 'event' }
                );
              }
            });
          }
        };
      "))
    ),
    
    uiOutput("tf_root_ui")
  )
}

# -----------------------------
# Data Processor substep UI
# -----------------------------
# parent_container_engine_id: if this DP is inside a container, pass the container's engine_id
#                             (e.g., "peptide_analysis") to enable context-aware options
tf_dp_substep_ui <- function(step_id, i, dp_state, open_state = TRUE, parent_container_engine_id = NULL) {
  op_id <- sprintf("%s__dp_op_%d", step_id, i)
  op_val <- dp_state$substeps[[i]]$op %||% "filter_threshold"
  mkid <- function(suffix) sprintf("%s__dp_%s_%d", step_id, suffix, i)

  details_id <- sprintf("%s__dp__sub_%d", step_id, i)

  # Grey background for DP substeps when inside peptide_analysis container
  is_in_peptide <- isTRUE(parent_container_engine_id %in% c("peptide_analysis", "dsilac_peptide_analysis"))

  tags$details(
    class = paste("tf-subdetails", if (is_in_peptide) "tf-dp-sub-in-peptide" else ""),
    open = if (isTRUE(open_state)) TRUE else NULL,
    `data-tf-details-id` = details_id,
    tags$summary(
      div(class = "tf-substep-badge", tf_letter(i)),
      div(class = "tf-substep-title", "Sub-step"),
      div(class = "tf-substep-spacer"),
      div(
        class = "tf-substep-actions",
        tags$button(
          type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          onclick = sprintf(
            "event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('tf_dp_rm_sub', {step_id:%s, idx:%d, nonce:Math.random()}, {priority:'event'});",
            jsonlite::toJSON(step_id, auto_unbox = TRUE), i
          ),
          HTML("&times;")
        )
      )
    ),
    div(
      class = "tf-subcard-bd",
      selectInput(op_id, "Function", choices = tf_dp_ops(), selected = op_val),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'filter_threshold'", op_id),
        numericInput(mkid("thr"), "Threshold value", value = dp_state$substeps[[i]]$opts$threshold %||% 0),
        selectInput(
          mkid("thr_dir"), "Direction",
          choices = c("Filter below (set to N/A)" = "below", "Filter above (set to N/A)" = "above"),
          selected = dp_state$substeps[[i]]$opts$direction %||% "below"
        ),
        div(class = "tf-note", "Applies to all data columns.")
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'filter_prefix'", op_id),
        selectInput(
          mkid("pref_col"), "ID column",
          choices = tf_id_cols_prefix(),
          selected = dp_state$substeps[[i]]$opts$id_col %||% "protein"
        ),
        textInput(mkid("pref_val"), "Prefix", value = dp_state$substeps[[i]]$opts$prefix %||% "Cont_")
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'filter_keyword'", op_id),
        selectInput(
          mkid("kw_col"), "ID column",
          choices = tf_id_cols_no_precursor(),
          selected = dp_state$substeps[[i]]$opts$id_col %||% "protein"
        ),
        textInput(mkid("kw_val"), "Keyword / string", value = dp_state$substeps[[i]]$opts$keyword %||% "Grand total")
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'aggregate_rows'", op_id),
        selectInput(
          mkid("agg_col"), "Aggregate by",
          choices = tf_id_cols_aggregate(parent_container_engine_id),
          selected = {
            # Default depends on context: "peptide" inside peptide_analysis, "protein" otherwise
            saved <- dp_state$substeps[[i]]$opts$id_col %||% NULL
            agg_choices <- tf_id_cols_aggregate(parent_container_engine_id)
            if (!is.null(saved) && saved %in% agg_choices) saved else agg_choices[[1]]
          }
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'average_rows'", op_id),
        selectInput(
          mkid("avg_col"), "Average by",
          choices = tf_id_cols_no_precursor(),
          selected = dp_state$substeps[[i]]$opts$id_col %||% "protein"
        ),
        selectInput(
          mkid("avg_type"), "Mean type",
          choices = c("Arithmetic" = "arithmetic", "Harmonic" = "harmonic"),
          selected = dp_state$substeps[[i]]$opts$mean_type %||% "arithmetic"
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'tag_remove_contaminants'", op_id),
        div(class = "tf-note", "No customization for this function (Protein ID is assumed).")
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'impute'", op_id),
        selectInput(
          mkid("imp_method"), "Method",
          choices = c("Flat value" = "flat", "Smallest value × multiplier" = "min_mult"),
          selected = dp_state$substeps[[i]]$opts$method %||% "flat"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'flat'", mkid("imp_method")),
          numericInput(mkid("imp_flat"), "Flat value", value = dp_state$substeps[[i]]$opts$flat_value %||% 10)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'min_mult'", mkid("imp_method")),
          numericInput(mkid("imp_mult"), "Multiplier", value = dp_state$substeps[[i]]$opts$multiplier %||% 0.1, step = 0.01)
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'filter_non_numeric'", op_id),
        div(class = "tf-note", "No customization for this function.")
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'filter_pct_missing'", op_id),
        numericInput(mkid("pct_min"), "Minimum % present", value = dp_state$substeps[[i]]$opts$min_pct_present %||% 50, min = 0, max = 100, step = 5),
        div(class = "tf-note", "Per sample group: if fewer than this % of replicates have a value, set the group\u2019s values to N/A for that row.")
      )
    )
  )
}

# -----------------------------
# Paired card UI
# -----------------------------
tf_paired_card_ui <- function(parent_step, registry, paired_engine_id, details_id, open_state = TRUE) {
  parent_step_id <- parent_step$step_id
  eng <- tf_engine_get_safe(paired_engine_id, registry)
  if (is.null(eng)) return(NULL)
  
  
  p_raw <- parent_step$paired %||% list()
  
  p <- list(params = list(), style = list())
  if (is.list(p_raw$engines) && is.list(p_raw$engines[[paired_engine_id]])) {
    p <- p_raw$engines[[paired_engine_id]]
  } else if (identical(p_raw$engine_id %||% NULL, paired_engine_id)) {
    p <- p_raw
  }
  
  p$params <- p$params %||% list()
  p$style  <- p$style  %||% list()
  
  kind <- tf_engine_kind(paired_engine_id, registry)
  params_schema <- eng$params_schema %||% list()
  style_schema  <- eng$style_schema  %||% list()

  # Hide control_only for GO-FCS engines when paired with PCA (no group comparisons, just PC loadings)
  parent_engine_id <- parent_step$engine_id %||% ""
  if (identical(parent_engine_id, "pca") && paired_engine_id %in% c("1dgofcs", "2dgofcs")) {
    params_schema <- Filter(function(f) {
      if (!is.list(f) || is.null(f$name)) return(TRUE)
      f$name != "control_only"
    }, params_schema)
  }

  # Hide fdr_cutoff and min_overlap for GO-ORA when paired with volcano (those are now per-config)
  if (identical(parent_engine_id, "volcano") && identical(paired_engine_id, "goora")) {
    params_schema <- Filter(function(f) {
      if (!is.list(f) || is.null(f$name)) return(TRUE)
      !(f$name %in% c("fdr_cutoff", "min_overlap"))
    }, params_schema)
  }

  split_p <- tf_partition_schema(kind, params_schema)
  split_s <- tf_partition_schema(kind, style_schema, force_all_adv = TRUE)
  
  basic_params <- split_p$basic
  adv_params   <- split_p$adv
  basic_style  <- split_s$basic
  adv_style    <- split_s$adv
  
  has_basic <- (length(basic_params) + length(basic_style)) > 0
  has_adv   <- (length(adv_params) + length(adv_style)) > 0
  
  adv_wrap_id <- sprintf("%s__paired__adv_wrap_%s", parent_step_id, paired_engine_id)
  btn_adv_id  <- sprintf("%s__paired__btn_adv_%s", parent_step_id, paired_engine_id)
  
  tags$details(
    class = "tf-card tf-secondary",
    open = if (isTRUE(open_state)) TRUE else NULL,
    `data-tf-details-id` = details_id,
    tags$summary(
      class = "tf-card-hd",
      div(class = "tf-step-badge", "\u21B3"),
      div(class = "tf-step-title", eng$label %||% eng$name %||% eng$id %||% paired_engine_id),
      div(class = "tf-step-spacer")
    ),
    
    div(
      class = "tf-card-bd",
      div(class = "tf-section-label", "Options"),
      
      if (!has_basic) {
        div(class = "tf-note", if (has_adv) "No basic options. Use “Show more options”." else "No engine options.")
      } else {
        tagList(
          if (length(basic_params) > 0) tf_render_fields(
            step_id = parent_step_id,
            kind = kind,
            schema = basic_params,
            values_list = p$params,
            prefix = sprintf("paired_%s__p", paired_engine_id),
            schema_all = params_schema
          ) else NULL,
          
          if (length(basic_style) > 0) tf_render_fields(
            step_id = parent_step_id,
            kind = kind,
            schema = basic_style,
            values_list = p$style,
            prefix = sprintf("paired_%s__s", paired_engine_id),
            schema_all = style_schema
          ) else NULL
        )
      },
      
      if (has_adv) tagList(
        tags$hr(),
        tags$button(
          id = btn_adv_id, type = "button",
          class = "btn btn-default btn-sm action-button",
          onclick = sprintf("event.preventDefault(); TerpFlow.toggle('%s');", adv_wrap_id),
          "Show more options"
        ),
        div(
          id = adv_wrap_id,
          class = "tf-aes-wrap",
          style = "display:none;",
          div(class = "tf-section-label", "Advanced options"),

          if (length(adv_params) > 0) tf_render_fields(
            step_id = parent_step_id,
            kind = kind,
            schema = adv_params,
            values_list = p$params,
            prefix = sprintf("paired_%s__p", paired_engine_id),
            schema_all = params_schema
          ) else NULL,
          
          if (length(adv_style) > 0) tf_render_fields(
            step_id = parent_step_id,
            kind = kind,
            schema = adv_style,
            values_list = p$style,
            prefix = sprintf("paired_%s__s", paired_engine_id),
            schema_all = style_schema
          ) else NULL
        )
      )
    )
  )
}

# -----------------------------
# Parent step card UI
# -----------------------------
tf_step_card_ui <- function(step, idx, n_steps, registry, open_state = TRUE, pipeline_type = "proteomics") {
  step_id <- step$step_id
  eng <- tf_engine_get_safe(step$engine_id, registry)
  kind <- tf_engine_kind(step$engine_id, registry)
  
  
  adv_wrap_id <- sprintf("%s__adv_wrap", step_id)
  btn_up_id <- sprintf("%s__btn_up", step_id)
  btn_dn_id <- sprintf("%s__btn_dn", step_id)
  btn_rm_id <- sprintf("%s__btn_rm", step_id)
  btn_adv_toggle_id <- sprintf("%s__btn_adv_toggle", step_id)
  
  is_dp <- identical(step$engine_id, "dataprocessor")
  
  dp_state <- NULL
  if (is_dp) {
    dp_state <- tryCatch(fromJSON(step$params$plan_json %||% "", simplifyVector = FALSE),
                         error = function(e) tf_dp_default_plan())
    if (is.null(dp_state$n) || is.null(dp_state$substeps)) dp_state <- tf_dp_default_plan()
  }
  
  is_pca      <- identical(step$engine_id, "pca")
  is_volcano  <- identical(step$engine_id, "volcano")
  is_rankplot <- identical(step$engine_id, "rankplot")
  supports_paired <- (!is_dp) && (is_pca || is_volcano || is_rankplot)
  
  paired_enable_id <- sprintf("%s__paired_enable", step_id)
  pca_loadings_id  <- sprintf("%s__p_loadings_corr", step_id)
  
  paired_enabled_default <- isTRUE((step$paired %||% list())$enabled)
  if (is_pca && is.null((step$paired %||% list())$enabled)) paired_enabled_default <- TRUE
  
  paired_ui <- NULL
  
  if (supports_paired) {
    paired_chk_id <- function(peid) sprintf("%s__paired_do_%s", step_id, peid)
    
    paired_cfg <- step$paired %||% list()
    paired_eng_cfg <- paired_cfg$engines %||% list()
    
    default_for <- function(peid) {
      if (is.list(paired_eng_cfg[[peid]]) && !is.null(paired_eng_cfg[[peid]]$enabled)) {
        return(isTRUE(paired_eng_cfg[[peid]]$enabled))
      }
      if (identical(paired_cfg$engine_id %||% NULL, peid)) return(isTRUE(paired_cfg$enabled))
      if (identical(peid, "goora") || identical(peid, "msea")) return(TRUE)
      FALSE
    }
    
    base_paired_ui <- tagList(
      tags$hr(),
      div(class = "tf-section-label", "Paired analysis"),
      checkboxInput(paired_enable_id, "Enable paired analysis", value = paired_enabled_default),
      
      if (is_pca) {
        if (pipeline_type == "metabolomics") {
          pca_cand <- c("msea")
          if ("pathway_fcs" %in% (names(registry$engines) %||% character())) pca_cand <- c(pca_cand, "pathway_fcs")
        } else {
          pca_cand <- c("goora")
          if ("1dgofcs" %in% (names(registry$engines) %||% character())) pca_cand <- c(pca_cand, "1dgofcs")
          if ("2dgofcs" %in% (names(registry$engines) %||% character())) pca_cand <- c(pca_cand, "2dgofcs")
        }
        pca_cand <- unique(pca_cand)
        pca_cand <- pca_cand[pca_cand %in% names(registry$engines)]

        conditionalPanel(
          condition = sprintf("input['%s'] == true && input['%s'] == true", pca_loadings_id, paired_enable_id),
          tagList(lapply(pca_cand, function(peid) {
            checkboxInput(
              paired_chk_id(peid),
              registry$engines[[peid]]$label %||% peid,
              value = default_for(peid)
            )
          })),
          div(class = "tf-note", "Linked paired card(s) will appear below.")
        )
      } else if (is_rankplot) {
        # Rankplot paired analysis UI
        rp_note <- if (pipeline_type == "metabolomics") {
          "When enabled, MSEA will run on highlighted metabolites (top/bottom) for each sample group. "
        } else {
          "When enabled, GO-ORA will run on highlighted proteins (top/bottom) for each sample group. "
        }
        conditionalPanel(
          condition = sprintf("input['%s'] == true", paired_enable_id),
          tagList(
            div(class = "tf-note",
                rp_note,
                "Highlight thresholds are set in the Params/Style section above.")
          )
        )
      } else {
        # Volcano paired analysis UI - multi-config enrichment
        goora_configs_ui_id <- sprintf("%s__goora_configs_ui", step_id)
        goora_add_btn_id <- sprintf("%s__goora_add_config", step_id)

        enrich_label <- if (pipeline_type == "metabolomics") "MSEA" else "GO-ORA"
        conditionalPanel(
          condition = sprintf("input['%s'] == true", paired_enable_id),
          tagList(
            div(class = "tf-section-label", paste0(enrich_label, " Configurations")),
            div(class = "tf-note",
                sprintf("Add multiple configurations to run %s with different filter settings. ", enrich_label),
                "Global settings (min term size, max terms) are shared across all configurations."),
            uiOutput(goora_configs_ui_id),
            actionButton(
              goora_add_btn_id,
              "+ Add Configuration",
              class = "btn btn-xs btn-default",
              style = "margin-top: 8px; margin-bottom: 12px;"
            )
          )
        )
      }
    )
    
    paired_ui <- if (is_pca) {
      conditionalPanel(
        condition = sprintf("input['%s'] == true", pca_loadings_id),
        base_paired_ui
      )
    } else {
      base_paired_ui
    }
  }
  
  params_schema <- eng$params_schema %||% list()
  style_schema  <- eng$style_schema  %||% list()
  
  split_p <- tf_partition_schema(kind, params_schema)
  split_s <- tf_partition_schema(kind, style_schema, force_all_adv = TRUE)
  
  basic_params <- split_p$basic
  adv_params   <- split_p$adv
  basic_style  <- split_s$basic
  adv_style    <- split_s$adv
  
  has_basic <- (length(basic_params) + length(basic_style)) > 0
  has_adv   <- (length(adv_params) + length(adv_style)) > 0
  
  tags$details(
    class = "tf-card tf-parent",
    open = if (isTRUE(open_state)) TRUE else NULL,
    `data-tf-details-id` = step_id,
    tags$summary(
      class = "tf-card-hd",
      div(class = "tf-step-badge", idx),
      div(class = "tf-step-title", eng$label %||% eng$name %||% eng$id %||% step$engine_id),
      div(class = "tf-step-spacer"),
      div(
        class = "tf-step-actions",
        tags$button(
          id = btn_up_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (idx == 1) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&#8593;")
        ),
        tags$button(
          id = btn_dn_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (idx == n_steps) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&#8595;")
        ),
        tags$button(
          id = btn_rm_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&times;")
        )
      )
    ),
    
    div(
      class = "tf-card-bd",
      div(class = "tf-section-label", "Options"),
      
      if (is_dp) {
        tagList(
          numericInput(
            sprintf("%s__dp_n", step_id),
            "# of functions (sub-steps)",
            value = dp_state$n %||% 1,
            min = 1, max = 25, step = 1
          ),
          uiOutput(sprintf("%s__dp_substeps_wrap", step_id)),
          tags$hr(),
          checkboxInput(
            sprintf("%s__dp_save", step_id),
            "Save processed Excel after this step",
            value = dp_state$save_processed_excel %||% TRUE
          )
        )
      } else {
        tagList(
          if (!has_basic) {
            div(class = "tf-note", if (has_adv) "No basic options. Use “Show more options”." else "No engine options.")
          } else {
            tagList(
              if (length(basic_params) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = basic_params,
                values_list = step$params %||% list(),
                prefix = "p",
                schema_all = params_schema
              ) else NULL,
              
              if (length(basic_style) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = basic_style,
                values_list = step$style %||% list(),
                prefix = "s",
                schema_all = style_schema
              ) else NULL
            )
          },
          
          if (!is.null(paired_ui)) paired_ui,
          
          if (has_adv) tagList(
            tags$hr(),
            tags$button(
              id = btn_adv_toggle_id, type = "button",
              class = "btn btn-default btn-sm action-button",
              onclick = sprintf("event.preventDefault(); TerpFlow.toggle('%s');", adv_wrap_id),
              "Show more options"
            ),
            div(
              id = adv_wrap_id,
              class = "tf-aes-wrap",
              style = "display:none;",
              div(class = "tf-section-label", "Advanced options"),

              if (length(adv_params) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = adv_params,
                values_list = step$params %||% list(),
                prefix = "p",
                schema_all = params_schema
              ) else NULL,
              
              if (length(adv_style) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = adv_style,
                values_list = step$style %||% list(),
                prefix = "s",
                schema_all = style_schema
              ) else NULL
            )
          )
        )
      }
    )
  )
}

tf_container_substep_card_ui <- function(parent_step_id, substep, idx, n_steps, registry, open_state = TRUE,
                                        disable_up = FALSE, disable_down = FALSE,
                                        parent_container_engine_id = NULL) {
  step_id <- substep$step_id
  eng <- tf_engine_get_safe(substep$engine_id, registry)
  kind <- tf_engine_kind(substep$engine_id, registry)

  is_locked <- isTRUE(substep$system_generated)
  disable_up <- isTRUE(disable_up) || is_locked
  disable_down <- isTRUE(disable_down) || is_locked

  btn_up_id <- sprintf("%s__btn_up", step_id)
  btn_dn_id <- sprintf("%s__btn_dn", step_id)
  btn_rm_id <- sprintf("%s__btn_rm", step_id)
  btn_adv_toggle_id <- sprintf("%s__btn_adv_toggle", step_id)
  adv_wrap_id <- sprintf("%s__adv_wrap", step_id)

  if (is.null(eng)) {
    title <- substep$engine_id %||% "Unknown engine"
    return(
      tags$details(
        class = paste("tf-subdetails", if (is_locked) "tf-locked" else ""),
        open = if (isTRUE(open_state)) TRUE else NULL,
        `data-tf-details-id` = step_id,
        tags$summary(
          div(class = "tf-substep-badge", tf_letter(idx)),
          div(class = "tf-substep-title", title),
          div(class = "tf-substep-spacer"),
          if (is_locked) span(class = "tf-lock-badge", "Locked") else NULL
        ),
        div(class = "tf-subcard-bd", div(class = "tf-note", "Engine not found in registry."))
      )
    )
  }

  is_dp <- identical(substep$engine_id, "dataprocessor")
  dp_state <- NULL
  if (is_dp) {
    dp_state <- tryCatch(fromJSON(substep$params$plan_json %||% "", simplifyVector = FALSE),
                         error = function(e) tf_dp_default_plan())
    if (is.null(dp_state$n) || is.null(dp_state$substeps)) dp_state <- tf_dp_default_plan()
  }

  params_schema <- eng$params_schema %||% list()
  style_schema  <- eng$style_schema  %||% list()
  split_p <- tf_partition_schema(kind, params_schema)
  split_s <- tf_partition_schema(kind, style_schema, force_all_adv = TRUE)
  basic_params <- split_p$basic
  adv_params   <- split_p$adv
  basic_style  <- split_s$basic
  adv_style    <- split_s$adv
  has_basic <- (length(basic_params) + length(basic_style)) > 0
  has_adv   <- (length(adv_params) + length(adv_style)) > 0

  tags$details(
    class = paste("tf-subdetails", if (is_locked) "tf-locked" else ""),
    open = if (isTRUE(open_state)) TRUE else NULL,
    `data-tf-details-id` = step_id,
    tags$summary(
      div(class = "tf-substep-badge", tf_letter(idx)),
      div(class = "tf-substep-title", eng$label %||% eng$name %||% eng$id %||% substep$engine_id),
      div(class = "tf-substep-spacer"),
      if (is_locked) span(class = "tf-lock-badge", "Locked") else NULL,
      div(
        class = "tf-substep-actions",
        tags$button(
          id = btn_up_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (isTRUE(disable_up) || idx == 1) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&#8593;")
        ),
        tags$button(
          id = btn_dn_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (isTRUE(disable_down) || idx == n_steps) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&#8595;")
        ),
        tags$button(
          id = btn_rm_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (is_locked) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&times;")
        )
      )
    ),
    div(
      class = "tf-subcard-bd",
      div(class = "tf-section-label", "Options"),
      if (is_dp) {
        tagList(
          numericInput(
            sprintf("%s__dp_n", step_id),
            "# of functions (sub-steps)",
            value = dp_state$n %||% 1,
            min = 1, max = 25, step = 1
          ),
          uiOutput(sprintf("%s__dp_substeps_wrap", step_id)),
          tags$hr(),
          checkboxInput(
            sprintf("%s__dp_save", step_id),
            "Save processed Excel after this step",
            value = dp_state$save_processed_excel %||% TRUE
          )
        )
      } else {
        tagList(
          if (!has_basic) {
            div(class = "tf-note", if (has_adv) "No basic options. Use “Show more options”." else "No engine options.")
          } else {
            tagList(
              if (length(basic_params) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = basic_params,
                values_list = substep$params %||% list(),
                prefix = "p",
                schema_all = params_schema
              ) else NULL,
              if (length(basic_style) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = basic_style,
                values_list = substep$style %||% list(),
                prefix = "s",
                schema_all = style_schema
              ) else NULL
            )
          },
          if (has_adv) tagList(
            tags$hr(),
            tags$button(
              id = btn_adv_toggle_id, type = "button",
              class = "btn btn-default btn-sm action-button",
              onclick = sprintf("event.preventDefault(); TerpFlow.toggle('%s');", adv_wrap_id),
              "Show more options"
            ),
            div(
              id = adv_wrap_id,
              class = "tf-aes-wrap",
              style = "display:none;",
              div(class = "tf-section-label", "Advanced options"),
              if (length(adv_params) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = adv_params,
                values_list = substep$params %||% list(),
                prefix = "p",
                schema_all = params_schema
              ) else NULL,
              if (length(adv_style) > 0) tf_render_fields(
                step_id = step_id,
                kind = kind,
                schema = adv_style,
                values_list = substep$style %||% list(),
                prefix = "s",
                schema_all = style_schema
              ) else NULL
            )
          )
        )
      }
    )
  )
}

tf_container_card_ui <- function(step, idx, n_steps, registry, open_state = TRUE, substeps_ui = NULL) {
  step_id <- step$step_id
  eng <- tf_engine_get_safe(step$engine_id, registry) %||% list()
  substeps_locked <- identical(step$engine_id, "idquant")

  btn_up_id <- sprintf("%s__btn_up", step_id)
  btn_dn_id <- sprintf("%s__btn_dn", step_id)
  btn_rm_id <- sprintf("%s__btn_rm", step_id)

  tags$details(
    class = "tf-card tf-parent",
    open = if (isTRUE(open_state)) TRUE else NULL,
    `data-tf-details-id` = step_id,
    tags$summary(
      class = "tf-card-hd",
      div(class = "tf-step-badge", idx),
      div(class = "tf-step-title", eng$label %||% eng$name %||% eng$id %||% step$engine_id),
      div(class = "tf-step-spacer"),
      div(
        class = "tf-step-actions",
        tags$button(
          id = btn_up_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (idx == 1) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&#8593;")
        ),
        tags$button(
          id = btn_dn_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          disabled = if (idx == n_steps) "disabled" else NULL,
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&#8595;")
        ),
        tags$button(
          id = btn_rm_id, type = "button",
          class = "btn btn-default btn-xs action-button tf-iconbtn",
          onclick = "event.preventDefault(); event.stopPropagation();",
          HTML("&times;")
        )
      )
    ),
    div(
      class = "tf-card-bd",
      div(class = "tf-section-label", "Substeps"),
      div(
        class = "tf-container-controls",
        if (!isTRUE(substeps_locked)) {
          tags$button(
            type = "button",
            class = "btn btn-primary btn-sm",
            onclick = sprintf(
              "event.preventDefault(); Shiny.setInputValue('tf_container_add_substep', {parent_step_id:%s, nonce:Math.random()}, {priority:'event'});",
              jsonlite::toJSON(step_id, auto_unbox = TRUE)
            ),
            "Add substep"
          )
        } else {
          span(class = "tf-lock-badge", "Locked")
        }
      ),
      div(class = "tf-container-substeps", substeps_ui %||% NULL)
    )
  )
}

# -----------------------------
# Server
# -----------------------------
page_pipeline_server <- function(input, output, session, app_state = NULL, state = NULL) {
  
  registry <- if (exists("msterp_engine_registry", mode = "function")) {
    msterp_engine_registry()
  } else {
    stop("No engine registry found. Source R/engines/registry.R (msterp_engine_registry) before page_pipeline_server().")
  }
  engines <- registry$engines %||% list()
  
  
  if (is.null(session$userData$terpflow_rv)) {
    session$userData$terpflow_rv <- reactiveVal(msterp_terpflow_new("New Pipeline", pipeline_type = "proteomics", registry = registry))
  }
  flow_rv <- session$userData$terpflow_rv
  
  if (is.null(session$userData$tf_built_rv)) {
    session$userData$tf_built_rv <- reactiveVal(NULL)
  }
  built_rv <- session$userData$tf_built_rv
  if (is.null(session$userData$tf_dirty_rv)) {
    session$userData$tf_dirty_rv <- reactiveVal(FALSE)
  }
  dirty_rv <- session$userData$tf_dirty_rv
  if (is.null(session$userData$tf_last_built_at)) {
    session$userData$tf_last_built_at <- reactiveVal(NULL)
  }
  last_built_at_rv <- session$userData$tf_last_built_at
  
  # UI mode + load mode + filename
  if (is.null(session$userData$tf_ui_mode_rv)) {
    session$userData$tf_ui_mode_rv <- reactiveVal("home")  # "home" | "editor"
  }
  ui_mode_rv <- session$userData$tf_ui_mode_rv
  
  if (is.null(session$userData$tf_load_mode_rv)) {
    session$userData$tf_load_mode_rv <- reactiveVal(NULL)  # NULL | "edit" | "duplicate" | "new"
  }
  load_mode_rv <- session$userData$tf_load_mode_rv
  
  if (is.null(session$userData$tf_loaded_file_rv)) {
    session$userData$tf_loaded_file_rv <- reactiveVal(NULL) # list(name, base)
  }
  loaded_file_rv <- session$userData$tf_loaded_file_rv

  # Pipeline type reactive value
  if (is.null(session$userData$tf_pipeline_type_rv)) {
    session$userData$tf_pipeline_type_rv <- reactiveVal("proteomics")
  }
  pipeline_type_rv <- session$userData$tf_pipeline_type_rv

  # Non-reactive open-state store
  if (is.null(session$userData$tf_open_env)) {
    session$userData$tf_open_env <- new.env(parent = emptyenv())
  }
  open_env <- session$userData$tf_open_env
  
  tf_get_open <- function(id, default = TRUE) {
    v <- tryCatch(get(id, envir = open_env, inherits = FALSE), error = function(e) NULL)
    if (is.null(v)) default else isTRUE(v)
  }
  tf_set_open <- function(id, value) assign(id, isTRUE(value), envir = open_env)
  tf_clear_open <- function() {
    nms <- ls(open_env, all.names = TRUE)
    if (length(nms) > 0) rm(list = nms, envir = open_env)
  }
  
  observeEvent(input$tf_details_state, {
    st <- input$tf_details_state
    if (is.null(st) || is.null(st$id)) return()
    tf_set_open(as.character(st$id), isTRUE(st$open))
  }, ignoreInit = TRUE)
  
  invalidate_built <- function() {
    if (!is.null(built_rv())) built_rv(NULL)
    if (!is.null(last_built_at_rv())) dirty_rv(TRUE)
  }
  
  # Add-step queue
  if (is.null(session$userData$tf_add_queue)) {
    session$userData$tf_add_queue <- reactiveVal(character())
  }
  add_queue_rv <- session$userData$tf_add_queue
  
  ensure_section_id <- function(flow) {
    if (is.null(flow$sections) || length(flow$sections) == 0) {
      flow$sections <- list(list(
        section_id = msterp_make_id("sec"),
        title = "Section 1",
        description = "",
        order = 1L
      ))
    }
    flow$sections[[1]]$section_id
  }
  
  # --- Enter editor helper (ensures input exists before updateTextInput) ---
  enter_editor <- function(flow, mode = c("new", "edit", "duplicate"), loaded_file = NULL) {
    mode <- match.arg(mode)

    # RULE: Force peptide_analysis to be first step if present
    flow <- tf_enforce_peptide_analysis_first(flow)

    flow_rv(flow)
    built_rv(NULL)
    tf_clear_open()
    dirty_rv(FALSE)
    last_built_at_rv(NULL)

    load_mode_rv(mode)
    loaded_file_rv(loaded_file)
    ui_mode_rv("editor")

    # Sync pipeline_type_rv from flow
    pipeline_type_rv(flow$pipeline_type %||% "proteomics")

    nm <- flow$pipeline_name %||% ""
    session$onFlushed(function() {
      updateTextInput(session, "pipeline_name", value = nm)
    }, once = TRUE)
  }
  
  # -----------------------------
  # Root UI (Home vs Editor)
  # -----------------------------
  output$tf_root_ui <- renderUI({
    mode <- ui_mode_rv()

    if (!identical(mode, "editor")) {
      pick <- load_mode_rv()
      show_file <- is.character(pick) && length(pick) == 1 && pick %in% c("edit", "duplicate")

      return(
        div(
          class = "tf-home",
          div(
            class = "tf-home-card",
            div(
              class = "tf-home-actions",
              selectInput("tf_pipeline_type_gate", "Data Type",
                choices = c("Proteomics" = "proteomics",
                            "Metabolomics" = "metabolomics",
                            "Multi-Dataset" = "multi"),
                selected = pipeline_type_rv()
              ),
              actionButton("tf_home_new", "Create new pipeline", class = "btn btn-primary"),
              actionButton("tf_home_edit", "Edit current pipeline", class = "btn btn-default"),
              actionButton("tf_home_dup", "Duplicate current pipeline and edit", class = "btn btn-default"),

              if (show_file) div(
                class = "tf-home-file",
                fileInput("tf_load_terpflow", label = NULL, accept = c(".terpflow")),
                div(class = "tf-home-note", "Upload a .terpflow file to continue.")
              ) else NULL
            )
          )
        )
      )
    }

    ui <- div(
      div(
        class = "tf-topbar",
        div(class = "tf-topbar-title", "Terpflow"),
        actionButton("tf_exit", "Exit", class = "tf-exit-btn")
      ),
      div(
        class = "tf-main",
        div(
          class = "tf-sidebar",
          textInput("pipeline_name", label = NULL, placeholder = "Name of new pipeline"),
          {
            pt <- flow_rv()$pipeline_type %||% "proteomics"
            pt_label <- c(proteomics = "Proteomics", metabolomics = "Metabolomics", multi = "Multi-Dataset")[pt] %||% pt
            tags$div(class = "tf-type-badge", pt_label)
          },
          textInput("tf_step_search", label = NULL, placeholder = "Search steps"),
          actionButton("add_step", "Add new step", class = "btn btn-primary"),
          tags$div(style = "height: 8px;"),

          actionButton("tf_build", "Build .terpflow", class = "btn btn-primary"),
          tags$div(style = "height: 8px;"),

          div(class = "tf-status", uiOutput("tf_status_line")),
          tags$div(style = "height: 8px;"),

          uiOutput("tf_download_ui")
        ),
        div(class = "tf-canvas", uiOutput("pipeline_canvas_steps"))
      )
    )
    ui
  })
  
  
  # -----------------------------
  # Home actions
  # -----------------------------
  observeEvent(input$tf_home_new, {
    pt <- input$tf_pipeline_type_gate %||% "proteomics"
    flow <- msterp_terpflow_new("New Pipeline", pipeline_type = pt, registry = registry)
    enter_editor(flow, mode = "new", loaded_file = NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_home_edit, {
    load_mode_rv("edit")
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_home_dup, {
    load_mode_rv("duplicate")
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_load_terpflow, {
    f <- input$tf_load_terpflow
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return()

    mode <- load_mode_rv() %||% NULL
    if (!mode %in% c("edit", "duplicate")) return()

    res <- tryCatch({
      flow <- msterp_terpflow_load(f$datapath, registry = registry)

      # Ensure pipeline_type exists (backwards compat)
      valid_types <- c("proteomics", "metabolomics", "multi")
      if (is.null(flow$pipeline_type) || !flow$pipeline_type %in% valid_types) {
        flow$pipeline_type <- "proteomics"
      }

      # file meta
      nm <- f$name %||% "pipeline.terpflow"
      base <- sub("\\.terpflow$", "", nm, ignore.case = TRUE)
      base <- gsub("[^A-Za-z0-9._-]+", "_", base)

      if (identical(mode, "duplicate")) {
        flow$pipeline_id <- msterp_make_id("terpflow")
        flow$created <- Sys.time()
        flow$pipeline_name <- paste0(flow$pipeline_name %||% base, "_copy")
      }

      list(flow = flow, meta = list(name = nm, base = base))
    }, error = function(e) {
      list(error = conditionMessage(e))
    })

    if (!is.null(res$error)) {
      showModal(modalDialog(
        title = "Could not load .terpflow",
        div(class = "tf-note", res$error),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    enter_editor(res$flow, mode = mode, loaded_file = res$meta)
  }, ignoreInit = TRUE)
  
  # Exit editor -> Home (do not stop app)
  observeEvent(input$tf_exit, {
    ui_mode_rv("home")
    load_mode_rv(NULL)
  }, ignoreInit = TRUE)

  # Pipeline type changes (from home gate selector)
  observeEvent(input$tf_pipeline_type_gate, {
    pipeline_type_rv(input$tf_pipeline_type_gate)
  }, ignoreInit = TRUE)

  # If pipeline name changes, invalidate built status
  observeEvent(input$pipeline_name, {
    invalidate_built()
  }, ignoreInit = TRUE)

  observeEvent(flow_rv(), {
    if (!is.null(last_built_at_rv())) dirty_rv(TRUE)
  }, ignoreInit = TRUE)
  
  # -----------------------------
  # DP: plan parsing helpers
  # -----------------------------
  get_dp_plan <- function(step) {
    pj <- step$params$plan_json %||% NULL
    if (is.null(pj) || !nzchar(pj)) return(tf_dp_default_plan())
    out <- tryCatch(fromJSON(pj, simplifyVector = FALSE), error = function(e) tf_dp_default_plan())
    if (is.null(out$n) || is.null(out$substeps)) tf_dp_default_plan() else out
  }
  
  build_dp_plan_from_inputs <- function(step) {
    plan <- get_dp_plan(step)
    
    step_id <- step$step_id
    n_id    <- sprintf("%s__dp_n", step_id)
    save_id <- sprintf("%s__dp_save", step_id)
    
    n_val <- isolate(input[[n_id]]) %||% plan$n %||% 1
    n_val <- suppressWarnings(as.integer(n_val))
    if (length(n_val) == 0 || is.na(n_val)) n_val <- plan$n %||% 1
    n_val <- max(1L, min(25L, n_val))
    plan$n <- n_val
    
    if (!is.null(isolate(input[[save_id]]))) {
      plan$save_processed_excel <- isolate(input[[save_id]])
    } else {
      plan$save_processed_excel <- plan$save_processed_excel %||% TRUE
    }
    
    if (length(plan$substeps) < n_val) {
      for (k in (length(plan$substeps) + 1):n_val) {
        plan$substeps[[k]] <- list(
          op = "filter_threshold",
          opts = list(threshold = 0, direction = "below", all_data_columns = TRUE)
        )
      }
    }
    if (length(plan$substeps) > n_val) plan$substeps <- plan$substeps[seq_len(n_val)]
    
    for (k in seq_len(n_val)) {
      op_id <- sprintf("%s__dp_op_%d", step_id, k)
      op <- isolate(input[[op_id]]) %||% plan$substeps[[k]]$op %||% "filter_threshold"
      plan$substeps[[k]]$op <- op
      
      opts <- plan$substeps[[k]]$opts %||% list()
      mkid <- function(suffix) sprintf("%s__dp_%s_%d", step_id, suffix, k)
      
      if (op == "filter_threshold") {
        if (!is.null(isolate(input[[mkid("thr")]])))     opts$threshold <- isolate(input[[mkid("thr")]])
        if (!is.null(isolate(input[[mkid("thr_dir")]]))) opts$direction <- isolate(input[[mkid("thr_dir")]])
        opts$threshold <- opts$threshold %||% 0
        opts$direction <- opts$direction %||% "below"
        opts$all_data_columns <- TRUE
        
      } else if (op == "filter_prefix") {
        if (!is.null(isolate(input[[mkid("pref_col")]]))) opts$id_col <- isolate(input[[mkid("pref_col")]])
        if (!is.null(isolate(input[[mkid("pref_val")]]))) opts$prefix <- isolate(input[[mkid("pref_val")]])
        opts$id_col <- opts$id_col %||% "protein"
        if (!opts$id_col %in% c("gene", "protein")) opts$id_col <- "protein"
        opts$prefix <- opts$prefix %||% "Cont_"
        
      } else if (op == "filter_keyword") {
        if (!is.null(isolate(input[[mkid("kw_col")]]))) opts$id_col <- isolate(input[[mkid("kw_col")]])
        if (!is.null(isolate(input[[mkid("kw_val")]]))) opts$keyword <- isolate(input[[mkid("kw_val")]])
        opts$id_col <- opts$id_col %||% "protein"
        if (!opts$id_col %in% c("gene", "protein")) opts$id_col <- "protein"
        opts$keyword <- opts$keyword %||% "Grand total"
        
      } else if (op == "aggregate_rows") {
        if (!is.null(isolate(input[[mkid("agg_col")]]))) opts$id_col <- isolate(input[[mkid("agg_col")]])
        opts$id_col <- opts$id_col %||% "protein"
        # Allow "peptide" for aggregate_rows inside Peptide Analysis container
        if (!opts$id_col %in% c("gene", "protein", "peptide")) opts$id_col <- "protein"
        
      } else if (op == "average_rows") {
        if (!is.null(isolate(input[[mkid("avg_col")]])))  opts$id_col <- isolate(input[[mkid("avg_col")]])
        if (!is.null(isolate(input[[mkid("avg_type")]]))) opts$mean_type <- isolate(input[[mkid("avg_type")]])
        opts$id_col <- opts$id_col %||% "protein"
        if (!opts$id_col %in% c("gene", "protein")) opts$id_col <- "protein"
        opts$mean_type <- opts$mean_type %||% "arithmetic"
        
      } else if (op == "tag_remove_contaminants") {
        # Default to protein (contaminant lists are protein-centric);
        # engine falls back to protein column if resolved column not found
        opts$id_col <- "protein"
        
      } else if (op == "impute") {
        if (!is.null(isolate(input[[mkid("imp_method")]]))) opts$method <- isolate(input[[mkid("imp_method")]])
        opts$method <- opts$method %||% "flat"

        if (identical(opts$method, "flat")) {
          if (!is.null(isolate(input[[mkid("imp_flat")]]))) opts$flat_value <- isolate(input[[mkid("imp_flat")]])
          opts$flat_value <- opts$flat_value %||% 10
          opts$multiplier <- NULL
        } else if (identical(opts$method, "min_mult")) {
          if (!is.null(isolate(input[[mkid("imp_mult")]]))) opts$multiplier <- isolate(input[[mkid("imp_mult")]])
          opts$multiplier <- opts$multiplier %||% 0.1
          opts$flat_value <- NULL
        }

      } else if (op == "filter_pct_missing") {
        if (!is.null(isolate(input[[mkid("pct_min")]]))) opts$min_pct_present <- isolate(input[[mkid("pct_min")]])
        opts$min_pct_present <- opts$min_pct_present %||% 50
      }
      
      plan$substeps[[k]]$opts <- opts
    }
    
    plan
  }

  find_step_any <- function(flow, step_id) {
    for (s in (flow$steps %||% list())) {
      if (identical(s$step_id, step_id)) return(s)
      if (identical(tolower(s$type %||% "engine"), "container")) {
        for (ss in (s$substeps %||% list())) {
          if (identical(ss$step_id, step_id)) return(ss)
        }
      }
    }
    NULL
  }
  
  # -----------------------------
  # Volcano GO-ORA multi-config state
  # (Defined here so collect_flow_from_inputs can access it)
  # -----------------------------
  goora_configs_rv <- reactiveValues(map = list())  # step_id -> list of configs

  # -----------------------------
  # Collect flow from inputs (unchanged logic)
  # -----------------------------
  collect_flow_from_inputs <- function(input, draft) {
    draft <- draft %||% list()

    if (!is.null(input$pipeline_name) && nzchar(input$pipeline_name)) {
      draft$pipeline_name <- input$pipeline_name
    }

    # Preserve pipeline_type from draft (already synced via observeEvent)
    # This ensures pipeline_type persists through collect/commit cycle
    if (!is.null(draft$pipeline_type)) {
      # Keep existing pipeline_type (don't overwrite)
    }

    steps <- draft$steps %||% list()
    if (!is.list(steps)) steps <- list()
    
    for (i in seq_along(steps)) {
      st <- steps[[i]]
      if (!is.list(st)) next
      
      sid <- st$step_id %||% sprintf("step_%02d", i)
      eid <- st$engine_id %||% ""
      
      eng <- registry$engines[[eid]] %||% NULL
      if (is.null(eng)) { steps[[i]] <- st; next }

      step_type <- tolower(st$type %||% eng$type %||% "engine")
      if (identical(step_type, "container")) {
        subs <- st$substeps %||% list()
        if (!is.list(subs)) subs <- list()

        for (j in seq_along(subs)) {
          ss <- subs[[j]]
          if (!is.list(ss)) next

          ssid <- ss$step_id %||% sprintf("%s_sub_%02d", sid, j)
          sseid <- ss$engine_id %||% ""
          ss_eng <- registry$engines[[sseid]] %||% NULL
          if (is.null(ss_eng)) { subs[[j]] <- ss; next }

          if (identical(sseid, "dataprocessor")) {
            plan <- build_dp_plan_from_inputs(ss)
            ss$params <- ss$params %||% list()
            if (!is.list(ss$params)) ss$params <- list()
            ss$params$plan_json <- jsonlite::toJSON(plan, auto_unbox = TRUE)
            ss$params$operation <- plan$substeps[[1]]$op %||% ss$params$operation %||% "filter_threshold"
            subs[[j]] <- ss
            next
          }

          ss$params <- ss$params %||% list(); if (!is.list(ss$params)) ss$params <- list()
          ss$style <- ss$style %||% list(); if (!is.list(ss$style)) ss$style <- list()

          for (f in tf_schema_fields(ss_eng$params_schema %||% list())) {
            if (!is.list(f) || is.null(f$name)) next
            id <- sprintf("%s__p_%s", ssid, f$name)
            v <- input[[id]]
            if (!is.null(v)) ss$params[[f$name]] <- v
          }
          for (f in tf_schema_fields(ss_eng$style_schema %||% list())) {
            if (!is.list(f) || is.null(f$name)) next
            if (isTRUE(f$hidden)) next
            id <- sprintf("%s__s_%s", ssid, f$name)
            v <- input[[id]]
            if (!is.null(v)) ss$style[[f$name]] <- v
          }

          subs[[j]] <- ss
        }

        st$substeps <- subs
        steps[[i]] <- st
        next
      }
      
      if (identical(eid, "dataprocessor")) {
        plan <- build_dp_plan_from_inputs(st)
        st$params <- st$params %||% list()
        if (!is.list(st$params)) st$params <- list()
        st$params$plan_json <- jsonlite::toJSON(plan, auto_unbox = TRUE)
        st$params$operation <- plan$substeps[[1]]$op %||% st$params$operation %||% "filter_threshold"
        steps[[i]] <- st
        next
      }
      
      st$params <- st$params %||% list(); if (!is.list(st$params)) st$params <- list()
      st$style <- st$style %||% list(); if (!is.list(st$style)) st$style <- list()

      # Collect params (compute-time parameters)
      for (f in tf_schema_fields(eng$params_schema %||% list())) {
        if (!is.list(f) || is.null(f$name)) next
        id <- sprintf("%s__p_%s", sid, f$name)
        v <- input[[id]]
        if (!is.null(v)) st$params[[f$name]] <- v
      }

      # Collect style (viewer-time parameters) - stored in .terpflow and written to step.json at run time
      # These become the "Pipeline Advanced Options" defaults for Result Viewer
      for (f in tf_schema_fields(eng$style_schema %||% list())) {
        if (!is.list(f) || is.null(f$name)) next
        if (isTRUE(f$hidden)) next  # Skip hidden fields
        id <- sprintf("%s__s_%s", sid, f$name)
        v <- input[[id]]
        if (!is.null(v)) st$style[[f$name]] <- v
      }
      
      
      if (eid %in% c("volcano", "pca", "rankplot")) {

        enabled <- isTRUE(input[[sprintf("%s__paired_enable", sid)]])
        
        if (identical(eid, "pca")) {
          loadings_on <- isTRUE(input[[sprintf("%s__p_loadings_corr", sid)]])
          enabled <- enabled && loadings_on
        }
        
        if (identical(eid, "volcano")) {
          peid <- if (pipeline_type_rv() == "metabolomics") "msea" else "goora"
          peng <- registry$engines[[peid]] %||% NULL

          # Collect global params (min_term_size, max_terms) from paired card
          global_params <- list()
          p_style <- (st$paired %||% list())$style %||% list(); if (!is.list(p_style)) p_style <- list()

          if (!is.null(peng)) {
            for (f in tf_schema_fields(peng$params_schema %||% list())) {
              if (!is.list(f) || is.null(f$name)) next
              # Only collect global params (min_term_size, max_terms), not per-config ones
              if (f$name %in% c("min_term_size", "max_terms")) {
                id <- sprintf("%s__paired_%s__p_%s", sid, peid, f$name)
                v <- input[[id]]
                if (!is.null(v)) global_params[[f$name]] <- v
              }
            }
            # Collect style for paired engine
            for (f in tf_schema_fields(peng$style_schema %||% list())) {
              if (!is.list(f) || is.null(f$name)) next
              if (isTRUE(f$hidden)) next
              id <- sprintf("%s__paired_%s__s_%s", sid, peid, f$name)
              v <- input[[id]]
              if (!is.null(v)) p_style[[f$name]] <- v
            }
          }

          # Collect GO-ORA configurations - use reactive values as source of truth
          # This ensures removed configs stay removed (stale inputs don't re-add them)
          configs <- goora_configs_rv$map[[sid]]

          if (!is.null(configs) && length(configs) > 0) {
            # Update config values from current UI inputs (if they exist)
            for (cfg_idx in seq_along(configs)) {
              cfg_prefix <- sprintf("%s__goora_cfg_%d", sid, cfg_idx)
              name_val <- input[[sprintf("%s_name", cfg_prefix)]]
              if (!is.null(name_val)) {
                configs[[cfg_idx]]$name <- name_val
                configs[[cfg_idx]]$fdr_cutoff <- input[[sprintf("%s_fdr", cfg_prefix)]] %||% configs[[cfg_idx]]$fdr_cutoff
                configs[[cfg_idx]]$min_overlap <- input[[sprintf("%s_min_overlap", cfg_prefix)]] %||% configs[[cfg_idx]]$min_overlap
                configs[[cfg_idx]]$include_unique_in_sig <- isTRUE(input[[sprintf("%s_include_unique", cfg_prefix)]])
              }
            }
          } else {
            # Initial load: scan inputs or create default
            configs <- list()
            cfg_idx <- 1
            while (TRUE) {
              cfg_prefix <- sprintf("%s__goora_cfg_%d", sid, cfg_idx)
              name_val <- input[[sprintf("%s_name", cfg_prefix)]]
              if (is.null(name_val)) break
              configs[[cfg_idx]] <- list(
                config_id = sprintf("cfg_%d", cfg_idx),
                name = name_val,
                fdr_cutoff = input[[sprintf("%s_fdr", cfg_prefix)]] %||% 0.05,
                min_overlap = input[[sprintf("%s_min_overlap", cfg_prefix)]] %||% 3,
                include_unique_in_sig = isTRUE(input[[sprintf("%s_include_unique", cfg_prefix)]])
              )
              cfg_idx <- cfg_idx + 1
            }
            if (length(configs) == 0) {
              configs <- list(list(
                config_id = "cfg_1",
                name = "Default",
                fdr_cutoff = 0.05,
                min_overlap = 3,
                include_unique_in_sig = FALSE
              ))
            }
            goora_configs_rv$map[[sid]] <- configs
          }

          st$paired <- list(
            enabled = enabled,
            engine_id = peid,
            global_params = global_params,
            style = p_style,
            configs = configs
          )
          
        } else if (identical(eid, "pca")) {

          if (pipeline_type_rv() == "metabolomics") {
            cand <- c("msea")
            if ("pathway_fcs" %in% (names(registry$engines) %||% character())) cand <- c(cand, "pathway_fcs")
          } else {
            cand <- c("goora")
            if ("1dgofcs" %in% (names(registry$engines) %||% character())) cand <- c(cand, "1dgofcs")
            if ("2dgofcs" %in% (names(registry$engines) %||% character())) cand <- c(cand, "2dgofcs")
          }
          cand <- unique(cand)
          cand <- cand[cand %in% names(registry$engines)]

          engines_cfg <- list()

          for (peid in cand) {
            peng <- registry$engines[[peid]] %||% NULL
            chk_id <- sprintf("%s__paired_do_%s", sid, peid)
            do_it <- enabled && isTRUE(input[[chk_id]])

            p_params <- ((st$paired %||% list())$engines %||% list())[[peid]]$params %||% list()
            if (!is.list(p_params)) p_params <- list()
            p_style <- ((st$paired %||% list())$engines %||% list())[[peid]]$style %||% list()
            if (!is.list(p_style)) p_style <- list()

            if (!is.null(peng)) {
              for (f in tf_schema_fields(peng$params_schema %||% list())) {
                if (!is.list(f) || is.null(f$name)) next
                id <- sprintf("%s__paired_%s__p_%s", sid, peid, f$name)
                v <- input[[id]]
                if (!is.null(v)) p_params[[f$name]] <- v
              }
              # Collect style for paired engine
              for (f in tf_schema_fields(peng$style_schema %||% list())) {
                if (!is.list(f) || is.null(f$name)) next
                if (isTRUE(f$hidden)) next
                id <- sprintf("%s__paired_%s__s_%s", sid, peid, f$name)
                v <- input[[id]]
                if (!is.null(v)) p_style[[f$name]] <- v
              }
            }

            engines_cfg[[peid]] <- list(
              enabled = do_it,
              params = p_params,
              style = p_style
            )
          }
          
          enabled_ids <- names(Filter(function(x) isTRUE(x$enabled), engines_cfg))
          legacy_engine_id <- if (length(enabled_ids) > 0) enabled_ids[[1]] else
            (if (pipeline_type_rv() == "metabolomics") "msea" else "goora")
          
          st$paired <- list(
            enabled = enabled,
            engine_id = legacy_engine_id,
            engines = engines_cfg
          )
        } else if (identical(eid, "rankplot")) {
          # Rankplot paired analysis: enrichment on highlighted entities per group
          # Uses highlight thresholds from params (topn_top, topn_bottom, threshold_above, threshold_below)
          st$paired <- list(
            enabled = enabled,
            engine_id = if (pipeline_type_rv() == "metabolomics") "msea" else "goora",
            params = list(
              fdr_cutoff = 0.05,
              min_term_size = 5,
              max_terms = 20
            ),
            style = list()
          )
        }
      }
      steps[[i]] <- st
    }
    
    draft$steps <- steps
    draft
  }
  
  commit_draft <- function() {
    invalidate_built()
    flow_rv(collect_flow_from_inputs(input, flow_rv()))
    invisible(NULL)
  }
  
  # -----------------------------
  # Bulk add step modal
  # -----------------------------
  output$tf_engine_list <- renderUI({
    ids <- tf_engine_ids(registry, pipeline_type = pipeline_type_rv())
    blocks <- list()
    cur_cat <- NULL
    
    for (engine_id in ids) {
      if (engine_id %in% TF_PAIRED_ONLY_ENGINES) next
      e <- engines[[engine_id]]
      cat <- e$category %||% "other"
      
      if (!identical(cat, cur_cat)) {
        cur_cat <- cat
        blocks[[length(blocks) + 1L]] <- div(
          style = "font-weight:700; margin: 8px 0 6px 0;",
          toupper(cur_cat)
        )
      }
      
      # Show "dSILAC" badge for engines that require SILAC data
      req <- e$requirements %||% list()
      silac_badge <- if (isTRUE(req$requires_silac)) {
        tags$span(class = "tf-silac-badge", "dSILAC")
      } else NULL

      blocks[[length(blocks) + 1L]] <- div(
        class = "tf-addrow",
        div(class = "tf-addname", e$label %||% e$name %||% e$id %||% engine_id, silac_badge),
        div(class = "tf-addspacer"),
        tags$button(
          type = "button",
          class = "btn btn-default tf-mini",
          onclick = sprintf(
            "Shiny.setInputValue('tf_add_engine', %s, {priority:'event'});",
            jsonlite::toJSON(engine_id, auto_unbox = TRUE)
          ),
          "+"
        )
      )
    }
    
    div(class = "tf-addstep-box", blocks)
  })
  
  output$tf_engine_queue <- renderUI({
    q <- add_queue_rv()
    if (length(q) == 0) {
      return(div(class = "tf-addstep-box", div(class = "tf-note", "No engines queued yet. Click + on the left.")))
    }
    
    rows <- lapply(seq_along(q), function(i) {
      engine_id <- q[[i]]
      e <- engines[[engine_id]]
      
      div(
        class = "tf-addrow",
        div(class = "tf-addname", e$label %||% engine_id),
        div(class = "tf-addspacer"),
        tags$button(
          type = "button",
          class = "btn btn-default tf-mini",
          onclick = sprintf("Shiny.setInputValue('tf_qrm_idx', %d, {priority:'event'});", i),
          "×"
        )
      )
    })
    
    div(class = "tf-addstep-box", rows)
  })
  
  observeEvent(input$add_step, {
    add_queue_rv(character())
    
    showModal(modalDialog(
      title = "Bulk add steps",
      div(
        class = "tf-addstep-wrap",
        div(class = "tf-addstep-col",
            div(style = "font-weight:700; margin-bottom:6px;", "Engines"),
            uiOutput("tf_engine_list")
        ),
        div(class = "tf-addstep-col",
            div(style = "font-weight:700; margin-bottom:6px;", "Queued"),
            uiOutput("tf_engine_queue")
        )
      ),
      footer = tagList(
        actionButton("tf_queue_clear", "Clear queue", class = "btn btn-default"),
        actionButton("tf_queue_add", "Add to pipeline", class = "btn btn-primary"),
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_queue_clear, {
    add_queue_rv(character())
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_add_engine, {
    eid <- input$tf_add_engine
    if (is.null(eid) || !nzchar(eid)) return()
    if (eid %in% TF_PAIRED_ONLY_ENGINES) return()
    add_queue_rv(c(add_queue_rv(), eid))
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_qrm_idx, {
    idx <- suppressWarnings(as.integer(input$tf_qrm_idx))
    q <- add_queue_rv()
    if (is.na(idx) || idx < 1 || idx > length(q)) return()
    add_queue_rv(q[-idx])
  }, ignoreInit = TRUE)
  
  observeEvent(input$tf_queue_add, {
    q <- add_queue_rv()
    if (length(q) == 0) return()
    
    q <- q[!q %in% TF_PAIRED_ONLY_ENGINES]
    if (length(q) == 0) return()
    
    removeModal()
    commit_draft()
    
    flow <- flow_rv()
    section_id <- ensure_section_id(flow)
    
    for (engine_id in q) {
      flow <- msterp_terpflow_add_step(flow, section_id = section_id, engine_id = engine_id, registry = registry)

      if (engine_id == "dataprocessor") {
        last <- length(flow$steps)
        flow$steps[[last]]$params$plan_json <- toJSON(tf_dp_default_plan(), auto_unbox = TRUE)
        flow$steps[[last]]$params$operation <- "filter_threshold"
      }
    }

    # RULE: Force peptide_analysis to be first step if present in the pipeline
    flow <- tf_enforce_peptide_analysis_first(flow)

    flow_rv(flow)
    add_queue_rv(character())
  }, ignoreInit = TRUE)

  # -----------------------------
  # Container: add substep modal
  # -----------------------------
  observeEvent(input$tf_container_add_substep, {
    st <- input$tf_container_add_substep
    if (is.null(st) || is.null(st$parent_step_id)) return()
    parent_step_id <- as.character(st$parent_step_id)

    flow <- flow_rv()
    parent <- NULL
    for (s in (flow$steps %||% list())) {
      if (identical(s$step_id, parent_step_id)) { parent <- s; break }
    }
    if (is.null(parent)) return()

    eng <- registry$engines[[parent$engine_id %||% ""]] %||% NULL
    allowed <- (eng$allowed_child_engines %||% character())
    allowed <- allowed[allowed %in% names(registry$engines)]
    allowed <- allowed[!vapply(allowed, function(eid) identical(tolower((registry$engines[[eid]]$type %||% "engine")), "container"), logical(1))]

    if (length(allowed) == 0) {
      showModal(modalDialog(
        title = "Add substep",
        div(class = "tf-note", "No engines are allowed inside this container."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    rows <- lapply(allowed, function(engine_id) {
      e <- registry$engines[[engine_id]] %||% list()
      div(
        class = "tf-addrow",
        div(class = "tf-addname", e$label %||% e$name %||% e$id %||% engine_id),
        div(class = "tf-addspacer"),
        tags$button(
          type = "button",
          class = "btn btn-default tf-mini",
          onclick = sprintf(
            "Shiny.setInputValue('tf_container_add_engine', {parent_step_id:%s, engine_id:%s, nonce:Math.random()}, {priority:'event'});",
            jsonlite::toJSON(parent_step_id, auto_unbox = TRUE),
            jsonlite::toJSON(engine_id, auto_unbox = TRUE)
          ),
          "+"
        )
      )
    })

    showModal(modalDialog(
      title = "Add substep",
      div(class = "tf-addstep-box", rows),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tf_container_add_engine, {
    st <- input$tf_container_add_engine
    if (is.null(st) || is.null(st$parent_step_id) || is.null(st$engine_id)) return()
    parent_step_id <- as.character(st$parent_step_id)
    engine_id <- as.character(st$engine_id)
    if (!nzchar(parent_step_id) || !nzchar(engine_id)) return()

    removeModal()
    commit_draft()

    flow <- flow_rv()
    flow <- tryCatch(
      msterp_terpflow_add_substep(flow, parent_step_id = parent_step_id, engine_id = engine_id, registry = registry),
      error = function(e) flow
    )

    # Ensure DataProcessor defaults when used as a substep
    for (i in seq_along(flow$steps %||% list())) {
      if (!identical(flow$steps[[i]]$step_id, parent_step_id)) next
      if (!identical(tolower(flow$steps[[i]]$type %||% "engine"), "container")) break
      subs <- flow$steps[[i]]$substeps %||% list()
      for (j in seq_along(subs)) {
        if (!identical(subs[[j]]$engine_id, "dataprocessor")) next
        subs[[j]]$params <- subs[[j]]$params %||% list()
        if (is.null(subs[[j]]$params$plan_json) || !nzchar(subs[[j]]$params$plan_json)) {
          subs[[j]]$params$plan_json <- toJSON(tf_dp_default_plan(), auto_unbox = TRUE)
          subs[[j]]$params$operation <- "filter_threshold"
        }
      }
      flow$steps[[i]]$substeps <- subs
      break
    }

    flow_rv(flow)
    invalidate_built()
  }, ignoreInit = TRUE)
  
  # -----------------------------
  # DP: remove substep handler
  # -----------------------------
  observeEvent(input$tf_dp_rm_sub, {
    st <- input$tf_dp_rm_sub
    if (is.null(st) || is.null(st$step_id) || is.null(st$idx)) return()
    
    step_id <- as.character(st$step_id)
    idx <- suppressWarnings(as.integer(st$idx))
    if (is.na(idx) || idx < 1) return()
    
    flow <- flow_rv()
    if (length(flow$steps %||% list()) == 0) return()

    updated <- FALSE

    for (i in seq_along(flow$steps)) {
      if (identical(flow$steps[[i]]$step_id, step_id) && identical(flow$steps[[i]]$engine_id, "dataprocessor")) {
        step <- flow$steps[[i]]
        plan <- get_dp_plan(step)  # Use stored plan, not rebuilt from potentially stale inputs
        if (idx > length(plan$substeps)) return()
        plan$substeps <- plan$substeps[-idx]
        if (length(plan$substeps) == 0) {
          plan$substeps <- list(list(
            op = "filter_threshold",
            opts = list(threshold = 0, direction = "below", all_data_columns = TRUE)
          ))
        }
        plan$n <- length(plan$substeps)
        step$params$plan_json <- toJSON(plan, auto_unbox = TRUE)
        step$params$operation <- plan$substeps[[1]]$op %||% step$params$operation
        flow$steps[[i]] <- step
        updated <- TRUE
        break
      }

      if (identical(tolower(flow$steps[[i]]$type %||% "engine"), "container")) {
        subs <- flow$steps[[i]]$substeps %||% list()
        for (j in seq_along(subs)) {
          if (!identical(subs[[j]]$step_id, step_id)) next
          if (!identical(subs[[j]]$engine_id, "dataprocessor")) return()

          step <- subs[[j]]
          plan <- get_dp_plan(step)  # Use stored plan, not rebuilt from potentially stale inputs
          if (idx > length(plan$substeps)) return()
          plan$substeps <- plan$substeps[-idx]
          if (length(plan$substeps) == 0) {
            plan$substeps <- list(list(
              op = "filter_threshold",
              opts = list(threshold = 0, direction = "below", all_data_columns = TRUE)
            ))
          }
          plan$n <- length(plan$substeps)
          step$params$plan_json <- toJSON(plan, auto_unbox = TRUE)
          step$params$operation <- plan$substeps[[1]]$op %||% step$params$operation

          subs[[j]] <- step
          flow$steps[[i]]$substeps <- subs
          updated <- TRUE
          break
        }
        if (updated) break
      }
    }

    if (!updated) return()

    flow_rv(flow)
    invalidate_built()
    updateNumericInput(session, sprintf("%s__dp_n", step_id), value = plan$n)
  }, ignoreInit = TRUE)
  
  # -----------------------------
  # Render canvas steps (parent + paired card)
  # -----------------------------
  output$pipeline_canvas_steps <- renderUI({
    flow <- flow_rv()
    steps <- flow$steps %||% list()
    search_term <- tolower(trimws(input$tf_step_search %||% ""))
    if (nzchar(search_term)) {
      step_label <- function(step) {
        eng <- tf_engine_get_safe(step$engine_id, registry) %||% list()
        paste(
          eng$label %||% "",
          eng$name %||% "",
          eng$id %||% "",
          step$engine_id %||% "",
          step$type %||% "",
          sep = " "
        )
      }
      step_matches <- function(step) {
        grepl(search_term, tolower(step_label(step)), fixed = TRUE)
      }
      step_has_match <- function(step) {
        eng <- tf_engine_get_safe(step$engine_id, registry) %||% list()
        step_type <- tolower(step$type %||% eng$type %||% "engine")
        if (step_matches(step)) return(TRUE)
        if (identical(step_type, "container")) {
          subs <- step$substeps %||% list()
          if (!is.list(subs)) return(FALSE)
          return(any(vapply(subs, step_matches, logical(1))))
        }
        FALSE
      }
      steps <- steps[vapply(steps, step_has_match, logical(1))]
    }
    n <- length(steps)
    
    if (n == 0) {
      msg <- if (nzchar(search_term)) "No steps match the search." else "No steps yet. Click \"Add new step\" to begin."
      return(div(class = "tf-note", msg))
    }
    
    out <- list()
    
    for (i in seq_len(n)) {
      s <- steps[[i]]

      parent_open <- tf_get_open(s$step_id, default = TRUE)
      eng <- tf_engine_get_safe(s$engine_id, registry) %||% list()
      step_type <- tolower(s$type %||% eng$type %||% "engine")

      if (identical(step_type, "container")) {
        subs <- s$substeps %||% list()
        if (!is.list(subs)) subs <- list()
        if (length(subs) > 0) {
          subs <- subs[order(vapply(subs, `[[`, integer(1), "order"))]
        }

        sub_uis <- lapply(seq_along(subs), function(j) {
          ss <- subs[[j]]
          ss_open <- tf_get_open(ss$step_id, default = TRUE)
          prev_sys <- (j > 1) && isTRUE((subs[[j - 1]]$system_generated %||% FALSE))
          next_sys <- (j < length(subs)) && isTRUE((subs[[j + 1]]$system_generated %||% FALSE))
          tf_container_substep_card_ui(
            parent_step_id = s$step_id,
            substep = ss,
            idx = j,
            n_steps = length(subs),
            registry = registry,
            open_state = ss_open,
            disable_up = (j == 1) || prev_sys,
            disable_down = (j == length(subs)) || next_sys,
            parent_container_engine_id = s$engine_id
          )
        })

        out[[length(out) + 1L]] <- tf_container_card_ui(
          step = s,
          idx = i,
          n_steps = n,
          registry = registry,
          open_state = parent_open,
          substeps_ui = tagList(sub_uis)
        )

        next
      }

      out[[length(out) + 1L]] <- tf_step_card_ui(step = s, idx = i, n_steps = n, registry = registry, open_state = parent_open, pipeline_type = pipeline_type_rv())
      
      if (s$engine_id %in% c("pca", "volcano")) {
        
        enable_id <- sprintf("%s__paired_enable", s$step_id)
        
        if (identical(s$engine_id, "pca")) {
          if (pipeline_type_rv() == "metabolomics") {
            cand <- c("msea")
            if ("pathway_fcs" %in% (names(registry$engines) %||% character())) cand <- c(cand, "pathway_fcs")
          } else {
            cand <- c("goora")
            if ("1dgofcs" %in% (names(registry$engines) %||% character())) cand <- c(cand, "1dgofcs")
            if ("2dgofcs" %in% (names(registry$engines) %||% character())) cand <- c(cand, "2dgofcs")
          }
          cand <- unique(cand)
          cand <- cand[cand %in% names(registry$engines)]
          
          if (length(cand) > 0) {
            loadings_id <- sprintf("%s__p_loadings_corr", s$step_id)
            base_cond <- sprintf("input['%s'] == true && input['%s'] == true", loadings_id, enable_id)
            
            out[[length(out) + 1L]] <- tagList(lapply(cand, function(peid) {
              chk_id <- sprintf("%s__paired_do_%s", s$step_id, peid)
              details_id <- sprintf("%s__paired__%s", s$step_id, peid)
              paired_open <- tf_get_open(details_id, default = TRUE)
              
              conditionalPanel(
                condition = sprintf("%s && input['%s'] == true", base_cond, chk_id),
                tf_paired_card_ui(
                  parent_step = s,
                  registry = registry,
                  paired_engine_id = peid,
                  details_id = details_id,
                  open_state = paired_open
                )
              )
            }))
          }
          
        } else if (identical(s$engine_id, "volcano")) {
          volcano_peid <- if (pipeline_type_rv() == "metabolomics") "msea" else "goora"
          if (volcano_peid %in% (names(registry$engines) %||% character())) {
            details_id <- sprintf("%s__paired__%s", s$step_id, volcano_peid)
            paired_open <- tf_get_open(details_id, default = TRUE)

            out[[length(out) + 1L]] <- conditionalPanel(
              condition = sprintf("input['%s'] == true", enable_id),
              tf_paired_card_ui(
                parent_step = s,
                registry = registry,
                paired_engine_id = volcano_peid,
                details_id = details_id,
                open_state = paired_open
              )
            )
          }
        }
      }
    }
    
    tagList(
      out,
      tags$script(HTML("if(window.TerpFlow && TerpFlow.bindDetails) TerpFlow.bindDetails();"))
    )
  })

  # -----------------------------
  # DP nested UI outputs
  # -----------------------------
  observe({
    flow <- flow_rv()
    if (length(flow$steps %||% list()) == 0) {
      return()
    }

    # Collect DP steps with their parent container engine_id (NULL if top-level)
    dp_steps <- list()
    for (s in (flow$steps %||% list())) {
      if (identical(s$engine_id, "dataprocessor")) {
        dp_steps[[length(dp_steps) + 1L]] <- list(step = s, parent_container_engine_id = NULL)
      }
      if (identical(tolower(s$type %||% "engine"), "container")) {
        for (ss in (s$substeps %||% list())) {
          if (identical(ss$engine_id, "dataprocessor")) {
            dp_steps[[length(dp_steps) + 1L]] <- list(step = ss, parent_container_engine_id = s$engine_id)
          }
        }
      }
    }

    for (dp_info in dp_steps) {
      local({
        s <- dp_info$step
        parent_engine_id <- dp_info$parent_container_engine_id
        step_id <- s$step_id
        wrap_id <- sprintf("%s__dp_substeps_wrap", step_id)

        output[[wrap_id]] <- renderUI({
          n_id <- sprintf("%s__dp_n", step_id)
          n_val <- input[[n_id]]
          if (is.null(n_val)) n_val <- 1
          n_val <- suppressWarnings(as.integer(n_val))
          if (length(n_val) == 0 || is.na(n_val)) n_val <- 1
          n_val <- max(1L, min(25L, n_val))

          flow_now <- flow_rv()
          step_now <- find_step_any(flow_now, step_id)
          if (is.null(step_now)) return(NULL)

          plan <- build_dp_plan_from_inputs(step_now)
          plan$n <- n_val

          if (length(plan$substeps) < n_val) {
            for (k in (length(plan$substeps) + 1):n_val) {
              plan$substeps[[k]] <- list(
                op = "filter_threshold",
                opts = list(threshold = 0, direction = "below", all_data_columns = TRUE)
              )
            }
          }
          if (length(plan$substeps) > n_val) plan$substeps <- plan$substeps[seq_len(n_val)]

          tagList(lapply(seq_len(n_val), function(k) {
            sub_id <- sprintf("%s__dp__sub_%d", step_id, k)
            tf_dp_substep_ui(step_id, k, plan,
                             open_state = tf_get_open(sub_id, default = TRUE),
                             parent_container_engine_id = parent_engine_id)
          }))
        })
      })
    }
  })

  # -----------------------------
  # Wire per-step buttons (up/down/remove)
  # -----------------------------
  obs <- reactiveValues(map = list())

  observe({
    flow <- flow_rv()
    ids <- vapply(flow$steps %||% list(), function(s) s$step_id, character(1))
    
    existing <- names(obs$map)
    removed <- setdiff(existing, ids)
    if (length(removed) > 0) {
      for (sid in removed) {
        handles <- obs$map[[sid]]
        if (is.list(handles)) {
          for (h in handles) if (!is.null(h) && is.function(h$destroy)) h$destroy()
        }
        obs$map[[sid]] <- NULL
      }
    }
    
    added <- setdiff(ids, existing)
    if (length(added) > 0) {
      for (sid in added) {
        local({
          step_id <- sid
          up_id <- sprintf("%s__btn_up", step_id)
          dn_id <- sprintf("%s__btn_dn", step_id)
          rm_id <- sprintf("%s__btn_rm", step_id)
          
          h_up <- observeEvent(input[[up_id]], {
            commit_draft()
            flow <- flow_rv()
            flow <- msterp_terpflow_move_step(flow, step_id = step_id, direction = "up")
            flow_rv(flow)
            invalidate_built()
          }, ignoreInit = TRUE)
          
          h_dn <- observeEvent(input[[dn_id]], {
            commit_draft()
            flow <- flow_rv()
            flow <- msterp_terpflow_move_step(flow, step_id = step_id, direction = "down")
            flow_rv(flow)
            invalidate_built()
          }, ignoreInit = TRUE)
          
          h_rm <- observeEvent(input[[rm_id]], {
            tryCatch(commit_draft(), error = function(e) NULL)
            flow <- flow_rv()
            flow <- msterp_terpflow_remove_step(flow, step_id = step_id)
            flow_rv(flow)
            invalidate_built()
          }, ignoreInit = TRUE)
          
          obs$map[[step_id]] <- list(h_up, h_dn, h_rm)
        })
      }
    }
  })

  # -----------------------------
  # Volcano GO-ORA multi-config UI and observers
  # (goora_configs_rv is defined earlier near collect_flow_from_inputs)
  # -----------------------------
  goora_obs <- reactiveValues(map = list())  # step_id -> observer handles
  goora_remove_obs <- reactiveValues(map = list())  # step_id -> list of remove button observer handles

  # Initialize/cleanup GO-ORA configs for volcano steps
  observe({
    flow <- flow_rv()
    volcano_ids <- vapply(
      Filter(function(s) identical(s$engine_id, "volcano"), flow$steps %||% list()),
      function(s) s$step_id,
      character(1)
    )
    if (length(volcano_ids) == 0) volcano_ids <- character(0)

    # Cleanup removed volcano steps
    existing <- names(goora_configs_rv$map)
    removed <- setdiff(existing, volcano_ids)
    for (sid in removed) {
      goora_configs_rv$map[[sid]] <- NULL
      handles <- goora_obs$map[[sid]]
      if (is.list(handles)) {
        for (h in handles) if (!is.null(h) && is.function(h$destroy)) h$destroy()
      }
      goora_obs$map[[sid]] <- NULL
    }

    # Initialize new volcano steps
    for (sid in volcano_ids) {
      if (is.null(goora_configs_rv$map[[sid]])) {
        # Load existing configs from step or set default
        step <- Filter(function(s) identical(s$step_id, sid), flow$steps)[[1]]
        existing_configs <- (step$paired %||% list())$configs
        if (is.null(existing_configs) || length(existing_configs) == 0) {
          # Migrate from old single-config format or create default
          old_include_unique <- isTRUE((step$paired %||% list())$include_unique_in_sig)
          old_fdr <- (step$paired %||% list())$params$fdr_cutoff %||% 0.05
          goora_configs_rv$map[[sid]] <- list(
            list(
              config_id = "cfg_1",
              name = "Default",
              fdr_cutoff = old_fdr,
              min_overlap = 3,
              include_unique_in_sig = old_include_unique
            )
          )
        } else {
          goora_configs_rv$map[[sid]] <- existing_configs
        }
      }
    }
  })

  # Render GO-ORA configs UI for each volcano step
  observe({
    flow <- flow_rv()
    for (s in (flow$steps %||% list())) {
      if (!identical(s$engine_id, "volcano")) next
      local({
        step_id <- s$step_id
        ui_id <- sprintf("%s__goora_configs_ui", step_id)

        output[[ui_id]] <- renderUI({
          configs <- goora_configs_rv$map[[step_id]] %||% list()

          if (length(configs) == 0) {
            return(div(class = "tf-note", "No configurations. Click 'Add Configuration' to add one."))
          }

          tagList(lapply(seq_along(configs), function(i) {
            cfg <- configs[[i]]
            cfg_prefix <- sprintf("%s__goora_cfg_%d", step_id, i)

            div(
              class = "tf-goora-config-row",
              style = "border: 1px solid var(--md-border); border-radius: 8px; padding: 10px; margin-bottom: 8px; background: var(--color-white);",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
                textInput(
                  sprintf("%s_name", cfg_prefix),
                  NULL,
                  value = cfg$name %||% sprintf("Config %d", i),
                  width = "200px"
                ),
                actionButton(
                  sprintf("%s_remove", cfg_prefix),
                  HTML("&times;"),
                  class = "btn btn-xs btn-danger",
                  style = "padding: 2px 8px;"
                )
              ),
              div(
                style = "display: flex; gap: 12px; flex-wrap: wrap;",
                div(
                  style = "flex: 1; min-width: 100px;",
                  numericInput(
                    sprintf("%s_fdr", cfg_prefix),
                    "FDR cutoff",
                    value = cfg$fdr_cutoff %||% 0.05,
                    min = 0, max = 1, step = 0.01,
                    width = "100%"
                  )
                ),
                div(
                  style = "flex: 1; min-width: 100px;",
                  numericInput(
                    sprintf("%s_min_overlap", cfg_prefix),
                    "Min overlap",
                    value = cfg$min_overlap %||% 3,
                    min = 1, step = 1,
                    width = "100%"
                  )
                ),
                div(
                  style = "flex: 2; min-width: 200px; display: flex; align-items: center; padding-top: 25px;",
                  checkboxInput(
                    sprintf("%s_include_unique", cfg_prefix),
                    "Include group-specific proteins",
                    value = isTRUE(cfg$include_unique_in_sig)
                  )
                )
              )
            )
          }))
        })
      })
    }
  })

  # Wire add/remove config button observers
  observe({
    flow <- flow_rv()
    volcano_ids <- vapply(
      Filter(function(s) identical(s$engine_id, "volcano"), flow$steps %||% list()),
      function(s) s$step_id,
      character(1)
    )
    if (length(volcano_ids) == 0) volcano_ids <- character(0)

    for (sid in volcano_ids) {
      if (!is.null(goora_obs$map[[sid]])) next  # Already wired

      local({
        step_id <- sid
        add_btn_id <- sprintf("%s__goora_add_config", step_id)

        # Add config button
        h_add <- observeEvent(input[[add_btn_id]], {
          configs <- goora_configs_rv$map[[step_id]] %||% list()
          new_idx <- length(configs) + 1
          configs[[new_idx]] <- list(
            config_id = sprintf("cfg_%d", new_idx),
            name = sprintf("Config %d", new_idx),
            fdr_cutoff = 0.05,
            min_overlap = 3,
            include_unique_in_sig = FALSE
          )
          goora_configs_rv$map[[step_id]] <- configs
        }, ignoreInit = TRUE)

        goora_obs$map[[step_id]] <- list(h_add)
      })
    }
  })

  # Wire remove config button observers (with proper cleanup to prevent duplicates)
  # Use observeEvent on goora_configs_rv$map to only trigger when configs actually change
  goora_remove_last_signature <- reactiveVal(NULL)

  observe({
    # Only react to flow changes (for volcano step detection)
    flow <- flow_rv()
    # Read configs in isolate to avoid reactive loop
    configs_map <- isolate(goora_configs_rv$map)

    # Build signature of current state
    volcano_ids <- character()
    for (s in (flow$steps %||% list())) {
      if (identical(s$engine_id, "volcano")) {
        volcano_ids <- c(volcano_ids, s$step_id)
      }
    }
    sig <- paste(volcano_ids, sapply(volcano_ids, function(sid) length(configs_map[[sid]] %||% list())), collapse = "|")

    # Skip if signature unchanged
    if (identical(sig, isolate(goora_remove_last_signature()))) return()
    goora_remove_last_signature(sig)

    for (step_id in volcano_ids) {
      configs <- configs_map[[step_id]] %||% list()

      # Destroy old observers for this step to prevent duplicates
      old_handles <- goora_remove_obs$map[[step_id]]
      if (is.list(old_handles)) {
        for (h in old_handles) {
          if (!is.null(h) && is.function(h$destroy)) h$destroy()
        }
      }

      # Create new observers for current configs
      new_handles <- list()
      for (i in seq_along(configs)) {
        local({
          idx <- i
          sid <- step_id
          remove_btn_id <- sprintf("%s__goora_cfg_%d_remove", sid, idx)

          h <- observeEvent(input[[remove_btn_id]], {
            cfgs <- goora_configs_rv$map[[sid]] %||% list()
            if (length(cfgs) > 1) {
              cfgs[[idx]] <- NULL
              # Re-index config_ids
              for (j in seq_along(cfgs)) {
                cfgs[[j]]$config_id <- sprintf("cfg_%d", j)
              }
              goora_configs_rv$map[[sid]] <- cfgs
            } else {
              showNotification("Cannot remove the last configuration.", type = "warning")
            }
          }, ignoreInit = TRUE)

          new_handles[[length(new_handles) + 1]] <<- h
        })
      }
      goora_remove_obs$map[[step_id]] <- new_handles
    }
  })

  # -----------------------------
  # Wire per-substep buttons (up/down/remove)
  # -----------------------------
  obs_sub <- reactiveValues(map = list())

  observe({
    flow <- flow_rv()
    if (length(flow$steps %||% list()) == 0) {
      # Still clean up stale substep observers when flow becomes empty
      existing <- names(obs_sub$map)
      for (sid in existing) {
        handles <- obs_sub$map[[sid]]
        if (is.list(handles)) {
          for (h in handles) if (!is.null(h) && is.function(h$destroy)) h$destroy()
        }
        obs_sub$map[[sid]] <- NULL
      }
      return()
    }

    parent_by_sub <- list()
    for (s in (flow$steps %||% list())) {
      if (!identical(tolower(s$type %||% "engine"), "container")) next
      for (ss in (s$substeps %||% list())) {
        if (is.null(ss$step_id) || !nzchar(ss$step_id)) next
        parent_by_sub[[ss$step_id]] <- s$step_id
      }
    }

    ids <- names(parent_by_sub)
    existing <- names(obs_sub$map)
    removed <- setdiff(existing, ids)
    if (length(removed) > 0) {
      for (sid in removed) {
        handles <- obs_sub$map[[sid]]
        if (is.list(handles)) {
          for (h in handles) if (!is.null(h) && is.function(h$destroy)) h$destroy()
        }
        obs_sub$map[[sid]] <- NULL
      }
    }

    added <- setdiff(ids, existing)
    if (length(added) > 0) {
      for (sid in added) {
        local({
          substep_id <- sid
          parent_step_id <- parent_by_sub[[sid]]

          up_id <- sprintf("%s__btn_up", substep_id)
          dn_id <- sprintf("%s__btn_dn", substep_id)
          rm_id <- sprintf("%s__btn_rm", substep_id)

          h_up <- observeEvent(input[[up_id]], {
            commit_draft()
            flow <- flow_rv()
            flow <- msterp_terpflow_move_substep(flow, parent_step_id = parent_step_id, substep_id = substep_id, direction = "up")
            flow_rv(flow)
            invalidate_built()
          }, ignoreInit = TRUE)

          h_dn <- observeEvent(input[[dn_id]], {
            commit_draft()
            flow <- flow_rv()
            flow <- msterp_terpflow_move_substep(flow, parent_step_id = parent_step_id, substep_id = substep_id, direction = "down")
            flow_rv(flow)
            invalidate_built()
          }, ignoreInit = TRUE)

          h_rm <- observeEvent(input[[rm_id]], {
            commit_draft()
            flow <- flow_rv()
            flow <- msterp_terpflow_remove_substep(flow, parent_step_id = parent_step_id, substep_id = substep_id)
            flow_rv(flow)
            invalidate_built()
          }, ignoreInit = TRUE)

          obs_sub$map[[substep_id]] <- list(h_up, h_dn, h_rm)
        })
      }
    }
  })

  # -----------------------------
  # Build / Export (sidebar)
  # -----------------------------
  observeEvent(input$tf_build, {
    built <- collect_flow_from_inputs(input, flow_rv())
    built_at <- Sys.time()
    built_rv(list(built_flow = built, built_at = built_at))
    last_built_at_rv(built_at)
    dirty_rv(FALSE)
  }, ignoreInit = TRUE)
  
  output$tf_status_line <- renderUI({
    last_built <- last_built_at_rv()
    built_now <- built_rv()
    dirty <- isTRUE(dirty_rv())

    if (is.null(last_built)) {
      return(div(class = "tf-status-line", div(class = "tf-status-flag", "Not built")))
    }

    status <- if (is.null(built_now) || dirty) "Needs rebuild" else "Built"
    flag_class <- if (dirty) "tf-status-flag dirty" else "tf-status-flag"
    div(
      class = "tf-status-line",
      div(class = flag_class, status),
      div(class = "tf-status-meta", paste("Last built:", format(last_built, "%Y-%m-%d %H:%M")))
    )
  })
  
  output$tf_download_ui <- renderUI({
    b <- built_rv()
    if (is.null(b) || isTRUE(dirty_rv())) return(NULL)
    downloadButton("export_pipeline_built", "Download .terpflow", class = "btn btn-default")
  })
  
  output$export_pipeline_built <- downloadHandler(
    filename = function() {
      meta <- loaded_file_rv()
      mode <- load_mode_rv() %||% NULL
      
      if (!is.null(meta) && mode %in% c("edit", "duplicate")) {
        base <- meta$base %||% "pipeline"
        if (identical(mode, "duplicate")) {
          if (!grepl("_copy$", base, ignore.case = TRUE)) base <- paste0(base, "_copy")
        }
        return(paste0(base, ".terpflow"))
      }
      
      b <- built_rv()
      flow <- if (!is.null(b)) b$built_flow else flow_rv()
      nm <- flow$pipeline_name %||% "pipeline"
      nm <- gsub("[^A-Za-z0-9._-]+", "_", nm)
      paste0(nm, ".terpflow")
    },
    content = function(file) {
      b <- built_rv()
      if (!is.null(b) && !is.null(b$built_flow)) {
        saveRDS(b$built_flow, file)
        return()
      }
      
      inp <- reactiveValuesToList(input)
      built <- collect_flow_from_inputs(inp, flow_rv())
      saveRDS(built, file)
    }
  )
}
