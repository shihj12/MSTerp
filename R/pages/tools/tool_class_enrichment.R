# R/pages/tools/tool_class_enrichment.R
# Chemical Class Enrichment Tool - Enrichment analysis of lipid classes, amino acids, etc.

tools_class_enrichment_defaults <- function() {
  eng <- msterp_engine_get("class_enrichment")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

# Safe input ID generator
tools_class_enrichment_safe_input_id <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+", "", x)
  if (!nzchar(x) || grepl("^[0-9]", x)) x <- paste0("id_", x)
  x
}

# Helper function to create editable class table UI for tools page
tools_class_enrichment_editable_table_ui <- function(
    id_prefix,
    df,
    term_id_col = "class_name",
    term_name_col = "class_name",
    hidden_term_ids = character(),
    term_labels_by_id = list(),
    metabolite_col_data = NULL
) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(div(class = "text-muted", "No classes to display."))
  }
  if (!(term_id_col %in% names(df))) {
    return(div(class = "text-danger", "Table is missing required columns for editing."))
  }

  term_ids <- as.character(df[[term_id_col]])
  cols <- names(df)

  # Columns to hide from display
  hide_cols <- c("metabolite_ids", "neglog10_fdr", "neglog10fdr", "n_term")

  # Build header: Visible, class_name, Search, then other columns
  other_cols <- setdiff(cols, c(term_id_col, hide_cols))
  if (term_name_col != term_id_col && term_name_col %in% other_cols) {
    other_cols <- setdiff(other_cols, term_name_col)
  }
  has_search <- !is.null(metabolite_col_data) && length(metabolite_col_data) == nrow(df)
  header_cols <- c("Visible", term_id_col, if (has_search) "Search" else NULL, other_cols)

  rows <- lapply(seq_len(nrow(df)), function(i) {
    term_id <- as.character(term_ids[[i]] %||% "")
    safe_term_id <- tools_class_enrichment_safe_input_id(term_id)

    vis_id <- paste0(id_prefix, "_vis_", safe_term_id)
    name_id <- paste0(id_prefix, "_name_", safe_term_id)

    default_name <- as.character(df[[term_id_col]][[i]] %||% "")
    display_name <- as.character(term_labels_by_id[[term_id]] %||% default_name)

    visible <- !(term_id %in% hidden_term_ids)

    # Build Search button HTML if metabolite data available
    search_btn <- NULL
    if (has_search) {
      metabolites <- as.character(metabolite_col_data[[i]] %||% "")
      metabolites_clean <- gsub("[,;|/]", "\n", metabolites)
      metabolites_clean <- gsub("\n+", "\n", metabolites_clean)
      metabolites_clean <- trimws(metabolites_clean)
      safe_term <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", display_name)))))
      safe_metabolites <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", metabolites_clean)))))
      search_btn <- tags$td(
        style = "text-align: center;",
        HTML(sprintf(
          '<button type="button" class="btn btn-xs btn-info class-search-metabolites-btn" data-term="%s" data-metabolites="%s" title="Search metabolites in HMDB"><i class="fa fa-search"></i></button>',
          safe_term, safe_metabolites
        ))
      )
    }

    tags$tr(
      tags$td(
        style = "width: 40px; text-align: center;",
        tags$div(
          style = "display: inline-block;",
          checkboxInput(vis_id, label = NULL, value = visible, width = "20px")
        )
      ),
      tags$td(
        style = "min-width: 150px;",
        textInput(
          name_id,
          label = NULL,
          value = display_name,
          width = "100%"
        )
      ),
      search_btn,
      lapply(other_cols, function(col) {
        val <- df[[col]][[i]]
        # Format numeric values
        if (is.numeric(val)) {
          if (col %in% c("fdr", "FDR", "p.adjust", "pvalue")) {
            val <- format(val, scientific = TRUE, digits = 2)
          } else {
            val <- format(val, digits = 3, nsmall = 2)
          }
        }
        tags$td(htmltools::htmlEscape(as.character(val %||% "")))
      })
    )
  })

  div(
    class = "tools-editable-class-table",
    tags$style(HTML("
      .tools-editable-class-table .form-group { margin-bottom: 0; }
      .tools-editable-class-table .checkbox { margin: 0; padding: 0; min-height: 0; }
      .tools-editable-class-table input[type='checkbox'] { margin: 0; }
    ")),
    tags$div(style = "overflow-x: auto; max-height: 400px; overflow-y: auto;",
             tags$table(
               class = "table table-sm table-striped",
               tags$thead(tags$tr(lapply(header_cols, function(h) {
                 if (h == "Visible") {
                   tags$th(style = "width: 50px; text-align: center;", h)
                 } else if (h == "Search") {
                   tags$th(style = "width: 50px; text-align: center;", h)
                 } else if (h == term_id_col) {
                   tags$th(style = "min-width: 150px;", h)
                 } else {
                   tags$th(h)
                 }
               }))),
               tags$tbody(rows)
             ))
  )
}

# Bind observers for editable class table
tools_class_enrichment_bind_editable_table <- function(
    id_prefix,
    df,
    term_id_col = "class_name",
    input,
    session,
    on_term_name_change = NULL,
    on_visibility_change = NULL
) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
  if (!(term_id_col %in% names(df))) return(invisible(NULL))

  if (is.null(session$userData$tools_editable_class_table_bound)) {
    session$userData$tools_editable_class_table_bound <- new.env(parent = emptyenv())
  }
  bound <- session$userData$tools_editable_class_table_bound

  term_ids <- unique(as.character(df[[term_id_col]]))
  term_ids <- term_ids[nzchar(term_ids)]

  for (term_id in term_ids) {
    local({
      term_id_local <- term_id
      safe_term_id <- tools_class_enrichment_safe_input_id(term_id_local)
      vis_id <- paste0(id_prefix, "_vis_", safe_term_id)
      name_id <- paste0(id_prefix, "_name_", safe_term_id)

      vis_key <- paste0("vis|", vis_id)
      if (!exists(vis_key, envir = bound, inherits = FALSE)) {
        assign(vis_key, TRUE, envir = bound)
        observeEvent(input[[vis_id]], {
          if (is.function(on_visibility_change)) {
            on_visibility_change(term_id_local, isTRUE(input[[vis_id]]))
          }
        }, ignoreInit = TRUE)
      }

      name_key <- paste0("name|", name_id)
      if (!exists(name_key, envir = bound, inherits = FALSE)) {
        assign(name_key, TRUE, envir = bound)
        observeEvent(input[[name_id]], {
          if (is.function(on_term_name_change)) {
            on_term_name_change(term_id_local, as.character(input[[name_id]] %||% ""))
          }
        }, ignoreInit = TRUE)
      }
    })
  }

  invisible(NULL)
}

# ============================================================
# Chemical Class Enrichment Tool UI
# ============================================================
tools_class_enrichment_ui <- function() {
  defs <- tools_class_enrichment_defaults()
  params_defaults <- defs$params %||% list()
  style_defaults <- defs$style %||% list()

  tagList(
    # JavaScript handlers
    tags$head(
      tags$script(HTML("
        // HMDB metabolite search button handler for class enrichment
        if (window.Shiny && !window.__tools_class_search_metabolites) {
          window.__tools_class_search_metabolites = true;
          $(document).on('click', '.class-search-metabolites-btn', function(e) {
            e.preventDefault();
            e.stopPropagation();
            var btn = e.target.closest('.class-search-metabolites-btn');
            if (!btn) return;
            var term = btn.getAttribute('data-term') || '';
            var metabolites = btn.getAttribute('data-metabolites') || '';
            if (!metabolites) return;
            Shiny.setInputValue('tools_class_search_metabolites_click', {
              term: term,
              metabolites: metabolites,
              ts: Date.now()
            });
          });
        }
      ")),
      tags$style(HTML("
        .class-search-metabolites-btn {
          padding: 2px 6px;
          font-size: 11px;
          cursor: pointer;
        }
      "))
    ),
    div(
      class = "top",
      actionButton("tools_class_enrichment_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("Chemical Class Enrichment", style = "margin: 0;")
    ),
    tags$p("Paste a metabolite list to run chemical class enrichment analysis against a MetaboBase."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("MetaboBase"),
        uiOutput("tools_class_enrichment_metabobase_status"),
        fileInput(
          "tools_class_enrichment_metabobase_file",
          "Load MetaboBase (.metabobase or .rds)",
          accept = c(".metabobase", ".rds"),
          placeholder = "No file selected"
        ),
        hr(),
        tags$h4("Metabolite Input"),
        textAreaInput(
          "tools_class_enrichment_metabolites",
          "Metabolite ID list (one per line)",
          rows = 10,
          placeholder = "HMDB0000001\nHMDB0000039\nC00001\nC00002"
        ),
        div(
          style = "display: flex; gap: 8px; margin-top: 5px;",
          actionButton("tools_class_enrichment_run", "Run Class Enrichment", class = "btn-primary btn-tool-action"),
          actionButton("tools_class_enrichment_reset", "Reset", class = "btn btn-default btn-tool-action")
        ),
        hr(),
        tools_collapse_section_ui(
          "tools_class_enrichment_params_section",
          "Parameters",
          open = FALSE,
          selectInput(
            "tools_class_enrichment_class_level",
            "Classification level",
            choices = c("Class" = "class", "Subclass" = "subclass", "Superclass" = "superclass"),
            selected = params_defaults$class_level %||% "class"
          ),
          numericInput(
            "tools_class_enrichment_fdr_cutoff",
            "FDR cutoff",
            value = params_defaults$fdr_cutoff %||% 0.05,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "tools_class_enrichment_min_class_size",
            "Min class size",
            value = params_defaults$min_class_size %||% 3,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_class_enrichment_max_terms",
            "Terms to show",
            value = params_defaults$max_terms %||% 20,
            min = 1,
            max = 200,
            step = 1
          )
        ),
        tools_collapse_section_ui(
          "tools_class_enrichment_plot_section",
          "Plot Options",
          open = FALSE,
          selectInput(
            "tools_class_enrichment_plot_type",
            "Plot type",
            choices = c("bar", "dot"),
            selected = style_defaults$plot_type %||% "bar"
          ),
          selectInput(
            "tools_class_enrichment_color_mode",
            "Coloring",
            choices = c("fdr", "flat"),
            selected = style_defaults$color_mode %||% "fdr"
          ),
          conditionalPanel(
            condition = "input.tools_class_enrichment_color_mode == 'fdr'",
            selectInput(
              "tools_class_enrichment_fdr_palette",
              "FDR color palette",
              choices = c("yellow_cap" = "Yellow (significant)", "blue_red" = "Blue-Red"),
              selected = style_defaults$fdr_palette %||% "yellow_cap"
            )
          ),
          conditionalPanel(
            condition = "input.tools_class_enrichment_color_mode == 'flat'",
            textInput(
              "tools_class_enrichment_flat_color",
              "Flat color (hex)",
              value = style_defaults$flat_color %||% "#B0B0B0"
            )
          ),
          sliderInput(
            "tools_class_enrichment_alpha",
            "Opacity",
            min = 0,
            max = 1,
            value = style_defaults$alpha %||% 0.8,
            step = 0.05
          ),
          checkboxInput(
            "tools_class_enrichment_flip_axis",
            "Flip horizontal axis",
            value = FALSE
          ),
          numericInput(
            "tools_class_enrichment_font_size",
            "Font size",
            value = style_defaults$font_size %||% 14,
            min = 6,
            max = 30,
            step = 1
          ),
          numericInput(
            "tools_class_enrichment_axis_text_size",
            "Axis text size",
            value = style_defaults$axis_text_size %||% 20,
            min = 6,
            max = 40,
            step = 1
          ),
          numericInput(
            "tools_class_enrichment_width",
            "Plot width (in)",
            value = style_defaults$width %||% 10,
            min = 2,
            max = 24,
            step = 0.5
          ),
          numericInput(
            "tools_class_enrichment_height",
            "Plot height (in)",
            value = style_defaults$height %||% 8,
            min = 2,
            max = 24,
            step = 0.5
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_class_enrichment_summary"),
        uiOutput("tools_class_enrichment_tabs")
      )
    )
  )
}

# ============================================================
# Chemical Class Enrichment Server Logic
# ============================================================
tools_class_enrichment_server <- function(input, output, session, app_state, rv, defs_class) {
  safe_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) default else v
  }

  safe_int <- function(x, default) {
    v <- suppressWarnings(as.integer(x))
    if (!is.finite(v) || v <= 0) default else v
  }

  parse_metabolite_input <- function(text) {
    if (is.null(text) || !nzchar(text)) return(character(0))
    lines <- unlist(strsplit(text, "\\r?\\n"))
    parts <- unlist(strsplit(lines, "[,;]"))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    unique(parts)
  }

  plot_dims_px <- function() {
    dpi <- 150
    w_in <- safe_num(input$tools_class_enrichment_width, defs_class$style$width %||% 10)
    h_in <- safe_num(input$tools_class_enrichment_height, defs_class$style$height %||% 8)

    max_px <- 2600
    w_px <- as.integer(round(w_in * dpi))
    h_px <- as.integer(round(h_in * dpi))

    scale <- min(1, max_px / max(w_px, h_px))
    list(
      w = as.integer(round(w_px * scale)),
      h = as.integer(round(h_px * scale))
    )
  }

  # Reset button handler
  observeEvent(input$tools_class_enrichment_reset, {
    rv$results <- NULL
    rv$rendered <- NULL
    rv$status_msg <- NULL
    rv$status_level <- NULL
    rv$input_count <- NULL
    rv$hidden_terms <- character()
    rv$term_labels <- list()
    rv$stored_params <- NULL
    rv$stored_metabolites <- NULL

    # Reset parameter inputs to defaults
    updateNumericInput(session, "tools_class_enrichment_fdr_cutoff", value = defs_class$params$fdr_cutoff %||% 0.05)
    updateNumericInput(session, "tools_class_enrichment_min_class_size", value = defs_class$params$min_class_size %||% 3)
    updateNumericInput(session, "tools_class_enrichment_max_terms", value = defs_class$params$max_terms %||% 20)
    updateTextAreaInput(session, "tools_class_enrichment_metabolites", value = "")
  }, ignoreInit = TRUE)

  # Handle HMDB metabolite search button clicks
  observeEvent(input$tools_class_search_metabolites_click, {
    req(input$tools_class_search_metabolites_click)
    metabolites <- input$tools_class_search_metabolites_click$metabolites

    if (!nzchar(metabolites)) return()

    metabolite_list <- unlist(strsplit(metabolites, "\n"))
    metabolite_list <- trimws(metabolite_list)
    metabolite_list <- metabolite_list[nzchar(metabolite_list)]

    if (length(metabolite_list) == 0) return()

    first_id <- metabolite_list[1]
    if (grepl("^HMDB", first_id, ignore.case = TRUE)) {
      url <- paste0("https://hmdb.ca/metabolites/", toupper(first_id))
    } else if (grepl("^C\\d{5}$", first_id)) {
      url <- paste0("https://www.genome.jp/entry/", first_id)
    } else {
      query <- paste(metabolite_list[1:min(3, length(metabolite_list))], collapse = " ")
      url <- paste0("https://hmdb.ca/unearth/q?query=", utils::URLencode(query, reserved = TRUE))
    }

    session$sendCustomMessage("tools_open_url", list(url = url))
  }, ignoreInit = TRUE)

  # Load MetaboBase from file
  observeEvent(input$tools_class_enrichment_metabobase_file, {
    req(input$tools_class_enrichment_metabobase_file)
    path <- input$tools_class_enrichment_metabobase_file$datapath

    tryCatch({
      mb <- readRDS(path)
      v <- metabobase_validate(mb)
      if (!v$ok) {
        rv$status_msg <- paste("Invalid MetaboBase:", paste(v$errors, collapse = "; "))
        rv$status_level <- "error"
        return()
      }
      app_state$metabobase <- mb
      rv$status_msg <- "MetaboBase loaded successfully."
      rv$status_level <- "info"
    }, error = function(e) {
      rv$status_msg <- paste("Error loading MetaboBase:", conditionMessage(e))
      rv$status_level <- "error"
    })
  }, ignoreInit = TRUE)

  output$tools_class_enrichment_metabobase_status <- renderUI({
    mb <- app_state$metabobase
    if (is.null(mb)) {
      return(tags$div(
        class = "text-danger",
        "No MetaboBase loaded. Load one above or build one in the Database page."
      ))
    }

    library_name <- mb$library_name %||% "(unnamed)"
    n_metabolites <- mb$n_metabolites %||% NA_integer_
    n_classes <- if (!is.null(mb$class_mappings) && is.data.frame(mb$class_mappings)) {
      length(unique(mb$class_mappings$class[!is.na(mb$class_mappings$class)]))
    } else NA_integer_

    tags$div(
      class = "text-muted",
      tags$strong("Active MetaboBase:"),
      " ",
      library_name,
      if (is.finite(n_metabolites)) {
        tags$div(sprintf("Metabolites: %s", n_metabolites))
      },
      if (is.finite(n_classes)) {
        tags$div(sprintf("Chemical classes: %s", n_classes))
      }
    )
  })

  # Helper to write progress.json
  write_progress <- function(path, status, message, pct = NULL) {
    obj <- list(status = status, message = message, pct = pct, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    tryCatch(jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = FALSE), error = function(e) NULL)
  }

  # Reactive for polling progress.json
  progress_class_rx <- reactive({
    if (identical(rv$run_status, "running")) {
      invalidateLater(500, session)
    }
    path <- rv$progress_path
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
  })

  # Polling observer for background process completion
  observe({
    if (is.null(rv$bg_process) || !identical(rv$run_status, "running")) return()

    if (!rv$bg_process$is_alive()) {
      result <- tryCatch(rv$bg_process$get_result(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))

      if (isTRUE(result$ok)) {
        rv$results <- result$result
        rv$rendered <- tb_render_class_enrichment(result$result, rv$pending_style, meta = NULL)
        rv$run_status <- "done"
        rv$status_msg <- NULL
      } else {
        rv$run_status <- "error"
        rv$status_msg <- result$error %||% "Analysis failed"
        rv$status_level <- "error"
      }
      rv$bg_process <- NULL
      if (!is.null(rv$progress_path) && file.exists(rv$progress_path)) {
        tryCatch(unlink(rv$progress_path), error = function(e) NULL)
      }
    } else {
      invalidateLater(500, session)
    }
  })

  observeEvent(input$tools_class_enrichment_run, {
    rv$status_msg <- NULL
    rv$status_level <- NULL
    rv$results <- NULL
    rv$rendered <- NULL
    rv$input_count <- NULL
    rv$hidden_terms <- character()
    rv$term_labels <- list()

    mb <- app_state$metabobase
    if (is.null(mb)) {
      rv$status_msg <- "No MetaboBase loaded. Load one above or build one in the Database page."
      rv$status_level <- "error"
      rv$run_status <- "idle"
      return()
    }

    metabolites <- parse_metabolite_input(input$tools_class_enrichment_metabolites)
    rv$input_count <- length(metabolites)

    if (length(metabolites) == 0) {
      rv$status_msg <- "Enter at least one metabolite ID to run class enrichment."
      rv$status_level <- "warn"
      rv$run_status <- "idle"
      return()
    }

    # Collect params and style from inputs
    params <- list(
      class_level = input$tools_class_enrichment_class_level %||% defs_class$params$class_level %||% "class",
      fdr_cutoff = safe_num(input$tools_class_enrichment_fdr_cutoff, defs_class$params$fdr_cutoff %||% 0.05),
      min_class_size = safe_int(input$tools_class_enrichment_min_class_size, defs_class$params$min_class_size %||% 3),
      max_terms = safe_int(input$tools_class_enrichment_max_terms, defs_class$params$max_terms %||% 20)
    )

    style <- list(
      plot_type = input$tools_class_enrichment_plot_type %||% defs_class$style$plot_type %||% "bar",
      color_mode = input$tools_class_enrichment_color_mode %||% defs_class$style$color_mode %||% "fdr",
      fdr_palette = input$tools_class_enrichment_fdr_palette %||% defs_class$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_class_enrichment_flat_color %||% "")) {
        input$tools_class_enrichment_flat_color
      } else {
        defs_class$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_class_enrichment_alpha, defs_class$style$alpha %||% 0.8),
      font_size = safe_int(input$tools_class_enrichment_font_size, defs_class$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_class_enrichment_axis_text_size, defs_class$style$axis_text_size %||% 20),
      width = safe_num(input$tools_class_enrichment_width, defs_class$style$width %||% 10),
      height = safe_num(input$tools_class_enrichment_height, defs_class$style$height %||% 8),
      flip_axis = isTRUE(input$tools_class_enrichment_flip_axis)
    )

    rv$pending_style <- style

    rv$progress_path <- tempfile(fileext = ".json")
    rv$run_start_time <- Sys.time()
    rv$run_status <- "running"
    write_progress(rv$progress_path, "running", "Starting class enrichment analysis...", 5)

    has_callr <- requireNamespace("callr", quietly = TRUE)

    if (has_callr) {
      app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

      rv$bg_process <- callr::r_bg(
        func = function(app_root, mb, metabolites, params, progress_path) {
          setwd(app_root)

          write_prog <- function(msg, pct) {
            obj <- list(status = "running", message = msg, pct = pct)
            tryCatch(jsonlite::write_json(obj, progress_path, auto_unbox = TRUE), error = function(e) NULL)
          }

          write_prog("Loading engine code...", 5)

          source(file.path(app_root, "R", "00_init.R"), local = FALSE)
          engine_files <- list.files(file.path(app_root, "R", "engines"), pattern = "\\.R$", full.names = TRUE)
          for (f in engine_files) source(f, local = FALSE)
          stats_files <- list.files(file.path(app_root, "R", "engines", "stats"), pattern = "\\.R$", full.names = TRUE)
          for (f in stats_files) source(f, local = FALSE)
          utils_files <- list.files(file.path(app_root, "R", "utils"), pattern = "\\.R$", full.names = TRUE)
          for (f in utils_files) source(f, local = FALSE)

          write_prog("Running class enrichment...", 30)

          payload <- list(
            ok = TRUE,
            params = params,
            query_metabolites = metabolites,
            metabobase = mb
          )

          result <- tryCatch({
            res <- stats_class_enrichment_run(payload, params = params, context = list(metabobase = mb))
            write_prog("Analysis complete!", 100)
            list(ok = TRUE, result = res)
          }, error = function(e) {
            list(ok = FALSE, error = conditionMessage(e))
          })

          result
        },
        args = list(
          app_root = app_root,
          mb = mb,
          metabolites = metabolites,
          params = params,
          progress_path = rv$progress_path
        ),
        package = TRUE,
        supervise = TRUE
      )
    } else {
      # Synchronous fallback
      payload <- list(
        ok = TRUE,
        params = params,
        query_metabolites = metabolites,
        metabobase = mb
      )

      res <- tryCatch({
        stats_class_enrichment_run(payload, params = params, context = list(metabobase = mb))
      }, error = function(e) {
        rv$status_msg <- conditionMessage(e)
        rv$status_level <- "error"
        rv$run_status <- "error"
        NULL
      })

      if (!is.null(res)) {
        rv$results <- res
        rv$rendered <- tb_render_class_enrichment(res, style, meta = NULL)
        rv$run_status <- "done"
      }
    }
  }, ignoreInit = TRUE)

  # Stop button handler
  observeEvent(input$tools_class_enrichment_stop, {
    if (!is.null(rv$bg_process) && rv$bg_process$is_alive()) {
      tryCatch(rv$bg_process$kill(), error = function(e) NULL)
      rv$bg_process <- NULL
      rv$run_status <- "idle"
      rv$status_msg <- "Analysis stopped by user"
      rv$status_level <- "warn"
    }
  }, ignoreInit = TRUE)

  output$tools_class_enrichment_summary <- renderUI({
    if (identical(rv$run_status, "running")) {
      p <- progress_class_rx()
      pct <- suppressWarnings(as.numeric(p$pct %||% 0))
      if (is.na(pct)) pct <- 0
      pct <- max(0, min(100, pct))
      msg <- p$message %||% "Processing..."

      return(tagList(
        div(class = "tool-status-row",
          div(class = "tool-status-pill running", "Running"),
          div(class = "tool-status-msg", msg),
          actionButton("tools_class_enrichment_stop", "Stop", class = "btn btn-danger btn-sm", style = "margin-left: auto;")
        ),
        div(class = "tool-progress-wrap",
          div(class = "tool-progress-bar",
            div(class = "tool-progress-fill active", style = sprintf("width:%s%%;", pct))
          )
        )
      ))
    }

    if (!is.null(rv$status_msg)) {
      cls <- if (identical(rv$status_level, "error")) "text-danger" else "text-warning"
      return(tags$div(class = cls, rv$status_msg))
    }

    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "Run class enrichment to see results."))
    }

    n_query <- res$data$query_count %||% NA_integer_
    n_background <- res$data$background_count %||% NA_integer_
    n_terms <- if (is.data.frame(res$data$terms %||% NULL)) nrow(res$data$terms) else 0L
    n_input <- rv$input_count %||% NA_integer_

    log_df <- res$data$log %||% NULL
    log_msg <- NULL
    if (!is.null(log_df) && nrow(log_df) > 0) {
      last <- log_df[nrow(log_df), , drop = FALSE]
      log_msg <- paste0("[", last$level, "] ", last$message)
    }

    tags$div(
      class = "card",
      tags$h4("Class Enrichment summary"),
      tags$p(sprintf("Input metabolites: %s", ifelse(is.finite(n_input), n_input, "NA"))),
      tags$p(sprintf("Matched to MetaboBase: %s", ifelse(is.finite(n_query), n_query, "NA"))),
      tags$p(sprintf("Background size: %s", ifelse(is.finite(n_background), n_background, "NA"))),
      tags$p(sprintf("Enriched classes: %s", n_terms)),
      if (!is.null(log_msg)) tags$p(log_msg)
    )
  })

  output$tools_class_enrichment_tabs <- renderUI({
    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No class enrichment results yet."))
    }

    w_in <- safe_num(input$tools_class_enrichment_width, defs_class$style$width %||% 10)
    h_in <- safe_num(input$tools_class_enrichment_height, defs_class$style$height %||% 8)
    ar <- w_in / h_in

    tagList(
      div(
        class = "tool-export-buttons",
        actionButton("tools_class_enrichment_download_png", "Download PNG", class = "btn btn-sm btn-default", icon = icon("download")),
        actionButton("tools_class_enrichment_download_pdf", "Download PDF", class = "btn btn-sm btn-default", icon = icon("file-pdf")),
        actionButton("tools_class_enrichment_copy_plot", "Copy Plot", class = "btn btn-sm btn-default", icon = icon("copy")),
        downloadButton("tools_class_enrichment_download_excel", "Download Excel", class = "btn btn-sm btn-default")
      ),
      div(
        class = "tool-plot-box",
        style = sprintf("--tool-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
        plotOutput("tools_class_enrichment_plot", height = "100%")
      ),
      div(
        class = "tool-table-wrap",
        uiOutput("tools_class_enrichment_table_ui")
      )
    )
  })

  # Build current style from inputs
  current_class_style <- reactive({
    list(
      plot_type = input$tools_class_enrichment_plot_type %||% defs_class$style$plot_type %||% "bar",
      color_mode = input$tools_class_enrichment_color_mode %||% defs_class$style$color_mode %||% "fdr",
      fdr_palette = input$tools_class_enrichment_fdr_palette %||% defs_class$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_class_enrichment_flat_color %||% "")) {
        input$tools_class_enrichment_flat_color
      } else {
        defs_class$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_class_enrichment_alpha, defs_class$style$alpha %||% 0.8),
      font_size = safe_int(input$tools_class_enrichment_font_size, defs_class$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_class_enrichment_axis_text_size, defs_class$style$axis_text_size %||% 20),
      width = safe_num(input$tools_class_enrichment_width, defs_class$style$width %||% 10),
      height = safe_num(input$tools_class_enrichment_height, defs_class$style$height %||% 8),
      flip_axis = isTRUE(input$tools_class_enrichment_flip_axis)
    )
  })

  # Get current ggplot object for export
  current_class_plot <- reactive({
    res <- rv$results
    if (is.null(res)) return(NULL)

    style <- current_class_style()
    hidden_terms <- rv$hidden_terms %||% character()
    term_labels <- rv$term_labels %||% list()

    meta <- list(visibility = list(hidden_terms = hidden_terms, term_labels = term_labels))
    rend <- tb_render_class_enrichment(res, style, meta = meta)

    p <- rend$plots[["class_plot"]]
    if (is.null(p) && length(rend$plots) > 0) {
      p <- rend$plots[[1]]
    }
    p
  })

  output$tools_class_enrichment_plot <- renderPlot({
    p <- current_class_plot()
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No enriched classes.")
      return(invisible(NULL))
    }
    suppressMessages(print(p))
  },
  width = function() plot_dims_px()$w,
  height = function() plot_dims_px()$h,
  res = 150
  )

  output$tools_class_enrichment_table_ui <- renderUI({
    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No table data."))
    }

    style <- current_class_style()
    rend <- tb_render_class_enrichment(res, style, meta = NULL)

    df <- rend$tables[["class_table"]]
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }

    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(tags$div(class = "text-muted", "No classes to display."))
    }

    hidden_term_ids <- rv$hidden_terms %||% character()
    term_labels_by_id <- rv$term_labels %||% list()

    term_id_col <- if ("class_name" %in% names(df)) "class_name" else NULL

    if (is.null(term_id_col)) {
      return(tags$div(class = "text-muted", "Table missing required columns."))
    }

    metabolite_col <- intersect(c("metabolite_ids", "metabolites"), names(df))[1]
    metabolite_col_data <- if (!is.na(metabolite_col)) df[[metabolite_col]] else NULL

    tools_class_enrichment_editable_table_ui(
      id_prefix = "tools_class_enrichment",
      df = df,
      term_id_col = term_id_col,
      term_name_col = term_id_col,
      hidden_term_ids = hidden_term_ids,
      term_labels_by_id = term_labels_by_id,
      metabolite_col_data = metabolite_col_data
    )
  })

  observe({
    res <- rv$results
    if (is.null(res)) return()

    style <- current_class_style()
    rend <- tb_render_class_enrichment(res, style, meta = NULL)

    df <- rend$tables[["class_table"]]
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }
    if (is.null(df) || !is.data.frame(df)) return()

    term_id_col <- if ("class_name" %in% names(df)) "class_name" else NULL
    if (is.null(term_id_col)) return()

    tools_class_enrichment_bind_editable_table(
      id_prefix = "tools_class_enrichment",
      df = df,
      term_id_col = term_id_col,
      input = input,
      session = session,
      on_term_name_change = function(term_id, new_name) {
        current_labels <- rv$term_labels %||% list()
        if (nzchar(new_name)) {
          current_labels[[term_id]] <- new_name
        } else {
          current_labels[[term_id]] <- NULL
        }
        rv$term_labels <- current_labels
      },
      on_visibility_change = function(term_id, is_visible) {
        current_hidden <- rv$hidden_terms %||% character()
        if (is_visible) {
          rv$hidden_terms <- setdiff(current_hidden, term_id)
        } else {
          if (!(term_id %in% current_hidden)) {
            rv$hidden_terms <- c(current_hidden, term_id)
          }
        }
      }
    )
  })

  # Export: Download PNG
  observeEvent(input$tools_class_enrichment_download_png, {
    p <- current_class_plot()
    if (is.null(p)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PNG",
      selectInput("tools_class_enrichment_png_dpi", "DPI", choices = c(150, 300, 600), selected = 300),
      downloadButton("tools_class_enrichment_png_confirm", "Download", class = "btn-primary"),
      actionButton("tools_class_enrichment_png_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_class_enrichment_png_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_class_enrichment_png_confirm <- downloadHandler(
    filename = function() {
      paste0("class_enrichment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      p <- current_class_plot()
      if (is.null(p)) return()
      style <- current_class_style()
      dpi <- as.numeric(input$tools_class_enrichment_png_dpi %||% 300)
      w <- style$width %||% 10
      h <- style$height %||% 8
      png_type <- if (capabilities("cairo")) "cairo-png" else NULL
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", dpi = dpi,
                      device = grDevices::png, type = png_type)
      removeModal()
    }
  )

  # Export: Download PDF
  observeEvent(input$tools_class_enrichment_download_pdf, {
    p <- current_class_plot()
    if (is.null(p)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PDF",
      downloadButton("tools_class_enrichment_pdf_confirm", "Download", class = "btn-primary"),
      actionButton("tools_class_enrichment_pdf_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_class_enrichment_pdf_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_class_enrichment_pdf_confirm <- downloadHandler(
    filename = function() {
      paste0("class_enrichment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      p <- current_class_plot()
      if (is.null(p)) return()
      style <- current_class_style()
      w <- style$width %||% 10
      h <- style$height %||% 8
      pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", device = pdf_device)
      removeModal()
    }
  )

  # Export: Copy Plot
  observeEvent(input$tools_class_enrichment_copy_plot, {
    session$sendCustomMessage("tools_copy_plot", list(id = "tools_class_enrichment_plot"))
  }, ignoreInit = TRUE)

  # Export: Download Excel
  output$tools_class_enrichment_download_excel <- downloadHandler(
    filename = function() {
      paste0("class_enrichment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      res <- rv$results
      if (is.null(res)) {
        writeLines("No results to export.", file)
        return()
      }

      style <- current_class_style()
      rend <- tb_render_class_enrichment(res, style, meta = NULL)

      df <- rend$tables[["class_table"]]
      if (is.null(df) && length(rend$tables) > 0) {
        df <- rend$tables[[1]]
      }

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        writeLines("No data to export.", file)
        return()
      }

      term_labels <- rv$term_labels %||% list()
      term_id_col <- if ("class_name" %in% names(df)) "class_name" else NULL

      if (!is.null(term_id_col) && length(term_labels) > 0) {
        for (i in seq_len(nrow(df))) {
          tid <- as.character(df[[term_id_col]][i])
          if (!is.null(term_labels[[tid]]) && nzchar(term_labels[[tid]])) {
            df[[term_id_col]][i] <- term_labels[[tid]]
          }
        }
      }

      if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Class Enrichment Results")
        openxlsx::writeData(wb, "Class Enrichment Results", df)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        utils::write.csv(df, file, row.names = FALSE)
      }
    }
  )
}
