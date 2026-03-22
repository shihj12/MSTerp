# R/pages/tools/tool_1dgofcs.R
# 1D GO-FCS Tool - 1D Functional Class Scoring

tools_1dgofcs_defaults <- function() {
  eng <- msterp_engine_get("1dgofcs")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

# Build protein_to_go and go_terms from annot_long if not already present
# This mirrors the logic in run_utils.R for pipeline execution
tools_1dgofcs_ensure_terpbase_go_mappings <- function(tb) {
  if (is.null(tb)) return(tb)

  # Build protein_to_go from annot_long if missing
  if (is.null(tb$protein_to_go) && !is.null(tb$annot_long)) {
    annot <- tb$annot_long
    if ("gene" %in% names(annot) && "ID" %in% names(annot)) {
      genes <- unique(annot$gene)
      protein_to_go <- lapply(genes, function(g) {
        unique(annot$ID[annot$gene == g])
      })
      names(protein_to_go) <- genes
      tb$protein_to_go <- protein_to_go
    }
  }

  # Build go_terms from terms_by_id if missing
  if (is.null(tb$go_terms) && !is.null(tb$terms_by_id)) {
    terms <- tb$terms_by_id
    if (all(c("ID", "Description", "ONTOLOGY") %in% names(terms))) {
      go_terms <- lapply(seq_len(nrow(terms)), function(i) {
        list(
          name = terms$Description[i],
          ontology = terms$ONTOLOGY[i]
        )
      })
      names(go_terms) <- terms$ID
      tb$go_terms <- go_terms
    }
  }

  tb
}

# ============================================================
# 1D GO-FCS Tool UI
# ============================================================
tools_1dgofcs_ui <- function() {
  defs <- tools_1dgofcs_defaults()
  params_defaults <- defs$params %||% list()
  style_defaults <- defs$style %||% list()

  tagList(
    div(
      class = "top",
      actionButton("tools_1dgofcs_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("1D GO-FCS", style = "margin: 0;")
    ),
    tags$p("Paste a gene list with scores to run 1D functional class scoring against a TerpBase."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("TerpBase"),
        uiOutput("tools_1dgofcs_terpbase_status"),
{
          terpbase_choices <- tools_default_terpbase_choices()
          default_selected <- if (length(terpbase_choices) > 0 && nzchar(terpbase_choices[1])) terpbase_choices[1] else ""
          selectInput(
            "tools_1dgofcs_terpbase_default_path",
            "Default TerpBase",
            choices = terpbase_choices,
            selected = default_selected
          )
        },
        actionButton(
          "tools_1dgofcs_terpbase_default_load",
          "Load default",
          class = "btn btn-default btn-sm btn-tool-action"
        ),
        fileInput(
          "tools_1dgofcs_terpbase_file",
          "Load TerpBase (.terpbase)",
          accept = c(".terpbase", ".rds"),
          placeholder = "No file selected"
        ),
        hr(),
        tags$h4("Ranked Gene List"),
        tags$p(class = "text-muted", "Enter gene/protein IDs in ranked order (one per line, best/top-ranked first)."),
        textAreaInput(
          "tools_1dgofcs_genes",
          "Ranked gene list (one ID per line)",
          rows = 10,
          placeholder = "TP53\nEGFR\nBRCA1\nMYC\nKRAS"
        ),
        div(
          style = "display: flex; gap: 8px; margin-top: 5px;",
          actionButton("tools_1dgofcs_run", "Run 1D GO-FCS", class = "btn-primary btn-tool-action"),
          actionButton("tools_1dgofcs_reset", "Reset", class = "btn btn-default btn-tool-action")
        ),
        hr(),
        tags$h4("View"),
        selectInput(
          "tools_1dgofcs_ontology_view",
          "Ontology",
          choices = c("Biological Process" = "BP",
                      "Molecular Function" = "MF", "Cellular Component" = "CC"),
          selected = "BP"
        ),
        tools_collapse_section_ui(
          "tools_1dgofcs_params_section",
          "Parameters",
          open = FALSE,
          numericInput(
            "tools_1dgofcs_fdr_cutoff",
            "FDR cutoff",
            value = params_defaults$fdr_cutoff %||% 0.03,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "tools_1dgofcs_min_term_size",
            "Min term size",
            value = params_defaults$min_term_size %||% 5,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_min_overlap",
            "Min overlap",
            value = params_defaults$min_overlap %||% 5,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_max_terms",
            "Terms to show (per ontology)",
            value = params_defaults$max_terms %||% 20,
            min = 1,
            max = 200,
            step = 1
          )
        ),
        tools_collapse_section_ui(
          "tools_1dgofcs_plot_section",
          "Plot Options",
          open = FALSE,
          selectInput(
            "tools_1dgofcs_plot_type",
            "Plot type",
            choices = c("bar", "dot"),
            selected = style_defaults$plot_type %||% "bar"
          ),
          selectInput(
            "tools_1dgofcs_color_mode",
            "Coloring",
            choices = c("fdr", "flat"),
            selected = style_defaults$color_mode %||% "fdr"
          ),
          conditionalPanel(
            condition = "input.tools_1dgofcs_color_mode == 'fdr'",
            selectInput(
              "tools_1dgofcs_fdr_palette",
              "FDR color palette",
              choices = c("yellow_cap" = "Yellow (significant)", "blue_red" = "Blue-Red"),
              selected = style_defaults$fdr_palette %||% "yellow_cap"
            )
          ),
          conditionalPanel(
            condition = "input.tools_1dgofcs_color_mode == 'flat'",
            textInput(
              "tools_1dgofcs_flat_color",
              "Flat color (hex)",
              value = style_defaults$flat_color %||% "#B0B0B0"
            )
          ),
          sliderInput(
            "tools_1dgofcs_alpha",
            "Opacity",
            min = 0,
            max = 1,
            value = style_defaults$alpha %||% 0.8,
            step = 0.05
          ),
          checkboxInput(
            "tools_1dgofcs_show_go_id",
            "Show GO ID in labels",
            value = isTRUE(style_defaults$show_go_id %||% FALSE)
          ),
          checkboxInput(
            "tools_1dgofcs_flip_axis",
            "Flip horizontal axis",
            value = FALSE
          ),
          numericInput(
            "tools_1dgofcs_font_size",
            "Font size",
            value = style_defaults$font_size %||% 14,
            min = 6,
            max = 30,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_axis_text_size",
            "Axis text size",
            value = style_defaults$axis_text_size %||% 20,
            min = 6,
            max = 40,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_width",
            "Plot width (in)",
            value = style_defaults$width %||% 14,
            min = 2,
            max = 24,
            step = 0.5
          ),
          numericInput(
            "tools_1dgofcs_height",
            "Plot height (in)",
            value = style_defaults$height %||% 6,
            min = 2,
            max = 24,
            step = 0.5
          )
        ),
        tools_collapse_section_ui(
          "tools_1dgofcs_filter_section",
          "Table Filters",
          open = FALSE,
          numericInput(
            "tools_1dgofcs_filter_score_min",
            "Min score",
            value = NA,
            step = 0.1
          ),
          numericInput(
            "tools_1dgofcs_filter_n_genes_min",
            "Min # genes",
            value = NA,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_filter_fdr_max",
            "Max FDR",
            value = NA,
            min = 0,
            max = 1,
            step = 0.001
          ),
          actionButton(
            "tools_1dgofcs_filter_clear",
            "Clear Filters",
            class = "btn btn-default btn-sm",
            style = "margin-top: 8px;"
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_1dgofcs_summary"),
        uiOutput("tools_1dgofcs_tabs")
      )
    )
  )
}

# ============================================================
# 1D GO-FCS Server Logic
# ============================================================
tools_1dgofcs_server <- function(input, output, session, app_state, rv_1dgofcs, defs_1dgofcs) {
  safe_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) default else v
  }

  safe_int <- function(x, default) {
    v <- suppressWarnings(as.integer(x))
    if (!is.finite(v) || v <= 0) default else v
  }

  # Reset button handler
  observeEvent(input$tools_1dgofcs_reset, {
    rv_1dgofcs$results <- NULL
    rv_1dgofcs$rendered <- NULL
    rv_1dgofcs$status_msg <- NULL
    rv_1dgofcs$status_level <- NULL
    rv_1dgofcs$input_count <- NULL
    rv_1dgofcs$hidden_terms <- character()
    rv_1dgofcs$term_labels <- list()
    # Clear stored parameters so they don't get restored on navigation
    rv_1dgofcs$stored_params <- NULL
    rv_1dgofcs$stored_genes <- NULL

    # Reset parameter inputs to defaults
    updateNumericInput(session, "tools_1dgofcs_fdr_cutoff", value = defs_1dgofcs$params$fdr_cutoff %||% 0.03)
    updateNumericInput(session, "tools_1dgofcs_min_term_size", value = defs_1dgofcs$params$min_term_size %||% 5)
    updateNumericInput(session, "tools_1dgofcs_min_overlap", value = defs_1dgofcs$params$min_overlap %||% 5)
    updateNumericInput(session, "tools_1dgofcs_max_terms", value = defs_1dgofcs$params$max_terms %||% 20)
    updateTextAreaInput(session, "tools_1dgofcs_genes", value = "")
  }, ignoreInit = TRUE)

  # Parse ranked gene list for 1D GO-FCS

  # Input: one gene per line, already in ranked order (best/top first)
  # Output: genes and scores where scores are derived from rank position
  parse_gene_score_input <- function(text) {
    if (is.null(text) || !nzchar(text)) return(list(genes = character(0), scores = numeric(0)))
    lines <- unlist(strsplit(text, "\\r?\\n"))
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]

    # Each line is just a gene ID
    genes <- lines[nzchar(lines)]

    if (length(genes) == 0) return(list(genes = character(0), scores = numeric(0)))

    # Convert rank position to scores: top-ranked gets highest score
    # Using descending scores so rank 1 (first line) = highest score
    n <- length(genes)
    scores <- seq(from = n, to = 1, by = -1)

    list(genes = genes, scores = scores)
  }

  plot_dims_px_1dgofcs <- function() {
    dpi <- 150
    w_in <- safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14)
    h_in <- safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6)

    max_px <- 2600
    w_px <- as.integer(round(w_in * dpi))
    h_px <- as.integer(round(h_in * dpi))

    scale <- min(1, max_px / max(w_px, h_px))
    list(
      w = as.integer(round(w_px * scale)),
      h = as.integer(round(h_px * scale))
    )
  }

  # TerpBase status for 1D GO-FCS
  output$tools_1dgofcs_terpbase_status <- renderUI({
    tb <- app_state$terpbase
    if (is.null(tb)) {
      return(tags$div(
        class = "text-danger",
        "No TerpBase loaded. Load one above or build one in the TerpBase page."
      ))
    }

    library_name <- tb$library_name %||% "(unnamed)"
    organism <- tb$organism %||% "(unknown)"
    n_ids <- NA_integer_
    if (!is.null(tb$annot_long) && "gene" %in% names(tb$annot_long)) {
      n_ids <- length(unique(tb$annot_long$gene))
    }

    tags$div(
      class = "text-muted",
      tags$strong("Active TerpBase:"),
      " ",
      library_name,
      tags$br(),
      sprintf("Organism: %s", organism),
      if (is.finite(n_ids)) {
        tags$div(sprintf("Unique identifiers: %s", n_ids))
      }
    )
  })

  # Helper to write progress.json
  write_progress <- function(path, status, message, pct = NULL) {
    obj <- list(status = status, message = message, pct = pct, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    tryCatch(jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = FALSE), error = function(e) NULL)
  }

  # Reactive for polling progress.json

  progress_1dgofcs_rx <- reactive({
    if (identical(rv_1dgofcs$run_status, "running")) {
      invalidateLater(500, session)
    }
    path <- rv_1dgofcs$progress_path
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
  })

  # Polling observer for background process completion

  observe({
    if (is.null(rv_1dgofcs$bg_process) || !identical(rv_1dgofcs$run_status, "running")) return()

    if (!rv_1dgofcs$bg_process$is_alive()) {
      result <- tryCatch(rv_1dgofcs$bg_process$get_result(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))

      if (isTRUE(result$ok)) {
        rv_1dgofcs$results <- result$result
        rv_1dgofcs$rendered <- tb_render_1dgofcs(result$result, rv_1dgofcs$pending_style, meta = NULL)
        rv_1dgofcs$run_status <- "done"
        rv_1dgofcs$status_msg <- NULL
      } else {
        rv_1dgofcs$run_status <- "error"
        rv_1dgofcs$status_msg <- result$error %||% "Analysis failed"
        rv_1dgofcs$status_level <- "error"
      }
      rv_1dgofcs$bg_process <- NULL
      # Clean up progress file
      if (!is.null(rv_1dgofcs$progress_path) && file.exists(rv_1dgofcs$progress_path)) {
        tryCatch(unlink(rv_1dgofcs$progress_path), error = function(e) NULL)
      }
    } else {
      invalidateLater(500, session)
    }
  })

  # Run 1D GO-FCS
  observeEvent(input$tools_1dgofcs_run, {
    # Only reset status and results, preserve parameters
    rv_1dgofcs$status_msg <- NULL
    rv_1dgofcs$status_level <- NULL
    rv_1dgofcs$results <- NULL
    rv_1dgofcs$rendered <- NULL
    rv_1dgofcs$input_count <- NULL
    # Reset visibility state for new run
    rv_1dgofcs$hidden_terms <- character()
    rv_1dgofcs$term_labels <- list()

    tb <- app_state$terpbase
    if (is.null(tb)) {
      rv_1dgofcs$status_msg <- "No TerpBase loaded. Load one above or build one in the TerpBase page."
      rv_1dgofcs$status_level <- "error"
      rv_1dgofcs$run_status <- "idle"
      return()
    }

    parsed <- parse_gene_score_input(input$tools_1dgofcs_genes)
    rv_1dgofcs$input_count <- length(parsed$genes)

    if (length(parsed$genes) == 0) {
      rv_1dgofcs$status_msg <- "Enter at least one gene to run 1D GO-FCS."
      rv_1dgofcs$status_level <- "warn"
      rv_1dgofcs$run_status <- "idle"
      return()
    }

    # Create named score vector
    scores <- parsed$scores
    names(scores) <- parsed$genes

    # Collect params and style from inputs (lightweight)
    params <- list(
      fdr_cutoff = safe_num(input$tools_1dgofcs_fdr_cutoff, defs_1dgofcs$params$fdr_cutoff %||% 0.03),
      min_term_size = safe_int(input$tools_1dgofcs_min_term_size, defs_1dgofcs$params$min_term_size %||% 5),
      min_overlap = safe_int(input$tools_1dgofcs_min_overlap, defs_1dgofcs$params$min_overlap %||% 5),
      max_terms = safe_int(input$tools_1dgofcs_max_terms, defs_1dgofcs$params$max_terms %||% 20)
    )

    style <- list(
      plot_type = input$tools_1dgofcs_plot_type %||% defs_1dgofcs$style$plot_type %||% "bar",
      color_mode = input$tools_1dgofcs_color_mode %||% defs_1dgofcs$style$color_mode %||% "fdr",
      fdr_palette = input$tools_1dgofcs_fdr_palette %||% defs_1dgofcs$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_1dgofcs_flat_color %||% "")) {
        input$tools_1dgofcs_flat_color
      } else {
        defs_1dgofcs$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_1dgofcs_alpha, defs_1dgofcs$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_1dgofcs_show_go_id),
      font_size = safe_int(input$tools_1dgofcs_font_size, defs_1dgofcs$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_1dgofcs_axis_text_size, defs_1dgofcs$style$axis_text_size %||% 20),
      width = safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14),
      height = safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6),
      flip_axis = isTRUE(input$tools_1dgofcs_flip_axis),
      ontology_filter = "all"  # Always compute all ontologies
    )

    # Store style for later use when rendering
    rv_1dgofcs$pending_style <- style

    # Show progress immediately - before any heavy work
    rv_1dgofcs$progress_path <- tempfile(fileext = ".json")
    rv_1dgofcs$run_start_time <- Sys.time()
    rv_1dgofcs$run_status <- "running"
    write_progress(rv_1dgofcs$progress_path, "running", "Starting 1D GO-FCS analysis...", 5)

    # Check if callr is available for async execution
    has_callr <- requireNamespace("callr", quietly = TRUE)

    if (has_callr) {
      app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

      rv_1dgofcs$bg_process <- callr::r_bg(
        func = function(app_root, tb, scores, params, progress_path) {
          # Set working directory (critical for file paths to resolve correctly)
          setwd(app_root)

          write_prog <- function(msg, pct) {
            obj <- list(status = "running", message = msg, pct = pct)
            tryCatch(jsonlite::write_json(obj, progress_path, auto_unbox = TRUE), error = function(e) NULL)
          }

          write_prog("Loading engine code...", 5)

          # Source all required files (same pattern as new_run page)
          source(file.path(app_root, "R", "00_init.R"), local = FALSE)
          engine_files <- list.files(file.path(app_root, "R", "engines"), pattern = "\\.R$", full.names = TRUE)
          for (f in engine_files) source(f, local = FALSE)
          stats_files <- list.files(file.path(app_root, "R", "engines", "stats"), pattern = "\\.R$", full.names = TRUE)
          for (f in stats_files) source(f, local = FALSE)
          utils_files <- list.files(file.path(app_root, "R", "utils"), pattern = "\\.R$", full.names = TRUE)
          for (f in utils_files) source(f, local = FALSE)

          write_prog("Preparing GO mappings...", 10)

          # Ensure terpbase has protein_to_go and go_terms mappings (moved to background)
          if (is.null(tb$protein_to_go) && !is.null(tb$annot_long)) {
            annot <- tb$annot_long
            if ("gene" %in% names(annot) && "ID" %in% names(annot)) {
              genes_list <- unique(annot$gene)
              protein_to_go <- lapply(genes_list, function(g) {
                unique(annot$ID[annot$gene == g])
              })
              names(protein_to_go) <- genes_list
              tb$protein_to_go <- protein_to_go
            }
          }
          if (is.null(tb$go_terms) && !is.null(tb$terms_by_id)) {
            terms <- tb$terms_by_id
            if (all(c("ID", "Description", "ONTOLOGY") %in% names(terms))) {
              go_terms <- lapply(seq_len(nrow(terms)), function(i) {
                list(name = terms$Description[i], ontology = terms$ONTOLOGY[i])
              })
              names(go_terms) <- terms$ID
              tb$go_terms <- go_terms
            }
          }

          payload <- list(
            ok = TRUE,
            params = params,
            scores = scores,
            terpbase = tb
          )

          write_prog("Running functional class scoring...", 30)

          result <- tryCatch({
            res <- stats_1dgofcs_run(payload, params = params, context = list(terpbase = tb))
            write_prog("Analysis complete!", 100)
            list(ok = TRUE, result = res)
          }, error = function(e) {
            list(ok = FALSE, error = conditionMessage(e))
          })

          result
        },
        args = list(
          app_root = app_root,
          tb = tb,
          scores = scores,
          params = params,
          progress_path = rv_1dgofcs$progress_path
        ),
        package = TRUE,
        supervise = TRUE
      )
    } else {
      # Synchronous fallback - must do prep work here
      tb <- tools_1dgofcs_ensure_terpbase_go_mappings(tb)
      payload <- list(
        ok = TRUE,
        params = params,
        scores = scores,
        terpbase = tb
      )

      res <- tryCatch({
        stats_1dgofcs_run(payload, params = params, context = list(terpbase = tb))
      }, error = function(e) {
        rv_1dgofcs$status_msg <- conditionMessage(e)
        rv_1dgofcs$status_level <- "error"
        rv_1dgofcs$run_status <- "error"
        NULL
      })

      if (!is.null(res)) {
        rv_1dgofcs$results <- res
        rv_1dgofcs$rendered <- tb_render_1dgofcs(res, style, meta = NULL)
        rv_1dgofcs$run_status <- "done"
      }
    }
  }, ignoreInit = TRUE)

  # Stop button handler
  observeEvent(input$tools_1dgofcs_stop, {
    if (!is.null(rv_1dgofcs$bg_process) && rv_1dgofcs$bg_process$is_alive()) {
      tryCatch(rv_1dgofcs$bg_process$kill(), error = function(e) NULL)
      rv_1dgofcs$bg_process <- NULL
      rv_1dgofcs$run_status <- "idle"
      rv_1dgofcs$status_msg <- "Analysis stopped by user"
      rv_1dgofcs$status_level <- "warn"
    }
  }, ignoreInit = TRUE)

  # 1D GO-FCS summary
  output$tools_1dgofcs_summary <- renderUI({
    # Show progress bar when running
    if (identical(rv_1dgofcs$run_status, "running")) {
      p <- progress_1dgofcs_rx()
      pct <- suppressWarnings(as.numeric(p$pct %||% 0))
      if (is.na(pct)) pct <- 0
      pct <- max(0, min(100, pct))
      msg <- p$message %||% "Processing..."

      return(tagList(
        div(class = "tool-status-row",
          div(class = "tool-status-pill running", "Running"),
          div(class = "tool-status-msg", msg),
          actionButton("tools_1dgofcs_stop", "Stop", class = "btn btn-danger btn-sm", style = "margin-left: auto;")
        ),
        div(class = "tool-progress-wrap",
          div(class = "tool-progress-bar",
            div(class = "tool-progress-fill active", style = sprintf("width:%s%%;", pct))
          )
        )
      ))
    }

    if (!is.null(rv_1dgofcs$status_msg)) {
      cls <- if (identical(rv_1dgofcs$status_level, "error")) "text-danger" else "text-warning"
      return(tags$div(class = cls, rv_1dgofcs$status_msg))
    }

    res <- rv_1dgofcs$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "Run 1D GO-FCS to see results."))
    }

    n_terms <- if (is.data.frame(res$data$terms %||% NULL)) nrow(res$data$terms) else 0L
    n_input <- rv_1dgofcs$input_count %||% NA_integer_

    log_df <- res$data$log %||% NULL
    log_msg <- NULL
    if (!is.null(log_df) && nrow(log_df) > 0) {
      last <- log_df[nrow(log_df), , drop = FALSE]
      log_msg <- paste0("[", last$level, "] ", last$message)
    }

    tags$div(
      class = "card",
      tags$h4("1D GO-FCS summary"),
      tags$p(sprintf("Input genes (ranked): %s", ifelse(is.finite(n_input), n_input, "NA"))),
      tags$p(sprintf("Enriched terms: %s", n_terms)),
      if (!is.null(log_msg)) tags$p(log_msg)
    )
  })

  # 1D GO-FCS tabs
  output$tools_1dgofcs_tabs <- renderUI({
    res <- rv_1dgofcs$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No 1D GO-FCS results yet."))
    }

    # Calculate aspect ratio from current inputs
    w_in <- safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14)
    h_in <- safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6)
    ar <- w_in / h_in

    tagList(
      # Export buttons
      div(
        class = "tool-export-buttons",
        actionButton("tools_1dgofcs_download_png", "Download PNG", class = "btn btn-sm btn-default", icon = icon("download")),
        actionButton("tools_1dgofcs_download_pdf", "Download PDF", class = "btn btn-sm btn-default", icon = icon("file-pdf")),
        actionButton("tools_1dgofcs_copy_plot", "Copy Plot", class = "btn btn-sm btn-default", icon = icon("copy")),
        downloadButton("tools_1dgofcs_download_excel", "Download Excel", class = "btn btn-sm btn-default")
      ),
      div(
        class = "tool-plot-box",
        style = sprintf("--tool-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
        plotOutput("tools_1dgofcs_plot", height = "100%")
      ),
      div(
        class = "tool-table-wrap",
        uiOutput("tools_1dgofcs_table_ui")
      )
    )
  })

  # Build current style from inputs (reactive helper)
  current_1dgofcs_style <- reactive({
    list(
      plot_type = input$tools_1dgofcs_plot_type %||% defs_1dgofcs$style$plot_type %||% "bar",
      color_mode = input$tools_1dgofcs_color_mode %||% defs_1dgofcs$style$color_mode %||% "fdr",
      fdr_palette = input$tools_1dgofcs_fdr_palette %||% defs_1dgofcs$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_1dgofcs_flat_color %||% "")) {
        input$tools_1dgofcs_flat_color
      } else {
        defs_1dgofcs$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_1dgofcs_alpha, defs_1dgofcs$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_1dgofcs_show_go_id),
      font_size = safe_int(input$tools_1dgofcs_font_size, defs_1dgofcs$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_1dgofcs_axis_text_size, defs_1dgofcs$style$axis_text_size %||% 20),
      width = safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14),
      height = safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6),
      flip_axis = isTRUE(input$tools_1dgofcs_flip_axis),
      ontology_filter = input$tools_1dgofcs_ontology_view %||% "BP"
    )
  })

  # Get current ggplot object for export
  current_1dgofcs_plot <- reactive({
    res <- rv_1dgofcs$results
    if (is.null(res)) return(NULL)

    style <- current_1dgofcs_style()
    hidden_terms <- rv_1dgofcs$hidden_terms %||% character()
    term_labels <- rv_1dgofcs$term_labels %||% list()

    meta <- list(visibility = list(hidden_terms = hidden_terms, term_labels = term_labels))
    rend <- tb_render_1dgofcs(res, style, meta = meta)

    ont <- tolower(input$tools_1dgofcs_ontology_view %||% "BP")
    plot_key <- paste0(ont, "_plot")
    p <- rend$plots[[plot_key]]

    if (is.null(p) && length(rend$plots) > 0) {
      p <- rend$plots[[1]]
    }
    p
  })

  # Reactive to re-render plot when plot options, ontology view, or visibility changes
  output$tools_1dgofcs_plot <- renderPlot({
    p <- current_1dgofcs_plot()
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No enriched terms for this ontology.")
      return(invisible(NULL))
    }
    suppressMessages(print(p))
  },
  width = function() plot_dims_px_1dgofcs()$w,
  height = function() plot_dims_px_1dgofcs()$h,
  res = 150
  )

  # Clear filters button handler
  observeEvent(input$tools_1dgofcs_filter_clear, {
    updateNumericInput(session, "tools_1dgofcs_filter_score_min", value = NA)
    updateNumericInput(session, "tools_1dgofcs_filter_n_genes_min", value = NA)
    updateNumericInput(session, "tools_1dgofcs_filter_fdr_max", value = NA)
  }, ignoreInit = TRUE)

  # Helper function to apply table filters
  apply_1dgofcs_table_filters <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

    # Filter by score min
    score_min <- input$tools_1dgofcs_filter_score_min
    if (!is.null(score_min) && is.finite(score_min) && "score" %in% names(df)) {
      df <- df[!is.na(df$score) & df$score >= score_min, , drop = FALSE]
    }

    # Filter by n_genes min (column may be n, n_genes, or count)
    n_min <- input$tools_1dgofcs_filter_n_genes_min
    if (!is.null(n_min) && is.finite(n_min)) {
      n_col <- intersect(c("n_genes", "n", "count", "Count"), names(df))[1]
      if (!is.na(n_col)) {
        df <- df[!is.na(df[[n_col]]) & df[[n_col]] >= n_min, , drop = FALSE]
      }
    }

    # Filter by FDR max
    fdr_max <- input$tools_1dgofcs_filter_fdr_max
    if (!is.null(fdr_max) && is.finite(fdr_max) && "fdr" %in% names(df)) {
      df <- df[!is.na(df$fdr) & df$fdr <= fdr_max, , drop = FALSE]
    }

    df
  }

  # Editable table UI with visibility checkboxes, term name editing, and search
  output$tools_1dgofcs_table_ui <- renderUI({
    res <- rv_1dgofcs$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No table data."))
    }

    ont <- tolower(input$tools_1dgofcs_ontology_view %||% "BP")
    style <- current_1dgofcs_style()
    rend <- tb_render_1dgofcs(res, style, meta = NULL)

    table_key <- paste0(ont, "_table")
    df <- rend$tables[[table_key]]
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }

    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(tags$div(class = "text-muted", "No terms to display."))
    }

    # Apply table filters
    df <- apply_1dgofcs_table_filters(df)

    if (nrow(df) == 0) {
      return(tags$div(class = "text-muted", "No terms match the current filters."))
    }

    hidden_term_ids <- rv_1dgofcs$hidden_terms %||% character()
    term_labels_by_id <- rv_1dgofcs$term_labels %||% list()

    # Determine term_id and term_name columns
    term_id_col <- if ("term_id" %in% names(df)) "term_id" else NULL
    term_name_col <- if ("term" %in% names(df)) "term" else if ("term_name" %in% names(df)) "term_name" else NULL

    if (is.null(term_id_col) || is.null(term_name_col)) {
      return(tags$div(class = "text-muted", "Table missing required columns."))
    }

    # Get gene column for search button
    gene_col <- intersect(c("genes", "geneID", "protein_ids", "core_enrichment", "Genes"), names(df))[1]
    gene_col_data <- if (!is.na(gene_col)) df[[gene_col]] else NULL

    # Build the editable table (matching result viewer exactly)
    tools_go_editable_table_ui(
      id_prefix = paste0("tools_1dgofcs_", ont),
      df = df,
      term_id_col = term_id_col,
      term_name_col = term_name_col,
      hidden_term_ids = hidden_term_ids,
      term_labels_by_id = term_labels_by_id,
      gene_col_data = gene_col_data
    )
  })

  # Bind observers for visibility and term name changes (matching result viewer pattern)
  observe({
    res <- rv_1dgofcs$results
    if (is.null(res)) return()

    ont <- tolower(input$tools_1dgofcs_ontology_view %||% "BP")
    style <- current_1dgofcs_style()
    rend <- tb_render_1dgofcs(res, style, meta = NULL)

    table_key <- paste0(ont, "_table")
    df <- rend$tables[[table_key]]
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }
    if (is.null(df) || !is.data.frame(df)) return()

    # Apply table filters
    df <- apply_1dgofcs_table_filters(df)
    if (nrow(df) == 0) return()

    # Determine term_id column
    term_id_col <- if ("term_id" %in% names(df)) "term_id" else NULL
    if (is.null(term_id_col)) return()

    # Bind observers using the same pattern as result viewer
    tools_bind_editable_go_table(
      id_prefix = paste0("tools_1dgofcs_", ont),
      df = df,
      term_id_col = term_id_col,
      input = input,
      session = session,
      on_term_name_change = function(term_id, new_name) {
        current_labels <- rv_1dgofcs$term_labels %||% list()
        if (nzchar(new_name)) {
          current_labels[[term_id]] <- new_name
        } else {
          current_labels[[term_id]] <- NULL
        }
        rv_1dgofcs$term_labels <- current_labels
      },
      on_visibility_change = function(term_id, is_visible) {
        current_hidden <- rv_1dgofcs$hidden_terms %||% character()
        if (is_visible) {
          rv_1dgofcs$hidden_terms <- setdiff(current_hidden, term_id)
        } else {
          if (!(term_id %in% current_hidden)) {
            rv_1dgofcs$hidden_terms <- c(current_hidden, term_id)
          }
        }
      }
    )
  })

  # Export: Download PNG
  observeEvent(input$tools_1dgofcs_download_png, {
    p <- current_1dgofcs_plot()
    if (is.null(p)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PNG",
      selectInput("tools_1dgofcs_png_dpi", "DPI", choices = c(150, 300, 600), selected = 300),
      downloadButton("tools_1dgofcs_png_confirm", "Download", class = "btn-primary"),
      actionButton("tools_1dgofcs_png_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_png_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_1dgofcs_png_confirm <- downloadHandler(
    filename = function() {
      ont <- input$tools_1dgofcs_ontology_view %||% "BP"
      paste0("1dgofcs_", tolower(ont), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      p <- current_1dgofcs_plot()
      if (is.null(p)) return()
      style <- current_1dgofcs_style()
      dpi <- as.numeric(input$tools_1dgofcs_png_dpi %||% 300)
      w <- style$width %||% 14
      h <- style$height %||% 6
      png_type <- if (capabilities("cairo")) "cairo-png" else NULL
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", dpi = dpi,
                      device = grDevices::png, type = png_type)
      removeModal()
    }
  )

  # Export: Download PDF
  observeEvent(input$tools_1dgofcs_download_pdf, {
    p <- current_1dgofcs_plot()
    if (is.null(p)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PDF",
      downloadButton("tools_1dgofcs_pdf_confirm", "Download", class = "btn-primary"),
      actionButton("tools_1dgofcs_pdf_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_pdf_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_1dgofcs_pdf_confirm <- downloadHandler(
    filename = function() {
      ont <- input$tools_1dgofcs_ontology_view %||% "BP"
      paste0("1dgofcs_", tolower(ont), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      p <- current_1dgofcs_plot()
      if (is.null(p)) return()
      style <- current_1dgofcs_style()
      w <- style$width %||% 14
      h <- style$height %||% 6
      pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", device = pdf_device)
      removeModal()
    }
  )

  # Export: Copy Plot
  observeEvent(input$tools_1dgofcs_copy_plot, {
    session$sendCustomMessage("tools_copy_plot", list(id = "tools_1dgofcs_plot"))
  }, ignoreInit = TRUE)

  # Export: Download Excel
  output$tools_1dgofcs_download_excel <- downloadHandler(
    filename = function() {
      ont <- input$tools_1dgofcs_ontology_view %||% "BP"
      paste0("1dgofcs_", tolower(ont), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      res <- rv_1dgofcs$results
      if (is.null(res)) {
        writeLines("No results to export.", file)
        return()
      }

      ont <- tolower(input$tools_1dgofcs_ontology_view %||% "BP")
      style <- current_1dgofcs_style()
      rend <- tb_render_1dgofcs(res, style, meta = NULL)

      table_key <- paste0(ont, "_table")
      df <- rend$tables[[table_key]]
      if (is.null(df) && length(rend$tables) > 0) {
        df <- rend$tables[[1]]
      }

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        writeLines("No data to export.", file)
        return()
      }

      term_labels <- rv_1dgofcs$term_labels %||% list()
      term_col <- if ("term" %in% names(df)) "term" else if ("term_name" %in% names(df)) "term_name" else NULL
      term_id_col <- if ("term_id" %in% names(df)) "term_id" else NULL

      if (!is.null(term_col) && !is.null(term_id_col) && length(term_labels) > 0) {
        for (i in seq_len(nrow(df))) {
          tid <- as.character(df[[term_id_col]][i])
          if (!is.null(term_labels[[tid]]) && nzchar(term_labels[[tid]])) {
            df[[term_col]][i] <- term_labels[[tid]]
          }
        }
      }

      if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "1D GO-FCS Results")
        openxlsx::writeData(wb, "1D GO-FCS Results", df)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        utils::write.csv(df, file, row.names = FALSE)
      }
    }
  )
}
