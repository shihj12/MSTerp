# R/pages/tools/tool_goora.R
# GO-ORA Tool - Gene Ontology Over-Representation Analysis

tools_goora_defaults <- function() {

  eng <- msterp_engine_get("goora")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

# Build protein_to_go and go_terms from annot_long if not already present
# This mirrors the logic in run_utils.R for pipeline execution
tools_ensure_terpbase_go_mappings <- function(tb) {
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

# Safe input ID generator (matches res_safe_input_id from page_results.R)
tools_safe_input_id <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+", "", x)
  if (!nzchar(x) || grepl("^[0-9]", x)) x <- paste0("id_", x)
  x
}

# Helper function to create editable GO table UI for tools page
# Matches res_editable_go_table_ui from page_results.R exactly
tools_go_editable_table_ui <- function(
    id_prefix,
    df,
    term_id_col = "term_id",
    term_name_col = "term_name",
    hidden_term_ids = character(),
    term_labels_by_id = list(),
    gene_col_data = NULL
) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(div(class = "text-muted", "No terms to display."))
  }
  if (!(term_id_col %in% names(df)) || !(term_name_col %in% names(df))) {
    return(div(class = "text-danger", "Table is missing required columns for editing."))
  }

  term_ids <- as.character(df[[term_id_col]])
  cols <- names(df)

  # Columns to hide from display (but still available in data)
  hide_cols <- c("genes", "gene_ids", "geneID", "protein_ids", "core_enrichment", "Genes",
                 "neglog10_fdr", "neglog10fdr", "n_term", "ontology", "score")

  # Build header: Visible, term_id, term_name, Search, then other columns
  other_cols <- setdiff(cols, c(term_id_col, term_name_col, hide_cols))
  has_search <- !is.null(gene_col_data) && length(gene_col_data) == nrow(df)
  header_cols <- c("Visible", term_id_col, term_name_col, if (has_search) "Search" else NULL, other_cols)

  rows <- lapply(seq_len(nrow(df)), function(i) {
    term_id <- as.character(term_ids[[i]] %||% "")
    safe_term_id <- tools_safe_input_id(term_id)

    vis_id <- paste0(id_prefix, "_vis_", safe_term_id)
    name_id <- paste0(id_prefix, "_name_", safe_term_id)

    default_name <- as.character(df[[term_name_col]][[i]] %||% "")
    display_name <- as.character(term_labels_by_id[[term_id]] %||% default_name)

    visible <- !(term_id %in% hidden_term_ids)

    # Build Search button HTML if gene data available
    search_btn <- NULL
    if (has_search) {
      genes <- as.character(gene_col_data[[i]] %||% "")
      genes_clean <- gsub("[,;|/]", "\n", genes)
      genes_clean <- gsub("\n+", "\n", genes_clean)
      genes_clean <- trimws(genes_clean)
      safe_term <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", display_name)))))
      safe_genes <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", genes_clean)))))
      search_btn <- tags$td(
        style = "text-align: center;",
        HTML(sprintf(
          '<button type="button" class="btn btn-xs btn-info go-search-genes-btn" data-term="%s" data-genes="%s" title="Search genes in UniProt"><i class="fa fa-search"></i></button>',
          safe_term, safe_genes
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
      tags$td(tags$code(term_id)),
      tags$td(
        style = "min-width: 200px;",
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
    class = "tools-editable-go-table",
    # Add CSS to constrain checkbox width
    tags$style(HTML("
      .tools-editable-go-table .form-group { margin-bottom: 0; }
      .tools-editable-go-table .checkbox { margin: 0; padding: 0; min-height: 0; }
      .tools-editable-go-table input[type='checkbox'] { margin: 0; }
    ")),
    tags$div(style = "overflow-x: auto; max-height: 400px; overflow-y: auto;",
             tags$table(
               class = "table table-sm table-striped",
               tags$thead(tags$tr(lapply(header_cols, function(h) {
                 if (h == "Visible") {
                   tags$th(style = "width: 50px; text-align: center;", h)
                 } else if (h == "Search") {
                   tags$th(style = "width: 50px; text-align: center;", h)
                 } else if (h == term_name_col) {
                   tags$th(style = "min-width: 200px;", h)
                 } else {
                   tags$th(h)
                 }
               }))),
               tags$tbody(rows)
             ))
  )
}

# Bind observers for editable GO table (matches res_bind_editable_go_table)
tools_bind_editable_go_table <- function(
    id_prefix,
    df,
    term_id_col = "term_id",
    input,
    session,
    on_term_name_change = NULL,
    on_visibility_change = NULL
) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
  if (!(term_id_col %in% names(df))) return(invisible(NULL))

  if (is.null(session$userData$tools_editable_go_table_bound)) {
    session$userData$tools_editable_go_table_bound <- new.env(parent = emptyenv())
  }
  bound <- session$userData$tools_editable_go_table_bound

  term_ids <- unique(as.character(df[[term_id_col]]))
  term_ids <- term_ids[nzchar(term_ids)]

  for (term_id in term_ids) {
    local({
      term_id_local <- term_id
      safe_term_id <- tools_safe_input_id(term_id_local)
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
# GO-ORA Tool UI
# ============================================================
tools_goora_ui <- function() {
  defs <- tools_goora_defaults()
  params_defaults <- defs$params %||% list()
  style_defaults <- defs$style %||% list()

  tagList(
    # JavaScript handlers for UniProt search and copy plot
    tags$head(
      tags$script(HTML("
        // UniProt gene search button handler for tools page
        if (window.Shiny && !window.__tools_go_search_genes) {
          window.__tools_go_search_genes = true;
          $(document).on('click', '.go-search-genes-btn', function(e) {
            e.preventDefault();
            e.stopPropagation();
            var btn = e.target.closest('.go-search-genes-btn');
            if (!btn) return;
            var term = btn.getAttribute('data-term') || '';
            var genes = btn.getAttribute('data-genes') || '';
            if (!genes) return;
            // Send to Shiny for UniProt lookup
            Shiny.setInputValue('tools_go_search_genes_click', {
              term: term,
              genes: genes,
              ts: Date.now()
            });
          });
        }
      ")),
      tags$script(HTML("
        // Open URL in new tab handler
        Shiny.addCustomMessageHandler('tools_open_url', function(payload) {
          if (payload && payload.url) {
            window.open(payload.url, '_blank');
          }
        });
      ")),
      tags$script(HTML("
        // Copy plot handler for tools page
        Shiny.addCustomMessageHandler('tools_copy_plot', function(payload) {
          var plotId = payload && payload.id ? payload.id : null;
          if (!plotId) return;

          var container = document.getElementById(plotId);
          var img = container ? container.querySelector('img') : null;

          function showNotification(msg, type) {
            Shiny.notifications.show({
              html: '<span>' + msg + '</span>',
              type: type,
              duration: 3000
            });
          }

          if (!img || !img.src) {
            showNotification('Plot not ready yet. Try again.', 'error');
            return;
          }

          if (!navigator.clipboard || !window.ClipboardItem) {
            showNotification('Clipboard API not available in this browser.', 'error');
            return;
          }

          fetch(img.src)
            .then(function(res) { return res.blob(); })
            .then(function(blob) {
              var item = new ClipboardItem({ [blob.type]: blob });
              return navigator.clipboard.write([item]);
            })
            .then(function() {
              var w = img.naturalWidth || 0;
              var h = img.naturalHeight || 0;
              var label = (w && h) ? ('Copied plot (' + w + ' x ' + h + ' px).') : 'Copied plot to clipboard.';
              showNotification(label, 'message');
            })
            .catch(function(err) {
              showNotification('Copy failed. Check browser permissions.', 'error');
              if (window.console && console.error) console.error(err);
            });
        });
      ")),
      tags$style(HTML("
        .go-search-genes-btn {
          padding: 2px 6px;
          font-size: 11px;
          cursor: pointer;
        }
        .tool-export-buttons {
          display: flex;
          flex-wrap: wrap;
          gap: 6px;
          margin: 10px 0;
        }
        .tool-export-buttons .btn {
          font-size: 12px;
        }
        .tool-table-wrap {
          margin-top: 15px;
        }
        .tool-table-wrap .form-group {
          margin-bottom: 0;
        }
        .tool-table-wrap input[type='checkbox'] {
          margin: 0;
        }
        .tool-table-wrap input[type='text'] {
          padding: 2px 6px;
          font-size: 12px;
          height: auto;
        }
        .go-term-hidden-row {
          opacity: 0.5;
          background-color: #f5f3f0 !important;
        }
      "))
    ),
    div(
      class = "top",
      actionButton("tools_goora_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("GO-ORA", style = "margin: 0;")
    ),
    tags$p("Paste a gene list to run GO over-representation analysis against a TerpBase."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("TerpBase"),
        uiOutput("tools_goora_terpbase_status"),
{
          terpbase_choices <- tools_default_terpbase_choices()
          default_selected <- if (length(terpbase_choices) > 0 && nzchar(terpbase_choices[1])) terpbase_choices[1] else ""
          selectInput(
            "tools_goora_terpbase_default_path",
            "Default TerpBase",
            choices = terpbase_choices,
            selected = default_selected
          )
        },
        actionButton(
          "tools_goora_terpbase_default_load",
          "Load default",
          class = "btn btn-default btn-sm btn-tool-action"
        ),
        fileInput(
          "tools_goora_terpbase_file",
          "Load TerpBase (.terpbase)",
          accept = c(".terpbase", ".rds"),
          placeholder = "No file selected"
        ),
        hr(),
        tags$h4("Gene Input"),
        textAreaInput(
          "tools_goora_genes",
          "Gene list (one per line)",
          rows = 10,
          placeholder = "TP53\nEGFR\nBRCA1"
        ),
        div(
          style = "display: flex; gap: 8px; margin-top: 5px;",
          actionButton("tools_goora_run", "Run GO-ORA", class = "btn-primary btn-tool-action"),
          actionButton("tools_goora_reset", "Reset", class = "btn btn-default btn-tool-action")
        ),
        hr(),
        tags$h4("View"),
        selectInput(
          "tools_goora_ontology_view",
          "Ontology",
          choices = c("Biological Process" = "BP",
                      "Molecular Function" = "MF", "Cellular Component" = "CC"),
          selected = "BP"
        ),
        tools_collapse_section_ui(
          "tools_goora_params_section",
          "Parameters",
          open = FALSE,
          numericInput(
            "tools_goora_fdr_cutoff",
            "FDR cutoff",
            value = params_defaults$fdr_cutoff %||% 0.05,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "tools_goora_min_term_size",
            "Min term size",
            value = params_defaults$min_term_size %||% 5,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_goora_min_overlap",
            "Min overlap",
            value = params_defaults$min_overlap %||% 3,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_goora_max_terms",
            "Terms to show (per ontology)",
            value = params_defaults$max_terms %||% 20,
            min = 1,
            max = 200,
            step = 1
          )
        ),
        tools_collapse_section_ui(
          "tools_goora_plot_section",
          "Plot Options",
          open = FALSE,
          selectInput(
            "tools_goora_plot_type",
            "Plot type",
            choices = c("bar", "dot"),
            selected = style_defaults$plot_type %||% "bar"
          ),
          selectInput(
            "tools_goora_color_mode",
            "Coloring",
            choices = c("fdr", "flat"),
            selected = style_defaults$color_mode %||% "fdr"
          ),
          conditionalPanel(
            condition = "input.tools_goora_color_mode == 'fdr'",
            selectInput(
              "tools_goora_fdr_palette",
              "FDR color palette",
              choices = c("yellow_cap" = "Yellow (significant)", "blue_red" = "Blue-Red"),
              selected = style_defaults$fdr_palette %||% "yellow_cap"
            )
          ),
          conditionalPanel(
            condition = "input.tools_goora_color_mode == 'flat'",
            textInput(
              "tools_goora_flat_color",
              "Flat color (hex)",
              value = style_defaults$flat_color %||% "#B0B0B0"
            )
          ),
          sliderInput(
            "tools_goora_alpha",
            "Opacity",
            min = 0,
            max = 1,
            value = style_defaults$alpha %||% 0.8,
            step = 0.05
          ),
          checkboxInput(
            "tools_goora_show_go_id",
            "Show GO ID in labels",
            value = isTRUE(style_defaults$show_go_id %||% FALSE)
          ),
          checkboxInput(
            "tools_goora_flip_axis",
            "Flip horizontal axis",
            value = FALSE
          ),
          selectInput(
            "tools_goora_x_axis_metric",
            "X-axis metric",
            choices = c("Fold Enrichment" = "fold_enrichment", "-log10(FDR)" = "neglog10_fdr"),
            selected = style_defaults$x_axis_metric %||% "fold_enrichment"
          ),
          numericInput(
            "tools_goora_font_size",
            "Font size",
            value = style_defaults$font_size %||% 14,
            min = 6,
            max = 30,
            step = 1
          ),
          numericInput(
            "tools_goora_axis_text_size",
            "Axis text size",
            value = style_defaults$axis_text_size %||% 20,
            min = 6,
            max = 40,
            step = 1
          ),
          numericInput(
            "tools_goora_width",
            "Plot width (in)",
            value = style_defaults$width %||% 12,
            min = 2,
            max = 24,
            step = 0.5
          ),
          numericInput(
            "tools_goora_height",
            "Plot height (in)",
            value = style_defaults$height %||% 6,
            min = 2,
            max = 24,
            step = 0.5
          )
        ),
        tools_collapse_section_ui(
          "tools_goora_filter_section",
          "Table Filters",
          open = FALSE,
          numericInput(
            "tools_goora_filter_fold_enrichment_min",
            "Min fold enrichment",
            value = NA,
            min = 0,
            step = 0.1
          ),
          numericInput(
            "tools_goora_filter_n_genes_min",
            "Min # genes",
            value = NA,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_goora_filter_fdr_max",
            "Max FDR",
            value = NA,
            min = 0,
            max = 1,
            step = 0.001
          ),
          actionButton(
            "tools_goora_filter_clear",
            "Clear Filters",
            class = "btn btn-default btn-sm",
            style = "margin-top: 8px;"
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_goora_summary"),
        uiOutput("tools_goora_tabs")
      )
    )
  )
}

# ============================================================
# GO-ORA Server Logic
# ============================================================
tools_goora_server <- function(input, output, session, app_state, rv, defs_goora) {
  safe_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) default else v
  }

  safe_int <- function(x, default) {
    v <- suppressWarnings(as.integer(x))
    if (!is.finite(v) || v <= 0) default else v
  }

  parse_gene_input <- function(text) {
    if (is.null(text) || !nzchar(text)) return(character(0))
    lines <- unlist(strsplit(text, "\\r?\\n"))
    parts <- unlist(strsplit(lines, "[,;]"))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    unique(parts)
  }

  plot_dims_px <- function() {
    dpi <- 150
    w_in <- safe_num(input$tools_goora_width, defs_goora$style$width %||% 12)
    h_in <- safe_num(input$tools_goora_height, defs_goora$style$height %||% 6)

    max_px <- 2600
    w_px <- as.integer(round(w_in * dpi))
    h_px <- as.integer(round(h_in * dpi))

    scale <- min(1, max_px / max(w_px, h_px))
    list(
      w = as.integer(round(w_px * scale)),
      h = as.integer(round(h_px * scale))
    )
  }

  # Reset button handler - clears results and resets parameters
  observeEvent(input$tools_goora_reset, {
    rv$results <- NULL
    rv$rendered <- NULL
    rv$status_msg <- NULL
    rv$status_level <- NULL
    rv$input_count <- NULL
    rv$hidden_terms <- character()
    rv$term_labels <- list()
    # Clear stored parameters so they don't get restored on navigation
    rv$stored_params <- NULL
    rv$stored_genes <- NULL

    # Reset parameter inputs to defaults
    updateNumericInput(session, "tools_goora_fdr_cutoff", value = defs_goora$params$fdr_cutoff %||% 0.05)
    updateNumericInput(session, "tools_goora_min_term_size", value = defs_goora$params$min_term_size %||% 5)
    updateNumericInput(session, "tools_goora_min_overlap", value = defs_goora$params$min_overlap %||% 3)
    updateNumericInput(session, "tools_goora_max_terms", value = defs_goora$params$max_terms %||% 20)
    updateTextAreaInput(session, "tools_goora_genes", value = "")
  }, ignoreInit = TRUE)

  # Handle UniProt gene search button clicks
  observeEvent(input$tools_go_search_genes_click, {
    req(input$tools_go_search_genes_click)
    genes <- input$tools_go_search_genes_click$genes
    term <- input$tools_go_search_genes_click$term

    if (!nzchar(genes)) return()

    # Split genes and build UniProt search URL
    gene_list <- unlist(strsplit(genes, "\n"))
    gene_list <- trimws(gene_list)
    gene_list <- gene_list[nzchar(gene_list)]

    if (length(gene_list) == 0) return()

    # Open UniProt search in new tab
    query <- paste(gene_list, collapse = " OR ")
    url <- paste0("https://www.uniprot.org/uniprotkb?query=", utils::URLencode(query, reserved = TRUE))

    # Use JavaScript to open URL
    session$sendCustomMessage("tools_open_url", list(url = url))
  }, ignoreInit = TRUE)

  output$tools_goora_terpbase_status <- renderUI({
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
  progress_goora_rx <- reactive({
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
        rv$rendered <- tb_render_goora(result$result, rv$pending_style, meta = NULL)
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

  observeEvent(input$tools_goora_run, {
    # Only reset status and results, preserve parameters
    rv$status_msg <- NULL
    rv$status_level <- NULL
    rv$results <- NULL
    rv$rendered <- NULL
    rv$input_count <- NULL
    # Reset visibility state for new run
    rv$hidden_terms <- character()
    rv$term_labels <- list()

    tb <- app_state$terpbase
    if (is.null(tb)) {
      rv$status_msg <- "No TerpBase loaded. Load one above or build one in the TerpBase page."
      rv$status_level <- "error"
      rv$run_status <- "idle"
      return()
    }

    genes <- parse_gene_input(input$tools_goora_genes)
    rv$input_count <- length(genes)

    if (length(genes) == 0) {
      rv$status_msg <- "Enter at least one gene symbol to run GO-ORA."
      rv$status_level <- "warn"
      rv$run_status <- "idle"
      return()
    }

    # Collect params and style from inputs (lightweight)
    params <- list(
      fdr_cutoff = safe_num(input$tools_goora_fdr_cutoff, defs_goora$params$fdr_cutoff %||% 0.05),
      min_term_size = safe_int(input$tools_goora_min_term_size, defs_goora$params$min_term_size %||% 5),
      min_overlap = safe_int(input$tools_goora_min_overlap, defs_goora$params$min_overlap %||% 3),
      max_terms = safe_int(input$tools_goora_max_terms, defs_goora$params$max_terms %||% 20)
    )

    style <- list(
      plot_type = input$tools_goora_plot_type %||% defs_goora$style$plot_type %||% "bar",
      color_mode = input$tools_goora_color_mode %||% defs_goora$style$color_mode %||% "fdr",
      fdr_palette = input$tools_goora_fdr_palette %||% defs_goora$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_goora_flat_color %||% "")) {
        input$tools_goora_flat_color
      } else {
        defs_goora$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_goora_alpha, defs_goora$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_goora_show_go_id),
      font_size = safe_int(input$tools_goora_font_size, defs_goora$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_goora_axis_text_size, defs_goora$style$axis_text_size %||% 20),
      width = safe_num(input$tools_goora_width, defs_goora$style$width %||% 12),
      height = safe_num(input$tools_goora_height, defs_goora$style$height %||% 6),
      flip_axis = isTRUE(input$tools_goora_flip_axis),
      x_axis_metric = input$tools_goora_x_axis_metric %||% defs_goora$style$x_axis_metric %||% "fold_enrichment",
      ontology_filter = "all"  # Always compute all ontologies
    )

    rv$pending_style <- style

    # Show progress immediately - before any heavy work
    rv$progress_path <- tempfile(fileext = ".json")
    rv$run_start_time <- Sys.time()
    rv$run_status <- "running"
    write_progress(rv$progress_path, "running", "Starting GO-ORA analysis...", 5)

    has_callr <- requireNamespace("callr", quietly = TRUE)

    if (has_callr) {
      app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

      rv$bg_process <- callr::r_bg(
        func = function(app_root, tb, genes, params, progress_path) {
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
            query_proteins = genes,
            terpbase = tb
          )

          write_prog("Running over-representation analysis...", 30)

          result <- tryCatch({
            res <- stats_goora_run(payload, params = params, context = list(terpbase = tb))
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
          genes = genes,
          params = params,
          progress_path = rv$progress_path
        ),
        package = TRUE,
        supervise = TRUE
      )
    } else {
      # Synchronous fallback - must do prep work here
      tb <- tools_ensure_terpbase_go_mappings(tb)
      payload <- list(
        ok = TRUE,
        params = params,
        query_proteins = genes,
        terpbase = tb
      )

      res <- tryCatch({
        stats_goora_run(payload, params = params, context = list(terpbase = tb))
      }, error = function(e) {
        rv$status_msg <- conditionMessage(e)
        rv$status_level <- "error"
        rv$run_status <- "error"
        NULL
      })

      if (!is.null(res)) {
        rv$results <- res
        rv$rendered <- tb_render_goora(res, style, meta = NULL)
        rv$run_status <- "done"
      }
    }
  }, ignoreInit = TRUE)

  # Stop button handler
  observeEvent(input$tools_goora_stop, {
    if (!is.null(rv$bg_process) && rv$bg_process$is_alive()) {
      tryCatch(rv$bg_process$kill(), error = function(e) NULL)
      rv$bg_process <- NULL
      rv$run_status <- "idle"
      rv$status_msg <- "Analysis stopped by user"
      rv$status_level <- "warn"
    }
  }, ignoreInit = TRUE)

  output$tools_goora_summary <- renderUI({
    if (identical(rv$run_status, "running")) {
      p <- progress_goora_rx()
      pct <- suppressWarnings(as.numeric(p$pct %||% 0))
      if (is.na(pct)) pct <- 0
      pct <- max(0, min(100, pct))
      msg <- p$message %||% "Processing..."

      return(tagList(
        div(class = "tool-status-row",
          div(class = "tool-status-pill running", "Running"),
          div(class = "tool-status-msg", msg),
          actionButton("tools_goora_stop", "Stop", class = "btn btn-danger btn-sm", style = "margin-left: auto;")
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
      return(tags$div(class = "text-muted", "Run GO-ORA to see results."))
    }

    qinfo <- res$data$query_info %||% list()
    n_query <- qinfo$n_query %||% NA_integer_
    n_background <- qinfo$n_background %||% NA_integer_
    n_terms <- if (is.data.frame(res$data$terms %||% NULL)) nrow(res$data$terms) else 0L
    n_input <- rv$input_count %||% NA_integer_
    n_unmatched <- if (is.finite(n_input) && is.finite(n_query)) n_input - n_query else NA_integer_

    log_df <- res$data$log %||% NULL
    log_msg <- NULL
    if (!is.null(log_df) && nrow(log_df) > 0) {
      last <- log_df[nrow(log_df), , drop = FALSE]
      log_msg <- paste0("[", last$level, "] ", last$message)
    }

    tags$div(
      class = "card",
      tags$h4("GO-ORA summary"),
      tags$p(sprintf("Input genes: %s", ifelse(is.finite(n_input), n_input, "NA"))),
      tags$p(sprintf("Matched to TerpBase: %s", ifelse(is.finite(n_query), n_query, "NA"))),
      if (is.finite(n_unmatched) && n_unmatched > 0) {
        tags$p(sprintf("Unmatched input genes: %s", n_unmatched))
      },
      tags$p(sprintf("Background size: %s", ifelse(is.finite(n_background), n_background, "NA"))),
      tags$p(sprintf("Enriched terms: %s", n_terms)),
      if (!is.null(log_msg)) tags$p(log_msg)
    )
  })

  output$tools_goora_tabs <- renderUI({
    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No GO-ORA results yet."))
    }

    # Calculate aspect ratio from current inputs
    w_in <- safe_num(input$tools_goora_width, defs_goora$style$width %||% 12)
    h_in <- safe_num(input$tools_goora_height, defs_goora$style$height %||% 6)
    ar <- w_in / h_in

    tagList(
      # Export buttons
      div(
        class = "tool-export-buttons",
        actionButton("tools_goora_download_png", "Download PNG", class = "btn btn-sm btn-default", icon = icon("download")),
        actionButton("tools_goora_download_pdf", "Download PDF", class = "btn btn-sm btn-default", icon = icon("file-pdf")),
        actionButton("tools_goora_copy_plot", "Copy Plot", class = "btn btn-sm btn-default", icon = icon("copy")),
        downloadButton("tools_goora_download_excel", "Download Excel", class = "btn btn-sm btn-default")
      ),
      div(
        class = "tool-plot-box",
        style = sprintf("--tool-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
        plotOutput("tools_goora_plot", height = "100%")
      ),
      div(
        class = "tool-table-wrap",
        uiOutput("tools_goora_table_ui")
      )
    )
  })

  # Build current style from inputs (reactive helper)
  current_goora_style <- reactive({
    list(
      plot_type = input$tools_goora_plot_type %||% defs_goora$style$plot_type %||% "bar",
      color_mode = input$tools_goora_color_mode %||% defs_goora$style$color_mode %||% "fdr",
      fdr_palette = input$tools_goora_fdr_palette %||% defs_goora$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_goora_flat_color %||% "")) {
        input$tools_goora_flat_color
      } else {
        defs_goora$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_goora_alpha, defs_goora$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_goora_show_go_id),
      font_size = safe_int(input$tools_goora_font_size, defs_goora$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_goora_axis_text_size, defs_goora$style$axis_text_size %||% 20),
      width = safe_num(input$tools_goora_width, defs_goora$style$width %||% 12),
      height = safe_num(input$tools_goora_height, defs_goora$style$height %||% 6),
      flip_axis = isTRUE(input$tools_goora_flip_axis),
      x_axis_metric = input$tools_goora_x_axis_metric %||% defs_goora$style$x_axis_metric %||% "fold_enrichment",
      ontology_filter = input$tools_goora_ontology_view %||% "BP"
    )
  })

  # Get current ggplot object for export
  current_goora_plot <- reactive({
    res <- rv$results
    if (is.null(res)) return(NULL)

    style <- current_goora_style()
    hidden_terms <- rv$hidden_terms %||% character()
    term_labels <- rv$term_labels %||% list()

    # Build meta with visibility info
    meta <- list(visibility = list(hidden_terms = hidden_terms, term_labels = term_labels))
    rend <- tb_render_goora(res, style, meta = meta)

    ont <- tolower(input$tools_goora_ontology_view %||% "BP")
    plot_key <- paste0(ont, "_plot")
    p <- rend$plots[[plot_key]]

    if (is.null(p) && length(rend$plots) > 0) {
      p <- rend$plots[[1]]
    }
    p
  })

  # Reactive to re-render plot when plot options, ontology view, or visibility changes
  output$tools_goora_plot <- renderPlot({
    p <- current_goora_plot()
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No enriched terms for this ontology.")
      return(invisible(NULL))
    }
    suppressMessages(print(p))
  },
  width = function() plot_dims_px()$w,
  height = function() plot_dims_px()$h,
  res = 150
  )

  # Clear filters button handler

  observeEvent(input$tools_goora_filter_clear, {
    updateNumericInput(session, "tools_goora_filter_fold_enrichment_min", value = NA)
    updateNumericInput(session, "tools_goora_filter_n_genes_min", value = NA)
    updateNumericInput(session, "tools_goora_filter_fdr_max", value = NA)
  }, ignoreInit = TRUE)

  # Helper function to apply table filters
  apply_goora_table_filters <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

    # Filter by fold_enrichment min
    fe_min <- input$tools_goora_filter_fold_enrichment_min
    if (!is.null(fe_min) && is.finite(fe_min) && "fold_enrichment" %in% names(df)) {
      df <- df[!is.na(df$fold_enrichment) & df$fold_enrichment >= fe_min, , drop = FALSE]
    }

    # Filter by n_genes min (column may be n, n_genes, or count)
    n_min <- input$tools_goora_filter_n_genes_min
    if (!is.null(n_min) && is.finite(n_min)) {
      n_col <- intersect(c("n_genes", "n", "count", "Count"), names(df))[1]
      if (!is.na(n_col)) {
        df <- df[!is.na(df[[n_col]]) & df[[n_col]] >= n_min, , drop = FALSE]
      }
    }

    # Filter by FDR max
    fdr_max <- input$tools_goora_filter_fdr_max
    if (!is.null(fdr_max) && is.finite(fdr_max) && "fdr" %in% names(df)) {
      df <- df[!is.na(df$fdr) & df$fdr <= fdr_max, , drop = FALSE]
    }

    df
  }

  # Editable table UI with visibility checkboxes, term name editing, and search
  output$tools_goora_table_ui <- renderUI({
    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No table data."))
    }

    ont <- tolower(input$tools_goora_ontology_view %||% "BP")
    style <- current_goora_style()
    rend <- tb_render_goora(res, style, meta = NULL)

    table_key <- paste0(ont, "_table")
    df <- rend$tables[[table_key]]
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }

    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(tags$div(class = "text-muted", "No terms to display."))
    }

    # Apply table filters
    df <- apply_goora_table_filters(df)

    if (nrow(df) == 0) {
      return(tags$div(class = "text-muted", "No terms match the current filters."))
    }

    hidden_term_ids <- rv$hidden_terms %||% character()
    term_labels_by_id <- rv$term_labels %||% list()

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
      id_prefix = paste0("tools_goora_", ont),
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
    res <- rv$results
    if (is.null(res)) return()

    ont <- tolower(input$tools_goora_ontology_view %||% "BP")
    style <- current_goora_style()
    rend <- tb_render_goora(res, style, meta = NULL)

    table_key <- paste0(ont, "_table")
    df <- rend$tables[[table_key]]
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }
    if (is.null(df) || !is.data.frame(df)) return()

    # Apply table filters
    df <- apply_goora_table_filters(df)
    if (nrow(df) == 0) return()

    # Determine term_id column
    term_id_col <- if ("term_id" %in% names(df)) "term_id" else NULL
    if (is.null(term_id_col)) return()

    # Bind observers using the same pattern as result viewer
    tools_bind_editable_go_table(
      id_prefix = paste0("tools_goora_", ont),
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
  observeEvent(input$tools_goora_download_png, {
    p <- current_goora_plot()
    if (is.null(p)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PNG",
      selectInput("tools_goora_png_dpi", "DPI", choices = c(150, 300, 600), selected = 300),
      downloadButton("tools_goora_png_confirm", "Download", class = "btn-primary"),
      actionButton("tools_goora_png_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_png_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_goora_png_confirm <- downloadHandler(
    filename = function() {
      ont <- input$tools_goora_ontology_view %||% "BP"
      paste0("goora_", tolower(ont), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      p <- current_goora_plot()
      if (is.null(p)) return()
      style <- current_goora_style()
      dpi <- as.numeric(input$tools_goora_png_dpi %||% 300)
      w <- style$width %||% 12
      h <- style$height %||% 6
      png_type <- if (capabilities("cairo")) "cairo-png" else NULL
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", dpi = dpi,
                      device = grDevices::png, type = png_type)
      removeModal()
    }
  )

  # Export: Download PDF
  observeEvent(input$tools_goora_download_pdf, {
    p <- current_goora_plot()
    if (is.null(p)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PDF",
      downloadButton("tools_goora_pdf_confirm", "Download", class = "btn-primary"),
      actionButton("tools_goora_pdf_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_pdf_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_goora_pdf_confirm <- downloadHandler(
    filename = function() {
      ont <- input$tools_goora_ontology_view %||% "BP"
      paste0("goora_", tolower(ont), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      p <- current_goora_plot()
      if (is.null(p)) return()
      style <- current_goora_style()
      w <- style$width %||% 12
      h <- style$height %||% 6
      pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", device = pdf_device)
      removeModal()
    }
  )

  # Export: Copy Plot
  observeEvent(input$tools_goora_copy_plot, {
    session$sendCustomMessage("tools_copy_plot", list(id = "tools_goora_plot"))
  }, ignoreInit = TRUE)

  # Export: Download Excel
  output$tools_goora_download_excel <- downloadHandler(
    filename = function() {
      ont <- input$tools_goora_ontology_view %||% "BP"
      paste0("goora_", tolower(ont), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      res <- rv$results
      if (is.null(res)) {
        writeLines("No results to export.", file)
        return()
      }

      ont <- tolower(input$tools_goora_ontology_view %||% "BP")
      style <- current_goora_style()
      rend <- tb_render_goora(res, style, meta = NULL)

      table_key <- paste0(ont, "_table")
      df <- rend$tables[[table_key]]
      if (is.null(df) && length(rend$tables) > 0) {
        df <- rend$tables[[1]]
      }

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        writeLines("No data to export.", file)
        return()
      }

      # Apply term labels for export
      term_labels <- rv$term_labels %||% list()
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
        openxlsx::addWorksheet(wb, "GO-ORA Results")
        openxlsx::writeData(wb, "GO-ORA Results", df)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        utils::write.csv(df, file, row.names = FALSE)
      }
    }
  )
}
