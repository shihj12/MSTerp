# R/pages/tools/tool_id_reconcile.R
# ID Reconcile Tool - Metabolite ID reconciliation against MetaboBase

# ============================================================
# UI
# ============================================================
tools_id_reconcile_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_reconcile_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("Metabolite ID Reconcile", style = "margin: 0;")
    ),
    tags$p("Upload a spreadsheet, select the column with metabolite names or IDs, and reconcile against MetaboBase."),
    two_panel_ui(
      left_ui = tagList(
        uiOutput("tools_reconcile_metabobase_status"),
        hr(),
        tags$h4("Data"),
        fileInput(
          "tools_reconcile_file",
          "Upload data (.xlsx or .xls)",
          accept = c(".xlsx", ".xls"),
          placeholder = "No file selected"
        ),
        uiOutput("tools_reconcile_col_select"),
        div(
          style = "display: flex; gap: 8px; margin-top: 5px;",
          actionButton("tools_reconcile_run", "Run", class = "btn-primary btn-tool-action"),
          actionButton("tools_reconcile_reset", "Reset", class = "btn btn-default btn-tool-action")
        )
      ),
      right_ui = tagList(
        uiOutput("tools_reconcile_status"),
        uiOutput("tools_reconcile_results_ui")
      )
    )
  )
}

# ============================================================
# Server
# ============================================================
tools_id_reconcile_server <- function(input, output, session, app_state, rv) {

  # Auto-load default MetaboBase if not already loaded
  isolate({
    if (is.null(app_state$metabobase)) {
      choices <- tools_default_metabobase_choices()
      path <- choices[1]
      if (!is.null(path) && nzchar(path) && file.exists(path)) {
        tryCatch({
          mb <- readRDS(path)
          v <- metabobase_validate(mb)
          if (v$ok) app_state$metabobase <- mb
        }, error = function(e) NULL)
      }
    }
  })

  output$tools_reconcile_metabobase_status <- renderUI({
    mb <- app_state$metabobase
    if (is.null(mb)) {
      return(tags$div(
        class = "text-danger",
        "No MetaboBase available. Place a .metabobase file in the metabobase/ directory."
      ))
    }

    library_name <- mb$library_name %||% "(unnamed)"
    n_metabolites <- mb$n_metabolites %||% NA_integer_

    tags$div(
      class = "text-muted",
      tags$strong("MetaboBase:"), " ", library_name,
      if (is.finite(n_metabolites)) tags$span(sprintf(" (%s metabolites)", n_metabolites))
    )
  })

  # ---- File upload & column detection ----
  observeEvent(input$tools_reconcile_file, {
    req(input$tools_reconcile_file)
    path <- input$tools_reconcile_file$datapath

    tryCatch({
      if (!requireNamespace("readxl", quietly = TRUE)) {
        rv$status_msg <- "Package 'readxl' is required to read xlsx files."
        rv$status_level <- "error"
        return()
      }
      df <- as.data.frame(readxl::read_excel(path))
      if (ncol(df) == 0) {
        rv$status_msg <- "Uploaded file has no columns."
        rv$status_level <- "error"
        return()
      }
      rv$raw_data <- df
      rv$result_df <- NULL
      rv$running <- FALSE
      rv$status_msg <- sprintf("Loaded %d rows, %d columns.", nrow(df), ncol(df))
      rv$status_level <- "info"
    }, error = function(e) {
      rv$status_msg <- paste("Error reading file:", conditionMessage(e))
      rv$status_level <- "error"
    })
  }, ignoreInit = TRUE)

  output$tools_reconcile_col_select <- renderUI({
    req(rv$raw_data)
    cols <- names(rv$raw_data)
    selectInput("tools_reconcile_id_col", "Column with metabolite names/IDs", choices = cols, selected = cols[1])
  })

  # ---- Run reconciliation ----
  observeEvent(input$tools_reconcile_run, {
    mb <- app_state$metabobase
    if (is.null(mb)) {
      rv$status_msg <- "No MetaboBase available."
      rv$status_level <- "error"
      return()
    }
    if (is.null(rv$raw_data)) {
      rv$status_msg <- "No data file uploaded."
      rv$status_level <- "error"
      return()
    }

    id_col <- input$tools_reconcile_id_col
    if (is.null(id_col) || !id_col %in% names(rv$raw_data)) {
      rv$status_msg <- "Selected column not found in data."
      rv$status_level <- "error"
      return()
    }

    # Show spinner
    rv$running <- TRUE
    rv$result_df <- NULL
    rv$status_msg <- NULL
    rv$status_level <- NULL

    df <- rv$raw_data
    id_vals <- as.character(df[[id_col]])
    query_ids <- unique(id_vals[!is.na(id_vals) & nzchar(id_vals)])

    if (length(query_ids) == 0) {
      rv$running <- FALSE
      rv$status_msg <- "No non-empty IDs found in selected column."
      rv$status_level <- "error"
      return()
    }

    # Run in background via callr so the spinner renders
    mb_path <- tempfile(fileext = ".rds")
    saveRDS(mb, mb_path)

    rv$bg_process <- callr::r_bg(
      function(query_ids, id_vals, df, mb_path, reconcile_fn_path, metabobase_path) {
        source(metabobase_path, local = TRUE)
        source(reconcile_fn_path, local = TRUE)
        mb <- readRDS(mb_path)

        reconciled <- metabobase_reconcile_ids(
          query_ids = query_ids,
          metabobase = mb,
          use_pubchem_resolver = TRUE
        )

        n_rows <- nrow(df)
        resolved_from <- rep(NA_character_, n_rows)
        layer_map <- reconciled$resolution_layer
        matched_layers <- layer_map[id_vals]
        resolved_from[!is.na(matched_layers)] <- as.character(matched_layers[!is.na(matched_layers)])

        resolved_met_ids <- as.character(reconciled$id_map[id_vals])

        kegg_ids <- rep(NA_character_, n_rows)
        hmdb_ids <- rep(NA_character_, n_rows)
        pubchem_cids <- rep(NA_character_, n_rows)

        id_xref <- mb$id_xref
        if (!is.null(id_xref) && is.data.frame(id_xref) && nrow(id_xref) > 0) {
          xref_idx <- match(resolved_met_ids, id_xref$metabolite_id)
          if ("kegg_id" %in% names(id_xref)) {
            kegg_ids[!is.na(xref_idx)] <- as.character(id_xref$kegg_id[xref_idx[!is.na(xref_idx)]])
          }
          if ("hmdb_id" %in% names(id_xref)) {
            hmdb_ids[!is.na(xref_idx)] <- as.character(id_xref$hmdb_id[xref_idx[!is.na(xref_idx)]])
          }
          pubchem_col <- if ("pubchem_cid" %in% names(id_xref)) "pubchem_cid" else if ("pubchem_id" %in% names(id_xref)) "pubchem_id" else NULL
          if (!is.null(pubchem_col)) {
            pubchem_cids[!is.na(xref_idx)] <- as.character(id_xref[[pubchem_col]][xref_idx[!is.na(xref_idx)]])
          }
        }

        df$resolved_from <- resolved_from
        df$kegg_id <- kegg_ids
        df$hmdb_id <- hmdb_ids
        df$pubchem_cid <- pubchem_cids

        list(df = df, stats = reconciled$stats)
      },
      args = list(
        query_ids = query_ids,
        id_vals = id_vals,
        df = df,
        mb_path = mb_path,
        reconcile_fn_path = normalizePath(file.path("R", "engines", "metabobase.R"), winslash = "/"),
        metabobase_path = normalizePath(file.path("R", "engines", "metabobase.R"), winslash = "/")
      ),
      supervise = TRUE
    )
  }, ignoreInit = TRUE)

  # ---- Poll background process ----
  observe({
    req(rv$bg_process)
    if (!isTRUE(rv$running)) return()

    invalidateLater(500, session)

    proc <- rv$bg_process
    if (proc$is_alive()) return()

    rv$running <- FALSE
    rv$bg_process <- NULL

    tryCatch({
      result <- proc$get_result()
      rv$result_df <- result$df

      s <- result$stats
      rv$status_msg <- sprintf(
        "Resolved %d/%d (direct=%d, name=%d, synonym=%d, xref=%d, pubchem=%d, unmatched=%d)",
        s$n_query - s$n_unmatched, s$n_query,
        s$n_direct, s$n_by_name, s$n_by_synonym, s$n_by_xref, s$n_by_pubchem, s$n_unmatched
      )
      rv$status_level <- "success"
    }, error = function(e) {
      rv$status_msg <- paste("Reconciliation failed:", conditionMessage(e))
      rv$status_level <- "error"
    })
  })

  # ---- Reset ----
  observeEvent(input$tools_reconcile_reset, {
    rv$raw_data <- NULL
    rv$result_df <- NULL
    rv$running <- FALSE
    rv$bg_process <- NULL
    rv$status_msg <- NULL
    rv$status_level <- NULL
  }, ignoreInit = TRUE)

  # ---- Status display ----
  output$tools_reconcile_status <- renderUI({
    msg <- rv$status_msg
    if (is.null(msg)) return(NULL)
    lvl <- rv$status_level %||% "info"
    cls <- switch(lvl, error = "text-danger", success = "text-success", "text-muted")
    tags$div(class = cls, style = "margin-bottom: 10px;", msg)
  })

  # ---- Results panel (spinner or table) ----
  output$tools_reconcile_results_ui <- renderUI({
    if (isTRUE(rv$running)) {
      return(div(
        class = "spinner-container",
        div(class = "spinner"),
        div(class = "spinner-text", "Reconciling metabolite IDs...")
      ))
    }

    df <- rv$result_df
    if (is.null(df)) return(NULL)

    tagList(
      downloadButton("tools_reconcile_download", "Download (.xlsx)", class = "btn-primary", style = "margin-bottom: 10px;"),
      DT::dataTableOutput("tools_reconcile_table")
    )
  })

  output$tools_reconcile_table <- DT::renderDataTable({
    req(rv$result_df)
    DT::datatable(
      rv$result_df,
      options = list(scrollX = TRUE, pageLength = 20),
      rownames = FALSE
    )
  })

  output$tools_reconcile_download <- downloadHandler(
    filename = function() {
      paste0("reconciled_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$result_df)
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        writeLines("openxlsx package not installed.", file)
        return()
      }
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "data")
      openxlsx::writeData(wb, "data", rv$result_df)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  invisible(NULL)
}
