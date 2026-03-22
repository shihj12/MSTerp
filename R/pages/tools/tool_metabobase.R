# R/pages/tools/tool_metabobase.R
# MetaboBase Builder Tool - Build and validate MetaboBase files for metabolite analyses

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# ============================================================
# MetaboBase Tool Defaults
# ============================================================
tools_metabobase_defaults <- function() {
  list(
    params = list(),
    style = list()
  )
}

# Derive a friendly display label from a database filename
# e.g. "hmdb_human.metabobase" -> "HMDB Human", "human_UniProt_jan_2026.terpbase" -> "UniProt Human"
.friendly_db_label <- function(filename) {
  name <- tools::file_path_sans_ext(basename(filename))
  parts <- tolower(strsplit(name, "[_\\-]")[[1]])

  source_label <- if ("uniprot" %in% parts) "UniProt"
                  else if ("hmdb" %in% parts) "HMDB"
                  else if ("kegg" %in% parts) "KEGG"
                  else if ("corum" %in% parts) "CORUM"
                  else ""

  organism_label <- if ("human" %in% parts) "Human"
                    else if ("mouse" %in% parts) "Mouse"
                    else if ("rat" %in% parts) "Rat"
                    else ""

  if (nzchar(source_label) && nzchar(organism_label)) {
    paste(source_label, organism_label)
  } else if (nzchar(source_label)) {
    source_label
  } else {
    tools::toTitleCase(gsub("[_\\-]", " ", name))
  }
}

# Helper to list available metabobase files
tools_default_metabobase_choices <- function() {
  base_dir <- "metabobase"
  if (!dir.exists(base_dir)) {
    return(c("No default metabobase found" = ""))
  }

  files <- list.files(base_dir, pattern = "\\.(metabobase|rds)$", ignore.case = TRUE)
  if (length(files) == 0) {
    return(c("No default metabobase found" = ""))
  }

  files <- sort(files)
  stats::setNames(file.path(base_dir, files), vapply(files, .friendly_db_label, character(1)))
}

# ============================================================
# MetaboBase Tool UI
# ============================================================
tools_metabobase_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_metabobase_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("MetaboBase Builder", style = "margin: 0;")
    ),
    tags$p("Build a MetaboBase annotation database for metabolite pathway and class enrichment analyses."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("Mode"),
        radioButtons(
          "tools_metabobase_mode",
          NULL,
          choices = c(
            "Build from CSV file" = "build_csv",
            "Build from KEGG (online)" = "build_kegg",
            "Build from HMDB (local XML)" = "build_hmdb",
            "Validate existing .metabobase" = "validate"
          ),
          selected = "build_csv"
        ),
        hr(),

        # Build from CSV mode
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'build_csv'",
          tags$h4("CSV Input"),
          fileInput(
            "tools_metabobase_csv_file",
            "Metabolite CSV file",
            accept = c(".csv", ".txt")
          ),
          textInput("tools_metabobase_csv_library_name", "Library name", value = ""),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_metabobase_build_csv", "Build MetaboBase", class = "btn-primary btn-tool-action"),
            actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Build from KEGG mode
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'build_kegg'",
          tags$h4("KEGG Library Builder"),
          selectInput(
            "tools_metabobase_kegg_organism",
            "Organism",
            choices = c(
              "Human (hsa)" = "hsa",
              "Mouse (mmu)" = "mmu",
              "Rat (rno)" = "rno",
              "Fruit fly (dme)" = "dme",
              "Yeast (sce)" = "sce",
              "E. coli (eco)" = "eco",
              "Arabidopsis (ath)" = "ath"
            ),
            selected = "hsa"
          ),
          textInput("tools_metabobase_kegg_library_name", "Library name (optional)", value = ""),
          tags$p(class = "text-muted", style = "font-size: 12px;",
            "This will fetch pathway data from the KEGG API. May require several minutes."
          ),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_metabobase_build_kegg", "Build from KEGG", class = "btn-primary btn-tool-action"),
            actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Build from HMDB mode
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'build_hmdb'",
          tags$h4("HMDB Library Builder"),
          fileInput(
            "tools_metabobase_hmdb_xml_file",
            "HMDB XML file (hmdb_metabolites.xml)",
            accept = c(".xml")
          ),
          textInput("tools_metabobase_hmdb_library_name", "Library name (optional)", value = ""),
          tags$p(class = "text-muted", style = "font-size: 12px;",
            "Upload the hmdb_metabolites.xml file from ",
            tags$a(href = "https://hmdb.ca/downloads", target = "_blank", "HMDB Downloads"),
            ". The uncompressed XML is ~7 GB. Building may take 10-30 minutes and requires ~8 GB RAM."
          ),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_metabobase_build_hmdb", "Build from HMDB", class = "btn-primary btn-tool-action"),
            actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Validate mode
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'validate'",
          tags$h4("MetaboBase File"),
          fileInput(
            "tools_metabobase_file",
            "Select .metabobase file",
            accept = c(".metabobase", ".rds")
          ),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_metabobase_validate", "Validate", class = "btn-primary btn-tool-action"),
            actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        hr(),
        uiOutput("tools_metabobase_download_ui"),

        hr(),
        tools_collapse_section_ui(
          "tools_metabobase_csv_info_section",
          "CSV Format Requirements",
          open = FALSE,
          tags$p(tags$strong("Required columns:")),
          tags$ul(
            tags$li(tags$code("metabolite_id"), " - Primary identifier"),
            tags$li(tags$code("name"), " - Common metabolite name")
          ),
          tags$p(tags$strong("Optional columns:")),
          tags$ul(
            tags$li(tags$code("hmdb_id"), " - HMDB ID"),
            tags$li(tags$code("kegg_id"), " - KEGG compound ID"),
            tags$li(tags$code("chebi_id"), " - ChEBI ID"),
            tags$li(tags$code("formula"), " - Molecular formula"),
            tags$li(tags$code("class"), " - Chemical class"),
            tags$li(tags$code("subclass"), " - Chemical subclass"),
            tags$li(tags$code("superclass"), " - Chemical superclass"),
            tags$li(tags$code("pathway_kegg"), " - KEGG pathway IDs (semicolon-separated)"),
            tags$li(tags$code("pathway_reactome"), " - Reactome pathway IDs (semicolon-separated)")
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_metabobase_status"),
        hr(),
        uiOutput("tools_metabobase_summary"),
        hr(),
        tags$h4("Messages"),
        verbatimTextOutput("tools_metabobase_messages")
      )
    )
  )
}

# ============================================================
# MetaboBase Tool Server
# ============================================================
tools_metabobase_server <- function(input, output, session, app_state, rv_metabobase) {

  # Message helper
  msg_push <- function(x) {
    rv_metabobase$messages <- c(rv_metabobase$messages, paste0(format(Sys.time(), "%H:%M:%S"), "  ", x))
  }

  # Format time helper
  format_hms <- function(seconds) {
    s <- max(0L, as.integer(round(seconds)))
    h <- s %/% 3600L
    m <- (s %% 3600L) %/% 60L
    sec <- s %% 60L
    sprintf("%02dh %02dm %02ds", h, m, sec)
  }

  # Busy indicator helper
  set_busy <- function(active, message = "", percent = NULL) {
    if (exists("msterp_set_busy", mode = "function", inherits = TRUE)) {
      msterp_set_busy(session, active = active, message = message, percent = percent)
    }
  }

  # Status icons
  icon_ok  <- function() tags$span(style = "color:var(--status-success-text);font-weight:700;margin-right:8px;", HTML("&#10003;"))
  icon_bad <- function() tags$span(style = "color:var(--status-error-text);font-weight:700;margin-right:8px;", HTML("&#10007;"))

  # CSV header validation
  csv_header_check <- reactive({
    f <- input$tools_metabobase_csv_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return(NULL)

    tryCatch(
      metabobase_validate_csv(f$datapath),
      error = function(e) list(
        ok = FALSE,
        required = msterp_metabobase_required_csv_cols(),
        present = character(0),
        missing = msterp_metabobase_required_csv_cols()
      )
    )
  })

  # Clear state when new file selected
  observeEvent(input$tools_metabobase_csv_file, {
    rv_metabobase$metabo <- NULL
    msg_push("Selected a CSV file.")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_metabobase_file, {
    rv_metabobase$metabo <- NULL
    msg_push("Selected a .metabobase file.")
  }, ignoreInit = TRUE)

  # Reset button
  observeEvent(input$tools_metabobase_reset, {
    rv_metabobase$metabo <- NULL
    rv_metabobase$messages <- character(0)
    rv_metabobase$status_msg <- NULL
    rv_metabobase$status_level <- NULL
  }, ignoreInit = TRUE)

  # Build from CSV
  observeEvent(input$tools_metabobase_build_csv, {
    req(input$tools_metabobase_csv_file)

    chk <- csv_header_check()
    if (is.null(chk) || !isTRUE(chk$ok)) {
      rv_metabobase$metabo <- NULL
      msg_push("Build blocked: missing required columns.")
      showNotification("Missing required columns: metabolite_id, name", type = "error")
      return()
    }

    rv_metabobase$metabo <- NULL
    msg_push("Starting CSV build.")

    t0 <- Sys.time()

    # Progress tracking
    prog <- 0
    progress_set <- function(value, detail = NULL) {
      prog <<- max(0, min(1, as.numeric(value)))
      set_busy(TRUE, message = detail %||% "Working...", percent = prog * 100)
    }
    progress_inc <- function(amount, detail = NULL) {
      prog <<- max(0, min(1, prog + as.numeric(amount)))
      set_busy(TRUE, message = detail %||% "Working...", percent = prog * 100)
    }

    set_busy(TRUE, "Starting...", 0)
    on.exit(set_busy(FALSE, "", NULL), add = TRUE)

    tryCatch({
      metabo <- metabobase_build_from_csv(
        path = input$tools_metabobase_csv_file$datapath,
        library_name = input$tools_metabobase_csv_library_name,
        progress_inc = progress_inc,
        progress_set = progress_set
      )

      v <- metabobase_validate(metabo)
      if (!v$ok) stop(paste(v$errors, collapse = "\n"))

      rv_metabobase$metabo <- metabo
      app_state$metabobase <- metabo

      dt_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      msg_push(paste0("Build complete. Time: ", format_hms(dt_sec)))
      showNotification("MetaboBase built successfully.", type = "message")

    }, error = function(e) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("Build failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Build from KEGG
  observeEvent(input$tools_metabobase_build_kegg, {
    organism <- input$tools_metabobase_kegg_organism %||% "hsa"
    library_name <- input$tools_metabobase_kegg_library_name

    rv_metabobase$metabo <- NULL
    msg_push(paste0("Starting KEGG build for organism: ", organism))

    t0 <- Sys.time()

    set_busy(TRUE, "Connecting to KEGG...", 0)
    on.exit(set_busy(FALSE, "", NULL), add = TRUE)

    # Progress callback for KEGG build
    progress_callback <- function(message) {
      msg_push(message)
      set_busy(TRUE, message, NULL)
    }

    tryCatch({
      metabo <- metabobase_build_kegg_library(
        organism = organism,
        library_name = if (nzchar(library_name %||% "")) library_name else NULL,
        output_file = NULL,
        progress_callback = progress_callback
      )

      v <- metabobase_validate(metabo)
      if (!v$ok) stop(paste(v$errors, collapse = "\n"))

      rv_metabobase$metabo <- metabo
      app_state$metabobase <- metabo

      dt_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      msg_push(paste0("KEGG build complete. Time: ", format_hms(dt_sec)))
      showNotification("MetaboBase built from KEGG.", type = "message")

    }, error = function(e) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("KEGG build failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Build from HMDB
  observeEvent(input$tools_metabobase_build_hmdb, {
    req(input$tools_metabobase_hmdb_xml_file)

    xml_file <- input$tools_metabobase_hmdb_xml_file
    library_name <- input$tools_metabobase_hmdb_library_name

    rv_metabobase$metabo <- NULL
    msg_push(paste0("Starting HMDB build from: ", xml_file$name))

    t0 <- Sys.time()

    set_busy(TRUE, "Parsing HMDB XML...", 0)
    on.exit(set_busy(FALSE, "", NULL), add = TRUE)

    progress_callback <- function(message) {
      msg_push(message)
      set_busy(TRUE, message, NULL)
    }

    tryCatch({
      metabo <- metabobase_build_hmdb_library(
        xml_path = xml_file$datapath,
        library_name = if (nzchar(library_name %||% "")) library_name else NULL,
        output_file = NULL,
        progress_callback = progress_callback
      )

      v <- metabobase_validate(metabo)
      if (!v$ok) stop(paste(v$errors, collapse = "\n"))

      rv_metabobase$metabo <- metabo
      app_state$metabobase <- metabo

      dt_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      msg_push(paste0("HMDB build complete. Time: ", format_hms(dt_sec)))
      showNotification("MetaboBase built from HMDB.", type = "message")

    }, error = function(e) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("HMDB build failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Validate existing MetaboBase
  observeEvent(input$tools_metabobase_validate, {
    req(input$tools_metabobase_file)

    rv_metabobase$metabo <- NULL
    msg_push("Validating .metabobase file.")

    tryCatch({
      metabo <- metabobase_load(input$tools_metabobase_file$datapath)
      rv_metabobase$metabo <- metabo
      app_state$metabobase <- metabo

      msg_push("Validation complete.")
      showNotification("MetaboBase validated and loaded.", type = "message")

    }, error = function(e) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("Validation failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Status UI
  output$tools_metabobase_status <- renderUI({
    mode <- input$tools_metabobase_mode %||% "build_csv"

    if (identical(mode, "build_csv")) {
      chk <- csv_header_check()
      required <- msterp_metabobase_required_csv_cols()

      if (is.null(chk)) {
        return(tagList(
          tags$h4("Status"),
          tags$p(tags$strong("Upload a metabolite CSV file."))
        ))
      }

      ok_set <- chk$present
      # Case-insensitive matching
      ok_set_lower <- tolower(ok_set)

      items <- lapply(required, function(col) {
        has <- tolower(col) %in% ok_set_lower
        tags$div(
          if (has) icon_ok() else icon_bad(),
          tags$span(col),
          style = "margin-bottom:6px;"
        )
      })

      return(tagList(
        tags$h4("Column Validation"),
        tags$div(items),
        tags$hr(),
        if (isTRUE(chk$ok)) {
          tags$div(icon_ok(), tags$strong("CSV columns: OK"))
        } else {
          tagList(
            tags$div(icon_bad(), tags$strong("CSV columns: MISSING REQUIRED")),
            tags$p(class = "text-muted", "Required: metabolite_id, name")
          )
        }
      ))
    }

    if (identical(mode, "build_kegg")) {
      organism <- input$tools_metabobase_kegg_organism %||% "hsa"
      return(tagList(
        tags$h4("Status"),
        tags$p("Ready to build KEGG library for: ", tags$strong(organism)),
        tags$p(class = "text-muted", "Click 'Build from KEGG' to start.")
      ))
    }

    if (identical(mode, "build_hmdb")) {
      f <- input$tools_metabobase_hmdb_xml_file
      if (is.null(f)) {
        return(tagList(
          tags$h4("Status"),
          tags$p(tags$strong("Select an HMDB XML file (hmdb_metabolites.xml)."))
        ))
      }
      return(tagList(
        tags$h4("Status"),
        tags$p("Ready to build from HMDB XML: ", tags$strong(f$name)),
        tags$p(class = "text-muted", "Click 'Build from HMDB' to start. This may take 10-30 minutes.")
      ))
    }

    # Validate mode
    f <- input$tools_metabobase_file
    if (is.null(f)) {
      return(tagList(
        tags$h4("Status"),
        tags$p(tags$strong("Select a .metabobase file and click Validate."))
      ))
    }

    if (is.null(rv_metabobase$metabo)) {
      return(tagList(
        tags$h4("Status"),
        icon_bad(), tags$strong("Not validated yet.")
      ))
    }

    v <- metabobase_validate(rv_metabobase$metabo)
    tagList(
      tags$h4("Status"),
      if (v$ok) {
        tags$div(icon_ok(), tags$strong("MetaboBase: Valid"))
      } else {
        tagList(
          tags$div(icon_bad(), tags$strong("MetaboBase: INVALID")),
          tags$ul(lapply(v$errors, tags$li))
        )
      }
    )
  })

  # Summary UI
  output$tools_metabobase_summary <- renderUI({
    metabo <- rv_metabobase$metabo
    if (is.null(metabo)) {
      return(tags$div(class = "text-muted", "No MetaboBase loaded."))
    }

    lines <- metabobase_summary_lines(metabo)
    tagList(
      tags$h4("Library Summary"),
      tags$pre(paste(lines, collapse = "\n"))
    )
  })

  # Messages
  output$tools_metabobase_messages <- renderText({
    if (!length(rv_metabobase$messages)) return(" ")
    paste(rv_metabobase$messages, collapse = "\n")
  })

  # Download button UI
  output$tools_metabobase_download_ui <- renderUI({
    if (is.null(rv_metabobase$metabo)) return(NULL)
    downloadButton("tools_metabobase_download", "Download .metabobase", class = "btn-success btn-tool-action")
  })

  # Download handler
  output$tools_metabobase_download <- downloadHandler(
    filename = function() {
      metabo <- rv_metabobase$metabo
      mode <- input$tools_metabobase_mode %||% "build_csv"

      base <- if (mode == "build_csv") {
        input$tools_metabobase_csv_library_name
      } else if (mode == "build_kegg") {
        input$tools_metabobase_kegg_library_name
      } else if (mode == "build_hmdb") {
        input$tools_metabobase_hmdb_library_name
      } else {
        ""
      }

      if (is.null(base) || !nzchar(base)) {
        base <- if (!is.null(metabo) && nzchar(metabo$library_name %||% "")) metabo$library_name else "metabobase"
      }
      paste0(base, ".metabobase")
    },
    content = function(file) {
      metabo <- isolate(rv_metabobase$metabo)
      if (is.null(metabo)) stop("No library built/validated yet.")
      metabobase_save(metabo, file = file)
    },
    contentType = "application/octet-stream"
  )
}
