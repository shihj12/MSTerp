# R/pages/tools/tool_terpbase.R
# TerpBase Builder Tool - Build and validate TerpBase files from UniProt exports

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# ============================================================
# TerpBase Tool Defaults
# ============================================================
tools_terpbase_defaults <- function() {
  list(
    params = list(),
    style = list()
  )
}

# ============================================================
# TerpBase Tool UI
# ============================================================
tools_terpbase_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_terpbase_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("TerpBase Builder", style = "margin: 0;")
    ),
    tags$p("Build a TerpBase annotation database from UniProt Excel exports for use in GO analyses."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("Mode"),
        radioButtons(
          "tools_terpbase_mode",
          NULL,
          choices = c(
            "Build .terpbase from UniProt Excel output" = "build",
            "Validate existing .terpbase" = "validate"
          ),
          selected = "build"
        ),
        hr(),

        # Build mode inputs
        conditionalPanel(
          condition = "input.tools_terpbase_mode == 'build'",
          tags$h4("UniProt Excel Input"),
          fileInput(
            "tools_terpbase_uniprot_file",
            "UniProt Excel (.xlsx / .xls)",
            accept = c(".xlsx", ".xls")
          ),
          textInput("tools_terpbase_library_name", "Library name", value = ""),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_terpbase_build", "Build TerpBase", class = "btn-primary btn-tool-action"),
            actionButton("tools_terpbase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Validate mode inputs
        conditionalPanel(
          condition = "input.tools_terpbase_mode == 'validate'",
          tags$h4("TerpBase File"),
          fileInput(
            "tools_terpbase_file",
            "Select .terpbase file",
            accept = c(".terpbase", ".rds")
          ),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_terpbase_validate", "Validate", class = "btn-primary btn-tool-action"),
            actionButton("tools_terpbase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        hr(),
        uiOutput("tools_terpbase_download_ui"),

        hr(),
        tools_collapse_section_ui(
          "tools_terpbase_info_section",
          "Required UniProt Columns",
          open = FALSE,
          tags$ul(
            tags$li(tags$code("Entry")),
            tags$li(tags$code("Protein names")),
            tags$li(tags$code("Gene Names"), " or ", tags$code("Gene Names (primary)")),
            tags$li(tags$code("Gene Ontology (biological process)")),
            tags$li(tags$code("Gene Ontology (cellular component)")),
            tags$li(tags$code("Gene Ontology (molecular function)")),
            tags$li(tags$code("Subcellular location [CC]")),
            tags$li(tags$code("Organism"))
          ),
          tags$p(class = "text-muted", style = "font-size: 12px; margin-top: 10px;",
            "Column headers must match UniProt export format exactly. ",
            "If both 'Gene Names' and 'Gene Names (primary)' are present, 'Gene Names (primary)' will be used."
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_terpbase_status"),
        hr(),
        uiOutput("tools_terpbase_summary"),
        hr(),
        tags$h4("Messages"),
        verbatimTextOutput("tools_terpbase_messages")
      )
    )
  )
}

# ============================================================
# TerpBase Tool Server
# ============================================================
tools_terpbase_server <- function(input, output, session, app_state, rv_terpbase) {

  # Message helper
  msg_push <- function(x) {
    rv_terpbase$messages <- c(rv_terpbase$messages, paste0(format(Sys.time(), "%H:%M:%S"), "  ", x))
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

  # UniProt header validation (fast, header-only read)
  uniprot_header_check <- reactive({
    f <- input$tools_terpbase_uniprot_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return(NULL)

    tryCatch(
      terpbase_validate_uniprot_excel(f$datapath),
      error = function(e) list(
        ok = FALSE,
        required = msterp_terpbase_required_uniprot_cols(),
        present = character(0),
        missing = msterp_terpbase_required_uniprot_cols()
      )
    )
  })

  # Clear state when new file selected
  observeEvent(input$tools_terpbase_uniprot_file, {
    rv_terpbase$terp <- NULL
    msg_push("Selected a UniProt output file.")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_terpbase_file, {
    rv_terpbase$terp <- NULL
    msg_push("Selected a .terpbase file.")
  }, ignoreInit = TRUE)

  # Reset button
  observeEvent(input$tools_terpbase_reset, {
    rv_terpbase$terp <- NULL
    rv_terpbase$messages <- character(0)
    rv_terpbase$status_msg <- NULL
    rv_terpbase$status_level <- NULL
  }, ignoreInit = TRUE)

  # Build TerpBase
  observeEvent(input$tools_terpbase_build, {
    req(input$tools_terpbase_uniprot_file)

    chk <- uniprot_header_check()
    if (is.null(chk) || !isTRUE(chk$ok)) {
      rv_terpbase$terp <- NULL
      msg_push("Build blocked: missing required columns (exact header names required).")
      showNotification("Missing required columns. Check UniProt export format.", type = "error")
      return()
    }

    rv_terpbase$terp <- NULL
    msg_push("Starting build.")

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
      terp <- terpbase_build_from_uniprot(
        path = input$tools_terpbase_uniprot_file$datapath,
        library_name = input$tools_terpbase_library_name,
        progress_inc = progress_inc,
        progress_set = progress_set
      )

      v <- terpbase_validate(terp)
      if (!v$ok) stop(paste(v$errors, collapse = "\n"))

      rv_terpbase$terp <- terp
      app_state$terpbase <- terp

      dt_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      msg_push(paste0("Build complete. Time: ", format_hms(dt_sec)))
      showNotification("TerpBase built successfully.", type = "message")

    }, error = function(e) {
      rv_terpbase$terp <- NULL
      msg_push(paste0("Build failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Validate existing TerpBase
  observeEvent(input$tools_terpbase_validate, {
    req(input$tools_terpbase_file)

    rv_terpbase$terp <- NULL
    msg_push("Validating .terpbase file.")

    tryCatch({
      terp <- terpbase_load(input$tools_terpbase_file$datapath)
      rv_terpbase$terp <- terp
      app_state$terpbase <- terp

      msg_push("Validation complete.")
      showNotification("TerpBase validated and loaded.", type = "message")

    }, error = function(e) {
      rv_terpbase$terp <- NULL
      msg_push(paste0("Validation failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Status UI
  output$tools_terpbase_status <- renderUI({
    mode <- input$tools_terpbase_mode %||% "build"

    if (identical(mode, "build")) {
      chk <- uniprot_header_check()
      required <- msterp_terpbase_required_uniprot_cols()

      if (is.null(chk)) {
        return(tagList(
          tags$h4("Status"),
          tags$p(tags$strong("Upload a UniProt output Excel file."))
        ))
      }

      ok_set <- chk$present
      items <- lapply(required, function(col) {
        has <- col %in% ok_set
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
          tags$div(icon_ok(), tags$strong("UniProt Excel columns: OK"))
        } else {
          tagList(
            tags$div(icon_bad(), tags$strong("UniProt Excel columns: MISSING REQUIRED HEADERS")),
            tags$p(class = "text-muted", "Headers must match UniProt export EXACTLY.")
          )
        }
      ))
    }

    # Validate mode
    f <- input$tools_terpbase_file
    if (is.null(f)) {
      return(tagList(
        tags$h4("Status"),
        tags$p(tags$strong("Select a .terpbase file and click Validate."))
      ))
    }

    if (is.null(rv_terpbase$terp)) {
      return(tagList(
        tags$h4("Status"),
        icon_bad(), tags$strong("Not validated yet.")
      ))
    }

    v <- terpbase_validate(rv_terpbase$terp)
    tagList(
      tags$h4("Status"),
      if (v$ok) {
        tags$div(icon_ok(), tags$strong("TerpBase: Valid"))
      } else {
        tagList(
          tags$div(icon_bad(), tags$strong("TerpBase: INVALID")),
          tags$ul(lapply(v$errors, tags$li))
        )
      }
    )
  })

  # Summary UI
  output$tools_terpbase_summary <- renderUI({
    terp <- rv_terpbase$terp
    if (is.null(terp)) {
      return(tags$div(class = "text-muted", "No TerpBase loaded."))
    }

    lines <- terpbase_summary_lines(terp)
    tagList(
      tags$h4("Library Summary"),
      tags$pre(paste(lines, collapse = "\n"))
    )
  })

  # Messages
  output$tools_terpbase_messages <- renderText({
    if (!length(rv_terpbase$messages)) return(" ")
    paste(rv_terpbase$messages, collapse = "\n")
  })

  # Download button UI
  output$tools_terpbase_download_ui <- renderUI({
    if (is.null(rv_terpbase$terp)) return(NULL)
    downloadButton("tools_terpbase_download", "Download .terpbase", class = "btn-success btn-tool-action")
  })

  # Download handler
  output$tools_terpbase_download <- downloadHandler(
    filename = function() {
      terp <- rv_terpbase$terp
      base <- input$tools_terpbase_library_name
      if (is.null(base) || !nzchar(base)) {
        base <- if (!is.null(terp) && nzchar(terp$library_name %||% "")) terp$library_name else "terpbase"
      }
      paste0(base, ".terpbase")
    },
    content = function(file) {
      terp <- isolate(rv_terpbase$terp)
      if (is.null(terp)) stop("No library built/validated yet.")
      terpbase_save(terp, file = file)
    },
    contentType = "application/octet-stream"
  )
}
