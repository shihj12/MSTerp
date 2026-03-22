# R/pages/page_database.R
library(shiny)

page_database_ui <- function() {
  msterp_page(
    title = "TerpBase",
    tags$p(""),
    two_panel_ui(
      left_ui = tagList(
        radioButtons(
          "db_mode",
          "Mode",
          choices = c(
            "Build .terpbase from UniProt Excel output" = "build",
            "Validate existing .terpbase" = "validate"
          ),
          selected = "build"
        ),
        
        conditionalPanel(
          condition = "input.db_mode == 'build'",
          fileInput(
            "db_uniprot_file",
            "UniProt Excel (.xlsx / .xls)",
            accept = c(".xlsx", ".xls")
          ),
          textInput("db_library_name", "New .terpbase name", value = ""),
          actionButton("db_build_terpbase", "Build .terpbase", class = "btn-primary")
        ),
        
        conditionalPanel(
          condition = "input.db_mode == 'validate'",
          fileInput(
            "db_terpbase_file",
            "Select .terpbase",
            accept = c(".terpbase", ".rds")
          ),
          actionButton("db_validate_terpbase", "Validate", class = "btn-primary")
        ),
        
        hr(),
        uiOutput("db_download_ui")
      ),
      
      right_ui = tagList(
        tags$h3("Status"),
        uiOutput("db_status_ui"),
        hr(),
        tags$h3("Library summary"),
        verbatimTextOutput("db_summary"),
        hr(),
        tags$h3("Database Updates"),
        tags$p("Check for newer versions of bundled databases (TerpBase, MetaboBase, ComplexBase)."),
        actionButton("db_check_updates", "Check for updates", class = "btn-outline-primary btn-sm"),
        uiOutput("db_updates_ui"),
        hr(),
        tags$h3("Messages"),
        verbatimTextOutput("db_messages")
      )
    )
  )
}


page_database_server <- function(input, output, session, app_state) {
  
  rv <- reactiveValues(
    terp = NULL,
    messages = character(0)
  )
  
  msg_push <- function(x) {
    rv$messages <- c(rv$messages, paste0(format(Sys.time(), "%H:%M:%S"), "  ", x))
  }
  
  format_hms <- function(seconds) {
    s <- max(0L, as.integer(round(seconds)))
    h <- s %/% 3600L
    m <- (s %% 3600L) %/% 60L
    sec <- s %% 60L
    sprintf("%02dh %02dm %02ds", h, m, sec)
  }
  
  set_busy <- function(active, message = "", percent = NULL) {
    # Requires your global helper from ui_notifications.R
    if (exists("msterp_set_busy", mode = "function", inherits = TRUE)) {
      msterp_set_busy(session, active = active, message = message, percent = percent)
    }
  }
  
  # -------- Status rendering helpers --------
  icon_ok  <- function() tags$span(style = "color:var(--status-success-text);font-weight:700;margin-right:8px;", HTML("&#10003;"))
  icon_bad <- function() tags$span(style = "color:var(--status-error-text);font-weight:700;margin-right:8px;", HTML("&#10007;"))
  
  # UniProt header check (fast; header-only read)
  uniprot_header_check <- reactive({
    f <- input$db_uniprot_file
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
  
  # -------- (new) Clear summary when user selects a new file --------
  observeEvent(input$db_uniprot_file, {
    rv$terp <- NULL
    msg_push("Selected a UniProt output file.")
  }, ignoreInit = TRUE)
  
  observeEvent(input$db_terpbase_file, {
    rv$terp <- NULL
    msg_push("Selected a .terpbase file.")
  }, ignoreInit = TRUE)
  
  # -------- Build .terpbase --------
  observeEvent(input$db_build_terpbase, {
    req(input$db_uniprot_file)
    
    chk <- uniprot_header_check()
    if (is.null(chk) || !isTRUE(chk$ok)) {
      rv$terp <- NULL
      msg_push("Build blocked: missing required columns (exact header names required).")
      return()
    }
    
    rv$terp <- NULL
    msg_push("Starting build.")
    
    t0 <- Sys.time()
    
    # Accurate percent is driven by engine progress_set/inc (0..1) -> overlay (0..100)
    prog <- 0
    progress_set <- function(value, detail = NULL) {
      prog <<- max(0, min(1, as.numeric(value)))
      set_busy(TRUE, message = detail %||% "Working…", percent = prog * 100)
    }
    progress_inc <- function(amount, detail = NULL) {
      prog <<- max(0, min(1, prog + as.numeric(amount)))
      set_busy(TRUE, message = detail %||% "Working…", percent = prog * 100)
    }
    
    set_busy(TRUE, "Starting…", 0)
    on.exit(set_busy(FALSE, "", NULL), add = TRUE)
    
    tryCatch({
      terp <- terpbase_build_from_uniprot(
        path = input$db_uniprot_file$datapath,
        library_name = input$db_library_name,
        progress_inc = progress_inc,
        progress_set = progress_set
      )
      
      v <- terpbase_validate(terp)
      if (!v$ok) stop(paste(v$errors, collapse = "\n"))
      
      rv$terp <- terp
      app_state$terpbase <- terp
      
      dt_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      msg_push(paste0("Build complete. Time: ", format_hms(dt_sec)))
      showNotification("Terpbase built successfully.", type = "message")
      
    }, error = function(e) {
      rv$terp <- NULL
      msg_push(paste0("Build failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)
  
  # -------- Validate existing .terpbase --------
  observeEvent(input$db_validate_terpbase, {
    req(input$db_terpbase_file)
    
    rv$terp <- NULL
    msg_push("Validating .terpbase file.")
    
    tryCatch({
      terp <- terpbase_load(input$db_terpbase_file$datapath)
      rv$terp <- terp
      app_state$terpbase <- terp
      
      msg_push("Validation complete.")
      
    }, error = function(e) {
      rv$terp <- NULL
      msg_push(paste0("Validation failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)
  
  # -------- Status UI --------
  output$db_status_ui <- renderUI({
    mode <- input$db_mode %||% "build"
    
    if (identical(mode, "build")) {
      chk <- uniprot_header_check()
      required <- msterp_terpbase_required_uniprot_cols()
      
      if (is.null(chk)) {
        return(tagList(
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
        tags$div(items),
        tags$hr(),
        if (isTRUE(chk$ok)) {
          tags$div(icon_ok(), tags$strong("UniProt Excel columns: OK"))
        } else {
          tagList(
            tags$div(icon_bad(), tags$strong("UniProt Excel columns: MISSING REQUIRED HEADERS")),
            tags$p("Headers must match UniProt export EXACTLY.")
          )
        }
      ))
      
    }
    
    # mode == validate
    f <- input$db_terpbase_file
    if (is.null(f)) {
      return(tagList(
        tags$p(tags$strong("Select a .terpbase file and click Validate."))
      ))
    }
    
    if (is.null(rv$terp)) {
      return(tagList(
        icon_bad(), tags$strong("Not validated yet.")
      ))
    }
    
    v <- terpbase_validate(rv$terp)
    if (v$ok) {
      tags$div(icon_ok(), tags$strong("Terpbase library: OK"))
    } else {
      tagList(
        tags$div(icon_bad(), tags$strong("Terpbase library: INVALID")),
        tags$ul(lapply(v$errors, tags$li))
      )
    }
  })
  
  # -------- Summary --------
  output$db_summary <- renderText({
    terp <- rv$terp
    if (is.null(terp)) {
      return(" ")
    }
    paste(terpbase_summary_lines(terp), collapse = "\n")
  })
  
  # -------- Messages --------
  output$db_messages <- renderText({
    if (!length(rv$messages)) return(" ")
    paste(rv$messages, collapse = "\n")
  })

  output$db_download_ui <- renderUI({
    if (is.null(rv$terp)) return(NULL)
    downloadButton("db_download_terpbase", "Download .terpbase", class = "btn-success")
  })
  
  # -------- Download --------
  output$db_download_terpbase <- downloadHandler(
    filename = function() {
      terp <- rv$terp
      base <- input$db_library_name
      if (is.null(base) || !nzchar(base)) {
        base <- if (!is.null(terp) && nzchar(terp$library_name %||% "")) terp$library_name else "terpbase"
      }
      paste0(base, ".terpbase")
    },
    content = function(file) {
      terp <- isolate(rv$terp)
      if (is.null(terp)) stop("No library built/validated yet.")
      terpbase_save(terp, file = file)
    },
    contentType = "application/octet-stream"
  )

  # -------- Database updates --------
  db_updates <- reactiveVal(NULL)

  observeEvent(input$db_check_updates, {
    msg_push("Checking for database updates...")
    updates <- tryCatch(
      db_check_updates(),
      error = function(e) {
        msg_push(sprintf("Update check failed: %s", e$message))
        NULL
      }
    )
    db_updates(updates)
    if (!is.null(updates)) {
      n_avail <- sum(updates$update_available, na.rm = TRUE)
      msg_push(sprintf("Found %d update(s) available.", n_avail))
    }
  })

  output$db_updates_ui <- renderUI({
    updates <- db_updates()
    if (is.null(updates) || nrow(updates) == 0) return(NULL)

    rows <- lapply(seq_len(nrow(updates)), function(i) {
      row <- updates[i, ]
      btn_id <- paste0("db_dl_", row$id)

      status <- if (row$update_available) {
        tags$span(style = "color:var(--status-warning-text);", sprintf("%s -> %s", row$local_version, row$remote_version))
      } else {
        tags$span(style = "color:var(--status-success-text);", row$local_version)
      }

      tags$div(
        style = "display:flex;align-items:center;justify-content:space-between;padding:4px 0;",
        tags$span(tags$strong(row$name), " (", sprintf("%.1f MB", row$size_mb), ")"),
        tags$span(
          status,
          if (row$update_available) {
            actionButton(btn_id, "Download", class = "btn-primary btn-sm", style = "margin-left:8px;")
          }
        )
      )
    })

    tagList(tags$div(style = "margin-top:12px;", rows))
  })

  # Dynamic observers for download buttons
  observe({
    updates <- db_updates()
    if (is.null(updates)) return()

    lapply(seq_len(nrow(updates)), function(i) {
      row <- updates[i, ]
      btn_id <- paste0("db_dl_", row$id)
      observeEvent(input[[btn_id]], {
        msg_push(sprintf("Downloading %s...", row$name))
        set_busy(TRUE, message = sprintf("Downloading %s...", row$name))
        result <- tryCatch(
          db_download_update(
            id = row$id, url = row$url, folder = row$folder,
            file = row$file, version = row$remote_version
          ),
          error = function(e) list(ok = FALSE, message = e$message)
        )
        set_busy(FALSE)
        msg_push(result$message)
        if (result$ok) {
          # Refresh the update list
          db_updates(db_check_updates())
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
}


