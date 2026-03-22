# R/pages/tools/tool_complexbase.R
# ComplexBase Builder Tool - Build and validate ComplexBase files from CORUM/ComplexPortal

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# ============================================================
# ComplexBase Tool Defaults
# ============================================================
tools_complexbase_defaults <- function() {
  list(
    params = list(),
    style = list()
  )
}

# ============================================================
# ComplexBase Tool UI
# ============================================================
tools_complexbase_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_complexbase_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("ComplexBase Builder", style = "margin: 0;")
    ),
    tags$p("Build a ComplexBase annotation database from CORUM or ComplexPortal for protein complex enrichment."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("Mode"),
        radioButtons(
          "tools_complexbase_mode",
          NULL,
          choices = c(
            "Build .complexbase from CORUM TSV" = "build_corum",
            "Build .complexbase from ComplexPortal TSV" = "build_complexportal",
            "Validate existing .complexbase" = "validate"
          ),
          selected = "build_corum"
        ),
        hr(),

        # Build from CORUM inputs
        conditionalPanel(
          condition = "input.tools_complexbase_mode == 'build_corum'",
          tags$h4("CORUM Input"),
          fileInput(
            "tools_complexbase_corum_file",
            "CORUM allComplexes.txt",
            accept = c(".txt", ".tsv")
          ),
          tags$small(
            class = "text-muted d-block mb-2",
            "Download from: ",
            tags$a(href = "https://mips.helmholtz-muenchen.de/corum/#download", target = "_blank", "CORUM Downloads")
          ),
          selectInput(
            "tools_complexbase_corum_organism",
            "Organism",
            choices = c(
              "Human" = "Human",
              "Mouse" = "Mouse",
              "Rat" = "Rat",
              "All organisms" = "all"
            ),
            selected = "Human"
          ),
          textInput("tools_complexbase_library_name", "Library name", value = ""),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_complexbase_build_corum", "Build ComplexBase", class = "btn-primary btn-tool-action"),
            actionButton("tools_complexbase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Build from ComplexPortal inputs
        conditionalPanel(
          condition = "input.tools_complexbase_mode == 'build_complexportal'",
          tags$h4("ComplexPortal Input"),
          fileInput(
            "tools_complexbase_cp_file",
            "ComplexPortal TSV export",
            accept = c(".txt", ".tsv")
          ),
          tags$small(
            class = "text-muted d-block mb-2",
            "Download from: ",
            tags$a(href = "https://www.ebi.ac.uk/complexportal/download", target = "_blank", "ComplexPortal Downloads")
          ),
          textInput("tools_complexbase_library_name_cp", "Library name", value = ""),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_complexbase_build_cp", "Build ComplexBase", class = "btn-primary btn-tool-action"),
            actionButton("tools_complexbase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Validate mode inputs
        conditionalPanel(
          condition = "input.tools_complexbase_mode == 'validate'",
          tags$h4("ComplexBase File"),
          fileInput(
            "tools_complexbase_file",
            "Select .complexbase file",
            accept = c(".complexbase", ".rds")
          ),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_complexbase_validate", "Validate", class = "btn-primary btn-tool-action"),
            actionButton("tools_complexbase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        hr(),
        uiOutput("tools_complexbase_download_ui"),

        hr(),
        tools_collapse_section_ui(
          "tools_complexbase_info_section",
          "About Protein Complex Databases",
          open = FALSE,
          tags$p("ComplexBase files enable protein complex enrichment analysis in MSTerp."),
          tags$h6("CORUM"),
          tags$p(class = "text-muted", style = "font-size: 12px;",
            "CORUM (Comprehensive Resource of Mammalian protein complexes) contains ",
            "manually curated protein complex data from literature."
          ),
          tags$h6("ComplexPortal"),
          tags$p(class = "text-muted", style = "font-size: 12px;",
            "ComplexPortal from EBI provides manually curated macromolecular complex data ",
            "across multiple species."
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_complexbase_status"),
        hr(),
        uiOutput("tools_complexbase_summary"),
        hr(),
        tags$h4("Messages"),
        verbatimTextOutput("tools_complexbase_messages")
      )
    )
  )
}

# ============================================================
# ComplexBase Tool Server
# ============================================================
tools_complexbase_server <- function(input, output, session, app_state, rv_complexbase) {

  # Message helper
  msg_push <- function(x) {
    rv_complexbase$messages <- c(rv_complexbase$messages, paste0(format(Sys.time(), "%H:%M:%S"), "  ", x))
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

  # Reset handler
  observeEvent(input$tools_complexbase_reset, {
    rv_complexbase$result <- NULL
    rv_complexbase$messages <- character()
    rv_complexbase$status <- NULL
  })

  # Back button handler
  observeEvent(input$tools_complexbase_back, {
    rv_complexbase$result <- NULL
    rv_complexbase$messages <- character()
    rv_complexbase$status <- NULL
    # Signal to parent to switch view
    session$userData$tools_current_tool("landing")
  })

  # Build from CORUM handler
  observeEvent(input$tools_complexbase_build_corum, {
    f <- input$tools_complexbase_corum_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
      showNotification("Please upload a CORUM file first", type = "warning")
      return()
    }

    rv_complexbase$messages <- character()
    rv_complexbase$status <- "building"
    set_busy(TRUE, "Building ComplexBase from CORUM...")
    msg_push("Starting CORUM build...")

    organism_filter <- input$tools_complexbase_corum_organism
    lib_name <- input$tools_complexbase_library_name

    tryCatch({
      result <- complexbase_build_from_corum(
        path = f$datapath,
        organism = if (organism_filter == "all") NULL else organism_filter,
        library_name = lib_name,
        log_fn = msg_push
      )

      if (result$ok) {
        rv_complexbase$result <- result$complexbase
        rv_complexbase$status <- "success"
        msg_push(sprintf("SUCCESS: Built ComplexBase with %d complexes, %d proteins",
                         nrow(result$complexbase$complexes),
                         length(unique(result$complexbase$protein_complex$protein_id))))
      } else {
        rv_complexbase$status <- "error"
        msg_push(paste("ERROR:", result$error))
      }
    }, error = function(e) {
      rv_complexbase$status <- "error"
      msg_push(paste("ERROR:", e$message))
    })

    set_busy(FALSE)
  })

  # Build from ComplexPortal handler
  observeEvent(input$tools_complexbase_build_cp, {
    f <- input$tools_complexbase_cp_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
      showNotification("Please upload a ComplexPortal file first", type = "warning")
      return()
    }

    rv_complexbase$messages <- character()
    rv_complexbase$status <- "building"
    set_busy(TRUE, "Building ComplexBase from ComplexPortal...")
    msg_push("Starting ComplexPortal build...")

    lib_name <- input$tools_complexbase_library_name_cp

    tryCatch({
      result <- complexbase_build_from_complexportal(
        path = f$datapath,
        library_name = lib_name,
        log_fn = msg_push
      )

      if (result$ok) {
        rv_complexbase$result <- result$complexbase
        rv_complexbase$status <- "success"
        msg_push(sprintf("SUCCESS: Built ComplexBase with %d complexes, %d proteins",
                         nrow(result$complexbase$complexes),
                         length(unique(result$complexbase$protein_complex$protein_id))))
      } else {
        rv_complexbase$status <- "error"
        msg_push(paste("ERROR:", result$error))
      }
    }, error = function(e) {
      rv_complexbase$status <- "error"
      msg_push(paste("ERROR:", e$message))
    })

    set_busy(FALSE)
  })

  # Validate handler
  observeEvent(input$tools_complexbase_validate, {
    f <- input$tools_complexbase_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
      showNotification("Please upload a ComplexBase file first", type = "warning")
      return()
    }

    rv_complexbase$messages <- character()
    rv_complexbase$status <- "validating"
    set_busy(TRUE, "Validating ComplexBase...")
    msg_push("Starting validation...")

    tryCatch({
      cb <- readRDS(f$datapath)
      validation <- complexbase_validate(cb)

      if (validation$ok) {
        rv_complexbase$result <- cb
        rv_complexbase$status <- "success"
        msg_push("Validation PASSED")
        msg_push(sprintf("  Schema version: %d", cb$schema_version %||% NA))
        msg_push(sprintf("  Organism: %s", cb$organism %||% "Unknown"))
        msg_push(sprintf("  Source: %s", cb$source %||% "Unknown"))
        msg_push(sprintf("  Complexes: %d", nrow(cb$complexes %||% data.frame())))
        msg_push(sprintf("  Proteins: %d", length(unique(cb$protein_complex$protein_id %||% character()))))
      } else {
        rv_complexbase$status <- "error"
        msg_push("Validation FAILED:")
        for (err in validation$errors) {
          msg_push(paste("  -", err))
        }
      }
    }, error = function(e) {
      rv_complexbase$status <- "error"
      msg_push(paste("ERROR reading file:", e$message))
    })

    set_busy(FALSE)
  })

  # Status output
  output$tools_complexbase_status <- renderUI({
    status <- rv_complexbase$status
    if (is.null(status)) {
      return(tags$div(class = "text-muted", "No operation in progress"))
    }

    switch(status,
      "building" = tags$div(class = "text-info", shiny::icon("spinner", class = "fa-spin"), " Building..."),
      "validating" = tags$div(class = "text-info", shiny::icon("spinner", class = "fa-spin"), " Validating..."),
      "success" = tags$div(icon_ok(), "Operation completed successfully"),
      "error" = tags$div(icon_bad(), "Operation failed - see messages below"),
      tags$div(class = "text-muted", "Unknown status")
    )
  })

  # Summary output
  output$tools_complexbase_summary <- renderUI({
    cb <- rv_complexbase$result
    if (is.null(cb)) {
      return(tags$div(class = "text-muted", "No ComplexBase loaded"))
    }

    n_complexes <- nrow(cb$complexes %||% data.frame())
    n_proteins <- length(unique(cb$protein_complex$protein_id %||% character()))
    n_mappings <- nrow(cb$protein_complex %||% data.frame())

    # Source breakdown
    source_counts <- if (!is.null(cb$complexes) && "source" %in% names(cb$complexes)) {
      table(cb$complexes$source)
    } else {
      NULL
    }

    tagList(
      tags$h4("Summary"),
      tags$table(
        class = "table table-sm",
        tags$tr(tags$td("Organism:"), tags$td(cb$organism %||% "Unknown")),
        tags$tr(tags$td("Source:"), tags$td(cb$source %||% "Unknown")),
        tags$tr(tags$td("Complexes:"), tags$td(format(n_complexes, big.mark = ","))),
        tags$tr(tags$td("Unique proteins:"), tags$td(format(n_proteins, big.mark = ","))),
        tags$tr(tags$td("Protein-complex mappings:"), tags$td(format(n_mappings, big.mark = ",")))
      ),
      if (!is.null(source_counts) && length(source_counts) > 1) {
        tagList(
          tags$h5("By Source"),
          tags$ul(
            lapply(names(source_counts), function(src) {
              tags$li(sprintf("%s: %d complexes", src, source_counts[[src]]))
            })
          )
        )
      }
    )
  })

  # Messages output
  output$tools_complexbase_messages <- renderText({
    paste(rv_complexbase$messages, collapse = "\n")
  })

  # Download UI
  output$tools_complexbase_download_ui <- renderUI({
    cb <- rv_complexbase$result
    if (is.null(cb)) return(NULL)

    lib_name <- cb$library_name %||% cb$organism %||% "complexbase"
    default_filename <- paste0(lib_name, ".complexbase")

    tagList(
      tags$h4("Download"),
      downloadButton("tools_complexbase_download", "Download .complexbase", class = "btn btn-success")
    )
  })

  # Download handler
  output$tools_complexbase_download <- downloadHandler(
    filename = function() {
      cb <- rv_complexbase$result
      lib_name <- cb$library_name %||% cb$organism %||% "complexbase"
      paste0(gsub("[^a-zA-Z0-9_-]", "_", lib_name), ".complexbase")
    },
    content = function(file) {
      saveRDS(rv_complexbase$result, file)
    }
  )
}

# ============================================================
# Build Functions
# ============================================================

#' Build ComplexBase from CORUM file
#'
#' @param path Path to CORUM allComplexes.txt file
#' @param organism Filter by organism (NULL for all)
#' @param library_name Optional library name
#' @param log_fn Logging function
#' @return list(ok, complexbase or error)
complexbase_build_from_corum <- function(path, organism = NULL, library_name = NULL, log_fn = message) {

  log_fn("Reading CORUM file...")

  # Read CORUM TSV (tab-separated)
  raw <- tryCatch(
    read.delim(path, stringsAsFactors = FALSE, quote = "", comment.char = ""),
    error = function(e) NULL
  )

  if (is.null(raw) || nrow(raw) == 0) {
    return(list(ok = FALSE, error = "Failed to read CORUM file or file is empty"))
  }

  log_fn(sprintf("Read %d rows from CORUM file", nrow(raw)))

  # Find columns flexibly (CORUM format varies across versions)
  id_col <- intersect(names(raw), c("ComplexID", "Complex.id", "complex_id"))[1]
  name_col <- intersect(names(raw), c("ComplexName", "Complex.name", "complex_name"))[1]
  organism_col <- intersect(names(raw), c("Organism", "organism"))[1]
  subunits_col <- intersect(names(raw), c(
    "subunits.UniProt.IDs.", "subunits(UniProt IDs)", "subunits_uniprot_id",
    "subunits_uniprot_ids", "Subunits.UniProt.IDs."))[1]
  gene_col <- intersect(names(raw), c(
    "subunits.Gene.name.", "subunits(Gene name)", "subunits_gene_name",
    "subunits_gene_names", "Subunits.Gene.name."))[1]

  if (is.na(id_col) || is.na(name_col) || is.na(subunits_col)) {
    return(list(ok = FALSE, error = paste0(
      "Could not find required CORUM columns. Available: ",
      paste(names(raw), collapse = ", "))))
  }

  log_fn(sprintf("Detected columns: id=%s, name=%s, subunits=%s, gene=%s",
                 id_col, name_col, subunits_col,
                 if (is.na(gene_col)) "none" else gene_col))

  # Filter by organism if specified
  if (!is.null(organism) && organism != "all" && !is.na(organism_col)) {
    raw <- raw[raw[[organism_col]] == organism, , drop = FALSE]
    log_fn(sprintf("Filtered to %d rows for organism: %s", nrow(raw), organism))
  }

  if (nrow(raw) == 0) {
    return(list(ok = FALSE, error = "No complexes found after filtering"))
  }

  # Build complexes data.frame
  log_fn("Building complexes table...")
  complexes <- data.frame(
    complex_id = paste0("CORUM_", raw[[id_col]]),
    complex_name = raw[[name_col]],
    source = "CORUM",
    n_subunits = NA_integer_,
    stringsAsFactors = FALSE
  )

  # Build protein_complex long table with gene_symbol
  log_fn("Building protein-complex mappings...")
  has_genes <- !is.na(gene_col)
  protein_complex_list <- lapply(seq_len(nrow(raw)), function(i) {
    complex_id <- paste0("CORUM_", raw[[id_col]][i])
    subunits_str <- raw[[subunits_col]][i]

    if (is.na(subunits_str) || !nzchar(subunits_str)) return(NULL)

    proteins <- trimws(strsplit(subunits_str, "[;,]")[[1]])
    proteins <- proteins[nzchar(proteins)]
    if (length(proteins) == 0) return(NULL)

    # Extract gene symbols if available (positionally aligned with UniProt IDs)
    gene_symbols <- if (has_genes) {
      gs_str <- raw[[gene_col]][i]
      if (!is.na(gs_str) && nzchar(gs_str)) {
        gs <- trimws(strsplit(gs_str, "[;,]")[[1]])
        if (length(gs) == length(proteins)) gs else rep(NA_character_, length(proteins))
      } else {
        rep(NA_character_, length(proteins))
      }
    } else {
      rep(NA_character_, length(proteins))
    }

    data.frame(
      protein_id = proteins,
      gene_symbol = gene_symbols,
      complex_id = complex_id,
      stringsAsFactors = FALSE
    )
  })

  protein_complex <- do.call(rbind, Filter(Negate(is.null), protein_complex_list))

  if (is.null(protein_complex) || nrow(protein_complex) == 0) {
    return(list(ok = FALSE, error = "No protein-complex mappings could be extracted"))
  }

  log_fn(sprintf("Found %d protein-complex mappings", nrow(protein_complex)))

  # Update n_subunits
  subunit_counts <- table(protein_complex$complex_id)
  complexes$n_subunits <- as.integer(subunit_counts[complexes$complex_id])

  # Build complex_proteins lookup list
  log_fn("Building fast lookup structures...")
  complex_proteins <- split(protein_complex$protein_id, protein_complex$complex_id)
  complex_proteins <- lapply(complex_proteins, unique)

  # Determine organism name
  org_name <- if (!is.null(organism) && organism != "all") {
    organism
  } else if ("Organism" %in% names(raw)) {
    orgs <- unique(raw$Organism)
    if (length(orgs) == 1) orgs else "Multiple"
  } else {
    "Unknown"
  }

  # Build final ComplexBase object
  cb <- list(
    schema_version = 1L,
    organism = org_name,
    source = "CORUM",
    library_name = if (nzchar(library_name %||% "")) library_name else NULL,
    created = Sys.time(),
    complexes = complexes,
    protein_complex = protein_complex,
    complex_proteins = complex_proteins
  )

  log_fn("ComplexBase build complete")

  list(ok = TRUE, complexbase = cb)
}

#' Build ComplexBase from ComplexPortal file
#'
#' @param path Path to ComplexPortal TSV file
#' @param library_name Optional library name
#' @param log_fn Logging function
#' @return list(ok, complexbase or error)
complexbase_build_from_complexportal <- function(path, library_name = NULL, log_fn = message) {

  log_fn("Reading ComplexPortal file...")

  # Read ComplexPortal TSV
  raw <- tryCatch(
    read.delim(path, stringsAsFactors = FALSE, quote = "", comment.char = ""),
    error = function(e) NULL
  )

  if (is.null(raw) || nrow(raw) == 0) {
    return(list(ok = FALSE, error = "Failed to read ComplexPortal file or file is empty"))
  }

  log_fn(sprintf("Read %d rows from ComplexPortal file", nrow(raw)))

  # ComplexPortal format varies, but typically has:
  # Complex ac, Recommended name, Identifiers (xrefs) including UniProt
  # Look for common column patterns
  id_col <- NULL
  name_col <- NULL
  components_col <- NULL

  for (candidate in c("Complex.ac", "Complex ac", "Accession", "ID")) {
    if (candidate %in% names(raw)) {
      id_col <- candidate
      break
    }
  }

  for (candidate in c("Recommended.name", "Recommended name", "Name", "Complex.name")) {
    if (candidate %in% names(raw)) {
      name_col <- candidate
      break
    }
  }

  for (candidate in c("Identifiers..Interactor.", "Identifiers (Interactor)", "Components", "Expanded.participant.list")) {
    if (candidate %in% names(raw)) {
      components_col <- candidate
      break
    }
  }

  if (is.null(id_col)) {
    return(list(ok = FALSE, error = "Could not find complex ID column in ComplexPortal file"))
  }
  if (is.null(name_col)) {
    return(list(ok = FALSE, error = "Could not find complex name column in ComplexPortal file"))
  }
  if (is.null(components_col)) {
    return(list(ok = FALSE, error = "Could not find components/identifiers column in ComplexPortal file"))
  }

  log_fn(sprintf("Using columns: ID=%s, Name=%s, Components=%s", id_col, name_col, components_col))

  # Build complexes data.frame
  log_fn("Building complexes table...")
  complexes <- data.frame(
    complex_id = paste0("CPX_", raw[[id_col]]),
    complex_name = raw[[name_col]],
    source = "ComplexPortal",
    n_subunits = NA_integer_,
    category = NA_character_,
    stringsAsFactors = FALSE
  )

  # Extract UniProt IDs from components column
  log_fn("Building protein-complex mappings...")

  # UniProt ID pattern
  uniprot_pattern <- "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9]"

  protein_complex_list <- lapply(seq_len(nrow(raw)), function(i) {
    complex_id <- paste0("CPX_", raw[[id_col]][i])
    components_str <- raw[[components_col]][i]

    if (is.na(components_str) || !nzchar(components_str)) {
      return(NULL)
    }

    # Extract UniProt IDs using regex
    proteins <- regmatches(components_str, gregexpr(uniprot_pattern, components_str))[[1]]
    proteins <- unique(proteins)

    if (length(proteins) == 0) {
      return(NULL)
    }

    data.frame(
      protein_id = proteins,
      gene_symbol = NA_character_,
      complex_id = complex_id,
      stringsAsFactors = FALSE
    )
  })

  protein_complex <- do.call(rbind, Filter(Negate(is.null), protein_complex_list))

  if (is.null(protein_complex) || nrow(protein_complex) == 0) {
    return(list(ok = FALSE, error = "No protein-complex mappings could be extracted"))
  }

  log_fn(sprintf("Found %d protein-complex mappings", nrow(protein_complex)))

  # Update n_subunits
  subunit_counts <- table(protein_complex$complex_id)
  complexes$n_subunits <- as.integer(subunit_counts[complexes$complex_id])

  # Build complex_proteins lookup list
  log_fn("Building fast lookup structures...")
  complex_proteins <- split(protein_complex$protein_id, protein_complex$complex_id)
  complex_proteins <- lapply(complex_proteins, unique)

  # Determine organism from file (if available)
  org_name <- if ("Organism" %in% names(raw)) {
    orgs <- unique(raw$Organism)
    if (length(orgs) == 1) orgs else "Multiple"
  } else {
    "Unknown"
  }

  # Build final ComplexBase object
  cb <- list(
    schema_version = 1L,
    organism = org_name,
    source = "ComplexPortal",
    library_name = if (nzchar(library_name %||% "")) library_name else NULL,
    created = Sys.time(),
    complexes = complexes,
    protein_complex = protein_complex,
    complex_proteins = complex_proteins
  )

  log_fn("ComplexBase build complete")

  list(ok = TRUE, complexbase = cb)
}
