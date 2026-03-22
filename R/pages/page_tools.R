# R/pages/page_tools.R
# Tools Landing Page - Orchestrates individual tool modules

library(shiny)

# Source individual tool modules
source(file.path("R", "pages", "tools", "tool_goora.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_1dgofcs.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_2dgofcs.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_msea.R"), local = FALSE)
# source(file.path("R", "pages", "tools", "tool_class_enrichment.R"), local = FALSE)  # disabled — no guaranteed database
source(file.path("R", "pages", "tools", "tool_pathway_fcs.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_terpbase.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_id_reconcile.R"), local = FALSE)
# ComplexBase and MetaboBase builders removed from UI — use scripts/ to rebuild
# source(file.path("R", "pages", "tools", "tool_complexbase.R"), local = FALSE)
# source(file.path("R", "pages", "tools", "tool_metabobase.R"), local = FALSE)

# ============================================================
# Shared Utilities
# ============================================================

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

# Helper: get user data database directory for a given folder
.db_user_dir <- function(folder) {
  base <- Sys.getenv("MSTERP_USER_DATA", unset = "")
  if (!nzchar(base)) base <- file.path(Sys.getenv("APPDATA", unset = ""), "MSTerp")
  file.path(base, "databases", folder)
}

tools_default_metabobase_choices <- function() {
  # Check user data folder first, then bundled
  dirs <- c(.db_user_dir("metabobase"), "metabobase")
  all_files <- character(0)
  for (d in dirs) {
    if (dir.exists(d)) {
      found <- list.files(d, pattern = "\\.(metabobase|rds)$", ignore.case = TRUE, full.names = TRUE)
      all_files <- c(all_files, found)
    }
  }
  # Deduplicate by basename (user data version takes priority)
  all_files <- all_files[!duplicated(basename(all_files))]
  if (length(all_files) == 0) return(c("No default metabobase found" = ""))
  all_files <- sort(all_files)
  stats::setNames(all_files, vapply(basename(all_files), .friendly_db_label, character(1)))
}

tools_default_terpbase_choices <- function() {
  dirs <- c(.db_user_dir("terpbase"), "terpbase")
  all_files <- character(0)
  for (d in dirs) {
    if (dir.exists(d)) {
      found <- list.files(d, pattern = "\\.(terpbase|rds)$", ignore.case = TRUE, full.names = TRUE)
      all_files <- c(all_files, found)
    }
  }
  all_files <- all_files[!duplicated(basename(all_files))]
  if (length(all_files) == 0) return(c("No default terpbase found" = ""))
  all_files <- sort(all_files)
  stats::setNames(all_files, vapply(basename(all_files), .friendly_db_label, character(1)))
}

tools_default_complexbase_choices <- function() {
  dirs <- c(.db_user_dir("complexbase"), "complexbase")
  all_files <- character(0)
  for (d in dirs) {
    if (dir.exists(d)) {
      found <- list.files(d, pattern = "\\.(complexbase|rds)$", ignore.case = TRUE, full.names = TRUE)
      all_files <- c(all_files, found)
    }
  }
  all_files <- all_files[!duplicated(basename(all_files))]
  if (length(all_files) == 0) return(c("No default complexbase found" = ""))
  all_files <- sort(all_files)
  stats::setNames(all_files, vapply(basename(all_files), .friendly_db_label, character(1)))
}

# ============================================================
# Tools Landing Page UI
# ============================================================
page_tools_ui <- function() {
  msterp_page(
    title = "Tools",
    tags$p("Standalone ad hoc utilities to complement the core MS Terp workflow."),
    uiOutput("tools_content")
  )
}

# ============================================================
# Tools Landing Page (card grid)
# ============================================================
tools_landing_ui <- function() {
  div(
    class = "grid",
    # GO-ORA card
    div(
      class = "card",
      tags$h3("GO-ORA"),
      tags$p("Gene Ontology over-representation analysis on gene lists."),
      actionButton("tools_open_goora", "Open GO-ORA", class = "btn btn-primary")
    ),
    # 1D GO-FCS card
    div(
      class = "card",
      tags$h3("1D GO-FCS"),
      tags$p("1D functional class scoring on a ranked gene list with scores."),
      actionButton("tools_open_1dgofcs", "Open 1D GO-FCS", class = "btn btn-primary")
    ),
    # 2D GO-FCS card
    div(
      class = "card",
      tags$h3("2D GO-FCS"),
      tags$p("2D functional class scoring on two ranked gene lists (scatter plot)."),
      actionButton("tools_open_2dgofcs", "Open 2D GO-FCS", class = "btn btn-primary")
    ),
    # MSEA card (Metabolite Pathway Enrichment)
    div(
      class = "card",
      tags$h3("MSEA"),
      tags$p("Metabolite Set Enrichment Analysis (pathway over-representation)."),
      actionButton("tools_open_msea", "Open MSEA", class = "btn btn-primary")
    ),
    # Chemical Class Enrichment card — disabled (no guaranteed database)
    # div(
    #   class = "card",
    #   tags$h3("Class Enrichment"),
    #   tags$p("Chemical class enrichment analysis for lipids, amino acids, etc."),
    #   actionButton("tools_open_class_enrichment", "Open Class Enrichment", class = "btn btn-primary")
    # ),
    # 1D Pathway FCS card
    div(
      class = "card",
      tags$h3("1D Pathway FCS"),
      tags$p("1D Pathway functional class scoring on a ranked metabolite list."),
      actionButton("tools_open_pathway_fcs", "Open 1D Pathway FCS", class = "btn btn-primary")
    ),
    # TerpBase Builder card
    div(
      class = "card",
      tags$h3("TerpBase Builder"),
      tags$p("Build and validate TerpBase annotation databases from UniProt exports."),
      actionButton("tools_open_terpbase", "Open TerpBase Builder", class = "btn btn-primary")
    ),
    # Metabolite ID Reconcile card
    div(
      class = "card",
      tags$h3("ID Reconcile"),
      tags$p("Reconcile metabolite names/IDs against a MetaboBase and annotate with cross-references."),
      actionButton("tools_open_id_reconcile", "Open ID Reconcile", class = "btn btn-primary")
    ),
    # QC snapshots card
    div(
      class = "card",
      tags$h3("QC Report"),
      tags$p("QC reports for raw intensity distributions and missingness."),
      tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Coming soon")
    )
  )
}

# ============================================================
# Server
# ============================================================
page_tools_server <- function(input, output, session, app_state) {
  defs_goora <- tools_goora_defaults()
  defs_1dgofcs <- tools_1dgofcs_defaults()
  defs_2dgofcs <- tools_2dgofcs_defaults()
  defs_msea <- tools_msea_defaults()
  # defs_class_enrichment <- tools_class_enrichment_defaults()  # disabled
  defs_pathway_fcs <- tools_pathway_fcs_defaults()
  defs_terpbase <- tools_terpbase_defaults()

  # Track which tool view is active
  current_tool <- reactiveVal("landing")

  # Reactive values for GO-ORA
  rv <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    # Stored parameters to persist across navigation
    stored_params = NULL,
    stored_genes = NULL
  )

  # Reactive values for 1D GO-FCS
  rv_1dgofcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    # Stored parameters to persist across navigation
    stored_params = NULL,
    stored_genes = NULL
  )

  # Reactive values for 2D GO-FCS
  rv_2dgofcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    # Stored parameters to persist across navigation
    stored_params = NULL,
    stored_genes = NULL,
    stored_x_label = NULL,
    stored_y_label = NULL
  )

  # Reactive values for MSEA
  rv_msea <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    hidden_terms = character(),
    term_labels = list(),
    stored_params = NULL,
    stored_metabolites = NULL
  )

  # Reactive values for Class Enrichment — disabled (no guaranteed database)
  # rv_class_enrichment <- reactiveValues(
  #   results = NULL,
  #   rendered = NULL,
  #   status_msg = NULL,
  #   status_level = NULL,
  #   input_count = NULL,
  #   hidden_terms = character(),
  #   term_labels = list(),
  #   stored_params = NULL,
  #   stored_metabolites = NULL
  # )

  # Reactive values for Pathway FCS
  rv_pathway_fcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    hidden_terms = character(),
    term_labels = list(),
    stored_params = NULL,
    stored_metabolites = NULL
  )

  # Reactive values for TerpBase Builder
  rv_terpbase <- reactiveValues(
    terp = NULL,
    messages = character(0),
    status_msg = NULL,
    status_level = NULL
  )

  # Reactive values for ID Reconcile
  rv_reconcile <- reactiveValues(
    raw_data = NULL,
    result_df = NULL,
    status_msg = NULL,
    status_level = NULL,
    running = FALSE,
    bg_process = NULL
  )


  # Shared TerpBase loading function
  tools_load_terpbase <- function(path, rv_target) {
    path <- as.character(path %||% "")
    if (!nzchar(path)) {
      rv_target$status_msg <- "No default TerpBase available."
      rv_target$status_level <- "error"
      return(invisible(FALSE))
    }

    if (!file.exists(path)) {
      rv_target$status_msg <- paste("TerpBase file not found:", path)
      rv_target$status_level <- "error"
      return(invisible(FALSE))
    }

    tryCatch({
      tb <- terpbase_load(path)
      if (!is.null(tb) && is.list(tb)) {
        app_state$terpbase <- tb
        rv_target$status_msg <- "TerpBase loaded successfully."
        rv_target$status_level <- "success"
      } else {
        rv_target$status_msg <- "Invalid TerpBase file format."
        rv_target$status_level <- "error"
      }
    }, error = function(e) {
      rv_target$status_msg <- paste("Error loading TerpBase:", e$message)
      rv_target$status_level <- "error"
    })
  }

  # Render the appropriate content based on current_tool
  output$tools_content <- renderUI({
    tool <- current_tool()
    if (tool == "goora") {
      tools_goora_ui()
    } else if (tool == "1dgofcs") {
      tools_1dgofcs_ui()
    } else if (tool == "2dgofcs") {
      tools_2dgofcs_ui()
    } else if (tool == "msea") {
      tools_msea_ui()
    # } else if (tool == "class_enrichment") {
    #   tools_class_enrichment_ui()
    } else if (tool == "pathway_fcs") {
      tools_pathway_fcs_ui()
    } else if (tool == "terpbase") {
      tools_terpbase_ui()
    } else if (tool == "id_reconcile") {
      tools_id_reconcile_ui()
    } else {
      tools_landing_ui()
    }
  })

  # Navigation between landing and tools
  observeEvent(input$tools_open_goora, {
    current_tool("goora")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_back, {
    # Save current parameters before leaving
    rv$stored_params <- list(
      fdr_cutoff = input$tools_goora_fdr_cutoff,
      min_term_size = input$tools_goora_min_term_size,
      min_overlap = input$tools_goora_min_overlap,
      max_terms = input$tools_goora_max_terms,
      ontology_view = input$tools_goora_ontology_view,
      plot_type = input$tools_goora_plot_type,
      color_mode = input$tools_goora_color_mode,
      fdr_palette = input$tools_goora_fdr_palette,
      flat_color = input$tools_goora_flat_color,
      alpha = input$tools_goora_alpha,
      show_go_id = input$tools_goora_show_go_id,
      flip_axis = input$tools_goora_flip_axis,
      font_size = input$tools_goora_font_size,
      axis_text_size = input$tools_goora_axis_text_size,
      width = input$tools_goora_width,
      height = input$tools_goora_height
    )
    rv$stored_genes <- input$tools_goora_genes
    current_tool("landing")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_open_1dgofcs, {
    current_tool("1dgofcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_back, {
    # Save current parameters before leaving
    rv_1dgofcs$stored_params <- list(
      fdr_cutoff = input$tools_1dgofcs_fdr_cutoff,
      min_term_size = input$tools_1dgofcs_min_term_size,
      max_terms = input$tools_1dgofcs_max_terms,
      ontology_view = input$tools_1dgofcs_ontology_view,
      plot_type = input$tools_1dgofcs_plot_type,
      color_mode = input$tools_1dgofcs_color_mode,
      fdr_palette = input$tools_1dgofcs_fdr_palette,
      flat_color = input$tools_1dgofcs_flat_color,
      alpha = input$tools_1dgofcs_alpha,
      show_go_id = input$tools_1dgofcs_show_go_id,
      flip_axis = input$tools_1dgofcs_flip_axis,
      font_size = input$tools_1dgofcs_font_size,
      axis_text_size = input$tools_1dgofcs_axis_text_size,
      width = input$tools_1dgofcs_width,
      height = input$tools_1dgofcs_height
    )
    rv_1dgofcs$stored_genes <- input$tools_1dgofcs_genes
    current_tool("landing")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_open_2dgofcs, {
    current_tool("2dgofcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_2dgofcs_back, {
    # Save current parameters before leaving
    rv_2dgofcs$stored_params <- list(
      fdr_cutoff = input$tools_2dgofcs_fdr_cutoff,
      min_term_size = input$tools_2dgofcs_min_term_size,
      max_terms = input$tools_2dgofcs_max_terms,
      ontology_view = input$tools_2dgofcs_ontology_view,
      color_mode = input$tools_2dgofcs_color_mode,
      fdr_palette = input$tools_2dgofcs_fdr_palette,
      flat_color = input$tools_2dgofcs_flat_color,
      dot_alpha = input$tools_2dgofcs_dot_alpha,
      show_ref_lines = input$tools_2dgofcs_show_ref_lines,
      show_diagonal_guides = input$tools_2dgofcs_show_diagonal_guides,
      label_font_size = input$tools_2dgofcs_label_font_size,
      axis_text_size = input$tools_2dgofcs_axis_text_size,
      width = input$tools_2dgofcs_width,
      height = input$tools_2dgofcs_height
    )
    rv_2dgofcs$stored_genes <- input$tools_2dgofcs_genes
    rv_2dgofcs$stored_x_label <- input$tools_2dgofcs_x_label
    rv_2dgofcs$stored_y_label <- input$tools_2dgofcs_y_label
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Navigation for MSEA
  observeEvent(input$tools_open_msea, {
    current_tool("msea")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_msea_back, {
    rv_msea$stored_params <- list(
      pathway_db = input$tools_msea_pathway_db,
      fdr_cutoff = input$tools_msea_fdr_cutoff,
      min_pathway_size = input$tools_msea_min_pathway_size,
      min_overlap = input$tools_msea_min_overlap,
      max_terms = input$tools_msea_max_terms,
      pathway_db_view = input$tools_msea_pathway_db_view,
      plot_type = input$tools_msea_plot_type,
      color_mode = input$tools_msea_color_mode,
      fdr_palette = input$tools_msea_fdr_palette,
      flat_color = input$tools_msea_flat_color,
      alpha = input$tools_msea_alpha,
      show_pathway_id = input$tools_msea_show_pathway_id,
      flip_axis = input$tools_msea_flip_axis,
      font_size = input$tools_msea_font_size,
      axis_text_size = input$tools_msea_axis_text_size,
      width = input$tools_msea_width,
      height = input$tools_msea_height
    )
    rv_msea$stored_metabolites <- input$tools_msea_metabolites
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Navigation for Class Enrichment — disabled (no guaranteed database)
  # observeEvent(input$tools_open_class_enrichment, {
  #   current_tool("class_enrichment")
  # }, ignoreInit = TRUE)
  #
  # observeEvent(input$tools_class_enrichment_back, {
  #   rv_class_enrichment$stored_params <- list(
  #     class_level = input$tools_class_enrichment_class_level,
  #     fdr_cutoff = input$tools_class_enrichment_fdr_cutoff,
  #     min_class_size = input$tools_class_enrichment_min_class_size,
  #     max_terms = input$tools_class_enrichment_max_terms,
  #     plot_type = input$tools_class_enrichment_plot_type,
  #     color_mode = input$tools_class_enrichment_color_mode,
  #     fdr_palette = input$tools_class_enrichment_fdr_palette,
  #     flat_color = input$tools_class_enrichment_flat_color,
  #     alpha = input$tools_class_enrichment_alpha,
  #     flip_axis = input$tools_class_enrichment_flip_axis,
  #     font_size = input$tools_class_enrichment_font_size,
  #     axis_text_size = input$tools_class_enrichment_axis_text_size,
  #     width = input$tools_class_enrichment_width,
  #     height = input$tools_class_enrichment_height
  #   )
  #   rv_class_enrichment$stored_metabolites <- input$tools_class_enrichment_metabolites
  #   current_tool("landing")
  # }, ignoreInit = TRUE)

  # Navigation for Pathway FCS
  observeEvent(input$tools_open_pathway_fcs, {
    current_tool("pathway_fcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_pathway_fcs_back, {
    rv_pathway_fcs$stored_params <- list(
      pathway_db = input$tools_pathway_fcs_pathway_db,
      fdr_cutoff = input$tools_pathway_fcs_fdr_cutoff,
      min_pathway_size = input$tools_pathway_fcs_min_pathway_size,
      min_overlap = input$tools_pathway_fcs_min_overlap,
      max_terms = input$tools_pathway_fcs_max_terms,
      plot_type = input$tools_pathway_fcs_plot_type,
      color_mode = input$tools_pathway_fcs_color_mode,
      flat_color = input$tools_pathway_fcs_flat_color,
      alpha = input$tools_pathway_fcs_alpha,
      show_pathway_id = input$tools_pathway_fcs_show_pathway_id,
      flip_axis = input$tools_pathway_fcs_flip_axis,
      font_size = input$tools_pathway_fcs_font_size,
      axis_text_size = input$tools_pathway_fcs_axis_text_size,
      width = input$tools_pathway_fcs_width,
      height = input$tools_pathway_fcs_height
    )
    rv_pathway_fcs$stored_metabolites <- input$tools_pathway_fcs_metabolites
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Navigation for TerpBase Builder
  observeEvent(input$tools_open_terpbase, {
    current_tool("terpbase")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_terpbase_back, {
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Navigation for ID Reconcile
  observeEvent(input$tools_open_id_reconcile, {
    current_tool("id_reconcile")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_reconcile_back, {
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Helper to restore parameters after UI is rendered
  # Uses observe + invalidateLater for a delayed update without shinyjs
  tools_restore_after_delay <- function(restore_fn, delay_ms = 150) {
    restore_done <- FALSE
    observe({
      if (!restore_done) {
        invalidateLater(delay_ms, session)
        restore_done <<- TRUE
      } else {
        restore_fn()
      }
    })
  }

  # Restore stored parameters when tools are opened
  observeEvent(current_tool(), {
    tool <- current_tool()

    # Restore GO-ORA parameters
    if (tool == "goora" && !is.null(rv$stored_params)) {
      p <- rv$stored_params
      genes <- rv$stored_genes
      # Schedule restore after UI renders
      observe({
        invalidateLater(150, session)
      }, once = TRUE)
      observe({
        # Check if input exists (UI has rendered)
        req(input$tools_goora_fdr_cutoff)
        isolate({
          if (!is.null(p$fdr_cutoff)) updateNumericInput(session, "tools_goora_fdr_cutoff", value = p$fdr_cutoff)
          if (!is.null(p$min_term_size)) updateNumericInput(session, "tools_goora_min_term_size", value = p$min_term_size)
          if (!is.null(p$min_overlap)) updateNumericInput(session, "tools_goora_min_overlap", value = p$min_overlap)
          if (!is.null(p$max_terms)) updateNumericInput(session, "tools_goora_max_terms", value = p$max_terms)
          if (!is.null(p$ontology_view)) updateSelectInput(session, "tools_goora_ontology_view", selected = p$ontology_view)
          if (!is.null(p$plot_type)) updateSelectInput(session, "tools_goora_plot_type", selected = p$plot_type)
          if (!is.null(p$color_mode)) updateSelectInput(session, "tools_goora_color_mode", selected = p$color_mode)
          if (!is.null(p$fdr_palette)) updateSelectInput(session, "tools_goora_fdr_palette", selected = p$fdr_palette)
          if (!is.null(p$flat_color)) updateTextInput(session, "tools_goora_flat_color", value = p$flat_color)
          if (!is.null(p$alpha)) updateSliderInput(session, "tools_goora_alpha", value = p$alpha)
          if (!is.null(p$show_go_id)) updateCheckboxInput(session, "tools_goora_show_go_id", value = p$show_go_id)
          if (!is.null(p$flip_axis)) updateCheckboxInput(session, "tools_goora_flip_axis", value = p$flip_axis)
          if (!is.null(p$font_size)) updateNumericInput(session, "tools_goora_font_size", value = p$font_size)
          if (!is.null(p$axis_text_size)) updateNumericInput(session, "tools_goora_axis_text_size", value = p$axis_text_size)
          if (!is.null(p$width)) updateNumericInput(session, "tools_goora_width", value = p$width)
          if (!is.null(p$height)) updateNumericInput(session, "tools_goora_height", value = p$height)
          if (!is.null(genes)) updateTextAreaInput(session, "tools_goora_genes", value = genes)
        })
      }, once = TRUE)
    }

    # Restore 1D GO-FCS parameters
    if (tool == "1dgofcs" && !is.null(rv_1dgofcs$stored_params)) {
      p <- rv_1dgofcs$stored_params
      genes <- rv_1dgofcs$stored_genes
      observe({
        req(input$tools_1dgofcs_fdr_cutoff)
        isolate({
          if (!is.null(p$fdr_cutoff)) updateNumericInput(session, "tools_1dgofcs_fdr_cutoff", value = p$fdr_cutoff)
          if (!is.null(p$min_term_size)) updateNumericInput(session, "tools_1dgofcs_min_term_size", value = p$min_term_size)
          if (!is.null(p$max_terms)) updateNumericInput(session, "tools_1dgofcs_max_terms", value = p$max_terms)
          if (!is.null(p$ontology_view)) updateSelectInput(session, "tools_1dgofcs_ontology_view", selected = p$ontology_view)
          if (!is.null(p$plot_type)) updateSelectInput(session, "tools_1dgofcs_plot_type", selected = p$plot_type)
          if (!is.null(p$color_mode)) updateSelectInput(session, "tools_1dgofcs_color_mode", selected = p$color_mode)
          if (!is.null(p$fdr_palette)) updateSelectInput(session, "tools_1dgofcs_fdr_palette", selected = p$fdr_palette)
          if (!is.null(p$flat_color)) updateTextInput(session, "tools_1dgofcs_flat_color", value = p$flat_color)
          if (!is.null(p$alpha)) updateSliderInput(session, "tools_1dgofcs_alpha", value = p$alpha)
          if (!is.null(p$show_go_id)) updateCheckboxInput(session, "tools_1dgofcs_show_go_id", value = p$show_go_id)
          if (!is.null(p$flip_axis)) updateCheckboxInput(session, "tools_1dgofcs_flip_axis", value = p$flip_axis)
          if (!is.null(p$font_size)) updateNumericInput(session, "tools_1dgofcs_font_size", value = p$font_size)
          if (!is.null(p$axis_text_size)) updateNumericInput(session, "tools_1dgofcs_axis_text_size", value = p$axis_text_size)
          if (!is.null(p$width)) updateNumericInput(session, "tools_1dgofcs_width", value = p$width)
          if (!is.null(p$height)) updateNumericInput(session, "tools_1dgofcs_height", value = p$height)
          if (!is.null(genes)) updateTextAreaInput(session, "tools_1dgofcs_genes", value = genes)
        })
      }, once = TRUE)
    }

    # Restore 2D GO-FCS parameters
    if (tool == "2dgofcs" && !is.null(rv_2dgofcs$stored_params)) {
      p <- rv_2dgofcs$stored_params
      genes <- rv_2dgofcs$stored_genes
      x_label <- rv_2dgofcs$stored_x_label
      y_label <- rv_2dgofcs$stored_y_label
      observe({
        req(input$tools_2dgofcs_fdr_cutoff)
        isolate({
          if (!is.null(p$fdr_cutoff)) updateNumericInput(session, "tools_2dgofcs_fdr_cutoff", value = p$fdr_cutoff)
          if (!is.null(p$min_term_size)) updateNumericInput(session, "tools_2dgofcs_min_term_size", value = p$min_term_size)
          if (!is.null(p$max_terms)) updateNumericInput(session, "tools_2dgofcs_max_terms", value = p$max_terms)
          if (!is.null(p$ontology_view)) updateSelectInput(session, "tools_2dgofcs_ontology_view", selected = p$ontology_view)
          if (!is.null(p$color_mode)) updateSelectInput(session, "tools_2dgofcs_color_mode", selected = p$color_mode)
          if (!is.null(p$fdr_palette)) updateSelectInput(session, "tools_2dgofcs_fdr_palette", selected = p$fdr_palette)
          if (!is.null(p$flat_color)) updateTextInput(session, "tools_2dgofcs_flat_color", value = p$flat_color)
          if (!is.null(p$dot_alpha)) updateSliderInput(session, "tools_2dgofcs_dot_alpha", value = p$dot_alpha)
          if (!is.null(p$show_ref_lines)) updateCheckboxInput(session, "tools_2dgofcs_show_ref_lines", value = p$show_ref_lines)
          if (!is.null(p$show_diagonal_guides)) updateCheckboxInput(session, "tools_2dgofcs_show_diagonal_guides", value = p$show_diagonal_guides)
          if (!is.null(p$label_font_size)) updateNumericInput(session, "tools_2dgofcs_label_font_size", value = p$label_font_size)
          if (!is.null(p$axis_text_size)) updateNumericInput(session, "tools_2dgofcs_axis_text_size", value = p$axis_text_size)
          if (!is.null(p$width)) updateNumericInput(session, "tools_2dgofcs_width", value = p$width)
          if (!is.null(p$height)) updateNumericInput(session, "tools_2dgofcs_height", value = p$height)
          if (!is.null(genes)) updateTextAreaInput(session, "tools_2dgofcs_genes", value = genes)
          if (!is.null(x_label)) updateTextInput(session, "tools_2dgofcs_x_label", value = x_label)
          if (!is.null(y_label)) updateTextInput(session, "tools_2dgofcs_y_label", value = y_label)
        })
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for standalone GO-ORA
  observeEvent(input$tools_goora_terpbase_default_load, {
    tools_load_terpbase(input$tools_goora_terpbase_default_path, rv)
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_terpbase_file, {
    req(input$tools_goora_terpbase_file)
    tools_load_terpbase(input$tools_goora_terpbase_file$datapath, rv)
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for 1D GO-FCS
  observeEvent(input$tools_1dgofcs_terpbase_default_load, {
    tools_load_terpbase(input$tools_1dgofcs_terpbase_default_path, rv_1dgofcs)
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_terpbase_file, {
    req(input$tools_1dgofcs_terpbase_file)
    tools_load_terpbase(input$tools_1dgofcs_terpbase_file$datapath, rv_1dgofcs)
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for 2D GO-FCS
  observeEvent(input$tools_2dgofcs_terpbase_default_load, {
    tools_load_terpbase(input$tools_2dgofcs_terpbase_default_path, rv_2dgofcs)
  }, ignoreInit = TRUE)

  observeEvent(input$tools_2dgofcs_terpbase_file, {
    req(input$tools_2dgofcs_terpbase_file)
    tools_load_terpbase(input$tools_2dgofcs_terpbase_file$datapath, rv_2dgofcs)
  }, ignoreInit = TRUE)

  # Initialize tool-specific server logic
  tools_goora_server(input, output, session, app_state, rv, defs_goora)
  tools_1dgofcs_server(input, output, session, app_state, rv_1dgofcs, defs_1dgofcs)
  tools_2dgofcs_server(input, output, session, app_state, rv_2dgofcs, defs_2dgofcs)
  tools_msea_server(input, output, session, app_state, rv_msea, defs_msea)
  # tools_class_enrichment_server(input, output, session, app_state, rv_class_enrichment, defs_class_enrichment)  # disabled
  tools_pathway_fcs_server(input, output, session, app_state, rv_pathway_fcs, defs_pathway_fcs)
  tools_terpbase_server(input, output, session, app_state, rv_terpbase)
  tools_id_reconcile_server(input, output, session, app_state, rv_reconcile)

  # Store current_tool in session$userData for child modules to access
  session$userData$tools_current_tool <- current_tool
}
