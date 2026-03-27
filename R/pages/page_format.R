# R/pages/page_format.R
# Format files page with:
# - Three tabs (New / Merge / Edit) inside a card tabset
# - New tab: two-panel layout
#   - Left: file + sheet selector (xlsx) + identifier setup + build + post-build download
#   - Right: sample group count + per-group bin + per-column rename
# - Merge tab: multi-file loader + bin previews + per-column rename + build + post-build download
# - Edit tab: load ONE formatted workbook, validate, allow edits (same UI style), build + post-build download

fmt_step_ui <- function(step, title, ...) {
  div(
    class = "fmt-step",
    div(
      class = "fmt-step-head",
      tags$span(class = "fmt-step-num", step),
      tags$span(class = "fmt-step-title", title)
    ),
    div(class = "fmt-step-body", ...)
  )
}

page_format_ui <- function() {
  # Custom tab button helper
  tab_btn <- function(id, label, active = FALSE) {
    tags$button(
      id = id,
      class = paste("fmt-tab-btn", if (active) "active" else ""),
      type = "button",
      label
    )
  }

  msterp_page(
    title = "Format files",
    actions = tags$div(
      class = "fmt-tab-bar",
      tab_btn("fmt_tab_new_btn", "New", active = TRUE),
      tab_btn("fmt_tab_merge_btn", "Merge"),
      tab_btn("fmt_tab_edit_btn", "Edit"),
      tab_btn("fmt_tab_compare_btn", "Compare")
    ),
    class = "format-page",

    # Complete custom styling for format page
    tags$style(htmltools::HTML("
      /* === Format Page Container === */
      #format-page {
        display: flex;
        flex-direction: column;
        height: 100%;
        width: 100%;
        min-height: 0;
      }

      /* === Custom Tab Bar === */
      .format-page .fmt-tab-bar {
        display: inline-flex;
        flex-direction: row;
        gap: 4px;
        padding: var(--tab-pill-pad);
        background: var(--tab-bg);
        border-radius: calc(var(--tab-radius) + 2px);
        border: 1px solid var(--tab-border);
        margin: 0 auto;
      }

      .format-page .fmt-tab-btn {
        padding: 10px 28px;
        font-size: 14px;
        font-weight: 700;
        font-family: inherit;
        color: var(--tab-text);
        background: transparent;
        border: none;
        border-radius: 8px;
        cursor: pointer;
        transition: all 0.15s ease;
        white-space: nowrap;
      }

      .format-page .fmt-tab-btn:hover {
        background: var(--tab-hover-bg);
        color: var(--tab-text-hover);
      }

      .format-page .fmt-tab-btn.active {
        background: var(--tab-active-bg);
        color: var(--tab-text-active);
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      }

      /* === Tab Content Area === */
      #format-page .fmt-tab-content {
        flex: 1 1 auto;
        min-height: 0;
        overflow: hidden;
        padding-top: 14px;
        position: relative;
      }

      #format-page .fmt-tab-pane {
        height: 100%;
        width: 100%;
        position: absolute;
        top: 0;
        left: 0;
        visibility: hidden;
        pointer-events: none;
      }

      #format-page .fmt-tab-pane.active {
        position: relative;
        visibility: visible;
        pointer-events: auto;
      }

      /* === Two Panel Layout inside tabs === */
      #format-page .fmt-two-panel {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(260px, 34%);
        gap: 16px;
        height: 100%;
        min-height: 0;
      }
      #format-page .fmt-two-panel.single {
        grid-template-columns: minmax(0, 1fr);
      }

      #format-page .fmt-panel {
        background: var(--color-white);
        border: 1px solid var(--md-card-border);
        border-radius: 12px;
        padding: 16px;
        overflow: auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      #format-page .fmt-panel-right {
        background: var(--bg-muted);
      }

      #format-page .fmt-panel h4 {
        font-size: 16px;
        font-weight: 700;
        color: var(--text-primary);
        margin: 0 0 12px 0;
        padding-bottom: 10px;
        border-bottom: 1px solid var(--md-border);
      }

      #format-page .fmt-panel h5 {
        font-size: 14px;
        font-weight: 600;
        color: var(--text-secondary);
        margin: 12px 0 8px 0;
      }

      #format-page .fmt-step {
        background: var(--color-white);
        border: 1px solid var(--md-border);
        border-radius: 12px;
        padding: 12px;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      #format-page .fmt-step-head {
        display: flex;
        align-items: center;
        gap: 8px;
      }
      #format-page .fmt-step-num {
        background: var(--md-red);
        color: #ffffff;
        font-weight: 700;
        font-size: 12px;
        padding: 2px 8px;
        border-radius: 999px;
      }
      #format-page .fmt-step-title {
        font-weight: 700;
        font-size: 13px;
        color: var(--text-primary);
      }
      #format-page .fmt-step-body {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      #format-page .fmt-advanced {
        border: 1px solid var(--md-border);
        border-radius: 10px;
        padding: 8px 10px;
        background: var(--color-white);
      }
      #format-page .fmt-advanced summary {
        cursor: pointer;
        font-weight: 700;
      }
      #format-page .fmt-advanced[open] summary {
        margin-bottom: 8px;
      }

      /* === Form Controls === */
      #format-page .form-group {
        margin-bottom: 12px;
      }

      #format-page label {
        font-size: 13px;
        font-weight: 600;
        color: var(--text-secondary);
      }

      #format-page .form-control,
      #format-page .shiny-input-container input,
      #format-page .shiny-input-container select {
        font-size: 13px;
        padding: 8px 12px;
        border-radius: 6px;
        border: 1px solid var(--md-border);
      }
      #format-page .input-group-btn .btn-file {
        height: 38px;
        padding: 8px 12px;
        line-height: 20px;
      }
      #format-page .input-group-btn + .form-control {
        height: 38px;
        padding: 8px 12px;
      }

      #format-page .btn {
        font-size: 13px;
        padding: 10px 18px;
        border-radius: 8px;
        font-weight: 700;
      }

      #format-page .btn-primary {
        background: var(--primary);
        border-color: var(--primary-dark);
        color: #fff;
      }

      #format-page .btn-primary:hover {
        background: var(--primary-dark);
      }

      /* === Status/Preview text === */
      #format-page pre,
      #format-page .shiny-text-output {
        font-size: 12px;
        background: var(--bg-muted);
        padding: 10px;
        border-radius: 6px;
        border: 1px solid var(--md-border);
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      }

      /* === Card styling inside panels === */
      #format-page .card,
      #format-page .bslib-card {
        background: var(--md-card-bg);
        border: 1px solid var(--md-card-border);
        border-radius: 10px;
        margin-bottom: 12px;
        box-shadow: var(--md-card-shadow);
      }

      #format-page .card-header {
        font-size: 13px;
        font-weight: 700;
        padding: 10px 12px;
        background: var(--color-white);
        border-bottom: 1px solid var(--md-border);
      }

      #format-page .card-body {
        padding: 12px;
        font-size: 13px;
      }

      #format-page .fmt-tip {
        background: var(--status-warn-bg);
        border: 1px solid var(--status-warn-border);
        border-radius: 10px;
        padding: 10px 12px;
        font-size: 12px;
        display: flex;
        flex-direction: column;
        gap: 8px;
      }
      #format-page .fmt-tip-title { font-weight: 700; }
      #format-page .fmt-tip-actions { display: flex; gap: 8px; flex-wrap: wrap; }

      #format-page .fmt-summary {
        background: var(--color-white);
        border: 1px dashed var(--md-card-border);
        border-radius: 10px;
        padding: 10px 12px;
        font-size: 12px;
        display: flex;
        flex-direction: column;
        gap: 6px;
      }
      #format-page .fmt-summary strong { font-size: 12px; }

      /* === File input styling === */
      #format-page .shiny-input-container {
        margin-bottom: 10px;
      }

      /* === HR styling === */
      #format-page hr {
        border: none;
        border-top: 1px solid var(--md-border);
        margin: 14px 0;
      }

      @media (max-width: 980px) {
        #format-page .fmt-two-panel {
          grid-template-columns: minmax(0, 1fr);
        }
      }

      /* Compare tab two-column layout */
      #format-page .fmt-compare-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 16px;
        margin-bottom: 16px;
      }

      #format-page .fmt-dataset-panel {
        background: var(--color-white);
        border: 1px solid var(--md-border);
        border-radius: 12px;
        padding: 16px;
      }

      #format-page .fmt-dataset-panel h5 {
        font-size: 15px;
        font-weight: 700;
        margin: 0 0 12px 0;
        color: var(--text-primary);
      }

      #format-page .fmt-overlap-preview {
        background: var(--status-info-bg);
        border: 1px solid var(--status-info-text);
        border-radius: 10px;
        padding: 14px;
        margin-bottom: 16px;
      }

      #format-page .fmt-labels-section {
        background: var(--status-warn-bg);
        border: 1px solid var(--status-warn-border);
        border-radius: 10px;
        padding: 14px;
        margin-bottom: 16px;
      }

      /* SILAC dropdown fixes: wider containers and taller dropdown panels */
      #format-page .silac-pair-row {
        display: flex;
        gap: 8px;
        align-items: center;
        margin-bottom: 8px;
        flex-wrap: wrap;
      }
      #format-page .silac-pair-row .silac-dd {
        flex: 1 1 160px;
        min-width: 160px;
      }
      #format-page .silac-pair-row .silac-dd-heavy {
        border-left: 3px solid #1a73e8;
        padding-left: 6px;
        border-radius: 4px;
      }
      #format-page .silac-pair-row .silac-dd-light {
        border-left: 3px solid #e8a31a;
        padding-left: 6px;
        border-radius: 4px;
      }
      #format-page .silac-pair-row .selectize-dropdown-content {
        max-height: 320px !important;
      }
      #format-page .silac-pair-row .shiny-input-container {
        margin-bottom: 0;
      }

      @media (max-width: 768px) {
        #format-page .fmt-compare-grid {
          grid-template-columns: 1fr;
        }
      }
    ")),

    # JavaScript for custom tab switching
    tags$script(htmltools::HTML("
      $(document).ready(function() {
        // Tab switching logic
        $(document).on('click', '.fmt-tab-btn', function() {
          var tabId = $(this).attr('id');
          var paneId = tabId.replace('_btn', '_pane');

          // Update button states
          $('.fmt-tab-btn').removeClass('active');
          $(this).addClass('active');

          // Update pane visibility
          $('.fmt-tab-pane').removeClass('active');
          $('#' + paneId).addClass('active');

          // Notify Shiny of tab change
          Shiny.setInputValue('fmt_active_tab', tabId.replace('fmt_tab_', '').replace('_btn', ''), {priority: 'event'});
        });

        // Blur-only text inputs for group names (prevents reactive updates on each keystroke)
        $(document).on('shiny:bound', function(e) {
          if (e.target && e.target.id && e.target.id.match(/^fmt_gname_\\d+$/)) {
            var $input = $(e.target);
            // Remove default Shiny event handlers for this input
            $input.off('change.shinyInput input.shinyInput keyup.shinyInput');
            // Only update on blur
            $input.on('blur.fmtBlur', function() {
              Shiny.setInputValue(this.id, $(this).val(), {priority: 'event'});
            });
          }
        });

        // Show loaded filename in fileInput after renderUI re-creates it
        Shiny.addCustomMessageHandler('msterp_show_file_name', function(payload) {
          if (!payload || !payload.id || !payload.name) return;
          // Delay to let renderUI finish creating the DOM element
          setTimeout(function() {
            var container = document.getElementById(payload.id);
            if (!container) return;
            var wrapper = container.closest('.shiny-input-container');
            if (!wrapper) wrapper = container;
            var textInput = wrapper.querySelector('input[type=\"text\"].form-control');
            if (!textInput) textInput = container.querySelector('input[type=\"text\"].form-control');
            if (textInput) textInput.value = payload.name;
          }, 100);
        });

        // Blur-only update for output file name inputs (prevents focus loss on keystroke)
        $(document).on('shiny:bound', function(e) {
          var id = e.target && e.target.id;
          if (!id) return;
          if (id === 'fmt_out_name' || id === 'merge_out_name' || id === 'edit_out_name') {
            var $input = $(e.target);
            $input.off('change.shinyInput input.shinyInput keyup.shinyInput');
            $input.on('blur.fmtOutBlur', function() {
              Shiny.setInputValue(this.id, $(this).val(), {priority: 'event'});
            });
            $input.on('keydown.fmtOutEnter', function(ev) {
              if (ev.key === 'Enter') $(this).blur();
            });
          }
        });

        // Inline validation for group name inputs
        $(document).on('blur.fmtGnameValidate', '[id^=\"fmt_gname_\"]', function() {
          var val = $(this).val().trim();
          var re = /^[A-Za-z][A-Za-z0-9 _.\\-]*$/;
          var warnId = this.id + '_warn';
          var $warn = $('#' + warnId);
          if (!$warn.length) {
            $warn = $('<div>', {id: warnId, style: 'color: #c62828; font-size: 11px; font-weight: 600; margin-top: 2px;'});
            $(this).closest('.shiny-input-container').after($warn);
          }
          if (val.length > 0 && !re.test(val)) {
            $warn.text('Invalid name: must start with a letter, only letters/numbers/spaces/underscores/dots/dashes allowed.').show();
          } else {
            $warn.hide();
          }
        });

        // Modal dismiss handler - apply column selections when modal closes
        $(document).on('hidden.bs.modal', '.modal', function() {
          var $modal = $(this);
          var $checkbox = $modal.find('[id^=\"fmt_modal_cols_\"]');
          if ($checkbox.length > 0) {
            var checkboxId = $checkbox.attr('id');
            // Extract group index from id (fmt_modal_cols_1 -> 1)
            var match = checkboxId.match(/fmt_modal_cols_(\\d+)/);
            if (match) {
              var groupIdx = parseInt(match[1], 10);
              Shiny.setInputValue('fmt_modal_dismissed', {
                time: Date.now(),
                group: groupIdx
              }, {priority: 'event'});
            }
          }
        });
      });
    ")),

    tags$div(
      id = "format-page",

      # Tab content area
      tags$div(
        class = "fmt-tab-content",

        # New tab pane
        tags$div(
          id = "fmt_tab_new_pane",
          class = "fmt-tab-pane active",
          uiOutput("fmt_new_content")
        ),

        # Merge tab pane
        tags$div(
          id = "fmt_tab_merge_pane",
          class = "fmt-tab-pane",
          uiOutput("fmt_merge_content")
        ),

        # Edit tab pane
        tags$div(
          id = "fmt_tab_edit_pane",
          class = "fmt-tab-pane",
          uiOutput("fmt_edit_content")
        ),

        # Compare tab pane
        tags$div(
          id = "fmt_tab_compare_pane",
          class = "fmt-tab-pane",
          uiOutput("fmt_compare_content")
        )
      )
    )
  )
}

page_format_server <- function(input, output, session, app_state) {
  
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 1 && (is.na(a) || !nzchar(as.character(a)))) return(b)
    a
  }
  
  safe_key <- function(x) {
    x <- as.character(x)
    x <- gsub("\\s+", "_", x)
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    if (nchar(x) > 80) x <- substr(x, 1, 80)
    x
  }
  
  busy_step <- function(active, msg = "", pct = NULL) {
    if (exists("msterp_set_busy", mode = "function", inherits = TRUE)) {
      msterp_set_busy(session, active, msg, pct)
      try(session$flushReact(), silent = TRUE)
      try(shiny::flushReact(), silent = TRUE)
    }
  }

  reset_file_input <- function(id) {
    session$sendCustomMessage("msterp_reset_file_input", list(id = id))
  }

  fmt_sample_raw_data <- function() {
    data.frame(
      Gene = c("TP53", "EGFR", "STAT3"),
      Protein = c("P04637", "P00533", "P40763"),
      Sample_A_1 = c(100, 200, 150),
      Sample_A_2 = c(110, 190, 160),
      Sample_B_1 = c(80, 210, 140),
      Sample_B_2 = c(90, 205, 145),
      check.names = FALSE
    )
  }

  fmt_make_sample_formatted <- function(out_path) {
    data_df <- fmt_sample_raw_data()

    meta <- list(
      schema_version = 1,
      data_type = "proteomics",
      analysis_level = "protein",
      id_primary_type = "gene",
      id_primary_col = "Gene",
      id_protein_col = "Protein",
      created_at = format(Sys.time(), "%Y-%m-%d")
    )

    groups_df <- data.frame(
      group_id = c(1, 2),
      group_name = c("Group A", "Group B"),
      color = c("#d50032", "#f0cf65"),
      is_control = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    )

    columns_df <- data.frame(
      data_col = c("Sample_A_1", "Sample_A_2", "Sample_B_1", "Sample_B_2"),
      display_name = c("GroupA_1", "GroupA_2", "GroupB_1", "GroupB_2"),
      group_id = c(1, 1, 2, 2),
      group_name = c("Group A", "Group A", "Group B", "Group B"),
      replicate = c(1, 2, 1, 2),
      include = TRUE,
      source_file = "sample",
      source_col = c("Sample_A_1", "Sample_A_2", "Sample_B_1", "Sample_B_2"),
      stringsAsFactors = FALSE
    )

    design_df <- msterp_build_design_sheet(meta, groups_df, columns_df)
    msterp_write_formatted_xlsx(out_path, data_df, design_df)
    invisible(out_path)
  }
  
  # =========================================================
  # NEW TAB STATE
  # =========================================================
  fmt_upload <- reactiveVal(NULL)   # list(path, name, ext, sheets)
  fmt_raw    <- reactiveVal(NULL)   # data.frame
  fmt_built_path <- reactiveVal(NULL)
  
  fmt_state <- reactiveValues(
    cols   = list(),        # list of character vectors, one per group (selected columns)
    rename = character(0),  # named character vector: names=source_col, values=output_col
    silac_nreps = list(),   # list of integer, one per group (number of SILAC replicates)
    silac_pairs = list()    # list of lists, one per group; each inner list has replicate entries
                            # with $heavy and $light column names
  )

  # Max groups for pre-registering dynamic observers (SILAC rep buttons, column modals)
  silac_max_groups <- 20L

  
  ensure_group_slots <- function(n) {
    n <- as.integer(n)
    if (is.na(n) || n < 1) n <- 1
    cur <- length(fmt_state$cols)
    if (cur < n) {
      fmt_state$cols[(cur + 1):n] <- replicate(n - cur, character(0), simplify = FALSE)
    } else if (cur > n) {
      fmt_state$cols <- fmt_state$cols[1:n]
    }
    invisible(TRUE)
  }
  
  fmt_clear_built <- function() {
    fmt_built_path(NULL)
  }

  get_ordered_cols <- function(i) {
    selected <- fmt_state$cols[[i]] %||% character(0)
    if (length(selected) == 0) return(character(0))

    rank_val <- input[[paste0("fmt_rank_", i)]]
    # Only trust rank_list values if they exactly match current selection
    # (guards against stale input values from a previously loaded file)
    cols <- if (!is.null(rank_val) && length(rank_val) > 0 &&
                all(rank_val %in% selected) && all(selected %in% rank_val)) {
      rank_val
    } else {
      selected
    }
    cols[!is.na(cols) & nzchar(as.character(cols))]
  }

  fmt_id_cols <- reactive({
    dtype <- tolower(trimws(input$fmt_data_type %||% "proteomics"))

    if (dtype == "metabolomics") {
      # Metabolomics: metabolite name (primary) and optional ID
      ids <- c(
        input$fmt_metabolite_name_col,
        input$fmt_metabolite_col
      )
    } else {
      # Proteomics: existing logic
      lvl <- tolower(trimws(input$fmt_analysis_level %||% "protein"))
      if (!lvl %in% c("protein", "peptide")) lvl <- "protein"
      ids <- c(
        input$fmt_gene_col,
        input$fmt_protein_col,
        if (identical(lvl, "peptide")) input$fmt_peptide_col else NULL
      )
    }
    ids <- ids[!is.null(ids) & nzchar(ids)]
    unique(ids)
  })
  
  fmt_data_cols <- reactive({
    df <- fmt_raw()
    req(!is.null(df))
    setdiff(names(df), fmt_id_cols())
  })
  
  fmt_read_current <- function() {
    up <- fmt_upload()
    req(!is.null(up), nzchar(up$path))
    
    busy_step(TRUE, "Reading file…", 10)
    
    ext <- tolower(up$ext %||% "")
    if (ext == "xlsx") {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        busy_step(FALSE)
        stop("Package 'readxl' is required to read .xlsx files.")
      }
      sheet <- input$fmt_sheet %||% (up$sheets[[1]] %||% 1)
      df <- readxl::read_excel(up$path, sheet = sheet)
      df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      res <- msterp_read_raw_file(up$path)  # expected to return list(data=...)
      df <- res$data
      df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    fmt_raw(df)
    
    ensure_group_slots(input$fmt_ngroups %||% 2)
    
    busy_step(FALSE)
    invisible(TRUE)
  }
  
  # =========================================================
  # NEW TAB UI
  # =========================================================
  output$fmt_new_content <- renderUI({
    has_data <- !is.null(fmt_raw())

    right_panel <- if (has_data) {
      tags$div(
        class = "fmt-panel fmt-panel-right",
        tags$h4("Status"),
        uiOutput("fmt_new_summary_ui"),
        verbatimTextOutput("fmt_status"),
        hr(),
        tags$h4("Columns"),
        tags$details(
          class = "fmt-advanced",
          tags$summary("Advanced: Rename output columns"),
          uiOutput("fmt_rename_ui")
        ),
        uiOutput("fmt_rename_validity_ui"),
        tags$h5("Unassigned columns"),
        verbatimTextOutput("fmt_unassigned")
      )
    } else {
      NULL
    }

    tags$div(
      class = paste("fmt-two-panel", if (!has_data) "single"),
      tags$div(
        class = "fmt-panel",
        tags$h4("Workflow"),
        if (has_data) div(
          style = "margin-bottom: 10px;",
          actionButton("fmt_reset", "Reset inputs", class = "btn btn-default btn-sm", width = "auto",
                       icon = icon("refresh"))
        ) else NULL,
        fmt_step_ui(
          "1",
          "Upload raw data",
          fileInput("fmt_file", "Upload raw data (.xlsx, .csv, .tsv)", accept = c(".xlsx", ".csv", ".tsv", ".txt")),
          div(
            style = "margin: 6px 0 0;",
            downloadButton("fmt_sample_new", "Sample raw CSV", class = "btn btn-default btn-sm")
          ),
          uiOutput("fmt_sheet_ui"),
          uiOutput("fmt_left_after_upload_ui")
        ),
        if (has_data) fmt_step_ui(
          "2",
          "Assign sample groups",
          numericInput("fmt_ngroups", "Number of sample groups", value = 2, min = 1, max = 20, step = 1),
          uiOutput("fmt_control_group_ui"),
          uiOutput("fmt_group_bins_ui")
        ) else NULL,
        if (has_data) fmt_step_ui(
          "3",
          "Build and export",
          uiOutput("fmt_build_ui"),
          uiOutput("fmt_postbuild_ui")
        ) else NULL
      ),
      right_panel
    )
  })

  output$fmt_sample_new <- downloadHandler(
    filename = function() "msterp_sample_raw.csv",
    content = function(file) {
      utils::write.csv(fmt_sample_raw_data(), file, row.names = FALSE)
    }
  )

  output$fmt_new_summary_ui <- renderUI({
    df <- fmt_raw()
    built <- !is.null(fmt_built_path())
    if (is.null(df)) {
      return(div(class = "fmt-summary", strong("Summary"), div("No raw file loaded yet.")))
    }

    div(
      class = "fmt-summary",
      strong("Summary"),
      div(sprintf("Rows: %d | Columns: %d", nrow(df), ncol(df))),
      div(sprintf("Groups: %s", input$fmt_ngroups %||% 0)),
      div(sprintf("Renames: %d", length(fmt_state$rename %||% character(0)))),
      div(if (built) "Formatted workbook ready to download." else "Build to generate the formatted workbook.")
    )
  })
  
  output$fmt_sheet_ui <- renderUI({
    up <- fmt_upload()
    if (is.null(up)) return(NULL)
    if (tolower(up$ext %||% "") != "xlsx") return(NULL)
    
    sheets <- up$sheets %||% character(0)
    if (length(sheets) <= 1) return(NULL)
    
    selectInput("fmt_sheet", "Sheet", choices = sheets, selected = input$fmt_sheet %||% sheets[[1]])
  })
  
  # Load raw file (and discover sheets if xlsx)
  # Auto-resets previous state when a new file is loaded
  observeEvent(input$fmt_file, {
    req(input$fmt_file$datapath, input$fmt_file$name)

    # Auto-reset all previous state (same as fmt_reset)
    old_ng <- input$fmt_ngroups %||% 2
    fmt_clear_built()
    fmt_raw(NULL)
    fmt_upload(NULL)
    fmt_state$cols   <- list()
    fmt_state$rename <- character(0)
    fmt_state$silac_nreps <- list()
    fmt_state$silac_pairs <- list()
    output$fmt_status <- renderText("")

    updateNumericInput(session, "fmt_ngroups", value = 2)
    updateSelectInput(session, "fmt_control_group", selected = "")
    updateSelectInput(session, "fmt_data_type", selected = "proteomics")
    updateSelectInput(session, "fmt_analysis_level", selected = "protein")
    updateSelectInput(session, "fmt_gene_col", selected = "")
    updateSelectInput(session, "fmt_protein_col", selected = "")
    updateSelectInput(session, "fmt_peptide_col", selected = "")
    updateSelectInput(session, "fmt_metabolite_name_col", selected = "")
    updateSelectInput(session, "fmt_metabolite_col", selected = "")
    updateTextInput(session, "fmt_out_name", value = "msterp_formatted")
    tryCatch(updateCheckboxInput(session, "fmt_is_silac", value = FALSE), error = function(e) NULL)

    has_colour <- requireNamespace("colourpicker", quietly = TRUE)
    for (i in seq_len(old_ng)) {
      updateTextInput(session, paste0("fmt_gname_", i), value = paste0("Sample group ", i))
      if (has_colour) {
        tryCatch(colourpicker::updateColourInput(session, paste0("fmt_gcol_", i), value = "#E03A3E"), error = function(e) NULL)
      } else {
        updateTextInput(session, paste0("fmt_gcol_", i), value = "#E03A3E")
      }
    }

    # Now load the new file
    ext <- tolower(tools::file_ext(input$fmt_file$name))
    up <- list(path = input$fmt_file$datapath, name = input$fmt_file$name, ext = ext, sheets = character(0))

    if (ext == "xlsx") {
      busy_step(TRUE, "Inspecting workbook\u2026", 5)
      if (!requireNamespace("readxl", quietly = TRUE)) {
        busy_step(FALSE)
        output$fmt_status <- renderText("Error: package 'readxl' is required for .xlsx files.")
        fmt_upload(NULL)
        return()
      }
      sh <- tryCatch(readxl::excel_sheets(input$fmt_file$datapath), error = function(e) e)
      busy_step(FALSE)
      if (inherits(sh, "error") || length(sh) == 0) {
        output$fmt_status <- renderText("Error: could not read sheets from workbook.")
        fmt_upload(NULL)
        return()
      }
      up$sheets <- sh
    }

    fmt_upload(up)

    # Read immediately (uses first sheet if xlsx)
    res <- tryCatch({
      fmt_read_current()
      TRUE
    }, error = function(e) e)

    if (inherits(res, "error")) {
      fmt_raw(NULL)
      output$fmt_status <- renderText(paste("Error:", res$message))
      return()
    }

    df <- fmt_raw()
    output$fmt_status <- renderText(sprintf("Loaded %d rows x %d columns.", nrow(df), ncol(df)))

    # Show loaded filename in the file input after renderUI re-creates it
    session$sendCustomMessage("msterp_show_file_name", list(id = "fmt_file", name = input$fmt_file$name))
  }, ignoreInit = TRUE)

  observeEvent(input$fmt_reset, {
    old_ng <- input$fmt_ngroups %||% 2

    fmt_upload(NULL)
    fmt_raw(NULL)
    fmt_state$cols <- list()
    fmt_state$rename <- character(0)
    fmt_state$silac_nreps <- list()
    fmt_state$silac_pairs <- list()
    fmt_clear_built()
    output$fmt_status <- renderText("")
    reset_file_input("fmt_file")

    updateNumericInput(session, "fmt_ngroups", value = 2)
    updateSelectInput(session, "fmt_control_group", selected = "")
    updateSelectInput(session, "fmt_data_type", selected = "proteomics")
    updateSelectInput(session, "fmt_analysis_level", selected = "protein")
    updateSelectInput(session, "fmt_gene_col", selected = "")
    updateSelectInput(session, "fmt_protein_col", selected = "")
    updateSelectInput(session, "fmt_peptide_col", selected = "")
    updateSelectInput(session, "fmt_metabolite_name_col", selected = "")
    updateSelectInput(session, "fmt_metabolite_col", selected = "")
    updateTextInput(session, "fmt_out_name", value = "msterp_formatted")
    tryCatch(updateCheckboxInput(session, "fmt_is_silac", value = FALSE), error = function(e) NULL)

    has_colour <- requireNamespace("colourpicker", quietly = TRUE)
    for (i in seq_len(old_ng)) {
      updateTextInput(session, paste0("fmt_gname_", i), value = paste0("Sample group ", i))
      if (has_colour) {
        tryCatch(colourpicker::updateColourInput(session, paste0("fmt_gcol_", i), value = "#E03A3E"), error = function(e) NULL)
      } else {
        updateTextInput(session, paste0("fmt_gcol_", i), value = "#E03A3E")
      }
    }
  }, ignoreInit = TRUE)
  
  # Re-read when sheet changes (xlsx only)
  observeEvent(input$fmt_sheet, {
    up <- fmt_upload()
    req(!is.null(up))
    if (tolower(up$ext %||% "") != "xlsx") return()
    
    fmt_clear_built()
    fmt_state$cols   <- list()
    fmt_state$rename <- character(0)
    fmt_state$silac_nreps <- list()
    fmt_state$silac_pairs <- list()

    res <- tryCatch({

      fmt_read_current()
      TRUE
    }, error = function(e) e)
    
    if (inherits(res, "error")) {
      fmt_raw(NULL)
      output$fmt_status <- renderText(paste("Error:", res$message))
      return()
    }
    
    df <- fmt_raw()
    output$fmt_status <- renderText(sprintf("Loaded %d rows x %d columns (sheet: %s).", nrow(df), ncol(df), input$fmt_sheet))
  }, ignoreInit = TRUE)
  
  # Left panel content after upload
  output$fmt_left_after_upload_ui <- renderUI({
    df <- fmt_raw()
    if (is.null(df)) return(tags$p("Upload a file to continue."))
    
    tagList(
      uiOutput("fmt_id_select_ui"),
      tags$small("ID columns are excluded from sample-group assignment on the right.")
    )
  })
  
  output$fmt_id_select_ui <- renderUI({
    df <- fmt_raw()
    req(!is.null(df))
    cols <- names(df)

    tagList(
      selectInput(
        "fmt_data_type",
        "Data type",
        choices = c("Proteomics" = "proteomics", "Metabolomics" = "metabolomics"),
        selected = input$fmt_data_type %||% "proteomics"
      ),
      conditionalPanel(
        condition = "input.fmt_data_type == 'proteomics'",
        selectInput(
          "fmt_analysis_level",
          "Analysis level",
          choices = c("Protein" = "protein", "Peptide" = "peptide"),
          selected = input$fmt_analysis_level %||% "protein"
        )
      ),
      conditionalPanel(
        condition = "input.fmt_data_type == 'proteomics'",
        checkboxInput(
          "fmt_is_silac",
          "Dynamic SILAC (pair Heavy / Light channels)",
          value = isolate(input$fmt_is_silac %||% FALSE)
        ),
        conditionalPanel(
          condition = "input.fmt_is_silac",
          tags$small(
            style = "color: var(--text-muted, #666); display: block; margin-top: -8px; margin-bottom: 8px;",
            "Enable for pulse-chase SILAC experiments. Each replicate requires a matched Heavy and Light intensity column."
          )
        )
      ),
      conditionalPanel(
        condition = "input.fmt_data_type == 'proteomics'",
        conditionalPanel(
          condition = "input.fmt_analysis_level == 'peptide'",
          selectInput("fmt_peptide_col", "Peptide ID column (required)", choices = cols)
        ),
        selectInput("fmt_protein_col", "Protein ID column (required)", choices = cols),
        conditionalPanel(
          condition = "input.fmt_analysis_level == 'peptide'",
          tags$small("Protein ID is required for aggregation from peptide-level to protein-level.")
        ),
        selectInput("fmt_gene_col", "Gene symbol column", choices = c("", cols), selected = ""),
        tags$small("Required for analysis (used across the app).")
      ),
      conditionalPanel(
        condition = "input.fmt_data_type == 'metabolomics'",
        selectInput("fmt_metabolite_name_col", "Metabolite name column (required)", choices = cols),
        tags$small("Primary identifier for metabolites (e.g., compound names)."),
        selectInput("fmt_metabolite_col", "Metabolite ID column (optional)",
                    choices = c("(none)" = "", cols), selected = ""),
        tags$small("Secondary identifier (KEGG, HMDB, etc.) for MetaboBase matching.")
      )
    )
  })
  
  # Any key change invalidates previously built workbook
  observeEvent(input$fmt_ngroups,  fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_data_type, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_gene_col, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_protein_col, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_peptide_col, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_analysis_level, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_metabolite_col, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_metabolite_name_col, fmt_clear_built(), ignoreInit = TRUE)
  observeEvent(input$fmt_is_silac, fmt_clear_built(), ignoreInit = TRUE)
  
  # Ensure group slots when ngroups changes
  observeEvent(input$fmt_ngroups, {
    ensure_group_slots(input$fmt_ngroups)
    if (isTRUE(input$fmt_is_silac)) {
      ensure_silac_slots(input$fmt_ngroups)
    }
  }, ignoreInit = TRUE)

  # Ensure SILAC slots when SILAC mode is enabled
  observeEvent(input$fmt_is_silac, {
    if (isTRUE(input$fmt_is_silac)) {
      ensure_silac_slots(input$fmt_ngroups %||% 2)
    }
  }, ignoreInit = TRUE)
  
  # If ID selection changes, drop those columns from group selections
  observeEvent(fmt_id_cols(), {
    ids <- fmt_id_cols()
    for (i in seq_along(fmt_state$cols)) {
      fmt_state$cols[[i]] <- setdiff(fmt_state$cols[[i]], ids)
    }
  }, ignoreInit = TRUE)

  # Control group selector UI
  output$fmt_control_group_ui <- renderUI({
    df <- fmt_raw()
    if (is.null(df)) return(NULL)

    ng <- input$fmt_ngroups %||% 2
    if (ng < 2) return(tags$p(tags$small("At least 2 groups required to define a control.")))

    # Build choices from group names
    choices <- c("None" = "")
    for (i in seq_len(ng)) {
      gname <- input[[paste0("fmt_gname_", i)]] %||% paste0("Sample group ", i)
      choices <- c(choices, stats::setNames(as.character(i), gname))
    }

    div(
      style = "margin-bottom: 12px; padding: 10px; background: var(--color-white); border-radius: 6px; border: 1px solid var(--md-border);",
      tags$label("Define WT / Control group:", style = "font-weight: 700; margin-bottom: 4px; display: block;"),
      selectInput("fmt_control_group", label = NULL, choices = choices,
                  selected = input$fmt_control_group %||% "", width = "100%"),
      tags$small(style = "color: var(--color-text-hint);",
                 "Comparisons will use this group as the reference/baseline when enabled.")
    )
  })

  # Helper: ensure SILAC replicate slots are initialized
  ensure_silac_slots <- function(ng) {
    ng <- as.integer(ng)
    if (is.na(ng) || ng < 1) ng <- 1
    cur_n <- length(fmt_state$silac_nreps)
    if (cur_n < ng) {
      for (k in (cur_n + 1):ng) {
        fmt_state$silac_nreps[[k]] <- 1L
        fmt_state$silac_pairs[[k]] <- list()
      }
    } else if (cur_n > ng) {
      fmt_state$silac_nreps <- fmt_state$silac_nreps[1:ng]
      fmt_state$silac_pairs <- fmt_state$silac_pairs[1:ng]
    }
  }

  # Group bin UI (right panel) with column rename inputs
  output$fmt_group_bins_ui <- renderUI({
    df <- fmt_raw()
    if (is.null(df)) return(tags$p("Upload a file, then configure groups."))

    ng <- input$fmt_ngroups %||% 2
    ensure_group_slots(ng)

    is_silac <- isTRUE(input$fmt_is_silac)
    has_colour   <- requireNamespace("colourpicker", quietly = TRUE)
    has_sortable <- requireNamespace("sortable", quietly = TRUE)

    if (is_silac) {
      isolate(ensure_silac_slots(ng))
      all_data_cols <- fmt_data_cols()

      # Collect all already-assigned columns across all groups/replicates
      all_assigned <- character(0)
      for (gi in seq_len(ng)) {
        nreps <- fmt_state$silac_nreps[[gi]] %||% 1L
        for (ri in seq_len(nreps)) {
          h_val <- input[[paste0("fmt_silac_h_", gi, "_", ri)]] %||% ""
          l_val <- input[[paste0("fmt_silac_l_", gi, "_", ri)]] %||% ""
          if (nzchar(h_val)) all_assigned <- c(all_assigned, h_val)
          if (nzchar(l_val)) all_assigned <- c(all_assigned, l_val)
        }
      }

      # Compute summary counts for SILAC preview
      total_reps <- sum(vapply(seq_len(ng), function(gi) fmt_state$silac_nreps[[gi]] %||% 1L, integer(1)))
      total_cols <- total_reps * 2L

      tagList(
        div(
          style = "background: #e8f0fe; border-radius: 8px; padding: 8px 12px; margin-bottom: 12px; font-size: 13px; color: #1a4d8c;",
          sprintf("%d group%s x %d replicate%s = %d H/L pair%s (%d data columns)",
                  ng, if (ng != 1) "s" else "",
                  total_reps, if (total_reps != 1) "s" else "",
                  total_reps, if (total_reps != 1) "s" else "",
                  total_cols)
        ),
        lapply(seq_len(ng), function(i) {
          gname_id <- paste0("fmt_gname_", i)
          gcol_id  <- paste0("fmt_gcol_", i)
          nreps    <- fmt_state$silac_nreps[[i]] %||% 1L

          bslib::card(
            bslib::card_header(
              div(style = "display:flex; gap:12px; align-items:center; justify-content:space-between;",
                  div(style = "flex:1;",
                      textInput(gname_id, label = NULL,
                                value = isolate(input[[gname_id]]) %||% paste0("Sample group ", i),
                                width = "100%")
                  ),
                  div(style = "width: 180px;",
                      if (has_colour) {
                        colourpicker::colourInput(gcol_id, label = NULL,
                                                  value = isolate(input[[gcol_id]]) %||% "#E03A3E",
                                                  showColour = "both", width = "100%")
                      } else {
                        textInput(gcol_id, label = NULL,
                                  value = isolate(input[[gcol_id]]) %||% "#E03A3E",
                                  width = "100%")
                      }
                  )
              )
            ),
            bslib::card_body(
              tagList(
                tags$p(tags$strong("Pair Heavy / Light columns per replicate:")),
                lapply(seq_len(nreps), function(r) {
                  h_id <- paste0("fmt_silac_h_", i, "_", r)
                  l_id <- paste0("fmt_silac_l_", i, "_", r)

                  # Build choices: available columns + currently selected value (so it stays selected)
                  cur_h <- isolate(input[[h_id]] %||% "")
                  cur_l <- isolate(input[[l_id]] %||% "")
                  # Columns already used in other slots (not this one)
                  other_assigned <- setdiff(all_assigned, c(cur_h, cur_l))
                  avail <- setdiff(all_data_cols, other_assigned)

                  div(
                    class = "silac-pair-row",
                    tags$span(style = "min-width:50px; font-weight:600;", paste0("Rep ", r, ":")),
                    div(class = "silac-dd silac-dd-heavy",
                        selectInput(h_id, label = NULL,
                                    choices = c("(Heavy)" = "", sort(avail)),
                                    selected = cur_h, width = "100%")
                    ),
                    div(class = "silac-dd silac-dd-light",
                        selectInput(l_id, label = NULL,
                                    choices = c("(Light)" = "", sort(avail)),
                                    selected = cur_l, width = "100%")
                    )
                  )
                }),
                div(
                  style = "display:flex; gap:8px; margin-top:4px;",
                  actionButton(paste0("fmt_silac_add_rep_", i), "+ Rep",
                               class = "btn-sm btn-outline-secondary"),
                  if (nreps > 1) {
                    actionButton(paste0("fmt_silac_rm_rep_", i), "- Rep",
                                 class = "btn-sm btn-outline-secondary")
                  } else NULL
                )
              )
            )
          )
        })
      )
    } else {
      # Standard (non-SILAC) column assignment UI
      tagList(
        lapply(seq_len(ng), function(i) {
          gname_id  <- paste0("fmt_gname_", i)
          gcol_id   <- paste0("fmt_gcol_", i)
          choose_id <- paste0("fmt_choose_cols_", i)

          ordered_cols <- get_ordered_cols(i)

          bslib::card(
            bslib::card_header(
              div(style = "display:flex; gap:12px; align-items:center; justify-content:space-between;",
                  div(style = "flex:1;",
                      textInput(gname_id, label = NULL, value = isolate(input[[gname_id]]) %||% paste0("Sample group ", i), width = "100%")
                  ),
                  div(style = "width: 180px;",
                      if (has_colour) {
                        colourpicker::colourInput(gcol_id, label = NULL, value = isolate(input[[gcol_id]]) %||% "#E03A3E", showColour = "both", width = "100%")
                      } else {
                        textInput(gcol_id, label = NULL, value = isolate(input[[gcol_id]]) %||% "#E03A3E", width = "100%")
                      }
                  ),
                  div(style = "width: 160px;",
                      actionButton(choose_id, "Select columns", width = "100%")
                  )
              )
            ),
            bslib::card_body(
              tagList(
                if (!has_sortable) {
                  tagList(
                    tags$p(tags$strong("Reorder requires package 'sortable'.")),
                    tags$p("Selected columns:"),
                    tags$pre(paste(ordered_cols, collapse = "\n"))
                  )
                } else if (length(ordered_cols) == 0) {
                  tags$p("No columns selected yet. Click 'Select columns'.")
                } else {
                  tagList(
                    tags$p(tags$strong("Drag to reorder (top = replicate 1):")),
                    sortable::rank_list(
                      text = NULL,
                      labels = ordered_cols,
                      input_id = paste0("fmt_rank_", i)
                    )
                  )
                },

                if (length(ordered_cols) > 0) {
                  tagList(
                    tags$hr(),
                    tags$p(tags$small("Rename output headers in the table below (Rename output columns)."))
                  )
                } else NULL
              )
            )
          )
        })
      )
    }
  })
  
  # Column selection modals (unique assignment across groups)
  # Registered once at startup; guard clauses prevent stale or invalid actions.
  for (i in seq_len(silac_max_groups)) {
    local({
      ii <- i
      choose_id <- paste0("fmt_choose_cols_", ii)
      modal_sel <- paste0("fmt_modal_cols_", ii)

      observeEvent(input[[choose_id]], {
        df <- fmt_raw()
        if (is.null(df)) return()
        ng <- input$fmt_ngroups %||% 2
        if (ii > ng) return()
        ensure_group_slots(ng)

        all_data_cols <- fmt_data_cols()

        current <- fmt_state$cols[[ii]] %||% character(0)
        others  <- if (ii <= length(fmt_state$cols)) {
          unlist(fmt_state$cols[-ii], use.names = FALSE)
        } else character(0)
        others  <- setdiff(others, current)

        choices <- sort(unique(c(current, setdiff(all_data_cols, others))))

        showModal(modalDialog(
          title = paste0("Select columns for Sample group ", ii),
          checkboxGroupInput(modal_sel, NULL, choices = choices, selected = current),
          tags$small("Selections will be applied when you close this dialog."),
          footer = modalButton("Done"),
          size = "m",
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
    })
  }

  # Handle modal dismiss - apply column selections when modal closes
  observeEvent(input$fmt_modal_dismissed, {
    dismissed <- input$fmt_modal_dismissed
    if (is.null(dismissed) || is.null(dismissed$group)) return()

    ii <- as.integer(dismissed$group)
    if (is.na(ii) || ii < 1) return()

    modal_sel <- paste0("fmt_modal_cols_", ii)
    selected <- input[[modal_sel]] %||% character(0)

    # Only update if we have a valid selection input
    if (!is.null(input[[modal_sel]])) {
      fmt_clear_built()

      # Remove selected from other groups
      for (j in seq_along(fmt_state$cols)) {
        if (j == ii) next
        fmt_state$cols[[j]] <- setdiff(fmt_state$cols[[j]], selected)
      }

      fmt_state$cols[[ii]] <- selected
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Unassigned columns
  output$fmt_unassigned <- renderText({
    df <- fmt_raw()
    if (is.null(df)) return("")
    is_silac <- isTRUE(input$fmt_is_silac)
    all_data_cols <- fmt_data_cols()

    if (is_silac) {
      # In SILAC mode, "assigned" means selected in any H/L dropdown
      ng <- input$fmt_ngroups %||% 2
      isolate(ensure_silac_slots(ng))
      assigned <- character(0)
      for (gi in seq_len(ng)) {
        nreps <- fmt_state$silac_nreps[[gi]] %||% 1L
        for (ri in seq_len(nreps)) {
          h_val <- input[[paste0("fmt_silac_h_", gi, "_", ri)]] %||% ""
          l_val <- input[[paste0("fmt_silac_l_", gi, "_", ri)]] %||% ""
          if (nzchar(h_val)) assigned <- c(assigned, h_val)
          if (nzchar(l_val)) assigned <- c(assigned, l_val)
        }
      }
    } else {
      assigned <- unlist(fmt_state$cols, use.names = FALSE)
    }

    unassigned <- setdiff(all_data_cols, assigned)
    if (length(unassigned) == 0) return("(none)")
    paste(unassigned, collapse = "\n")
  })

  # SILAC replicate add/remove observers
  # Registered once at startup for up to silac_max_groups groups.
  # Guard clauses inside each handler ensure they only act when valid.
  for (i in seq_len(silac_max_groups)) {
    local({
      ii <- i
      add_id <- paste0("fmt_silac_add_rep_", ii)
      rm_id  <- paste0("fmt_silac_rm_rep_", ii)

      observeEvent(input[[add_id]], {
        if (!isTRUE(input$fmt_is_silac)) return()
        ng <- input$fmt_ngroups %||% 2
        if (ii > ng) return()
        ensure_silac_slots(ng)
        cur <- isolate(fmt_state$silac_nreps[[ii]]) %||% 1L
        fmt_state$silac_nreps[[ii]] <- cur + 1L
        fmt_clear_built()
      }, ignoreInit = TRUE)

      observeEvent(input[[rm_id]], {
        if (!isTRUE(input$fmt_is_silac)) return()
        ng <- input$fmt_ngroups %||% 2
        if (ii > ng) return()
        ensure_silac_slots(ng)
        cur <- isolate(fmt_state$silac_nreps[[ii]]) %||% 1L
        if (cur > 1L) {
          fmt_state$silac_nreps[[ii]] <- cur - 1L
          fmt_clear_built()
        }
      }, ignoreInit = TRUE)
    })
  }

  # Invalidate build when any SILAC dropdown changes
  observe({
    if (!isTRUE(input$fmt_is_silac)) return()
    ng <- input$fmt_ngroups %||% 2
    isolate(ensure_silac_slots(ng))
    for (gi in seq_len(ng)) {
      nreps <- isolate(fmt_state$silac_nreps[[gi]]) %||% 1L
      for (ri in seq_len(nreps)) {
        input[[paste0("fmt_silac_h_", gi, "_", ri)]]
        input[[paste0("fmt_silac_l_", gi, "_", ri)]]
      }
    }
    fmt_clear_built()
  })

  # =========================================================
  # NEW TAB: Rename output columns (DT table)
  # =========================================================
  
  fmt_assignment_df <- reactive({
    df <- fmt_raw()
    if (is.null(df)) return(data.frame())
    
    ng <- input$fmt_ngroups %||% 2
    ensure_group_slots(ng)
    
    out <- list()
    for (i in seq_len(ng)) {
      ordered <- get_ordered_cols(i)
      if (length(ordered) == 0) next
      
      gname <- input[[paste0("fmt_gname_", i)]] %||% paste0("Sample group ", i)
      
      out[[length(out) + 1]] <- data.frame(
        group_index = i,
        group_name  = as.character(gname),
        replicate   = seq_along(ordered),
        source_col  = as.character(ordered),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    
    if (length(out) == 0) return(data.frame())
    do.call(rbind, out)
  })
  
  # Ensure default rename entries exist for assigned cols
  observe({
    a <- fmt_assignment_df()
    if (is.null(a) || nrow(a) == 0) return()
    
    rn <- fmt_state$rename
    miss <- setdiff(a$source_col, names(rn))
    if (length(miss) > 0) {
      fmt_state$rename <- c(rn, stats::setNames(miss, miss))
    }
  })
  
  output$fmt_rename_ui <- renderUI({
    a <- fmt_assignment_df()
    if (is.null(a) || nrow(a) == 0) return(tags$p("Assign columns to groups to enable renaming."))
    if (!requireNamespace("DT", quietly = TRUE)) {
      return(tags$p("Install package 'DT' to use the rename table (install.packages('DT'))."))
    }
    DT::DTOutput("fmt_rename_dt")
  })
  
  output$fmt_rename_dt <- DT::renderDT({
    a <- fmt_assignment_df()
    req(!is.null(a), nrow(a) > 0)
    
    rn <- fmt_state$rename
    out_col <- unname(rn[a$source_col])
    out_col[is.na(out_col) | !nzchar(trimws(as.character(out_col)))] <- a$source_col
    
    tab <- data.frame(
      Group     = a$group_name,
      Replicate = a$replicate,
      Source    = a$source_col,
      Output    = out_col,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    DT::datatable(
      tab,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
      options  = list(pageLength = 12, scrollX = TRUE, ordering = FALSE)
    )
  }, server = FALSE)
  
  observeEvent(input$fmt_rename_dt_cell_edit, {
    info <- input$fmt_rename_dt_cell_edit
    if (is.null(info$col) || info$col != 3) return()  # Output col (0-based)
    
    a <- fmt_assignment_df()
    req(!is.null(a), nrow(a) >= info$row)
    
    src <- a$source_col[[info$row]]
    nm  <- trimws(as.character(info$value))
    if (!nzchar(nm)) nm <- src
    
    rn <- fmt_state$rename
    rn[[src]] <- nm
    fmt_state$rename <- rn
    
    fmt_clear_built()
  }, ignoreInit = TRUE)
  
  fmt_rename_validity <- reactive({
    a <- fmt_assignment_df()
    if (is.null(a) || nrow(a) == 0) return(list(ok = TRUE, errors = character(0)))
    
    rn <- fmt_state$rename
    out <- unname(rn[a$source_col])
    out[is.na(out) | !nzchar(trimws(as.character(out)))] <- a$source_col
    
    errs <- character(0)
    if (any(!nzchar(trimws(out)))) errs <- c(errs, "Some output names are empty.")
    if (anyDuplicated(out) > 0) errs <- c(errs, "Output column names must be unique (duplicates exist).")
    
    ids <- fmt_id_cols()
    if (length(ids) > 0 && any(out %in% ids)) {
      errs <- c(errs, "Some output names collide with identifier column names.")
    }
    
    list(ok = length(errs) == 0, errors = errs)
  })
  
  output$fmt_rename_validity_ui <- renderUI({
    v <- fmt_rename_validity()
    if (is.null(v) || isTRUE(v$ok)) return(NULL)
    
    bslib::card(
      bslib::card_body(
        tags$div(style = "font-weight:700; color:var(--status-error-text);", "Rename issues"),
        tags$ul(lapply(v$errors, tags$li))
      )
    )
  })
  
  # Build design + renamed data for NEW
  build_format_design <- function() {
    df <- fmt_raw()
    req(!is.null(df))

    data_type <- tolower(trimws(input$fmt_data_type %||% "proteomics"))
    if (!data_type %in% c("proteomics", "metabolomics")) data_type <- "proteomics"

    # For metabolomics, fix analysis_level to "metabolite"
    if (data_type == "metabolomics") {
      analysis_level <- "metabolite"
      id_metabolite_name <- trimws(as.character(input$fmt_metabolite_name_col %||% ""))
      id_metabolite <- trimws(as.character(input$fmt_metabolite_col %||% ""))
      req(nzchar(id_metabolite_name))
      id_primary_type <- "Metabolite Name"
      id_primary_col <- id_metabolite_name
    } else {
      # Proteomics: existing logic
      analysis_level <- tolower(trimws(input$fmt_analysis_level %||% "protein"))
      if (!analysis_level %in% c("protein", "peptide")) analysis_level <- "protein"

      id_gene <- trimws(as.character(input$fmt_gene_col %||% ""))
      id_prot <- input$fmt_protein_col %||% ""
      id_pep <- input$fmt_peptide_col %||% ""

      req(nzchar(id_prot))
      if (identical(analysis_level, "peptide")) req(nzchar(id_pep))
      if (!nzchar(id_gene)) stop("Gene symbol column is required.")

      id_primary_type <- if (identical(analysis_level, "peptide")) "Peptide ID" else "Protein ID"
      id_primary_col  <- if (identical(analysis_level, "peptide")) id_pep else id_prot
    }
    
    ng <- input$fmt_ngroups %||% 2
    ensure_group_slots(ng)

    # Get control group index (empty string means "none")
    control_idx <- input$fmt_control_group %||% ""
    control_idx <- if (nzchar(control_idx)) as.integer(control_idx) else NA_integer_

    groups <- data.frame(
      group_id   = paste0("group_", seq_len(ng)),
      group_name = vapply(seq_len(ng), function(i) input[[paste0("fmt_gname_", i)]] %||% paste0("Sample group ", i), character(1)),
      color      = vapply(seq_len(ng), function(i) input[[paste0("fmt_gcol_", i)]] %||% "#E03A3E", character(1)),
      is_control = seq_len(ng) == control_idx,
      stringsAsFactors = FALSE
    )

    # Validate group names (user-facing) with shared regex helper.
    ok_names <- msterp_is_valid_group_name(groups$group_name)
    if (any(!ok_names)) {
      bad <- unique(trimws(as.character(groups$group_name[!ok_names])))
      bad <- bad[nzchar(bad)]
      stop(sprintf(
        "Invalid group name(s): %s\nAllowed pattern: %s",
        paste(bad, collapse = ", "),
        msterp_group_name_regex()
      ))
    }
    
    # Build column records + rename mapping
    is_silac <- isTRUE(input$fmt_is_silac) && data_type == "proteomics"
    col_rows <- list()
    rename_from <- character(0)
    rename_to   <- character(0)

    if (is_silac) {
      # SILAC mode: build columns from H/L pair dropdowns
      ensure_silac_slots(ng)

      for (i in seq_len(ng)) {
        nreps <- fmt_state$silac_nreps[[i]] %||% 1L
        for (r in seq_len(nreps)) {
          h_col <- input[[paste0("fmt_silac_h_", i, "_", r)]] %||% ""
          l_col <- input[[paste0("fmt_silac_l_", i, "_", r)]] %||% ""

          if (!nzchar(h_col) || !nzchar(l_col)) {
            stop(sprintf("Group '%s' Rep %d: both Heavy and Light columns must be selected.",
                         groups$group_name[[i]], r))
          }

          # Two column records per replicate: one heavy, one light
          col_rows[[length(col_rows) + 1]] <- data.frame(
            data_col     = c(h_col, l_col),
            display_name = c(h_col, l_col),
            group_id     = groups$group_id[[i]],
            group_name   = groups$group_name[[i]],
            replicate    = c(as.integer(r), as.integer(r)),
            include      = TRUE,
            source_file  = NA_character_,
            source_col   = c(h_col, l_col),
            channel      = c("heavy", "light"),
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      # Standard mode: build columns from sortable rank lists
      for (i in seq_len(ng)) {
        ordered <- get_ordered_cols(i)
        if (length(ordered) == 0) next

        # Validate all ordered columns exist in the current raw data
        valid_raw_cols <- names(df)
        invalid <- setdiff(ordered, valid_raw_cols)
        if (length(invalid) > 0) {
          stop(sprintf("Columns no longer present in data: %s. Please re-select columns for group %d.",
                       paste(invalid, collapse = ", "), i))
        }

        rn <- fmt_state$rename

        out_names <- vapply(ordered, function(col) {
          nm <- rn[[col]] %||% col
          nm <- trimws(as.character(nm))
          if (!nzchar(nm)) nm <- col
          nm
        }, character(1))


        # checks
        if (anyDuplicated(out_names) > 0) stop("Within a group, output column names must be unique.")
        if (any(out_names %in% fmt_id_cols())) stop("Output column names cannot collide with identifier column names.")

        rename_from <- c(rename_from, ordered)
        rename_to   <- c(rename_to, out_names)

        col_rows[[length(col_rows) + 1]] <- data.frame(
          data_col     = out_names,          # output header
          display_name = out_names,
          group_id     = groups$group_id[[i]],
          group_name   = groups$group_name[[i]],
          replicate    = seq_along(out_names),
          include      = TRUE,
          source_file  = NA_character_,
          source_col   = ordered,            # original header in raw file
          stringsAsFactors = FALSE
        )
      }
    }

    col_df <- do.call(rbind, col_rows)
    if (is.null(col_df) || nrow(col_df) == 0) stop("No data columns assigned to any sample group.")

    # global checks across ALL groups
    if (anyDuplicated(col_df$data_col) > 0) stop("Output column names must be unique across all groups.")
    final_cols <- c(fmt_id_cols(), col_df$data_col)
    if (anyDuplicated(final_cols) > 0) stop("Final workbook would have duplicated column names. Adjust output names and retry.")
    
    meta <- list(
      schema_version   = "1.0",
      created_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      data_type        = data_type,
      analysis_level   = analysis_level,
      id_primary_type  = id_primary_type,
      id_primary_col   = id_primary_col
    )

    # Add SILAC flag if enabled
    if (is_silac) {
      meta$is_silac <- "TRUE"
    }

    # Add type-specific fields
    if (data_type == "metabolomics") {
      meta$id_metabolite_name_col <- id_metabolite_name
      meta$id_metabolite_col <- if (nzchar(id_metabolite)) id_metabolite else ""
    } else {
      meta$id_gene_col <- id_gene
      meta$id_protein_col <- id_prot
      meta$id_peptide_col <- if (identical(analysis_level, "peptide")) id_pep else ""
    }
    
    design_df <- msterp_build_design_sheet(meta, groups, col_df)
    
    # Apply renaming to data sheet
    df2 <- df
    rvec <- setNames(rename_to, rename_from)
    names(df2) <- ifelse(names(df2) %in% names(rvec), rvec[names(df2)], names(df2))
    
    list(data = df2, design = design_df)
  }
  
  output$fmt_status <- renderText("")

  fmt_build_validity <- reactive({
    df <- fmt_raw()
    if (is.null(df)) return(list(ok = FALSE, errors = c("Upload a file to continue.")))

    errs <- character(0)
    data_type <- tolower(trimws(input$fmt_data_type %||% "proteomics"))
    if (!data_type %in% c("proteomics", "metabolomics")) data_type <- "proteomics"

    if (data_type == "metabolomics") {
      id_metabolite_name <- trimws(as.character(input$fmt_metabolite_name_col %||% ""))
      if (!nzchar(id_metabolite_name)) errs <- c(errs, "Metabolite name column is required.")
    } else {
      # Proteomics validation
      analysis_level <- tolower(trimws(input$fmt_analysis_level %||% "protein"))
      if (!analysis_level %in% c("protein", "peptide")) analysis_level <- "protein"

      id_prot <- trimws(as.character(input$fmt_protein_col %||% ""))
      id_gene <- trimws(as.character(input$fmt_gene_col %||% ""))
      id_pep <- trimws(as.character(input$fmt_peptide_col %||% ""))

      if (!nzchar(id_prot)) errs <- c(errs, "Protein ID column is required.")
      if (analysis_level == "peptide" && !nzchar(id_pep)) errs <- c(errs, "Peptide ID column is required.")
      if (!nzchar(id_gene)) errs <- c(errs, "Gene symbol column is required.")

      # SILAC validation
      if (isTRUE(input$fmt_is_silac)) {
        ng <- input$fmt_ngroups %||% 2
        isolate(ensure_silac_slots(ng))
        all_silac_cols <- character(0)
        for (gi in seq_len(ng)) {
          nreps <- fmt_state$silac_nreps[[gi]] %||% 1L
          for (ri in seq_len(nreps)) {
            h_val <- input[[paste0("fmt_silac_h_", gi, "_", ri)]] %||% ""
            l_val <- input[[paste0("fmt_silac_l_", gi, "_", ri)]] %||% ""
            if (!nzchar(h_val) || !nzchar(l_val)) {
              gname <- input[[paste0("fmt_gname_", gi)]] %||% paste0("Group ", gi)
              errs <- c(errs, sprintf("%s Rep %d: select both Heavy and Light columns.", gname, ri))
            } else if (identical(h_val, l_val)) {
              gname <- input[[paste0("fmt_gname_", gi)]] %||% paste0("Group ", gi)
              errs <- c(errs, sprintf("%s Rep %d: Heavy and Light cannot be the same column.", gname, ri))
            }
            if (nzchar(h_val)) {
              if (h_val %in% all_silac_cols) {
                errs <- c(errs, sprintf("Column '%s' is assigned to multiple slots.", h_val))
              }
              all_silac_cols <- c(all_silac_cols, h_val)
            }
            if (nzchar(l_val)) {
              if (l_val %in% all_silac_cols) {
                errs <- c(errs, sprintf("Column '%s' is assigned to multiple slots.", l_val))
              }
              all_silac_cols <- c(all_silac_cols, l_val)
            }
          }
        }
      }
    }

    # Validate group names
    ng <- input$fmt_ngroups %||% 2
    group_names <- vapply(seq_len(ng), function(i) {
      input[[paste0("fmt_gname_", i)]] %||% paste0("Sample group ", i)
    }, character(1))
    ok_names <- msterp_is_valid_group_name(group_names)
    if (any(!ok_names)) {
      bad <- group_names[!ok_names]
      errs <- c(errs, sprintf(
        "Invalid group name(s): %s. Names must start with a letter and contain only letters, numbers, spaces, underscores, dots, or dashes.",
        paste(bad, collapse = ", ")
      ))
    }

    # Check that each group has at least one data column assigned (non-SILAC mode)
    is_silac <- isTRUE(input$fmt_is_silac) && data_type == "proteomics"
    if (!is_silac) {
      for (i in seq_len(ng)) {
        ordered <- get_ordered_cols(i)
        if (length(ordered) == 0) {
          gname <- input[[paste0("fmt_gname_", i)]] %||% paste0("Sample group ", i)
          errs <- c(errs, sprintf("'%s' has no data columns selected.", gname))
        }
      }
    }

    list(ok = length(errs) == 0, errors = errs)
  })

  output$fmt_build_ui <- renderUI({
    v <- fmt_build_validity()

    if (isTRUE(v$ok)) {
      return(actionButton("fmt_build", "Build formatted workbook", class = "btn-primary", width = "100%"))
    }

    # Show ALL validation errors, not just the first one
    err_items <- lapply(v$errors, function(e) tags$li(e))
    tagList(
      tags$button(
        type = "button",
        class = "btn btn-primary",
        style = "width:100%; opacity:0.7; cursor:not-allowed;",
        disabled = "disabled",
        "Build formatted workbook"
      ),
      tags$div(
        style = "margin-top: 8px; color: var(--status-error-text); font-weight: 700; font-size: 12px;",
        tags$ul(style = "padding-left: 18px; margin: 4px 0;", err_items)
      )
    )
  })
  
  # Build button -> writes temp workbook, gates download UI
  observeEvent(input$fmt_build, {
    df <- fmt_raw()
    if (is.null(df)) return()
    
    busy_step(TRUE, "Building formatted workbook…", 15)
    fmt_clear_built()
    
    tmp <- tempfile(fileext = ".xlsx")
    res <- tryCatch({
      out <- build_format_design()
      busy_step(TRUE, "Writing .xlsx…", 75)
      msterp_write_formatted_xlsx(tmp, out$data, out$design)
      tmp
    }, error = function(e) e)
    
    busy_step(FALSE)
    
    if (inherits(res, "error")) {
      fmt_built_path(NULL)
      output$fmt_status <- renderText(paste("Build error:", res$message))
      return()
    }
    
    fmt_built_path(res)
    output$fmt_status <- renderText("Workbook built. You can download it now.")
  }, ignoreInit = TRUE)
  
  # Post-build UI (only appears once built)
  output$fmt_postbuild_ui <- renderUI({
    req(!is.null(fmt_built_path()))
    tagList(
      hr(),
      textInput("fmt_out_name", "Output file name (no extension)", value = isolate(input$fmt_out_name) %||% "msterp_formatted"),
      downloadButton("fmt_download", "Download formatted .xlsx", class = "btn-primary", width = "100%")
    )
  })
  
  output$fmt_download <- downloadHandler(
    filename = function() paste0(input$fmt_out_name %||% "msterp_formatted", ".xlsx"),
    content = function(file) {
      path <- fmt_built_path()
      req(!is.null(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )
  
  # =========================================================
  # MERGE TAB
  # =========================================================
  merge_preview <- reactiveVal(NULL)
  merge_map     <- reactiveVal(NULL)     # editable mapping (data.frame)
  merge_built_path <- reactiveVal(NULL)
  merge_file_cache <- reactiveVal(list()) # cache uploaded file info across re-renders
  
  merge_clear_built <- function() {
    merge_built_path(NULL)
  }
  
  output$fmt_merge_content <- renderUI({
    has_preview <- !is.null(merge_preview())

    right_panel <- if (has_preview) {
      tags$div(
        class = "fmt-panel fmt-panel-right",
        tags$h4("Status"),
        uiOutput("fmt_merge_summary_ui"),
        verbatimTextOutput("merge_status"),
        hr(),
        tags$h4("Preview"),
        uiOutput("merge_bins_preview"),
        hr(),
        tags$details(
          class = "fmt-advanced",
          tags$summary("Advanced: Rename merged columns"),
          uiOutput("merge_colname_editor_ui")
        ),
        uiOutput("merge_validity_ui"),
        uiOutput("merge_conflicts_ui")
      )
    } else {
      NULL
    }

    tags$div(
      class = paste("fmt-two-panel", if (!has_preview) "single"),
      tags$div(
        class = "fmt-panel",
        tags$h4("Workflow"),
        fmt_step_ui(
          "1",
          "Upload formatted files",
          numericInput("merge_nfiles", "Number of files", value = 2, min = 2, step = 1),
          div(
            style = "margin: 6px 0 0;",
            downloadButton("fmt_sample_merge", "Sample formatted .xlsx", class = "btn btn-default btn-sm")
          ),
          uiOutput("merge_file_inputs"),
          actionButton("merge_load", "Load & preview", class = "btn-primary", width = "100%")
        ),
        if (has_preview) fmt_step_ui(
          "2",
          "Build merge",
          actionButton("merge_build", "Build merged workbook", class = "btn-primary", width = "100%"),
          uiOutput("merge_postbuild_ui"),
          actionButton("merge_reset", "Reset inputs", class = "btn btn-default", width = "100%")
        ) else NULL
      ),
      right_panel
    )
  })

  output$fmt_sample_merge <- downloadHandler(
    filename = function() "msterp_sample_formatted.xlsx",
    content = function(file) {
      fmt_make_sample_formatted(file)
    }
  )

  output$fmt_merge_summary_ui <- renderUI({
    prev <- merge_preview()
    built <- !is.null(merge_built_path())
    if (is.null(prev)) {
      return(div(class = "fmt-summary", strong("Summary"), div("No formatted files loaded yet.")))
    }

    div(
      class = "fmt-summary",
      strong("Summary"),
      div(sprintf("Files: %d | Level: %s", length(prev$file_ids %||% character(0)), prev$level %||% "unknown")),
      div(sprintf("Primary ID: %s", prev$primary_col %||% "unknown")),
      div(if (built) "Merged workbook ready to download." else "Fix mapping issues and build the merged workbook.")
    )
  })
  
  output$merge_file_inputs <- renderUI({
    n <- input$merge_nfiles %||% 2
    tagList(lapply(seq_len(n), function(i) {
      fileInput(paste0("merge_file_", i), paste0("File ", i, " (.xlsx formatted)"), multiple = FALSE, accept = c(".xlsx"))
    }))
  })

  # Cache uploaded file info so paths survive fileInput re-renders
  observe({
    n <- input$merge_nfiles %||% 2
    cache <- isolate(merge_file_cache())
    changed <- FALSE
    for (i in seq_len(n)) {
      fi <- input[[paste0("merge_file_", i)]]
      if (!is.null(fi) && !is.null(fi$datapath) && nzchar(fi$datapath)) {
        key <- as.character(i)
        if (is.null(cache[[key]]) || cache[[key]]$datapath != fi$datapath) {
          cache[[key]] <- list(datapath = fi$datapath, name = fi$name)
          changed <- TRUE
        }
      }
    }
    # Trim cache entries beyond current nfiles
    cache_keys <- as.integer(names(cache))
    excess <- cache_keys[cache_keys > n]
    if (length(excess) > 0) {
      for (k in excess) cache[[as.character(k)]] <- NULL
      changed <- TRUE
    }
    if (changed) merge_file_cache(cache)
  })

  merge_paths <- reactive({
    n <- input$merge_nfiles %||% 2
    cache <- merge_file_cache()
    paths <- character(0)
    labels <- character(0)

    for (i in seq_len(n)) {
      fi <- input[[paste0("merge_file_", i)]]
      if (!is.null(fi) && !is.null(fi$datapath) && nzchar(fi$datapath)) {
        paths  <- c(paths, fi$datapath)
        labels <- c(labels, tools::file_path_sans_ext(fi$name))
      } else {
        # Fall back to cached file info (survives fileInput re-renders)
        ci <- cache[[as.character(i)]]
        if (!is.null(ci)) {
          paths  <- c(paths, ci$datapath)
          labels <- c(labels, tools::file_path_sans_ext(ci$name))
        }
      }
    }
    list(paths = paths, labels = make.names(labels, unique = TRUE))
  })
  
  observeEvent(input$merge_load, {
    merge_clear_built()

    mp <- merge_paths()
    if (length(mp$paths) < 2) {
      merge_preview(NULL)
      merge_map(NULL)
      output$merge_status <- renderText("Upload at least 2 formatted files before loading.")
      return()
    }

    busy_step(TRUE, "Reading workbooks…", 10)
    prev <- tryCatch(msterp_merge_preview(mp$paths, file_ids = mp$labels), error = function(e) e)
    busy_step(FALSE)

    if (inherits(prev, "error")) {
      merge_preview(NULL)
      merge_map(NULL)
      output$merge_status <- renderText(paste("Error:", prev$message))
      return()
    }

    merge_preview(prev)
    merge_map(prev$mapping)

    output$merge_status <- renderText(sprintf(
      "Loaded %d files. Level: %s. Primary ID: %s.",
      length(prev$file_ids), prev$level, prev$primary_col
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$merge_reset, {
    merge_preview(NULL)
    merge_map(NULL)
    merge_clear_built()
    merge_file_cache(list())
    output$merge_status <- renderText("")

    n <- input$merge_nfiles %||% 2
    for (i in seq_len(n)) {
      reset_file_input(paste0("merge_file_", i))
    }
    updateNumericInput(session, "merge_nfiles", value = 2)
  }, ignoreInit = TRUE)
  
  output$merge_postbuild_ui <- renderUI({
    req(!is.null(merge_built_path()))
    tagList(
      hr(),
      textInput("merge_out_name", "Output file name (no extension)", value = isolate(input$merge_out_name) %||% "msterp_merged"),
      downloadButton("merge_download", "Download merged .xlsx", width = "100%")
    )
  })
  
  output$merge_bins_preview <- renderUI({
    prev <- merge_preview()
    m <- merge_map()
    if (is.null(prev) || is.null(m)) return(tags$p("Upload 2+ formatted files."))
    
    fids <- prev$file_ids
    has_colour <- requireNamespace("colourpicker", quietly = TRUE)
    
    tagList(
      lapply(fids, function(fid) {
        mm <- m[m$source_file == fid, , drop = FALSE]
        if (nrow(mm) == 0) return(NULL)
        
        # group cards based on mapping (reflects edits)
        groups <- unique(mm[, c("group_id", "group_name", "color"), drop = FALSE])
        
        cols_by_group <- split(mm$merged_col, mm$group_id)
        
        bslib::card(
          bslib::card_header(tags$strong(fid)),
          bslib::card_body(
            div(
              style = "display:grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 12px;",
              lapply(seq_len(nrow(groups)), function(k) {
                gid <- groups$group_id[[k]]
                id_name <- paste0("merge_gname__", fid, "__", gid)
                id_col  <- paste0("merge_gcol__",  fid, "__", gid)
                
                cols <- cols_by_group[[gid]]
                cols <- cols[!is.na(cols) & nzchar(as.character(cols))]
                
                bslib::card(
                  bslib::card_header(
                    div(style="display:flex; gap:10px; align-items:center; justify-content:space-between;",
                        div(style="flex:1;",
                            textInput(id_name, label = NULL, value = isolate(input[[id_name]]) %||% groups$group_name[[k]], width = "100%")
                        ),
                        div(style="width: 170px;",
                            if (has_colour) {
                              colourpicker::colourInput(id_col, label = NULL, value = isolate(input[[id_col]]) %||% groups$color[[k]], showColour = "both", width = "100%")
                            } else {
                              textInput(id_col, label = NULL, value = isolate(input[[id_col]]) %||% groups$color[[k]], width = "100%")
                            }
                        )
                    )
                  ),
                  bslib::card_body(
                    if (length(cols) == 0) tags$em("No included columns") else tags$ul(lapply(cols, tags$li))
                  )
                )
              })
            )
          )
        )
      })
    )
  })
  
  # Sync group name + color edits into mapping
  observe({
    m <- merge_map()
    prev <- merge_preview()
    req(!is.null(m), !is.null(prev))
    
    inlist <- reactiveValuesToList(input)
    m_new <- m
    
    for (fid in unique(m$source_file)) {
      mm <- m[m$source_file == fid, , drop = FALSE]
      groups <- unique(mm[, c("group_id"), drop = FALSE])
      for (gid in groups$group_id) {
        id_name <- paste0("merge_gname__", fid, "__", gid)
        id_col  <- paste0("merge_gcol__",  fid, "__", gid)
        
        nm <- inlist[[id_name]]
        cl <- inlist[[id_col]]
        
        rows <- which(m_new$source_file == fid & m_new$group_id == gid)
        if (length(rows) == 0) next
        
        if (!is.null(nm) && nzchar(as.character(nm))) m_new$group_name[rows] <- as.character(nm)
        if (!is.null(cl) && nzchar(as.character(cl))) m_new$color[rows]      <- as.character(cl)
      }
    }
    
    # Renumber replicates after group name/color edits
    m_new <- m_new[order(m_new$group_name, m_new$color,
                         m_new$source_file, m_new$replicate), ]
    grp_key <- paste0(m_new$group_name, "\u2016", m_new$color)
    for (key in unique(grp_key)) {
      rows <- which(grp_key == key)
      m_new$replicate[rows] <- seq_along(rows)
    }

    if (!isTRUE(all.equal(m_new, m))) {
      merge_map(m_new)
      merge_clear_built()
    }
  })
  
  output$merge_colname_editor_ui <- renderUI({
    m <- merge_map()
    if (is.null(m) || nrow(m) == 0) return(tags$p("Upload 2+ formatted files."))
    if (!requireNamespace("DT", quietly = TRUE)) {
      return(tags$p("Install package 'DT' to use the rename table (install.packages('DT'))."))
    }
    DT::DTOutput("merge_rename_dt")
  })
  
  output$merge_rename_dt <- DT::renderDT({
    m <- merge_map()
    req(!is.null(m), nrow(m) > 0)
    
    tab <- data.frame(
      File      = m$source_file,
      Source    = m$source_col,
      Group     = m$group_name,
      Replicate = m$replicate,
      Name      = m$merged_col,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    DT::datatable(
      tab,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3))),
      options  = list(pageLength = 12, scrollX = TRUE, ordering = FALSE)
    )
  }, server = FALSE)
  
  observeEvent(input$merge_rename_dt_cell_edit, {
    info <- input$merge_rename_dt_cell_edit
    if (is.null(info$col) || info$col != 4) return()  # Name col (0-based)
    
    m <- merge_map()
    req(!is.null(m), nrow(m) >= info$row)
    
    nm <- trimws(as.character(info$value))
    if (!nzchar(nm)) nm <- m$merged_col[[info$row]]
    
    # single rename -> applies to both data + display
    m$merged_col[[info$row]]   <- nm
    m$display_name[[info$row]] <- nm
    
    merge_map(m)
    merge_clear_built()
  }, ignoreInit = TRUE)
  
  
  
  merge_validity <- reactive({
    m <- merge_map()
    if (is.null(m)) return(list(ok = FALSE, errors = c("No mapping yet.")))
    
    errs <- character(0)
    if (any(is.na(m$merged_col) | !nzchar(as.character(m$merged_col)))) errs <- c(errs, "Some column names are empty.")
    if (any(duplicated(m$merged_col))) errs <- c(errs, "Column names must be unique (duplicates exist).")
    list(ok = length(errs) == 0, errors = errs)
  })
  
  
  output$merge_validity_ui <- renderUI({
    v <- merge_validity()
    if (is.null(v)) return(NULL)
    
    bslib::card(
      bslib::card_body(
        tags$div(style = paste0("font-weight:700; color:", if (isTRUE(v$ok)) "var(--status-success-text)" else "var(--status-error-text)", ";"),
                 if (isTRUE(v$ok)) "Valid mapping" else "Invalid mapping"
        ),
        if (!isTRUE(v$ok)) tags$ul(lapply(v$errors, tags$li)) else NULL
      )
    )
  })
  
  output$merge_conflicts_ui <- renderUI({
    v <- merge_validity()
    m <- merge_map()
    if (is.null(m) || isTRUE(v$ok)) return(NULL)
    
    dup_nm <- duplicated(m$merged_col) | duplicated(m$merged_col, fromLast = TRUE)
    idx <- which(dup_nm)
    
    if (length(idx) == 0) return(NULL)
    
    bslib::card(
      bslib::card_header("Resolve duplicates"),
      bslib::card_body(
        tags$p("Edit only the conflicting items below. Suggested: prefix with file label."),
        tagList(lapply(seq_along(idx), function(t) {
          r <- idx[[t]]
          tagList(
            tags$hr(),
            tags$div(tags$strong(paste0(m$source_file[[r]], " :: ", m$source_col[[r]]))),
            textInput(paste0("merge_fix_nm_", r), "Column name (data + display)", value = m$merged_col[[r]])
          )
        })),
        actionButton("merge_apply_fixes", "Apply fixes", class = "btn-primary")
      )
    )
  })
  
  
  observeEvent(input$merge_apply_fixes, {
    m <- merge_map()
    req(!is.null(m))
    
    for (r in seq_len(nrow(m))) {
      id1 <- paste0("merge_fix_nm_", r)
      if (!is.null(input[[id1]])) {
        nm <- trimws(as.character(input[[id1]]))
        if (nzchar(nm)) {
          m$merged_col[[r]]   <- nm
          m$display_name[[r]] <- nm
        }
      }
    }
    
    merge_map(m)
    merge_clear_built()
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$merge_build, {
    prev <- merge_preview()
    m <- merge_map()
    req(!is.null(prev), !is.null(m))
    
    v <- merge_validity()
    if (!isTRUE(v$ok)) {
      output$merge_status <- renderText("Fix mapping issues (right panel) before building merged workbook.")
      return()
    }
    
    merge_clear_built()
    
    tmp <- tempfile(fileext = ".xlsx")
    
    # Use your merger progress callback (requires msterp_merge_execute(..., progress=...))
    merge_need <- c("source_file","source_col","merged_col","display_name","group_name","replicate","color")
    merge_miss <- setdiff(merge_need, names(m))
    if (length(merge_miss) > 0) {
      output$merge_status <- renderText(paste("Mapping is missing columns:", paste(merge_miss, collapse = ", "),
                                              "\nHas:", paste(names(m), collapse = ", ")))
      return()
    }
    res <- tryCatch({
      busy_step(TRUE, "Merging files…", 10)
      msterp_merge_execute(
        prev,
        m[, merge_need, drop = FALSE],
        tmp,
        progress = function(msg, pct) busy_step(TRUE, msg, pct)
      )
      tmp
    }, error = function(e) e)
    
    busy_step(FALSE)
    
    if (inherits(res, "error")) {
      merge_built_path(NULL)
      output$merge_status <- renderText(paste("Merge error:", res$message))
      return()
    }
    
    merge_built_path(res)
    output$merge_status <- renderText("Merged workbook built. You can download it now.")
  }, ignoreInit = TRUE)
  
  output$merge_download <- downloadHandler(
    filename = function() paste0((input$merge_out_name %||% "msterp_merged"), ".xlsx"),
    content = function(file) {
      path <- merge_built_path()
      req(!is.null(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )
  
  # =========================================================
  # EDIT TAB (replaces Validate): load ONE formatted workbook, edit, rebuild
  # =========================================================
  edit_obj <- reactiveVal(NULL)
  edit_map <- reactiveVal(NULL)
  edit_built_path <- reactiveVal(NULL)
  
  edit_clear_built <- function() edit_built_path(NULL)
  
  output$fmt_edit_content <- renderUI({
    has_obj <- !is.null(edit_obj())

    right_panel <- if (has_obj) {
      tags$div(
        class = "fmt-panel fmt-panel-right",
        tags$h4("Status"),
        uiOutput("fmt_edit_summary_ui"),
        verbatimTextOutput("edit_status"),
        hr(),
        tags$h4("Preview"),
        uiOutput("edit_bins_preview"),
        hr(),
        tags$details(
          class = "fmt-advanced",
          tags$summary("Advanced: Rename columns"),
          uiOutput("edit_colname_editor_ui")
        ),
        uiOutput("edit_validity_ui")
      )
    } else {
      NULL
    }

    tags$div(
      class = paste("fmt-two-panel", if (!has_obj) "single"),
      tags$div(
        class = "fmt-panel",
        tags$h4("Workflow"),
        fmt_step_ui(
          "1",
          "Upload formatted workbook",
          fileInput("edit_file", "Upload formatted MSTerp .xlsx", accept = c(".xlsx")),
          div(
            style = "margin: 6px 0 0;",
            downloadButton("fmt_sample_edit", "Sample formatted .xlsx", class = "btn btn-default btn-sm")
          )
        ),
        if (has_obj) fmt_step_ui(
          "2",
          "Build edited workbook",
          actionButton("edit_build", "Build edited workbook", class = "btn-primary", width = "100%"),
          uiOutput("edit_postbuild_ui"),
          actionButton("edit_reset", "Reset inputs", class = "btn btn-default", width = "100%")
        ) else NULL
      ),
      right_panel
    )
  })

  output$fmt_sample_edit <- downloadHandler(
    filename = function() "msterp_sample_formatted.xlsx",
    content = function(file) {
      fmt_make_sample_formatted(file)
    }
  )

  output$fmt_edit_summary_ui <- renderUI({
    obj <- edit_obj()
    built <- !is.null(edit_built_path())
    if (is.null(obj)) {
      return(div(class = "fmt-summary", strong("Summary"), div("No formatted file loaded yet.")))
    }

    groups <- tryCatch(msterp_extract_groups(obj$design), error = function(e) NULL)
    n_groups <- if (is.data.frame(groups)) nrow(groups) else 0
    div(
      class = "fmt-summary",
      strong("Summary"),
      div(sprintf("Rows: %d | Columns: %d", nrow(obj$data), ncol(obj$data))),
      div(sprintf("Groups: %d", n_groups)),
      div(if (built) "Edited workbook ready to download." else "Review columns, then rebuild.")
    )
  })
  
  observeEvent(input$edit_file, {
    req(input$edit_file$datapath, input$edit_file$name)
    
    edit_clear_built()
    edit_obj(NULL)
    edit_map(NULL)
    
    busy_step(TRUE, "Reading workbook…", 10)
    obj <- tryCatch(msterp_read_formatted_xlsx(input$edit_file$datapath), error = function(e) e)
    busy_step(FALSE)
    
    if (inherits(obj, "error")) {
      output$edit_status <- renderText(paste("Error:", obj$message))
      return()
    }
    
    v <- msterp_validate_formatted(obj)
    if (!isTRUE(v$ok)) {
      output$edit_status <- renderText(
        paste0(
          "This file is NOT a formatted MSTerp workbook.\n",
          "Use the New tab to build from scratch.\n\n",
          paste(v$errors, collapse = "\n")
        )
      )
      return()
    }
    
    meta   <- msterp_extract_meta(obj$design)
    groups <- msterp_extract_groups(obj$design)
    cols   <- msterp_extract_columns(obj$design)
    
    fid <- make.names(tools::file_path_sans_ext(input$edit_file$name), unique = TRUE)
    if (length(fid) == 0 || is.na(fid) || !nzchar(fid)) fid <- "uploaded_file"
    
    if (is.null(cols) || !is.data.frame(cols) || nrow(cols) == 0) {
      output$edit_status <- renderText(
        paste0(
          "Loaded workbook, but no included column records were found in the design sheet.\n",
          "This usually means the 'design' sheet is missing/invalid 'column' rows or 'include' is not set.\n",
          "Try rebuilding the workbook in the New tab."
        )
      )
      return()
    }
    
    # ---- Ensure required fields exist (defensive) ----
    if (!"data_col" %in% names(cols)) stop("Design sheet is missing required column field: data_col")
    if (!"display_name" %in% names(cols)) cols$display_name <- cols$data_col
    if (!"group_id" %in% names(cols)) cols$group_id <- NA_character_
    if (!"group_name" %in% names(cols)) cols$group_name <- NA_character_
    if (!"replicate" %in% names(cols)) cols$replicate <- NA_integer_
    if (!"color" %in% names(cols)) cols$color <- NA_character_
    
    n <- nrow(cols)
    
    source_col <- as.character(cols$data_col)
    if (length(source_col) != n) stop("Internal error: data_col length mismatch.")
    
    display_nm <- as.character(cols$display_name)
    if (length(display_nm) != n) display_nm <- rep_len(NA_character_, n)
    display_nm[is.na(display_nm) | !nzchar(display_nm)] <- source_col[is.na(display_nm) | !nzchar(display_nm)]
    
    group_id   <- as.character(cols$group_id);   if (length(group_id)   != n) group_id   <- rep_len(NA_character_, n)
    group_name <- as.character(cols$group_name); if (length(group_name) != n) group_name <- rep_len(NA_character_, n)
    
    rep_vec <- suppressWarnings(as.integer(cols$replicate))
    if (length(rep_vec) != n) rep_vec <- rep_len(NA_integer_, n)
    
    # ---- Fill group_name/color from group records via lookup (no merge()) ----
    # Keep existing per-column values if present; only fill blanks.
    if (!is.null(groups) && is.data.frame(groups) && nrow(groups) > 0 && "group_id" %in% names(groups)) {
      if ("group_name" %in% names(groups)) {
        gname_map <- stats::setNames(as.character(groups$group_name), as.character(groups$group_id))
        fill_name <- unname(gname_map[group_id])
        need <- is.na(group_name) | !nzchar(group_name)
        group_name[need] <- fill_name[need]
      }
      
      if ("color" %in% names(groups)) {
        gcol_map <- stats::setNames(as.character(groups$color), as.character(groups$group_id))
        fill_col <- unname(gcol_map[group_id])
        
        col_existing <- as.character(cols$color)
        if (length(col_existing) != n) col_existing <- rep_len(NA_character_, n)
        needc <- is.na(col_existing) | !nzchar(col_existing)
        col_existing[needc] <- fill_col[needc]
        color <- col_existing
      } else {
        color <- as.character(cols$color)
        if (length(color) != n) color <- rep_len(NA_character_, n)
      }
    } else {
      color <- as.character(cols$color)
      if (length(color) != n) color <- rep_len(NA_character_, n)
    }
    
    mapping <- data.frame(
      source_file  = rep_len(fid, n),
      source_col   = source_col,
      merged_col   = source_col,
      display_name = display_nm,
      group_id     = group_id,
      group_name   = group_name,
      replicate    = rep_vec,
      color        = color,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    edit_obj(obj)
    edit_map(mapping)
    
    output$edit_status <- renderText(
      paste(
        "Loaded formatted workbook.",
        paste0("analysis_level: ", meta$analysis_level),
        paste0("primary_id_col: ", meta$id_primary_col),
        paste0("included data columns: ", nrow(mapping)),
        sep = "\n"
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$edit_reset, {
    edit_obj(NULL)
    edit_map(NULL)
    edit_clear_built()
    output$edit_status <- renderText("")
    reset_file_input("edit_file")
  }, ignoreInit = TRUE)
  
  output$edit_postbuild_ui <- renderUI({
    req(!is.null(edit_built_path()))
    tagList(
      hr(),
      textInput("edit_out_name", "Output file name (no extension)", value = isolate(input$edit_out_name) %||% "msterp_edited"),
      downloadButton("edit_download", "Download edited .xlsx", width = "100%")
    )
  })
  
  output$edit_bins_preview <- renderUI({
    m <- edit_map()
    if (is.null(m) || nrow(m) == 0) return(tags$p("Upload a formatted workbook to edit."))
    
    has_colour <- requireNamespace("colourpicker", quietly = TRUE)
    
    groups <- unique(m[, c("group_id","group_name","color"), drop = FALSE])
    cols_by_group <- split(m$merged_col, m$group_id)
    
    div(
      style = "display:grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 12px;",
      lapply(seq_len(nrow(groups)), function(k) {
        gid <- groups$group_id[[k]]
        id_name <- paste0("edit_gname__", gid)
        id_col  <- paste0("edit_gcol__",  gid)
        
        cols <- cols_by_group[[gid]]
        cols <- cols[!is.na(cols) & nzchar(as.character(cols))]
        
        bslib::card(
          bslib::card_header(
            div(style="display:flex; gap:10px; align-items:center; justify-content:space-between;",
                div(style="flex:1;",
                    textInput(id_name, label = NULL, value = isolate(input[[id_name]]) %||% groups$group_name[[k]], width = "100%")
                ),
                div(style="width: 170px;",
                    if (has_colour) {
                      colourpicker::colourInput(id_col, label = NULL, value = isolate(input[[id_col]]) %||% groups$color[[k]], showColour = "both", width = "100%")
                    } else {
                      textInput(id_col, label = NULL, value = isolate(input[[id_col]]) %||% groups$color[[k]], width = "100%")
                    }
                )
            )
          ),
          bslib::card_body(
            if (length(cols) == 0) tags$em("No included columns") else tags$ul(lapply(cols, tags$li))
          )
        )
      })
    )
  })
  
  # Sync group name/color edits into edit_map
  observe({
    m <- edit_map()
    req(!is.null(m), nrow(m) > 0)
    
    inlist <- reactiveValuesToList(input)
    m_new <- m
    
    groups <- unique(m_new$group_id)
    for (gid in groups) {
      id_name <- paste0("edit_gname__", gid)
      id_col  <- paste0("edit_gcol__",  gid)
      
      nm <- inlist[[id_name]]
      cl <- inlist[[id_col]]
      
      rows <- which(m_new$group_id == gid)
      if (length(rows) == 0) next
      
      if (!is.null(nm) && nzchar(as.character(nm))) m_new$group_name[rows] <- as.character(nm)
      if (!is.null(cl) && nzchar(as.character(cl))) m_new$color[rows]      <- as.character(cl)
    }
    
    if (!isTRUE(all.equal(m_new, m))) {
      edit_map(m_new)
      edit_clear_built()
    }
  })
  
  output$edit_colname_editor_ui <- renderUI({
    m <- edit_map()
    if (is.null(m) || nrow(m) == 0) return(tags$p("Load a workbook to rename columns."))
    if (!requireNamespace("DT", quietly = TRUE)) {
      return(tags$p("Install package 'DT' to use the rename table (install.packages('DT'))."))
    }
    DT::DTOutput("edit_rename_dt")
  })
  
  output$edit_rename_dt <- DT::renderDT({
    m <- edit_map()
    req(!is.null(m), nrow(m) > 0)
    
    tab <- data.frame(
      Source    = m$source_col,
      Name      = m$merged_col,
      Group     = m$group_name,
      Replicate = m$replicate,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    DT::datatable(
      tab,
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 2, 3))),
      options  = list(pageLength = 12, scrollX = TRUE, ordering = FALSE)
    )
  }, server = FALSE)
  
  observeEvent(input$edit_rename_dt_cell_edit, {
    info <- input$edit_rename_dt_cell_edit
    if (is.null(info$col) || info$col != 1) return()  # Name col (0-based)
    
    m <- edit_map()
    req(!is.null(m), nrow(m) >= info$row)
    
    nm <- trimws(as.character(info$value))
    if (!nzchar(nm)) nm <- m$merged_col[[info$row]]
    
    # single rename -> applies to both data + display
    m$merged_col[[info$row]]   <- nm
    m$display_name[[info$row]] <- nm
    
    edit_map(m)
    edit_clear_built()
  }, ignoreInit = TRUE)
  
  edit_validity <- reactive({
    m <- edit_map()
    if (is.null(m)) return(list(ok = FALSE, errors = c("No workbook loaded.")))
    
    errs <- character(0)
    if (any(is.na(m$merged_col) | !nzchar(as.character(m$merged_col)))) errs <- c(errs, "Some column names are empty.")
    if (any(duplicated(m$merged_col))) errs <- c(errs, "Column names must be unique (duplicates exist).")
    list(ok = length(errs) == 0, errors = errs)
  })
  
  
  output$edit_validity_ui <- renderUI({
    v <- edit_validity()
    if (is.null(v)) return(NULL)
    
    bslib::card(
      bslib::card_body(
        tags$div(style = paste0("font-weight:700; color:", if (isTRUE(v$ok)) "var(--status-success-text)" else "var(--status-error-text)", ";"),
                 if (isTRUE(v$ok)) "Valid edits" else "Invalid edits"
        ),
        if (!isTRUE(v$ok)) tags$ul(lapply(v$errors, tags$li)) else NULL
      )
    )
  })
  
  observeEvent(input$edit_build, {
    obj <- edit_obj()
    m <- edit_map()
    req(!is.null(obj), !is.null(m), nrow(m) > 0)
    
    v <- edit_validity()
    if (!isTRUE(v$ok)) {
      output$edit_status <- renderText("Fix edit issues (duplicates / empty names) before building.")
      return()
    }
    
    edit_clear_built()
    
    busy_step(TRUE, "Building edited workbook…", 15)
    
    tmp <- tempfile(fileext = ".xlsx")
    res <- tryCatch({
      # Rename data columns
      df <- obj$data
      ren <- setNames(m$merged_col, m$source_col)
      if (!all(names(ren) %in% names(df))) {
        miss <- names(ren)[!names(ren) %in% names(df)]
        stop("Uploaded data is missing expected columns: ", paste(miss, collapse = ", "))
      }
      names(df) <- ifelse(names(df) %in% names(ren), ren[names(df)], names(df))
      
      # Rebuild design sheet
      meta <- msterp_extract_meta(obj$design)
      meta$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      groups <- unique(m[, c("group_id","group_name","color"), drop = FALSE])
      col_recs <- data.frame(
        data_col     = m$merged_col,
        display_name = m$merged_col,
        group_id     = m$group_id,
        group_name   = m$group_name,
        replicate    = as.integer(m$replicate),
        include      = TRUE,
        source_file  = m$source_file %||% NA_character_,
        source_col   = m$source_col,
        stringsAsFactors = FALSE
      )
      
      design_df <- msterp_build_design_sheet(meta, groups, col_recs)
      
      busy_step(TRUE, "Writing .xlsx…", 80)
      msterp_write_formatted_xlsx(tmp, df, design_df)
      tmp
    }, error = function(e) e)
    
    busy_step(FALSE)
    
    if (inherits(res, "error")) {
      edit_built_path(NULL)
      output$edit_status <- renderText(paste("Build error:", res$message))
      return()
    }
    
    edit_built_path(res)
    output$edit_status <- renderText("Edited workbook built. You can download it now.")
  }, ignoreInit = TRUE)
  
  output$edit_download <- downloadHandler(
    filename = function() paste0((input$edit_out_name %||% "msterp_edited"), ".xlsx"),
    content = function(file) {
      path <- edit_built_path()
      req(!is.null(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )

  # =========================================================
  # COMPARE TAB: Link two formatted workbooks for cross-dataset analysis
  # =========================================================

  # Compare tab state
  fmt_compare_a <- reactiveVal(NULL)  # list(path, name, data, design, metadata, groups, proteins)
  fmt_compare_b <- reactiveVal(NULL)
  fmt_compare_labels <- reactiveValues(a = "Dataset A", b = "Dataset B")
  fmt_compare_overlap <- reactiveVal(NULL)  # list(proteins, count, pct, shared_groups)
  fmt_compare_built_path <- reactiveVal(NULL)  # path to built multi-dataset file

  fmt_compare_clear_built <- function() {
    fmt_compare_built_path(NULL)
  }

  # Helper function to update overlap calculation
  .compare_update_overlap <- function() {
    a <- fmt_compare_a()
    b <- fmt_compare_b()

    if (is.null(a) || is.null(b)) {
      fmt_compare_overlap(NULL)
      return()
    }

    # Determine if this is a mixed-entity comparison (protein-metabolomics)
    data_type_a <- a$metadata$data_type %||% "proteomics"
    data_type_b <- b$metadata$data_type %||% "proteomics"
    is_mixed <- (data_type_a != data_type_b)

    if (is_mixed) {
      # Mixed entities: report sample/group overlap instead of entity overlap
      shared_groups <- intersect(a$groups, b$groups)
      fmt_compare_overlap(list(
        proteins = character(0),
        count = 0,
        total_a = length(a$proteins),
        total_b = length(b$proteins),
        pct = 0,
        shared_groups = shared_groups,
        is_mixed = TRUE,
        n_shared_groups = length(shared_groups),
        entity_label_a = if (data_type_a == "proteomics") "proteins" else "metabolites",
        entity_label_b = if (data_type_b == "proteomics") "proteins" else "metabolites"
      ))
    } else {
      # Same entity type: standard entity overlap
      overlap_proteins <- intersect(a$proteins, b$proteins)
      total_unique <- length(union(a$proteins, b$proteins))
      pct <- if (total_unique > 0) round(100 * length(overlap_proteins) / total_unique, 1) else 0
      shared_groups <- intersect(a$groups, b$groups)

      fmt_compare_overlap(list(
        proteins = overlap_proteins,
        count = length(overlap_proteins),
        total_a = length(a$proteins),
        total_b = length(b$proteins),
        pct = pct,
        shared_groups = shared_groups,
        is_mixed = FALSE
      ))
    }
  }

  # File upload observer for Dataset A
  observeEvent(input$fmt_compare_file_a, {
    f <- input$fmt_compare_file_a
    if (is.null(f)) return()

    # Clear built file when dataset changes
    fmt_compare_clear_built()

    result <- tryCatch({
      loaded <- nr_load_formatted(f$datapath)
      if (!isTRUE(loaded$ok)) return(NULL)

      # Extract protein IDs
      prot_col <- loaded$metadata$id_protein_col
      if (!nzchar(prot_col) || !prot_col %in% names(loaded$data)) {
        prot_col <- loaded$metadata$id_primary_col
      }
      proteins <- unique(as.character(loaded$data[[prot_col]]))
      proteins <- proteins[!is.na(proteins) & nzchar(proteins)]

      # Extract groups
      groups <- if (nrow(loaded$metadata$groups) > 0) {
        as.character(loaded$metadata$groups$group_name)
      } else {
        unique(as.character(loaded$metadata$columns$group_name))
      }

      list(
        path = f$datapath,
        name = f$name,
        data = loaded$data,
        design = loaded$design,
        metadata = loaded$metadata,
        groups = groups,
        proteins = proteins
      )
    }, error = function(e) NULL)

    fmt_compare_a(result)
    .compare_update_overlap()
  }, ignoreInit = TRUE)

  # File upload observer for Dataset B
  observeEvent(input$fmt_compare_file_b, {
    f <- input$fmt_compare_file_b
    if (is.null(f)) return()

    # Clear built file when dataset changes
    fmt_compare_clear_built()

    result <- tryCatch({
      loaded <- nr_load_formatted(f$datapath)
      if (!isTRUE(loaded$ok)) return(NULL)

      # Extract protein IDs
      prot_col <- loaded$metadata$id_protein_col
      if (!nzchar(prot_col) || !prot_col %in% names(loaded$data)) {
        prot_col <- loaded$metadata$id_primary_col
      }
      proteins <- unique(as.character(loaded$data[[prot_col]]))
      proteins <- proteins[!is.na(proteins) & nzchar(proteins)]

      # Extract groups
      groups <- if (nrow(loaded$metadata$groups) > 0) {
        as.character(loaded$metadata$groups$group_name)
      } else {
        unique(as.character(loaded$metadata$columns$group_name))
      }

      list(
        path = f$datapath,
        name = f$name,
        data = loaded$data,
        design = loaded$design,
        metadata = loaded$metadata,
        groups = groups,
        proteins = proteins
      )
    }, error = function(e) NULL)

    fmt_compare_b(result)
    .compare_update_overlap()
  }, ignoreInit = TRUE)

  # Label input observers
  observeEvent(input$fmt_compare_label_a, {
    val <- trimws(input$fmt_compare_label_a %||% "")
    if (nzchar(val)) fmt_compare_labels$a <- val
  }, ignoreInit = TRUE)

  observeEvent(input$fmt_compare_label_b, {
    val <- trimws(input$fmt_compare_label_b %||% "")
    if (nzchar(val)) fmt_compare_labels$b <- val
  }, ignoreInit = TRUE)

  # Build Multi-Dataset File button handler
  observeEvent(input$fmt_compare_build, {
    a <- fmt_compare_a()
    b <- fmt_compare_b()
    overlap <- fmt_compare_overlap()

    if (is.null(a) || is.null(b) || is.null(overlap)) {
      showNotification("Please upload both datasets first", type = "error")
      return()
    }

    if (overlap$count == 0 && !isTRUE(overlap$is_mixed)) {
      showNotification("No overlapping IDs found. Building anyway.", type = "warning")
    }

    fmt_compare_clear_built()
    busy_step(TRUE, "Building multi-dataset file...", 10)

    tmp <- tempfile(fileext = ".xlsx")
    res <- tryCatch({
      # Determine data types of both datasets
      data_type_a <- a$metadata$data_type %||% "proteomics"
      data_type_b <- b$metadata$data_type %||% "proteomics"

      # Determine multi_subtype
      multi_subtype <- if (data_type_a == "proteomics" && data_type_b == "proteomics") {
        "protein-protein"
      } else if (data_type_a == "metabolomics" && data_type_b == "metabolomics") {
        "metabolomics-metabolomics"
      } else {
        "protein-metabolomics"
      }

      # Build meta for design sheet
      meta <- list(
        schema_version = "1.0",
        data_type = "multi",
        multi_subtype = multi_subtype,
        dataset_a_label = fmt_compare_labels$a,
        dataset_b_label = fmt_compare_labels$b,
        dataset_a_name = a$name,
        dataset_b_name = b$name,
        n_aligned = overlap$count,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

      # Copy ID column metadata from dataset A (primary alignment source)
      for (key in c("analysis_level", "id_primary_type", "id_primary_col",
                    "id_protein_col", "id_gene_col", "id_peptide_col",
                    "id_metabolite_col", "id_metabolite_name_col")) {
        if (!is.null(a$metadata[[key]]) && nzchar(a$metadata[[key]] %||% "")) {
          meta[[key]] <- a$metadata[[key]]
        }
      }

      # For mixed-entity (protein-metabolomics), also store dataset B's ID metadata
      if (multi_subtype == "protein-metabolomics") {
        # Resolve dataset B's primary and display ID columns
        b_primary <- b$metadata$id_metabolite_col %||%
                     b$metadata$id_primary_col %||%
                     b$metadata$id_protein_col %||% ""
        b_display <- b$metadata$id_metabolite_name_col %||%
                     b$metadata$id_gene_col %||% ""
        b_level <- b$metadata$analysis_level %||% "metabolite"

        if (nzchar(b_primary)) meta$dataset_b_id_primary_col <- b_primary
        if (nzchar(b_display)) meta$dataset_b_id_display_col <- b_display
        meta$dataset_b_analysis_level <- b_level
      }

      # Build groups dataframe - combine groups from both datasets with prefixes
      groups_a <- a$metadata$groups
      groups_b <- b$metadata$groups

      if (is.null(groups_a) || nrow(groups_a) == 0) {
        groups_a <- data.frame(
          group_id = seq_along(a$groups),
          group_name = a$groups,
          color = rep("#888888", length(a$groups)),
          is_control = FALSE,
          stringsAsFactors = FALSE
        )
      }
      if (is.null(groups_b) || nrow(groups_b) == 0) {
        groups_b <- data.frame(
          group_id = seq_along(b$groups),
          group_name = b$groups,
          color = rep("#888888", length(b$groups)),
          is_control = FALSE,
          stringsAsFactors = FALSE
        )
      }

      # Prefix group names to distinguish datasets
      groups_a$group_name <- paste0("A:", groups_a$group_name)
      groups_a$group_id <- paste0("a_", groups_a$group_id)
      groups_b$group_name <- paste0("B:", groups_b$group_name)
      groups_b$group_id <- paste0("b_", groups_b$group_id)

      # Ensure both have same columns
      all_group_cols <- c("group_id", "group_name", "color", "is_control")
      for (col in all_group_cols) {
        if (!col %in% names(groups_a)) groups_a[[col]] <- NA
        if (!col %in% names(groups_b)) groups_b[[col]] <- NA
      }
      groups_a <- groups_a[, all_group_cols, drop = FALSE]
      groups_b <- groups_b[, all_group_cols, drop = FALSE]

      groups_df <- rbind(groups_a, groups_b)

      # Build column records - combine from both datasets
      cols_a <- a$metadata$columns
      cols_b <- b$metadata$columns

      if (!is.null(cols_a) && nrow(cols_a) > 0) {
        cols_a$source_dataset <- "a"
        cols_a$group_id <- paste0("a_", cols_a$group_id)
        cols_a$group_name <- paste0("A:", cols_a$group_name)
      }
      if (!is.null(cols_b) && nrow(cols_b) > 0) {
        cols_b$source_dataset <- "b"
        cols_b$group_id <- paste0("b_", cols_b$group_id)
        cols_b$group_name <- paste0("B:", cols_b$group_name)
      }

      # Ensure both have same columns
      all_col_cols <- c("data_col", "display_name", "group_id", "group_name",
                        "replicate", "include", "source_file", "source_col", "source_dataset")
      for (col in all_col_cols) {
        if (!is.null(cols_a) && !col %in% names(cols_a)) cols_a[[col]] <- NA
        if (!is.null(cols_b) && !col %in% names(cols_b)) cols_b[[col]] <- NA
      }
      if (!is.null(cols_a)) cols_a <- cols_a[, all_col_cols, drop = FALSE]
      if (!is.null(cols_b)) cols_b <- cols_b[, all_col_cols, drop = FALSE]

      columns_df <- rbind(cols_a, cols_b)

      busy_step(TRUE, "Writing multi-dataset .xlsx...", 60)

      # Write workbook with data_a and data_b sheets
      msterp_require_pkgs(c("openxlsx"))
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "data_a")
      openxlsx::addWorksheet(wb, "data_b")
      openxlsx::addWorksheet(wb, "design")

      openxlsx::writeData(wb, sheet = "data_a", x = a$data)
      openxlsx::writeData(wb, sheet = "data_b", x = b$data)

      # Build design sheet
      design_df <- msterp_build_design_sheet(meta, groups_df, columns_df)
      openxlsx::writeData(wb, sheet = "design", x = design_df)

      openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
      tmp
    }, error = function(e) e)

    busy_step(FALSE)

    if (inherits(res, "error")) {
      fmt_compare_built_path(NULL)
      showNotification(paste("Build error:", res$message), type = "error")
      return()
    }

    fmt_compare_built_path(res)
    if (isTRUE(overlap$is_mixed)) {
      showNotification(
        sprintf("Cross-omics multi-dataset file built (%s + %s). Ready to download.",
                overlap$entity_label_a %||% "dataset A",
                overlap$entity_label_b %||% "dataset B"),
        type = "message"
      )
    } else {
      showNotification(
        sprintf("Multi-dataset file built: %d aligned IDs. Ready to download.", overlap$count),
        type = "message"
      )
    }
  }, ignoreInit = TRUE)

  # Download handler for multi-dataset file
  output$fmt_compare_download <- downloadHandler(
    filename = function() {
      a <- fmt_compare_a()
      b <- fmt_compare_b()
      name_a <- tools::file_path_sans_ext(a$name %||% "datasetA")
      name_b <- tools::file_path_sans_ext(b$name %||% "datasetB")
      paste0("multi_", name_a, "_", name_b, ".xlsx")
    },
    content = function(file) {
      path <- fmt_compare_built_path()
      req(!is.null(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )

  # Compare tab UI rendering
  output$fmt_compare_content <- renderUI({
    a <- fmt_compare_a()
    b <- fmt_compare_b()
    overlap <- fmt_compare_overlap()
    built_path <- fmt_compare_built_path()

    label_for <- function(meta) {
      if (is.null(meta) || is.null(meta$data_type)) return("IDs")
      dt <- tolower(as.character(meta$data_type %||% ""))
      if (identical(dt, "metabolomics")) return("Metabolites")
      if (identical(dt, "proteomics")) return("Proteins")
      "IDs"
    }
    label_a <- if (!is.null(a) && !is.null(a$metadata)) label_for(a$metadata) else "IDs"
    label_b <- if (!is.null(b) && !is.null(b$metadata)) label_for(b$metadata) else "IDs"
    overlap_label <- if (!is.null(a) && !is.null(b) && identical(label_a, label_b)) {
      paste("Overlapping", tolower(label_a))
    } else {
      "Overlapping IDs"
    }

    tags$div(
      class = "fmt-two-panel single",
      tags$div(
        class = "fmt-panel",
        tags$h4("Compare Datasets"),

        # Two-column upload grid
        tags$div(
          class = "fmt-compare-grid",

          # Dataset A panel
          tags$div(
            class = "fmt-dataset-panel",
            tags$h5("Dataset A"),
            fileInput(
              "fmt_compare_file_a", "Upload formatted .xlsx",
              accept = c(".xlsx", ".xls"),
              width = "100%"
            ),
            if (!is.null(a)) {
              tags$div(
                class = "fmt-summary",
                tags$strong(a$name),
                tags$div(sprintf("Groups: %s", paste(a$groups, collapse = ", "))),
                tags$div(sprintf("%s: %s", label_a, format(length(a$proteins), big.mark = ",")))
              )
            } else {
              tags$div(class = "text-muted", "No file uploaded")
            }
          ),

          # Dataset B panel
          tags$div(
            class = "fmt-dataset-panel",
            tags$h5("Dataset B"),
            fileInput(
              "fmt_compare_file_b", "Upload formatted .xlsx",
              accept = c(".xlsx", ".xls"),
              width = "100%"
            ),
            if (!is.null(b)) {
              tags$div(
                class = "fmt-summary",
                tags$strong(b$name),
                tags$div(sprintf("Groups: %s", paste(b$groups, collapse = ", "))),
                tags$div(sprintf("%s: %s", label_b, format(length(b$proteins), big.mark = ",")))
              )
            } else {
              tags$div(class = "text-muted", "No file uploaded")
            }
          )
        ),

        # Overlap preview (shown when both datasets loaded)
        if (!is.null(overlap)) {
          if (isTRUE(overlap$is_mixed)) {
            # Mixed-entity mode: show sample/group alignment instead of entity overlap
            tags$div(
              class = "fmt-overlap-preview",
              tags$strong("Cross-Omics Alignment Preview"),
              tags$div(sprintf("Dataset A: %s %s",
                              format(overlap$total_a, big.mark = ","),
                              overlap$entity_label_a %||% "features")),
              tags$div(sprintf("Dataset B: %s %s",
                              format(overlap$total_b, big.mark = ","),
                              overlap$entity_label_b %||% "features")),
              tags$div(style = "margin-top: 6px; font-style: italic; color: var(--color-text-subtle);",
                       "Different entity types - alignment by paired samples (group + replicate)"),
              if (length(overlap$shared_groups) > 0) {
                tags$div(style = "margin-top: 4px;",
                         sprintf("Shared groups (%d): %s",
                                length(overlap$shared_groups),
                                paste(overlap$shared_groups, collapse = ", ")))
              } else {
                tags$div(class = "text-muted", style = "margin-top: 4px;",
                         "No shared group names - samples will be paired by replicate order")
              }
            )
          } else {
            # Same-entity-type: standard entity overlap
            tags$div(
              class = "fmt-overlap-preview",
              tags$strong("Alignment Preview"),
              tags$div(
                sprintf("%s: %s (%s%%)",
                        overlap_label,
                        format(overlap$count, big.mark = ","),
                        overlap$pct)
              ),
              if (length(overlap$shared_groups) > 0) {
                tags$div(sprintf("Shared groups: %s", paste(overlap$shared_groups, collapse = ", ")))
              } else {
                tags$div(class = "text-muted", "No shared group names")
              }
            )
          }
        },

        # Dataset labels section
        if (!is.null(a) && !is.null(b)) {
          tags$div(
            class = "fmt-labels-section",
            tags$strong("Dataset Labels"),
            tags$p(class = "text-muted", style = "font-size: 12px; margin: 4px 0 10px 0;",
                   "Labels appear in axis titles and legends"),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px;",
              textInput("fmt_compare_label_a", "Dataset A label",
                        value = isolate(fmt_compare_labels$a), width = "100%"),
              textInput("fmt_compare_label_b", "Dataset B label",
                        value = isolate(fmt_compare_labels$b), width = "100%")
            )
          )
        },

        # Build and download multi-dataset file
        # Allow building when: same-entity overlap > 0, OR mixed entities with shared groups
        if (!is.null(overlap) && (overlap$count > 0 || (isTRUE(overlap$is_mixed) && length(overlap$shared_groups) > 0))) {
          tags$div(
            style = "margin-top: 16px;",
            actionButton(
              "fmt_compare_build", "Build Multi-Dataset File",
              class = "btn btn-primary",
              icon = icon("layer-group")
            ),
            if (!is.null(built_path)) {
              tags$span(
                style = "margin-left: 12px;",
                downloadButton(
                  "fmt_compare_download", "Download",
                  class = "btn btn-success",
                  icon = icon("download")
                ),
                tags$span(
                  style = "margin-left: 8px; color: var(--status-success-text); font-weight: 600;",
                  icon("check-circle"),
                  "Ready"
                )
              )
            }
          )
        }
      )
    )
  })

}
