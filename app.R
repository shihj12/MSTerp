 # app.R
suppressPackageStartupMessages({
  library(patchwork)
  library(shiny)
  library(bslib)
  library(DT)
  library(colourpicker)
  library(sortable)
  library(shinyFiles)
  library(htmltools)

  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(readxl)
  library(openxlsx)
  library(jsonlite)

  library(ggplot2)
  library(plotly)
  library(scales)
  library(pheatmap)
  library(ggplotify)
  library(viridisLite)
  library(svglite)
  library(leaflet)

  library(limma)

  library(httr)
  library(zip)
  library(later)
  library(callr)
  library(progress)
  library(rvest)
  library(xml2)
  library(ggvenn)

  library(igraph)
  library(visNetwork)
})

source_required <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  source(path, local = FALSE)
}
source_dir_r <- function(dir_path) {
  files <- sort(list.files(dir_path, pattern = "\\.R$", full.names = TRUE))
  for (f in files) {
    tryCatch(
      source(f, local = FALSE),
      error = function(e) {
        message("FAILED while sourcing: ", f)
        stop(e)
      }
    )
  }
}

call_page_server <- function(server_fn_name, input, output, session, app_state) {
  if (is.null(server_fn_name) || !nzchar(server_fn_name)) return(invisible(NULL))
  if (!exists(server_fn_name, mode = "function", inherits = TRUE)) return(invisible(NULL))
  
  fn <- get(server_fn_name, mode = "function", inherits = TRUE)
  fmls <- names(formals(fn))
  args <- list()
  
  if ("input"     %in% fmls) args$input     <- input
  if ("output"    %in% fmls) args$output    <- output
  if ("session"   %in% fmls) args$session   <- session
  if ("app_state" %in% fmls) args$app_state <- app_state
  
  do.call(fn, args)
}

source_required(file.path("R", "00_init.R"))
source_required(file.path("R", "ui_notifications.R"))
source_required(file.path("R", "ui_shell.R"))
source_required(file.path("R", "state_app.R"))
source_dir_r(file.path("R", "engines"))
source_dir_r(file.path("R", "engines", "stats"))
source_dir_r(file.path("R", "utils"))
source_dir_r(file.path("R", "pages"))


# -----------------------------
# Init + UI
# -----------------------------
init_common_assets()

# Dynamic UI that switches between landing page and shell
ui <- fluidPage(
  msterp_theme_head(),
  uiOutput("app_container")
)

server <- function(input, output, session) {
  app_state <- reactiveValues(
    terpbase = NULL,
    complexbase = NULL,
    datasets = list(),
    pipelines = list(),
    last_run = NULL
  )

  current_page <- reactiveVal("home")

  # Track whether we're in shell mode (any page except home)
  in_shell_mode <- reactiveVal(FALSE)

  set_page <- function(page_id) {
    if (!page_id %in% names(MSTERP_PAGES)) return(invisible(NULL))

    # Update shell mode first (before page change)
    new_shell_mode <- (page_id != "home")
    old_shell_mode <- in_shell_mode()

    # Update page
    current_page(page_id)

    if (new_shell_mode != old_shell_mode) {
      in_shell_mode(new_shell_mode)
    }

    session$sendCustomMessage("msterp_set_active_nav", list(page_id = page_id))

    # Sidebar removed — no auto-collapse needed
  }

  output$app_container <- renderUI({
    shell_mode <- in_shell_mode()

    if (!shell_mode) {
      page_home_ui()
    } else {
      div(
        class = "msterp-wrap",
        topbar_ui(),
        div(
          class = "msterp-shell",
          div(class = "msterp-content", uiOutput("page_ui"))
        ),
        msterp_busy_overlay_ui()
      )
    }
  })

  # Sidebar navigation
  observeEvent(input$nav_home,     set_page("home"))
  observeEvent(input$nav_results,  set_page("results"))
  observeEvent(input$nav_format,   set_page("format"))
  observeEvent(input$nav_newrun,   set_page("new_run"))
  observeEvent(input$nav_pipeline, set_page("pipeline"))
  observeEvent(input$nav_tools,    set_page("tools"))
  observeEvent(input$nav_db,       set_page("database"))
  observeEvent(input$nav_tutorial, set_page("tutorial"))
  observeEvent(input$nav_about,    set_page("about"))

  # Home buttons (if present)
  observeEvent(input$home_go_results,  set_page("results"))
  observeEvent(input$home_go_format,   set_page("format"))
  observeEvent(input$home_go_newrun,   set_page("new_run"))
  observeEvent(input$home_go_newrun2,  set_page("new_run"))
  observeEvent(input$home_go_pipeline, set_page("pipeline"))
  observeEvent(input$home_go_tools,    set_page("tools"))
  observeEvent(input$home_go_db,       set_page("database"))
  observeEvent(input$home_go_tutorial, set_page("tutorial"))
  observeEvent(input$home_go_about,    set_page("about"))

  # Render page UI (for non-home pages inside the shell)
  output$page_ui <- renderUI({
    pg_id <- current_page()
    if (pg_id == "home") return(NULL)  

    pg <- MSTERP_PAGES[[pg_id]]

    ui_name <- pg$ui
    if (!exists(ui_name, mode = "function", inherits = TRUE)) {
      return(tags$div(
        tags$h3("Missing page UI function"),
        tags$pre(sprintf("Function not found: %s\nExpected in: R/pages/...", ui_name))
      ))
    }

    get(ui_name, mode = "function", inherits = TRUE)()
  })

  lapply(names(MSTERP_PAGES), function(pid) {
    call_page_server(MSTERP_PAGES[[pid]]$server, input, output, session, app_state)
  })
  observe({
    session$sendCustomMessage("msterp_set_active_nav", list(page_id = current_page()))
  }, priority = 1000)
}

shinyApp(ui, server)
