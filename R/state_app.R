# R/state_app.R
# Store function names (strings) so this file never fails if loaded before pages.

MSTERP_PAGES <- list(
  home     = list(ui = "page_home_ui",     server = "page_home_server"),
  results  = list(ui = "page_results_ui",  server = "page_results_server"),
  format   = list(ui = "page_format_ui",   server = "page_format_server"),
  new_run  = list(ui = "page_new_run_ui",  server = "page_new_run_server"),
  pipeline = list(ui = "page_pipeline_ui", server = "page_pipeline_server"),
  tools    = list(ui = "page_tools_ui",    server = "page_tools_server"),
  database = list(ui = "page_database_ui", server = "page_database_server"),
  tutorial = list(ui = "page_tutorial_ui", server = "page_tutorial_server"),
  about    = list(ui = "page_about_ui",    server = "page_about_server")
)
