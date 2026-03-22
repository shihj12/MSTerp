# R/00_init.R
library(shiny)

init_common_assets <- function() {
  options(shiny.maxRequestSize = 10 * 1024^3) # 10 gigs
  
  # Keep 'static/' URLs stable across the app
  if (dir.exists("www")) shiny::addResourcePath("static", normalizePath("www"))
}
