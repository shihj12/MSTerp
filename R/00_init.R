# R/00_init.R
library(shiny)

init_common_assets <- function() {
  options(shiny.maxRequestSize = 10 * 1024^3) # 10 gigs

  # Keep 'static/' URLs stable across the app
  if (dir.exists("www")) shiny::addResourcePath("static", normalizePath("www"))
}

# Application version. Release builds write the git tag (without the leading "v")
# into a VERSION file at the app root during packaging (see
# .github/workflows/build.yml and desktop-webview2/installer.iss). When running
# from source without that file, fall back to "dev". Cached after first read.
.msterp_version_cache <- NULL
msterp_app_version <- function() {
  if (!is.null(.msterp_version_cache)) return(.msterp_version_cache)
  v <- tryCatch(
    if (file.exists("VERSION")) trimws(readLines("VERSION", n = 1, warn = FALSE)) else "",
    error = function(e) ""
  )
  if (length(v) != 1 || is.na(v) || !nzchar(v)) v <- "dev"
  .msterp_version_cache <<- v
  v
}
