#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

install_if_missing <- function(packages, quiet = TRUE) {
  missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) == 0) {
    return(character())
  }
  install.packages(missing, quiet = quiet)
  missing[!vapply(missing, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
}

core_cran <- c(
  "jsonlite",
  "openxlsx",
  "readxl",
  "dplyr",
  "stringr",
  "tidyr",
  "tibble",
  "httr",
  "xml2",
  "later",
  "callr",
  "zip",
  "ggplot2",
  "plotly",
  "htmltools",
  "htmlwidgets",
  "gtable",
  "patchwork",
  "pheatmap",
  "ggrepel",
  "ggplotify",
  "viridisLite",
  "visNetwork",
  "igraph",
  "rlang",
  "svglite",
  "progress",
  "scales",
  "shiny",
  "shinyFiles"
)

optional_cran <- c(
  "DT",
  "bslib",
  "colourpicker",
  "sortable",
  "shinyjs",
  "rvest",
  "leaflet",
  "ggvenn",
  "rankplot"
)

cat("Installing core CRAN packages for optional MSTerp legacy migration helpers...\n")
core_failures <- install_if_missing(core_cran)

cat("Installing optional CRAN packages used by the legacy R UI and richer exports...\n")
optional_failures <- tryCatch(install_if_missing(optional_cran), error = function(e) optional_cran)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", quiet = TRUE)
}

bioc_failures <- character()
if (requireNamespace("BiocManager", quietly = TRUE)) {
  bioc_missing <- c("limma")[!vapply(c("limma"), requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(bioc_missing) > 0) {
    BiocManager::install(bioc_missing, ask = FALSE, update = FALSE, quiet = TRUE)
    bioc_failures <- bioc_missing[!vapply(bioc_missing, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  }
} else {
  bioc_failures <- "BiocManager"
}

cat("\nPackage status summary\n")
cat("----------------------\n")
cat("Core failures: ", if (length(core_failures)) paste(core_failures, collapse = ", ") else "none", "\n", sep = "")
cat("Optional failures: ", if (length(optional_failures)) paste(optional_failures, collapse = ", ") else "none", "\n", sep = "")
cat("Bioconductor failures: ", if (length(bioc_failures)) paste(bioc_failures, collapse = ", ") else "none", "\n", sep = "")

if (length(core_failures) > 0 || length(bioc_failures) > 0) {
  quit(status = 1L)
}
