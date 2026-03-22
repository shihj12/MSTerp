#!/usr/bin/env Rscript
# ==========================================================
# build_hmdb_metabobase.R - Build MetaboBase from HMDB XML
#
# Usage:
#   Rscript scripts/build_hmdb_metabobase.R <path/to/hmdb_metabolites.xml> [output_path]
#
# Examples:
#   Rscript scripts/build_hmdb_metabobase.R hmdb_metabolites.xml
#   Rscript scripts/build_hmdb_metabobase.R hmdb_metabolites.xml metabobase/hmdb_human.metabobase
#
# The HMDB XML file can be downloaded from:
#   https://hmdb.ca/system/downloads/current/hmdb_metabolites.zip
#   (approximately 7 GB uncompressed)
#
# Requirements:
#   - R packages: xml2, dplyr, stringr, tidyr, tibble
#   - At least 8 GB RAM available
#
# ==========================================================

# Set working directory to project root if running from scripts/
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
if (basename(script_dir) == "scripts") {
  setwd(dirname(script_dir))
}

# Check we're in the right directory
if (!file.exists("R/engines/metabobase.R")) {
  stop("Please run this script from the MSTerp project root directory:\n",
       "  cd /path/to/MSTerp\n",
       "  Rscript scripts/build_hmdb_metabobase.R path/to/hmdb_metabolites.xml")
}

# Source metabobase module
message("Loading metabobase module...")
source("R/engines/metabobase.R")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  message("ERROR: No XML file path provided.\n")
  message("Usage: Rscript scripts/build_hmdb_metabobase.R <path/to/hmdb_metabolites.xml> [output_path]")
  message("\nDownload the HMDB XML from: https://hmdb.ca/system/downloads/current/hmdb_metabolites.zip")
  quit(save = "no", status = 1)
}

xml_path <- args[1]
output_path <- if (length(args) >= 2) args[2] else "metabobase/hmdb_human.metabobase"

# Validate input file exists
if (!file.exists(xml_path)) {
  message("ERROR: XML file not found: ", xml_path)
  quit(save = "no", status = 1)
}

# Report file size
file_size_gb <- file.info(xml_path)$size / (1024^3)
message("=========================================")
message("Building HMDB MetaboBase")
message("=========================================\n")
message(sprintf("Input: %s (%.2f GB)", xml_path, file_size_gb))
message(sprintf("Output: %s\n", output_path))

# Create output directory if needed
output_dir <- dirname(output_path)
if (!dir.exists(output_dir) && nzchar(output_dir) && output_dir != ".") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  message(sprintf("Created output directory: %s", output_dir))
}

# Build the MetaboBase
message("[1/2] Parsing HMDB XML and building MetaboBase...")
message("      (This may take 10-30 minutes depending on hardware)\n")

t0 <- Sys.time()

mb <- tryCatch({
  metabobase_build_hmdb_library(
    xml_path = xml_path,
    library_name = "HMDB Human",
    output_file = NULL,
    progress_callback = function(msg) message("      ", msg)
  )
}, error = function(e) {
  message("\nERROR: ", e$message)
  message("\nStack trace:")
  traceback()
  quit(save = "no", status = 1)
})

dt <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
message(sprintf("\n      Build completed in %.1f minutes", dt))

# Validate
message("\n[2/2] Validating MetaboBase...")
validation <- metabobase_validate(mb)
if (!validation$ok) {
  message("Validation failed:")
  for (err in validation$errors) message("  - ", err)
  quit(save = "no", status = 1)
}
message("      Validation passed!")

# Print summary
message("\n=== MetaboBase Summary ===")
for (line in metabobase_summary_lines(mb)) {
  message(line)
}

# Save
message(sprintf("\nSaving to %s...", output_path))
metabobase_save(mb, output_path)

# Report file size
out_size <- file.info(output_path)$size
message(sprintf("Done! File size: %.2f MB", out_size / 1024 / 1024))

message("\n=========================================")
message("HMDB MetaboBase build complete!")
message("=========================================")
