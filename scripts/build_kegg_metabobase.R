#!/usr/bin/env Rscript
# ==========================================================
# build_kegg_metabobase.R - Build MetaboBase from KEGG pathways
#
# Usage:
#   Rscript scripts/build_kegg_metabobase.R [organism] [output_path]
#
# Examples:
#   Rscript scripts/build_kegg_metabobase.R hsa metabobase/kegg_human.metabobase
#   Rscript scripts/build_kegg_metabobase.R mmu metabobase/kegg_mouse.metabobase
#
# Supported organisms:
#   hsa - Homo sapiens (Human)
#   mmu - Mus musculus (Mouse)
#   rno - Rattus norvegicus (Rat)
#   dme - Drosophila melanogaster (Fruit fly)
#   cel - Caenorhabditis elegans (Nematode)
#   sce - Saccharomyces cerevisiae (Yeast)
#   eco - Escherichia coli
#   ath - Arabidopsis thaliana
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
       "  Rscript scripts/build_kegg_metabobase.R")
}

# Source metabobase module
message("Loading metabobase module...")
source("R/engines/metabobase.R")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
organism <- if (length(args) >= 1) args[1] else "hsa"
output_path <- if (length(args) >= 2) args[2] else paste0("metabobase/kegg_", organism, ".metabobase")

# Validate organism code
valid_organisms <- c("hsa", "mmu", "rno", "dme", "cel", "sce", "eco", "ath")
organism_names <- c(
  hsa = "Homo sapiens (Human)",
  mmu = "Mus musculus (Mouse)",
  rno = "Rattus norvegicus (Rat)",
  dme = "Drosophila melanogaster (Fruit fly)",
  cel = "Caenorhabditis elegans (Nematode)",
  sce = "Saccharomyces cerevisiae (Yeast)",
  eco = "Escherichia coli",
  ath = "Arabidopsis thaliana"
)

if (!organism %in% valid_organisms) {
  message("ERROR: Invalid organism code: ", organism)
  message("\nValid organism codes:")
  for (code in valid_organisms) {
    message(sprintf("  %s - %s", code, organism_names[code]))
  }
  quit(save = "no", status = 1)
}

message("=========================================")
message("Building KEGG MetaboBase")
message("=========================================\n")
message(sprintf("Organism: %s (%s)", organism, organism_names[organism]))
message(sprintf("Output path: %s\n", output_path))

# Create output directory if needed
output_dir <- dirname(output_path)
if (!dir.exists(output_dir) && nzchar(output_dir) && output_dir != ".") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  message(sprintf("Created output directory: %s", output_dir))
}

# Build the MetaboBase
message("[1/2] Fetching KEGG pathways and compounds...")
message("      (This may take several minutes due to KEGG API rate limits)\n")

mb <- tryCatch({
  metabobase_build_kegg_library(
    organism = organism,
    library_name = paste0("KEGG ", organism_names[organism]),
    output_file = NULL,  # Don't save yet, we'll validate first
    progress_callback = function(msg) message("      ", msg)
  )
}, error = function(e) {
  message("\nERROR: ", e$message)
  message("\nStack trace:")
  traceback()
  quit(save = "no", status = 1)
})

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
file_size <- file.info(output_path)$size
message(sprintf("Done! File size: %.2f MB", file_size / 1024 / 1024))

message("\n=========================================")
message("KEGG MetaboBase build complete!")
message("=========================================")
