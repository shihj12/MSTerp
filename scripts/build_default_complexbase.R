# scripts/build_default_complexbase.R
# Build default ComplexBase files from CORUM database
#
# Run this script from the MSTerp root directory:
#   Rscript scripts/build_default_complexbase.R
#
# This will download CORUM data and create:
#   - complexbase/human_corum.complexbase
#   - complexbase/mouse_corum.complexbase

# Source required modules
source("R/engines/complexbase.R")
source("R/pages/tools/tool_complexbase.R")

# Create output directory
if (!dir.exists("complexbase")) {
  dir.create("complexbase")
}

# CORUM download URL (core complexes)
# Note: CORUM requires manual download from https://mips.helmholtz-muenchen.de/corum/#download
# This script expects the file to be at data/corum_allComplexes.txt

corum_file <- "complexbase/corum_allComplexes.txt"

if (!file.exists(corum_file)) {
  message("CORUM file not found at: ", corum_file)
  message("")
  message("To build default complexbase files:")
  message("1. Go to https://mips.helmholtz-muenchen.de/corum/#download")
  message("2. Download 'allComplexes.txt' (Complete CORUM dataset)")
  message("3. Save it as complexbase/corum_allComplexes.txt")
  message("4. Re-run this script")
  message("")
  message("Alternatively, use the ComplexBase Builder tool in MSTerp to build from your own download.")
  quit(save = "no", status = 1)
}

message("Building default ComplexBase files from CORUM...")

# Build Human ComplexBase
message("\n=== Building Human ComplexBase ===")
human_result <- complexbase_build_from_corum(
  path = corum_file,
  organism = "Human",
  library_name = "human_corum",
  log_fn = message
)

if (human_result$ok) {
  human_path <- "complexbase/human_corum.complexbase"
  saveRDS(human_result$complexbase, human_path)
  message("Saved: ", human_path)
  message(sprintf("  Complexes: %d", nrow(human_result$complexbase$complexes)))
  message(sprintf("  Proteins: %d", length(unique(human_result$complexbase$protein_complex$protein_id))))
} else {
  message("ERROR building human complexbase: ", human_result$error)
}

# Build Mouse ComplexBase
message("\n=== Building Mouse ComplexBase ===")
mouse_result <- complexbase_build_from_corum(
  path = corum_file,
  organism = "Mouse",
  library_name = "mouse_corum",
  log_fn = message
)

if (mouse_result$ok) {
  mouse_path <- "complexbase/mouse_corum.complexbase"
  saveRDS(mouse_result$complexbase, mouse_path)
  message("Saved: ", mouse_path)
  message(sprintf("  Complexes: %d", nrow(mouse_result$complexbase$complexes)))
  message(sprintf("  Proteins: %d", length(unique(mouse_result$complexbase$protein_complex$protein_id))))
} else {
  message("ERROR building mouse complexbase: ", mouse_result$error)
}

message("\n=== Done ===")
message("Default ComplexBase files created in complexbase/ directory")
