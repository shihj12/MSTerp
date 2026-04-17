#!/usr/bin/env Rscript
# Diagnostic: print structure of a .terpflow file so you can verify whether
# paired GO-ORA configs actually carry database="complex" through save/load.
#
# Usage: Rscript scripts/inspect_terpflow.R path/to/pipeline.terpflow

`%||%` <- function(a, b) if (!is.null(a)) a else b

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript inspect_terpflow.R <path.terpflow>")
path <- args[[1]]
if (!file.exists(path)) stop("File not found: ", path)

flow <- readRDS(path)
cat(sprintf("File:          %s\n", path))
cat(sprintf("pipeline_name: %s\n", flow$pipeline_name %||% "(none)"))
cat(sprintf("pipeline_type: %s\n", flow$pipeline_type %||% "(none)"))
cat(sprintf("schema_version: %s\n", as.character(flow$schema_version %||% "(none)")))
cat(sprintf("steps:         %d\n\n", length(flow$steps %||% list())))

for (s in (flow$steps %||% list())) {
  cat(sprintf("- step %s (engine=%s, enabled=%s)\n",
              s$step_id %||% "?", s$engine_id %||% "?", isTRUE(s$enabled %||% TRUE)))
  p <- s$paired
  if (is.null(p)) { cat("    paired: (none)\n"); next }
  cat(sprintf("    paired enabled=%s engine=%s configs=%d\n",
              isTRUE(p$enabled), p$engine_id %||% "?",
              length(p$configs %||% list())))
  if (length(p$configs %||% list()) == 0) {
    cat("      (no per-config array — legacy schema)\n")
    cat(sprintf("      paired$params$database = %s\n",
                as.character(p$params$database %||% "(missing)")))
    next
  }
  for (c in p$configs) {
    cat(sprintf("      cfg %-10s name=%-20s database=%s\n",
                c$config_id %||% "?",
                c$name %||% "(unnamed)",
                as.character(c$database %||% "(missing)")))
  }
}
