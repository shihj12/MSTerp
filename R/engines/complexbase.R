# =========================================================
# R/engines/complexbase.R - Protein Complex Database Loader
#
# Provides loader, validator, and lookup utilities for protein
# complex databases (CORUM, ComplexPortal) in .complexbase format.
#
# ComplexBase schema (v1):
#   $schema_version: 1L
#   $organism: "Homo sapiens"
#   $source: "CORUM+ComplexPortal"
#   $created: POSIXct timestamp
#   $complexes: data.frame(complex_id, complex_name, source, n_subunits, category)
#   $protein_complex: data.frame(protein_id, gene_symbol, complex_id) - long format
#   $complex_proteins: list(complex_id = c(protein_ids...)) - fast lookup
#
# Usage pattern:
#   cb <- complexbase_load("path/to/human.complexbase")
#   proteins <- complexbase_get_complex_proteins(cb, "CORUM:123")
#   complexes <- complexbase_get_protein_complexes(cb, "P12345")
# =========================================================

# Null-coalescing operator (if not already defined in scope)
`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# Schema version constant
COMPLEXBASE_SCHEMA_VERSION <- 1L

# Column mapping for term-based enrichment integration
# Maps ComplexBase columns to enrichment engine expectations
COMPLEXBASE_TERM_COLS <- list(

  term_id = "complex_id",
  term_name = "complex_name",
  ontology = "source"
)

# -----------------------------------------------------------------------------
# Core Functions: Load and Validate
# -----------------------------------------------------------------------------

#' Load a ComplexBase file
#'
#' Reads a .complexbase file (RDS format) and validates its structure.
#' Returns the validated ComplexBase object or NULL with a warning on failure.
#'
#' @param path Path to the .complexbase file
#' @return ComplexBase object (list) or NULL if validation fails
#' @export
complexbase_load <- function(path) {
  if (!file.exists(path)) {
    warning("ComplexBase file not found: ", path)
    return(NULL)
  }

  cb <- tryCatch(
    readRDS(path),
    error = function(e) {
      warning("Failed to read ComplexBase file: ", e$message)
      NULL
    }
  )

  if (is.null(cb)) {
    return(NULL)
  }

  validation <- complexbase_validate(cb)
  if (!validation$ok) {
    warning("ComplexBase validation failed:\n  ", paste(validation$errors, collapse = "\n  "))
    return(NULL)
  }

  cb
}

#' Validate a ComplexBase object
#'
#' Checks that a ComplexBase object conforms to the expected schema.
#' Verifies schema version, required fields, and column structure.
#'
#' @param cb ComplexBase object to validate
#' @return list(ok = TRUE/FALSE, errors = character vector of error messages)
#' @export
complexbase_validate <- function(cb) {
  errors <- character(0)

  # Must be a list

if (!is.list(cb)) {
    return(list(ok = FALSE, errors = "ComplexBase object is not a list"))
  }

  # Check schema_version
  if (is.null(cb$schema_version)) {
    errors <- c(errors, "Missing schema_version field")
  } else if (!is.numeric(cb$schema_version) || cb$schema_version != COMPLEXBASE_SCHEMA_VERSION) {
    errors <- c(errors, sprintf("Invalid schema_version: expected %d, got %s",
                                 COMPLEXBASE_SCHEMA_VERSION,
                                 as.character(cb$schema_version)))
  }

  # Check required top-level fields
  required_fields <- c("complexes", "protein_complex", "complex_proteins")
  missing_fields <- setdiff(required_fields, names(cb))
  if (length(missing_fields) > 0) {
    errors <- c(errors, paste("Missing required fields:", paste(missing_fields, collapse = ", ")))
  }

  # Validate complexes data.frame
  if ("complexes" %in% names(cb)) {
    if (!is.data.frame(cb$complexes)) {
      errors <- c(errors, "complexes must be a data.frame")
    } else {
      required_cols <- c("complex_id", "complex_name", "source", "n_subunits")
      missing_cols <- setdiff(required_cols, names(cb$complexes))
      if (length(missing_cols) > 0) {
        errors <- c(errors, paste("complexes missing columns:", paste(missing_cols, collapse = ", ")))
      }
    }
  }

  # Validate protein_complex data.frame
  if ("protein_complex" %in% names(cb)) {
    if (!is.data.frame(cb$protein_complex)) {
      errors <- c(errors, "protein_complex must be a data.frame")
    } else {
      required_cols <- c("protein_id", "complex_id")
      missing_cols <- setdiff(required_cols, names(cb$protein_complex))
      if (length(missing_cols) > 0) {
        errors <- c(errors, paste("protein_complex missing columns:", paste(missing_cols, collapse = ", ")))
      }
    }
  }

  # Validate complex_proteins list
  if ("complex_proteins" %in% names(cb)) {
    if (!is.list(cb$complex_proteins)) {
      errors <- c(errors, "complex_proteins must be a list")
    }
  }

  list(ok = length(errors) == 0, errors = errors)
}

# -----------------------------------------------------------------------------
# Lookup Functions
# -----------------------------------------------------------------------------

#' Get proteins in a complex
#'
#' Returns the protein IDs (UniProt) that are subunits of a given complex.
#'
#' @param cb ComplexBase object
#' @param complex_id Complex identifier (e.g., "CORUM:123")
#' @return Character vector of protein IDs, or character(0) if not found
#' @export
complexbase_get_complex_proteins <- function(cb, complex_id) {
  if (is.null(cb) || is.null(cb$complex_proteins)) {
    return(character(0))
  }

  proteins <- cb$complex_proteins[[complex_id]]
  if (is.null(proteins)) {
    return(character(0))
  }

  as.character(proteins)
}

#' Get complexes containing a protein
#'
#' Returns the complex IDs that contain a given protein.
#'
#' @param cb ComplexBase object
#' @param protein_id Protein identifier (UniProt ID)
#' @return Character vector of complex IDs, or character(0) if not found
#' @export
complexbase_get_protein_complexes <- function(cb, protein_id) {
  if (is.null(cb) || is.null(cb$protein_complex)) {
    return(character(0))
  }

  matches <- cb$protein_complex[cb$protein_complex$protein_id == protein_id, , drop = FALSE]

  if (nrow(matches) == 0) {
    return(character(0))
  }

  unique(as.character(matches$complex_id))
}

#' Get complex metadata
#'
#' Returns the metadata (name, source, subunit count, category) for a complex.
#'
#' @param cb ComplexBase object
#' @param complex_id Complex identifier
#' @return Single-row data.frame with complex info, or empty data.frame if not found
#' @export
complexbase_get_complex_info <- function(cb, complex_id) {
  # Return empty data.frame with correct structure if cb is NULL
  empty_df <- data.frame(
    complex_id = character(0),
    complex_name = character(0),
    source = character(0),
    n_subunits = integer(0),
    category = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(cb) || is.null(cb$complexes)) {
    return(empty_df)
  }

  match_row <- cb$complexes[cb$complexes$complex_id == complex_id, , drop = FALSE]

  if (nrow(match_row) == 0) {
    return(empty_df)
  }

  match_row[1, , drop = FALSE]
}

#' Build term-to-proteins mapping for enrichment
#'
#' Converts the complex_proteins list from UniProt IDs to gene symbols
#' using the embedded gene_symbol column in protein_complex.
#' All enrichment queries use gene symbols, so the background must match.
#'
#' @param cb ComplexBase object
#' @return Named list: list(complex_id = c(gene_symbols...))
#' @export
complexbase_build_term_proteins <- function(cb) {
  if (is.null(cb) || is.null(cb$complex_proteins)) {
    return(list())
  }

  pc <- cb$protein_complex
  if (is.null(pc) || !"gene_symbol" %in% names(pc)) {
    # No gene_symbol column — return raw complex_proteins as-is
    return(cb$complex_proteins)
  }

  # Build UniProt → gene symbol mapping from embedded protein_complex
  embedded <- pc[!is.na(pc$gene_symbol) & nzchar(pc$gene_symbol), , drop = FALSE]
  if (nrow(embedded) == 0) {
    return(cb$complex_proteins)
  }

  mapping <- stats::setNames(
    as.character(embedded$gene_symbol),
    as.character(embedded$protein_id)
  )

  # Convert each complex's UniProt IDs to gene symbols
  term_proteins <- lapply(cb$complex_proteins, function(uniprots) {
    genes <- unname(mapping[uniprots])
    genes <- genes[!is.na(genes) & nzchar(genes)]
    unique(genes)
  })

  # Remove complexes with no mapped genes
  term_proteins <- term_proteins[vapply(term_proteins, length, integer(1)) > 0]
  term_proteins
}

# -----------------------------------------------------------------------------
# Enrichment Integration Helpers
# -----------------------------------------------------------------------------

#' Get term info for enrichment engines
#'
#' Returns complex metadata in a format compatible with enrichment engines
#' (same column names as GO term data). Enables the same enrichment logic
#' to be used for both GO and protein complex enrichment.
#'
#' @param cb ComplexBase object
#' @return data.frame with columns: term_id, term_name, ontology, n_genes
#' @export
complexbase_get_term_info <- function(cb) {
  # Return empty data.frame with correct structure if cb is NULL
  empty_df <- data.frame(
    term_id = character(0),
    term_name = character(0),
    ontology = character(0),
    n_genes = integer(0),
    stringsAsFactors = FALSE
  )

  if (is.null(cb) || is.null(cb$complexes)) {
    return(empty_df)
  }

  complexes <- cb$complexes

  # Map columns to enrichment engine expectations
  # Use unified ontology so CORUM + ComplexPortal appear as one group
  term_info <- data.frame(
    term_id = as.character(complexes$complex_id),
    term_name = as.character(complexes$complex_name),
    ontology = "Protein Complex",
    source = as.character(complexes$source),
    n_genes = as.integer(complexes$n_subunits),
    stringsAsFactors = FALSE
  )

  # Preserve category if present (useful for filtering)
  if ("category" %in% names(complexes)) {
    term_info$category <- as.character(complexes$category)
  }

  term_info
}

#' Save a ComplexBase object
#'
#' Validates and saves a ComplexBase object to disk in RDS format.
#'
#' @param cb ComplexBase object to save
#' @param path Output file path (.complexbase)
#' @return Invisible TRUE on success
#' @export
complexbase_save <- function(cb, path) {
  validation <- complexbase_validate(cb)
  if (!validation$ok) {
    stop("Cannot save invalid ComplexBase:\n  ", paste(validation$errors, collapse = "\n  "))
  }

  saveRDS(cb, file = path)
  invisible(TRUE)
}

#' Create a ComplexBase summary
#'
#' Returns summary statistics about a ComplexBase object.
#'
#' @param cb ComplexBase object
#' @return Character vector of summary lines
#' @export
complexbase_summary <- function(cb) {
  if (is.null(cb)) {
    return("NULL ComplexBase object")
  }

  validation <- complexbase_validate(cb)
  if (!validation$ok) {
    return(c("Invalid ComplexBase object:", paste0("  - ", validation$errors)))
  }

  n_complexes <- nrow(cb$complexes)
  n_proteins <- length(unique(cb$protein_complex$protein_id))
  n_mappings <- nrow(cb$protein_complex)

  # Source breakdown
  source_counts <- if ("source" %in% names(cb$complexes)) {
    table(cb$complexes$source)
  } else {
    NULL
  }

  source_str <- if (!is.null(source_counts)) {
    paste(names(source_counts), source_counts, sep = ": ", collapse = ", ")
  } else {
    "unknown"
  }

  c(
    paste0("Organism: ", cb$organism %||% "(unknown)"),
    paste0("Source: ", cb$source %||% "(unknown)"),
    paste0("Created: ", format(cb$created %||% NA)),
    paste0("Complexes: ", n_complexes),
    paste0("Unique proteins: ", n_proteins),
    paste0("Protein-complex mappings: ", n_mappings),
    paste0("Source breakdown: ", source_str)
  )
}
