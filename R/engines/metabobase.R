# R/engines/metabobase.R
# MetaboBase: Metabolite annotation database for pathway and class enrichment
#
# Parallel structure to terpbase.R but for metabolites:
#  - Pathway mappings (KEGG, Reactome) instead of GO terms
#  - Chemical class mappings (lipid classes, etc.)
#  - Cross-reference ID mapping (HMDB, KEGG, ChEBI)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

# -----------------------------
# Constants / schema
# -----------------------------
METABOBASE_SCHEMA_VERSION <- 1L

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

#' Required columns for MetaboBase CSV input
#' @return Character vector of required column names
msterp_metabobase_required_csv_cols <- function() {
  c(
    "metabolite_id",  # Primary identifier (user's choice: HMDB, KEGG, ChEBI, or name)
    "name"            # Common metabolite name
  )
}

#' Optional columns for MetaboBase CSV input
#' @return Character vector of optional column names
msterp_metabobase_optional_csv_cols <- function() {
  c(
    "hmdb_id",          # HMDB ID (HMDB0000001)
    "kegg_id",          # KEGG compound ID (C00001)
    "chebi_id",         # ChEBI ID
    "pubchem_id",       # PubChem CID
    "inchikey",         # InChIKey
    "formula",          # Molecular formula
    "class",            # Chemical class (Lipid, Amino acid, etc.)
    "subclass",         # Chemical subclass
    "superclass",       # Chemical superclass
    "pathway_kegg",     # KEGG pathway IDs (semicolon-separated)
    "pathway_reactome"  # Reactome pathway IDs (semicolon-separated)
  )
}

# -----------------------------
# Header validation for CSV
# -----------------------------
metabobase_csv_headers <- function(path) {
  hdr <- read.csv(path, nrows = 0, check.names = FALSE)
  names(hdr)
}

metabobase_validate_csv <- function(path) {
  required <- msterp_metabobase_required_csv_cols()
  cols <- metabobase_csv_headers(path)
  cols_lower <- tolower(cols)
  missing <- setdiff(tolower(required), cols_lower)
  list(ok = length(missing) == 0, required = required, present = cols, missing = missing)
}

# -----------------------------
# Build from CSV
# -----------------------------
#' Build MetaboBase from a CSV file
#' @param path Path to CSV file with metabolite annotations
#' @param library_name Optional name for this library
#' @param progress_inc Progress callback (amount, detail)
#' @param progress_set Progress callback (value, detail)
#' @return MetaboBase object
metabobase_build_from_csv <- function(
    path,
    library_name = NULL,
    progress_inc = function(amount, detail = NULL) NULL,
    progress_set = function(value, detail = NULL) NULL
) {
  required_cols <- msterp_metabobase_required_csv_cols()

  progress_set(0.05, "Reading CSV")
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  # Normalize column names to lowercase for matching
  names(raw) <- tolower(names(raw))

  missing <- setdiff(tolower(required_cols), names(raw))
  if (length(missing)) {
    stop(
      "Missing required columns in CSV:\n  ",
      paste(missing, collapse = ", "),
      "\n\nRequired columns: ", paste(required_cols, collapse = ", ")
    )
  }

  progress_inc(0.10, "Parsing columns")

  # Build base metabolite table
  df_base <- tibble(
    metabolite_id = as.character(raw[["metabolite_id"]]),
    name          = as.character(raw[["name"]] %||% raw[["metabolite_id"]]),
    hmdb_id       = as.character(raw[["hmdb_id"]] %||% NA_character_),
    kegg_id       = as.character(raw[["kegg_id"]] %||% NA_character_),
    chebi_id      = as.character(raw[["chebi_id"]] %||% NA_character_),
    pubchem_id    = as.character(raw[["pubchem_id"]] %||% NA_character_),
    inchikey      = as.character(raw[["inchikey"]] %||% NA_character_),
    formula       = as.character(raw[["formula"]] %||% NA_character_),
    class         = as.character(raw[["class"]] %||% NA_character_),
    subclass      = as.character(raw[["subclass"]] %||% NA_character_),
    superclass    = as.character(raw[["superclass"]] %||% NA_character_),
    pathway_kegg  = as.character(raw[["pathway_kegg"]] %||% NA_character_),
    pathway_reactome = as.character(raw[["pathway_reactome"]] %||% NA_character_)
  )

  # Clean up empty strings to NA
  df_base <- df_base %>%
    mutate(across(everything(), ~ifelse(. == "" | . == "NA", NA_character_, .)))

  progress_inc(0.15, "Building pathway tables")

  # Parse KEGG pathways (semicolon-separated)
  kegg_long <- df_base %>%
    filter(!is.na(pathway_kegg), pathway_kegg != "") %>%
    select(metabolite_id, pathway_kegg) %>%
    mutate(pathway_items = strsplit(pathway_kegg, ";", fixed = TRUE)) %>%
    unnest(pathway_items) %>%
    mutate(
      pathway_items = str_trim(pathway_items),
      pathway_type = "KEGG"
    ) %>%
    filter(pathway_items != "") %>%
    transmute(
      metabolite_id,
      pathway_id = pathway_items,
      pathway_type
    )

  # Parse Reactome pathways (semicolon-separated)
  reactome_long <- df_base %>%
    filter(!is.na(pathway_reactome), pathway_reactome != "") %>%
    select(metabolite_id, pathway_reactome) %>%
    mutate(pathway_items = strsplit(pathway_reactome, ";", fixed = TRUE)) %>%
    unnest(pathway_items) %>%
    mutate(
      pathway_items = str_trim(pathway_items),
      pathway_type = "Reactome"
    ) %>%
    filter(pathway_items != "") %>%
    transmute(
      metabolite_id,
      pathway_id = pathway_items,
      pathway_type
    )

  # Combine pathway annotations
  annot_long <- bind_rows(kegg_long, reactome_long) %>%
    filter(!is.na(metabolite_id), metabolite_id != "") %>%
    distinct(metabolite_id, pathway_id, pathway_type)

  # Add pathway names (will be populated by API fetch if available)
  annot_long <- annot_long %>%
    mutate(pathway_name = pathway_id)  # Default: use ID as name

  progress_inc(0.15, "Building class mappings")

  # Build chemical class mappings
  class_mappings <- df_base %>%
    filter(!is.na(class) | !is.na(subclass) | !is.na(superclass)) %>%
    select(metabolite_id, class, subclass, superclass) %>%
    distinct()

  progress_inc(0.15, "Grouping pathways")

  # Build terms_by_id (pathway term info)
  terms_by_id <- annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )

  progress_inc(0.10, "Building ID cross-reference")

  # Build ID cross-reference table
  id_xref <- df_base %>%
    select(metabolite_id, hmdb_id, kegg_id, chebi_id, pubchem_id) %>%
    filter(!is.na(metabolite_id)) %>%
    distinct()

  progress_inc(0.10, "Building output")

  # Build metabolite metadata
  metabolite_meta <- df_base %>%
    select(metabolite_id, name, hmdb_id, kegg_id, chebi_id, formula, class, subclass) %>%
    distinct()

  # Build synonyms table from name column (lowercase for matching)
  synonyms_df <- metabolite_meta %>%
    filter(!is.na(name), name != "") %>%
    transmute(metabolite_id, synonym = tolower(trimws(name))) %>%
    distinct()

  progress_set(1, "Done")

  list(
    schema_version     = METABOBASE_SCHEMA_VERSION,
    data_type          = "metabolomics",
    metabobase_version = 2L,
    library_name       = library_name %||% NULL,
    created            = Sys.time(),
    n_raw_rows         = nrow(raw),
    n_metabolites      = length(unique(df_base$metabolite_id)),
    annot_long         = annot_long,
    terms_by_id        = terms_by_id,
    class_mappings     = class_mappings,
    id_xref            = id_xref,
    metabolite_meta    = metabolite_meta,
    synonyms           = synonyms_df
  )
}

# -----------------------------
# KEGG API functions
# -----------------------------
#' Fetch pathway annotations from KEGG API
#' @param compound_ids Character vector of KEGG compound IDs (e.g., "C00001")
#' @param progress_inc Progress callback
#' @return Data frame with metabolite_id, pathway_id, pathway_name, pathway_type
metabobase_fetch_kegg <- function(
    compound_ids,
    progress_inc = function(amount, detail = NULL) NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for KEGG API access. Install with: install.packages('httr')")
  }

  # Clean compound IDs
  compound_ids <- unique(trimws(as.character(compound_ids)))
  compound_ids <- compound_ids[nzchar(compound_ids)]

  if (length(compound_ids) == 0) {
    return(tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    ))
  }

  results <- list()
  batch_size <- 10  # KEGG API limit
  n_batches <- ceiling(length(compound_ids) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(compound_ids))
    batch <- compound_ids[start_idx:end_idx]

    progress_inc(0.8 / n_batches, paste0("Fetching KEGG batch ", i, "/", n_batches))

    for (cpd_id in batch) {
      tryCatch({
        # KEGG API: get compound info including pathways
        url <- paste0("https://rest.kegg.jp/get/", cpd_id)
        resp <- httr::GET(url, httr::timeout(30))

        if (httr::status_code(resp) == 200) {
          content <- httr::content(resp, "text", encoding = "UTF-8")

          # Parse PATHWAY section
          pathway_section <- FALSE
          pathways <- list()

          for (line in strsplit(content, "\n")[[1]]) {
            if (grepl("^PATHWAY", line)) {
              pathway_section <- TRUE
              # First pathway on same line
              match <- regmatches(line, regexec("(map\\d+)\\s+(.+)", line))[[1]]
              if (length(match) == 3) {
                pathways <- c(pathways, list(c(match[2], trimws(match[3]))))
              }
            } else if (pathway_section && grepl("^\\s+", line)) {
              # Continuation of PATHWAY section
              match <- regmatches(line, regexec("(map\\d+)\\s+(.+)", line))[[1]]
              if (length(match) == 3) {
                pathways <- c(pathways, list(c(match[2], trimws(match[3]))))
              }
            } else if (pathway_section && !grepl("^\\s+", line)) {
              # End of PATHWAY section
              pathway_section <- FALSE
            }
          }

          if (length(pathways) > 0) {
            for (pw in pathways) {
              results <- c(results, list(tibble(
                metabolite_id = cpd_id,
                pathway_id = pw[1],
                pathway_name = pw[2],
                pathway_type = "KEGG"
              )))
            }
          }
        }

        Sys.sleep(0.1)  # Rate limiting
      }, error = function(e) {
        warning(paste("Failed to fetch KEGG data for", cpd_id, ":", e$message))
      })
    }
  }

  if (length(results) > 0) {
    bind_rows(results)
  } else {
    tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    )
  }
}

#' Fetch pathway annotations from Reactome API
#' @param chebi_ids Character vector of ChEBI IDs (e.g., "15377" for water)
#' @param progress_inc Progress callback
#' @return Data frame with metabolite_id, pathway_id, pathway_name, pathway_type
metabobase_fetch_reactome <- function(
    chebi_ids,
    progress_inc = function(amount, detail = NULL) NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for Reactome API access. Install with: install.packages('httr')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for Reactome API access. Install with: install.packages('jsonlite')")
  }

  # Clean ChEBI IDs (remove "CHEBI:" prefix if present)
  chebi_ids <- unique(trimws(as.character(chebi_ids)))
  chebi_ids <- chebi_ids[nzchar(chebi_ids)]
  chebi_ids <- gsub("^CHEBI:", "", chebi_ids, ignore.case = TRUE)

  if (length(chebi_ids) == 0) {
    return(tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    ))
  }

  results <- list()

  for (i in seq_along(chebi_ids)) {
    chebi_id <- chebi_ids[i]
    progress_inc(0.8 / length(chebi_ids), paste0("Fetching Reactome ", i, "/", length(chebi_ids)))

    tryCatch({
      # Reactome API: query by ChEBI ID
      url <- paste0("https://reactome.org/ContentService/data/query/CHEBI:", chebi_id, "/pathways")
      resp <- httr::GET(url, httr::timeout(30), httr::add_headers(Accept = "application/json"))

      if (httr::status_code(resp) == 200) {
        content <- httr::content(resp, "text", encoding = "UTF-8")
        pathways <- jsonlite::fromJSON(content, simplifyVector = FALSE)

        if (length(pathways) > 0) {
          for (pw in pathways) {
            results <- c(results, list(tibble(
              metabolite_id = paste0("CHEBI:", chebi_id),
              pathway_id = pw$stId %||% NA_character_,
              pathway_name = pw$displayName %||% pw$name %||% NA_character_,
              pathway_type = "Reactome"
            )))
          }
        }
      }

      Sys.sleep(0.1)  # Rate limiting
    }, error = function(e) {
      warning(paste("Failed to fetch Reactome data for CHEBI:", chebi_id, ":", e$message))
    })
  }

  if (length(results) > 0) {
    bind_rows(results) %>%
      filter(!is.na(pathway_id))
  } else {
    tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    )
  }
}

# -----------------------------
# ID mapping functions
# -----------------------------
#' Map metabolite IDs between different systems using MetaboBase
#' @param ids Character vector of IDs to map
#' @param from_type Source ID type: "hmdb", "kegg", "chebi", "pubchem", "metabolite_id"
#' @param to_type Target ID type: "hmdb", "kegg", "chebi", "pubchem", "metabolite_id"
#' @param metabobase MetaboBase object
#' @return Named character vector mapping input IDs to output IDs
metabobase_map_ids <- function(ids, from_type, to_type, metabobase) {
  if (is.null(metabobase$id_xref) || !is.data.frame(metabobase$id_xref)) {
    warning("MetaboBase has no ID cross-reference table")
    return(stats::setNames(rep(NA_character_, length(ids)), ids))
  }

  from_col <- paste0(from_type, "_id")
  to_col <- paste0(to_type, "_id")

  # Handle "metabolite_id" as special case (no "_id" suffix)
  if (from_type == "metabolite_id") from_col <- "metabolite_id"
  if (to_type == "metabolite_id") to_col <- "metabolite_id"

  if (!from_col %in% names(metabobase$id_xref)) {
    warning(paste("Source ID type not found in MetaboBase:", from_type))
    return(stats::setNames(rep(NA_character_, length(ids)), ids))
  }
  if (!to_col %in% names(metabobase$id_xref)) {
    warning(paste("Target ID type not found in MetaboBase:", to_type))
    return(stats::setNames(rep(NA_character_, length(ids)), ids))
  }

  xref <- metabobase$id_xref
  mapping <- xref[[to_col]]
  names(mapping) <- xref[[from_col]]

  result <- mapping[as.character(ids)]
  names(result) <- ids
  result
}

# -----------------------------
# Validate / load / save
# -----------------------------
metabobase_validate <- function(x) {
  errs <- character(0)
  warns <- character(0)

  if (!is.list(x)) errs <- c(errs, "MetaboBase object is not a list.")
  if (is.list(x)) {
    req_fields <- c("schema_version", "metabobase_version", "created", "annot_long", "terms_by_id", "metabolite_meta")
    missing <- setdiff(req_fields, names(x))
    if (length(missing)) errs <- c(errs, paste0("Missing fields: ", paste(missing, collapse = ", ")))

    # Validate data_type if present (optional for backwards compat)
    if ("data_type" %in% names(x)) {
      if (!identical(x$data_type, "metabolomics")) {
        errs <- c(errs, sprintf("MetaboBase data_type must be 'metabolomics', got '%s'", x$data_type))
      }
    } else {
      # Missing data_type: warn but don't error (backwards compatibility)
      warns <- c(warns, "Missing data_type field; assuming 'metabolomics' (legacy file)")
    }

    if (!("schema_version" %in% names(x)) || !is.numeric(x$schema_version)) {
      errs <- c(errs, "schema_version missing or not numeric.")
    }

    if (!("annot_long" %in% names(x)) || !is.data.frame(x$annot_long)) {
      errs <- c(errs, "annot_long missing or not a data.frame.")
    } else {
      needed <- c("metabolite_id", "pathway_id", "pathway_type")
      miss2 <- setdiff(needed, names(x$annot_long))
      if (length(miss2)) errs <- c(errs, paste0("annot_long missing columns: ", paste(miss2, collapse = ", ")))
    }

    if (!("terms_by_id" %in% names(x)) || !is.data.frame(x$terms_by_id)) {
      errs <- c(errs, "terms_by_id missing or not a data.frame.")
    } else {
      needed <- c("pathway_type", "pathway_id", "term_metabolites", "n_metabolites")
      miss2 <- setdiff(needed, names(x$terms_by_id))
      if (length(miss2)) errs <- c(errs, paste0("terms_by_id missing columns: ", paste(miss2, collapse = ", ")))
    }

    if (!("metabolite_meta" %in% names(x)) || !is.data.frame(x$metabolite_meta)) {
      errs <- c(errs, "metabolite_meta missing or not a data.frame.")
    } else {
      needed <- c("metabolite_id", "name")
      miss2 <- setdiff(needed, names(x$metabolite_meta))
      if (length(miss2)) errs <- c(errs, paste0("metabolite_meta missing columns: ", paste(miss2, collapse = ", ")))
    }
  }

  list(ok = length(errs) == 0, errors = errs, warnings = warns)
}

metabobase_save <- function(x, file) {
  v <- metabobase_validate(x)
  if (!v$ok) stop(paste(v$errors, collapse = "\n"))
  saveRDS(x, file = file)
  invisible(TRUE)
}

metabobase_load <- function(file) {
  x <- readRDS(file)
  v <- metabobase_validate(x)
  if (!v$ok) stop(paste(v$errors, collapse = "\n"))
  x
}

metabobase_summary_lines <- function(x) {
  v <- metabobase_validate(x)
  if (!v$ok) return(c("Invalid MetaboBase object:", paste0("- ", v$errors)))

  n_ids <- length(unique(x$annot_long$metabolite_id))

  pathway_counts <- x$terms_by_id %>%
    count(pathway_type, name = "n_pathways") %>%
    arrange(pathway_type)

  pathway_str <- if (nrow(pathway_counts)) {
    paste(pathway_counts$pathway_type, pathway_counts$n_pathways, sep = ": ", collapse = ", ")
  } else {
    "none"
  }

  class_count <- if (!is.null(x$class_mappings) && nrow(x$class_mappings) > 0) {
    length(unique(x$class_mappings$class[!is.na(x$class_mappings$class)]))
  } else {
    0
  }

  c(
    paste0("Library name: ", x$library_name %||% "(none)"),
    paste0("Created: ", format(x$created)),
    paste0("Rows in source: ", x$n_raw_rows),
    paste0("Total metabolites: ", x$n_metabolites %||% n_ids),
    paste0("Metabolites with pathway annotations: ", n_ids),
    paste0("Pathways: ", pathway_str),
    paste0("Chemical classes: ", class_count)
  )
}

# -----------------------------
# Multi-layer ID Reconciliation
# -----------------------------
#' Reconcile query metabolite IDs against a MetaboBase
#'
#' Tries 5 layers: (1) direct match, (2) name match, (3) synonym match,
#' (4) cross-reference match (HMDB, KEGG, ChEBI, PubChem CID, CAS),
#' (5) PubChem name resolver (optional, API calls).
#'
#' @param query_ids Character vector of query metabolite identifiers
#' @param metabobase MetaboBase object
#' @param use_pubchem_resolver Logical; if TRUE, attempt PubChem synonym-based resolution for unmatched names
#' @param log_callback Function(level, msg) for logging; default is silent
#' @return list(id_map, resolution_layer, stats) where id_map is named vector
#'         query_id → metabolite_id, resolution_layer is named vector
#'         query_id → layer_name (one of "direct", "name", "synonym",
#'         "xref:<col>", "pubchem"), stats is list of counts per layer
metabobase_reconcile_ids <- function(query_ids, metabobase,
                                      use_pubchem_resolver = FALSE,
                                      log_callback = function(level, msg) NULL) {
  add_log <- log_callback

  # Background universe: all metabolite IDs with pathway annotations

  background <- unique(metabobase$annot_long$metabolite_id)

  # Layer 1: Direct match
  direct_matches <- intersect(query_ids, background)
  n_direct <- length(direct_matches)
  unmatched <- setdiff(query_ids, background)

  # Track mapping: original_query_id -> matched_metabolite_id
  query_id_map <- stats::setNames(direct_matches, direct_matches)
  # Track which resolution layer matched each query ID
  resolution_layer <- stats::setNames(rep("direct", length(direct_matches)), direct_matches)
  n_by_name <- 0L
  n_by_synonym <- 0L
  n_by_xref <- 0L

  if (length(unmatched) > 0) {
    unmatched_lower <- tolower(trimws(unmatched))

    # Layer 2: Name match (case-insensitive against metabolite_meta$name)
    metabolite_meta <- metabobase$metabolite_meta
    if (!is.null(metabolite_meta) && is.data.frame(metabolite_meta) &&
        "name" %in% names(metabolite_meta) && "metabolite_id" %in% names(metabolite_meta)) {
      name_to_id <- stats::setNames(metabolite_meta$metabolite_id, tolower(trimws(metabolite_meta$name)))
      name_to_id <- name_to_id[!is.na(names(name_to_id)) & nzchar(names(name_to_id))]
      name_hits <- name_to_id[unmatched_lower]
      found_idx <- !is.na(name_hits)
      if (any(found_idx)) {
        new_map <- stats::setNames(as.character(name_hits[found_idx]), unmatched[found_idx])
        query_id_map <- c(query_id_map, new_map)
        resolution_layer <- c(resolution_layer, stats::setNames(rep("name", length(new_map)), names(new_map)))
        n_by_name <- sum(found_idx)
        unmatched <- unmatched[!found_idx]
        unmatched_lower <- unmatched_lower[!found_idx]
      }
    }

    # Layer 3: Synonym match (case-insensitive against metabobase$synonyms)
    synonyms <- metabobase$synonyms
    if (length(unmatched) > 0 && !is.null(synonyms) && is.data.frame(synonyms) && nrow(synonyms) > 0 &&
        "synonym" %in% names(synonyms) && "metabolite_id" %in% names(synonyms)) {
      syn_to_id <- stats::setNames(synonyms$metabolite_id, synonyms$synonym)
      # Remove duplicates (keep first)
      syn_to_id <- syn_to_id[!duplicated(names(syn_to_id))]
      syn_hits <- syn_to_id[unmatched_lower]
      found_idx <- !is.na(syn_hits)
      if (any(found_idx)) {
        new_map <- stats::setNames(as.character(syn_hits[found_idx]), unmatched[found_idx])
        query_id_map <- c(query_id_map, new_map)
        resolution_layer <- c(resolution_layer, stats::setNames(rep("synonym", length(new_map)), names(new_map)))
        n_by_synonym <- sum(found_idx)
        unmatched <- unmatched[!found_idx]
        unmatched_lower <- unmatched_lower[!found_idx]
      }
    }

    # Layer 4: Cross-reference match (HMDB, ChEBI, PubChem CID, PubChem SID, CAS, KEGG)
    id_xref <- metabobase$id_xref
    if (length(unmatched) > 0 && !is.null(id_xref) && is.data.frame(id_xref) && nrow(id_xref) > 0) {
      xref_cols <- intersect(
        c("hmdb_id", "chebi_id", "pubchem_cid", "pubchem_id", "cas_id", "kegg_id"),
        names(id_xref)
      )
      for (xc in xref_cols) {
        if (length(unmatched) == 0) break
        xref_vals <- id_xref[[xc]]
        if (all(is.na(xref_vals))) next
        xref_map <- stats::setNames(id_xref$metabolite_id, tolower(trimws(as.character(xref_vals))))
        xref_map <- xref_map[!is.na(names(xref_map)) & nzchar(names(xref_map))]
        xref_hits <- xref_map[unmatched_lower]
        found_idx <- !is.na(xref_hits)
        if (any(found_idx)) {
          new_map <- stats::setNames(as.character(xref_hits[found_idx]), unmatched[found_idx])
          query_id_map <- c(query_id_map, new_map)
          resolution_layer <- c(resolution_layer, stats::setNames(rep(paste0("xref:", xc), length(new_map)), names(new_map)))
          n_by_xref <- n_by_xref + sum(found_idx)
          unmatched <- unmatched[!found_idx]
          unmatched_lower <- unmatched_lower[!found_idx]
        }
      }
    }
  }

  # Layer 5: PubChem synonym resolver (API calls for remaining unmatched)
  # Fetches PubChem synonyms for each unmatched name, then matches those
  # synonyms against the metabobase's name/synonym/xref tables.
  # Works for any metabobase (HMDB, KEGG, CSV). Results are cached locally.
  n_by_pubchem <- 0L
  if (length(unmatched) > 0 && isTRUE(use_pubchem_resolver)) {
    tryCatch({
      resolver_path <- file.path("R", "engines", "pubchem_resolver.R")
      if (!file.exists(resolver_path)) {
        for (try_path in c(resolver_path, file.path("..", "..", resolver_path))) {
          if (file.exists(try_path)) { resolver_path <- try_path; break }
        }
      }
      if (file.exists(resolver_path)) {
        source(resolver_path, local = TRUE)
        add_log("INFO", sprintf("PubChem synonym resolver: querying %d unmatched names",
                                length(unmatched)))
        resolved <- pubchem_synonym_resolve_names(
          unmatched, metabobase,
          progress_callback = function(msg) add_log("INFO", msg)
        )
        if (length(resolved) > 0) {
          # Filter to IDs that exist in the pathway background universe
          resolved <- resolved[resolved %in% background]
          if (length(resolved) > 0) {
            new_map <- stats::setNames(as.character(resolved), names(resolved))
            query_id_map <- c(query_id_map, new_map)
            resolution_layer <- c(resolution_layer, stats::setNames(rep("pubchem", length(new_map)), names(new_map)))
            n_by_pubchem <- length(resolved)
            unmatched <- setdiff(unmatched, names(resolved))
          }
        }
      } else {
        add_log("WARN", "PubChem resolver requested but pubchem_resolver.R not found")
      }
    }, error = function(e) {
      add_log("WARN", sprintf("PubChem synonym resolver failed: %s", e$message))
    })
  }

  n_unmatched <- length(unmatched)
  add_log("INFO", sprintf(
    "ID reconciliation: %d direct, %d by name, %d by synonym, %d by xref, %d by pubchem, %d unmatched (of %d query)",
    n_direct, n_by_name, n_by_synonym, n_by_xref, n_by_pubchem, n_unmatched, length(query_ids)))

  if (n_unmatched > 0 && n_unmatched <= 10) {
    add_log("DEBUG", sprintf("Unmatched metabolites: %s", paste(unmatched, collapse = ", ")))
  } else if (n_unmatched > 10) {
    add_log("DEBUG", sprintf("Unmatched metabolites (first 10): %s", paste(head(unmatched, 10), collapse = ", ")))
  }

  list(
    id_map = query_id_map,
    resolution_layer = resolution_layer,
    unmatched = unmatched,
    stats = list(
      n_query = length(query_ids),
      n_direct = n_direct,
      n_by_name = n_by_name,
      n_by_synonym = n_by_synonym,
      n_by_xref = n_by_xref,
      n_by_pubchem = n_by_pubchem,
      n_unmatched = n_unmatched
    )
  )
}

#' Merge pathway annotations into an existing MetaboBase
#' @param metabobase Existing MetaboBase object
#' @param new_annotations Data frame with columns: metabolite_id, pathway_id, pathway_name, pathway_type
#' @return Updated MetaboBase object
metabobase_merge_annotations <- function(metabobase, new_annotations) {
  if (!is.data.frame(new_annotations) || nrow(new_annotations) == 0) {
    return(metabobase)
  }

  # Merge into annot_long
  metabobase$annot_long <- bind_rows(
    metabobase$annot_long,
    new_annotations %>%
      select(any_of(c("metabolite_id", "pathway_id", "pathway_name", "pathway_type")))
  ) %>%
    distinct(metabolite_id, pathway_id, pathway_type, .keep_all = TRUE)

  # Rebuild terms_by_id
  metabobase$terms_by_id <- metabobase$annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )

  metabobase
}

# -----------------------------
# Pre-built Library Functions
# -----------------------------

#' List available pre-built MetaboBase libraries
#' @param metabobase_dir Directory containing .metabobase files (default: metabobase/)
#' @return Data frame with library info (name, organism, pathways, file)
metabobase_list_libraries <- function(metabobase_dir = NULL) {
  if (is.null(metabobase_dir)) {
    # Try to find metabobase directory relative to app
    # Check user data folder first (updated databases), then bundled locations
    user_db_dir <- file.path(
      Sys.getenv("MSTERP_USER_DATA", unset = file.path(Sys.getenv("APPDATA", unset = ""), "MSTerp")),
      "databases", "metabobase"
    )
    candidates <- c(
      user_db_dir,
      "metabobase",
      file.path(getwd(), "metabobase"),
      file.path(dirname(getwd()), "metabobase")
    )
    for (cand in candidates) {
      if (dir.exists(cand)) {
        metabobase_dir <- cand
        break
      }
    }
  }

  if (is.null(metabobase_dir) || !dir.exists(metabobase_dir)) {
    return(data.frame(
      name = character(0),
      organism = character(0),
      n_metabolites = integer(0),
      n_pathways = integer(0),
      file = character(0),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(metabobase_dir, pattern = "\\.metabobase$", full.names = TRUE)

  if (length(files) == 0) {
    return(data.frame(
      name = character(0),
      organism = character(0),
      n_metabolites = integer(0),
      n_pathways = integer(0),
      file = character(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- lapply(files, function(f) {
    tryCatch({
      mb <- readRDS(f)
      data.frame(
        name = mb$library_name %||% basename(f),
        organism = mb$organism %||% "unknown",
        n_metabolites = mb$n_metabolites %||% length(unique(mb$annot_long$metabolite_id)),
        n_pathways = nrow(mb$terms_by_id),
        file = f,
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  do.call(rbind, Filter(Negate(is.null), results))
}

#' Build a complete KEGG pathway library for an organism
#' @param organism KEGG organism code (e.g., "hsa" for human, "mmu" for mouse)
#' @param library_name Name for the library
#' @param output_file Output file path (optional, will save if provided)
#' @param progress_callback Progress callback function(message)
#' @return MetaboBase object with KEGG pathways
metabobase_build_kegg_library <- function(
    organism = "hsa",
    library_name = NULL,
    output_file = NULL,
    progress_callback = function(msg) message(msg)
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install with: install.packages('httr')")
  }

  # Note: KEGG compound/pathway associations are universal (not organism-specific)

  # The organism parameter is kept for metadata purposes but the actual metabolite
  # database is built from reference pathways which apply across all organisms.
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

  org_name <- organism_names[organism] %||% organism
  library_name <- library_name %||% paste0("KEGG ", org_name)

  progress_callback(paste0("Building KEGG library for ", org_name))
  progress_callback("Note: KEGG metabolite pathways are universal across organisms")

 # Step 1: Get all reference metabolic pathways (map* pathways have compound links)
  # Organism-specific pathways (hsa*, mmu*) link to genes, not compounds
  # We use reference pathways and filter to metabolic ones
  progress_callback("Fetching reference pathway list from KEGG...")
  pathway_url <- "https://rest.kegg.jp/list/pathway"
  resp <- httr::GET(pathway_url, httr::timeout(60))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch KEGG pathway list")
  }

  pathway_text <- httr::content(resp, "text", encoding = "UTF-8")
  pathway_lines <- strsplit(pathway_text, "\n")[[1]]
  pathway_lines <- pathway_lines[nzchar(pathway_lines)]

  pathways <- lapply(pathway_lines, function(line) {
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) >= 2) {
      pw_id <- gsub("^path:", "", parts[1])
      # Only keep reference pathways (map*) - these have compound associations
      if (grepl("^map", pw_id)) {
        list(
          id = pw_id,
          name = parts[2]
        )
      } else NULL
    } else NULL
  })
  pathways <- Filter(Negate(is.null), pathways)

  progress_callback(sprintf("Found %d reference pathways", length(pathways)))

  # Step 2: Get compounds for each pathway
  all_annotations <- list()
  all_compounds <- list()

  for (i in seq_along(pathways)) {
    pw <- pathways[[i]]

    if (i %% 10 == 0 || i == length(pathways)) {
      progress_callback(sprintf("Processing pathway %d/%d: %s", i, length(pathways), pw$name))
    }

    # Get pathway compounds using reference pathway ID
    cpd_url <- paste0("https://rest.kegg.jp/link/compound/", pw$id)
    cpd_resp <- httr::GET(cpd_url, httr::timeout(30))

    if (httr::status_code(cpd_resp) == 200) {
      cpd_text <- httr::content(cpd_resp, "text", encoding = "UTF-8")
      cpd_lines <- strsplit(cpd_text, "\n")[[1]]
      cpd_lines <- cpd_lines[nzchar(cpd_lines)]

      for (cpd_line in cpd_lines) {
        parts <- strsplit(cpd_line, "\t")[[1]]
        if (length(parts) >= 2) {
          cpd_id <- gsub("^cpd:", "", parts[2])
          all_annotations <- c(all_annotations, list(data.frame(
            metabolite_id = cpd_id,
            pathway_id = pw$id,
            pathway_name = pw$name,
            pathway_type = "KEGG",
            stringsAsFactors = FALSE
          )))
          all_compounds[[cpd_id]] <- TRUE
        }
      }
    }

    Sys.sleep(0.1)  # Rate limiting
  }

  if (length(all_annotations) == 0) {
    stop("No compound-pathway associations found")
  }

  annot_long <- do.call(rbind, all_annotations)
  progress_callback(sprintf("Found %d compound-pathway associations", nrow(annot_long)))

  # Step 3: Get compound names, synonyms, cross-references, and formulas
  progress_callback("Fetching compound details (names, synonyms, cross-refs)...")
  compound_ids <- names(all_compounds)
  compound_names <- stats::setNames(compound_ids, compound_ids)  # Default: ID as name
  compound_formulas <- stats::setNames(rep(NA_character_, length(compound_ids)), compound_ids)
  all_synonyms <- list()     # list of data.frames: metabolite_id, synonym
  all_dblinks <- list()      # list of named lists: cpd_id -> list(pubchem, chebi, cas)

  # Helper: parse a single KEGG compound record (text block for one compound)
  parse_kegg_compound <- function(record_text) {
    lines <- strsplit(record_text, "\n")[[1]]
    result <- list(cpd_id = NULL, name = NULL, synonyms = character(0),
                   formula = NULL, dblinks = list())

    # Extract ENTRY to get compound ID
    entry_line <- grep("^ENTRY", lines, value = TRUE)
    if (length(entry_line) > 0) {
      entry_match <- regmatches(entry_line[1], regexec("(C\\d{5})", entry_line[1]))[[1]]
      if (length(entry_match) >= 2) result$cpd_id <- entry_match[2]
    }
    if (is.null(result$cpd_id)) return(NULL)

    # --- Parse NAME field (multi-line, semicolon-separated synonyms) ---
    name_lines <- character(0)
    in_name <- FALSE
    for (line in lines) {
      if (grepl("^NAME", line)) {
        in_name <- TRUE
        name_lines <- c(name_lines, sub("^NAME\\s+", "", line))
      } else if (in_name && grepl("^\\s+", line)) {
        name_lines <- c(name_lines, trimws(line))
      } else if (in_name) {
        break
      }
    }
    if (length(name_lines) > 0) {
      name_text <- paste(name_lines, collapse = " ")
      syns <- trimws(strsplit(name_text, ";")[[1]])
      syns <- syns[nzchar(syns)]
      if (length(syns) > 0) {
        result$name <- syns[1]
        result$synonyms <- syns
      }
    }

    # --- Parse FORMULA field ---
    for (line in lines) {
      if (grepl("^FORMULA", line)) {
        result$formula <- trimws(sub("^FORMULA\\s+", "", line))
        break
      }
    }

    # --- Parse DBLINKS section (multi-line) ---
    dblink_lines <- character(0)
    in_dblinks <- FALSE
    for (line in lines) {
      if (grepl("^DBLINKS", line)) {
        in_dblinks <- TRUE
        dblink_lines <- c(dblink_lines, sub("^DBLINKS\\s+", "", line))
      } else if (in_dblinks && grepl("^\\s+", line)) {
        dblink_lines <- c(dblink_lines, trimws(line))
      } else if (in_dblinks) {
        break
      }
    }
    for (dl in dblink_lines) {
      dl_match <- regmatches(dl, regexec("^([^:]+):\\s*(.+)$", dl))[[1]]
      if (length(dl_match) >= 3) {
        db_name <- trimws(dl_match[2])
        db_val <- trimws(strsplit(dl_match[3], "\\s+")[[1]][1])
        result$dblinks[[db_name]] <- db_val
      }
    }

    result
  }

  # Batch fetch compound info using KEGG multi-compound GET (up to 10 per request)
  batch_size <- 10
  n_batches <- ceiling(length(compound_ids) / batch_size)
  max_retries <- 3
  n_parsed <- 0

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(compound_ids))
    batch <- compound_ids[start_idx:end_idx]

    if (i %% 5 == 0 || i == n_batches) {
      progress_callback(sprintf("Fetching compound info batch %d/%d (%d parsed so far)",
                                i, n_batches, n_parsed))
    }

    # Use KEGG batch endpoint: /get/C00001+C00002+C00003+...
    batch_url <- paste0("https://rest.kegg.jp/get/", paste(batch, collapse = "+"))
    batch_text <- NULL

    for (attempt in seq_len(max_retries)) {
      tryCatch({
        resp <- httr::GET(batch_url, httr::timeout(60))
        status <- httr::status_code(resp)

        if (status == 200) {
          batch_text <- httr::content(resp, "text", encoding = "UTF-8")
          break
        } else if (status == 403) {
          # Rate limited — wait and retry
          Sys.sleep(2 * attempt)
        }
      }, error = function(e) NULL)
    }

    if (!is.null(batch_text) && nzchar(batch_text)) {
      # Split multi-compound response by "///" delimiter
      records <- strsplit(batch_text, "///")[[1]]

      for (record in records) {
        record <- trimws(record)
        if (!nzchar(record)) next

        parsed <- tryCatch(parse_kegg_compound(record), error = function(e) NULL)
        if (is.null(parsed) || is.null(parsed$cpd_id)) next

        cpd_id <- parsed$cpd_id
        n_parsed <- n_parsed + 1

        # Store name
        if (!is.null(parsed$name)) {
          compound_names[cpd_id] <- parsed$name
        }

        # Store synonyms
        for (s in parsed$synonyms) {
          all_synonyms <- c(all_synonyms, list(data.frame(
            metabolite_id = cpd_id,
            synonym = tolower(trimws(s)),
            stringsAsFactors = FALSE
          )))
        }

        # Store formula
        if (!is.null(parsed$formula)) {
          compound_formulas[cpd_id] <- parsed$formula
        }

        # Store dblinks
        if (length(parsed$dblinks) > 0) {
          all_dblinks[[cpd_id]] <- parsed$dblinks
        }
      }
    }

    # Rate limiting: 0.3s between batch requests to avoid 403s
    Sys.sleep(0.3)
  }

  # Build synonyms table
  synonyms_df <- if (length(all_synonyms) > 0) {
    syn_df <- do.call(rbind, all_synonyms)
    syn_df <- syn_df[!duplicated(paste0(syn_df$metabolite_id, ":", syn_df$synonym)), , drop = FALSE]
    rownames(syn_df) <- NULL
    syn_df
  } else {
    data.frame(metabolite_id = character(0), synonym = character(0), stringsAsFactors = FALSE)
  }

  progress_callback(sprintf("Parsed %d synonyms across %d compounds", nrow(synonyms_df), length(compound_ids)))

  # Build metabolite_meta (with formula)
  metabolite_meta <- data.frame(
    metabolite_id = compound_ids,
    name = compound_names[compound_ids],
    kegg_id = compound_ids,
    formula = compound_formulas[compound_ids],
    stringsAsFactors = FALSE
  )

  # Build terms_by_id
  terms_by_id <- annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )

  # Build id_xref with cross-references from DBLINKS
  id_xref <- data.frame(
    metabolite_id = compound_ids,
    kegg_id = compound_ids,
    pubchem_id = NA_character_,
    chebi_id = NA_character_,
    cas_id = NA_character_,
    stringsAsFactors = FALSE
  )
  for (cpd_id in names(all_dblinks)) {
    links <- all_dblinks[[cpd_id]]
    idx <- match(cpd_id, id_xref$metabolite_id)
    if (!is.na(idx)) {
      if (!is.null(links[["PubChem"]])) id_xref$pubchem_id[idx] <- links[["PubChem"]]
      if (!is.null(links[["ChEBI"]])) id_xref$chebi_id[idx] <- links[["ChEBI"]]
      if (!is.null(links[["CAS"]])) id_xref$cas_id[idx] <- links[["CAS"]]
    }
  }

  progress_callback(sprintf("Cross-references: %d PubChem SIDs, %d ChEBI, %d CAS",
    sum(!is.na(id_xref$pubchem_id)), sum(!is.na(id_xref$chebi_id)), sum(!is.na(id_xref$cas_id))))

  # Convert PubChem SIDs → CIDs for name resolver compatibility
  # KEGG DBLINKS stores PubChem Substance IDs (SIDs), but PubChem name lookup
  # returns Compound IDs (CIDs). We need CIDs for the PubChem name resolver.
  id_xref$pubchem_cid <- NA_character_
  sids_valid <- id_xref$pubchem_id[!is.na(id_xref$pubchem_id)]
  if (length(sids_valid) > 0) {
    progress_callback(sprintf("Converting %d PubChem SIDs to CIDs...", length(sids_valid)))
    sid_to_cid <- character(0)
    batch_sz <- 200  # PubChem allows up to 200 SIDs per request
    sid_batches <- split(sids_valid, ceiling(seq_along(sids_valid) / batch_sz))

    n_batches <- length(sid_batches)
    n_failed <- 0L
    for (b_idx in seq_along(sid_batches)) {
      batch_sids <- sid_batches[[b_idx]]
      sid_str <- paste(batch_sids, collapse = ",")
      cid_url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sid/",
                        sid_str, "/cids/JSON")

      # Retry up to 3 times with exponential backoff for 503/429 errors
      success <- FALSE
      for (attempt in 1:3) {
        tryCatch({
          cid_resp <- httr::GET(cid_url, httr::timeout(60))
          status <- httr::status_code(cid_resp)

          if (status == 200) {
            cid_body <- jsonlite::fromJSON(
              httr::content(cid_resp, "text", encoding = "UTF-8"),
              simplifyVector = FALSE
            )
            info_list <- cid_body$InformationList$Information
            if (is.list(info_list)) {
              for (info in info_list) {
                sid_val <- as.character(info$SID)
                cid_vals <- info$CID
                if (!is.null(cid_vals) && length(cid_vals) > 0) {
                  sid_to_cid[sid_val] <- as.character(cid_vals[[1]])
                }
              }
            }
            success <- TRUE
          } else if (status %in% c(503, 429)) {
            Sys.sleep(2^attempt)  # 2s, 4s, 8s backoff
            next
          }
        }, error = function(e) NULL)
        if (success) break
      }
      if (!success) n_failed <- n_failed + 1L

      Sys.sleep(0.5)  # Conservative rate limit (2 req/sec)

      if (b_idx %% 5 == 0 || b_idx == n_batches) {
        progress_callback(sprintf("SID->CID batch %d/%d (%d mapped, %d failed)",
                                  b_idx, n_batches, length(sid_to_cid), n_failed))
      }
    }

    # Populate pubchem_cid column
    if (length(sid_to_cid) > 0) {
      for (i in which(!is.na(id_xref$pubchem_id))) {
        sid <- id_xref$pubchem_id[i]
        cid <- sid_to_cid[sid]
        if (!is.na(cid)) id_xref$pubchem_cid[i] <- cid
      }
      progress_callback(sprintf("Mapped %d PubChem CIDs (from %d SIDs)",
                                sum(!is.na(id_xref$pubchem_cid)), length(sids_valid)))
    }
  }

  # Build final metabobase
  metabobase <- list(
    schema_version = METABOBASE_SCHEMA_VERSION,
    data_type = "metabolomics",
    metabobase_version = 2L,
    library_name = library_name,
    organism = org_name,
    organism_code = organism,
    source = "KEGG",
    created = Sys.time(),
    n_raw_rows = nrow(annot_long),
    n_metabolites = length(compound_ids),
    annot_long = annot_long,
    terms_by_id = terms_by_id,
    class_mappings = data.frame(
      metabolite_id = character(0),
      class = character(0),
      subclass = character(0),
      superclass = character(0),
      stringsAsFactors = FALSE
    ),
    id_xref = id_xref,
    metabolite_meta = metabolite_meta,
    synonyms = synonyms_df
  )

  progress_callback(sprintf("Built MetaboBase: %d metabolites, %d pathways",
                            length(compound_ids), nrow(terms_by_id)))

  # Save if output file provided
 if (!is.null(output_file)) {
    metabobase_save(metabobase, output_file)
    progress_callback(sprintf("Saved to: %s", output_file))
  }

  metabobase
}

#' Build a MetaboBase from HMDB XML file
#'
#' Parses the HMDB hmdb_metabolites.xml file (all ~220K metabolites) and builds a
#' MetaboBase with SMPDB pathway annotations and KEGG cross-references.
#' All metabolites are included for maximum name/synonym coverage; only those with
#' pathway annotations contribute to enrichment results.
#'
#' @param xml_path Path to hmdb_metabolites.xml file
#' @param library_name Name for the library (default: "HMDB Human")
#' @param output_file Output file path (optional, will save if provided)
#' @param progress_callback Progress callback function(message)
#' @return MetaboBase object with SMPDB/KEGG pathways
metabobase_build_hmdb_library <- function(
    xml_path,
    library_name = NULL,
    output_file = NULL,
    progress_callback = function(msg) message(msg)
) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required for HMDB XML parsing. Install with: install.packages('xml2')")
  }

  library_name <- library_name %||% "HMDB Human"

  # ------------------------------------------------------------------
  # Bulk read + strsplit parser: read the entire file as a string,
  # split on </metabolite> boundaries, regex-extract fields from
  # each small text chunk. Avoids slow DOM parsing and slow
  # line-by-line R loops on 6GB files.
  # ------------------------------------------------------------------

  # ------------------------------------------------------------------
  # Vectorized extraction: read all lines, use vectorized regex to
  # extract ALL tag values at once, then assign to metabolite blocks
  # using metabolite boundary indices. Avoids per-metabolite loops.
  # ------------------------------------------------------------------
  progress_callback("Reading entire HMDB XML into memory (readLines)...")
  file_size <- file.info(xml_path)$size
  progress_callback(sprintf("File size: %.2f GB", file_size / 1024^3))

  all_lines <- readLines(xml_path, warn = FALSE, encoding = "UTF-8")
  n_lines <- length(all_lines)
  progress_callback(sprintf("Read %d lines. Extracting fields with vectorized regex...", n_lines))

  # Helper: vectorized extraction of <tag>value</tag> from all_lines
  # Returns a data.frame(line_idx, value) for all lines containing the tag
  extract_field_vec <- function(tag) {
    pat <- paste0("<", tag, ">([^<]+)</", tag, ">")
    hits <- grep(pat, all_lines, perl = TRUE)
    if (length(hits) == 0) return(data.frame(idx = integer(0), val = character(0), stringsAsFactors = FALSE))
    vals <- sub(paste0(".*<", tag, ">([^<]+)</", tag, ">.*"), "\\1", all_lines[hits], perl = TRUE)
    data.frame(idx = hits, val = trimws(vals), stringsAsFactors = FALSE)
  }

  # Find metabolite block boundaries
  progress_callback("Finding metabolite boundaries...")
  met_starts <- which(grepl("<metabolite>", all_lines, fixed = TRUE))
  met_ends <- which(grepl("</metabolite>", all_lines, fixed = TRUE))

  if (length(met_starts) == 0 || length(met_ends) == 0) {
    stop("No <metabolite> blocks found in XML")
  }
  n_pairs <- min(length(met_starts), length(met_ends))
  met_starts <- met_starts[1:n_pairs]
  met_ends <- met_ends[1:n_pairs]
  n_metabolites_total <- n_pairs
  progress_callback(sprintf("Found %d metabolite blocks", n_metabolites_total))

  # Assign each line index to its metabolite block index using findInterval
  # met_starts[k] <= line_idx < met_starts[k+1] => block k
  assign_block <- function(line_indices) {
    findInterval(line_indices, met_starts)
  }

  # Extract all fields vectorized
  progress_callback("Extracting accession IDs...")
  df_accession <- extract_field_vec("accession")

  progress_callback("Extracting names...")
  df_name <- extract_field_vec("name")

  progress_callback("Extracting cross-references (kegg_id, pubchem, chebi, cas, formula)...")
  df_kegg <- extract_field_vec("kegg_id")
  df_pubchem <- extract_field_vec("pubchem_compound_id")
  df_chebi <- extract_field_vec("chebi_id")
  df_cas <- extract_field_vec("cas_registry_number")
  df_formula <- extract_field_vec("chemical_formula")

  progress_callback("Extracting taxonomy...")
  df_superclass <- extract_field_vec("super_class")
  df_class <- extract_field_vec("class")
  df_subclass <- extract_field_vec("sub_class")

  progress_callback("Extracting synonyms...")
  df_synonym <- extract_field_vec("synonym")

  progress_callback("Extracting pathway fields...")
  df_smpdb <- extract_field_vec("smpdb_id")
  df_kegg_map <- extract_field_vec("kegg_map_id")
  # For pathway names, we need the <name> tags inside <pathway> blocks
  # Find <pathway> and </pathway> boundaries
  pw_starts <- which(grepl("<pathway>", all_lines, fixed = TRUE))
  pw_ends <- which(grepl("</pathway>", all_lines, fixed = TRUE))

  progress_callback("All fields extracted. Building per-metabolite tables...")

  # Free the lines vector — we have all data in field vectors now
  rm(all_lines)
  gc()

  # Assign block indices to each field
  df_accession$block <- assign_block(df_accession$idx)
  df_name$block <- assign_block(df_name$idx)
  df_kegg$block <- assign_block(df_kegg$idx)
  df_pubchem$block <- assign_block(df_pubchem$idx)
  df_chebi$block <- assign_block(df_chebi$idx)
  df_cas$block <- assign_block(df_cas$idx)
  df_formula$block <- assign_block(df_formula$idx)
  df_superclass$block <- assign_block(df_superclass$idx)
  df_class$block <- assign_block(df_class$idx)
  df_subclass$block <- assign_block(df_subclass$idx)
  df_synonym$block <- assign_block(df_synonym$idx)
  df_smpdb$block <- assign_block(df_smpdb$idx)
  df_kegg_map$block <- assign_block(df_kegg_map$idx)

  progress_callback("Assigned block indices. Building metabolite_meta...")

  # For fields that appear once per metabolite, get the FIRST occurrence per block
  # (accession always appears first inside <metabolite>)
  first_per_block <- function(df) {
    df[!duplicated(df$block), , drop = FALSE]
  }

  acc <- first_per_block(df_accession)

  # Build lookup: block_index -> hmdb_id
  block_to_hmdb <- stats::setNames(acc$val, as.character(acc$block))
  valid_blocks <- as.integer(names(block_to_hmdb))

  # For single-value fields, get first per block and lookup
  get_first_val <- function(df, blocks) {
    fp <- first_per_block(df)
    vals <- stats::setNames(fp$val, as.character(fp$block))
    unname(vals[as.character(blocks)])
  }

  # Build metabolite_meta
  metabolite_meta <- data.frame(
    metabolite_id = block_to_hmdb[as.character(valid_blocks)],
    name = get_first_val(df_name, valid_blocks),
    hmdb_id = block_to_hmdb[as.character(valid_blocks)],
    kegg_id = get_first_val(df_kegg, valid_blocks),
    chebi_id = get_first_val(df_chebi, valid_blocks),
    formula = get_first_val(df_formula, valid_blocks),
    stringsAsFactors = FALSE
  )
  rownames(metabolite_meta) <- NULL
  # Replace empty string with NA
  for (col in names(metabolite_meta)) {
    metabolite_meta[[col]][!nzchar(metabolite_meta[[col]])] <- NA_character_
  }

  n_metabolites_final <- nrow(metabolite_meta)
  progress_callback(sprintf("Built metabolite_meta: %d metabolites", n_metabolites_final))

  # Build id_xref
  id_xref <- data.frame(
    metabolite_id = metabolite_meta$metabolite_id,
    hmdb_id = metabolite_meta$hmdb_id,
    kegg_id = metabolite_meta$kegg_id,
    chebi_id = metabolite_meta$chebi_id,
    pubchem_cid = get_first_val(df_pubchem, valid_blocks),
    cas_id = get_first_val(df_cas, valid_blocks),
    stringsAsFactors = FALSE
  )

  # Build class_mappings
  class_mappings <- data.frame(
    metabolite_id = metabolite_meta$metabolite_id,
    superclass = get_first_val(df_superclass, valid_blocks),
    class = get_first_val(df_class, valid_blocks),
    subclass = get_first_val(df_subclass, valid_blocks),
    stringsAsFactors = FALSE
  )
  class_mappings <- class_mappings[
    !is.na(class_mappings$class) | !is.na(class_mappings$subclass) | !is.na(class_mappings$superclass),
    , drop = FALSE
  ]
  rownames(class_mappings) <- NULL

  progress_callback("Building synonyms table...")

  # Build synonyms: synonym -> metabolite_id (many per block)
  syn_hmdb <- block_to_hmdb[as.character(df_synonym$block)]
  valid_syn <- !is.na(syn_hmdb)
  synonyms_df <- data.frame(
    metabolite_id = syn_hmdb[valid_syn],
    synonym = tolower(trimws(df_synonym$val[valid_syn])),
    stringsAsFactors = FALSE
  )
  # Also add primary names as synonyms
  name_syns <- metabolite_meta[!is.na(metabolite_meta$name), c("metabolite_id", "name")]
  if (nrow(name_syns) > 0) {
    name_syns_df <- data.frame(
      metabolite_id = name_syns$metabolite_id,
      synonym = tolower(trimws(name_syns$name)),
      stringsAsFactors = FALSE
    )
    synonyms_df <- rbind(synonyms_df, name_syns_df)
  }
  synonyms_df <- synonyms_df[!duplicated(paste0(synonyms_df$metabolite_id, ":", synonyms_df$synonym)), , drop = FALSE]
  rownames(synonyms_df) <- NULL
  progress_callback(sprintf("Synonyms: %d entries", nrow(synonyms_df)))

  progress_callback("Building pathway annotations...")

  # Build annot_long from SMPDB and KEGG pathway annotations
  # Each smpdb_id/kegg_map_id line is inside a <pathway> block inside a <metabolite> block
  # Map each to its metabolite block

  # For pathway names, we need to associate each smpdb_id/kegg_map_id with its <pathway> block's <name>
  # Strategy: for each smpdb_id line, find the nearest preceding <pathway> block's <name>
  # Simpler: find all <name> tags inside pathway blocks by checking pw_starts/pw_ends

  # Build pathway name lookup for each pathway block
  # A <name> tag inside a pathway block (between pw_starts[k] and pw_ends[k])
  if (nrow(df_name) > 0 && length(pw_starts) > 0 && length(pw_ends) > 0) {
    # For each name line, check if it's inside a pathway block
    n_pw <- min(length(pw_starts), length(pw_ends))
    pw_starts_n <- pw_starts[1:n_pw]
    pw_ends_n <- pw_ends[1:n_pw]

    # Assign each name line to a pathway block (if any)
    pw_block_of_name <- findInterval(df_name$idx, pw_starts_n)
    # A name line is inside pathway block k if: pw_starts_n[k] <= idx <= pw_ends_n[k]
    # pw_block_of_name == 0 means before first pathway block
    valid_pw_idx <- pmax(pw_block_of_name, 1L)  # clamp to 1 to avoid index-0
    in_pw <- pw_block_of_name > 0 & pw_block_of_name <= n_pw &
             df_name$idx <= pw_ends_n[valid_pw_idx]
    pw_name_lookup <- df_name[in_pw, , drop = FALSE]
    pw_name_lookup$pw_block <- pw_block_of_name[in_pw]
    # First name per pathway block
    pw_name_lookup <- pw_name_lookup[!duplicated(pw_name_lookup$pw_block), , drop = FALSE]
    pw_name_map <- stats::setNames(pw_name_lookup$val, as.character(pw_name_lookup$pw_block))
  } else {
    pw_name_map <- character(0)
  }

  # For smpdb_id entries, find their pathway block and metabolite block
  annot_rows <- list()
  n_with_pathways <- 0L

  if (nrow(df_smpdb) > 0 && length(pw_starts) > 0) {
    n_pw <- min(length(pw_starts), length(pw_ends))
    df_smpdb$pw_block <- findInterval(df_smpdb$idx, pw_starts[1:n_pw])
    df_smpdb$pw_name <- unname(pw_name_map[as.character(df_smpdb$pw_block)])
    df_smpdb$pw_name[is.na(df_smpdb$pw_name)] <- df_smpdb$val[is.na(df_smpdb$pw_name)]
    df_smpdb$hmdb_id <- block_to_hmdb[as.character(df_smpdb$block)]

    valid <- !is.na(df_smpdb$hmdb_id)
    if (any(valid)) {
      annot_rows[[length(annot_rows) + 1L]] <- data.frame(
        metabolite_id = df_smpdb$hmdb_id[valid],
        pathway_id = df_smpdb$val[valid],
        pathway_name = df_smpdb$pw_name[valid],
        pathway_type = "SMPDB",
        stringsAsFactors = FALSE
      )
    }
  }

  if (nrow(df_kegg_map) > 0 && length(pw_starts) > 0) {
    n_pw <- min(length(pw_starts), length(pw_ends))
    df_kegg_map$pw_block <- findInterval(df_kegg_map$idx, pw_starts[1:n_pw])
    df_kegg_map$pw_name <- unname(pw_name_map[as.character(df_kegg_map$pw_block)])
    df_kegg_map$pw_name[is.na(df_kegg_map$pw_name)] <- df_kegg_map$val[is.na(df_kegg_map$pw_name)]
    df_kegg_map$hmdb_id <- block_to_hmdb[as.character(df_kegg_map$block)]

    valid <- !is.na(df_kegg_map$hmdb_id)
    if (any(valid)) {
      annot_rows[[length(annot_rows) + 1L]] <- data.frame(
        metabolite_id = df_kegg_map$hmdb_id[valid],
        pathway_id = df_kegg_map$val[valid],
        pathway_name = df_kegg_map$pw_name[valid],
        pathway_type = "KEGG",
        stringsAsFactors = FALSE
      )
    }
  }

  annot_long <- if (length(annot_rows) > 0) {
    al <- do.call(rbind, annot_rows)
    al <- al[!duplicated(paste0(al$metabolite_id, ":", al$pathway_id, ":", al$pathway_type)), , drop = FALSE]
    rownames(al) <- NULL
    al
  } else {
    data.frame(
      metabolite_id = character(0), pathway_id = character(0),
      pathway_name = character(0), pathway_type = character(0),
      stringsAsFactors = FALSE
    )
  }

  n_with_pathways <- length(unique(annot_long$metabolite_id))
  n_metabolites <- n_metabolites_final

  # All tables already built above by vectorized extraction
  progress_callback(sprintf("Total metabolites: %d (%d with pathways)", n_metabolites, n_with_pathways))
  progress_callback(sprintf("Pathway annotations: %d (SMPDB: %d, KEGG: %d)",
    nrow(annot_long),
    sum(annot_long$pathway_type == "SMPDB"),
    sum(annot_long$pathway_type == "KEGG")))

  # Build terms_by_id
  terms_by_id <- annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )
  progress_callback(sprintf("Unique pathways: %d", nrow(terms_by_id)))

  n_classes <- length(unique(class_mappings$class[!is.na(class_mappings$class)]))
  progress_callback(sprintf("Chemical classes: %d unique", n_classes))
  gc()

  # Build final metabobase
  metabobase <- list(
    schema_version = METABOBASE_SCHEMA_VERSION,
    data_type = "metabolomics",
    metabobase_version = 2L,
    library_name = library_name,
    organism = "Homo sapiens (Human)",
    organism_code = "hsa",
    source = "HMDB",
    created = Sys.time(),
    n_raw_rows = nrow(annot_long),
    n_metabolites = n_metabolites,
    annot_long = annot_long,
    terms_by_id = terms_by_id,
    class_mappings = class_mappings,
    id_xref = id_xref,
    metabolite_meta = metabolite_meta,
    synonyms = synonyms_df
  )

  progress_callback(sprintf("Built MetaboBase: %d metabolites, %d pathways",
                            n_metabolites, nrow(terms_by_id)))

  # Save if output file provided
  if (!is.null(output_file)) {
    metabobase_save(metabobase, output_file)
    progress_callback(sprintf("Saved to: %s", output_file))
  }

  metabobase
}

#' Load a pre-built MetaboBase library by name
#' @param name Library name (partial match supported)
#' @param metabobase_dir Directory containing .metabobase files
#' @return MetaboBase object
metabobase_load_library <- function(name, metabobase_dir = NULL) {
  libs <- metabobase_list_libraries(metabobase_dir)

  if (nrow(libs) == 0) {
    stop("No MetaboBase libraries found. Build one with metabobase_build_kegg_library()")
  }

  # Try exact match first
  idx <- which(tolower(libs$name) == tolower(name))

  # Try partial match
  if (length(idx) == 0) {
    idx <- grep(name, libs$name, ignore.case = TRUE)
  }

  if (length(idx) == 0) {
    stop(paste0(
      "Library '", name, "' not found.\nAvailable libraries:\n  ",
      paste(libs$name, collapse = "\n  ")
    ))
  }

  if (length(idx) > 1) {
    warning(paste0("Multiple matches found, using first: ", libs$name[idx[1]]))
    idx <- idx[1]
  }

  metabobase_load(libs$file[idx])
}
