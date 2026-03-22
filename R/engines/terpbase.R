# R/engines/terpbase.R
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

# -----------------------------
# Constants / schema
# -----------------------------
TERPBASE_SCHEMA_VERSION <- 1L

msterp_terpbase_required_uniprot_cols <- function() {
  c(
    "Entry",
    "Protein names",
    "Gene Names",
    "Gene Ontology (biological process)",
    "Gene Ontology (cellular component)",
    "Gene Ontology (molecular function)",
    "Subcellular location [CC]",
    "Organism"
  )
}

# Alternate column name that takes priority if present
TERPBASE_GENE_NAMES_PRIMARY <- "Gene Names (primary)"

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

ucfirst <- function(x) {
  x <- as.character(x)
  i <- nzchar(x)
  x[i] <- paste0(toupper(substr(x[i], 1, 1)), substr(x[i], 2, nchar(x[i])))
  x
}

parse_gene_symbol <- function(gene_names_vec) {
  x <- as.character(gene_names_vec)
  x[is.na(x)] <- ""
  first <- vapply(
    strsplit(x, "\\s+", perl = TRUE),
    function(tokens) if (length(tokens)) tokens[1] else "",
    character(1)
  )
  first <- trimws(first)
  first[first == ""] <- NA_character_
  first
}

# -----------------------------
# (new) Fast header validation for UniProt Excel
# -----------------------------
terpbase_uniprot_headers <- function(path) {
  # n_max=0 reads only headers (fast)
  hdr <- readxl::read_xlsx(path, n_max = 0)
  names(hdr)
}

terpbase_validate_uniprot_excel <- function(path) {
  required <- msterp_terpbase_required_uniprot_cols()
  cols <- terpbase_uniprot_headers(path)

  # Accept "Gene Names (primary)" as an alternative to "Gene Names"
  effective_missing <- setdiff(required, cols)
  if ("Gene Names" %in% effective_missing && TERPBASE_GENE_NAMES_PRIMARY %in% cols) {
    effective_missing <- setdiff(effective_missing, "Gene Names")
  }

  list(ok = length(effective_missing) == 0, required = required, present = cols, missing = effective_missing)
}

# -----------------------------
# Build from UniProt Excel
# -----------------------------
terpbase_build_from_uniprot <- function(
    path,
    library_name = NULL,
    progress_inc = function(amount, detail = NULL) NULL,
    progress_set = function(value, detail = NULL) NULL
) {
  required_cols <- msterp_terpbase_required_uniprot_cols()

  progress_set(0.05, "Reading Excel")
  raw <- readxl::read_xlsx(path)

  # Check for required columns, allowing "Gene Names (primary)" as alternative to "Gene Names"
  missing <- setdiff(required_cols, names(raw))
  if ("Gene Names" %in% missing && TERPBASE_GENE_NAMES_PRIMARY %in% names(raw)) {
    missing <- setdiff(missing, "Gene Names")
  }
  if (length(missing)) {
    stop(
      "Missing required columns in Excel:\n  ",
      paste(missing, collapse = ", "),
      "\n\nHeaders must match UniProt export EXACTLY."
    )
  }

  # Prefer "Gene Names (primary)" over "Gene Names" when both exist
  gene_names_col <- if (TERPBASE_GENE_NAMES_PRIMARY %in% names(raw)) {
    TERPBASE_GENE_NAMES_PRIMARY
  } else {
    "Gene Names"
  }

  progress_inc(0.10, "Parsing columns")
  df_base <- tibble(
    Entry         = raw[["Entry"]],
    Protein_names = raw[["Protein names"]],
    Gene_Names    = raw[[gene_names_col]],
    BP            = raw[["Gene Ontology (biological process)"]],
    CC            = raw[["Gene Ontology (cellular component)"]],
    MF            = raw[["Gene Ontology (molecular function)"]],
    Subcell       = raw[["Subcellular location [CC]"]],
    Organism      = raw[["Organism"]]
  ) %>%
    mutate(
      gene_symbol = parse_gene_symbol(Gene_Names)
    )
  
  # Auto organism label from UniProt column
  org_vals <- unique(trimws(na.omit(as.character(df_base$Organism))))
  org_label <- if (length(org_vals) == 1) org_vals[[1]] else paste(org_vals, collapse = " | ")
  
  progress_inc(0.10, "Building ontology tables")
  go_long <- df_base %>%
    select(Entry, gene_symbol, BP, CC, MF) %>%
    pivot_longer(
      cols      = c(BP, CC, MF),
      names_to  = "ONTOLOGY",
      values_to = "go_text"
    ) %>%
    mutate(
      ONTOLOGY = case_when(
        ONTOLOGY == "BP" ~ "BP",
        ONTOLOGY == "CC" ~ "CC",
        ONTOLOGY == "MF" ~ "MF",
        TRUE             ~ ONTOLOGY
      )
    ) %>%
    filter(!is.na(go_text), go_text != "")
  
  if (!nrow(go_long)) stop("No GO annotations found in any of the GO columns.")
  
  progress_inc(0.15, "Splitting ontologies")
  go_long2 <- go_long %>%
    mutate(go_items = strsplit(go_text, ";", fixed = TRUE)) %>%
    unnest(go_items) %>%
    mutate(go_items = str_trim(go_items)) %>%
    filter(go_items != "")
  
  progress_inc(0.10, "Parsing descriptions")
  m <- stringr::str_match(go_long2$go_items, "^(.*?)\\s*\\[(GO:\\d+)\\]$")
  go_long2$Description <- ucfirst(m[, 2])
  go_long2$ID          <- m[, 3]
  
  go_long2 <- go_long2 %>%
    filter(!is.na(ID), !is.na(Description), Description != "") %>%
    select(Entry, gene_symbol, ONTOLOGY, ID, Description)
  
  if (!nrow(go_long2)) {
    stop("After parsing, no valid description entries were found.")
  }
  
  progress_inc(0.15, "Mapping ontologies")
  annot_symbol <- go_long2 %>%
    filter(!is.na(gene_symbol), gene_symbol != "") %>%
    transmute(gene = gene_symbol, ID, ONTOLOGY, Description)
  
  annot_entry <- go_long2 %>%
    transmute(gene = Entry, ID, ONTOLOGY, Description)
  
  annot_long <- bind_rows(annot_symbol, annot_entry) %>%
    filter(!is.na(gene), gene != "") %>%
    distinct(gene, ID, ONTOLOGY, Description)
  
  progress_inc(0.15, "Grouping")
  terms_by_id <- annot_long %>%
    group_by(ONTOLOGY, ID, Description) %>%
    summarise(
      term_genes = list(sort(unique(gene))),
      n_genes    = length(unique(gene)),
      .groups    = "drop"
    )
  
  progress_inc(0.10, "Building output")
  gene_meta <- df_base %>%
    transmute(
      entry            = Entry,
      gene_symbol      = gene_symbol,
      protein_name     = Protein_names,
      subcell_location = Subcell,
      organism         = Organism
    )
  
  progress_set(1, "Done")
  
  list(
    schema_version   = TERPBASE_SCHEMA_VERSION,
    data_type        = "proteomics",
    terpbase_version = 1L,
    organism         = org_label,
    organism_values  = org_vals,
    library_name     = library_name %||% NULL,
    created          = Sys.time(),
    n_raw_rows       = nrow(raw),
    annot_long       = annot_long,
    terms_by_id      = terms_by_id,
    gene_meta        = gene_meta
  )
}

# -----------------------------
# Validate / load / save
# -----------------------------
terpbase_validate <- function(x) {
  errs <- character(0)
  warns <- character(0)

  if (!is.list(x)) errs <- c(errs, "Terpbase object is not a list.")
  if (is.list(x)) {
    req_fields <- c("schema_version", "terpbase_version", "organism", "created", "annot_long", "terms_by_id", "gene_meta")
    missing <- setdiff(req_fields, names(x))
    if (length(missing)) errs <- c(errs, paste0("Missing fields: ", paste(missing, collapse = ", ")))

    # Validate data_type if present (optional for backwards compat)
    if ("data_type" %in% names(x)) {
      if (!identical(x$data_type, "proteomics")) {
        errs <- c(errs, sprintf("TerpBase data_type must be 'proteomics', got '%s'", x$data_type))
      }
    } else {
      # Missing data_type: warn but don't error (backwards compatibility)
      warns <- c(warns, "Missing data_type field; assuming 'proteomics' (legacy file)")
    }

    if (!("schema_version" %in% names(x)) || !is.numeric(x$schema_version)) {
      errs <- c(errs, "schema_version missing or not numeric.")
    }

    if (!("annot_long" %in% names(x)) || !is.data.frame(x$annot_long)) {
      errs <- c(errs, "annot_long missing or not a data.frame.")
    } else {
      needed <- c("gene", "ID", "ONTOLOGY", "Description")
      miss2 <- setdiff(needed, names(x$annot_long))
      if (length(miss2)) errs <- c(errs, paste0("annot_long missing columns: ", paste(miss2, collapse = ", ")))
    }

    if (!("terms_by_id" %in% names(x)) || !is.data.frame(x$terms_by_id)) {
      errs <- c(errs, "terms_by_id missing or not a data.frame.")
    } else {
      needed <- c("ONTOLOGY", "ID", "Description", "term_genes", "n_genes")
      miss2 <- setdiff(needed, names(x$terms_by_id))
      if (length(miss2)) errs <- c(errs, paste0("terms_by_id missing columns: ", paste(miss2, collapse = ", ")))
    }

    if (!("gene_meta" %in% names(x)) || !is.data.frame(x$gene_meta)) {
      errs <- c(errs, "gene_meta missing or not a data.frame.")
    } else {
      needed <- c("entry", "gene_symbol", "protein_name", "subcell_location", "organism")
      miss2 <- setdiff(needed, names(x$gene_meta))
      if (length(miss2)) errs <- c(errs, paste0("gene_meta missing columns: ", paste(miss2, collapse = ", ")))
    }
  }

  list(ok = length(errs) == 0, errors = errs, warnings = warns)
}

terpbase_save <- function(x, file) {
  v <- terpbase_validate(x)
  if (!v$ok) stop(paste(v$errors, collapse = "\n"))
  saveRDS(x, file = file)
  invisible(TRUE)
}

terpbase_load <- function(file) {
  x <- readRDS(file)
  v <- terpbase_validate(x)
  if (!v$ok) stop(paste(v$errors, collapse = "\n"))
  x
}

terpbase_summary_lines <- function(x) {
  v <- terpbase_validate(x)
  if (!v$ok) return(c("Invalid terpbase object:", paste0("- ", v$errors)))
  
  n_ids <- length(unique(x$annot_long$gene))
  term_counts <- x$terms_by_id %>%
    count(ONTOLOGY, name = "n_terms") %>%
    arrange(ONTOLOGY)
  
  term_str <- if (nrow(term_counts)) {
    paste(term_counts$ONTOLOGY, term_counts$n_terms, sep = ": ", collapse = ", ")
  } else {
    "none"
  }
  
  c(
    paste0("Library name: ", x$library_name %||% "(none)"),
    paste0("Organism: ", x$organism %||% "(unknown)"),
    paste0("Created: ", format(x$created)),
    paste0("Rows in Excel: ", x$n_raw_rows),
    paste0("Unique identifiers (gene symbols + Entry IDs): ", n_ids),
    paste0("Ontology terms: ", term_str),
    "Identifiers include both Gene Symbols and UniProt Entry IDs."
  )
}
