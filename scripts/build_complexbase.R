#!/usr/bin/env Rscript
# ==========================================================
# build_complexbase.R - Build ComplexBase from CORUM + Complex Portal
#
# Usage: Rscript scripts/build_complexbase.R [output_path]
# Default output: terpbase/human.complexbase
#
# Data sources:
#   CORUM: https://mips.helmholtz-muenchen.de/corum/
#   Complex Portal: https://www.ebi.ac.uk/complexportal/
#
# This script downloads protein complex data from both sources,
# parses the data to extract human complexes, merges them,
# validates using complexbase_validate, and saves to .complexbase format.
# ==========================================================

# -----------------------------------------------------------------------------
# Package Management
# -----------------------------------------------------------------------------

required_pkgs <- c("httr", "jsonlite", "readr", "dplyr")

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org", quiet = TRUE)
}

library(httr)
library(jsonlite)
library(readr)
library(dplyr)

# -----------------------------------------------------------------------------
# CORUM Download and Parsing
# -----------------------------------------------------------------------------

#' Download and parse CORUM protein complex data
#'
#' Downloads the CORUM allComplexes.txt file and parses it to extract
#' human protein complexes with their subunit information.
#'
#' @param cache_dir Directory to cache downloaded files (default: "data_cache")
#' @param max_retries Number of download retry attempts (default: 3)
#' @return list with complexes data.frame and protein_complex data.frame
download_corum <- function(cache_dir = "data_cache", max_retries = 3,
                           organism_filter = "Human|Homo sapiens",
                           organism_name = "human",
                           local_file = NULL) {
  # If a local CORUM file is provided, use it directly (skip download)
  if (!is.null(local_file) && nzchar(local_file) && file.exists(local_file)) {
    message(sprintf("  Using local CORUM file: %s", local_file))
    txt_file <- local_file
  } else {
  message("  Downloading CORUM data...")

  # CORUM download URLs - try multiple sources
  # Primary: Direct CORUM download (may return HTML if their server changed)
  # Fallback: Harmonizome mirror (GMT format with gene symbols)
  corum_urls <- c(
    "https://mips.helmholtz-muenchen.de/corum/download/allComplexes.txt.zip",
    "https://mips.helmholtz-muenchen.de/corum/download/allComplexes.txt"
  )
  harmonizome_url <- "https://maayanlab.cloud/static/hdfs/harmonizome/data/corum/gene_set_library_crisp.gmt.gz"
  corum_url <- corum_urls[1]  # Start with zip

  # Ensure cache directory exists
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  zip_file <- file.path(cache_dir, "allComplexes.txt.zip")
  txt_file <- file.path(cache_dir, "allComplexes.txt")

  # Check cache first
  if (file.exists(txt_file)) {
    file_age_days <- as.numeric(difftime(Sys.time(), file.info(txt_file)$mtime, units = "days"))
    if (file_age_days < 30) {
      message("  Using cached CORUM data (", round(file_age_days, 1), " days old)")
    } else {
      message("  Cache is stale, re-downloading...")
      file.remove(txt_file)
    }
  }

  # Download if not cached
  if (!file.exists(txt_file)) {
    download_success <- FALSE

    # Try each URL format
    for (url_idx in seq_along(corum_urls)) {
      if (download_success) break

      current_url <- corum_urls[url_idx]
      is_zip <- grepl("\\.zip$", current_url)
      message(sprintf("  Trying URL: %s", current_url))

      for (attempt in 1:max_retries) {
        tryCatch({
          message(sprintf("  Download attempt %d/%d...", attempt, max_retries))

          # Download to appropriate file
          download_file <- if (is_zip) zip_file else txt_file

          # CORUM server sometimes has SSL certificate issues, so we allow insecure
          response <- GET(
            current_url,
            write_disk(download_file, overwrite = TRUE),
            timeout(300),  # 5 minute timeout
            config(ssl_verifypeer = FALSE)  # CORUM has certificate issues
          )

          if (status_code(response) == 200) {
            if (is_zip) {
              # Try to unzip
              unzip_result <- tryCatch({
                unzip(zip_file, exdir = cache_dir)
                TRUE
              }, warning = function(w) {
                message("  Unzip warning: ", w$message)
                FALSE
              }, error = function(e) {
                message("  Unzip error: ", e$message)
                FALSE
              })

              if (unzip_result && file.exists(txt_file)) {
                message("  Download and extraction successful")
                download_success <- TRUE
                break
              } else {
                # Check if downloaded file is actually plain text (not a real zip)
                first_line <- tryCatch(readLines(zip_file, n = 1, warn = FALSE), error = function(e) "")
                if (grepl("ComplexID|Complex|subunits", first_line, ignore.case = TRUE)) {
                  file.copy(zip_file, txt_file, overwrite = TRUE)
                  message("  File was plain text, not zip - copied successfully")
                  download_success <- TRUE
                  break
                } else {
                  message("  Zip extraction failed, trying next URL...")
                  break  # Try next URL
                }
              }
            } else {
              # Direct text file download
              # Verify it looks like valid data
              first_line <- tryCatch(readLines(txt_file, n = 1, warn = FALSE), error = function(e) "")
              if (grepl("ComplexID|Complex|subunits", first_line, ignore.case = TRUE)) {
                message("  Download successful")
                download_success <- TRUE
                break
              } else {
                message("  Downloaded file doesn't look like CORUM data")
                break  # Try next URL
              }
            }
          } else {
            warning(sprintf("HTTP %d from CORUM server", status_code(response)))
            if (attempt == max_retries) break  # Try next URL
            Sys.sleep(5)
          }
        }, error = function(e) {
          message(sprintf("  Attempt %d failed: %s", attempt, e$message))
          if (attempt == max_retries) break  # Try next URL
          Sys.sleep(5)
        })
      }
    }

    # If primary CORUM download failed, try Harmonizome mirror
    if (!download_success) {
      message("  Primary CORUM download failed, trying Harmonizome mirror...")
      gmt_file <- file.path(cache_dir, "corum_harmonizome.gmt.gz")

      for (attempt in 1:max_retries) {
        tryCatch({
          message(sprintf("  Harmonizome download attempt %d/%d...", attempt, max_retries))

          response <- GET(
            harmonizome_url,
            write_disk(gmt_file, overwrite = TRUE),
            timeout(120)
          )

          if (status_code(response) == 200 && file.exists(gmt_file)) {
            # Decompress and check content
            gmt_content <- tryCatch({
              con <- gzfile(gmt_file, "rt")
              lines <- readLines(con, n = 5)
              close(con)
              lines
            }, error = function(e) NULL)

            if (!is.null(gmt_content) && length(gmt_content) > 0) {
              message("  Harmonizome download successful")
              download_success <- TRUE

              # Parse GMT format and return early with Harmonizome data
              message("  Parsing Harmonizome GMT format...")
              con <- gzfile(gmt_file, "rt")
              gmt_lines <- readLines(con)
              close(con)

              # GMT format: complex_name \t description \t gene1 \t gene2 \t ...
              complexes_list <- lapply(seq_along(gmt_lines), function(i) {
                parts <- strsplit(gmt_lines[i], "\t")[[1]]
                if (length(parts) >= 3) {
                  complex_name <- parts[1]
                  genes <- parts[3:length(parts)]
                  genes <- genes[nzchar(genes)]
                  if (length(genes) > 0) {
                    return(list(
                      complex_id = paste0("CORUM_HM_", i),
                      complex_name = complex_name,
                      genes = genes
                    ))
                  }
                }
                return(NULL)
              })
              complexes_list <- Filter(Negate(is.null), complexes_list)

              message(sprintf("  Found %d complexes from Harmonizome", length(complexes_list)))

              # Build complexes data.frame
              complexes <- data.frame(
                complex_id = sapply(complexes_list, `[[`, "complex_id"),
                complex_name = sapply(complexes_list, `[[`, "complex_name"),
                source = "CORUM",
                n_subunits = sapply(complexes_list, function(x) length(x$genes)),
                stringsAsFactors = FALSE
              )

              # Build protein_complex mapping
              # Note: Harmonizome uses gene symbols, not UniProt IDs
              protein_complex_list <- lapply(complexes_list, function(cx) {
                data.frame(
                  protein_id = cx$genes,  # Gene symbols
                  complex_id = cx$complex_id,
                  stringsAsFactors = FALSE
                )
              })
              protein_complex <- bind_rows(protein_complex_list)

              message(sprintf("  CORUM (Harmonizome): %d complexes, %d gene-complex mappings",
                              nrow(complexes), nrow(protein_complex)))

              return(list(
                complexes = complexes,
                protein_complex = protein_complex,
                source = "Harmonizome"
              ))
            }
          }
        }, error = function(e) {
          message(sprintf("  Harmonizome attempt %d failed: %s", attempt, e$message))
        })

        if (attempt < max_retries) Sys.sleep(2)
      }
    }

    if (!download_success) {
      stop("Failed to download CORUM data from any source (including Harmonizome)")
    }
  }
  } # end else (download path)

  # Parse the TSV file (original CORUM format)
  message("  Parsing CORUM data...")

  corum_raw <- read_tsv(txt_file, show_col_types = FALSE, progress = FALSE)

  # Filter to specified organism
  # CORUM uses organism name in Organism column
  if ("Organism" %in% names(corum_raw)) {
    corum_filtered <- corum_raw %>%
      filter(grepl(organism_filter, Organism, ignore.case = TRUE))
  } else if ("organism" %in% names(corum_raw)) {
    corum_filtered <- corum_raw %>%
      filter(grepl(organism_filter, organism, ignore.case = TRUE))
  } else {
    # Fallback: take all if no organism column
    corum_filtered <- corum_raw
  }

  message(sprintf("  Found %d %s complexes in CORUM", nrow(corum_filtered), organism_name))

  # Parse the data
  # CORUM columns of interest:
  # - ComplexID or Complex id
  # - ComplexName or Complex name
  # - subunits(UniProt IDs) - semicolon separated

  # Find the column names (CORUM format varies slightly across versions)
  id_col <- intersect(names(corum_filtered), c("ComplexID", "Complex id", "complex_id"))[1]
  name_col <- intersect(names(corum_filtered), c("ComplexName", "Complex name", "complex_name"))[1]
  subunits_col <- intersect(names(corum_filtered), c(
    "subunits(UniProt IDs)", "subunits_uniprot_ids", "subunits_uniprot_id",
    "Subunits (UniProt IDs)"))[1]
  # Gene name column (for UniProt→gene symbol mapping)
  gene_col <- intersect(names(corum_filtered), c(
    "subunits(Gene name)", "subunits_gene_name", "subunits_gene_names",
    "Subunits (Gene name)"))[1]

  if (is.na(id_col) || is.na(name_col) || is.na(subunits_col)) {
    # Debug: show available columns
    message("  Available columns: ", paste(names(corum_filtered), collapse = ", "))
    stop("Could not find required CORUM columns. Expected ComplexID, ComplexName, and subunits(UniProt IDs)")
  }

  if (!is.na(gene_col)) {
    message(sprintf("  Found gene name column: %s (will build gene_symbol mapping)", gene_col))
  }

  # Build complexes data.frame — also grab gene names if available
  has_genes <- !is.na(gene_col)
  complexes <- corum_filtered %>%
    transmute(
      complex_id = paste0("CORUM_", .data[[id_col]]),
      complex_name = .data[[name_col]],
      source = "CORUM",
      subunits_raw = .data[[subunits_col]],
      genes_raw = if (has_genes) .data[[gene_col]] else NA_character_
    ) %>%
    # Remove rows with empty subunits
    filter(!is.na(subunits_raw) & nzchar(subunits_raw))

  # Parse subunits and gene names
  complexes <- complexes %>%
    rowwise() %>%
    mutate(
      subunits_list = list(unique(trimws(unlist(strsplit(subunits_raw, ";|,"))))),
      genes_list = list(if (!is.na(genes_raw) && nzchar(genes_raw))
        trimws(unlist(strsplit(genes_raw, ";|,"))) else character(0))
    ) %>%
    ungroup() %>%
    mutate(
      n_subunits = sapply(subunits_list, length)
    ) %>%
    # Remove complexes with no valid subunits
    filter(n_subunits > 0)

  # Build protein_complex mapping (long format) with gene_symbol where available
  protein_complex_list <- lapply(seq_len(nrow(complexes)), function(i) {
    proteins <- complexes$subunits_list[[i]]
    genes <- complexes$genes_list[[i]]
    # Filter out empty strings and clean UniProt IDs
    proteins <- proteins[nzchar(proteins)]
    # Remove isoform suffixes (e.g., P12345-1 -> P12345)
    proteins <- sub("-[0-9]+$", "", proteins)

    if (length(proteins) == 0) return(NULL)

    # Match gene symbols to UniProt IDs positionally (CORUM stores them in same order)
    gene_symbols <- if (length(genes) == length(proteins)) genes else rep(NA_character_, length(proteins))

    data.frame(
      protein_id = proteins,
      gene_symbol = gene_symbols,
      complex_id = complexes$complex_id[i],
      stringsAsFactors = FALSE
    )
  })

  protein_complex <- bind_rows(protein_complex_list)

  # Report gene symbol coverage
  n_with_gene <- sum(!is.na(protein_complex$gene_symbol) & nzchar(protein_complex$gene_symbol))
  message(sprintf("  Gene symbol coverage: %d / %d mappings (%.0f%%)",
                  n_with_gene, nrow(protein_complex),
                  100 * n_with_gene / max(1, nrow(protein_complex))))

  # Clean up complexes data.frame (remove helper columns)
  complexes_final <- complexes %>%
    select(complex_id, complex_name, source, n_subunits)

  message(sprintf("  CORUM: %d complexes, %d protein-complex mappings",
                  nrow(complexes_final), nrow(protein_complex)))

  list(
    complexes = as.data.frame(complexes_final),
    protein_complex = as.data.frame(protein_complex)
  )
}

# -----------------------------------------------------------------------------
# Complex Portal Download and Parsing
# -----------------------------------------------------------------------------

#' Download and parse Complex Portal protein complex data
#'
#' Downloads human complexes from the EBI Complex Portal and parses
#' the data to extract complexes with their subunit information.
#'
#' @param cache_dir Directory to cache downloaded files (default: "data_cache")
#' @param max_retries Number of download retry attempts (default: 3)
#' @return list with complexes data.frame and protein_complex data.frame
download_complex_portal <- function(cache_dir = "data_cache", max_retries = 3,
                                    taxon_id = "9606", organism_name = "human") {
  message("  Downloading Complex Portal data...")

  # Organism name mappings for FTP paths
  taxon_to_name <- c(
    "9606" = "homo_sapiens",
    "10090" = "mus_musculus",
    "10116" = "rattus_norvegicus"
  )
  ftp_name <- taxon_to_name[taxon_id] %||% taxon_id

  # Complex Portal FTP download (more reliable than the web export API)
  # Try multiple URL formats as the API changes
  cp_urls <- c(
    # Current FTP location (by taxon ID)
    sprintf("https://ftp.ebi.ac.uk/pub/databases/intact/complex/current/complextab/%s.tsv", taxon_id),
    # Alternative FTP path (by species name)
    sprintf("https://ftp.ebi.ac.uk/pub/databases/intact/complex/current/complextab/%s.tsv", ftp_name),
    # Web export API (may return HTML error page)
    sprintf("https://www.ebi.ac.uk/complexportal/complex/export?format=tsv&species=%s", taxon_id)
  )

  # Ensure cache directory exists
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  tsv_file <- file.path(cache_dir, sprintf("complex_portal_%s.tsv", organism_name))

  # Check cache first
  if (file.exists(tsv_file)) {
    # Verify it's not an HTML error page
    first_line <- tryCatch(readLines(tsv_file, n = 1), error = function(e) "")
    if (grepl("<!DOCTYPE|<html", first_line, ignore.case = TRUE)) {
      message("  Cached file is HTML error page, re-downloading...")
      file.remove(tsv_file)
    } else {
      file_age_days <- as.numeric(difftime(Sys.time(), file.info(tsv_file)$mtime, units = "days"))
      if (file_age_days < 30) {
        message("  Using cached Complex Portal data (", round(file_age_days, 1), " days old)")
      } else {
        message("  Cache is stale, re-downloading...")
        file.remove(tsv_file)
      }
    }
  }

  # Download if not cached
  if (!file.exists(tsv_file)) {
    download_success <- FALSE

    for (cp_url in cp_urls) {
      if (download_success) break

      message(sprintf("  Trying URL: %s", cp_url))

      for (attempt in 1:max_retries) {
        tryCatch({
          message(sprintf("  Download attempt %d/%d...", attempt, max_retries))

          response <- GET(
            cp_url,
            write_disk(tsv_file, overwrite = TRUE),
            timeout(300)  # 5 minute timeout
          )

          if (status_code(response) == 200) {
            # Verify it's not an HTML error page
            first_line <- tryCatch(readLines(tsv_file, n = 1), error = function(e) "")
            if (grepl("<!DOCTYPE|<html", first_line, ignore.case = TRUE)) {
              message("  Received HTML error page instead of TSV")
              file.remove(tsv_file)
              break  # Try next URL
            }
            message("  Download successful")
            download_success <- TRUE
            break
          } else {
            warning(sprintf("HTTP %d from Complex Portal server", status_code(response)))
            if (attempt == max_retries) break  # Try next URL
            Sys.sleep(5)
          }
        }, error = function(e) {
          message(sprintf("  Attempt %d failed: %s", attempt, e$message))
          if (attempt == max_retries) break  # Try next URL
          Sys.sleep(5)
        })
      }
    }

    if (!download_success || !file.exists(tsv_file)) {
      stop("Failed to download Complex Portal data from any source")
    }
  }

  # Parse the TSV file
  message("  Parsing Complex Portal data...")

  cp_raw <- read_tsv(tsv_file, show_col_types = FALSE, progress = FALSE)

  message(sprintf("  Found %d complexes in Complex Portal", nrow(cp_raw)))

  # Complex Portal columns:
  # - Complex ac (or #Complex ac) - the CPX-XXXX identifier
  # - Recommended name
  # - Identifiers/participants column - format varies: uniprotkb:P12345|intact:EBI-XXXX or similar

  # Find column names (header format may vary across versions)
  ac_col <- intersect(names(cp_raw), c("#Complex ac", "Complex ac", "complex_ac", "Complex AC"))[1]
  name_col <- intersect(names(cp_raw), c("Recommended name", "recommended_name", "Name"))[1]

  # Participants column has changed names over versions
  participants_col <- intersect(names(cp_raw), c(
    "Identifiers (and stoichiometry) of molecules in complex",  # Current format (2025+)
    "Expanded participant list",  # Alternative current format
    "Identifiers (participants)",  # Older format
    "Identifiers",
    "participants",
    "Participants"
  ))[1]

  if (is.na(ac_col) || is.na(name_col) || is.na(participants_col)) {
    message("  Available columns: ", paste(names(cp_raw), collapse = ", "))
    stop("Could not find required Complex Portal columns")
  }

  message(sprintf("  Using columns: ac=%s, name=%s, participants=%s", ac_col, name_col, participants_col))

  # Build complexes data.frame
  complexes <- cp_raw %>%
    transmute(
      # Complex Portal IDs already have CPX- prefix
      complex_id = paste0("CPX_", .data[[ac_col]]),
      complex_name = .data[[name_col]],
      source = "ComplexPortal",
      participants_raw = .data[[participants_col]]
    ) %>%
    # Remove rows with empty participants
    filter(!is.na(participants_raw) & nzchar(participants_raw))

  # Parse participants to extract UniProt IDs
  # Formats vary:
  #   - Old: uniprotkb:P12345|intact:EBI-123|...
  #   - New (with stoichiometry): uniprotkb:P12345(1)|uniprotkb:Q9876(2)|...
  #   - Expanded list format: P12345;Q9876;...
  extract_uniprot_ids <- function(participants_str) {
    if (is.na(participants_str) || !nzchar(participants_str)) {
      return(character(0))
    }

    uniprot_ids <- character(0)

    # Method 1: Look for uniprotkb: prefixed entries
    # Split by pipe or semicolon to get individual identifiers
    parts <- unlist(strsplit(participants_str, "[|;]"))

    # Extract UniProt IDs (format: uniprotkb:P12345 or uniprotkb:P12345(1))
    uniprot_parts <- grep("uniprotkb:|uniprot:", parts, value = TRUE, ignore.case = TRUE)

    if (length(uniprot_parts) > 0) {
      # Extract the ID part after the colon
      uniprot_ids <- sub("^.*uniprotkb?:", "", uniprot_parts, ignore.case = TRUE)

      # Clean up: remove stoichiometry numbers like (1), isoform suffixes, etc.
      uniprot_ids <- sub("\\([0-9]+\\)$", "", uniprot_ids)  # Remove (1), (2), etc.
      uniprot_ids <- sub("-[0-9]+$", "", uniprot_ids)       # Remove isoform suffixes
      uniprot_ids <- sub("\\(.*\\)$", "", uniprot_ids)      # Remove other parenthetical info
      uniprot_ids <- trimws(uniprot_ids)
    }

    # Method 2: If no uniprotkb: entries found, try to extract UniProt ID patterns directly
    # UniProt IDs are 6-10 alphanumeric characters starting with [OPQ] or [A-NR-Z][0-9]
    if (length(uniprot_ids) == 0) {
      # Look for bare UniProt IDs (e.g., from "Expanded participant list" column)
      uniprot_pattern <- "\\b([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9])\\b"
      matches <- regmatches(participants_str, gregexpr(uniprot_pattern, participants_str, perl = TRUE))[[1]]
      if (length(matches) > 0) {
        uniprot_ids <- matches
      }
    }

    unique(uniprot_ids[nzchar(uniprot_ids)])
  }

  complexes <- complexes %>%
    rowwise() %>%
    mutate(
      subunits_list = list(extract_uniprot_ids(participants_raw))
    ) %>%
    ungroup() %>%
    mutate(
      n_subunits = sapply(subunits_list, length)
    ) %>%
    # Remove complexes with no valid subunits
    filter(n_subunits > 0)

  # Build protein_complex mapping (long format)
  protein_complex_list <- lapply(seq_len(nrow(complexes)), function(i) {
    proteins <- complexes$subunits_list[[i]]
    if (length(proteins) == 0) return(NULL)

    data.frame(
      protein_id = proteins,
      gene_symbol = NA_character_,
      complex_id = complexes$complex_id[i],
      stringsAsFactors = FALSE
    )
  })

  protein_complex <- bind_rows(protein_complex_list)

  # Clean up complexes data.frame
  complexes_final <- complexes %>%
    select(complex_id, complex_name, source, n_subunits)

  message(sprintf("  Complex Portal: %d complexes, %d protein-complex mappings",
                  nrow(complexes_final), nrow(protein_complex)))

  list(
    complexes = as.data.frame(complexes_final),
    protein_complex = as.data.frame(protein_complex)
  )
}

# -----------------------------------------------------------------------------
# Merge and Build Functions
# -----------------------------------------------------------------------------

#' Merge CORUM and Complex Portal databases
#'
#' Combines complexes from both sources, preserving source information.
#' Builds the fast-lookup complex_proteins list.
#'
#' @param corum Output from download_corum()
#' @param complex_portal Output from download_complex_portal()
#' @return list with merged complexes, protein_complex, and complex_proteins
merge_complex_databases <- function(corum, complex_portal) {
  message("Merging CORUM and Complex Portal databases...")

  # Combine complexes data.frames
  complexes <- rbind(corum$complexes, complex_portal$complexes)

  # Ensure both have gene_symbol column for rbind compatibility
  if ("gene_symbol" %in% names(corum$protein_complex) && !"gene_symbol" %in% names(complex_portal$protein_complex)) {
    complex_portal$protein_complex$gene_symbol <- NA_character_
  }
  if ("gene_symbol" %in% names(complex_portal$protein_complex) && !"gene_symbol" %in% names(corum$protein_complex)) {
    corum$protein_complex$gene_symbol <- NA_character_
  }

  # Combine protein_complex mappings
  protein_complex <- rbind(corum$protein_complex, complex_portal$protein_complex)

  # Remove duplicate protein-complex pairs (shouldn't happen with different prefixes)
  protein_complex <- protein_complex %>%
    distinct(protein_id, complex_id, .keep_all = TRUE)

  # Build fast lookup list: complex_id -> c(protein_ids)
  complex_proteins <- split(
    protein_complex$protein_id,
    protein_complex$complex_id
  )
  complex_proteins <- lapply(complex_proteins, unique)

  # Deduplicate: when CORUM and ComplexPortal have the same member set,
  # keep ComplexPortal (better naming) and drop the CORUM duplicate
  member_sigs <- vapply(complex_proteins, function(x) paste(sort(x), collapse = ";"), character(1))
  dup_sigs <- member_sigs[duplicated(member_sigs)]
  if (length(dup_sigs) > 0) {
    # For each duplicated signature, find all complex_ids that share it
    remove_ids <- character(0)
    for (sig in unique(dup_sigs)) {
      ids_with_sig <- names(member_sigs)[member_sigs == sig]
      # Keep ComplexPortal (CPX_) entries, remove CORUM duplicates
      cpx_ids <- ids_with_sig[grepl("^CPX_", ids_with_sig)]
      corum_ids <- ids_with_sig[grepl("^CORUM_", ids_with_sig)]
      if (length(cpx_ids) > 0 && length(corum_ids) > 0) {
        remove_ids <- c(remove_ids, corum_ids)
      }
    }
    if (length(remove_ids) > 0) {
      message(sprintf("  Deduplicating: removing %d CORUM complexes with identical ComplexPortal entries",
                      length(remove_ids)))
      complexes <- complexes[!complexes$complex_id %in% remove_ids, , drop = FALSE]
      protein_complex <- protein_complex[!protein_complex$complex_id %in% remove_ids, , drop = FALSE]
      complex_proteins <- complex_proteins[!names(complex_proteins) %in% remove_ids]
    }
  }

  message(sprintf("  Merged: %d complexes, %d unique proteins, %d protein-complex mappings",
                  nrow(complexes),
                  length(unique(protein_complex$protein_id)),
                  nrow(protein_complex)))

  list(
    complexes = complexes,
    protein_complex = protein_complex,
    complex_proteins = complex_proteins
  )
}

#' Build a ComplexBase object from merged data
#'
#' Creates the final ComplexBase structure with schema version
#' and metadata fields.
#'
#' @param merged Output from merge_complex_databases()
#' @param organism_name Scientific name of the organism
#' @return ComplexBase object ready for validation and saving
build_complexbase <- function(merged, organism_name = "Homo sapiens") {
  list(
    schema_version = 1L,
    organism = organism_name,
    source = "CORUM+ComplexPortal",
    created = Sys.time(),
    complexes = merged$complexes,
    protein_complex = merged$protein_complex,
    complex_proteins = merged$complex_proteins
  )
}

#' Print summary statistics for the ComplexBase
#'
#' @param cb ComplexBase object
print_summary <- function(cb) {
  message("\n=== ComplexBase Summary ===")
  message(sprintf("Organism: %s", cb$organism))
  message(sprintf("Source: %s", cb$source))
  message(sprintf("Created: %s", format(cb$created)))
  message(sprintf("Total complexes: %d", nrow(cb$complexes)))
  message(sprintf("Unique proteins: %d", length(unique(cb$protein_complex$protein_id))))
  message(sprintf("Protein-complex mappings: %d", nrow(cb$protein_complex)))

  # Source breakdown
  source_counts <- table(cb$complexes$source)
  message("\nComplexes by source:")
  for (src in names(source_counts)) {
    message(sprintf("  %s: %d", src, source_counts[src]))
  }

  # Subunit statistics
  message("\nSubunit count statistics:")
  message(sprintf("  Mean: %.1f", mean(cb$complexes$n_subunits)))
  message(sprintf("  Median: %.0f", median(cb$complexes$n_subunits)))
  message(sprintf("  Range: %d - %d", min(cb$complexes$n_subunits), max(cb$complexes$n_subunits)))
  message("===========================\n")
}

# -----------------------------------------------------------------------------
# Main Execution
# -----------------------------------------------------------------------------

if (!interactive()) {
  # Parse command line arguments
  # Usage: Rscript scripts/build_complexbase.R [organism] [output_path] [cache_dir]
  # organism: human (default), mouse, rat
  args <- commandArgs(trailingOnly = TRUE)

  # Organism configuration
  organism_config <- list(
    human = list(
      name = "Homo sapiens",
      taxon = "9606",
      corum_filter = "Human|Homo sapiens",
      default_output = "complexbase/human.complexbase"
    ),
    mouse = list(
      name = "Mus musculus",
      taxon = "10090",
      corum_filter = "Mouse|Mus musculus",
      default_output = "complexbase/mouse.complexbase"
    ),
    rat = list(
      name = "Rattus norvegicus",
      taxon = "10116",
      corum_filter = "Rat|Rattus norvegicus",
      default_output = "complexbase/rat.complexbase"
    )
  )

  # Parse organism argument
  organism_arg <- if (length(args) >= 1) tolower(args[1]) else "human"
  if (!organism_arg %in% names(organism_config)) {
    message("ERROR: Invalid organism '", organism_arg, "'")
    message("Valid organisms: ", paste(names(organism_config), collapse = ", "))
    quit(save = "no", status = 1)
  }
  org <- organism_config[[organism_arg]]

  output_path <- if (length(args) >= 2) args[2] else org$default_output
  cache_dir <- if (length(args) >= 3) args[3] else "data_cache"

  message("=========================================")
  message("Building ComplexBase from CORUM + Complex Portal")
  message("=========================================\n")
  message(sprintf("Organism: %s (%s)", org$name, organism_arg))
  message(sprintf("Output path: %s", output_path))
  message(sprintf("Cache directory: %s\n", cache_dir))

  # Download and parse CORUM (prefer local file if available)
  local_corum <- file.path("complexbase", "corum_allComplexes.txt")
  if (file.exists(local_corum)) {
    message(sprintf("[1/4] Using local CORUM file: %s", local_corum))
  } else {
    local_corum <- NULL
    message("[1/4] Downloading CORUM...")
  }
  corum <- tryCatch(
    download_corum(
      cache_dir = cache_dir,
      organism_filter = org$corum_filter,
      organism_name = organism_arg,
      local_file = local_corum
    ),
    error = function(e) {
      message("ERROR downloading CORUM: ", e$message)
      message("Continuing with empty CORUM data...")
      list(
        complexes = data.frame(
          complex_id = character(0),
          complex_name = character(0),
          source = character(0),
          n_subunits = integer(0),
          stringsAsFactors = FALSE
        ),
        protein_complex = data.frame(
          protein_id = character(0),
          complex_id = character(0),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  # Download and parse Complex Portal
  message("\n[2/4] Downloading Complex Portal...")
  cp <- tryCatch(
    download_complex_portal(
      cache_dir = cache_dir,
      taxon_id = org$taxon,
      organism_name = organism_arg
    ),
    error = function(e) {
      message("ERROR downloading Complex Portal: ", e$message)
      message("Continuing with empty Complex Portal data...")
      list(
        complexes = data.frame(
          complex_id = character(0),
          complex_name = character(0),
          source = character(0),
          n_subunits = integer(0),
          stringsAsFactors = FALSE
        ),
        protein_complex = data.frame(
          protein_id = character(0),
          complex_id = character(0),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  # Merge databases
  message("\n[3/4] Merging databases...")
  merged <- merge_complex_databases(corum, cp)

  # Build ComplexBase object
  message("\n[4/4] Building and validating ComplexBase...")
  cb <- build_complexbase(merged, organism_name = org$name)

  # Load validation function from complexbase.R
  # Find the script's location to resolve relative path
  script_dir <- getwd()
  complexbase_path <- file.path(script_dir, "R", "engines", "complexbase.R")

  if (!file.exists(complexbase_path)) {
    # Try relative to script location
    complexbase_path <- "../R/engines/complexbase.R"
  }

  if (file.exists(complexbase_path)) {
    source(complexbase_path)
    validation <- complexbase_validate(cb)

    if (!validation$ok) {
      stop("Validation failed:\n  ", paste(validation$errors, collapse = "\n  "))
    }
    message("  Validation passed!")
  } else {
    message("  Warning: complexbase.R not found, skipping validation")
  }

  # Print summary
  print_summary(cb)

  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Save the ComplexBase
  message(sprintf("Saving to %s...", output_path))
  saveRDS(cb, output_path)

  # Report file size
  file_size <- file.info(output_path)$size
  message(sprintf("Done! File size: %.2f MB", file_size / 1024 / 1024))

  message("\n=========================================")
  message("ComplexBase build complete!")
  message("=========================================")
}
