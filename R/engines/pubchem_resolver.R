# =========================================================
# R/engines/pubchem_resolver.R - PubChem Synonym Resolution
#
# Resolves unmatched metabolite names by fetching their
# synonyms from PubChem PUG REST API, then matching those
# synonyms against the metabobase's existing lookup tables.
#
# Works for any metabobase (HMDB, KEGG, CSV) and caches
# results locally for fast repeat runs.
#
# Designed to work standalone (no Shiny dependencies)
# so it can be called from callr::r_bg() processes.
# =========================================================

#' Get path to PubChem synonym cache file
#' @return Character path to the .rds cache file
pubchem_cache_path <- function() {
  cache_dir <- tryCatch(
    tools::R_user_dir("MSTerp", which = "cache"),
    error = function(e) tempdir()
  )
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(cache_dir, "pubchem_synonym_cache.rds")
}

#' Load PubChem synonym cache from disk
#' @return Named list: query_name_lower -> character vector of synonyms
pubchem_load_synonym_cache <- function() {
  path <- pubchem_cache_path()
  if (file.exists(path)) {
    tryCatch(readRDS(path), error = function(e) list())
  } else {
    list()
  }
}

#' Save PubChem synonym cache to disk
#' @param cache Named list: query_name_lower -> character vector of synonyms
pubchem_save_synonym_cache <- function(cache) {
  path <- pubchem_cache_path()
  tryCatch(saveRDS(cache, path), error = function(e) NULL)
}

#' Fetch all synonyms for a chemical name from PubChem
#'
#' Uses PUG REST: /compound/name/{name}/synonyms/JSON
#' PubChem synonyms typically include HMDB IDs, KEGG IDs, CAS numbers,
#' IUPAC names, and common names — making them useful for cross-database matching.
#'
#' @param name Chemical name string
#' @param max_retries Number of retries on 503/timeout
#' @return Character vector of synonyms (lowercased, trimmed), or character(0) on failure
pubchem_fetch_synonyms <- function(name, max_retries = 1L) {
  if (is.na(name) || !nzchar(trimws(name))) return(character(0))

  encoded_name <- utils::URLencode(trimws(name), reserved = TRUE)
  url <- paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
    encoded_name, "/synonyms/JSON"
  )

  for (attempt in seq_len(1L + max_retries)) {
    resp <- tryCatch(
      httr::GET(url, httr::timeout(15)),
      error = function(e) NULL
    )

    if (is.null(resp)) {
      Sys.sleep(2)
      next
    }

    status <- httr::status_code(resp)

    if (status == 200) {
      body <- tryCatch(
        jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                           simplifyVector = TRUE),
        error = function(e) NULL
      )
      if (!is.null(body) &&
          !is.null(body$InformationList) &&
          !is.null(body$InformationList$Information)) {
        info <- body$InformationList$Information
        if (is.data.frame(info) && "Synonym" %in% names(info) && nrow(info) > 0) {
          syns <- info$Synonym[[1]]
          if (is.character(syns) && length(syns) > 0) {
            return(tolower(trimws(syns)))
          }
        }
      }
      return(character(0))
    }

    if (status == 404) return(character(0))

    if (status %in% c(503, 500, 429)) {
      Sys.sleep(2 * attempt)
      next
    }

    return(character(0))
  }

  character(0)
}

#' Build reverse-lookup tables from a metabobase for synonym matching
#' @param metabobase MetaboBase object
#' @return List of named vectors for fast lookup
.build_metabobase_reverse_lookups <- function(metabobase) {
  background <- unique(metabobase$annot_long$metabolite_id)

  # Direct ID match (Layer 1 style)
  direct_ids <- stats::setNames(background, tolower(background))

  # Name lookup (Layer 2 style)
  name_to_id <- character(0)
  meta <- metabobase$metabolite_meta
  if (!is.null(meta) && is.data.frame(meta) &&
      "name" %in% names(meta) && "metabolite_id" %in% names(meta)) {
    name_to_id <- stats::setNames(meta$metabolite_id, tolower(trimws(meta$name)))
    name_to_id <- name_to_id[!is.na(names(name_to_id)) & nzchar(names(name_to_id))]
  }

  # Synonym lookup (Layer 3 style)
  syn_to_id <- character(0)
  syns <- metabobase$synonyms
  if (!is.null(syns) && is.data.frame(syns) && nrow(syns) > 0 &&
      "synonym" %in% names(syns) && "metabolite_id" %in% names(syns)) {
    syn_to_id <- stats::setNames(syns$metabolite_id, syns$synonym)
    syn_to_id <- syn_to_id[!duplicated(names(syn_to_id))]
  }

  # Cross-reference lookups (Layer 4 style)
  xref_maps <- list()
  id_xref <- metabobase$id_xref
  if (!is.null(id_xref) && is.data.frame(id_xref) && nrow(id_xref) > 0) {
    xref_cols <- intersect(
      c("hmdb_id", "chebi_id", "pubchem_cid", "pubchem_id", "cas_id", "kegg_id"),
      names(id_xref)
    )
    for (xc in xref_cols) {
      xref_vals <- id_xref[[xc]]
      if (!all(is.na(xref_vals))) {
        xm <- stats::setNames(id_xref$metabolite_id, tolower(trimws(as.character(xref_vals))))
        xm <- xm[!is.na(names(xm)) & nzchar(names(xm))]
        xref_maps[[xc]] <- xm
      }
    }
  }

  list(
    direct_ids = direct_ids,
    name_to_id = name_to_id,
    syn_to_id  = syn_to_id,
    xref_maps  = xref_maps
  )
}

#' Try to match a vector of synonyms against metabobase lookup tables
#' @param synonyms Character vector of lowercased synonyms from PubChem
#' @param lookup_tables Output of .build_metabobase_reverse_lookups()
#' @return Single metabolite_id string, or NA_character_ if no match
.match_synonyms_against_metabobase <- function(synonyms, lookup_tables) {
  # Try direct ID match first (synonym might literally be an HMDB or KEGG ID)
  hits <- lookup_tables$direct_ids[synonyms]
  hit <- hits[!is.na(hits)]
  if (length(hit) > 0) return(as.character(hit[1]))

  # Try name match
  hits <- lookup_tables$name_to_id[synonyms]
  hit <- hits[!is.na(hits)]
  if (length(hit) > 0) return(as.character(hit[1]))

  # Try synonym match
  if (length(lookup_tables$syn_to_id) > 0) {
    hits <- lookup_tables$syn_to_id[synonyms]
    hit <- hits[!is.na(hits)]
    if (length(hit) > 0) return(as.character(hit[1]))
  }

  # Try cross-reference matches
  for (xm in lookup_tables$xref_maps) {
    hits <- xm[synonyms]
    hit <- hits[!is.na(hits)]
    if (length(hit) > 0) return(as.character(hit[1]))
  }

  NA_character_
}

#' Resolve unmatched metabolite names via PubChem synonym lookup
#'
#' For each unmatched name: fetch synonyms from PubChem, then try to match
#' each synonym against the metabobase's name/synonym/xref tables.
#' Results are cached locally to avoid repeated API calls.
#'
#' @param names Character vector of unmatched metabolite names
#' @param metabobase MetaboBase object (needs metabolite_meta, synonyms, id_xref)
#' @param rate_delay Seconds between API calls (default 0.25 = 4 req/sec)
#' @param progress_callback Optional function(msg) for progress logging
#' @return Named character vector: original_name -> metabolite_id (only matches)
pubchem_synonym_resolve_names <- function(names, metabobase,
                                          rate_delay = 0.25,
                                          progress_callback = NULL) {
  if (length(names) == 0) return(character(0))

  log_msg <- function(msg) {
    if (is.function(progress_callback)) progress_callback(msg)
  }

  # Build reverse-lookup tables from metabobase (once)
  lookup_tables <- .build_metabobase_reverse_lookups(metabobase)

  # Load cache
  cache <- pubchem_load_synonym_cache()

  n_total <- length(names)
  log_msg(sprintf("PubChem synonym resolver: processing %d unmatched names...", n_total))

  result_names <- character(0)
  result_ids   <- character(0)
  n_cache_hit  <- 0L
  n_api_called <- 0L
  n_matched    <- 0L
  cache_dirty  <- FALSE

  for (i in seq_along(names)) {
    nm <- names[i]
    nm_lower <- tolower(trimws(nm))

    # Progress every 50 names
    if (i %% 50 == 0 || i == n_total) {
      log_msg(sprintf("PubChem synonym resolver: %d/%d processed (%d matched, %d API calls, %d cache hits)",
                      i, n_total, n_matched, n_api_called, n_cache_hit))
    }

    # Check cache first
    syns <- cache[[nm_lower]]
    if (!is.null(syns)) {
      n_cache_hit <- n_cache_hit + 1L
    } else {
      # Query PubChem API
      n_api_called <- n_api_called + 1L
      syns <- pubchem_fetch_synonyms(nm)
      cache[[nm_lower]] <- syns
      cache_dirty <- TRUE

      # Rate limiting (only for actual API calls)
      Sys.sleep(rate_delay)
    }

    if (length(syns) == 0) next

    # Try to match any synonym against metabobase
    matched_id <- .match_synonyms_against_metabobase(syns, lookup_tables)

    if (!is.na(matched_id)) {
      n_matched <- n_matched + 1L
      result_names <- c(result_names, nm)
      result_ids   <- c(result_ids, matched_id)
    }
  }

  # Save updated cache
  if (cache_dirty) {
    pubchem_save_synonym_cache(cache)
  }

  log_msg(sprintf("PubChem synonym resolver complete: %d/%d matched (%d API calls, %d cache hits)",
                  n_matched, n_total, n_api_called, n_cache_hit))

  if (length(result_names) > 0) {
    stats::setNames(result_ids, result_names)
  } else {
    character(0)
  }
}
