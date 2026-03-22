# =========================================================
# R/utils/uniprot.R — UniProt API utilities
#
# Functions for querying UniProt REST API to retrieve:
# - Gene symbols
# - Functional annotations
# - Protein metadata
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Species taxonomy ID mapping
species_to_taxid <- function(species) {
  species <- tolower(trimws(species))
  switch(species,
    "human" = 9606,
    "mouse" = 10090,
    "rat" = 10116,
    "zebrafish" = 7955,
    "fly" = 7227,
    "worm" = 6239,
    "yeast" = 559292,
    9606  # default to human
  )
}

# Check if ID looks like Ensembl format
looks_like_ensembl <- function(id) {
  grepl("^ENS[A-Z]*[GPT]\\d+", id, ignore.case = TRUE)
}

# Check if ID looks like UniProt accession format
# UniProt accessions: [OPQ][0-9][A-Z0-9]{3}[0-9] or [A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9]
# Optionally followed by -N for isoforms
looks_like_uniprot_acc <- function(id) {
  if (is.null(id) || !nzchar(id)) return(FALSE)
  # Standard UniProt accession patterns
  grepl("^[OPQ][0-9][A-Z0-9]{3}[0-9](-\\d+)?$", id, ignore.case = FALSE) ||
    grepl("^[A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9](-\\d+)?$", id, ignore.case = FALSE) ||
    # Newer 10-character accessions
    grepl("^[A-NR-Z][0-9][A-Z][A-Z0-9]{2}[0-9][A-Z][A-Z0-9]{2}[0-9](-\\d+)?$", id, ignore.case = FALSE)
}

# Query UniProt search API and return first accession
.uniprot_first_acc <- function(query, size = 1) {
  url <- sprintf(
    "https://rest.uniprot.org/uniprotkb/search?query=%s&fields=accession&size=%d&format=json",
    URLencode(query, reserved = TRUE), size
  )
  r <- tryCatch(httr::GET(url, httr::timeout(8)), error = function(e) NULL)
  if (is.null(r) || httr::http_error(r)) return(NA_character_)
  txt <- tryCatch(httr::content(r, "text", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(NA_character_)
  js <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(js) || is.null(js$results) || !NROW(js$results)) return(NA_character_)
  js$results$primaryAccession[1]
}

# Lookup UniProt accession from gene ID, Ensembl ID, or already-formatted accession
lookup_uniprot_acc_fallback <- function(id, species) {
  if (is.null(id) || !nzchar(id)) return(NA_character_)

  # If the ID already looks like a UniProt accession, return it directly
  # This handles PCA-paired 1DGOFCS where protein_ids are already accessions
  if (looks_like_uniprot_acc(id)) {
    return(toupper(trimws(id)))
  }

  tax <- species_to_taxid(species)

  if (looks_like_ensembl(id)) {
    acc <- .uniprot_first_acc(sprintf("(xref:Ensembl:%s) AND organism_id:%d AND reviewed:true", id, tax))
    if (!is.na(acc)) return(acc)
    acc <- .uniprot_first_acc(sprintf("(xref:Ensembl:%s) AND organism_id:%d", id, tax))
    if (!is.na(acc)) return(acc)
  }

  acc <- .uniprot_first_acc(sprintf("(gene_exact:%s OR gene:%s) AND organism_id:%d AND reviewed:true", id, id, tax))
  if (!is.na(acc)) return(acc)
  acc <- .uniprot_first_acc(sprintf("(gene_exact:%s OR gene:%s) AND organism_id:%d", id, id, tax))
  if (!is.na(acc)) return(acc)

  NA_character_
}

# Fetch gene symbol and function text from UniProt JSON
fetch_uniprot_info <- function(acc, try_canonical = TRUE) {
  if (is.na(acc) || !nzchar(acc)) {
    return(list(gene = NA_character_, function_text = NA_character_))
  }
  acc <- toupper(trimws(acc))

  url <- sprintf("https://rest.uniprot.org/uniprotkb/%s.json", acc)
  r <- tryCatch(httr::GET(url, httr::timeout(8)), error = function(e) NULL)
  if (is.null(r) || httr::http_error(r)) {
    if (try_canonical && grepl("-\\d+$", acc)) {
      acc_main <- sub("-\\d+$", "", acc)
      if (nzchar(acc_main) && !identical(acc_main, acc)) {
        return(fetch_uniprot_info(acc_main, try_canonical = FALSE))
      }
    }
    return(list(gene = NA_character_, function_text = NA_character_))
  }

  txt <- tryCatch(httr::content(r, "text", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) {
    return(list(gene = NA_character_, function_text = NA_character_))
  }
  js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(js)) {
    return(list(gene = NA_character_, function_text = NA_character_))
  }

  # Extract gene symbol
  gene_symbol <- NA_character_
  if (!is.null(js$genes) && length(js$genes)) {
    g1 <- js$genes[[1]]
    if (!is.null(g1$geneName) && !is.null(g1$geneName$value) && nzchar(g1$geneName$value)) {
      gene_symbol <- g1$geneName$value
    } else if (!is.null(g1$synonyms) && length(g1$synonyms)) {
      syn_values <- vapply(
        g1$synonyms,
        function(x) x$value %||% "",
        character(1)
      )
      syn_values <- syn_values[nzchar(syn_values)]
      if (length(syn_values)) gene_symbol <- syn_values[1]
    }
  }

  # Extract function text
  fun_txt <- NA_character_
  if (!is.null(js$comments) && length(js$comments)) {
    vals <- character()
    for (cmt in js$comments) {
      ct <- tryCatch(cmt$commentType, error = function(e) NULL)
      if (!identical(ct, "FUNCTION")) next
      txs <- tryCatch(cmt$texts, error = function(e) NULL)
      if (is.null(txs) || !length(txs)) next
      for (tx in txs) {
        v <- tryCatch(tx$value, error = function(e) NULL)
        if (is.character(v) && nzchar(v)) vals <- c(vals, v)
      }
    }
    if (length(vals)) {
      fun_txt <- paste(vals, collapse = " ")
    }
  }

  list(
    gene          = gene_symbol,
    function_text = fun_txt
  )
}

# Vectorized wrapper for batch lookups
fetch_uniprot_batch <- function(accs, progress = TRUE) {
  if (progress && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "Fetching UniProt [:bar] :percent eta: :eta",
      total = length(accs),
      clear = FALSE
    )
  } else {
    pb <- NULL
  }

  results <- lapply(accs, function(acc) {
    if (!is.null(pb)) pb$tick()
    fetch_uniprot_info(acc)
  })

  list(
    genes = vapply(results, function(x) x$gene %||% NA_character_, character(1)),
    functions = vapply(results, function(x) x$function_text %||% NA_character_, character(1))
  )
}
