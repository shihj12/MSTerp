# =========================================================
# R/utils/metabolite_lookup.R — Metabolite Database API Utilities
#
# Functions for querying HMDB and KEGG REST APIs to retrieve:
# - Metabolite names and descriptions
# - Chemical formulas and classification
# - External database links
#
# Modeled after R/utils/uniprot.R
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Detect metabolite ID type
#'
#' @param id Character string
#' @return "hmdb", "kegg", or "name"
detect_metabolite_id_type <- function(id) {
  id <- trimws(as.character(id %||% ""))
  if (!nzchar(id)) return("name")

  if (grepl("^HMDB\\d+$", id, ignore.case = TRUE)) return("hmdb")
  if (grepl("^C\\d{5}$", id)) return("kegg")
  "name"
}

#' Fetch metabolite info from HMDB XML API
#'
#' @param hmdb_id HMDB accession (e.g., "HMDB0000001")
#' @return Named list with name, description, formula, class_hierarchy, url
fetch_hmdb_info <- function(hmdb_id) {
  hmdb_id <- toupper(trimws(as.character(hmdb_id)))
  empty <- list(
    name = NA_character_, description = NA_character_,
    formula = NA_character_, class_hierarchy = NA_character_,
    url = paste0("https://hmdb.ca/metabolites/", hmdb_id),
    source = "HMDB"
  )

  url <- sprintf("https://hmdb.ca/metabolites/%s.xml", hmdb_id)
  r <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)
  if (is.null(r) || httr::http_error(r)) return(empty)

  txt <- tryCatch(httr::content(r, "text", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(empty)

  doc <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
  if (is.null(doc)) return(empty)

  ns <- xml2::xml_ns(doc)

  # Helper to extract text from first matching node
  xval <- function(xpath) {
    nodes <- tryCatch(xml2::xml_find_first(doc, xpath, ns), error = function(e) NULL)
    if (is.null(nodes)) return(NA_character_)
    val <- xml2::xml_text(nodes)
    if (!nzchar(val)) return(NA_character_)
    val
  }

  name <- xval(".//d1:name") %||% xval(".//name")
  description <- xval(".//d1:description") %||% xval(".//description")
  formula <- xval(".//d1:chemical_formula") %||% xval(".//chemical_formula")

  # Build class hierarchy
  kingdom <- xval(".//d1:kingdom") %||% xval(".//kingdom")
  super_class <- xval(".//d1:super_class") %||% xval(".//super_class")
  klass <- xval(".//d1:class") %||% xval(".//class")
  sub_class <- xval(".//d1:sub_class") %||% xval(".//sub_class")

  hierarchy_parts <- c(kingdom, super_class, klass, sub_class)
  hierarchy_parts <- hierarchy_parts[!is.na(hierarchy_parts) & nzchar(hierarchy_parts)]
  class_hierarchy <- if (length(hierarchy_parts)) paste(hierarchy_parts, collapse = " > ") else NA_character_

  # Truncate description to reasonable length
  if (!is.na(description) && nchar(description) > 500) {
    description <- paste0(substr(description, 1, 497), "...")
  }

  list(
    name = name,
    description = description,
    formula = formula,
    class_hierarchy = class_hierarchy,
    url = paste0("https://hmdb.ca/metabolites/", hmdb_id),
    source = "HMDB"
  )
}

#' Fetch metabolite info from KEGG REST API
#'
#' @param kegg_id KEGG compound ID (e.g., "C00001")
#' @return Named list with name, formula, exact_mass, url
fetch_kegg_compound_info <- function(kegg_id) {
  kegg_id <- trimws(as.character(kegg_id))
  empty <- list(
    name = NA_character_, description = NA_character_,
    formula = NA_character_, class_hierarchy = NA_character_,
    url = paste0("https://www.genome.jp/entry/", kegg_id),
    source = "KEGG"
  )

  url <- sprintf("https://rest.kegg.jp/get/%s", kegg_id)
  r <- tryCatch(httr::GET(url, httr::timeout(8)), error = function(e) NULL)
  if (is.null(r) || httr::http_error(r)) return(empty)

  txt <- tryCatch(httr::content(r, "text", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(empty)

  lines <- strsplit(txt, "\n")[[1]]

  # Parse KEGG flat-file format
  extract_field <- function(field) {
    idx <- grep(paste0("^", field, "\\s+"), lines)
    if (length(idx) == 0) return(NA_character_)
    val <- sub(paste0("^", field, "\\s+"), "", lines[idx[1]])
    trimws(val)
  }

  name_raw <- extract_field("NAME")
  # KEGG names end with semicolon, may have multiple names separated by semicolons
  if (!is.na(name_raw)) {
    name_raw <- sub(";\\s*$", "", name_raw)
  }

  formula <- extract_field("FORMULA")
  exact_mass <- extract_field("EXACT_MASS")

  # Build description from exact mass if available
  desc_parts <- character(0)
  if (!is.na(formula)) desc_parts <- c(desc_parts, paste0("Formula: ", formula))
  if (!is.na(exact_mass)) desc_parts <- c(desc_parts, paste0("Exact mass: ", exact_mass))
  description <- if (length(desc_parts)) paste(desc_parts, collapse = " | ") else NA_character_

  list(
    name = name_raw,
    description = description,
    formula = formula,
    class_hierarchy = NA_character_,
    url = paste0("https://www.genome.jp/entry/", kegg_id),
    source = "KEGG"
  )
}

#' Fetch metabolite info by name via PubChem REST API
#'
#' Queries PubChem PUG REST for compound properties by name.
#' Falls back to HMDB search URL if PubChem lookup fails.
#'
#' @param name Metabolite common name
#' @return Named list with name, description, formula, class_hierarchy, url, source
fetch_metabolite_by_name <- function(name) {
  name <- trimws(as.character(name %||% ""))
  hmdb_url <- paste0("https://hmdb.ca/unearth/q?query=",
                      utils::URLencode(name, reserved = TRUE))
  empty <- list(
    name = name,
    description = NA_character_,
    formula = NA_character_,
    class_hierarchy = NA_character_,
    url = hmdb_url,
    source = "HMDB Search"
  )
  if (!nzchar(name)) return(empty)

  # Try PubChem PUG REST for compound properties
  encoded <- utils::URLencode(name, reserved = TRUE)
  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
                encoded, "/property/MolecularFormula,MolecularWeight,IUPACName/JSON")
  r <- tryCatch(httr::GET(url, httr::timeout(8)), error = function(e) NULL)
  if (is.null(r) || httr::http_error(r)) return(empty)

  body <- tryCatch(
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = TRUE),
    error = function(e) NULL
  )
  props <- body$PropertyTable$Properties
  if (is.null(props) || !is.data.frame(props) || nrow(props) == 0) return(empty)

  row <- props[1, ]
  cid <- row$CID
  formula <- if ("MolecularFormula" %in% names(row)) as.character(row$MolecularFormula) else NA_character_
  mw <- if ("MolecularWeight" %in% names(row)) as.character(row$MolecularWeight) else NA_character_
  iupac <- if ("IUPACName" %in% names(row)) as.character(row$IUPACName) else NA_character_

  desc_parts <- character(0)
  if (!is.na(iupac) && nzchar(iupac)) desc_parts <- c(desc_parts, paste0("IUPAC: ", iupac))
  if (!is.na(mw) && nzchar(mw)) desc_parts <- c(desc_parts, paste0("MW: ", mw))
  description <- if (length(desc_parts)) paste(desc_parts, collapse = " | ") else NA_character_

  pubchem_url <- if (!is.null(cid) && !is.na(cid)) {
    paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid)
  } else hmdb_url

  list(
    name = name,
    description = description,
    formula = formula,
    class_hierarchy = NA_character_,
    url = pubchem_url,
    source = "PubChem"
  )
}

#' Fetch metabolite info with auto-detection of ID type
#'
#' @param id Metabolite ID or name
#' @return Named list with name, description, formula, class_hierarchy, url, source
fetch_metabolite_info <- function(id) {
  id_type <- detect_metabolite_id_type(id)
  switch(id_type,
    hmdb = fetch_hmdb_info(id),
    kegg = fetch_kegg_compound_info(id),
    name = fetch_metabolite_by_name(id)
  )
}
