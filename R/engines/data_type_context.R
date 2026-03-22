# =========================================================
# R/engines/data_type_context.R — Data Type Context Factory
#
# Provides terminology, column-name mappings, and labels
# that vary by omics data type (proteomics, metabolomics,
# multi-dataset).  Every engine receives a context object
# via payload$data_type_context so it can produce correct
# user-facing text without hard-coding "gene" everywhere.
# =========================================================

#' Build a data-type context object
#'
#' @param data_type Character: "proteomics", "metabolomics", or "multi"
#' @param metadata  List of run metadata (as produced by nr_extract_metadata)
#' @return Named list with terminology and column-name mappings
msterp_data_type_context <- function(data_type, metadata = list()) {
  data_type <- tolower(trimws(as.character(data_type %||% "proteomics")[1]))

  ctx <- switch(data_type,
    "proteomics" = list(
      data_type      = "proteomics",
      entity_label   = "gene",
      entity_plural  = "genes",
      list_label     = "gene list",
      id_col_key     = "id_gene_col",
      id_fallback    = "id_primary_col",
      output_id_col  = "gene_symbol",
      primary_id_col = "protein_id",
      db_type        = "terpbase"
    ),
    "metabolomics" = list(
      data_type      = "metabolomics",
      entity_label   = "metabolite",
      entity_plural  = "metabolites",
      list_label     = "metabolite list",
      id_col_key     = "id_metabolite_name_col",
      id_fallback    = "id_primary_col",
      output_id_col  = "metabolite_id",
      primary_id_col = "metabolite_id",
      db_type        = "metabobase"
    ),
    "multi" = list(
      data_type      = "multi",
      entity_label   = "feature",
      entity_plural  = "features",
      list_label     = "feature list",
      id_col_key     = "id_primary_col",
      id_fallback    = "id_primary_col",
      output_id_col  = "feature_id",
      primary_id_col = "feature_id",
      db_type        = NULL
    ),
    # Fallback — treat unknown types as proteomics for safety
    list(
      data_type      = data_type,
      entity_label   = "feature",
      entity_plural  = "features",
      list_label     = "feature list",
      id_col_key     = "id_primary_col",
      id_fallback    = "id_primary_col",
      output_id_col  = "feature_id",
      primary_id_col = "feature_id",
      db_type        = NULL
    )
  )

  ctx
}

#' Return singular or plural entity label based on count
#'
#' @param ctx  Data-type context (from msterp_data_type_context)
#' @param n    Integer count
#' @return Character label, e.g. "1 gene" or "5 metabolites"
ctx_label <- function(ctx, n = 1) {
  if (n == 1) ctx$entity_label else ctx$entity_plural
}

#' Resolve the best display-ID column from metadata + ids data.frame
#'
#' Tries ctx$id_col_key first, then ctx$id_fallback.
#' @param ctx      Data-type context
#' @param metadata Run metadata list
#' @param id_names Column names available in the ids data.frame
#' @return Column name (character) or NULL if nothing found
ctx_resolve_id_col <- function(ctx, metadata, id_names) {
  col <- as.character(metadata[[ctx$id_col_key]] %||% "")[1]
  if (nzchar(col) && col %in% id_names) return(col)

  col <- as.character(metadata[[ctx$id_fallback]] %||% "")[1]
  if (nzchar(col) && col %in% id_names) return(col)

  NULL
}
