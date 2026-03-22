# R/engines/stats/peptide_aggregate_to_protein.R - Peptide -> Protein Aggregation Engine

`%||%` <- function(a, b) if (!is.null(a)) a else b

stats_peptide_aggregate_to_protein_run <- function(payload, params, context = NULL) {
  engine_id <- "peptide_aggregate_to_protein"

  mat <- payload$mat
  ids <- payload$ids
  meta <- payload$metadata %||% list()

  if (is.null(mat) || !is.matrix(mat)) {
    return(list(engine_id = engine_id, params = params, data = list(error = "Missing matrix in payload")))
  }
  if (is.null(ids) || !is.data.frame(ids)) {
    return(list(engine_id = engine_id, params = params, data = list(error = "Missing ids table in payload")))
  }

  id_protein_col <- as.character(meta$id_protein_col %||% "")[1]
  id_gene_col <- as.character(meta$id_gene_col %||% "")[1]

  if (!nzchar(id_protein_col) || !id_protein_col %in% names(ids)) {
    return(list(
      engine_id = engine_id,
      params = params,
      data = list(error = sprintf("Protein ID column not found in ids: %s", id_protein_col %||% ""))
    ))
  }

  # Track input row count for log

  rows_before <- nrow(mat)

  prot <- as.character(ids[[id_protein_col]])
  prot[is.na(prot)] <- ""
  keep <- nzchar(prot)
  if (!any(keep)) {
    return(list(engine_id = engine_id, params = params, data = list(error = "No non-empty protein IDs found for aggregation")))
  }

  prot <- prot[keep]
  mat_in <- mat[keep, , drop = FALSE]
  ids_in <- ids[keep, , drop = FALSE]

  # Split peptide rows by protein ID
  idx_by_prot <- split(seq_len(nrow(mat_in)), prot)
  prot_ids <- names(idx_by_prot)

  # Select aggregation function based on params
  method <- tolower(params$method %||% "sum")
  agg_fun <- switch(method,
    "arithmetic_mean" = function(x) mean(x, na.rm = TRUE),
    "harmonic_mean" = function(x) {
      # Harmonic mean: n / sum(1/x) for positive values
      x <- x[!is.na(x) & x > 0]
      if (length(x) == 0) return(NA_real_)
      length(x) / sum(1 / x)
    },
    # Default to sum
    function(x) sum(x, na.rm = TRUE)
  )

  mat_out <- t(vapply(idx_by_prot, function(rows) {
    apply(mat_in[rows, , drop = FALSE], 2, agg_fun)
  }, numeric(ncol(mat_in))))
  colnames(mat_out) <- colnames(mat_in)
  rownames(mat_out) <- prot_ids

  # Silent zero-to-NA replacement: replace exact zeros with NA

  mat_out[mat_out == 0] <- NA_real_

  pick_first_nonempty <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0) NA_character_ else x[[1]]
  }

  ids_out <- data.frame(prot = prot_ids, stringsAsFactors = FALSE)
  names(ids_out)[[1]] <- id_protein_col

  if (nzchar(id_gene_col) && id_gene_col %in% names(ids_in)) {
    gene_out <- vapply(idx_by_prot, function(rows) pick_first_nonempty(ids_in[[id_gene_col]][rows]), character(1))
    ids_out[[id_gene_col]] <- unname(gene_out)
  }

  # Track output row count for log
  rows_after <- nrow(mat_out)

  # Summary table with per-protein peptide counts
  summary_df <- data.frame(
    id_protein = prot_ids,
    n_peptides = vapply(idx_by_prot, length, integer(1)),
    stringsAsFactors = FALSE
  )

  # Aggregation log table showing rows before/after (similar to Data Processor)
  aggregation_log <- data.frame(
    Operation = c("Aggregate Peptides \u2192 Proteins"),
    Method = c(method),
    `Rows Before` = c(rows_before),
    `Rows After` = c(rows_after),
    `Proteins` = c(length(prot_ids)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  list(
    engine_id = engine_id,
    params = params,
    data = list(
      mat = mat_out,
      ids = ids_out,
      summary = summary_df,
      aggregation_log = aggregation_log
    )
  )
}
