# R/engines/enrichment_utils.R
# Shared utilities for enrichment engines (goora, 1dgofcs, 2dgofcs)

#' Build term-to-protein (or term-to-compound) mapping using vectorized split()
#'
#' Replaces the O(n^2) loop pattern: term_proteins[[id]] <- c(term_proteins[[id]], pid)
#' with O(n) split() + lapply(unique).
#'
#' @param source Either a named list (protein -> term vectors) or a data.frame
#'   with entity and term columns.
#' @param pid_col (optional) Column name for entity IDs when source is a data.frame.
#'   If NULL, auto-detected from standard candidates.
#' @param term_col (optional) Column name for term IDs when source is a data.frame.
#'   If NULL, auto-detected from standard candidates.
#' @return Named list: term_id -> character vector of unique entity IDs
build_term_proteins <- function(source, pid_col = NULL, term_col = NULL) {
  if (is.null(source) || length(source) == 0) return(list())

  if (is.data.frame(source)) {
    # --- Data.frame format ---
    if (nrow(source) == 0) return(list())

    # Auto-detect columns if not provided
    if (is.null(pid_col)) {
      candidates <- intersect(
        c("protein_id", "uniprot_id", "id", "compound_id", "hmdb_id", "kegg_id"),
        names(source)
      )
      if (length(candidates) == 0) return(list())
      pid_col <- candidates[1]
    }
    if (is.null(term_col)) {
      candidates <- intersect(
        c("go_id", "go_ids", "term_id", "pathway_id", "kegg_pathway"),
        names(source)
      )
      if (length(candidates) == 0) return(list())
      term_col <- candidates[1]
    }

    pids <- as.character(source[[pid_col]])
    raw_terms <- as.character(source[[term_col]])

    # Expand comma/semicolon-separated terms
    split_terms <- strsplit(raw_terms, "[,;]")
    expanded_pid <- rep(pids, lengths(split_terms))
    expanded_term <- trimws(unlist(split_terms, use.names = FALSE))

    keep <- !is.na(expanded_term) & nzchar(expanded_term)
    if (!any(keep)) return(list())

    result <- split(expanded_pid[keep], expanded_term[keep])
    lapply(result, unique)

  } else if (is.list(source)) {
    # --- Named list format: entity -> term vectors ---
    nms <- names(source)
    if (is.null(nms) || length(nms) == 0) return(list())

    lens <- lengths(source)
    if (sum(lens) == 0) return(list())

    all_terms <- unlist(source, use.names = FALSE)
    all_pids <- rep(nms, lens)

    # Clean
    all_terms <- as.character(all_terms)
    keep <- !is.na(all_terms) & nzchar(all_terms)
    if (!any(keep)) return(list())

    result <- split(all_pids[keep], all_terms[keep])
    lapply(result, unique)

  } else {
    list()
  }
}
