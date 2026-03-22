# R/engines/merger.R
# Merge 2+ formatted MSTerp workbooks (data + design) into one workbook.

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || !nzchar(as.character(a)))) return(b)
  a
}
# --- REPLACE your entire msterp_collapse_dupes_first() with this (type-safe vapply) ---

msterp_collapse_dupes_first <- function(df, key_col, value_cols, coerce_char = FALSE) {
  if (!key_col %in% names(df)) stop("Key column not found: ", key_col)
  if (length(value_cols) == 0) return(unique(df[, key_col, drop = FALSE]))
  
  key <- df[[key_col]]
  keep <- !is.na(key) & nzchar(as.character(key))
  df <- df[keep, , drop = FALSE]
  
  if (anyDuplicated(df[[key_col]]) == 0) return(df[, c(key_col, value_cols), drop = FALSE])
  
  idx <- split(seq_len(nrow(df)), df[[key_col]])
  out <- data.frame(stringsAsFactors = FALSE, check.names = FALSE)
  out[[key_col]] <- names(idx)
  
  for (col in value_cols) {
    v <- df[[col]]
    
    if (coerce_char) {
      out[[col]] <- vapply(idx, function(ii) {
        y <- v[ii]
        y <- y[!is.na(y) & nzchar(as.character(y))]
        if (length(y) == 0) return(NA_character_)
        as.character(y[[1]])
      }, FUN.VALUE = character(1))
    } else {
      # Preserve a sensible output type
      if (is.factor(v)) {
        fv <- NA_character_
        out[[col]] <- vapply(idx, function(ii) {
          y <- v[ii]
          y <- y[!is.na(y)]
          if (length(y) == 0) return(fv)
          as.character(y[[1]])
        }, FUN.VALUE = character(1))
      } else if (is.integer(v)) {
        fv <- NA_integer_
        out[[col]] <- vapply(idx, function(ii) {
          y <- v[ii]
          y <- y[!is.na(y)]
          if (length(y) == 0) return(fv)
          y[[1]]
        }, FUN.VALUE = fv)
      } else if (is.numeric(v)) {
        fv <- NA_real_
        out[[col]] <- vapply(idx, function(ii) {
          y <- v[ii]
          y <- y[!is.na(y)]
          if (length(y) == 0) return(fv)
          y[[1]]
        }, FUN.VALUE = fv)
      } else if (is.logical(v)) {
        fv <- NA
        out[[col]] <- vapply(idx, function(ii) {
          y <- v[ii]
          y <- y[!is.na(y)]
          if (length(y) == 0) return(fv)
          y[[1]]
        }, FUN.VALUE = fv)
      } else {
        fv <- NA_character_
        out[[col]] <- vapply(idx, function(ii) {
          y <- v[ii]
          y <- y[!is.na(y) & nzchar(as.character(y))]
          if (length(y) == 0) return(fv)
          as.character(y[[1]])
        }, FUN.VALUE = character(1))
      }
    }
  }
  
  out
}


msterp_extract_meta <- function(design_df) {
  meta <- design_df[design_df$record_type == "meta", c("key", "value"), drop = FALSE]
  out <- as.list(as.character(meta$value))
  names(out) <- as.character(meta$key)
  out
}

msterp_extract_groups <- function(design_df) {
  g <- design_df[design_df$record_type == "group", , drop = FALSE]
  keep <- intersect(c("group_id", "group_name", "color", "is_control"), names(g))
  g <- g[, keep, drop = FALSE]
  g <- unique(g)

  # Ensure is_control is present (defaults to FALSE if not in design)
  if (!"is_control" %in% names(g)) {
    g$is_control <- FALSE
  } else {
    # Normalize is_control to logical, preserving NA when provided
    if (is.logical(g$is_control)) {
      # keep as-is
    } else if (is.numeric(g$is_control) || is.integer(g$is_control)) {
      g$is_control <- ifelse(is.na(g$is_control), NA, g$is_control != 0)
    } else {
      v <- tolower(trimws(as.character(g$is_control)))
      g$is_control <- ifelse(is.na(v) | !nzchar(v), NA, v %in% c("true", "t", "1", "yes"))
    }
  }
  g
}

msterp_extract_columns <- function(design_df) {
  cdf <- design_df[design_df$record_type == "column", , drop = FALSE]
  
  # Ensure minimum cols exist
  needed <- c("data_col", "display_name", "group_id", "group_name", "replicate", "include")
  for (nm in needed) if (!nm %in% names(cdf)) cdf[[nm]] <- NA
  if (!"source_file" %in% names(cdf)) cdf$source_file <- NA
  if (!"source_col"  %in% names(cdf)) cdf$source_col  <- NA
  
  # Keep only included columns (vector-safe)
  if ("include" %in% names(cdf)) {
    cdf$include <- if (is.logical(cdf$include)) {
      cdf$include
    } else {
      tolower(as.character(cdf$include)) %in% c("true", "t", "1", "yes", "y")
    }
    cdf <- cdf[!is.na(cdf$include) & cdf$include, , drop = FALSE]
  }
  
  cdf
}

msterp_primary_id_from_meta <- function(meta) {
  lvl <- tolower(trimws(as.character(meta$analysis_level %||% "")))
  pcol <- meta$id_primary_col
  ptype <- meta$id_primary_type
  if (!nzchar(lvl) || is.null(pcol) || is.null(ptype)) {
    stop("Missing primary id meta (analysis_level / id_primary_type / id_primary_col).")
  }
  list(level = lvl, primary_type = ptype, primary_col = pcol)
}

msterp_id_cols_from_meta <- function(meta) {
  # Keep a stable ordering: Gene, Protein (peptide removed)
  candidates <- c("id_gene_col", "id_protein_col")
  vals <- lapply(candidates, function(k) meta[[k]])
  cols <- unlist(vals, use.names = FALSE)
  cols <- cols[!is.na(cols) & nzchar(as.character(cols))]
  unique(as.character(cols))
}

msterp_merge_preview <- function(paths, file_ids = NULL) {
  msterp_require_pkgs(c("dplyr"))
  
  if (length(paths) < 2) stop("Provide 2 or more formatted files to merge.")
  
  datasets <- lapply(paths, function(p) {
    obj <- msterp_read_formatted_xlsx(p)
    val <- msterp_validate_formatted(obj)
    if (!isTRUE(val$ok)) {
      stop("Invalid formatted file: ", p, "\n- ", paste(val$errors, collapse = "\n- "))
    }
    obj
  })
  
  if (is.null(file_ids)) {
    file_ids <- tools::file_path_sans_ext(basename(paths))
  } else {
    if (length(file_ids) != length(paths)) stop("file_ids must have same length as paths.")
    file_ids <- make.names(file_ids, unique = TRUE)
  }
  
  metas  <- lapply(datasets, function(x) msterp_extract_meta(x$design))
  prims  <- lapply(metas, msterp_primary_id_from_meta)
  levels <- vapply(prims, function(z) z$level, character(1))
  if (length(unique(levels)) != 1) {
    stop("Files are not the same analysis level: ", paste(unique(levels), collapse = ", "))
  }
  if (!all(levels %in% c("protein", "peptide"))) {
    stop("Only protein or peptide level is supported. Found: ", paste(unique(levels), collapse = ", "))
  }
  
  # ID columns must match (same names, same order)
  id_cols_list <- lapply(metas, msterp_id_cols_from_meta)
  id_sig <- vapply(id_cols_list, function(v) paste(v, collapse = "|"), character(1))
  if (length(unique(id_sig)) != 1) {
    stop(
      "Input files do not have the same identifier columns.\nFound signatures:\n",
      paste(paste0(" - ", file_ids, ": ", id_sig), collapse = "\n")
    )
  }
  id_cols <- id_cols_list[[1]]
  primary_col <- prims[[1]]$primary_col
  
  maps <- Map(function(obj, fid) {
    cols <- msterp_extract_columns(obj$design)
    gr   <- msterp_extract_groups(obj$design)
    
    if (!"group_id" %in% names(cols)) cols$group_id <- NA_character_
    if (!"group_name" %in% names(cols)) cols$group_name <- NA_character_
    
    # Bring in authoritative group_name + color from group records
    if (!is.null(gr) && nrow(gr) > 0 && "group_id" %in% names(gr)) {
      gr2 <- gr[, intersect(c("group_id", "group_name", "color"), names(gr)), drop = FALSE]
      if ("group_name" %in% names(gr2)) names(gr2)[names(gr2) == "group_name"] <- "group_name_from_group"
      if ("color" %in% names(gr2))      names(gr2)[names(gr2) == "color"]      <- "color_from_group"
      
      cols <- dplyr::left_join(cols, gr2, by = "group_id")
      
      if ("group_name_from_group" %in% names(cols)) {
        cols$group_name <- ifelse(
          is.na(cols$group_name) | cols$group_name == "",
          cols$group_name_from_group,
          cols$group_name
        )
        cols$group_name_from_group <- NULL
      }
      
      if (!"color" %in% names(cols)) cols$color <- NA_character_
      if ("color_from_group" %in% names(cols)) {
        cols$color <- ifelse(
          is.na(cols$color) | cols$color == "",
          cols$color_from_group,
          cols$color
        )
        cols$color_from_group <- NULL
      }
    } else {
      if (!"color" %in% names(cols)) cols$color <- NA_character_
    }
    
    cols$source_file <- fid
    cols$source_col  <- cols$data_col
    cols
  }, datasets, file_ids)
  
  map_all <- dplyr::bind_rows(maps)
  
  # Suggest merged_col and display_name (must be unique)
  map_all$merged_col   <- map_all$data_col
  map_all$display_name <- ifelse(
    is.na(map_all$display_name) | map_all$display_name == "",
    map_all$data_col,
    map_all$display_name
  )
  
  # Resolve collisions for merged_col by prefixing with source_file
  dup_merged <- duplicated(map_all$merged_col) | duplicated(map_all$merged_col, fromLast = TRUE)
  map_all$merged_col[dup_merged] <- paste0(map_all$source_file[dup_merged], "__", map_all$merged_col[dup_merged])
  
  # Resolve collisions for display_name similarly
  dup_disp <- duplicated(map_all$display_name) | duplicated(map_all$display_name, fromLast = TRUE)
  map_all$display_name[dup_disp] <- paste0(map_all$source_file[dup_disp], "__", map_all$display_name[dup_disp])
  
  still_bad <- any(duplicated(map_all$merged_col)) || any(duplicated(map_all$display_name))
  
  list(
    level       = levels[[1]],
    primary_col = primary_col,
    id_cols     = id_cols,
    file_ids    = file_ids,
    datasets    = datasets,
    mapping     = map_all,
    needs_edit  = still_bad
  )
}

# --- REPLACE your entire msterp_merge_execute() with this (adds progress support + uses it) ---

msterp_merge_execute <- function(preview, mapping_df, out_path, progress = NULL) {
  msterp_require_pkgs(c("dplyr"))
  
  prog <- function(msg, pct = NULL) {
    if (is.function(progress)) {
      try(progress(msg, pct), silent = TRUE)
    }
  }
  
  prog("Validating inputs…", 5)
  
  if (is.null(preview) || is.null(preview$datasets)) stop("preview is missing datasets.")
  if (!is.data.frame(mapping_df) || nrow(mapping_df) == 0) stop("mapping_df must be a non-empty data.frame.")
  
  # ensure display_name exists and is filled
  if (!"display_name" %in% names(mapping_df)) mapping_df$display_name <- mapping_df$merged_col
  mapping_df$display_name[is.na(mapping_df$display_name) | !nzchar(as.character(mapping_df$display_name))] <- mapping_df$merged_col
  
  req_cols <- c("source_file", "source_col", "merged_col", "display_name", "group_name", "replicate", "color")
  if (!all(req_cols %in% names(mapping_df))) {
    stop("mapping_df missing required columns: ", paste(setdiff(req_cols, names(mapping_df)), collapse = ", "))
  }
  
  if (any(is.na(mapping_df$merged_col) | !nzchar(as.character(mapping_df$merged_col)))) {
    stop("merged_col contains empty names. Fix and retry.")
  }
  if (any(duplicated(mapping_df$merged_col))) stop("merged_col must be unique across all columns. Fix duplicates and retry.")
  
  datasets <- preview$datasets
  file_ids <- preview$file_ids
  primary_col <- preview$primary_col
  id_cols <- preview$id_cols
  
  # Ensure each input file has some mapped columns
  for (fid in file_ids) {
    map_i <- mapping_df[mapping_df$source_file == fid, , drop = FALSE]
    if (nrow(map_i) == 0) stop("No mapped measurement columns for file: ", fid)
  }
  
  # 1) UNION of primary IDs across all files
  prog("Building union of primary IDs…", 15)
  all_primary <- NULL
  for (i in seq_along(datasets)) {
    df <- datasets[[i]]$data
    fid <- file_ids[[i]]
    if (!primary_col %in% names(df)) stop("Primary ID column not found in file: ", fid)
    
    p <- unique(df[[primary_col]])
    p <- p[!is.na(p) & nzchar(as.character(p))]
    
    tmp <- data.frame(p, stringsAsFactors = FALSE, check.names = FALSE)
    names(tmp) <- primary_col
    
    all_primary <- if (is.null(all_primary)) tmp else dplyr::full_join(all_primary, tmp, by = primary_col)
    
    # lightweight progress update
    prog(sprintf("Building union of primary IDs… (%d/%d)", i, length(datasets)),
         15 + round(10 * i / max(1, length(datasets))))
  }
  
  # 2) ID map: coalesce secondary IDs across files (warn on conflicts)
  prog("Reconciling identifier columns…", 30)
  secondary_cols <- setdiff(id_cols, primary_col)
  
  id_wide <- all_primary
  for (i in seq_along(datasets)) {
    df <- datasets[[i]]$data
    fid <- file_ids[[i]]
    
    ids_i <- df[, id_cols, drop = FALSE]
    ids_i <- ids_i[!is.na(ids_i[[primary_col]]) & nzchar(as.character(ids_i[[primary_col]])), , drop = FALSE]
    
    if (anyDuplicated(ids_i[[primary_col]]) > 0) {
      warning(sprintf(
        "File '%s' has duplicated primary IDs in '%s'. Collapsing duplicates by first non-empty per ID column.",
        fid, primary_col
      ))
      ids_i <- msterp_collapse_dupes_first(ids_i, primary_col, secondary_cols, coerce_char = TRUE)
    } else {
      ids_i <- ids_i[, c(primary_col, secondary_cols), drop = FALSE]
    }
    
    ren <- setNames(paste0(secondary_cols, "__", fid), secondary_cols)
    ids_i2 <- ids_i
    names(ids_i2) <- ifelse(names(ids_i2) %in% names(ren), ren[names(ids_i2)], names(ids_i2))
    
    id_wide <- dplyr::left_join(id_wide, ids_i2, by = primary_col)
    
    prog(sprintf("Reconciling identifier columns… (%d/%d)", i, length(datasets)),
         30 + round(15 * i / max(1, length(datasets))))
  }
  
  # conflict warnings
  for (col in secondary_cols) {
    pat <- paste0("^", gsub("([\\W])", "\\\\\\1", col), "__")
    src_cols <- grep(pat, names(id_wide), value = TRUE)
    if (length(src_cols) <= 1) next
    
    vals <- id_wide[, src_cols, drop = FALSE]
    distinct_n <- apply(vals, 1, function(x) {
      x <- x[!is.na(x) & nzchar(as.character(x))]
      length(unique(as.character(x)))
    })
    n_conflict <- sum(distinct_n > 1, na.rm = TRUE)
    if (n_conflict > 0) {
      warning(sprintf(
        "ID mapping mismatch for '%s' across files: %d primary IDs have conflicting values. Proceeding by file order (first non-empty).",
        col, n_conflict
      ))
    }
  }
  
  # coalesce to final id map
  id_final <- all_primary
  for (col in secondary_cols) {
    pat <- paste0("^", gsub("([\\W])", "\\\\\\1", col), "__")
    src_cols <- grep(pat, names(id_wide), value = TRUE)
    if (length(src_cols) == 0) {
      id_final[[col]] <- NA_character_
    } else {
      id_final[[col]] <- do.call(dplyr::coalesce, as.list(id_wide[, src_cols, drop = FALSE]))
    }
  }
  id_final <- id_final[, c(primary_col, secondary_cols), drop = FALSE]
  
  # 3) Merge measurement columns onto the UNION of primary IDs
  prog("Merging measurement columns…", 50)
  merged_meas <- all_primary
  for (i in seq_along(datasets)) {
    df <- datasets[[i]]$data
    fid <- file_ids[[i]]
    
    map_i <- mapping_df[mapping_df$source_file == fid, , drop = FALSE]
    src_cols <- map_i$source_col
    
    if (!all(src_cols %in% names(df))) {
      missing <- src_cols[!src_cols %in% names(df)]
      stop("File ", fid, " is missing mapped data columns in 'data' sheet: ", paste(missing, collapse = ", "))
    }
    
    meas_i <- df[, c(primary_col, src_cols), drop = FALSE]
    meas_i <- meas_i[!is.na(meas_i[[primary_col]]) & nzchar(as.character(meas_i[[primary_col]])), , drop = FALSE]
    
    if (anyDuplicated(meas_i[[primary_col]]) > 0) {
      warning(sprintf(
        "File '%s' has duplicated primary IDs in '%s'. Collapsing duplicates by first non-NA per data column.",
        fid, primary_col
      ))
      meas_i <- msterp_collapse_dupes_first(meas_i, primary_col, src_cols, coerce_char = FALSE)
    }
    
    rename_vec <- setNames(map_i$merged_col, map_i$source_col)
    names(meas_i) <- ifelse(names(meas_i) %in% names(rename_vec), rename_vec[names(meas_i)], names(meas_i))
    
    merged_meas <- dplyr::left_join(merged_meas, meas_i, by = primary_col)
    
    prog(sprintf("Merging measurement columns… (%d/%d)", i, length(datasets)),
         50 + round(25 * i / max(1, length(datasets))))
  }
  
  merged_data <- dplyr::left_join(id_final, merged_meas, by = primary_col)
  
  # 4) Build merged design sheet (from edited mapping_df)
  prog("Building merged design…", 80)
  
  groups <- unique(mapping_df[, c("group_name", "color"), drop = FALSE])
  groups$group_id <- paste0("group_", seq_len(nrow(groups)))
  groups <- groups[, c("group_id", "group_name", "color"), drop = FALSE]
  
  mapping_df2 <- dplyr::left_join(mapping_df, groups, by = c("group_name", "color"))
  
  col_recs <- data.frame(
    data_col     = mapping_df2$merged_col,
    display_name = mapping_df2$display_name,
    group_id     = mapping_df2$group_id,
    group_name   = mapping_df2$group_name,
    replicate    = as.integer(mapping_df2$replicate),
    include      = TRUE,
    source_file  = mapping_df2$source_file,
    source_col   = mapping_df2$source_col,
    stringsAsFactors = FALSE
  )
  
  meta1 <- msterp_extract_meta(datasets[[1]]$design)
  meta <- list(
    schema_version   = "1.0",
    created_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    analysis_level   = preview$level,
    id_primary_type  = meta1$id_primary_type,
    id_primary_col   = primary_col,
    id_gene_col      = meta1$id_gene_col %||% "",
    id_protein_col   = meta1$id_protein_col %||% "",
    id_peptide_col   = meta1$id_peptide_col %||% "",
    merged_from      = paste(preview$file_ids, collapse = "|")
  )
  
  design_df <- msterp_build_design_sheet(meta = meta, groups_df = groups, columns_df = col_recs)
  
  prog("Writing merged workbook…", 95)
  msterp_write_formatted_xlsx(out_path, merged_data, design_df)
  prog("Done.", 100)
  
  invisible(out_path)
}
