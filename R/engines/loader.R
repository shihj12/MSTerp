# R/engines/loader.R
# Raw file loader + formatted workbook IO (data + design sheets)

msterp_require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         "\nInstall with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && (length(a) > 1 || (!is.na(a) && nzchar(as.character(a))))) a else b


msterp_file_ext <- function(path) {
  tolower(tools::file_ext(path))
}

msterp_read_raw_file <- function(path, sheet = NULL) {
  ext <- msterp_file_ext(path)
  
  if (ext %in% c("xlsx", "xlsm", "xls")) {
    msterp_require_pkgs(c("readxl"))
    sheets <- readxl::excel_sheets(path)
    if (length(sheets) == 0) stop("No sheets found in Excel file: ", path)
    if (is.null(sheet)) sheet <- sheets[[1]]
    df <- readxl::read_excel(path, sheet = sheet)
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    return(list(data = df, source = list(path = path, ext = ext, sheet = sheet, sheets = sheets)))
  }
  
  if (ext %in% c("csv", "tsv", "txt")) {
    # Default delimiter: csv -> comma; tsv/txt -> tab
    delim <- if (ext == "csv") "," else "\t"
    df <- utils::read.table(path, sep = delim, header = TRUE, quote = "\"",
                            comment.char = "", stringsAsFactors = FALSE,
                            check.names = FALSE)
    return(list(data = df, source = list(path = path, ext = ext, sheet = NULL, sheets = NULL)))
  }
  
  stop("Unsupported file type: .", ext, "\nSupported: .xlsx, .csv, .tsv, .txt")
}

msterp_build_design_sheet <- function(meta, groups_df, columns_df) {
  # meta: named list
  # groups_df: data.frame with group_id, group_name, color, is_control (optional)
  # columns_df: data.frame with data_col, display_name, group_id, group_name, replicate, include, source_file, source_col

  if (!is.list(meta) || length(meta) == 0) stop("meta must be a non-empty named list.")
  if (!all(c("group_id", "group_name", "color") %in% names(groups_df))) {
    stop("groups_df must contain: group_id, group_name, color")
  }
  if (!all(c("data_col", "display_name", "group_id", "group_name", "replicate", "include") %in% names(columns_df))) {
    stop("columns_df must contain: data_col, display_name, group_id, group_name, replicate, include")
  }

  # Meta rows
  meta_df <- data.frame(
    record_type = "meta",
    key = names(meta),
    value = vapply(meta, function(x) paste0(x, collapse = "|"), character(1)),
    stringsAsFactors = FALSE
  )

  # Group rows
  group_df <- groups_df
  group_df$record_type <- "group"
  group_df$key <- NA_character_
  group_df$value <- NA_character_

  # Ensure is_control is present (defaults to FALSE if not provided)
  if (!"is_control" %in% names(group_df)) {
    group_df$is_control <- FALSE
  }

  # Column rows
  col_df <- columns_df
  col_df$record_type <- "column"
  col_df$key <- NA_character_
  col_df$value <- NA_character_

  # Normalize column set
  all_cols <- c(
    "record_type", "key", "value",
    "group_id", "group_name", "color", "is_control",
    "data_col", "display_name", "replicate", "include",
    "source_file", "source_col", "source_dataset", "channel"
  )

  # Ensure all exist
  for (nm in all_cols) {
    if (!nm %in% names(meta_df)) meta_df[[nm]] <- NA_character_
    if (!nm %in% names(group_df)) group_df[[nm]] <- NA_character_
    if (!nm %in% names(col_df))   col_df[[nm]]   <- NA_character_
  }

  meta_df <- meta_df[, all_cols, drop = FALSE]
  group_df <- group_df[, all_cols, drop = FALSE]
  col_df   <- col_df[, all_cols, drop = FALSE]

  rbind(meta_df, group_df, col_df)
}

msterp_group_name_regex <- function() {
  "^[A-Za-z][A-Za-z0-9 _.-]*$"
}

msterp_is_valid_group_name <- function(name, regex = msterp_group_name_regex()) {
  name <- as.character(name %||% "")
  vapply(name, function(x) {
    x <- trimws(as.character(x %||% ""))
    nzchar(x) && grepl(regex, x, perl = TRUE)
  }, logical(1))
}

msterp_write_formatted_xlsx <- function(out_path, data_df, design_df) {
  msterp_require_pkgs(c("openxlsx"))
  
  if (!is.data.frame(data_df)) stop("data_df must be a data.frame.")
  if (!is.data.frame(design_df)) stop("design_df must be a data.frame.")
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "data")
  openxlsx::addWorksheet(wb, "design")
  
  openxlsx::writeData(wb, sheet = "data", x = data_df)
  openxlsx::writeData(wb, sheet = "design", x = design_df)
  
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  invisible(out_path)
}

msterp_read_formatted_xlsx <- function(path) {
  msterp_require_pkgs(c("readxl"))

  sheets <- readxl::excel_sheets(path)
  sheets_l <- tolower(sheets)

  # Check for single-dataset (data + design) or multi-dataset (data_a + data_b + design) format
  has_single_data <- "data" %in% sheets_l
  has_multi_data <- all(c("data_a", "data_b") %in% sheets_l)
  has_design <- "design" %in% sheets_l

  if (!has_design) {
    stop("Formatted file must contain sheet: 'design'. Found: ", paste(sheets, collapse = ", "))
  }
  if (!has_single_data && !has_multi_data) {
    stop("Formatted file must contain sheet 'data' or sheets 'data_a' and 'data_b'. Found: ", paste(sheets, collapse = ", "))
  }

  is_multi_dataset <- has_multi_data && !has_single_data

  # Read design sheet
  design_df <- readxl::read_excel(path, sheet = sheets[match("design", sheets_l)])
  design_df <- as.data.frame(design_df, stringsAsFactors = FALSE, check.names = FALSE)

  # Coerce common cols if present
  if ("include" %in% names(design_df)) {
    if (is.character(design_df$include)) {
      design_df$include <- tolower(design_df$include) %in% c("true", "t", "1", "yes", "y")
    } else {
      design_df$include <- as.logical(design_df$include)
    }
  }
  if ("replicate" %in% names(design_df)) {
    suppressWarnings(design_df$replicate <- as.integer(design_df$replicate))
  }

  if (is_multi_dataset) {
    # Multi-dataset format: read data_a and data_b separately
    data_a <- readxl::read_excel(path, sheet = sheets[match("data_a", sheets_l)])
    data_a <- as.data.frame(data_a, stringsAsFactors = FALSE, check.names = FALSE)

    data_b <- readxl::read_excel(path, sheet = sheets[match("data_b", sheets_l)])
    data_b <- as.data.frame(data_b, stringsAsFactors = FALSE, check.names = FALSE)

    list(
      path = path,
      data = NULL,
      data_a = data_a,
      data_b = data_b,
      design = design_df,
      is_multi_dataset = TRUE
    )
  } else {
    # Single-dataset format
    data_df <- readxl::read_excel(path, sheet = sheets[match("data", sheets_l)])
    data_df <- as.data.frame(data_df, stringsAsFactors = FALSE, check.names = FALSE)

    list(path = path, data = data_df, design = design_df, is_multi_dataset = FALSE)
  }
}

# R/engines/loader.R
# --- update msterp_validate_formatted() to enforce the new "secondary IDs required" rule ---
# Replace the "need_keys" section + add the level-specific checks shown below.

msterp_validate_formatted <- function(obj) {
  errors <- character(0)
  warnings <- character(0)

  # Check for multi-dataset vs single-dataset format
  is_multi <- isTRUE(obj$is_multi_dataset)

  if (is_multi) {
    # Multi-dataset: require data_a and data_b
    if (is.null(obj$data_a) || !is.data.frame(obj$data_a)) errors <- c(errors, "Missing/invalid 'data_a' sheet.")
    if (is.null(obj$data_b) || !is.data.frame(obj$data_b)) errors <- c(errors, "Missing/invalid 'data_b' sheet.")
  } else {
    # Single-dataset: require data
    if (is.null(obj$data) || !is.data.frame(obj$data)) errors <- c(errors, "Missing/invalid 'data' sheet.")
  }
  if (is.null(obj$design) || !is.data.frame(obj$design)) errors <- c(errors, "Missing/invalid 'design' sheet.")
  
  if (length(errors) > 0) return(list(ok = FALSE, errors = errors, warnings = warnings))
  
  d <- obj$design
  if (!"record_type" %in% names(d)) errors <- c(errors, "Design sheet missing column: record_type")
  
  meta <- d[d$record_type == "meta", c("key", "value"), drop = FALSE]
  meta_keys <- meta$key
  meta_map <- stats::setNames(as.character(meta$value), meta$key)
  
  need_keys <- c("schema_version", "analysis_level", "id_primary_type", "id_primary_col", "created_at")
  missing_keys <- setdiff(need_keys, meta_keys)
  if (length(missing_keys) > 0) errors <- c(errors, paste0("Missing meta keys: ", paste(missing_keys, collapse = ", ")))

  # Analysis level validation:
  # - Canonical casing is lowercase ("protein"/"peptide"/"metabolite")
  # - For peptide-level formatted inputs, a protein ID column is still required so the pipeline can aggregate peptides to proteins.
  # - For metabolite-level, a metabolite ID column is required instead of protein ID column.
  lvl <- meta_map[["analysis_level"]]
  lvl_normalized <- tolower(trimws(as.character(lvl %||% "")))

  # Backwards compatibility: Infer data_type from analysis_level if missing
  # This supports legacy formatted files created before data_type was added.
  #
  # Inference rules:
  # - analysis_level = "metabolite" -> data_type = "metabolomics"
  # - analysis_level = "protein" or "peptide" -> data_type = "proteomics"
  #
  # A warning is produced to inform the user that the value was inferred.
  # This warning is informational only - validation still passes (ok = TRUE).
  raw_data_type <- meta_map[["data_type"]]

  # Handle missing, empty string, NA, and NULL consistently
  has_data_type <- !is.null(raw_data_type) &&
                   !is.na(raw_data_type) &&
                   nzchar(trimws(as.character(raw_data_type)))

  if (!has_data_type) {
    # Infer from analysis_level
    if (lvl_normalized == "metabolite") {
      inferred_data_type <- "metabolomics"
    } else {
      inferred_data_type <- "proteomics"
    }
    warnings <- c(warnings, sprintf(
      "Missing data_type meta key; inferred '%s' from analysis_level '%s'",
      inferred_data_type, lvl_normalized
    ))
    data_type <- inferred_data_type
  } else {
    data_type <- tolower(trimws(raw_data_type))
  }
  if (!lvl_normalized %in% c("protein", "peptide", "metabolite")) {
    errors <- c(errors, sprintf(
      "Only 'protein', 'peptide', or 'metabolite' levels are supported. Found: '%s'", lvl %||% "(missing)"
    ))
  }

  # Validate data_type value
  if (!data_type %in% c("proteomics", "metabolomics")) {
    errors <- c(errors, sprintf(
      "data_type must be 'proteomics' or 'metabolomics'. Found: '%s'",
      data_type
    ))
  }

  # Cross-validate data_type with analysis_level
  if (data_type == "proteomics" && lvl_normalized == "metabolite") {
    errors <- c(errors, "data_type = 'proteomics' is incompatible with analysis_level = 'metabolite'")
  }
  if (data_type == "metabolomics" && lvl_normalized %in% c("protein", "peptide")) {
    errors <- c(errors, sprintf(
      "data_type = 'metabolomics' is incompatible with analysis_level = '%s'",
      lvl_normalized
    ))
  }

  # Helper to get column names from data (handles multi-dataset)
  get_data_cols <- function() {
    if (is_multi) {
      # For multi-dataset, combine column names from both datasets
      union(names(obj$data_a), names(obj$data_b))
    } else if (!is.null(obj$data) && is.data.frame(obj$data)) {
      names(obj$data)
    } else {
      character(0)
    }
  }

  has_data <- if (is_multi) {
    !is.null(obj$data_a) && is.data.frame(obj$data_a) && nrow(obj$data_a) > 0
  } else {
    !is.null(obj$data) && is.data.frame(obj$data) && nrow(obj$data) > 0
  }

  # ID column requirements depend on analysis level
  if (lvl_normalized == "metabolite") {
    # Metabolite level: require id_metabolite_name_col (primary), id_metabolite_col optional
    if (is.na(meta_map[["id_metabolite_name_col"]]) || !nzchar(meta_map[["id_metabolite_name_col"]])) {
      errors <- c(errors, "Metabolite name column is required for metabolite-level data (meta key id_metabolite_name_col must be non-empty).")
    } else if (has_data) {
      data_cols <- get_data_cols()
      metab_name_col <- as.character(meta_map[["id_metabolite_name_col"]])
      if (nzchar(metab_name_col) && !metab_name_col %in% data_cols) {
        errors <- c(errors, sprintf("Metabolite name column '%s' not found in data sheet.", metab_name_col))
      }
      # Also check optional ID column if provided
      metab_id_col <- as.character(meta_map[["id_metabolite_col"]] %||% "")
      if (nzchar(metab_id_col) && !metab_id_col %in% data_cols) {
        warnings <- c(warnings, sprintf("Metabolite ID column '%s' not found in data sheet.", metab_id_col))
      }
    }
  } else {
    # Protein/peptide level: require id_protein_col
    if (is.na(meta_map[["id_protein_col"]]) || !nzchar(meta_map[["id_protein_col"]])) {
      errors <- c(errors, "Protein ID column is required (meta key id_protein_col must be non-empty).")
    } else if (has_data) {
      data_cols <- get_data_cols()
      prot_col <- as.character(meta_map[["id_protein_col"]])
      if (nzchar(prot_col) && !prot_col %in% data_cols) {
        errors <- c(errors, sprintf("Protein ID column '%s' not found in data sheet.", prot_col))
      }
    }
  }
  
  # ... keep the rest of your existing group/column checks unchanged ...
  
  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

