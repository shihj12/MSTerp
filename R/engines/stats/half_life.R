# =========================================================
# R/engines/stats/half_life.R — Dynamic SILAC Half-Life Engine
#
# Computes protein half-lives from paired Heavy/Light channels:
#   t_half = t * ln(2) / ln(1 + H/L)
#
# Input:  SILAC formatted data with paired H/L columns per replicate
# Output: Half-life matrix (proteins x replicates) + QC plots
#
# Contract v1.0: Must output data$log, data$summary, data$mat, data$ids,
#   data$samples, data$groups (for context update)
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Execute half-life engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters (overrides payload$params)
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_half_life_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()
  params <- params %||% payload$params %||% list()

  # Initialize log
  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries <<- c(log_entries, list(list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level,
      message = msg
    )))
  }

  make_log_df <- function() {
    if (length(log_entries) == 0) {
      return(data.frame(time = character(), level = character(), message = character(),
                        stringsAsFactors = FALSE))
    }
    do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
  }

  error_result <- function(msg) {
    add_log("ERROR", msg)
    list(
      engine_id = "half_life",
      params = params,
      data = list(
        log = make_log_df(),
        summary = data.frame(metric = "error", value = msg, stringsAsFactors = FALSE)
      )
    )
  }

  # Validate payload
  if (!isTRUE(payload$ok)) {
    return(error_result(payload$error %||% "Invalid payload"))
  }

  # Validate SILAC mode
  if (!isTRUE(payload$is_silac)) {
    return(error_result("Half-life engine requires SILAC data (format file must have is_silac = TRUE)"))
  }

  mat <- payload$mat
  ids <- payload$ids
  samples <- payload$samples

  # Validate channel column exists in samples
  if (!"channel" %in% names(samples)) {
    return(error_result("Samples table missing 'channel' column. Re-format the file with SILAC mode enabled."))
  }

  # Extract params
  time_value <- as.numeric(params$time_value %||% 24)
  time_unit  <- as.character(params$time_unit %||% "hour")
  normalize_ratios <- if (is.null(params$normalize_ratios)) TRUE else isTRUE(params$normalize_ratios)
  ratio_min  <- as.numeric(params$ratio_min %||% 0.02)
  ratio_max  <- as.numeric(params$ratio_max %||% 100)

  add_log("INFO", sprintf("Input: %d features x %d columns", nrow(mat), ncol(mat)))
  add_log("INFO", sprintf("Pulse time: %g %s", time_value, time_unit))
  add_log("INFO", sprintf("Median normalization: %s", ifelse(normalize_ratios, "ON", "OFF")))
  add_log("INFO", sprintf("Ratio filter: [%.4g, %.4g]", ratio_min, ratio_max))

  # Identify H/L pairs by (group_name, replicate)
  groups <- unique(samples$group_name)
  groups <- groups[!is.na(groups) & nzchar(groups)]

  ratio_cols   <- list()   # named list of ratio vectors
  pair_labels  <- character()
  pair_groups  <- character()
  pair_reps    <- integer()
  skipped      <- 0L

  for (g in groups) {
    g_samples <- samples[samples$group_name == g, , drop = FALSE]
    reps <- sort(unique(g_samples$replicate))

    for (r in reps) {
      rep_samples <- g_samples[g_samples$replicate == r, , drop = FALSE]
      h_rows <- rep_samples[tolower(trimws(rep_samples$channel)) == "heavy", , drop = FALSE]
      l_rows <- rep_samples[tolower(trimws(rep_samples$channel)) == "light", , drop = FALSE]

      if (nrow(h_rows) != 1 || nrow(l_rows) != 1) {
        add_log("WARN", sprintf(
          "Group '%s' Rep %d: expected 1 Heavy and 1 Light column, got %d H and %d L. Skipping.",
          g, r, nrow(h_rows), nrow(l_rows)
        ))
        skipped <- skipped + 1L
        next
      }

      h_col <- h_rows$sample_col[1]
      l_col <- l_rows$sample_col[1]

      heavy <- mat[, h_col]
      light <- mat[, l_col]

      # Compute H/L ratio
      ratio <- heavy / light

      # Handle edge cases: non-positive, infinite, NA
      bad_mask <- is.na(ratio) | is.infinite(ratio) | ratio <= 0
      n_bad <- sum(bad_mask)
      if (n_bad > 0) {
        add_log("INFO", sprintf("  %s_Rep%d: %d/%d values set to NA (zero/negative/infinite H/L ratio)",
                                g, r, n_bad, length(ratio)))
        ratio[bad_mask] <- NA_real_
      }

      # Apply user ratio range filter
      out_of_range <- which(!is.na(ratio) & (ratio < ratio_min | ratio > ratio_max))
      if (length(out_of_range) > 0) {
        add_log("INFO", sprintf("  %s_Rep%d: %d/%d ratios filtered (outside [%.4g, %.4g])",
                                g, r, length(out_of_range), length(ratio), ratio_min, ratio_max))
        ratio[out_of_range] <- NA_real_
      }

      label <- sprintf("%s_Rep%d", g, r)
      ratio_cols[[label]] <- ratio
      pair_labels <- c(pair_labels, label)
      pair_groups <- c(pair_groups, g)
      pair_reps   <- c(pair_reps, as.integer(r))
    }
  }

  n_pairs <- length(ratio_cols)
  if (n_pairs == 0) {
    return(error_result(sprintf(
      "No valid Heavy/Light pairs found (%d pair(s) skipped due to missing channels)", skipped
    )))
  }

  add_log("INFO", sprintf("Found %d valid H/L pair(s), %d skipped", n_pairs, skipped))

  # Build ratio matrix (proteins x replicates)
  ratio_mat <- do.call(cbind, ratio_cols)
  colnames(ratio_mat) <- pair_labels
  rownames(ratio_mat) <- rownames(mat)

  # Optional median normalization (per replicate column)
  if (normalize_ratios && n_pairs > 0) {
    for (j in seq_len(ncol(ratio_mat))) {
      med <- median(ratio_mat[, j], na.rm = TRUE)
      if (!is.na(med) && med > 0) {
        ratio_mat[, j] <- ratio_mat[, j] / med
        add_log("INFO", sprintf("  Normalized %s: median H/L = %.4f", pair_labels[j], med))
      } else {
        add_log("WARN", sprintf("  Could not normalize %s: median H/L = %s",
                                pair_labels[j], ifelse(is.na(med), "NA", as.character(med))))
      }
    }
  }

  # Compute half-lives: t_half = t * ln(2) / ln(1 + H/L)
  halflife_mat <- time_value * log(2) / log(1 + ratio_mat)

  # Clamp non-positive or infinite half-lives to NA
  bad_hl <- is.na(halflife_mat) | is.infinite(halflife_mat) | halflife_mat <= 0
  n_bad_hl <- sum(bad_hl)
  if (n_bad_hl > 0) {
    halflife_mat[bad_hl] <- NA_real_
    add_log("INFO", sprintf("Clamped %d non-positive/infinite half-life values to NA", n_bad_hl))
  }

  colnames(halflife_mat) <- pair_labels
  rownames(halflife_mat) <- rownames(mat)

  # Summary statistics
  n_valid <- sum(!is.na(halflife_mat))
  n_total <- length(halflife_mat)
  add_log("INFO", sprintf("Output: %d features x %d replicates", nrow(halflife_mat), ncol(halflife_mat)))
  add_log("INFO", sprintf("Valid half-life values: %d / %d (%.1f%%)",
                           n_valid, n_total, 100 * n_valid / max(n_total, 1)))

  if (n_valid > 0) {
    hl_vals <- halflife_mat[!is.na(halflife_mat)]
    add_log("INFO", sprintf("Half-life range: %.2f - %.2f %s (median: %.2f)",
                             min(hl_vals), max(hl_vals), time_unit, median(hl_vals)))
  }

  # Build updated samples table (collapsed from 2N to N columns)
  new_samples <- data.frame(
    sample_col = pair_labels,
    group_name = pair_groups,
    replicate  = pair_reps,
    stringsAsFactors = FALSE
  )

  new_groups <- unique(new_samples$group_name)

  # Build summary log table for results viewer
  half_life_log <- data.frame(
    Metric = c("Pulse Time", "Time Unit", "Median Normalized",
               "Ratio Filter Min", "Ratio Filter Max",
               "Input Columns", "Output Replicates", "Features",
               "Valid Values", "Percent Valid"),
    Value = c(
      as.character(time_value), time_unit,
      ifelse(normalize_ratios, "Yes", "No"),
      as.character(ratio_min), as.character(ratio_max),
      as.character(ncol(mat)), as.character(n_pairs),
      as.character(nrow(halflife_mat)),
      as.character(n_valid),
      sprintf("%.1f%%", 100 * n_valid / max(n_total, 1))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  elapsed <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Half-life computation completed in %.2f seconds", elapsed))

  # Contract-compliant results
  list(
    engine_id = "half_life",
    params = params,
    data = list(
      # Processed data for downstream engines (context update)
      mat = halflife_mat,
      ids = ids,
      samples = new_samples,
      groups = new_groups,
      # Results display
      half_life_log = half_life_log,
      log = make_log_df(),
      summary = data.frame(
        metric = c("n_features", "n_replicates", "n_valid", "elapsed_sec"),
        value = c(nrow(halflife_mat), n_pairs, n_valid, round(elapsed, 2)),
        stringsAsFactors = FALSE
      ),
      # QC data for rendering
      qc = list(
        ratio_data = ratio_mat,
        halflife_data = halflife_mat,
        pair_labels = pair_labels,
        pair_groups = pair_groups,
        time_unit = time_unit,
        ratio_min = ratio_min,
        ratio_max = ratio_max
      ),
      # Preserve metadata
      metadata = payload$metadata
    )
  )
}
