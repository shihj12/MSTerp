# =========================================================
# R/engines/stats/vert_dis.R — Vertical Distribution Engine
#
# Produces long-form values table for box/violin plots.
#
# Contract v1.1: data$values with value, sample, group, replicate
# =========================================================

#' Execute vert_dis engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
#'   - data$values: data.frame with value, sample, group, replicate
stats_vert_dis_run <- function(payload, params = NULL, context = NULL) {
  # Track engine start time
  engine_start <- Sys.time()

  params <- params %||% payload$params %||% list()

  # Initialize log entries
  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries <<- c(log_entries, list(list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level,
      message = msg
    )))
  }

  if (!isTRUE(payload$ok)) {
    return(list(
      engine_id = "vert_dis",
      params = params,
      data = list(
        values = data.frame(
          value = numeric(0),
          sample = character(0),
          group = character(0),
          replicate = integer(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  mat <- payload$mat
  samples <- payload$samples
  groups <- payload$groups

  add_log("INFO", sprintf("Initiating vert_dis: %d proteins x %d groups",
                          nrow(mat), length(groups)))

  # Get parameters
  compare_mode <- params$compare_mode %||% "avg_groups"
  # log_transform moved to style_schema — applied at render time for viewer-time control
  # For backward compatibility, still apply if present in params AND no raw flag
  log_transform <- params$log_transform %||% "none"

  # Legacy: apply log_transform at compute time only if explicitly in params

  # (new runs use style_schema so log_transform won't be in params)
  applied_transform <- "none"
  if (!is.null(params$log_transform)) {
    if (log_transform == "log10") {
      mat[mat <= 0] <- NA
      mat <- log10(mat)
      add_log("INFO", "Applied log10 transformation (legacy params)")
      applied_transform <- "log10"
    } else if (log_transform == "log2") {
      mat[mat <= 0] <- NA
      mat <- log2(mat)
      add_log("INFO", "Applied log2 transformation (legacy params)")
      applied_transform <- "log2"
    }
  } else {
    add_log("INFO", "Storing raw values (log_transform at render time)")
  }

  add_log("INFO", sprintf("Compare mode: %s", compare_mode))

  # Build long-form values table
  values_list <- list()

  if (compare_mode == "avg_groups") {
    # Average within groups
    for (grp in groups) {
      grp_samples <- samples[samples$group_name == grp, , drop = FALSE]
      grp_cols <- intersect(grp_samples$sample_col, colnames(mat))

      if (length(grp_cols) > 0) {
        grp_mean <- rowMeans(mat[, grp_cols, drop = FALSE], na.rm = TRUE)
        valid <- !is.na(grp_mean) & is.finite(grp_mean)

        values_list <- c(values_list, list(
          data.frame(
            value = grp_mean[valid],
            sample = sprintf("%s_avg", grp),
            group = grp,
            replicate = NA_integer_,
            stringsAsFactors = FALSE
          )
        ))
      }
    }
  } else {
    # Individual samples
    for (grp in groups) {
      grp_samples <- samples[samples$group_name == grp, , drop = FALSE]

      for (i in seq_len(nrow(grp_samples))) {
        s <- grp_samples[i, ]
        col <- s$sample_col
        if (!col %in% colnames(mat)) next

        vals <- mat[, col]
        valid <- !is.na(vals) & is.finite(vals)

        rep_num <- s$replicate
        if (is.null(rep_num) || is.na(rep_num)) rep_num <- i

        values_list <- c(values_list, list(
          data.frame(
            value = vals[valid],
            sample = col,
            group = grp,
            replicate = as.integer(rep_num),
            stringsAsFactors = FALSE
          )
        ))
      }
    }
  }

  # Combine results
  values_df <- if (length(values_list) > 0) {
    do.call(rbind, values_list)
  } else {
    data.frame(
      value = numeric(0),
      sample = character(0),
      group = character(0),
      replicate = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # Compute summary stats
  summary_list <- list()
  for (grp in groups) {
    grp_vals <- values_df$value[values_df$group == grp]
    if (length(grp_vals) > 0) {
      summary_list <- c(summary_list, list(
        data.frame(
          group = grp,
          n = length(grp_vals),
          mean = mean(grp_vals, na.rm = TRUE),
          median = median(grp_vals, na.rm = TRUE),
          q25 = quantile(grp_vals, 0.25, na.rm = TRUE),
          q75 = quantile(grp_vals, 0.75, na.rm = TRUE),
          sd = sd(grp_vals, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      ))
    }
  }

  summary_df <- if (length(summary_list) > 0) {
    do.call(rbind, summary_list)
  } else {
    data.frame(group = character(0), n = integer(0), mean = numeric(0),
               median = numeric(0), q25 = numeric(0), q75 = numeric(0),
               sd = numeric(0), stringsAsFactors = FALSE)
  }

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Vert_dis completed in %.2f seconds (%d values)",
                          engine_duration, nrow(values_df)))

  # Build log data.frame from accumulated entries
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  # Extract group colors from metadata (same pattern as heatmap.R)
  meta_groups <- payload$metadata$groups
  if (!is.null(meta_groups) && is.data.frame(meta_groups) &&
      "group_name" %in% names(meta_groups) && "color" %in% names(meta_groups)) {
    color_map <- stats::setNames(
      as.character(meta_groups$color),
      as.character(meta_groups$group_name)
    )
    group_colors <- color_map[groups]
    missing <- is.na(group_colors) | !nzchar(group_colors)
    if (any(missing)) {
      auto_colors <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
      group_colors[missing] <- auto_colors
    }
    names(group_colors) <- groups
  } else {
    group_colors <- grDevices::hcl.colors(max(1, length(groups)), palette = "Dark 3")
    group_colors <- group_colors[seq_along(groups)]
    names(group_colors) <- groups
  }

  list(
    engine_id = "vert_dis",
    params = params,
    data = list(
      values = values_df,
      summary = summary_df,
      group_colors = group_colors,
      applied_transform = applied_transform,
      log = log_df
    )
  )
}
