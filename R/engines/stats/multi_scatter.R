# =========================================================
# R/engines/stats/multi_scatter.R - Multi-Dataset Scatter Plot Engine
#
# Plots cross-dataset scatter using pre-computed axis values (scores_x, scores_y)
# from the multi-dataset payload. Supports highlighting, quadrant coloring,
# and protein labeling.
#
# Contract v1.1:
#  - data$scatter_data: data.frame with primary_id, display_id, x, y, quadrant
#  - data$correlation: list with pearson, spearman, n
#  - data$x_label, data$y_label: axis labels
#  - data$metadata: payload metadata
#  - data$log: execution log data.frame
# =========================================================

# Note: %||% operator is defined in R/utils/uniprot.R and sourced before this file

#' Execute multi_scatter engine
#'
#' @param payload Multi-dataset payload from nr_build_multi_payload
#' @param params Engine parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_multi_scatter_run <- function(payload, params = NULL, context = NULL) {
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

  # Build empty result helper
  empty_result <- function(error_msg = NULL) {
    log_df <- .multi_scatter_build_log_df(log_entries)
    if (!is.null(error_msg)) {
      add_log("ERROR", error_msg)
      log_df <- .multi_scatter_build_log_df(log_entries)
    }
    list(
      engine_id = "multi_scatter",
      params = params,
      data = list(
        scatter_data = data.frame(
          primary_id = character(0), display_id = character(0),
          protein_id = character(0), gene_symbol = character(0),
          x = numeric(0), y = numeric(0), quadrant = character(0),
          stringsAsFactors = FALSE
        ),
        correlation = list(pearson = NA, spearman = NA, n = 0),
        x_label = "X-axis",
        y_label = "Y-axis",
        metadata = list(),
        log = log_df
      )
    )
  }

  add_log("INFO", "Starting multi-scatter analysis")

  # Validate payload mode
  if (is.null(payload$mode) || payload$mode != "multi_dataset") {
    return(empty_result("multi_scatter requires multi_dataset payload"))
  }

  scores_x <- payload$scores_x
  scores_y <- payload$scores_y

  if (is.null(scores_x) || is.null(scores_y)) {
    return(empty_result("Missing scores_x or scores_y in payload - ensure axis configuration is set"))
  }

  # Align scores to common proteins
  common_ids <- intersect(names(scores_x), names(scores_y))
  if (length(common_ids) == 0) {
    return(empty_result("No common proteins between x and y scores"))
  }

  x_vals <- scores_x[common_ids]
  y_vals <- scores_y[common_ids]

  # Remove NAs
  valid_mask <- !is.na(x_vals) & !is.na(y_vals)
  valid_ids <- common_ids[valid_mask]
  x_vals <- x_vals[valid_mask]
  y_vals <- y_vals[valid_mask]

  if (length(valid_ids) == 0) {
    return(empty_result("No valid data points after removing NAs"))
  }

  add_log("INFO", sprintf("Plotting %d features (from %d common)", length(valid_ids), length(common_ids)))

  # Get display names if available
  ids_df <- payload$ids_a %||% payload$ids_b
  display_names <- if (!is.null(ids_df) && "gene_symbol" %in% names(ids_df)) {
    symbol_map <- stats::setNames(ids_df$gene_symbol, ids_df$protein_id)
    symbol_map[valid_ids]
  } else if (!is.null(ids_df) && "display_id" %in% names(ids_df)) {
    symbol_map <- stats::setNames(ids_df$display_id, ids_df$primary_id)
    symbol_map[valid_ids]
  } else {
    valid_ids
  }

  # Build data frame for plotting
  plot_df <- data.frame(
    primary_id = valid_ids,
    display_id = display_names,
    protein_id = valid_ids,        # backward compat
    gene_symbol = display_names,   # backward compat
    x = as.numeric(x_vals),
    y = as.numeric(y_vals),
    stringsAsFactors = FALSE
  )

  # Quadrant assignment (based on 0/0 or median)
  center_mode <- params$center_mode %||% "zero"

  x_mid <- if (center_mode == "median") median(plot_df$x, na.rm = TRUE) else 0
  y_mid <- if (center_mode == "median") median(plot_df$y, na.rm = TRUE) else 0

  plot_df$quadrant <- ifelse(
    plot_df$x >= x_mid & plot_df$y >= y_mid, "Q1 (+/+)",
    ifelse(
      plot_df$x < x_mid & plot_df$y >= y_mid, "Q2 (-/+)",
      ifelse(
        plot_df$x < x_mid & plot_df$y < y_mid, "Q3 (-/-)",
        "Q4 (+/-)"
      )
    )
  )

  # Calculate correlation
  cor_pearson <- cor(plot_df$x, plot_df$y, use = "complete.obs", method = "pearson")
  cor_spearman <- cor(plot_df$x, plot_df$y, use = "complete.obs", method = "spearman")

  add_log("INFO", sprintf("Correlation: Pearson=%.3f, Spearman=%.3f", cor_pearson, cor_spearman))

  # Log completion
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Multi-scatter analysis completed in %.2f seconds", engine_duration))

  # Build log data.frame
  log_df <- .multi_scatter_build_log_df(log_entries)

  # Return contract-compliant result
  list(
    engine_id = "multi_scatter",
    params = params,
    data = list(
      scatter_data = plot_df,
      correlation = list(
        pearson = cor_pearson,
        spearman = cor_spearman,
        n = nrow(plot_df)
      ),
      x_label = payload$x_label %||% "X-axis",
      y_label = payload$y_label %||% "Y-axis",
      metadata = payload$metadata,
      log = log_df
    )
  )
}

# Legacy alias for backward compatibility
run_multi_scatter <- function(payload, params, context) {
  stats_multi_scatter_run(payload, params, context)
}


# =========================================================
# Internal Helper Functions
# =========================================================

#' Build log data frame from log entries
#' @noRd
.multi_scatter_build_log_df <- function(log_entries) {
  if (length(log_entries) == 0) {
    return(data.frame(
      time = character(0),
      level = character(0),
      message = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))
}


#' Get multi_scatter engine info
multi_scatter_engine_info <- function() {
  list(
    engine_id = "multi_scatter",
    label = "Multi-Dataset Scatter",
    description = "Cross-dataset scatter plot comparing values from two linked datasets"
  )
}
