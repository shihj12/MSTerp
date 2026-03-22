# =========================================================
# R/engines/stats/multi_correlation.R - Multi-Dataset Correlation Engine
#
# Computes and visualizes correlation between datasets with regression line.
# Provides both Pearson and Spearman correlation with confidence intervals.
#
# Contract v1.1:
#  - data$correlation_data: data.frame with primary_id, display_id, x, y, residual, is_outlier
#  - data$statistics: summary data.frame with metric names and values
#  - data$regression: list with slope, intercept, r_squared
#  - data$pearson: list with r, p, ci_lower, ci_upper
#  - data$spearman: list with rho, p
#  - data$x_label, data$y_label: axis labels
#  - data$metadata: payload metadata
#  - data$log: execution log data.frame
# =========================================================

# Note: %||% operator is defined in R/utils/uniprot.R and sourced before this file

#' Execute multi_correlation engine
#'
#' @param payload Multi-dataset payload from nr_build_multi_payload
#' @param params Engine parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_multi_correlation_run <- function(payload, params = NULL, context = NULL) {
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
    log_df <- .multi_correlation_build_log_df(log_entries)
    if (!is.null(error_msg)) {
      add_log("ERROR", error_msg)
      log_df <- .multi_correlation_build_log_df(log_entries)
    }
    list(
      engine_id = "multi_correlation",
      params = params,
      data = list(
        correlation_data = data.frame(
          primary_id = character(0), display_id = character(0),
          protein_id = character(0), gene_symbol = character(0),
          x = numeric(0), y = numeric(0), residual = numeric(0),
          std_residual = numeric(0), is_outlier = logical(0),
          stringsAsFactors = FALSE
        ),
        statistics = data.frame(metric = character(0), value = character(0), stringsAsFactors = FALSE),
        regression = list(slope = NA, intercept = NA, r_squared = NA),
        pearson = list(r = NA, p = NA, ci_lower = NA, ci_upper = NA),
        spearman = list(rho = NA, p = NA),
        x_label = "X-axis",
        y_label = "Y-axis",
        metadata = list(),
        log = log_df
      )
    )
  }

  add_log("INFO", "Starting multi-correlation analysis")

  # Validate payload mode
  if (is.null(payload$mode) || payload$mode != "multi_dataset") {
    return(empty_result("multi_correlation requires multi_dataset payload"))
  }

  scores_x <- payload$scores_x
  scores_y <- payload$scores_y

  if (is.null(scores_x) || is.null(scores_y)) {
    return(empty_result("Missing scores_x or scores_y in payload"))
  }

  # Align and filter
  common_ids <- intersect(names(scores_x), names(scores_y))
  x_vals <- scores_x[common_ids]
  y_vals <- scores_y[common_ids]

  valid_mask <- !is.na(x_vals) & !is.na(y_vals)
  valid_ids <- common_ids[valid_mask]
  x_vals <- x_vals[valid_mask]
  y_vals <- y_vals[valid_mask]

  n <- length(valid_ids)
  if (n < 3) {
    return(empty_result("Insufficient data points for correlation (need at least 3)"))
  }

  add_log("INFO", sprintf("Computing correlation for %d features", n))

  # Correlation method from params
  method <- params$method %||% "pearson"

  # Pearson correlation with CI
  pearson_test <- tryCatch({
    cor.test(x_vals, y_vals, method = "pearson", conf.level = 0.95)
  }, error = function(e) NULL)

  # Spearman correlation with p-value
  spearman_test <- tryCatch({
    cor.test(x_vals, y_vals, method = "spearman", exact = FALSE)
  }, error = function(e) NULL)

  # Linear regression for regression line
  lm_fit <- lm(y_vals ~ x_vals)
  lm_summary <- summary(lm_fit)

  # Get display names
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

  # Build data frame
  plot_df <- data.frame(
    primary_id = valid_ids,
    display_id = display_names,
    protein_id = valid_ids,        # backward compat
    gene_symbol = display_names,   # backward compat
    x = as.numeric(x_vals),
    y = as.numeric(y_vals),
    stringsAsFactors = FALSE
  )

  # Add residuals for outlier detection
  plot_df$residual <- residuals(lm_fit)
  plot_df$std_residual <- rstandard(lm_fit)
  plot_df$is_outlier <- abs(plot_df$std_residual) > 2

  # Statistics summary
  stats_summary <- data.frame(
    metric = c("Pearson r", "Pearson p-value", "Pearson 95% CI",
               "Spearman rho", "Spearman p-value",
               "R-squared", "Regression slope", "Regression intercept", "N"),
    value = c(
      sprintf("%.4f", pearson_test$estimate %||% NA),
      sprintf("%.2e", pearson_test$p.value %||% NA),
      if (!is.null(pearson_test$conf.int)) sprintf("[%.3f, %.3f]", pearson_test$conf.int[1], pearson_test$conf.int[2]) else "NA",
      sprintf("%.4f", spearman_test$estimate %||% NA),
      sprintf("%.2e", spearman_test$p.value %||% NA),
      sprintf("%.4f", lm_summary$r.squared),
      sprintf("%.4f", coef(lm_fit)[2]),
      sprintf("%.4f", coef(lm_fit)[1]),
      as.character(n)
    ),
    stringsAsFactors = FALSE
  )

  add_log("INFO", sprintf("Pearson r = %.3f (p = %.2e), Spearman rho = %.3f",
                          pearson_test$estimate %||% NA,
                          pearson_test$p.value %||% NA,
                          spearman_test$estimate %||% NA))

  # Log completion
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Multi-correlation analysis completed in %.2f seconds", engine_duration))

  # Build log data.frame
  log_df <- .multi_correlation_build_log_df(log_entries)

  # Return contract-compliant result
  list(
    engine_id = "multi_correlation",
    params = params,
    data = list(
      correlation_data = plot_df,
      statistics = stats_summary,
      regression = list(
        slope = coef(lm_fit)[2],
        intercept = coef(lm_fit)[1],
        r_squared = lm_summary$r.squared
      ),
      pearson = list(
        r = pearson_test$estimate %||% NA,
        p = pearson_test$p.value %||% NA,
        ci_lower = if (!is.null(pearson_test$conf.int)) pearson_test$conf.int[1] else NA,
        ci_upper = if (!is.null(pearson_test$conf.int)) pearson_test$conf.int[2] else NA
      ),
      spearman = list(
        rho = spearman_test$estimate %||% NA,
        p = spearman_test$p.value %||% NA
      ),
      x_label = payload$x_label %||% "X-axis",
      y_label = payload$y_label %||% "Y-axis",
      metadata = payload$metadata,
      log = log_df
    )
  )
}

# Legacy alias for backward compatibility
run_multi_correlation <- function(payload, params, context) {
  stats_multi_correlation_run(payload, params, context)
}


# =========================================================
# Internal Helper Functions
# =========================================================

#' Build log data frame from log entries
#' @noRd
.multi_correlation_build_log_df <- function(log_entries) {
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


#' Get multi_correlation engine info
multi_correlation_engine_info <- function() {
  list(
    engine_id = "multi_correlation",
    label = "Multi-Dataset Correlation",
    description = "Correlation analysis between two linked datasets with regression"
  )
}
