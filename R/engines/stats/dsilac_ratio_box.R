# =========================================================
# R/engines/stats/dsilac_ratio_box.R
# dSILAC QC — vertical distribution of Heavy/Light ratios.
# Used by dsilac_ratio_box_peptide and dsilac_ratio_box_protein.
# Produces long-form values table compatible with tb_render_vert_dis.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

stats_dsilac_ratio_box_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()
  params <- params %||% payload$params %||% list()
  engine_id <- payload$engine_id %||% "dsilac_ratio_box"

  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries[[length(log_entries) + 1]] <<- list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level, message = msg
    )
  }
  make_log_df <- function() {
    if (length(log_entries) == 0) {
      return(data.frame(time = character(), level = character(), message = character(),
                        stringsAsFactors = FALSE))
    }
    do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
  }

  empty_values <- function() {
    data.frame(value = numeric(0), sample = character(0), group = character(0),
               replicate = integer(0), stringsAsFactors = FALSE)
  }

  error_result <- function(msg) {
    add_log("ERROR", msg)
    list(engine_id = engine_id, params = params,
         data = list(values = empty_values(), summary = data.frame(),
                     log = make_log_df()))
  }

  if (!isTRUE(payload$ok)) return(error_result(payload$error %||% "Invalid payload"))
  if (!isTRUE(payload$is_silac)) {
    return(error_result("Ratio box QC requires SILAC data (is_silac = TRUE)."))
  }

  mat <- payload$mat
  samples <- payload$samples
  if (!"channel" %in% names(samples)) {
    return(error_result("Samples table missing 'channel' column. Re-format with SILAC mode."))
  }

  samples$channel_norm <- tolower(trimws(samples$channel))
  compare_mode <- params$compare_mode %||% "all_reps"
  groups <- unique(samples$group_name)
  groups <- groups[!is.na(groups) & nzchar(groups)]

  # Pair H/L columns per (group, replicate)
  values_list <- list()

  for (g in groups) {
    g_samples <- samples[samples$group_name == g, , drop = FALSE]
    reps <- sort(unique(suppressWarnings(as.integer(g_samples$replicate))))
    reps <- reps[!is.na(reps)]

    rep_ratios <- list()

    for (r in reps) {
      rep_rows <- g_samples[suppressWarnings(as.integer(g_samples$replicate)) == r, , drop = FALSE]
      h_rows <- rep_rows[rep_rows$channel_norm %in% c("heavy", "h"), , drop = FALSE]
      l_rows <- rep_rows[rep_rows$channel_norm %in% c("light", "l"), , drop = FALSE]
      if (nrow(h_rows) != 1 || nrow(l_rows) != 1) {
        add_log("WARN", sprintf("Group '%s' Rep %d: expected 1 H + 1 L column, got %dH/%dL. Skipping.",
                                g, r, nrow(h_rows), nrow(l_rows)))
        next
      }
      h_col <- h_rows$sample_col[1]
      l_col <- l_rows$sample_col[1]
      if (!h_col %in% colnames(mat) || !l_col %in% colnames(mat)) next
      heavy <- mat[, h_col]
      light <- mat[, l_col]
      ratio <- heavy / light
      ratio[!is.finite(ratio)] <- NA
      ratio <- ratio[is.finite(ratio)]
      if (length(ratio) == 0) next

      sample_label <- sprintf("%s_R%d", g, r)
      rep_ratios[[sample_label]] <- ratio

      if (compare_mode == "all_reps") {
        values_list[[length(values_list) + 1]] <- data.frame(
          value = ratio, sample = sample_label,
          group = g, replicate = as.integer(r),
          stringsAsFactors = FALSE
        )
      }
    }

    if (compare_mode == "avg_groups" && length(rep_ratios) > 0) {
      # Concatenate all replicate ratios under one group box
      all_r <- unlist(rep_ratios, use.names = FALSE)
      values_list[[length(values_list) + 1]] <- data.frame(
        value = all_r, sample = sprintf("%s_avg", g),
        group = g, replicate = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }

  values_df <- if (length(values_list) > 0) do.call(rbind, values_list) else empty_values()

  summary_df <- if (nrow(values_df) > 0) {
    do.call(rbind, lapply(split(values_df, values_df$sample), function(d) {
      data.frame(
        sample = d$sample[1],
        group = d$group[1],
        n = nrow(d),
        mean = mean(d$value, na.rm = TRUE),
        median = median(d$value, na.rm = TRUE),
        sd = sd(d$value, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(sample = character(0), group = character(0), n = integer(0),
               mean = numeric(0), median = numeric(0), sd = numeric(0),
               stringsAsFactors = FALSE)
  }

  add_log("INFO", sprintf("dsilac_ratio_box finished in %.2fs (%d values)",
                          as.numeric(difftime(Sys.time(), engine_start, units = "secs")),
                          nrow(values_df)))

  list(
    engine_id = engine_id,
    params = params,
    data = list(
      values = values_df,
      summary = summary_df,
      applied_transform = "none",
      log = make_log_df()
    )
  )
}

stats_dsilac_ratio_box_peptide_run <- function(payload, params = NULL, context = NULL) {
  payload$engine_id <- "dsilac_ratio_box_peptide"
  stats_dsilac_ratio_box_run(payload, params, context)
}

stats_dsilac_ratio_box_protein_run <- function(payload, params = NULL, context = NULL) {
  payload$engine_id <- "dsilac_ratio_box_protein"
  stats_dsilac_ratio_box_run(payload, params, context)
}
