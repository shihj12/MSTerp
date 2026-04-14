# =========================================================
# R/engines/stats/dsilac_isotope_density.R
# dSILAC QC — light vs heavy isotope intensity distributions
# Used by dsilac_isotope_density_peptide and dsilac_isotope_density_protein.
# Produces long-form values table with channel info so the renderer can
# delegate to tb_render_hor_dis with per-group/channel faceting.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

stats_dsilac_isotope_density_run <- function(payload, params = NULL, context = NULL) {
  engine_start <- Sys.time()
  params <- params %||% payload$params %||% list()
  engine_id <- payload$engine_id %||% "dsilac_isotope_density"

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
    data.frame(
      value = numeric(0), sample = character(0), group = character(0),
      replicate = integer(0), channel = character(0),
      stringsAsFactors = FALSE
    )
  }

  error_result <- function(msg) {
    add_log("ERROR", msg)
    list(
      engine_id = engine_id, params = params,
      data = list(values = empty_values(), summary = data.frame(),
                  log = make_log_df())
    )
  }

  if (!isTRUE(payload$ok)) return(error_result(payload$error %||% "Invalid payload"))
  if (!isTRUE(payload$is_silac)) {
    return(error_result("Isotope density QC requires SILAC data (is_silac = TRUE)."))
  }

  mat <- payload$mat
  samples <- payload$samples
  if (!"channel" %in% names(samples)) {
    return(error_result("Samples table missing 'channel' column. Re-format with SILAC mode."))
  }

  compare_mode <- params$compare_mode %||% "all_reps"
  groups <- unique(samples$group_name)
  groups <- groups[!is.na(groups) & nzchar(groups)]

  channel_norm <- tolower(trimws(samples$channel))
  samples$channel_norm <- ifelse(channel_norm %in% c("heavy", "h"), "heavy",
                         ifelse(channel_norm %in% c("light", "l"), "light", NA_character_))

  values_list <- list()

  if (compare_mode == "avg_groups") {
    for (g in groups) {
      for (ch in c("light", "heavy")) {
        sub <- samples[samples$group_name == g & !is.na(samples$channel_norm) &
                         samples$channel_norm == ch, , drop = FALSE]
        cols <- intersect(sub$sample_col, colnames(mat))
        if (length(cols) == 0) next
        avg <- rowMeans(mat[, cols, drop = FALSE], na.rm = TRUE)
        avg <- avg[is.finite(avg)]
        if (length(avg) == 0) next
        values_list[[length(values_list) + 1]] <- data.frame(
          value = avg,
          sample = sprintf("%s_%s_avg", g, ch),
          group = g,
          replicate = NA_integer_,
          channel = ch,
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    for (i in seq_len(nrow(samples))) {
      s <- samples[i, ]
      ch <- s$channel_norm
      if (is.na(ch)) next
      col <- s$sample_col
      if (!col %in% colnames(mat)) next
      vals <- mat[, col]
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0) next
      rep_num <- suppressWarnings(as.integer(s$replicate))
      if (is.na(rep_num)) rep_num <- i
      values_list[[length(values_list) + 1]] <- data.frame(
        value = vals,
        sample = col,
        group = as.character(s$group_name),
        replicate = rep_num,
        channel = ch,
        stringsAsFactors = FALSE
      )
    }
  }

  values_df <- if (length(values_list) > 0) do.call(rbind, values_list) else empty_values()

  add_log("INFO", sprintf("Built %d values rows across %d groups (%s)",
                          nrow(values_df), length(groups), compare_mode))

  # Summary stats per group × channel
  summary_df <- if (nrow(values_df) > 0) {
    split_key <- paste(values_df$group, values_df$channel, sep = "||")
    do.call(rbind, lapply(split(values_df, split_key), function(d) {
      data.frame(
        group = d$group[1],
        channel = d$channel[1],
        n = nrow(d),
        mean = mean(d$value, na.rm = TRUE),
        median = median(d$value, na.rm = TRUE),
        sd = sd(d$value, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(group = character(0), channel = character(0), n = integer(0),
               mean = numeric(0), median = numeric(0), sd = numeric(0),
               stringsAsFactors = FALSE)
  }

  add_log("INFO", sprintf("dsilac_isotope_density finished in %.2fs",
                          as.numeric(difftime(Sys.time(), engine_start, units = "secs"))))

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

stats_dsilac_isotope_density_peptide_run <- function(payload, params = NULL, context = NULL) {
  payload$engine_id <- "dsilac_isotope_density_peptide"
  stats_dsilac_isotope_density_run(payload, params, context)
}

stats_dsilac_isotope_density_protein_run <- function(payload, params = NULL, context = NULL) {
  payload$engine_id <- "dsilac_isotope_density_protein"
  stats_dsilac_isotope_density_run(payload, params, context)
}
