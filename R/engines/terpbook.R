# =========================================================
# R/terpbook.R Ã¢â‚¬â€ Graphing engine (render from data + style)
# - Always produces ggplot for export
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Safe numeric extraction from style with fallback
tb_num <- function(x, default = 0) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) < 1) return(default)
  v <- v[[1]]
  if (is.finite(v)) v else default
}

tb_require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package required: ", pkg)
  }
}

#'' Format numbers with k suffix for display
#''
#'' Examples:
#'' - 1234 -> "1.2k"
#'' - 567  -> "567"
#''
#'' @param x Numeric vector (or coercible)
#'' @return Character vector
format_k_suffix <- function(x) {
  if (is.null(x)) return(character())
  x_num <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x_num))

  ok <- is.finite(x_num)
  small <- ok & abs(x_num) < 1000
  big <- ok & abs(x_num) >= 1000

  out[small] <- format(x_num[small], trim = TRUE, scientific = FALSE)
  out[big] <- sprintf("%.1fk", x_num[big] / 1000)
  out
}

tb_is_equal <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)

  if (is.numeric(a) && is.numeric(b)) {
    if (length(a) != length(b)) return(FALSE)
    return(all(abs(a - b) < 1e-9))
  }
  identical(a, b)
}

tb_fdr_palette <- function(n = 100) {
  grDevices::colorRampPalette(c("black", "#2727d6", "#881bff", "#ff7ab0", "yellow"))(n)
}

# Convenience wrapper: tb_fdr_color_scale(palette, type)
# Maps "continuous" -> "color" for tb_fdr_scale compatibility
tb_fdr_color_scale <- function(palette = "yellow_cap", type = "color") {
  scale_type <- if (type == "continuous") "color" else type
  tb_fdr_scale(type = scale_type, palette = palette)
}

# Helper to create FDR color scale that properly scales to input data and shows a legend
# Returns a ggplot2 scale_color_* or scale_fill_* layer
# type: "color" or "fill"
# fdr_values: vector of FDR values to scale to (uses data range if provided)
# palette: "yellow_cap" (default, low FDR = yellow) or "blue_red" (low FDR = #2e66a0, high = #d14f58)
tb_fdr_scale <- function(type = "color", fdr_values = NULL, flat_color = NULL, palette = "yellow_cap") {
  # Two palette options:
  # "yellow_cap" (default): black -> blue -> purple -> pink -> yellow (low FDR = significant = yellow)
  # "blue_red": #d14f58 (red, high FDR) -> #2e66a0 (blue, low FDR/significant)
  if (palette == "blue_red") {
    pal_colors <- c("#2e66a0", "#6a8ab8", "#b0b0b0", "#c88080", "#d14f58")  # low FDR (blue) to high FDR (red)
  } else {
    pal_colors <- c("yellow", "#ff7ab0", "#881bff", "#2727d6", "black")  # low FDR (significant) to high FDR
  }

  if (!is.null(flat_color)) {
    # Flat color mode - no legend
    if (type == "fill") {
      return(ggplot2::scale_fill_identity())
    } else {
      return(ggplot2::scale_color_identity())
    }
  }

  # Determine limits from data if provided
  # FIX: Ensure we have valid limits and handle edge cases to prevent grey colors
  limits <- NULL
  if (!is.null(fdr_values) && length(fdr_values) > 0) {
    # Replace NA, NaN, Inf with reasonable values, and ensure positive for log10
    fdr_clean <- fdr_values[is.finite(fdr_values) & fdr_values > 0]
    if (length(fdr_clean) > 0) {
      min_fdr <- min(fdr_clean)
      max_fdr <- max(fdr_clean)
      # Ensure minimum is not too small (avoid log10 issues)
      min_fdr <- max(min_fdr, 1e-300)
      # Ensure we have a valid range
      if (min_fdr >= max_fdr) {
        max_fdr <- min_fdr * 10
      }
      limits <- c(min_fdr, max_fdr)
    }
  }

  # Default limits if none computed
  if (is.null(limits)) {
    limits <- c(1e-10, 0.05)
  }

  # Use gradient scale with legend
  # FIX: Use na.value to prevent grey for NA values
  if (type == "fill") {
    ggplot2::scale_fill_gradientn(
      colors = pal_colors,
      name = "FDR",
      trans = "log10",
      limits = limits,
      oob = scales::squish,
      na.value = pal_colors[1],  # Use lowest FDR color (most significant) for NA
      labels = function(x) format(x, scientific = TRUE, digits = 2)
    )
  } else {
    ggplot2::scale_color_gradientn(
      colors = pal_colors,
      name = "FDR",
      trans = "log10",
      limits = limits,
      oob = scales::squish,
      na.value = pal_colors[1],  # Use lowest FDR color (most significant) for NA
      labels = function(x) format(x, scientific = TRUE, digits = 2)
    )
  }
}

# Format FDR values to 2 significant figures in scientific notation
# Returns formatted character string(s)
tb_format_fdr <- function(x) {
  x <- as.numeric(x)
  vapply(x, function(v) {
    if (!is.finite(v)) return("NA")
    if (v == 0) return("0")
    # Format to 2 significant figures in scientific notation (e.g., 1.2e-04)
    formatC(signif(v, 2), format = "e", digits = 1)
  }, character(1))
}

tb_format_sig <- function(x, digits = 2) {
  x <- as.numeric(x)
  vapply(x, function(v) {
    if (!is.finite(v)) return("NA")
    formatC(v, format = "fg", digits = as.integer(digits))
  }, character(1))
}

# Flip a comparison label: "log2(A/B)" -> "log2(B/A)", "A/B" -> "B/A"
# No-op for labels without "/" comparison (e.g., "PC1", "Score")
tb_flip_comparison_label <- function(label) {
  if (is.null(label) || !nzchar(label) || !grepl("/", label, fixed = TRUE)) return(label)
  sub("([^/()]+)/([^/()]+)", "\\2/\\1", label)
}

# -----------------------------------------------------------------------------
# Protein Complex Enrichment Support
# -----------------------------------------------------------------------------

#' Detect if enrichment results are for protein complex database
#'
#' Checks term_id column for CORUM_ or CPX_ prefixes indicating complex data.
#'
#' @param df data.frame with enrichment results (must have term_id column)
#' @return TRUE if complex mode, FALSE otherwise
tb_detect_complex_mode <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(FALSE)
  if (!"term_id" %in% names(df)) return(FALSE)
  if (nrow(df) == 0) return(FALSE)

  # Check if any term_id starts with complex prefixes (CORUM:, CORUM_, CPX:, CPX_)
  any(grepl("^(CORUM[_:]|CPX[_:])", df$term_id, perl = TRUE))
}

#' Get member proteins for a complex from ComplexBase
#'
#' Looks up member proteins for a given complex ID. Returns overlap proteins
#' (from query) and all complex members if ComplexBase is available.
#'
#' @param term_row Single row from enrichment results (must have term_id and protein_ids)
#' @param complexbase ComplexBase object (or NULL)
#' @return list with: all_members, overlap, n_all, n_overlap
tb_get_complex_member_proteins <- function(term_row, complexbase = NULL) {
  term_id <- as.character(term_row$term_id %||% "")
  is_complex <- grepl("^(CORUM_|CPX_)", term_id)

  # Get overlap proteins from result (protein_ids column)
  overlap_str <- as.character(term_row$protein_ids %||% term_row$genes %||% "")
  overlap_members <- if (nzchar(overlap_str)) {
    trimws(unlist(strsplit(overlap_str, "[,;]")))
  } else {
    character(0)
  }

  if (is_complex && !is.null(complexbase)) {
    # Try to get all complex members from ComplexBase
    all_members <- tryCatch({
      if (exists("complexbase_get_complex_proteins", mode = "function")) {
        complexbase_get_complex_proteins(complexbase, term_id)
      } else {
        # Source complexbase.R if function not available
        source_file <- file.path(dirname(sys.frame(1)$ofile %||% "."), "complexbase.R")
        if (file.exists(source_file)) {
          source(source_file, local = TRUE)
          complexbase_get_complex_proteins(complexbase, term_id)
        } else {
          character(0)
        }
      }
    }, error = function(e) character(0))

    return(list(
      all_members = all_members,
      overlap = overlap_members,
      n_all = length(all_members),
      n_overlap = length(overlap_members)
    ))
  } else {
    # GO term or no complexbase - just return overlap proteins
    return(list(
      all_members = character(0),
      overlap = overlap_members,
      n_all = 0L,
      n_overlap = length(overlap_members)
    ))
  }
}

#' Get valid complex sources for tab splitting
#'
#' Returns the complex sources present in the data (CORUM, ComplexPortal).
#'
#' @param df data.frame with enrichment results (must have ontology column)
#' @return character vector of sources present
tb_get_complex_sources <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(character(0))
  if (!"ontology" %in% names(df)) return(character(0))

  sources_present <- unique(as.character(df$ontology))
  # Filter to known complex sources (prefix match to handle variants like CORUM_Harmonizome)
  sources_present[grepl("^(CORUM|ComplexPortal)", sources_present)]
}

tb_theme_base <- function(axis_text_size = 20, axis_style = "clean") {
  axis_text_size <- suppressWarnings(as.numeric(axis_text_size %||% 20))
  if (!is.finite(axis_text_size) || axis_text_size <= 0) axis_text_size <- 20
  axis_style <- tolower(as.character(axis_style %||% "clean"))

  if (axis_style == "bold") {
    # Prism-style: bold axes, black border, no gridlines
    ggplot2::theme_classic(base_size = axis_text_size) +
      ggplot2::theme(
        text = ggplot2::element_text(color = "black"),
        axis.line = ggplot2::element_line(color = "black", linewidth = 1.5),
        axis.ticks = ggplot2::element_line(color = "black", linewidth = 1.5),
        axis.ticks.length = ggplot2::unit(0.25, "cm"),
        axis.text = ggplot2::element_text(color = "black", face = "bold"),
        axis.title = ggplot2::element_text(color = "black", face = "bold"),
        plot.title = ggplot2::element_text(color = "black", face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "black"),
        plot.caption = ggplot2::element_text(color = "black"),
        legend.text = ggplot2::element_text(color = "black"),
        legend.title = ggplot2::element_text(color = "black"),
        strip.text = ggplot2::element_text(color = "black"),
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank()
      )
  } else {
    # Clean cowplot-style: minimal, publication-ready, NO GRIDLINES
    ggplot2::theme_classic(base_size = axis_text_size) +
      ggplot2::theme(
        text = ggplot2::element_text(color = "black"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.5),
        axis.text = ggplot2::element_text(color = "black"),
        axis.title = ggplot2::element_text(color = "black", face = "plain"),
        plot.title = ggplot2::element_text(color = "black", face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "black"),
        plot.caption = ggplot2::element_text(color = "black"),
        legend.text = ggplot2::element_text(color = "black"),
        legend.title = ggplot2::element_text(color = "black"),
        strip.text = ggplot2::element_text(color = "black"),
        legend.key = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
      )
  }
}

tb_log_axis_label <- function(title, log_transform, format = c("expression", "html", "unicode")) {

  format <- match.arg(format)
  title <- title %||% "Intensity"
  lt <- as.character(log_transform %||% "none")
  if (lt == "none") return(title)

  if (format == "expression") {
    # R expression for ggplot2 static rendering
    if (lt == "log10") {
      return(as.expression(bquote(Log[10](.(title)))))
    }
    if (lt == "log2") {
      return(as.expression(bquote(Log[2](.(title)))))
    }
  } else if (format == "html") {
    # HTML subscript for plotly interactive rendering
    if (lt == "log10") {
      return(paste0("Log<sub>10</sub>(", title, ")"))
    }
    if (lt == "log2") {
      return(paste0("Log<sub>2</sub>(", title, ")"))
    }
  } else if (format == "unicode") {
    # Unicode subscript characters for plain text contexts
    if (lt == "log10") {
      return(paste0("Log\u2081\u2080(", title, ")"))
    }
    if (lt == "log2") {
      return(paste0("Log\u2082(", title, ")"))
    }
  }
  title
}

tb_force_black_text <- function(p) {
  if (!inherits(p, "ggplot")) return(p)

  p + ggplot2::theme(
    text = ggplot2::element_text(color = "black"),
    axis.text = ggplot2::element_text(color = "black"),
    axis.text.x = ggplot2::element_text(color = "black"),
    axis.text.y = ggplot2::element_text(color = "black"),
    axis.title = ggplot2::element_text(color = "black"),
    plot.title = ggplot2::element_text(color = "black"),
    plot.subtitle = ggplot2::element_text(color = "black"),
    plot.caption = ggplot2::element_text(color = "black"),
    legend.text = ggplot2::element_text(color = "black"),
    legend.title = ggplot2::element_text(color = "black"),
    strip.text = ggplot2::element_text(color = "black"),
    strip.text.x = ggplot2::element_text(color = "black"),
    strip.text.y = ggplot2::element_text(color = "black")
  )
}

tb_apply_ranges_xy <- function(df, style) {
  mode <- style$xy_range_mode %||% "auto"
  if (mode != "manual") return(df)
  df[df$x >= (style$xy_min %||% min(df$x)) & df$x <= (style$xy_max %||% max(df$x)) &
       df$y >= (style$xy_min %||% min(df$y)) & df$y <= (style$xy_max %||% max(df$y)), , drop = FALSE]
}

tb_find_point_xy <- function(df, id, id_col = "gene", x_col = "log2fc", y_col = "neglog10p") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  if (!(id_col %in% names(df))) return(NULL)
  i <- match(id, df[[id_col]])
  if (!is.finite(i)) return(NULL)
  list(x0 = df[[x_col]][[i]], y0 = df[[y_col]][[i]])
}

.tb_is_valid_range <- function(r) {
  is.numeric(r) && length(r) == 2L && all(is.finite(r)) && !isTRUE(all(r[[1]] == r[[2]]))
}

.tb_denormalize_xy <- function(x_norm, y_norm, x_range, y_range) {
  if (!.tb_is_valid_range(x_range) || !.tb_is_valid_range(y_range)) {
    return(list(x = NA_real_, y = NA_real_))
  }

  x_norm <- suppressWarnings(as.numeric(x_norm))
  y_norm <- suppressWarnings(as.numeric(y_norm))
  if (length(x_norm) == 0 || length(y_norm) == 0) return(list(x = NA_real_, y = NA_real_))
  x_norm <- x_norm[[1]]
  y_norm <- y_norm[[1]]
  if (!is.finite(x_norm) || !is.finite(y_norm)) return(list(x = NA_real_, y = NA_real_))

  x_norm <- pmin(1, pmax(0, x_norm))
  y_norm <- pmin(1, pmax(0, y_norm))

  list(
    x = x_range[[1]] + x_norm * (x_range[[2]] - x_range[[1]]),
    y = y_range[[1]] + y_norm * (y_range[[2]] - y_range[[1]])
  )
}

.tb_label_xy_from_state <- function(label_state, x_range, y_range) {
  label_state <- label_state %||% list()

  if (is.list(label_state) && !is.null(label_state$x_range) && !is.null(label_state$y_range)) {
    # When x_range/y_range are present in state, interpret x/y as normalized [0,1]
    # and denormalize to the *current* axis ranges so labels track style changes.
    return(.tb_denormalize_xy(label_state$x, label_state$y, x_range, y_range))
  }

  as_num1 <- function(v) {
    if (is.null(v)) return(NA_real_)
    out <- suppressWarnings(as.numeric(v))
    if (length(out) == 0) return(NA_real_)
    out[[1]]
  }

  list(
    x = as_num1(label_state$x),
    y = as_num1(label_state$y)
  )
}

# ---- Overview renderer (60/40 layout) ----------------------------------------

res_render_overview <- function(run_root, manifest, nodes_df, terpbook_filename = NULL, has_unsaved_changes = FALSE) {
  # Build 60/40 flex layout for Overview panel
  # Left side (60%): Run Overview + Run log

  # Right side (40%): Downloads + Update .terpbook


  # Extract run info
  run_name <- terpbook_filename %||% manifest$run_name %||% basename(run_root) %||% "Unknown"

  # Extract date from run log if available

  run_log_path <- file.path(run_root, "run_log.txt")
  run_date <- NULL
  if (file.exists(run_log_path)) {
    log_lines <- tryCatch(readLines(run_log_path, n = 20, warn = FALSE), error = function(e) character())
    # Look for date pattern in log
    date_line <- grep("^\\d{4}-\\d{2}-\\d{2}", log_lines, value = TRUE)
    if (length(date_line) > 0) {
      run_date <- substr(date_line[1], 1, 10)
    }
  }
  if (is.null(run_date)) {
    # Fallback to file modification time
    run_date <- tryCatch({
      format(file.info(run_root)$mtime, "%Y-%m-%d")
    }, error = function(e) "Unknown")
  }

  # Build steps list from nodes_df (include all steps including Data Processor)
  steps_list <- character()
  if (!is.null(nodes_df) && is.data.frame(nodes_df) && nrow(nodes_df) > 0) {
    step_nodes <- nodes_df[nodes_df$kind == "step", , drop = FALSE]
    if (nrow(step_nodes) > 0) {
      for (i in seq_len(nrow(step_nodes))) {
        eng <- step_nodes$engine_id[i]
        lbl <- step_nodes$label[i] %||% eng
        steps_list <- c(steps_list, lbl)
      }
    }
  }

  # Unsaved indicator
  unsaved_indicator <- if (isTRUE(has_unsaved_changes)) {
    shiny::span(style = "color: var(--accent-gold); margin-left: 8px;", shiny::icon("circle"), " Unsaved changes")
  } else {
    NULL
  }

  # Build the 60/40 layout
  shiny::div(
    class = "res-overview-layout",
    style = "display: flex; gap: 16px; height: 100%;",

    # LEFT SIDE (60%) - Run Overview + Run Log
    shiny::div(
      style = "flex: 0 0 60%; display: flex; flex-direction: column; gap: 16px; overflow: auto;",

      # Run Overview card
      shiny::div(
        class = "card",
        style = "padding: 16px; background: var(--color-white); border-radius: 8px;",
        shiny::h4("Run Overview", style = "margin-top: 0;"),
        shiny::tags$table(
          style = "width: 100%;",
          shiny::tags$tr(
            shiny::tags$td(style = "font-weight: bold; width: 120px;", "Run name:"),
            shiny::tags$td(run_name, unsaved_indicator)
          ),
          shiny::tags$tr(
            shiny::tags$td(style = "font-weight: bold;", "Date:"),
            shiny::tags$td(run_date)
          ),
          shiny::tags$tr(
            shiny::tags$td(style = "font-weight: bold; vertical-align: top;", "Steps:"),
            shiny::tags$td(
              if (length(steps_list) > 0) {
                shiny::tags$ul(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(steps_list, function(s) shiny::tags$li(s))
                )
              } else {
                shiny::em("No steps")
              }
            )
          )
        )
      ),

      # Run Log card
      shiny::div(
        class = "card",
        style = "padding: 16px; background: var(--color-white); border-radius: 8px; flex: 1; overflow: auto;",
        shiny::h4("Run log", style = "margin-top: 0;"),
        shiny::div(
          style = "max-height: 300px; overflow-y: auto; font-family: monospace; font-size: 12px; background: var(--color-white); padding: 8px; border: 1px solid var(--border-light); border-radius: 4px;",
          if (file.exists(run_log_path)) {
            log_content <- tryCatch(readLines(run_log_path, warn = FALSE), error = function(e) "Error reading log")
            shiny::pre(paste(log_content, collapse = "\n"), style = "margin: 0; white-space: pre-wrap;")
          } else {
            shiny::em("No run log available")
          }
        )
      )
    ),

    # RIGHT SIDE (40%) - Downloads + Update .terpbook
    shiny::div(
      style = "flex: 0 0 40%; display: flex; flex-direction: column; gap: 16px;",

      # Downloads card
      shiny::div(
        class = "card",
        style = "padding: 16px; background: var(--color-white); border-radius: 8px;",
        shiny::h4("Downloads", style = "margin-top: 0;"),
        shiny::div(
          style = "display: flex; flex-direction: column; gap: 8px;",
          shiny::downloadButton("res_download_terpbook", "Download .terpbook", class = "btn-primary", style = "width: 100%;"),
          shiny::downloadButton("res_download_results_zip", "Download results (ZIP)", class = "btn-secondary", style = "width: 100%;"),
          shiny::downloadButton("res_download_plots_zip", "Download plots (ZIP)", class = "btn-secondary", style = "width: 100%;")
        )
      ),

      # Update .terpbook card
      shiny::div(
        class = "card",
        style = "padding: 16px; background: var(--color-white); border-radius: 8px;",
        shiny::h4("Update .terpbook", style = "margin-top: 0;"),
        shiny::p(style = "font-size: 13px; color: var(--color-text-hint);",
                 "Save your current style changes and annotations back to the .terpbook file."),
        shiny::actionButton("res_save_terpbook", "Save changes", class = "btn-success", style = "width: 100%;",
                            icon = shiny::icon("save"))
      )
    )
  )
}

# ---- Engine renderers --------------------------------------------------------

# Helper function to create a single spearman scatter plot
.tb_scatter_correlation_single_plot <- function(df_plot, style, correlations_row, log_transform, label_x = NULL, label_y = NULL) {
  tb_require_pkg("ggplot2")

  # Swap X/Y axes if enabled (per-plot, viewer-only)
  swap_axes <- isTRUE(style$swap_axes %||% FALSE)
  if (swap_axes) {
    # Swap x and y data columns
    tmp <- df_plot$x
    df_plot$x <- df_plot$y
    df_plot$y <- tmp
    # Swap axis labels
    tmp_label <- label_x
    label_x <- label_y
    label_y <- tmp_label
  }

  axis_text_size <- tb_num(style$axis_text_size, 20)
  point_alpha <- tb_num(style$point_alpha, 1)
  point_size <- tb_num(style$point_size, 3)
  mode <- style$point_color_mode %||% "density"

  # apply manual xy filter (visual)
  df_plot <- tb_apply_ranges_xy(df_plot, style)

  # Add hover text for plotly interactivity
  gene_col <- df_plot$gene_symbol %||% df_plot$gene_id %||% df_plot$gene %||% df_plot$id %||% rownames(df_plot)
  if (is.null(gene_col)) gene_col <- seq_len(nrow(df_plot))
  df_plot$hover_text <- sprintf(
    "%s\nX: %.3f\nY: %.3f",
    gene_col, df_plot$x, df_plot$y
  )

  if (mode == "density") {
    df_plot$denscol <- grDevices::densCols(df_plot$x, df_plot$y, colramp = tb_fdr_palette)

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, text = hover_text)) +
      ggplot2::geom_point(ggplot2::aes(color = denscol),
                          alpha = point_alpha, size = point_size) +
      ggplot2::scale_color_identity()
  } else {
    flat <- style$flat_color %||% "#B0B0B0"
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, text = hover_text)) +
      ggplot2::geom_point(color = flat, alpha = point_alpha, size = point_size)
  }

  # optional best-fit line options
  line_type <- style$line_type %||% "none"
  if (line_type != "none") {
    line_col <- style$line_color %||% "#4245FF"
    line_size <- tb_num(style$line_size, 0.8)

    if (line_type == "y_equals_x") {
      # y = x identity line
      p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, color = line_col, linewidth = line_size)
    } else if (line_type == "best_fit") {
      # Linear regression: y = mx + b
      if (nrow(df_plot) > 1) {
        fit <- stats::lm(y ~ x, data = df_plot)
        p <- p + ggplot2::geom_abline(
          intercept = stats::coef(fit)[1],
          slope = stats::coef(fit)[2],
          color = line_col, linewidth = line_size
        )
      }
    } else if (line_type == "best_fit_zero_intercept") {
      # Linear regression through origin: y = mx (no intercept)
      if (nrow(df_plot) > 1) {
        fit <- stats::lm(y ~ x - 1, data = df_plot)  # -1 removes intercept
        p <- p + ggplot2::geom_abline(
          intercept = 0,
          slope = stats::coef(fit)[1],
          color = line_col, linewidth = line_size
        )
      }
    }
  }

  # Backwards compatibility: support old line_showequation (now deprecated)
  if (isTRUE(style$line_showequation %||% FALSE) && line_type == "none") {
    line_col <- style$line_color %||% "#4245FF"
    p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, color = line_col, linewidth = 0.8)
  }

  # correlation text and equation
  # Support both old style (show_rho) and new style (show_correlation) for backwards compatibility
  show_corr <- isTRUE(style$show_correlation %||% style$show_rho %||% TRUE)
  show_eq <- isTRUE(style$show_equation %||% FALSE) && line_type != "none"

  # Determine which correlation methods to display (viewer_schema options)
  show_pearson <- isTRUE(style$show_pearson %||% FALSE)
  show_spearman <- isTRUE(style$show_spearman %||% TRUE)
  show_kendall <- isTRUE(style$show_kendall %||% FALSE)

  # Extract correlation values from correlations_row
  rho_pearson <- if (!is.null(correlations_row$rho_pearson)) correlations_row$rho_pearson else NA_real_
  rho_spearman <- if (!is.null(correlations_row$rho_spearman)) correlations_row$rho_spearman else {
    # Backwards compatibility: old data may have 'rho' instead of 'rho_spearman'
    if (!is.null(correlations_row$rho)) correlations_row$rho else NA_real_
  }
  rho_kendall <- if (!is.null(correlations_row$rho_kendall)) correlations_row$rho_kendall else NA_real_

  # Check if any correlation is available to show
  has_any_corr <- (show_pearson && is.finite(rho_pearson)) ||
                  (show_spearman && is.finite(rho_spearman)) ||
                  (show_kendall && is.finite(rho_kendall))

  if ((show_corr && has_any_corr) || show_eq) {
    # FIX: Use manual range if set, otherwise use data range for correlation label positioning
    xy_range_mode <- style$xy_range_mode %||% "auto"
    if (xy_range_mode == "manual" && !is.null(style$xy_min) && !is.null(style$xy_max)) {
      xr <- c(style$xy_min, style$xy_max)
      yr <- c(style$xy_min, style$xy_max)
    } else {
      xr <- range(df_plot$x, na.rm = TRUE)
      yr <- range(df_plot$y, na.rm = TRUE)
    }
    # Support both old (rho_position_*) and new (corr_position_*) style names
    x_frac <- tb_num(style$corr_position_x %||% style$rho_position_x, 0.05)
    y_frac <- tb_num(style$corr_position_y %||% style$rho_position_y, 0.95)

    x_pos <- xr[1] + x_frac * diff(xr)
    y_pos <- yr[1] + y_frac * diff(yr)

    # Support both old (rho_text_size) and new (corr_text_size) style names
    corr_text_size <- suppressWarnings(as.numeric(style$corr_text_size %||% style$rho_text_size %||% 8))
    if (!is.finite(corr_text_size) || corr_text_size <= 0) corr_text_size <- 8
    text_size <- corr_text_size / 3.5
    text_color <- style$corr_color %||% style$rho_color %||% "#000000"

    # Build correlation text based on selected methods
    if (show_corr && has_any_corr) {
      corr_lines <- character(0)
      if (show_pearson && is.finite(rho_pearson)) {
        corr_lines <- c(corr_lines, sprintf("r = %.3f", rho_pearson))
      }
      if (show_spearman && is.finite(rho_spearman)) {
        corr_lines <- c(corr_lines, sprintf("\u03C1 = %.3f", rho_spearman))
      }
      if (show_kendall && is.finite(rho_kendall)) {
        corr_lines <- c(corr_lines, sprintf("\u03C4 = %.3f", rho_kendall))
      }

      # Add each correlation line
      line_offset <- diff(yr) * 0.06  # ~6% of y range per line
      for (i in seq_along(corr_lines)) {
        y_line_pos <- y_pos - (i - 1) * line_offset
        p <- p + ggplot2::annotate(
          "text", x = x_pos, y = y_line_pos, label = corr_lines[i],
          hjust = 0, vjust = 1,
          size = text_size,
          color = text_color
        )
      }

      # Track how many lines we've used for equation positioning
      n_corr_lines <- length(corr_lines)
    } else {
      n_corr_lines <- 0
      line_offset <- diff(yr) * 0.06
    }

    # Add equation below correlations (when best-fit line is shown)
    if (show_eq && nrow(df_plot) > 1) {
      # Compute equation based on line type
      eq_txt <- NULL
      if (line_type == "y_equals_x") {
        eq_txt <- "y = x"
      } else if (line_type == "best_fit") {
        fit <- stats::lm(y ~ x, data = df_plot)
        intercept <- stats::coef(fit)[1]
        slope <- stats::coef(fit)[2]
        eq_txt <- sprintf("y = %.3fx %s %.3f",
                          slope,
                          if (intercept >= 0) "+" else "-",
                          abs(intercept))
      } else if (line_type == "best_fit_zero_intercept") {
        fit <- stats::lm(y ~ x - 1, data = df_plot)
        slope <- stats::coef(fit)[1]
        eq_txt <- sprintf("y = %.3fx", slope)
      }

      if (!is.null(eq_txt)) {
        y_eq_pos <- y_pos - n_corr_lines * line_offset

        p <- p + ggplot2::annotate(
          "text", x = x_pos, y = y_eq_pos, label = eq_txt,
          hjust = 0, vjust = 1,
          size = text_size,
          color = text_color
        )
      }
    }
  }

  # axis labels: Use group names if provided, otherwise fallback to generic axis_title
  # FIX: Removed default 'Intensity' - now uses actual group/sample labels
  x_title <- label_x %||% style$axis_title %||% NULL
  y_title <- label_y %||% style$axis_title %||% NULL

  # Apply log transform annotation to axis titles if provided
  x_label <- if (!is.null(x_title)) tb_log_axis_label(x_title, log_transform) else NULL
  y_label <- if (!is.null(y_title)) tb_log_axis_label(y_title, log_transform) else NULL

  p <- p +
    ggplot2::labs(x = x_label, y = y_label) +
    tb_theme_base(axis_text_size, axis_style = style$axis_style %||% "clean")

  # FIX: Apply manual xy range limits via coord_cartesian (handles min=0 properly)
  # Use expand = FALSE to strictly enforce manual limits even when data doesn't span full range
  xy_range_mode <- style$xy_range_mode %||% "auto"
  if (xy_range_mode == "manual") {
    xy_min <- style$xy_min
    xy_max <- style$xy_max
    # Only apply if both values are provided
    if (!is.null(xy_min) && !is.null(xy_max)) {
      p <- p + ggplot2::coord_cartesian(
        xlim = c(xy_min, xy_max),
        ylim = c(xy_min, xy_max),
        expand = FALSE
      )
    }
  }

  p
}

tb_render_scatter_correlation <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  df <- results$data$points
  if (is.null(df) || !is.data.frame(df)) stop("scatter_correlation results$data$points missing.")

  log_transform <- results$params$log_transform %||% "none"
  correlations <- results$data$correlations

  # Check if there are multiple pair_ids
  pair_ids <- unique(df$pair_id)

  if (!is.null(pair_ids) && length(pair_ids) > 1) {
    # Multiple comparisons - render each as a separate plot
    plots <- list()

    for (pid in pair_ids) {
      df_pair <- df[df$pair_id == pid, , drop = FALSE]
      if (nrow(df_pair) > 0) {
        # Get correlation values and axis labels for this pair from correlations table
        corr_row <- NULL
        label_x <- NULL
        label_y <- NULL
        if (!is.null(correlations) && is.data.frame(correlations)) {
          corr_row <- correlations[correlations$pair_id == pid, , drop = FALSE]
          if (nrow(corr_row) > 0) {
            label_x <- corr_row$label_x[[1]]
            label_y <- corr_row$label_y[[1]]
          } else {
            corr_row <- NULL
          }
        }
        # If no corr_row, create an empty one
        if (is.null(corr_row) || nrow(corr_row) == 0) {
          corr_row <- data.frame(
            rho_pearson = NA_real_, rho_spearman = NA_real_, rho_kendall = NA_real_,
            stringsAsFactors = FALSE
          )
        }
        plots[[pid]] <- .tb_scatter_correlation_single_plot(df_pair, style, corr_row, log_transform, label_x, label_y)
      }
    }

    if (length(plots) == 0) {
      stop("scatter_correlation results$data$points has no valid pairs.")
    }

    return(list(plots = plots, tables = list(correlations = correlations)))
  }

  # Single comparison or no pair_id - render all points together
  label_x <- NULL
  label_y <- NULL
  corr_row <- NULL
  if (!is.null(correlations) && is.data.frame(correlations) && nrow(correlations) > 0) {
    corr_row <- correlations[1, , drop = FALSE]
    label_x <- correlations$label_x[[1]]
    label_y <- correlations$label_y[[1]]
  }
  # If no corr_row, create an empty one
  if (is.null(corr_row) || nrow(corr_row) == 0) {
    corr_row <- data.frame(
      rho_pearson = NA_real_, rho_spearman = NA_real_, rho_kendall = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  p <- .tb_scatter_correlation_single_plot(df, style, corr_row, log_transform, label_x, label_y)

  list(plots = list(scatter_correlation = p), tables = list(correlations = correlations))
}

# Legacy alias for backwards compatibility
tb_render_spearman <- tb_render_scatter_correlation

# Helper function to create a single volcano plot from a points data.frame
.tb_volcano_single_plot <- function(df, style, meta, comparison_name = "", apply_fdr = TRUE,
                                    fc_threshold = NULL, p_threshold = NULL) {
  tb_require_pkg("ggplot2")

  # FIX: Prefer gene_symbol; fall back to gene_id, then legacy gene fields.

  if (is.null(df$gene)) {
    df$gene <- df$gene_symbol %||% df$gene_id %||% df$id %||% df$Gene %||% ""
  }
  # Ensure gene is character (not list or other types that cause JS serialization issues)
  df$gene <- as.character(df$gene)

  # Flip FC direction if enabled (per-plot, viewer-only)
  # This negates log2fc to swap up/down interpretation
  flip_fc <- isTRUE(style$flip_fc %||% FALSE)
  if (flip_fc && !is.null(df$log2fc)) {
    df$log2fc <- -df$log2fc
  }

  # thresholds: use passed compute-time params, fallback to defaults
  fc_thr <- suppressWarnings(as.numeric(fc_threshold %||% c(-1, 1)))
  if (length(fc_thr) != 2 || any(!is.finite(fc_thr))) fc_thr <- c(-1, 1)

  p_thr <- suppressWarnings(as.numeric(p_threshold %||% 0.05))
  if (!is.finite(p_thr) || p_thr <= 0) p_thr <- 0.05

  col_up <- style$col_sig_up %||% "#FF4242"
  col_dn <- style$col_sig_down %||% "#4242FF"
  col_ns <- style$col_nonsig %||% "#B0B0B0"

  # FIX: Use padj when apply_fdr=TRUE, otherwise use raw pval
  # This ensures the y-axis matches what was used for significance calling
  if (isTRUE(apply_fdr) && !is.null(df$padj)) {
    pvec <- df$padj
    y_axis_label_p <- expression(-log[10](p[adj]))
    hover_p_label <- "p-adj"
  } else {
    pvec <- df$pval %||% df$p %||% df$padj
    y_axis_label_p <- expression(-log[10](p))
    hover_p_label <- "p-value"
  }
  if (is.null(pvec)) stop("volcano points missing p-value column (expected pval, p, or padj).")
  pvec <- suppressWarnings(as.numeric(pvec))
  pvec <- pmax(pvec, 1e-300)
  pvec[!is.finite(pvec)] <- 1
  df$pval_display <- pvec
  df$neglog10p <- -log10(df$pval_display)
  df$neglog10p <- pmax(df$neglog10p, 0)  # Ensure no points render below y=0 axis

  # FIX: Use pval_display for significance coloring (matches y-axis)
  df$sig <- "nonsig"
  df$sig[df$log2fc >= max(fc_thr) & df$pval_display <= p_thr] <- "up"
  df$sig[df$log2fc <= min(fc_thr) & df$pval_display <= p_thr] <- "down"

  # Optional hide list MUST happen before ggplot() so it actually hides points
  hide_ids <- unique(as.character(meta$visibility$hide_ids %||% character()))
  hide_ids <- hide_ids[nzchar(hide_ids)]
  if (length(hide_ids) > 0) df <- df[!(df$gene %in% hide_ids), , drop = FALSE]

  # Compute plot limits once and reuse (also for clamping saved label positions)
  # Respect x_range_mode and y_range_mode: use manual values only when mode is "manual"
  x_range_mode <- style$x_range_mode %||% "auto"
  y_range_mode <- style$y_range_mode %||% "auto"

  # X limits: auto uses data range with padding, manual uses user values
 if (x_range_mode == "manual") {
    xlim <- suppressWarnings(as.numeric(c(style$x_min %||% -7, style$x_max %||% 7)))
  } else {
    # Auto: compute from data with symmetric padding
    x_data_range <- range(df$log2fc, na.rm = TRUE, finite = TRUE)
    x_pad <- max(abs(x_data_range)) * 1.1
    xlim <- c(-x_pad, x_pad)
  }
  if (length(xlim) != 2 || any(!is.finite(xlim))) xlim <- c(-7, 7)
  xlim <- sort(xlim)

  # Y limits: auto uses 0 to data max, manual uses user values
  if (y_range_mode == "manual") {
    y_min_val <- style$y_min %||% 0
    ylim <- suppressWarnings(as.numeric(c(y_min_val, style$y_max %||% max(df$neglog10p, na.rm = TRUE))))
  } else {
    ylim <- c(0, max(df$neglog10p, na.rm = TRUE) * 1.05)
  }
  if (length(ylim) != 2 || any(!is.finite(ylim))) ylim <- c(0, max(df$neglog10p, na.rm = TRUE))
  ylim <- sort(ylim)

  # FIX: Add hover text for plotly with correct FC/p-value data (use displayed p-value)
  df$hover_text <- sprintf(
    "%s\nlog2FC: %.3f\n%s: %.2e",
    df$gene,
    df$log2fc,
    hover_p_label,
    df$pval_display
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = log2fc, y = neglog10p, text = hover_text)) +
    ggplot2::geom_point(ggplot2::aes(color = sig), size = tb_num(style$point_size, 3), alpha = 0.9) +
    ggplot2::scale_color_manual(values = c(up = col_up, down = col_dn, nonsig = col_ns))

  # ---- Highlight Groups Rendering ----
  # Parse highlight groups from style and overlay colored points
  highlight_map_json <- style$highlight_groups_map %||% "{}"
  highlight_map <- tryCatch(jsonlite::fromJSON(highlight_map_json, simplifyVector = FALSE), error = function(e) list())
  plot_key <- if (nzchar(comparison_name)) comparison_name else "volcano_plot"
  highlight_groups <- highlight_map[[plot_key]] %||% list()

  if (length(highlight_groups) > 0) {
    # Build gene -> group mapping (last group wins - process in order, overwrite)
    gene_to_group <- list()
    for (g in highlight_groups) {
      genes <- trimws(unlist(strsplit(as.character(g$genes %||% ""), "\n", fixed = TRUE)))
      genes <- genes[nzchar(genes)]
      for (gene in genes) {
        gene_to_group[[gene]] <- g  # Last group claiming gene wins
      }
    }

    # Reverse mapping: group_id -> genes (after conflict resolution)
    group_to_genes <- list()
    for (gene in names(gene_to_group)) {
      gid <- gene_to_group[[gene]]$id
      group_to_genes[[gid]] <- c(group_to_genes[[gid]], gene)
    }

    # Track which groups have data for legend
    groups_with_data <- character()

    # Add layer for each group (process in order so last groups render on top)
    for (g in highlight_groups) {
      gid <- g$id
      genes_in_group <- group_to_genes[[gid]]
      if (length(genes_in_group) == 0) next

      df_hl <- df[df$gene %in% genes_in_group, , drop = FALSE]
      if (nrow(df_hl) == 0) next

      group_name <- g$name %||% gid
      fill_color <- g$color %||% "#FF6600"
      has_outline <- isTRUE(g$outline)
      outline_width <- as.numeric(g$outline_width %||% 1)

      # Create a factor column for consistent legend
      df_hl$highlight_group <- factor(group_name, levels = group_name)
      groups_with_data <- c(groups_with_data, group_name)

      if (has_outline) {
        # Shape 21 = filled circle with border
        p <- p + ggplot2::geom_point(
          data = df_hl,
          ggplot2::aes(x = log2fc, y = neglog10p, fill = highlight_group),
          shape = 21,
          size = tb_num(style$point_size, 3) + 0.5,
          color = "black",
          stroke = outline_width,
          inherit.aes = FALSE
        )
      } else {
        # No outline - use fill color as border too
        p <- p + ggplot2::geom_point(
          data = df_hl,
          ggplot2::aes(x = log2fc, y = neglog10p, fill = highlight_group),
          shape = 21,
          size = tb_num(style$point_size, 3) + 0.5,
          color = fill_color,
          stroke = 0.5,
          inherit.aes = FALSE
        )
      }
    }

    # Add fill scale for legend (only groups that have data)
    if (length(groups_with_data) > 0) {
      fill_values <- stats::setNames(
        sapply(highlight_groups, function(g) g$color %||% "#FF6600"),
        sapply(highlight_groups, function(g) g$name %||% g$id)
      )
      # Filter to only groups with data
      fill_values <- fill_values[names(fill_values) %in% groups_with_data]

      p <- p + ggplot2::scale_fill_manual(
        values = fill_values,
        name = "Highlight",
        guide = ggplot2::guide_legend(order = 2)
      )
      # Reorder significance legend to come first
      p <- p + ggplot2::guides(
        color = ggplot2::guide_legend(title = "Significance", order = 1)
      )
    }
  }

  if (isTRUE(style$show_cut_lines %||% FALSE)) {
    p <- p +
      ggplot2::geom_vline(xintercept = c(min(fc_thr), max(fc_thr)), linetype = "dotted", color = "black") +
      ggplot2::geom_hline(yintercept = -log10(p_thr), linetype = "dotted", color = "black")
  }

  # FIX: Dynamic x-axis label reflects comparison (e.g., log2(BafA1/Control))
  # Use bquote() for subscript notation in ggplot
  x_axis_label <- if (nzchar(comparison_name)) {
    # Format as log[2](Group1/Group2) from Group1_vs_Group2
    parts <- unlist(strsplit(comparison_name, "_vs_", fixed = TRUE))
    if (length(parts) == 2) {
      g1 <- if (flip_fc) parts[2] else parts[1]
      g2 <- if (flip_fc) parts[1] else parts[2]
      bquote(log[2](.(g1) / .(g2)))
    } else {
      bquote(log[2]*FC ~ (.(comparison_name)))
    }
  } else {
    expression(log[2]*FC)
  }

  # FIX: Use scale_y_continuous with expand = c(0, 0.05) so y=0 touches x-axis
  # FIX: Use dynamic y-axis label based on apply_fdr setting
  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"
  if (identical(tolower(style$view_mode %||% ""), "interactive")) {
    axis_text_size <- 12
    axis_style <- "clean"
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = format_k_suffix, expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, clip = "on") +
    ggplot2::labs(x = x_axis_label, y = y_axis_label_p) +
    tb_theme_base(axis_text_size, axis_style = axis_style) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"))

  # Get per-plot gene labels from label_genes_map (JSON map: plot_name -> labels)
  label_map_json <- style$label_genes_map %||% "{}"
  label_map <- tryCatch(jsonlite::fromJSON(label_map_json, simplifyVector = FALSE), error = function(e) list())
  # Use comparison_name to look up labels; fall back to generic "volcano_plot" for single-plot case
  plot_key <- if (nzchar(comparison_name)) comparison_name else "volcano_plot"
  labs <- label_map[[plot_key]] %||% ""
  labs <- trimws(unlist(strsplit(as.character(labs), "\n", fixed = TRUE)))
  labs <- labs[nzchar(labs)]
  labs <- intersect(labs, df$gene)

  # Saved plotly label positions (reflected in ggplot)
  saved <- meta$plotly$labels_by_plot[[plot_key]] %||%
    meta$plotly$labels_by_plot$default %||%
    meta$plotly$labels %||% list()

  if (length(labs) > 0) {
    df_lab <- df[df$gene %in% labs, , drop = FALSE]

    if (nrow(df_lab) > 0) {
      if (identical(style$label_mode %||% "color_sig", "hide_nonsig")) {
        df_lab <- df_lab[df_lab$sig != "nonsig", , drop = FALSE]
      }
    }

    if (nrow(df_lab) > 0) {
      df_lab$lx <- NA_real_
      df_lab$ly <- NA_real_

      for (i in seq_len(nrow(df_lab))) {
        id <- as.character(df_lab$gene[[i]])
        s <- saved[[id]] %||% list()

        pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)

        lx <- pos$x
        ly <- pos$y

        if (!is.finite(lx)) lx <- df_lab$log2fc[[i]]
        if (!is.finite(ly)) ly <- df_lab$neglog10p[[i]]

        if (is.finite(lx) && is.finite(ly) && is.null(s$x_range) && is.null(s$y_range)) {
          # If the label has no saved position (or it's invalid), give a small default offset.
          # (Normalized saved positions will already be placed by .tb_label_xy_from_state.)
          if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) || !is.finite(suppressWarnings(as.numeric(s$y)))) {
            lx <- df_lab$log2fc[[i]] + ifelse(df_lab$log2fc[[i]] < 0, -0.5, 0.5)
            ly <- df_lab$neglog10p[[i]] + 0.5
          }
        }

        # Clamp to visible plot window so labels can't "disappear" via clipping
        lx <- max(xlim[[1]], min(xlim[[2]], lx))
        ly <- max(ylim[[1]], min(ylim[[2]], ly))

        df_lab$lx[[i]] <- lx
        df_lab$ly[[i]] <- ly
      }

      # FIX: Use label_font_size style parameter for gene label size
      label_font_size <- suppressWarnings(as.numeric(style$label_font_size %||% 12))
      if (!is.finite(label_font_size) || label_font_size <= 0) label_font_size <- 12
      label_size <- label_font_size / 3  # Convert points to ggplot mm scale
      p <- p +
        ggplot2::geom_segment(
          data = df_lab,
          ggplot2::aes(x = log2fc, y = neglog10p, xend = lx, yend = ly),
          linewidth = 0.3, color = "black"
        ) +
        ggplot2::geom_text(
          data = df_lab,
          ggplot2::aes(x = lx, y = ly, label = gene),
          size = label_size, color = "black", hjust = 0.5, vjust = 0
        )
    }
  }

  # Store plot limits as attributes for use by summary cards
  attr(p, "xlim") <- xlim
  attr(p, "ylim") <- ylim

  p
}

# ============================================================
# Plotly-based interactive volcano plot for label editing
# - Click on point: triggers uniprot search (handled by Shiny)
# - Drag annotations: repositions labels (captured via relayout events)
# - Exports to ggplot for final output
# ============================================================
tb_volcano_plotly <- function(df, style, meta, xlim, ylim, labs, saved, comparison_name = "", apply_fdr = TRUE,
                              fc_threshold = NULL, p_threshold = NULL) {
  tb_require_pkg("plotly")

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Normalize gene column
  if (is.null(df$gene)) {
    df$gene <- df$gene_symbol %||% df$gene_id %||% df$id %||% df$Gene %||% ""
  }
  df$gene <- as.character(df$gene)

  # Flip FC direction if enabled (per-plot, viewer-only)
  # This negates log2fc to swap up/down interpretation
  flip_fc <- isTRUE(style$flip_fc %||% FALSE)
  if (flip_fc && !is.null(df$log2fc)) {
    df$log2fc <- -df$log2fc
    # Also flip the xlim to keep same visual range
    xlim <- -rev(xlim)
  }

  # Thresholds
  fc_thr <- suppressWarnings(as.numeric(fc_threshold %||% c(-1, 1)))
  if (length(fc_thr) != 2 || any(!is.finite(fc_thr))) fc_thr <- c(-1, 1)

  p_thr <- suppressWarnings(as.numeric(p_threshold %||% 0.05))
  if (!is.finite(p_thr) || p_thr <= 0) p_thr <- 0.05

  # Colors
  col_up <- style$col_sig_up %||% "#FF4242"
  col_dn <- style$col_sig_down %||% "#4242FF"
  col_ns <- style$col_nonsig %||% "#B0B0B0"

  # P-value column based on apply_fdr
  if (isTRUE(apply_fdr) && !is.null(df$padj)) {
    pvec <- df$padj
    hover_p_label <- "p-adj"
  } else {
    pvec <- df$pval %||% df$p %||% df$padj
    hover_p_label <- "p-value"
  }
  pvec <- suppressWarnings(as.numeric(pvec))
  pvec <- pmax(pvec, 1e-300)
  pvec[!is.finite(pvec)] <- 1
  df$pval_display <- pvec
  df$neglog10p <- -log10(df$pval_display)

  # Significance coloring
  df$sig <- "nonsig"
  df$sig[df$log2fc >= max(fc_thr) & df$pval_display <= p_thr] <- "up"
  df$sig[df$log2fc <= min(fc_thr) & df$pval_display <= p_thr] <- "down"

  # Apply hide_ids filter
  hide_ids <- unique(as.character(meta$visibility$hide_ids %||% character()))
  hide_ids <- hide_ids[nzchar(hide_ids)]
  if (length(hide_ids) > 0) df <- df[!(df$gene %in% hide_ids), , drop = FALSE]

  # Map significance to colors
  color_map <- c(up = col_up, down = col_dn, nonsig = col_ns)
  df$color <- color_map[df$sig]

  # Build annotations and shapes for labels
  annotations <- list()
  shapes <- list()
  label_size <- suppressWarnings(as.numeric(style$label_font_size %||% 12))
  if (!is.finite(label_size) || label_size <= 0) label_size <- 12

  if (length(labs) > 0) {
    df_lab <- df[df$gene %in% labs, , drop = FALSE]

    if (nrow(df_lab) > 0) {
      if (identical(style$label_mode %||% "color_sig", "hide_nonsig")) {
        df_lab <- df_lab[df_lab$sig != "nonsig", , drop = FALSE]
      }
    }

    if (nrow(df_lab) > 0) {
      for (i in seq_len(nrow(df_lab))) {
        id <- as.character(df_lab$gene[[i]])
        s <- saved[[id]] %||% list()

        # Get saved position or compute default
        lx <- suppressWarnings(as.numeric(s$x %||% NA_real_))
        ly <- suppressWarnings(as.numeric(s$y %||% NA_real_))

        # Apply range-normalized positions if available
        if (!is.null(s$x_range) && !is.null(s$y_range)) {
          pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)
          lx <- pos$x
          ly <- pos$y
        }

        if (!is.finite(lx)) lx <- df_lab$log2fc[[i]]
        if (!is.finite(ly)) ly <- df_lab$neglog10p[[i]]

        # Default offset if no saved position
        if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) ||
            !is.finite(suppressWarnings(as.numeric(s$y)))) {
          if (is.null(s$x_range) && is.null(s$y_range)) {
            x_offset <- ifelse(df_lab$log2fc[[i]] < 0, -0.5, 0.5) * diff(xlim) / 7
            y_offset <- 0.5 * diff(ylim) / 10
            lx <- df_lab$log2fc[[i]] + x_offset
            ly <- df_lab$neglog10p[[i]] + y_offset
          }
        }

        # Clamp to visible plot window
        lx <- max(xlim[[1]], min(xlim[[2]], lx))
        ly <- max(ylim[[1]], min(ylim[[2]], ly))

        # Connector line (as a shape)
        shapes <- c(shapes, list(list(
          type = "line",
          x0 = df_lab$log2fc[[i]],
          y0 = df_lab$neglog10p[[i]],
          x1 = lx,
          y1 = ly,
          xref = "x",
          yref = "y",
          line = list(color = "#333333", width = 1)
        )))

        # Label annotation (draggable) - add small Y offset to separate from line endpoint
        label_y_offset <- 0.02 * diff(ylim)
        annotations <- c(annotations, list(list(
          x = lx,
          y = ly + label_y_offset,
          text = id,
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          font = list(size = label_size, color = "#000000"),
          xanchor = "center",
          yanchor = "bottom",
          captureevents = TRUE,
          name = id
        )))
      }
    }
  }

  # Threshold lines (if enabled)
  show_cut_lines <- isTRUE(style$show_cut_lines %||% FALSE)
  if (show_cut_lines) {
    # Vertical fold-change thresholds
    shapes <- c(shapes, list(
      list(type = "line", x0 = min(fc_thr), x1 = min(fc_thr), y0 = ylim[[1]], y1 = ylim[[2]],
           xref = "x", yref = "y", line = list(color = "gray", width = 1, dash = "dash")),
      list(type = "line", x0 = max(fc_thr), x1 = max(fc_thr), y0 = ylim[[1]], y1 = ylim[[2]],
           xref = "x", yref = "y", line = list(color = "gray", width = 1, dash = "dash")),
      # Horizontal p-value threshold
      list(type = "line", x0 = xlim[[1]], x1 = xlim[[2]], y0 = -log10(p_thr), y1 = -log10(p_thr),
           xref = "x", yref = "y", line = list(color = "gray", width = 1, dash = "dash"))
    ))
  }

  # Dynamic x-axis label (use HTML subscript for plotly)
  x_axis_label <- if (nzchar(comparison_name)) {
    parts <- unlist(strsplit(comparison_name, "_vs_", fixed = TRUE))
    if (length(parts) == 2) {
      p1 <- if (flip_fc) parts[2] else parts[1]
      p2 <- if (flip_fc) parts[1] else parts[2]
      paste0("log<sub>2</sub>(", p1, "/", p2, ")")
    } else {
      paste0("log<sub>2</sub>FC (", comparison_name, ")")
    }
  } else {
    "log<sub>2</sub>FC"
  }

  # Y-axis label
  y_axis_label <- if (isTRUE(apply_fdr)) "-log10(p-adj)" else "-log10(p)"

  # Create plotly
  p <- plotly::plot_ly(
    data = df,
    x = ~log2fc,
    y = ~neglog10p,
    customdata = ~gene,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = ~color,
      size = tb_num(style$point_size, 3) * 2.5,
      opacity = 0.9
    ),
    text = ~paste0(gene, "\nlog2FC: ", round(log2fc, 3), "\n", hover_p_label, ": ",
                   format(pval_display, scientific = TRUE, digits = 2)),
    hoverinfo = "text",
    source = "volcano_plotly",
    name = "Genes",
    showlegend = FALSE
  )

  # ---- Highlight Groups Rendering (Plotly) ----
  highlight_map_json <- style$highlight_groups_map %||% "{}"
  highlight_map <- tryCatch(jsonlite::fromJSON(highlight_map_json, simplifyVector = FALSE), error = function(e) list())
  plot_key_hl <- if (nzchar(comparison_name)) comparison_name else "volcano_plot"
  highlight_groups <- highlight_map[[plot_key_hl]] %||% list()

  if (length(highlight_groups) > 0) {
    # Build gene -> group mapping (last group wins)
    gene_to_group <- list()
    for (g in highlight_groups) {
      genes <- trimws(unlist(strsplit(as.character(g$genes %||% ""), "\n", fixed = TRUE)))
      genes <- genes[nzchar(genes)]
      for (gene in genes) {
        gene_to_group[[gene]] <- g
      }
    }

    # Reverse mapping: group_id -> genes
    group_to_genes <- list()
    for (gene in names(gene_to_group)) {
      gid <- gene_to_group[[gene]]$id
      group_to_genes[[gid]] <- c(group_to_genes[[gid]], gene)
    }

    # Add traces for each highlight group
    for (g in highlight_groups) {
      gid <- g$id
      genes_in_group <- group_to_genes[[gid]]
      if (length(genes_in_group) == 0) next

      df_hl <- df[df$gene %in% genes_in_group, , drop = FALSE]
      if (nrow(df_hl) == 0) next

      group_name <- g$name %||% gid
      fill_color <- g$color %||% "#FF6600"
      has_outline <- isTRUE(g$outline)
      outline_width <- as.numeric(g$outline_width %||% 1)

      marker_config <- if (has_outline) {
        list(
          color = fill_color,
          size = tb_num(style$point_size, 3) * 2.5 + 2,
          opacity = 1,
          line = list(color = "black", width = outline_width)
        )
      } else {
        list(
          color = fill_color,
          size = tb_num(style$point_size, 3) * 2.5 + 2,
          opacity = 1,
          line = list(color = fill_color, width = 0.5)
        )
      }

      p <- p %>% plotly::add_trace(
        data = df_hl,
        x = ~log2fc,
        y = ~neglog10p,
        customdata = ~gene,
        type = "scatter",
        mode = "markers",
        marker = marker_config,
        text = ~paste0(gene, "\nlog2FC: ", round(log2fc, 3), "\n", hover_p_label, ": ",
                       format(pval_display, scientific = TRUE, digits = 2)),
        hoverinfo = "text",
        name = group_name,
        legendgroup = "highlight",
        showlegend = TRUE
      )
    }
  }

  # Show legend only if there are highlight groups
  has_hl_groups <- length(highlight_groups) > 0

  p <- p %>%
    plotly::layout(
      xaxis = list(
        title = x_axis_label,
        range = xlim,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        constrain = "domain"
      ),
      yaxis = list(
        title = y_axis_label,
        range = ylim,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        constrain = "domain"
      ),
      shapes = shapes,
      annotations = annotations,
      showlegend = has_hl_groups,
      legend = list(x = 1.02, y = 1, xanchor = "left"),
      dragmode = "pan",
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) %>%
    plotly::config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = FALSE
      ),
      displayModeBar = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
    )

  p <- plotly::event_register(p, "plotly_click")
  p <- plotly::event_register(p, "plotly_relayout")

  p
}

# ============================================================
# Plotly-based interactive 2D GO-FCS plot for label editing
# - Click on point: does nothing (labels added via styles panel textbox only)
# - Drag annotations: repositions labels (captured via relayout events)
# - Exports to ggplot for final output
# ============================================================
tb_2dgofcs_plotly <- function(df_plot, style, meta, xlim, ylim, labs, saved, x_label, y_label, plot_key = "2dgofcs_plot") {
  tb_require_pkg("plotly")

  if (is.null(df_plot) || !is.data.frame(df_plot) || nrow(df_plot) == 0) {
    return(NULL)
  }

  # Style parameters
  label_size <- suppressWarnings(as.numeric(style$label_font_size %||% 12))
  if (!is.finite(label_size) || label_size <= 0) label_size <- 12
  alpha <- tb_num(style$dot_alpha %||% style$alpha, 0.8)
  fdr_palette <- style$fdr_palette %||% "yellow_cap"

  # Color mode
  cm <- style$color_mode %||% "fdr"
  use_flat_color <- (cm == "flat")
  flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

  # FDR color mapping
  if (!use_flat_color) {
    # Generate FDR colors
    fdr_vals <- df_plot$fdr
    fdr_vals <- pmax(fdr_vals, 1e-300)
    fdr_vals[!is.finite(fdr_vals)] <- 0.05

    if (fdr_palette == "blue_red") {
      # Blue (low FDR) to red (high FDR)
      pal_colors <- c("#2e66a0", "#6a8ab8", "#b0b0b0", "#c88080", "#d14f58")
    } else {
      # Yellow cap: yellow (low) to black (high)
      pal_colors <- c("yellow", "#ff7ab0", "#881bff", "#2727d6", "black")
    }

    # Normalize FDR to 0-1 scale using log10
    log_fdr <- log10(fdr_vals)
    log_min <- log10(min(fdr_vals[fdr_vals > 0], na.rm = TRUE))
    log_max <- log10(max(fdr_vals, na.rm = TRUE))
    if (log_min >= log_max) log_max <- log_min + 1

    norm_fdr <- (log_fdr - log_min) / (log_max - log_min)
    norm_fdr[!is.finite(norm_fdr)] <- 0
    norm_fdr <- pmax(0, pmin(1, norm_fdr))

    # Map to color palette
    color_scale <- grDevices::colorRamp(pal_colors)
    df_plot$color <- grDevices::rgb(color_scale(norm_fdr) / 255)
  } else {
    df_plot$color <- flat_color
  }

  # Build annotations and shapes for labels
  annotations <- list()
  shapes <- list()

  if (length(labs) > 0) {
    df_lab <- df_plot[df_plot$term %in% labs, , drop = FALSE]

    if (nrow(df_lab) > 0) {
      for (i in seq_len(nrow(df_lab))) {
        term_id <- as.character(df_lab$term_id[[i]] %||% df_lab$term_original[[i]] %||% df_lab$term[[i]])
        display_term <- as.character(df_lab$term[[i]])
        # Append GO ID if enabled (without resetting custom labels)
        if (isTRUE(style$show_go_id) && !is.null(term_id) && nzchar(term_id)) {
          # Only append if not already present
          if (!grepl(term_id, display_term, fixed = TRUE)) {
            display_term <- paste0(display_term, " (", term_id, ")")
          }
        }
        s <- saved[[term_id]] %||% saved[[df_lab$term_original[[i]]]] %||% saved[[display_term]] %||% list()

        # Get saved position or compute default
        lx <- suppressWarnings(as.numeric(s$x %||% NA_real_))
        ly <- suppressWarnings(as.numeric(s$y %||% NA_real_))

        # Apply range-normalized positions if available
        if (!is.null(s$x_range) && !is.null(s$y_range)) {
          pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)
          lx <- pos$x
          ly <- pos$y
        }

        if (!is.finite(lx)) lx <- df_lab$score_x[[i]]
        if (!is.finite(ly)) ly <- df_lab$score_y[[i]]

        # Default offset if no saved position
        if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) ||
            !is.finite(suppressWarnings(as.numeric(s$y)))) {
          if (is.null(s$x_range) && is.null(s$y_range)) {
            # Offset based on quadrant
            x_offset <- ifelse(df_lab$score_x[[i]] < 0, -0.05, 0.05) * diff(xlim)
            y_offset <- ifelse(df_lab$score_y[[i]] < 0, -0.05, 0.05) * diff(ylim)
            lx <- df_lab$score_x[[i]] + x_offset
            ly <- df_lab$score_y[[i]] + y_offset
          }
        }

        # Clamp to visible plot window
        lx <- max(xlim[[1]], min(xlim[[2]], lx))
        ly <- max(ylim[[1]], min(ylim[[2]], ly))

        # Connector line (as a shape)
        shapes <- c(shapes, list(list(
          type = "line",
          x0 = df_lab$score_x[[i]],
          y0 = df_lab$score_y[[i]],
          x1 = lx,
          y1 = ly,
          xref = "x",
          yref = "y",
          line = list(color = "#333333", width = 1)
        )))

        # Label annotation (draggable) - add small Y offset to separate from line endpoint
        label_y_offset <- 0.015 * diff(ylim)
        annotations <- c(annotations, list(list(
          x = lx,
          y = ly + label_y_offset,
          text = display_term,
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          font = list(size = label_size, color = "#000000"),
          xanchor = "center",
          yanchor = "bottom",
          captureevents = TRUE,
          name = term_id
        )))
      }
    }
  }

  # Reference lines
  show_ref_lines <- isTRUE(style$show_ref_lines %||% TRUE)
  ref_line_size <- tb_num(style$ref_line_size, 1)
  ref_line_dotted <- isTRUE(style$ref_line_dotted %||% FALSE)
  ref_line_dash <- if (ref_line_dotted) "dash" else "solid"

  if (show_ref_lines) {
    shapes <- c(shapes, list(
      list(type = "line", x0 = xlim[[1]], x1 = xlim[[2]], y0 = 0, y1 = 0,
           xref = "x", yref = "y", line = list(color = "black", width = ref_line_size, dash = ref_line_dash)),
      list(type = "line", x0 = 0, x1 = 0, y0 = ylim[[1]], y1 = ylim[[2]],
           xref = "x", yref = "y", line = list(color = "black", width = ref_line_size, dash = ref_line_dash))
    ))
  }

  # Diagonal guides
  show_diagonal_guides <- isTRUE(style$show_diagonal_guides %||% FALSE)
  if (show_diagonal_guides) {
    diagonal_guide_size <- tb_num(style$diagonal_guide_size, 1)
    shapes <- c(shapes, list(
      list(type = "line", x0 = xlim[[1]], x1 = xlim[[2]], y0 = xlim[[1]], y1 = xlim[[2]],
           xref = "x", yref = "y", line = list(color = "gray60", width = diagonal_guide_size, dash = "dot")),
      list(type = "line", x0 = xlim[[1]], x1 = xlim[[2]], y0 = -xlim[[1]], y1 = -xlim[[2]],
           xref = "x", yref = "y", line = list(color = "gray60", width = diagonal_guide_size, dash = "dot"))
    ))
  }

  # Calculate aspect ratio
  x_span <- diff(xlim)
  y_span <- diff(ylim)

  # Create plotly
  p <- plotly::plot_ly(
    data = df_plot,
    x = ~score_x,
    y = ~score_y,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = ~color,
      size = ~pmin(n, 50) / 2 + 5,  # Size based on gene count
      opacity = alpha
    ),
    text = ~paste0(term, "\nScore X: ", round(score_x, 3), "\nScore Y: ", round(score_y, 3),
                   "\nFDR: ", format(fdr, scientific = TRUE, digits = 2), "\nGenes: ", n),
    hoverinfo = "text",
    source = paste0("gofcs_plotly_", plot_key)
  )

  p <- p %>%
    plotly::layout(
      xaxis = list(
        title = x_label,
        range = xlim,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        constrain = "domain"
      ),
      yaxis = list(
        title = y_label,
        range = ylim,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        scaleanchor = "x",
        scaleratio = y_span / x_span,
        constrain = "domain"
      ),
      shapes = shapes,
      annotations = annotations,
      showlegend = FALSE,
      dragmode = "pan",
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) %>%
    plotly::config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = FALSE
      ),
      displayModeBar = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
    )
  p <- plotly::event_register(p, "plotly_relayout")

  p
}

# Create summary cards as a separate ggplot to be stacked above the volcano plot
# Layout: Sig Down (blue) left, Info (grey) center, Sig Up (red) right
# Returns a ggplot object that can be combined with the main plot using patchwork
.tb_volcano_create_summary_cards <- function(comp, style, comparison_name = "",
                                              stat_mode_param = NULL, apply_fdr_param = NULL) {
  comparison <- comp$comparison %||% list()
  group_b <- comparison$group_b %||% "Treatment"
  group_a <- comparison$group_a %||% "Control"
  flip_fc <- isTRUE(style$flip_fc %||% FALSE)

  # Format title (swap group order when flip_fc is enabled)
  title <- if (nzchar(comparison_name)) {
    parts <- unlist(strsplit(comparison_name, "_vs_", fixed = TRUE))
    if (length(parts) == 2) {
      if (flip_fc) parts <- rev(parts)
      paste0(parts[1], " vs. ", parts[2])
    } else {
      comparison_name
    }
  } else {
    if (flip_fc) paste0(group_a, " vs. ", group_b) else paste0(group_b, " vs. ", group_a)
  }

  # Use passed params first, then comparison, then defaults
  stat_mode_raw <- stat_mode_param %||% comparison$stat_mode %||% "t-test"
  stat_mode <- if (identical(tolower(stat_mode_raw), "limma")) "Limma" else "t-test"
  apply_fdr <- apply_fdr_param %||% comparison$apply_fdr %||% TRUE
  apply_fdr_label <- if (isTRUE(apply_fdr)) "FDR Corr." else "No FDR Corr."

  n_up <- comparison$n_sig_up %||% 0
  n_down <- comparison$n_sig_down %||% 0
  n_unique_b <- comparison$n_unique_b %||% 0
  n_unique_a <- comparison$n_unique_a %||% 0

  # Swap up/down counts and group labels when flip_fc is enabled
  if (flip_fc) {
    tmp <- n_up; n_up <- n_down; n_down <- tmp
    tmp <- n_unique_b; n_unique_b <- n_unique_a; n_unique_a <- tmp
    tmp <- group_b; group_b <- group_a; group_a <- tmp
  }

  col_up <- style$col_sig_up %||% "#FF4242"
  col_down <- style$col_sig_down %||% "#4245FF"
  col_bg <- "#E8E8E8"

  # Use a fixed coordinate system for the summary cards (0-1 range)
  # Card proportions: left 26%, center 44%, right 26%, with 1% gaps
  card_gap <- 0.01

  # Left card bounds
  x_left_l <- 0
  x_right_l <- 0.26
  x_center_l <- (x_left_l + x_right_l) / 2

  # Right card bounds
  x_left_r <- 0.74
  x_right_r <- 1
  x_center_r <- (x_left_r + x_right_r) / 2

  # Center card bounds
  x_left_c <- x_right_l + card_gap
  x_right_c <- x_left_r - card_gap
  x_center_c <- (x_left_c + x_right_c) / 2

  # Y bounds (full height)
  y_bot <- 0
  y_top <- 1
  y_center <- 0.5

  # Text sizes - use style parameter with default, title slightly larger than body
  body_size <- style$summary_text_size %||% 3.5
  title_size <- body_size + 0.5

  # Create a minimal ggplot for the summary cards
  cards <- ggplot2::ggplot() +
    # Blue sig down card (LEFT)
    ggplot2::annotate("rect", xmin = x_left_l, xmax = x_right_l, ymin = y_bot, ymax = y_top,
                      fill = col_down, color = NA) +
    ggplot2::annotate("text", x = x_center_l, y = y_center + 0.18,
                      label = paste0("Sig. Down: ", n_down),
                      size = body_size, color = "white", fontface = "bold", hjust = 0.5) +
    ggplot2::annotate("text", x = x_center_l, y = y_center - 0.18,
                      label = paste0(group_a, " Specific: ", n_unique_a),
                      size = body_size * 0.85, color = "white", hjust = 0.5) +
    # Grey info card (CENTER)
    ggplot2::annotate("rect", xmin = x_left_c, xmax = x_right_c, ymin = y_bot, ymax = y_top,
                      fill = col_bg, color = NA) +
    ggplot2::annotate("text", x = x_center_c, y = y_center + 0.18,
                      label = title, size = title_size, fontface = "bold", hjust = 0.5) +
    ggplot2::annotate("text", x = x_center_c, y = y_center - 0.18,
                      label = paste0(stat_mode, " | ", apply_fdr_label),
                      size = body_size, hjust = 0.5) +
    # Red sig up card (RIGHT)
    ggplot2::annotate("rect", xmin = x_left_r, xmax = x_right_r, ymin = y_bot, ymax = y_top,
                      fill = col_up, color = NA) +
    ggplot2::annotate("text", x = x_center_r, y = y_center + 0.18,
                      label = paste0("Sig. Up: ", n_up),
                      size = body_size, color = "white", fontface = "bold", hjust = 0.5) +
    ggplot2::annotate("text", x = x_center_r, y = y_center - 0.18,
                      label = paste0(group_b, " Specific: ", n_unique_b),
                      size = body_size * 0.85, color = "white", hjust = 0.5) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 2, r = 5, b = 2, l = 5, unit = "pt"))

  cards
}

# Combine summary cards with volcano plot using patchwork
.tb_volcano_add_summary_cards <- function(p, comp, style, comparison_name = "",
                                          stat_mode_param = NULL, apply_fdr_param = NULL,
                                          xlim = NULL, ylim = NULL) {
  tb_require_pkg("patchwork")

  # Create the summary cards as a separate plot

  cards <- .tb_volcano_create_summary_cards(comp, style, comparison_name,
                                            stat_mode_param, apply_fdr_param)

  # Stack cards above plot: cards take ~8% of height, plot takes ~92%
  combined <- patchwork::wrap_plots(cards, p, ncol = 1, heights = c(0.08, 0.92))

  combined
}

tb_render_volcano <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  unique_label <- "Group Specific Protein"

  view_mode <- tolower(as.character(style$view_mode %||% "export_preview"))
  # Show summary info as subtitle when enabled (default TRUE) and not in interactive mode
  show_summary <- isTRUE(style$show_summary_cards %||% TRUE) &&
    !identical(view_mode, "interactive")

  # Check for multiple comparisons structure
  comparisons <- results$data$comparisons

  if (!is.null(comparisons) && is.list(comparisons) && length(comparisons) > 0) {
    # Multiple comparisons - render each as a separate plot
    plots <- list()
    tables <- list()

    for (comp_name in names(comparisons)) {
      comp <- comparisons[[comp_name]]
      df <- comp$points
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        # FIX: Pass apply_fdr from comparison so plot uses correct p-value column
        comp_apply_fdr <- comp$comparison$apply_fdr %||% results$params$apply_fdr %||% TRUE
        # FIX: Pass compute-time thresholds from comparison metadata for correct coloring
        comp_fc_threshold <- comp$comparison$fc_threshold %||% results$params$fc_threshold
        comp_p_threshold <- comp$comparison$sig_threshold %||% results$params$p_threshold
        p <- .tb_volcano_single_plot(df, style, meta, comp_name, apply_fdr = comp_apply_fdr,
                                     fc_threshold = comp_fc_threshold, p_threshold = comp_p_threshold)

        # Extract stat_mode from results$params (not in comparison object)
        comp_stat_mode <- results$params$stat_mode %||% "t-test"

        # Add summary cards if enabled
        if (isTRUE(show_summary)) {
          plot_xlim <- attr(p, "xlim") %||% c(-6, 6)
          plot_ylim <- attr(p, "ylim") %||% c(0, 7)
          p <- .tb_volcano_add_summary_cards(p, comp, style, comparison_name = comp_name,
                                             stat_mode_param = comp_stat_mode, apply_fdr_param = comp_apply_fdr,
                                             xlim = plot_xlim, ylim = plot_ylim)
        }

        # Build table for reference (shown when summary is disabled)
        n_up <- comp$comparison$n_sig_up %||% 0
        n_down <- comp$comparison$n_sig_down %||% 0
        n_unique_a <- comp$comparison$n_unique_a %||% 0
        n_unique_b <- comp$comparison$n_unique_b %||% 0

        stat_mode_raw <- results$params$stat_mode %||% "t-test"
        apply_fdr_raw <- comp$comparison$apply_fdr %||% results$params$apply_fdr %||% TRUE
        stat_mode_display <- if (identical(stat_mode_raw, "limma")) "limma" else "t-test"
        apply_fdr_display <- if (isTRUE(apply_fdr_raw)) "Yes" else "No"

        comp_table <- data.frame(
          Comparison = comp_name,
          `Stat Mode` = stat_mode_display,
          `Apply FDR` = apply_fdr_display,
          `Sig. Up` = n_up,
          `Sig. Down` = n_down,
          `Unique in Group A` = n_unique_a,
          `Unique in Group B` = n_unique_b,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        if (!is.null(comp$comparison)) {
          names(comp_table)[names(comp_table) == "Unique in Group A"] <- paste0(unique_label, " (", comp$comparison$group_a, ")")
          names(comp_table)[names(comp_table) == "Unique in Group B"] <- paste0(unique_label, " (", comp$comparison$group_b, ")")
        }

        tables[[comp_name]] <- comp_table
        plots[[comp_name]] <- p
      }
    }

    # Also include summary table (accessible as "volcano_summary") when summary subtitle is disabled
    if (!isTRUE(show_summary)) {
      summary_df <- results$data$summary %||% NULL
      if (!is.null(summary_df) && is.data.frame(summary_df) && ncol(summary_df) >= 6) {
        col5 <- names(summary_df)[5]
        col6 <- names(summary_df)[6]
        grp_a <- sub("^unique_", "", col5)
        grp_b <- sub("^unique_", "", col6)
        names(summary_df)[5] <- paste0(unique_label, " (", grp_a, ")")
        names(summary_df)[6] <- paste0(unique_label, " (", grp_b, ")")
      }
      tables[["volcano_summary"]] <- summary_df
    } else {
      tables <- list()
    }

    if (length(plots) == 0) {
      stop("volcano results$data$comparisons has no valid data.")
    }

    return(list(plots = plots, tables = tables))
  }

  # Fallback: single comparison (backward compatibility)
  df <- results$data$points
  if (is.null(df) || !is.data.frame(df)) stop("volcano results$data$points missing.")

  # FIX: Pass apply_fdr and compute-time thresholds for single-comparison case
  single_apply_fdr <- results$params$apply_fdr %||% TRUE
  single_fc_threshold <- results$params$fc_threshold
  single_p_threshold <- results$params$p_threshold
  p <- .tb_volcano_single_plot(df, style, meta, apply_fdr = single_apply_fdr,
                               fc_threshold = single_fc_threshold, p_threshold = single_p_threshold)

  # Add summary cards if enabled
  if (isTRUE(show_summary)) {
    comp <- list(comparison = results$data$comparison %||% list())
    single_stat_mode <- results$params$stat_mode %||% "t-test"
    plot_xlim <- attr(p, "xlim") %||% c(-6, 6)
    plot_ylim <- attr(p, "ylim") %||% c(0, 7)
    p <- .tb_volcano_add_summary_cards(p, comp, style, comparison_name = "",
                                       stat_mode_param = single_stat_mode, apply_fdr_param = single_apply_fdr,
                                       xlim = plot_xlim, ylim = plot_ylim)
    summary_df <- NULL
  } else {
    summary_df <- results$data$summary %||% NULL
    if (!is.null(summary_df) && is.data.frame(summary_df) && ncol(summary_df) >= 6) {
      col5 <- names(summary_df)[5]
      col6 <- names(summary_df)[6]
      grp_a <- sub("^unique_", "", col5)
      grp_b <- sub("^unique_", "", col6)
      names(summary_df)[5] <- paste0(unique_label, " (", grp_a, ")")
      names(summary_df)[6] <- paste0(unique_label, " (", grp_b, ")")
    }
  }

  list(
    plots = list(volcano_plot = p),
    tables = if (isTRUE(show_summary)) list() else list(volcano_summary = summary_df)
  )
}

# ---- MA Plot renderer ---------------------------------------------------------

.tb_ma_single_plot <- function(df, style, meta, comp_info, comp_name = "") {
  col_up    <- style$col_sig_up   %||% "#FF4242"
  col_down  <- style$col_sig_down %||% "#4245FF"
  col_ns    <- style$col_nonsig   %||% "#B0B0B0"
  point_sz  <- tb_num(style$point_size, 2)
  flip_y    <- isTRUE(style$flip_y_axis %||% FALSE)
  label_mode <- style$label_mode %||% "color_sig"

  plot_df <- df
  if (flip_y) plot_df$log2fc <- -plot_df$log2fc

  if (identical(label_mode, "hide_nonsig")) {
    plot_df <- plot_df[plot_df$significance %in% c("up", "down"), , drop = FALSE]
  }

  # Viewer-time A-axis transform: recompute from raw mean_a / mean_b if present.
  a_transform <- style$a_axis_transform %||% comp_info$a_axis_transform %||% "log2"
  mean_type   <- comp_info$mean_type %||% "arithmetic"
  if (all(c("mean_a", "mean_b") %in% names(plot_df))) {
    ma <- plot_df$mean_a
    mb <- plot_df$mean_b
    if (identical(mean_type, "harmonic")) {
      safe_a <- ifelse(ma > 0, ma, NA_real_)
      safe_b <- ifelse(mb > 0, mb, NA_real_)
      a_raw <- 2 / (1 / safe_a + 1 / safe_b)
    } else {
      a_raw <- (ma + mb) / 2
    }
    if (identical(a_transform, "log2")) {
      a_raw[!is.na(a_raw) & a_raw <= 0] <- NA_real_
      plot_df$a_value <- log2(a_raw)
    } else if (identical(a_transform, "log10")) {
      a_raw[!is.na(a_raw) & a_raw <= 0] <- NA_real_
      plot_df$a_value <- log10(a_raw)
    } else {
      plot_df$a_value <- a_raw
    }
    plot_df <- plot_df[is.finite(plot_df$a_value), , drop = FALSE]
  }

  color_map <- c(up = col_up, down = col_down, ns = col_ns)

  # Axis titles — subscript formatting for log2 / log10
  a_mean_label <- switch(mean_type, "harmonic" = "harmonic mean", "arithmetic mean")
  a_title <- if (nzchar(style$x_axis_title %||% "")) {
    style$x_axis_title
  } else if (identical(a_transform, "log2")) {
    bquote(log[2] * "(" * .(a_mean_label) * ")")
  } else if (identical(a_transform, "log10")) {
    bquote(log[10] * "(" * .(a_mean_label) * ")")
  } else {
    sprintf("%s abundance", tools::toTitleCase(a_mean_label))
  }

  grp_a <- comp_info$group_a %||% "A"
  grp_b <- comp_info$group_b %||% "B"
  # Y-axis title reflects flip state (always log2 fold change)
  y_title <- if (flip_y) {
    bquote(log[2] * "(" * .(grp_a) * " / " * .(grp_b) * ")")
  } else {
    bquote(log[2] * "(" * .(grp_b) * " / " * .(grp_a) * ")")
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = a_value, y = log2fc, color = significance)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#888888") +
    ggplot2::geom_point(size = point_sz, alpha = 0.8) +
    ggplot2::scale_color_manual(values = color_map,
                                breaks = c("up", "ns", "down"),
                                labels = c(up = "Up", ns = "NS", down = "Down"),
                                drop = FALSE,
                                guide = "none") +
    ggplot2::labs(
      title = if (nzchar(comp_name)) comp_name else NULL,
      x = a_title, y = y_title, color = NULL
    ) +
    tb_theme_base(tb_num(style$axis_text_size, 16),
                  axis_style = style$axis_style %||% "clean")

  # FC cutoff lines
  if (isTRUE(style$show_cut_lines %||% TRUE)) {
    fc_thr <- comp_info$fc_threshold %||% c(-1, 1)
    y_cuts <- if (flip_y) -rev(fc_thr) else fc_thr
    p <- p + ggplot2::geom_hline(yintercept = y_cuts, linetype = "dotted", color = "#666666")
  }

  # Axis ranges
  if (identical(style$x_range_mode %||% "auto", "manual")) {
    p <- p + ggplot2::coord_cartesian(
      xlim = c(tb_num(style$x_min, 0), tb_num(style$x_max, 12)),
      ylim = if (identical(style$y_range_mode %||% "auto", "manual"))
        c(tb_num(style$y_min, -5), tb_num(style$y_max, 5)) else NULL
    )
  } else if (identical(style$y_range_mode %||% "auto", "manual")) {
    p <- p + ggplot2::coord_cartesian(
      ylim = c(tb_num(style$y_min, -5), tb_num(style$y_max, 5))
    )
  }

  p
}

tb_render_ma_plot <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  comparisons <- results$data$comparisons
  if (is.null(comparisons) || length(comparisons) == 0) {
    return(list(
      plots = list(ma_plot = ggplot2::ggplot() + ggplot2::labs(title = "No MA plot data")),
      tables = list(ma_plot_summary = results$data$summary %||% data.frame())
    ))
  }

  plots <- list()
  for (comp_name in names(comparisons)) {
    comp <- comparisons[[comp_name]]
    df <- comp$points
    if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
      plots[[comp_name]] <- .tb_ma_single_plot(df, style, meta, comp$comparison, comp_name)
    }
  }

  if (length(plots) == 0) {
    plots$ma_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No valid points")
  }

  list(
    plots = plots,
    tables = list(ma_plot_summary = results$data$summary %||% data.frame())
  )
}

tb_render_goora <- function(results, style, meta) {
  # GO-ORA with optional BP/MF/CC tabs
  tb_require_pkg("ggplot2")

  data_obj <- results$data %||% list()

  # --- Ontology Filter (v1.5 migration) ---

  # style$ontology_filter takes precedence; fall back to params$ontology for legacy terpbooks
  ontology_filter <- style$ontology_filter %||% results$params$ontology %||% "all"

  # Auto-reset ontology filter for complex mode: if filter is a GO ontology but data is complex,
  # the filter would silently empty all rows — reset to "all" to show all complex sources
  if (!is.null(data_obj$terms) && is.data.frame(data_obj$terms) &&
      "ontology" %in% names(data_obj$terms) &&
      tb_detect_complex_mode(data_obj$terms) &&
      ontology_filter %in% c("BP", "MF", "CC")) {
    ontology_filter <- "all"
  }

  # Apply ontology filter to terms if column exists
  if (!identical(ontology_filter, "all") && !is.null(data_obj$terms) &&
      is.data.frame(data_obj$terms) && "ontology" %in% names(data_obj$terms)) {
    data_obj$terms <- data_obj$terms[grepl(ontology_filter, data_obj$terms$ontology, ignore.case = TRUE), , drop = FALSE]
  }
  # --- End ontology filter ---

  available_tabs <- character()

  # Check for explicit tab structure (data$BP, data$MF, data$CC, or complex sources)
  for (tab in c("BP", "MF", "CC", "CORUM", "ComplexPortal")) {
    if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
      available_tabs <- c(available_tabs, tab)
    }
  }

  # FIX: Check if terms has an 'ontology' column we can split on
  df <- data_obj$terms
  if (length(available_tabs) == 0 && !is.null(df) && is.data.frame(df) && "ontology" %in% names(df)) {
    # Detect if this is complex enrichment (CORUM/ComplexPortal) or GO enrichment (BP/MF/CC)
    is_complex_mode <- tb_detect_complex_mode(df)

    if (is_complex_mode) {
      # Split by complex source (CORUM, ComplexPortal)
      complex_sources <- tb_get_complex_sources(df)
      for (src in complex_sources) {
        src_df <- df[df$ontology == src, , drop = FALSE]
        if (nrow(src_df) > 0) {
          data_obj[[src]] <- list(terms = src_df)
          available_tabs <- c(available_tabs, src)
        }
      }
    } else {
      # Split terms by GO ontology column to create tabs
      ontologies_present <- unique(df$ontology)
      for (ont in c("BP", "MF", "CC")) {
        if (ont %in% ontologies_present) {
          ont_df <- df[df$ontology == ont, , drop = FALSE]
          if (nrow(ont_df) > 0) {
            data_obj[[ont]] <- list(terms = ont_df)
            available_tabs <- c(available_tabs, ont)
          }
        }
      }
    }
  }

  # If no tabs, use old single-dataset format
  if (length(available_tabs) == 0) {
    df <- data_obj$terms
    if (is.null(df) || !is.data.frame(df)) stop("goora results$data$terms missing.")

    if (!"term_id" %in% names(df)) {
      for (col in c("TermID", "termID", "go_id", "GO", "ID")) {
        if (col %in% names(df)) { df$term_id <- df[[col]]; break }
      }
    }

    # Normalize column names - use first available column
    if (!"term" %in% names(df)) {
      for (col in c("term_name", "Term", "pathway", "Pathway", "term_id")) {
        if (col %in% names(df)) { df$term <- df[[col]]; break }
      }
    }
    if (!"fdr" %in% names(df)) {
      for (col in c("FDR", "p.adjust", "padj", "pval")) {
        if (col %in% names(df)) { df$fdr <- df[[col]]; break }
      }
    }
    if (!"n" %in% names(df)) {
      for (col in c("n_genes", "count", "Count", "GeneCount", "size")) {
        if (col %in% names(df)) { df$n <- df[[col]]; break }
      }
      if (!"n" %in% names(df)) df$n <- 1
    }
    if (!"genes" %in% names(df)) {
      # FIX: Include protein_ids from GO-ORA engine output
      for (col in c("Genes", "gene_ids", "geneID", "protein_ids")) {
        if (col %in% names(df)) { df$genes <- df[[col]]; break }
      }
      if (!"genes" %in% names(df)) df$genes <- ""
    }
    # Normalize fold_enrichment column
    if (!"fold_enrichment" %in% names(df)) {
      for (col in c("FoldEnrichment", "foldEnrichment", "enrichmentRatio", "enrichment_ratio")) {
        if (col %in% names(df)) { df$fold_enrichment <- df[[col]]; break }
      }
      if (!"fold_enrichment" %in% names(df)) df$fold_enrichment <- 1
    }

    # Store original term name for lookups (before any modifications)
    df$term_original <- df$term

    # Keep full df for table (with all terms), filter only for plot
    df_all <- df
    hidden_terms <- character(0)
    term_labels <- list()
    if (!is.null(meta) && !is.null(meta$visibility)) {
      hidden_terms <- meta$visibility$hidden_terms %||% character(0)
      term_labels <- meta$visibility$term_labels %||% list()
    }
    # Filter by term_id if available, otherwise fall back to term_original
    if ("term_id" %in% names(df_all)) {
      df_plot <- df_all[!(df_all$term_id %in% hidden_terms) & !(df_all$term_original %in% hidden_terms), , drop = FALSE]
    } else {
      df_plot <- df_all[!(df_all$term_original %in% hidden_terms), , drop = FALSE]
    }

    # FIX: Apply custom term labels FIRST (before GO ID suffix)
    # Labels can be keyed by term_id or term_original
    if (length(term_labels) > 0 && nrow(df_plot) > 0) {
      for (i in seq_len(nrow(df_plot))) {
        # Try term_id first, then term_original
        tid <- if ("term_id" %in% names(df_plot)) df_plot$term_id[i] else NULL
        orig <- df_plot$term_original[i]
        custom_label <- NULL
        if (!is.null(tid) && !is.null(term_labels[[tid]]) && nzchar(term_labels[[tid]])) {
          custom_label <- term_labels[[tid]]
        } else if (!is.null(term_labels[[orig]]) && nzchar(term_labels[[orig]])) {
          custom_label <- term_labels[[orig]]
        }
        if (!is.null(custom_label)) {
          df_plot$term[i] <- custom_label
        }
      }
    }

    # FIX: Apply GO ID label formatting AFTER custom labels
    show_go_id <- isTRUE(style$show_go_id %||% FALSE)
    if (show_go_id && "term_id" %in% names(df_plot) && nrow(df_plot) > 0) {
      for (i in seq_len(nrow(df_plot))) {
        term_name <- df_plot$term[i]
        term_id <- df_plot$term_id[i]
        if (!is.na(term_id) && grepl("^GO:", term_id) && !grepl(term_id, term_name, fixed = TRUE)) {
          df_plot$term[i] <- paste0(term_name, " (", term_id, ")")
        }
      }
    }

    if (nrow(df_all) == 0) {
      return(list(plots = list(), tables = list()))
    }

    plot_type <- style$plot_type %||% "bar"
    axis_text_size <- tb_num(style$axis_text_size, 20)
    font_size <- tb_num(style$font_size, 14)
    alpha <- tb_num(style$alpha, 0.8)
    fdr_palette <- style$fdr_palette %||% "yellow_cap"
    flip_axis <- isTRUE(style$flip_axis %||% FALSE)
    x_axis_metric <- style$x_axis_metric %||% "neglog10_fdr"

    # Color mode
    cm <- style$color_mode %||% "fdr"
    use_flat_color <- (cm == "flat")
    flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

    # Generate plot only if there are visible terms
    p <- NULL
    if (nrow(df_plot) > 0) {
      # FIX: Use term_id as unique identifier to prevent bar merging when display names are same
      if ("term_id" %in% names(df_plot)) {
        df_plot$term_key <- df_plot$term_id
      } else {
        df_plot$term_key <- paste0("term_", seq_len(nrow(df_plot)))
      }

      # Compute x-axis value based on selected metric
      if (x_axis_metric == "neglog10_fdr") {
        df_plot$x_value <- -log10(pmax(df_plot$fdr, 1e-100))
        x_label <- "-log10(FDR)"
      } else {
        df_plot$x_value <- df_plot$fold_enrichment
        x_label <- "Fold Enrichment"
      }

      # Order by x_value (highest at top) for plotting
      ordered_keys <- unique(df_plot$term_key[order(df_plot$x_value)])
      df_plot$term_key <- factor(df_plot$term_key, levels = ordered_keys)

      # Create lookup for display labels (term_key -> term display name)
      term_display_labels <- stats::setNames(df_plot$term, df_plot$term_key)

      # Y-axis scale with display labels
      y_scale <- ggplot2::scale_y_discrete(
        labels = term_display_labels,
        position = if (flip_axis) "right" else "left"
      )

      x_scale <- if (flip_axis) {
        ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = c(0, 0.05)))
      } else {
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
      }

      if (plot_type == "dot") {
        if (use_flat_color) {
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
            ggplot2::geom_point(ggplot2::aes(size = n), color = flat_color, alpha = alpha) +
            ggplot2::scale_size_continuous(name = "# Genes", range = c(2, 10)) +
            y_scale + x_scale
        } else {
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
            ggplot2::geom_point(ggplot2::aes(size = n, color = fdr), alpha = alpha) +
            tb_fdr_scale("color", df_plot$fdr, palette = fdr_palette) +
            ggplot2::scale_size_continuous(name = "# Genes", range = c(2, 10)) +
            y_scale + x_scale
        }
      } else {
        if (use_flat_color) {
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
            ggplot2::geom_col(fill = flat_color, alpha = alpha) +
            x_scale + y_scale
        } else {
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
            ggplot2::geom_col(ggplot2::aes(fill = fdr), alpha = alpha) +
            tb_fdr_scale("fill", df_plot$fdr, palette = fdr_palette) +
            x_scale + y_scale
        }
      }

      # Query count subtitle
      show_query_count <- isTRUE(style$show_query_count %||% TRUE)
      n_query <- results$data$query_info$n_query %||% NA_integer_
      subtitle_text <- if (show_query_count && is.finite(as.numeric(n_query))) {
        sprintf("n = %d", as.integer(n_query))
      } else NULL

      # FIX: font_size now applies to term labels AND axis titles
      p <- p +
        ggplot2::labs(x = x_label, y = NULL, subtitle = subtitle_text) +
        tb_theme_base(axis_text_size, axis_style = style$axis_style %||% "clean") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = font_size),
          axis.title.x = ggplot2::element_text(size = font_size),
          legend.position = if (flip_axis) "left" else "right"
        )
    }

    df_display <- df_all
    if ("fdr" %in% names(df_display)) df_display$fdr <- tb_format_fdr(df_display$fdr)
    if ("fold_enrichment" %in% names(df_display)) df_display$fold_enrichment <- tb_format_sig(df_display$fold_enrichment, 2)
    if ("n" %in% names(df_display)) df_display$n <- suppressWarnings(as.integer(df_display$n))

    # Return ALL terms in table (for reversible hide/show)
    return(list(plots = list(goora_plot = p), tables = list(goora_table = df_display)))
  }

  # Multi-tab structure
  plot_type <- style$plot_type %||% "bar"
  all_plots <- list()
  all_tables <- list()
  multi_n_query <- results$data$query_info$n_query %||% NA_integer_

  for (tab in available_tabs) {
    tab_data <- data_obj[[tab]]
    rendered <- tb_render_go_tab(tab, tab_data, style, plot_type, meta,
                                 n_query = multi_n_query)

    for (pn in names(rendered$plots)) {
      all_plots[[pn]] <- rendered$plots[[pn]]
    }
    for (tn in names(rendered$tables)) {
      all_tables[[tn]] <- rendered$tables[[tn]]
    }
  }

  list(
    plots = all_plots,
    tables = all_tables,
    tabs = available_tabs
  )
}

tb_render_subloc <- function(results, style, meta) {

  tb_require_pkg("ggplot2")

  df <- results$data$points
  if (is.null(df) || !is.data.frame(df)) stop("subloc results$data$points missing.")

  axis_text_size <- tb_num(style$axis_text_size, 20)
  ylab_base <- style$y_axis_title %||% "Intensity"
  alpha <- tb_num(style$alpha, 0.3)
  label_rotation <- as.numeric(style$label_rotation %||% "0")

  # Apply log axis annotation if log transform was used
  log_transform <- results$params$log_transform %||% "none"
  ylab <- tb_log_axis_label(ylab_base, log_transform)

  cm <- style$color_mode %||% "group"
  flat_color <- style$flat_color %||% "#B0B0B0"

  df$bin   <- df$bin   %||% df$Bin   %||% df$Location %||% df$loc %||% df$Loc
  df$group <- df$group %||% df$Group %||% df$SampleGroup %||% df$sample_group
  if (is.null(df$bin) || is.null(df$group)) {
    stop("subloc points require 'bin' and 'group' columns.")
  }

  # Store all bin levels before filtering
  all_bins <- unique(as.character(df$bin))

  # Filter hidden bins based on visibility settings (reversible hide/show)
  hidden_bins <- character(0)
  if (!is.null(meta) && !is.null(meta$visibility) && !is.null(meta$visibility$hidden_bins)) {
    hidden_bins <- meta$visibility$hidden_bins
    df <- df[!df$bin %in% hidden_bins, , drop = FALSE]
  }

  df$bin   <- factor(df$bin)
  df$group <- factor(df$group)

  # Build group_colors AFTER factoring so levels() returns the group names.
  # Upstream (stats/subloc.R) populates results$data$group_colors as a named
  # character vector keyed by group; fall back to a default palette if absent
  # or if levels don't match the stored names.
  group_colors <- results$data$group_colors %||% NULL
  if (is.list(group_colors)) group_colors <- unlist(group_colors)
  group_levels <- levels(df$group)
  needs_default <- is.null(group_colors) ||
    length(group_colors) == 0 ||
    !all(group_levels %in% names(group_colors))
  if (needs_default) {
    group_colors <- scales::hue_pal()(length(group_levels))
    names(group_colors) <- group_levels
  }
  group_colors <- group_colors[group_levels]

  dodge_width <- 0.8
  dodge <- ggplot2::position_dodge2(width = dodge_width, preserve = "single")

  # Compute point size from boxplot width:
  # After dodging, each group's boxplot width is approximately dodge_width / n_groups_per_bin
  # Use 0.8 of that as the point "circumference" proxy -> convert to ggplot size units
  # ggplot size is diameter in mm; using a factor to make points visually appropriate
  n_groups <- nlevels(df$group)
  effective_box_width <- dodge_width / max(n_groups, 1)
  # Point size: 0.8 * effective_box_width scaled to reasonable ggplot size (1-3 range)
  point_size <- suppressWarnings(as.numeric(style$point_size %||% NA_real_)[1])
  if (!is.finite(point_size) || point_size <= 0) {
    point_size <- max(0.5, min(2.5, effective_box_width * 0.8 * 3))
  }


  # Compute outliers only (points beyond 1.5*IQR from Q1/Q3 fences - standard Tukey boxplot)
  df_outliers <- do.call(rbind, lapply(split(df, interaction(df$bin, df$group, drop = TRUE)), function(sub) {
    if (nrow(sub) < 4) return(sub[FALSE, , drop = FALSE])  # Need enough data for quartiles
    q <- stats::quantile(sub$value, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lower_fence <- q[1] - 1.5 * iqr
    upper_fence <- q[2] + 1.5 * iqr
    sub[sub$value < lower_fence | sub$value > upper_fence, , drop = FALSE]
  }))

  # Key fix: use fill=group so boxplots are computed per (bin, group) and dodged correctly.
  # Show only outlier points beyond the whisker range (not all points)
  if (cm == "flat") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = bin, y = value)) +
      ggplot2::geom_boxplot(alpha = alpha, position = dodge, outlier.shape = NA,
                            fill = flat_color, color = flat_color)
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = bin, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = alpha, position = dodge, outlier.shape = NA)
  }

  # Add outlier points only if there are any
  if (!is.null(df_outliers) && nrow(df_outliers) > 0) {
    if (cm == "flat") {
      p <- p + ggplot2::geom_jitter(
        data = df_outliers,
        position = ggplot2::position_jitterdodge(jitter.width = effective_box_width * 0.3, dodge.width = dodge_width),
        size = point_size,
        alpha = 0.6,
        shape = 16,
        color = flat_color
      )
    } else {
      p <- p + ggplot2::geom_jitter(
        data = df_outliers,
        ggplot2::aes(group = group, color = group),
        position = ggplot2::position_jitterdodge(jitter.width = effective_box_width * 0.3, dodge.width = dodge_width),
        size = point_size,
        alpha = 0.6,
        shape = 16
      )
    }
  }
  if (cm == "flat") {
    vals <- stats::setNames(rep(flat_color, nlevels(df$group)), levels(df$group))
    p <- p +
      ggplot2::scale_fill_manual(values = vals, guide = "none") +
      ggplot2::scale_color_manual(values = vals, guide = "none")
  } else {
    # Single scale_fill_manual with values AND legend guide — avoids the earlier
    # bug where a second scale call without values overrode the first.
    p <- p +
      ggplot2::scale_fill_manual(values = group_colors, drop = FALSE,
                                 guide = ggplot2::guide_legend(title = NULL)) +
      ggplot2::scale_color_manual(values = group_colors, drop = FALSE,
                                  guide = ggplot2::guide_legend(title = NULL))
  }

  # Apply label rotation for x-axis (bin/category labels)
  rotation_angle <- label_rotation
  rotation_hjust <- if (rotation_angle == 45) 1 else if (rotation_angle == 90) 1 else 0.5
  rotation_vjust <- if (rotation_angle == 45) 1 else if (rotation_angle == 90) 0.5 else 1

  p <- p +
    ggplot2::labs(x = NULL, y = ylab) +
    tb_theme_base(axis_text_size, axis_style = style$axis_style %||% "clean") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = rotation_angle,
        hjust = rotation_hjust,
        vjust = rotation_vjust
      )
    )

  # FIX: Add global mean reference line when show_global_mean is TRUE (style option)
  # FIX: Use customizable color and thickness from style options
  show_global_mean <- isTRUE(style$show_global_mean %||% FALSE)
  if (show_global_mean && nrow(df) > 0) {
    global_mean <- mean(df$value, na.rm = TRUE)
    if (is.finite(global_mean)) {
      mean_line_color <- style$global_mean_color %||% "#FF0000"
      mean_line_size <- tb_num(style$global_mean_size, 1)
      # Add horizontal line for global mean
      p <- p + ggplot2::geom_hline(yintercept = global_mean, linetype = "dashed",
                                   color = mean_line_color, linewidth = mean_line_size, alpha = 0.9)
    }
  }

  # Apply value-axis range limits if in manual mode
  y_range_mode <- style$y_range_mode %||% "auto"
  if (y_range_mode == "manual") {
    y_min <- style$y_min %||% 0
    y_max <- style$y_max %||% 12
    p <- p + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
  }


  # Format counts table with categories as columns
  # Row 1: protein counts, Row 2: show/hide checkboxes
  counts_df <- results$data$counts
  if (!is.null(counts_df) && is.data.frame(counts_df) && nrow(counts_df) > 0) {
    # Use all_bins to ensure hidden bins still appear in table for re-enabling
    bin_names <- counts_df$bin
    bin_counts <- counts_df$count

    # Row 1: Protein counts
    row1 <- c("Proteins", as.character(bin_counts))
    names(row1) <- c("Metric", bin_names)

    # Row 2: Show/Hide checkboxes (checked = visible, unchecked = hidden)
    # Generate HTML checkboxes for each bin
    row2_checks <- vapply(bin_names, function(b) {
      is_visible <- !(b %in% hidden_bins)
      checked_attr <- if (is_visible) " checked" else ""
      safe_bin <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", b)))))
      sprintf(
        '<input type="checkbox" class="subloc-bin-toggle-cb" data-bin="%s"%s title="%s" />',
        safe_bin, checked_attr, if (is_visible) "Click to hide" else "Click to show"
      )
    }, character(1))
    row2 <- c("Show", row2_checks)
    names(row2) <- c("Metric", bin_names)

    # Combine into data frame
    counts_table <- data.frame(
      rbind(row1, row2),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    rownames(counts_table) <- NULL
  } else {
    counts_table <- NULL
  }

  list(
    plots  = list(subloc_plot = p),
    tables = list(subloc_counts = counts_table),
    all_bins = all_bins  # Return all bins for potential use by UI
  )
}

# ---- GO/Complex enrichment with BP/MF/CC or CORUM/ComplexPortal tabs ----------------------------------------

tb_render_go_tab <- function(tab_name, tab_data, style, plot_type = "bar", meta = NULL,
                             is_fcs = FALSE, score_label = NULL, n_query = NA_integer_) {
  # Render a single enrichment tab (GO: BP/MF/CC or Complex: CORUM/ComplexPortal)
  # is_fcs: if TRUE, force score axis to [-1, 1] with specific tick labels
  # score_label: custom x-axis label (e.g., "PC1", "log2(BafA1/Control)"), NULL uses default
  tb_require_pkg("ggplot2")

  df <- tab_data$terms %||% tab_data$data %||% tab_data
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(plots = list(), tables = list()))
  }

  # Detect complex mode (CORUM/ComplexPortal) for label adjustments
  is_complex_mode <- tb_detect_complex_mode(df)

  # Normalize column names - use first available column
  if (!"term_id" %in% names(df)) {
    for (col in c("TermID", "termID", "go_id", "GO", "ID", "complex_id")) {
      if (col %in% names(df)) { df$term_id <- df[[col]]; break }
    }
  }
  if (!"term" %in% names(df)) {
    for (col in c("term_name", "Term", "pathway", "Pathway", "term_id")) {
      if (col %in% names(df)) { df$term <- df[[col]]; break }
    }
  }
  if (!"fdr" %in% names(df)) {
    for (col in c("FDR", "p.adjust", "padj", "pval")) {
      if (col %in% names(df)) { df$fdr <- df[[col]]; break }
    }
  }
  if (!"n" %in% names(df)) {
    for (col in c("n_genes", "count", "Count", "GeneCount", "size")) {
      if (col %in% names(df)) { df$n <- df[[col]]; break }
    }
    if (!"n" %in% names(df)) df$n <- 1
  }
  if (!"genes" %in% names(df)) {
    # FIX: Include protein_ids from GO-ORA engine output
    for (col in c("Genes", "gene_ids", "geneID", "protein_ids")) {
      if (col %in% names(df)) { df$genes <- df[[col]]; break }
    }
    if (!"genes" %in% names(df)) df$genes <- ""
  }
  # Normalize fold_enrichment column - FIX: also look for 'score' column for 1D-GOFCS
  if (!"fold_enrichment" %in% names(df)) {
    for (col in c("FoldEnrichment", "foldEnrichment", "enrichmentRatio", "enrichment_ratio", "score")) {
      if (col %in% names(df)) { df$fold_enrichment <- df[[col]]; break }
    }
    if (!"fold_enrichment" %in% names(df)) df$fold_enrichment <- 1
  }

  # Flip FC direction if enabled (per-plot, viewer-only for 1dgofcs)
  # This negates score values to swap enrichment direction
  flip_fc <- isTRUE(style$flip_fc %||% FALSE)
  if (flip_fc && !is.null(df$fold_enrichment)) {
    df$fold_enrichment <- -df$fold_enrichment
  }

  # Store original term name for lookups (before any modifications)
  df$term_original <- df$term

  # Keep full df for table (with all terms), filter only for plot
  df_all <- df
  hidden_terms <- character(0)
  term_labels <- list()
  if (!is.null(meta) && !is.null(meta$visibility)) {
    hidden_terms <- meta$visibility$hidden_terms %||% character(0)
    term_labels <- meta$visibility$term_labels %||% list()
  }
  # Filter by term_id if available, otherwise fall back to term_original
  if ("term_id" %in% names(df_all)) {
    df_plot <- df_all[!(df_all$term_id %in% hidden_terms) & !(df_all$term_original %in% hidden_terms), , drop = FALSE]
  } else {
    df_plot <- df_all[!(df_all$term_original %in% hidden_terms), , drop = FALSE]
  }

  # FIX: Apply custom term labels FIRST (before GO ID suffix)
  # Labels can be keyed by term_id or term_original
  if (length(term_labels) > 0 && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      # Try term_id first, then term_original
      tid <- if ("term_id" %in% names(df_plot)) df_plot$term_id[i] else NULL
      orig <- df_plot$term_original[i]
      custom_label <- NULL
      if (!is.null(tid) && !is.null(term_labels[[tid]]) && nzchar(term_labels[[tid]])) {
        custom_label <- term_labels[[tid]]
      } else if (!is.null(term_labels[[orig]]) && nzchar(term_labels[[orig]])) {
        custom_label <- term_labels[[orig]]
      }
      if (!is.null(custom_label)) {
        df_plot$term[i] <- custom_label
      }
    }
  }

  # FIX: Apply GO/Complex ID label formatting AFTER custom labels
  # This appends GO ID or complex ID to whatever the current label is (original or custom)
  show_go_id <- isTRUE(style$show_go_id %||% FALSE)
  if (show_go_id && "term_id" %in% names(df_plot) && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      term_name <- df_plot$term[i]
      term_id <- df_plot$term_id[i]
      # Append if term_id looks like a GO ID or complex ID and isn't already in the name
      if (!is.na(term_id) && grepl("^(GO:|CORUM_|CPX_)", term_id) && !grepl(term_id, term_name, fixed = TRUE)) {
        df_plot$term[i] <- paste0(term_name, " (", term_id, ")")
      }
    }
  }

  if (nrow(df_all) == 0) {
    return(list(plots = list(), tables = list()))
  }

  # Size legend label: "# Subunits" for complexes, "# Genes" for GO

  size_legend_name <- if (is_complex_mode) "# Subunits" else "# Genes"

  # Extract style parameters
  axis_text_size <- tb_num(style$axis_text_size, 20)
  font_size <- tb_num(style$font_size, 14)
  alpha <- tb_num(style$alpha, 0.8)
  fdr_palette <- style$fdr_palette %||% "yellow_cap"
  flip_axis <- isTRUE(style$flip_axis %||% FALSE)
  x_axis_metric <- style$x_axis_metric %||% "neglog10_fdr"

  # Color mode
  cm <- style$color_mode %||% "fdr"
  use_flat_color <- (cm == "flat")
  flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

  # Generate plot only if there are visible terms
  p <- NULL
  if (nrow(df_plot) > 0) {
    # FIX: Use term_id as unique identifier to prevent bar merging when display names are same
    # Create a unique key column - prefer term_id, fall back to row index if missing
    if ("term_id" %in% names(df_plot)) {
      df_plot$term_key <- df_plot$term_id
    } else {
      df_plot$term_key <- paste0("term_", seq_len(nrow(df_plot)))
    }

    # For non-FCS (GO-ORA): support x_axis_metric toggle between fold_enrichment and -log10(FDR)
    # For FCS engines: always use fold_enrichment (which contains the score)
    if (!is_fcs && x_axis_metric == "neglog10_fdr") {
      df_plot$x_value <- -log10(pmax(df_plot$fdr, 1e-100))
      ora_x_label <- "-log10(FDR)"
    } else {
      df_plot$x_value <- df_plot$fold_enrichment
      ora_x_label <- "Fold Enrichment"
    }

    # Order by x_value (highest at top) - use absolute value for 1D-GOFCS scores
    ordered_keys <- unique(df_plot$term_key[order(abs(df_plot$x_value))])
    df_plot$term_key <- factor(df_plot$term_key, levels = ordered_keys)

    # Create lookup for display labels (term_key -> term display name)
    term_display_labels <- stats::setNames(df_plot$term, df_plot$term_key)

    # FCS mode: fixed [-1, 1] range with specific tick labels
    fcs_x_scale <- if (is_fcs) {
      if (flip_axis) {
        ggplot2::scale_x_reverse(
          limits = c(1, -1),
          breaks = c(1, 0.5, 0, -0.5, -1),
          labels = c("1", "0.5", "0", "-0.5", "-1"),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        )
      } else {
        ggplot2::scale_x_continuous(
          limits = c(-1, 1),
          breaks = c(-1, -0.5, 0, 0.5, 1),
          labels = c("-1", "-0.5", "0", "0.5", "1"),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        )
      }
    } else {
      if (flip_axis) {
        ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = c(0, 0.05)))
      } else {
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
      }
    }

    # Y-axis scale with display labels (maps term_key to term display name)
    y_scale <- ggplot2::scale_y_discrete(
      labels = term_display_labels,
      position = if (flip_axis) "right" else "left"
    )

    # Create plot using x_value as x-axis, term_key as y-axis
    if (plot_type == "dot") {
      if (use_flat_color) {
        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
          ggplot2::geom_point(ggplot2::aes(size = n), color = flat_color, alpha = alpha) +
          ggplot2::scale_size_continuous(name = size_legend_name, range = c(2, 10)) +
          fcs_x_scale + y_scale
      } else {
        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
          ggplot2::geom_point(ggplot2::aes(size = n, color = fdr), alpha = alpha) +
          tb_fdr_scale("color", df_plot$fdr, palette = fdr_palette) +
          ggplot2::scale_size_continuous(name = size_legend_name, range = c(2, 10)) +
          fcs_x_scale + y_scale
      }
    } else {
      if (use_flat_color) {
        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
          ggplot2::geom_col(fill = flat_color, alpha = alpha) +
          fcs_x_scale + y_scale
      } else {
        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = term_key)) +
          ggplot2::geom_col(ggplot2::aes(fill = fdr), alpha = alpha) +
          tb_fdr_scale("fill", df_plot$fdr, palette = fdr_palette) +
          fcs_x_scale + y_scale
      }
    }

    # FIX: font_size now applies to term labels AND axis titles
    # FIX: Use score_label if provided, else "Score" for FCS, ora_x_label for ORA
    x_axis_label <- if (!is.null(score_label) && nzchar(score_label)) {
      score_label
    } else if (is_fcs) {
      "Score"
    } else {
      ora_x_label
    }
    # Query count subtitle (ORA only; FCS uses full ranked list)
    show_query_count <- isTRUE(style$show_query_count %||% TRUE)
    subtitle_text <- if (!is_fcs && show_query_count && is.finite(as.numeric(n_query))) {
      sprintf("n = %d", as.integer(n_query))
    } else NULL
    p <- p +
      ggplot2::labs(x = x_axis_label, y = NULL, subtitle = subtitle_text) +
      tb_theme_base(axis_text_size, axis_style = style$axis_style %||% "clean") +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = font_size),
        axis.title.x = ggplot2::element_text(size = font_size),
        legend.position = if (flip_axis) "left" else "right"
      )
  }

  plot_name <- paste0(tolower(tab_name), "_plot")
  table_name <- paste0(tolower(tab_name), "_table")

  # Format display columns - use ALL terms (including hidden) for reversible hide/show
  df_display <- df_all
  if ("fdr" %in% names(df_display)) {
    df_display$fdr <- tb_format_fdr(df_display$fdr)
  }
  if ("fold_enrichment" %in% names(df_display)) {
    df_display$fold_enrichment <- tb_format_sig(df_display$fold_enrichment, 2)
  }
  if ("n" %in% names(df_display)) {
    df_display$n <- suppressWarnings(as.integer(df_display$n))
  }

  # Update column names for complex mode to show more meaningful labels
  # This affects table display - columns like "ontology" become "Source" for complexes
  if (is_complex_mode) {
    col_renames <- c(
      ontology = "Source",       # CORUM/ComplexPortal instead of BP/MF/CC
      n = "Subunits"             # Subunits in query instead of # genes
    )
    for (old_name in names(col_renames)) {
      if (old_name %in% names(df_display)) {
        names(df_display)[names(df_display) == old_name] <- col_renames[[old_name]]
      }
    }
  }

  list(
    plots = stats::setNames(list(p), plot_name),
    tables = stats::setNames(list(df_display), table_name),
    is_complex_mode = is_complex_mode  # Pass to UI for filter options
  )
}

tb_render_idquant <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  df <- results$data$counts
  if (is.null(df) || !is.data.frame(df)) stop("idquant results$data$counts missing.")

  # Normalize column names - engine produces "metric", renderer expects "level"
  if ("metric" %in% names(df) && !"level" %in% names(df)) {
    df$level <- df$metric
  }

  df$group <- factor(df$group)
  df$level <- factor(df$level %||% df$metric %||% "count")

  label_identified <- "Quantified"
  label_quantified <- "Reproducibly Quantified"

  level_vals <- levels(df$level)
  level_labels <- vapply(level_vals, function(lv) {
    if (grepl("quant", lv, ignore.case = TRUE)) return(label_quantified)
    if (grepl("ident", lv, ignore.case = TRUE)) return(label_identified)
    lv
  }, character(1))
  names(level_labels) <- level_vals
  df$level <- factor(level_labels[as.character(df$level)], levels = unique(level_labels))

  view_mode <- style$view_mode %||% "combined"
  # Back-compat: legacy "quantified_only" maps to new "repro_quant_only"
  if (identical(view_mode, "quantified_only")) view_mode <- "repro_quant_only"

  if (identical(view_mode, "repro_quant_only")) {
    keep_levels <- levels(df$level)[grepl("reproduc", levels(df$level), ignore.case = TRUE)]
    if (length(keep_levels) == 0) {
      keep_levels <- levels(df$level)[grepl("quant", levels(df$level), ignore.case = TRUE)]
    }
    if (length(keep_levels) > 0) {
      df <- df[df$level %in% keep_levels, , drop = FALSE]
      df$level <- factor(as.character(df$level), levels = keep_levels)
    }
  }

  # Get custom colors for reproducibly quantified vs quantified
  color_quantified <- style$color_quantified %||% "#1f77b4"
  color_identified <- style$color_identified %||% "#ff7f0e"

  # Bar outline style options
  show_outline <- isTRUE(style$show_bar_outline %||% FALSE)
  outline_color <- if (show_outline) (style$bar_outline_color %||% "#000000") else NA
  outline_width <- tb_num(style$bar_outline_width, 0.5)
  if (!is.finite(outline_width) || outline_width < 0) outline_width <- 0.5
  outline_lw <- if (show_outline) outline_width else 0

  # Map level display labels to colors
  level_vals <- levels(df$level)
  color_map <- stats::setNames(rep("#888888", length(level_vals)), level_vals)
  # Match common level names case-insensitively
  for (lv in level_vals) {
    if (grepl("reproduc", lv, ignore.case = TRUE)) {
      color_map[[lv]] <- color_quantified
    } else if (grepl("quant|ident|detect", lv, ignore.case = TRUE)) {
      color_map[[lv]] <- color_identified
    }
  }

  # Group-level bar plot (original)
  show_values <- isTRUE(style$show_values %||% FALSE)
  value_label_size <- tb_num(style$value_label_size, 3.5)
  y_expand_mult <- if (show_values) c(0, 0.15) else c(0, 0.05)

  p_group <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = n, fill = level)) +
    ggplot2::geom_col(position = "dodge", color = outline_color, linewidth = outline_lw) +
    ggplot2::scale_fill_manual(values = color_map) +
    ggplot2::scale_y_continuous(labels = format_k_suffix, expand = ggplot2::expansion(mult = y_expand_mult)) +
    ggplot2::labs(x = NULL, y = "Count") +
    tb_theme_base(tb_num(style$axis_text_size, 20), axis_style = style$axis_style %||% "clean") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))

  if (show_values) {
    p_group <- p_group +
      ggplot2::geom_text(
        ggplot2::aes(label = as.integer(n)),
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -0.3,
        size = value_label_size
      )
  }

  # Apply y-axis limit if in manual mode
  y_limit_mode <- style$y_limit_mode %||% "auto"
  if (y_limit_mode == "manual") {
    ymax <- style$ymax_protein %||% 10000
    p_group <- p_group + ggplot2::coord_cartesian(ylim = c(0, ymax), clip = if (show_values) "off" else "on")
  } else if (show_values) {
    p_group <- p_group + ggplot2::coord_cartesian(clip = "off")
  }

  # Build plots and tables lists
  # FIX: Table names must match plot names for res_plot_pick sync to work
  # view_mode controls which plot(s) are emitted so the picker is just the view selector:
  #   - combined: both group + replicate
  #   - quant_only: only the per-replicate quantified plot
  #   - repro_quant_only: only the group-level reproducibly-quantified plot
  plots <- list()
  tables <- list()
  if (view_mode %in% c("combined", "repro_quant_only")) {
    plots$idquant_group <- p_group
    tables$idquant_group <- df
  }

  # Replicate-level bar plot - shown in combined and quant_only modes
  rep_df <- results$data$replicate_counts
  if (view_mode %in% c("combined", "quant_only") &&
      !is.null(rep_df) && is.data.frame(rep_df) && nrow(rep_df) > 0) {
    if (!"replicate" %in% names(rep_df)) {
      rep_df$replicate <- ave(
        rep_df$sample_col %||% seq_len(nrow(rep_df)),
        rep_df$group,
        FUN = function(x) seq_along(x)
      )
    }
    rep_df$replicate <- suppressWarnings(as.integer(rep_df$replicate %||% NA))
    if (anyNA(rep_df$replicate)) {
      rep_df$replicate <- ave(
        rep_df$replicate,
        rep_df$group,
        FUN = function(x) {
          x[is.na(x)] <- seq_along(x)[is.na(x)]
          x
        }
      )
      rep_df$replicate <- suppressWarnings(as.integer(rep_df$replicate))
    }

    # Create a label for each replicate that maintains order: "GroupName_Rep#"
    # Ensure replicates are ordered by group then by replicate number
    rep_df <- rep_df[order(rep_df$group, rep_df$replicate), , drop = FALSE]

    # Create ordered factor for x-axis to maintain design/column ordering
    rep_df$replicate_label <- paste0(rep_df$group, "_R", rep_df$replicate)
    rep_df$replicate_label <- factor(rep_df$replicate_label, levels = unique(rep_df$replicate_label))

    # Create group factor for coloring
    rep_df$group <- factor(rep_df$group, levels = unique(rep_df$group))

    # Replicate bar plot uses color_identified for all bars (not group colors)
    # since replicate counts are "quantified" counts per replicate
    rep_label <- "Quantified Count per Replicate"
    rep_y_expand <- if (show_values) c(0, 0.15) else c(0, 0.05)
    p_rep <- ggplot2::ggplot(rep_df, ggplot2::aes(x = replicate_label, y = n)) +
      ggplot2::geom_col(width = 1, fill = color_identified, color = outline_color, linewidth = outline_lw) +
      ggplot2::scale_y_continuous(labels = format_k_suffix, expand = ggplot2::expansion(mult = rep_y_expand)) +
      ggplot2::labs(x = NULL, y = rep_label) +
      tb_theme_base(tb_num(style$axis_text_size, 20), axis_style = style$axis_style %||% "clean") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)) +
      ggplot2::theme(legend.position = "none")

    if (show_values) {
      p_rep <- p_rep +
        ggplot2::geom_text(
          ggplot2::aes(label = as.integer(n)),
          vjust = -0.3,
          size = value_label_size
        )
    }

    # Apply y-axis limit if in manual mode
    if (y_limit_mode == "manual") {
      ymax <- style$ymax_protein %||% 10000
      p_rep <- p_rep + ggplot2::coord_cartesian(ylim = c(0, ymax), clip = if (show_values) "off" else "on")
    } else if (show_values) {
      p_rep <- p_rep + ggplot2::coord_cartesian(clip = "off")
    }

    plots$idquant_replicate <- p_rep
    # FIX: Table name must match plot name for res_plot_pick sync to work
    tables$idquant_replicate <- rep_df[, c("group", "replicate", "n"), drop = FALSE]
  }

  # In combined mode, stack the two plots into a single output so the plot-picker
  # disappears and view_mode is the only graph selector the user sees.
  if (identical(view_mode, "combined") &&
      !is.null(plots$idquant_group) && !is.null(plots$idquant_replicate) &&
      requireNamespace("patchwork", quietly = TRUE)) {
    stacked <- patchwork::wrap_plots(plots$idquant_group, plots$idquant_replicate, ncol = 1)
    plots <- list(idquant_group = stacked)
    # Keep both tables accessible on the single plot key so table view isn't lost.
    tables <- list(idquant_group = df)
  }

  list(plots = plots, tables = tables)
}

# ---- IDQuant Container Child Engines ----------------------------------------

tb_render_idquant_id_quant <- function(results, style, meta) {
  tb_render_idquant(results, style, meta)
}

tb_render_idquant_average_value <- function(results, style, meta) {
  tb_require_pkg("ggplot2")
  tb_require_pkg("scales")

  df <- results$data$average_value
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    # Legacy terpbooks may not have precomputed average_value; compute from raw intensity data.
    mat <- results$data$intensity_mat
    smeta <- results$data$sample_meta
    if (!is.null(mat) && is.matrix(mat) && !is.null(smeta) && is.data.frame(smeta)) {
      groups <- unique(as.character(smeta$group_name %||% smeta$group %||% ""))
      groups <- groups[nzchar(groups)]

      avg_list <- list()
      for (grp in groups) {
        cols <- smeta$sample_col[(smeta$group_name %||% smeta$group) == grp]
        cols <- intersect(as.character(cols), colnames(mat))
        if (length(cols) == 0) next

        grp_mat <- mat[, cols, drop = FALSE]
        protein_means <- rowMeans(grp_mat, na.rm = TRUE)
        protein_means <- protein_means[is.finite(protein_means) & !is.na(protein_means) & protein_means > 0]

        avg_list[[length(avg_list) + 1L]] <- data.frame(
          group = grp,
          mean_value = mean(protein_means, na.rm = TRUE),
          median_value = stats::median(protein_means, na.rm = TRUE),
          n_proteins = length(protein_means),
          stringsAsFactors = FALSE
        )
      }

      df <- if (length(avg_list) > 0) do.call(rbind, avg_list) else data.frame(
        group = character(), mean_value = numeric(), median_value = numeric(), n_proteins = integer(),
        stringsAsFactors = FALSE
      )
    }

    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      p_empty <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No Average Value data available",
                          size = 6, color = "gray50") +
        ggplot2::theme_void()

      return(list(
        plots = list(idquant_average_value_plot = p_empty),
        tables = list(idquant_average_value_table = data.frame())
      ))
    }
  }

  df$group <- factor(df$group)

  transform <- style$transform %||% "log10"
  y_raw <- suppressWarnings(as.numeric(df$mean_value))
  y <- y_raw
  if (identical(transform, "log2")) {
    y <- ifelse(is.finite(y_raw) & y_raw > 0, log2(y_raw), NA_real_)
  } else if (identical(transform, "log10")) {
    y <- ifelse(is.finite(y_raw) & y_raw > 0, log10(y_raw), NA_real_)
  }
  df$y <- y
  y_axis_title <- style$y_axis_title %||% "Mean intensity"
  ylab <- tb_log_axis_label(y_axis_title, transform)

  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"
  show_bar_outline <- isTRUE(style$show_bar_outline %||% FALSE)
  bar_outline_color <- as.character(style$bar_outline_color %||% "#000000")
  bar_outline_width <- tb_num(style$bar_outline_width, 0.8)
  if (!is.finite(bar_outline_width) || bar_outline_width < 0) bar_outline_width <- 0.8

  bar_color_mode <- style$bar_color_mode %||% "group"
  if (!bar_color_mode %in% c("group", "flat")) bar_color_mode <- "group"
  bar_color <- as.character(style$bar_color %||% "#1f77b4")

  show_values <- isTRUE(style$show_values %||% FALSE)
  value_label_size <- tb_num(style$value_label_size, 3.5)
  if (!is.finite(value_label_size) || value_label_size <= 0) value_label_size <- 3.5
  show_error_bars <- isTRUE(style$show_error_bars %||% FALSE)
  show_significance <- isTRUE(style$show_significance %||% FALSE)
  sig_display_mode <- as.character(style$sig_display_mode %||% "stars")[1]
  sig_text_size <- tb_num(style$sig_text_size, 3.5)
  if (!is.finite(sig_text_size) || sig_text_size <= 0) sig_text_size <- 3.5
  y_expand_mult <- if (show_values || show_significance) c(0, 0.15) else c(0, 0.05)
  if (show_values) {
    fmt_sig3 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      out <- rep(NA_character_, length(x))
      ok <- is.finite(x)
      out[ok] <- format(signif(x[ok], 3), trim = TRUE, scientific = FALSE)
      out
    }
    df$label_value <- fmt_sig3(df$y)
  }

  outline_color <- if (show_bar_outline) bar_outline_color else NA
  outline_lw <- if (show_bar_outline) bar_outline_width else 0

  group_colors <- results$data$group_colors %||% NULL
  if (is.null(group_colors) || length(group_colors) == 0) {
    group_colors <- scales::hue_pal()(nlevels(df$group))
    names(group_colors) <- levels(df$group)
  } else {
    missing <- levels(df$group)[!levels(df$group) %in% names(group_colors)]
    if (length(missing) > 0) {
      auto_colors <- grDevices::hcl.colors(length(missing), palette = "Dark 3")
      names(auto_colors) <- missing
      group_colors <- c(group_colors, auto_colors)
    }
  }
  group_colors <- group_colors[levels(df$group)]

  p <- if (bar_color_mode == "group") {
    ggplot2::ggplot(df, ggplot2::aes(x = group, y = y, fill = group)) +
      ggplot2::geom_col(color = outline_color, linewidth = outline_lw) +
      ggplot2::scale_fill_manual(values = group_colors, guide = "none")
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = group, y = y)) +
      ggplot2::geom_col(fill = bar_color, color = outline_color, linewidth = outline_lw)
  }

  p <- p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = y_expand_mult)) +
    ggplot2::labs(x = NULL, y = ylab) +
    tb_theme_base(axis_text_size, axis_style = axis_style)

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label_value),
        vjust = -0.3,
        size = value_label_size
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # --- Error bars and significance (p-value brackets) ---
  sample_avg <- results$data$sample_averages
  if (is.null(sample_avg) && (show_error_bars || show_significance)) {
    # Compute from raw intensity matrix if not precomputed (legacy terpbooks)
    mat_raw <- results$data$intensity_mat
    smeta_raw <- results$data$sample_meta
    if (!is.null(mat_raw) && is.matrix(mat_raw) && !is.null(smeta_raw) && is.data.frame(smeta_raw)) {
      sa_list <- list()
      for (grp in levels(df$group)) {
        cols <- smeta_raw$sample_col[(smeta_raw$group_name %||% smeta_raw$group) == grp]
        cols <- intersect(as.character(cols), colnames(mat_raw))
        for (col in cols) {
          vals <- mat_raw[, col]
          vals <- vals[is.finite(vals) & !is.na(vals) & vals > 0]
          sa_list[[length(sa_list) + 1L]] <- data.frame(
            group = grp, sample_col = col,
            mean_value = if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(sa_list) > 0) sample_avg <- do.call(rbind, sa_list)
    }
  }

  if (!is.null(sample_avg) && is.data.frame(sample_avg) && nrow(sample_avg) > 0) {
    # Apply same transform to per-sample values
    sa <- sample_avg
    sa$y <- suppressWarnings(as.numeric(sa$mean_value))
    if (identical(transform, "log2")) {
      sa$y <- ifelse(is.finite(sa$y) & sa$y > 0, log2(sa$y), NA_real_)
    } else if (identical(transform, "log10")) {
      sa$y <- ifelse(is.finite(sa$y) & sa$y > 0, log10(sa$y), NA_real_)
    }
    sa <- sa[is.finite(sa$y), , drop = FALSE]

    # Compute SEM per group
    sem_df <- do.call(rbind, lapply(levels(df$group), function(g) {
      vals <- sa$y[sa$group == g]
      n <- length(vals)
      data.frame(
        group = g,
        sem = if (n >= 2) stats::sd(vals) / sqrt(n) else 0,
        stringsAsFactors = FALSE
      )
    }))
    df <- merge(df, sem_df, by = "group", all.x = TRUE)
    df$group <- factor(df$group, levels = levels(factor(df$group)))

    # Add error bars
    if (show_error_bars && !all(is.na(df$sem) | df$sem == 0)) {
      p <- p + ggplot2::geom_errorbar(
        data = df,
        ggplot2::aes(x = group, ymin = y - sem, ymax = y + sem),
        width = 0.2, linewidth = 0.5, inherit.aes = FALSE
      )
    }

    # Add significance brackets
    if (show_significance && nlevels(df$group) >= 2) {
      group_levels <- levels(df$group)
      pairs <- utils::combn(group_levels, 2, simplify = FALSE)

      y_max_data <- max(df$y + ifelse(is.finite(df$sem), df$sem, 0), na.rm = TRUE)
      if (!is.finite(y_max_data)) y_max_data <- max(df$y, na.rm = TRUE)
      if (!is.finite(y_max_data)) y_max_data <- 1

      y_step <- y_max_data * 0.08
      current_y <- y_max_data * 1.05

      for (pair in pairs) {
        vals_a <- sa$y[sa$group == pair[1]]
        vals_b <- sa$y[sa$group == pair[2]]

        if (length(vals_a) < 2 || length(vals_b) < 2) next

        pval <- tryCatch({
          tt <- stats::t.test(vals_a, vals_b)
          tt$p.value
        }, error = function(e) NA_real_)

        if (is.na(pval) || !is.finite(pval)) next

        # Format label
        if (pval >= 0.05) {
          label_text <- "n.s."
        } else if (sig_display_mode == "pvalue") {
          if (pval < 0.001) label_text <- "p < 0.001"
          else label_text <- sprintf("p = %s", signif(pval, 2))
        } else {
          if (pval < 0.001) label_text <- "***"
          else if (pval < 0.01) label_text <- "**"
          else label_text <- "*"
        }

        x_a <- which(group_levels == pair[1])
        x_b <- which(group_levels == pair[2])

        p <- p +
          ggplot2::annotate("segment", x = x_a, xend = x_a,
                            y = current_y - y_step * 0.3, yend = current_y, linewidth = 0.4) +
          ggplot2::annotate("segment", x = x_a, xend = x_b,
                            y = current_y, yend = current_y, linewidth = 0.4) +
          ggplot2::annotate("segment", x = x_b, xend = x_b,
                            y = current_y, yend = current_y - y_step * 0.3, linewidth = 0.4) +
          ggplot2::annotate("text", x = (x_a + x_b) / 2, y = current_y + y_step * 0.2,
                            label = label_text, size = sig_text_size)

        current_y <- current_y + y_step
      }

      # Ensure clipping is off so brackets are visible
      p <- p + ggplot2::coord_cartesian(clip = "off")
    }
  }

  tbl <- df[, c("group", "mean_value", "median_value", "n_proteins"), drop = FALSE]
  fmt3 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    out <- rep(NA_character_, length(x))
    ok <- is.finite(x)
    out[ok] <- formatC(signif(x[ok], 3), format = "e", digits = 2)
    out
  }
  tbl$mean_value <- fmt3(tbl$mean_value)
  tbl$median_value <- fmt3(tbl$median_value)
  tbl$group <- as.character(tbl$group)

  plots <- list(idquant_average_value = p)
  tables <- list(idquant_average_value = tbl)

  # --- Replicate-level bar plot (individual samples) ---
  if (is.null(sample_avg)) {
    # Compute from raw intensity matrix if not already available
    mat_raw <- results$data$intensity_mat
    smeta_raw <- results$data$sample_meta
    if (!is.null(mat_raw) && is.matrix(mat_raw) && !is.null(smeta_raw) && is.data.frame(smeta_raw)) {
      sa_list <- list()
      for (grp in levels(df$group)) {
        cols <- smeta_raw$sample_col[(smeta_raw$group_name %||% smeta_raw$group) == grp]
        cols <- intersect(as.character(cols), colnames(mat_raw))
        for (col in cols) {
          vals <- mat_raw[, col]
          vals <- vals[is.finite(vals) & !is.na(vals) & vals > 0]
          sa_list[[length(sa_list) + 1L]] <- data.frame(
            group = grp, sample_col = col,
            mean_value = if (length(vals) > 0) mean(vals, na.rm = TRUE) else NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(sa_list) > 0) sample_avg <- do.call(rbind, sa_list)
    }
  }

  if (!is.null(sample_avg) && is.data.frame(sample_avg) && nrow(sample_avg) > 0) {
    sa_rep <- sample_avg
    sa_rep$y <- suppressWarnings(as.numeric(sa_rep$mean_value))
    if (identical(transform, "log2")) {
      sa_rep$y <- ifelse(is.finite(sa_rep$y) & sa_rep$y > 0, log2(sa_rep$y), NA_real_)
    } else if (identical(transform, "log10")) {
      sa_rep$y <- ifelse(is.finite(sa_rep$y) & sa_rep$y > 0, log10(sa_rep$y), NA_real_)
    }
    sa_rep <- sa_rep[is.finite(sa_rep$y), , drop = FALSE]

    if (nrow(sa_rep) > 0) {
      # Assign replicate numbers within each group
      sa_rep$group <- factor(sa_rep$group, levels = levels(df$group))
      sa_rep <- sa_rep[order(sa_rep$group, sa_rep$sample_col), , drop = FALSE]
      sa_rep$replicate <- ave(seq_len(nrow(sa_rep)), sa_rep$group, FUN = seq_along)
      sa_rep$replicate_label <- paste0(sa_rep$group, "_R", sa_rep$replicate)
      sa_rep$replicate_label <- factor(sa_rep$replicate_label, levels = unique(sa_rep$replicate_label))

      rep_y_expand <- if (show_values) c(0, 0.15) else c(0, 0.05)

      p_rep <- if (bar_color_mode == "group") {
        ggplot2::ggplot(sa_rep, ggplot2::aes(x = replicate_label, y = y, fill = group)) +
          ggplot2::geom_col(color = outline_color, linewidth = outline_lw) +
          ggplot2::scale_fill_manual(values = group_colors, guide = "none")
      } else {
        ggplot2::ggplot(sa_rep, ggplot2::aes(x = replicate_label, y = y)) +
          ggplot2::geom_col(fill = bar_color, color = outline_color, linewidth = outline_lw)
      }

      p_rep <- p_rep +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = rep_y_expand)) +
        ggplot2::labs(x = NULL, y = ylab) +
        tb_theme_base(axis_text_size, axis_style = axis_style) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10))

      if (show_values) {
        sa_rep$label_value <- fmt_sig3(sa_rep$y)
        p_rep <- p_rep +
          ggplot2::geom_text(
            data = sa_rep,
            ggplot2::aes(label = label_value),
            vjust = -0.3,
            size = value_label_size
          ) +
          ggplot2::coord_cartesian(clip = "off")
      }

      plots$idquant_average_value_replicate <- p_rep
      rep_tbl <- sa_rep[, c("group", "sample_col", "mean_value"), drop = FALSE]
      rep_tbl$mean_value <- fmt3(rep_tbl$mean_value)
      tables$idquant_average_value_replicate <- rep_tbl
    }
  }

  list(plots = plots, tables = tables)
}

# ---- IDQuant Child Views -----------------------------------------------------

# IDQuant v2 child-view model:
# - New terpbooks write 5 child views under each IDQuant step (engine_id: idquant_id_quant/idquant_average_value/idquant_cv_scatter/idquant_cv_bar/idquant_overlap).
# - Child views reuse the parent results.rds and compute view-specific summaries at render time.
# - Legacy terpbooks (single idquant node) continue to render via tb_render_idquant().

#' Render IDQuant group child view
#'
#' Child view contract:
#' - Uses parent IDQuant `results$data$counts` and shared raw data fields.
#' - Returns ggplot(s) and data.frame table(s) for the Results viewer.
#'
#' @param results Engine results (from stats_idquant_run)
#' @param style Viewer-time style overrides
#' @param meta Node metadata
#' @return list(plots, tables)
tb_render_idquant_group <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  df <- results$data$counts
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No IDQuant data available",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_group_plot = p_empty),
      tables = list(idquant_group_table = data.frame())
    ))
  }

  # Normalize column names: engine produces `metric`, renderer expects `level`
  if ("metric" %in% names(df) && !"level" %in% names(df)) {
    df$level <- df$metric
  }
  df$group <- factor(df$group)
  df$level <- factor(df$level %||% df$metric %||% "count")

  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"
  show_count_labels <- isTRUE(style$show_count_labels %||% FALSE)
  count_label_size <- tb_num(style$count_label_size, 3.5)
  show_significance <- isTRUE(style$show_significance %||% FALSE)
  sig_display_mode <- as.character(style$sig_display_mode %||% "stars")[1]
  sig_text_size <- tb_num(style$sig_text_size, 3.5)
  if (!is.finite(sig_text_size) || sig_text_size <= 0) sig_text_size <- 3.5
  sig_metric <- as.character(style$sig_metric %||% "identified")[1]
  if (!sig_metric %in% c("identified", "quantified")) sig_metric <- "identified"

  y_expand_mult <- if (show_count_labels || show_significance) c(0, 0.15) else c(0, 0.05)

  # Custom fill colors from style
  color_quantified <- as.character(style$color_quantified %||% "#1f77b4")
  color_identified <- as.character(style$color_identified %||% "#ff7f0e")
  fill_colors <- c("quantified" = color_quantified, "identified" = color_identified)

  show_bar_outline <- isTRUE(style$show_bar_outline %||% FALSE)
  bar_outline_color <- as.character(style$bar_outline_color %||% "#000000")
  bar_outline_width <- tb_num(style$bar_outline_width, 0.5)
  outline_color <- if (show_bar_outline) bar_outline_color else NA
  outline_lw <- if (show_bar_outline) bar_outline_width else 0

  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = n, fill = level)) +
    ggplot2::geom_col(position = "dodge", color = outline_color, linewidth = outline_lw) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::scale_y_continuous(labels = format_k_suffix, expand = ggplot2::expansion(mult = y_expand_mult)) +
    ggplot2::labs(x = NULL, y = "Count") +
    tb_theme_base(axis_text_size, axis_style = axis_style) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))

  if (show_count_labels) {
    show_values <- isTRUE(style$show_values %||% FALSE)
    label_var <- if (show_values) "n" else "n"
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = n),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = -0.3,
      size = count_label_size
    )
  }

  # --- Significance brackets (t-test on per-replicate counts) ---
  if (show_significance && nlevels(df$group) >= 2) {
    rep_counts <- results$data$replicate_counts
    mat_raw <- results$data$intensity_mat
    smeta_raw <- results$data$sample_meta

    # Build per-replicate identified/quantified counts if not precomputed
    if (is.null(rep_counts) && !is.null(mat_raw) && is.matrix(mat_raw) &&
        !is.null(smeta_raw) && is.data.frame(smeta_raw)) {
      rc_list <- list()
      groups_all <- unique(as.character(smeta_raw$group_name %||% smeta_raw$group))
      for (grp in groups_all) {
        grp_samples <- smeta_raw[as.character(smeta_raw$group_name %||% smeta_raw$group) == grp, , drop = FALSE]
        grp_cols <- intersect(as.character(grp_samples$sample_col), colnames(mat_raw))
        for (col in grp_cols) {
          vals <- mat_raw[, col]
          n_det <- sum(!is.na(vals) & is.finite(vals) & vals > 0)
          rc_list[[length(rc_list) + 1L]] <- data.frame(
            group = grp, sample_col = col, n = n_det, stringsAsFactors = FALSE
          )
        }
      }
      if (length(rc_list) > 0) rep_counts <- do.call(rbind, rc_list)
    }

    # For the "quantified" metric, we need to compute per-replicate
    # quantified counts (detected in ALL replicates of the group).
    # Since "quantified" is a group-level metric (not per-replicate),
    # we use the "identified" (detected) per-replicate counts for t-tests.
    # For the "identified" metric (renamed "Quantified" in the UI),
    # we use per-replicate detection counts directly.

    if (!is.null(rep_counts) && is.data.frame(rep_counts) && nrow(rep_counts) > 0) {
      group_levels <- levels(df$group)
      pairs <- utils::combn(group_levels, 2, simplify = FALSE)

      y_max_data <- max(df$n, na.rm = TRUE)
      if (!is.finite(y_max_data)) y_max_data <- 1

      y_step <- y_max_data * 0.06
      current_y <- y_max_data * 1.05

      for (pair in pairs) {
        vals_a <- rep_counts$n[rep_counts$group == pair[1]]
        vals_b <- rep_counts$n[rep_counts$group == pair[2]]

        if (length(vals_a) < 2 || length(vals_b) < 2) next

        pval <- tryCatch({
          tt <- stats::t.test(vals_a, vals_b)
          tt$p.value
        }, error = function(e) NA_real_)

        if (is.na(pval) || !is.finite(pval)) next

        # Format label
        if (pval >= 0.05) {
          label_text <- "n.s."
        } else if (sig_display_mode == "pvalue") {
          if (pval < 0.001) label_text <- "p < 0.001"
          else label_text <- sprintf("p = %s", signif(pval, 2))
        } else {
          if (pval < 0.001) label_text <- "***"
          else if (pval < 0.01) label_text <- "**"
          else label_text <- "*"
        }

        x_a <- which(group_levels == pair[1])
        x_b <- which(group_levels == pair[2])

        p <- p +
          ggplot2::annotate("segment", x = x_a, xend = x_a,
                            y = current_y - y_step * 0.3, yend = current_y, linewidth = 0.4) +
          ggplot2::annotate("segment", x = x_a, xend = x_b,
                            y = current_y, yend = current_y, linewidth = 0.4) +
          ggplot2::annotate("segment", x = x_b, xend = x_b,
                            y = current_y, yend = current_y - y_step * 0.3, linewidth = 0.4) +
          ggplot2::annotate("text", x = (x_a + x_b) / 2, y = current_y + y_step * 0.2,
                            label = label_text, size = sig_text_size)

        current_y <- current_y + y_step
      }
    }
  }

  # Ensure clipping is off when we have labels or brackets
  if (show_count_labels || show_significance) {
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }

  list(
    plots = list(idquant_group_plot = p),
    tables = list(idquant_group_table = within(df, { n <- format_k_suffix(n) }))
  )
}
#' Render IDQuant replicate child view
#'
#' Produces a per-protein table with replicate intensity columns and CV% per group.
#' CV% is computed on raw intensities; transform (none/log2/log10) is applied for display.
#'
#' @param results Engine results (from stats_idquant_run)
#' @param style Viewer-time style overrides
#' @param meta Node metadata
#' @return list(plots, tables)
tb_render_idquant_replicate <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  mat <- results$data$intensity_mat
  smeta <- results$data$sample_meta

  if (is.null(mat) || !is.matrix(mat) || is.null(smeta) || !is.data.frame(smeta)) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "IDQuant raw intensity data missing (legacy run)",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_replicate_plot = p_empty),
      tables = list(idquant_replicate_table = data.frame())
    ))
  }

  transform <- style$transform %||% "log10"
  tx <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (transform == "log2") return(ifelse(is.finite(x) & x > 0, log2(x), NA_real_))
    if (transform == "log10") return(ifelse(is.finite(x) & x > 0, log10(x), NA_real_))
    x
  }

  protein_id <- rownames(mat)
  if (is.null(protein_id) || any(!nzchar(protein_id))) {
    protein_id <- paste0("protein_", seq_len(nrow(mat)))
  }

  # Per-replicate summary plot (median intensity per replicate)
  smeta$sample_col <- as.character(smeta$sample_col)
  smeta$group_name <- as.character(smeta$group_name %||% smeta$group)
  smeta$replicate <- suppressWarnings(as.integer(smeta$replicate %||% NA))
  smeta <- smeta[!is.na(smeta$sample_col) & nzchar(smeta$sample_col), , drop = FALSE]
  smeta <- smeta[smeta$sample_col %in% colnames(mat), , drop = FALSE]

  if (nrow(smeta) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No sample metadata available for IDQuant",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_replicate_plot = p_empty),
      tables = list(idquant_replicate_table = data.frame())
    ))
  }

  smeta$replicate_label <- ifelse(
    is.na(smeta$replicate),
    smeta$sample_col,
    paste0(smeta$group_name, "_R", smeta$replicate)
  )

  rep_summ <- do.call(rbind, lapply(seq_len(nrow(smeta)), function(i) {
    sc <- smeta$sample_col[[i]]
    v <- tx(mat[, sc])
    v_ok <- v[is.finite(v)]
    data.frame(
      group = smeta$group_name[[i]],
      replicate = smeta$replicate[[i]],
      sample_col = sc,
      replicate_label = smeta$replicate_label[[i]],
      median_intensity = if (length(v_ok) > 0) stats::median(v_ok) else NA_real_,
      n_detected = sum(is.finite(mat[, sc]) & mat[, sc] > 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  rep_summ$replicate_label <- factor(rep_summ$replicate_label, levels = unique(rep_summ$replicate_label))
  rep_summ$group <- factor(rep_summ$group)

  ylab <- if (transform == "none") "Intensity" else paste0("Intensity (", transform, ")")
  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"
  show_count_labels <- isTRUE(style$show_count_labels %||% FALSE)
  count_label_size <- tb_num(style$count_label_size, 3.5)

  y_expand_mult <- if (show_count_labels) c(0, 0.15) else c(0, 0.05)

  p <- ggplot2::ggplot(rep_summ, ggplot2::aes(x = replicate_label, y = n_detected, fill = group)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(labels = format_k_suffix, expand = ggplot2::expansion(mult = y_expand_mult)) +
    ggplot2::labs(x = NULL, y = "Count") +
    tb_theme_base(axis_text_size, axis_style = axis_style) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10))

  if (show_count_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = n_detected),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = -0.3,
      size = count_label_size
    )
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }

  # Per-protein replicate table with CV% per group
  groups <- unique(smeta$group_name)
  groups <- groups[nzchar(groups)]

  tbl_list <- list()
  for (g in groups) {
    gm <- smeta[smeta$group_name == g, , drop = FALSE]
    gm <- gm[order(gm$replicate, gm$sample_col), , drop = FALSE]
    cols <- intersect(gm$sample_col, colnames(mat))
    if (length(cols) == 0) next

    grp_mat <- mat[, cols, drop = FALSE]
    raw_vals <- as.matrix(grp_mat)
    raw_vals[!is.finite(raw_vals) | raw_vals <= 0] <- NA_real_

    mean_raw <- rowMeans(raw_vals, na.rm = TRUE)
    sd_raw <- apply(raw_vals, 1, stats::sd, na.rm = TRUE)
    n_obs <- apply(raw_vals, 1, function(x) sum(is.finite(x)))

    cv_pct <- ifelse(is.finite(mean_raw) & mean_raw > 0 & n_obs >= 2, 100 * sd_raw / mean_raw, NA_real_)

    df <- data.frame(
      protein_id = protein_id,
      group = g,
      abundance = tx(mean_raw),
      cv_pct = cv_pct,
      n_detected = n_obs,
      stringsAsFactors = FALSE
    )

        rep_ids <- gm$replicate
    if (length(rep_ids) != length(cols)) {
      rep_ids <- seq_along(cols)
    }
    rep_ids[is.na(rep_ids)] <- seq_along(cols)[is.na(rep_ids)]
    rep_names <- make.unique(paste0("rep_", rep_ids))

    disp_mat <- apply(grp_mat, 2, tx)
    disp_df <- as.data.frame(disp_mat, stringsAsFactors = FALSE)
    disp_df <- disp_df[, , drop = FALSE]
    names(disp_df) <- rep_names

    tbl_list[[g]] <- cbind(df, disp_df)
  }

    tbl <- data.frame()
  if (length(tbl_list) > 0) {
    fixed_cols <- c("protein_id", "group", "abundance", "cv_pct", "n_detected")
    all_cols <- Reduce(union, lapply(tbl_list, names))
    rep_cols <- setdiff(all_cols, fixed_cols)
    ordered_cols <- c(fixed_cols, sort(rep_cols))

    tbl_list <- lapply(tbl_list, function(d) {
      missing <- setdiff(ordered_cols, names(d))
      for (m in missing) d[[m]] <- NA_real_
      d <- d[, ordered_cols, drop = FALSE]
      d
    })

    tbl <- do.call(rbind, tbl_list)
    rownames(tbl) <- NULL

  if (nrow(tbl) > 0 && "abundance" %in% names(tbl)) {
    tbl$abundance <- format_k_suffix(tbl$abundance)
  }

  }


  if (nrow(tbl) > 0) {
    fmt_cols <- setdiff(names(tbl), c("protein_id", "group", "cv_pct", "n_detected"))
    for (col in fmt_cols) {
      if (is.numeric(tbl[[col]]) || is.integer(tbl[[col]])) {
        tbl[[col]] <- format_k_suffix(tbl[[col]])
      }
    }
  }

  list(
    plots = list(idquant_replicate_plot = p),
    tables = list(idquant_replicate_table = tbl)
  )
}

#' Build CV export data frame for Excel download
#'
#' Pure function that computes CV% statistics from IDQuant results and returns
#' a data.frame suitable for Excel export with raw numeric columns.
#'
#' @param results Engine results (from stats_idquant_run)
#' @param style Style list containing min_replicates and cv_bin_1..5 fields
#' @return data.frame with columns: protein_id, group, cv_pct, mean_abundance_raw, n_detected, cv_bin_label
#' @export
build_cv_export_df <- function(results, style) {
  mat <- results$data$intensity_mat
  smeta <- results$data$sample_meta

  if (is.null(mat) || !is.matrix(mat) || is.null(smeta) || !is.data.frame(smeta)) {
    return(data.frame(
      protein_id = character(),
      group = character(),
      cv_pct = numeric(),
      mean_abundance_raw = numeric(),
      n_detected = integer(),
      cv_bin_label = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Extract style settings with defaults

  min_replicates <- suppressWarnings(as.integer(style$min_replicates %||% 2L))
  if (!is.finite(min_replicates) || min_replicates < 1) min_replicates <- 2L
  min_replicates <- max(1L, min(10L, min_replicates))

  cv_bin_1 <- suppressWarnings(as.numeric(style$cv_bin_1 %||% 10))
  cv_bin_2 <- suppressWarnings(as.numeric(style$cv_bin_2 %||% 20))
  cv_bin_3 <- suppressWarnings(as.numeric(style$cv_bin_3 %||% 30))
  cv_bin_4 <- suppressWarnings(as.numeric(style$cv_bin_4 %||% 50))
  cv_bin_5 <- suppressWarnings(as.numeric(style$cv_bin_5 %||% 100))
  if (!is.finite(cv_bin_1)) cv_bin_1 <- 10
  if (!is.finite(cv_bin_2)) cv_bin_2 <- 20
  if (!is.finite(cv_bin_3)) cv_bin_3 <- 30
  if (!is.finite(cv_bin_4)) cv_bin_4 <- 50
  if (!is.finite(cv_bin_5)) cv_bin_5 <- 100

  # Number of bins to display (2-6)
  num_bins <- suppressWarnings(as.integer(style$num_bins %||% 6L))
  if (!is.finite(num_bins) || num_bins < 2) num_bins <- 6L
  num_bins <- max(2L, min(6L, num_bins))

  protein_id <- rownames(mat)
  if (is.null(protein_id) || any(!nzchar(protein_id))) {
    protein_id <- paste0("protein_", seq_len(nrow(mat)))
  }

  smeta$sample_col <- as.character(smeta$sample_col)
  smeta$group_name <- as.character(smeta$group_name %||% smeta$group)
  smeta <- smeta[!is.na(smeta$sample_col) & nzchar(smeta$sample_col), , drop = FALSE]
  smeta <- smeta[smeta$sample_col %in% colnames(mat), , drop = FALSE]

  groups <- unique(smeta$group_name)
  groups <- groups[nzchar(groups)]

  stat_list <- list()
  for (g in groups) {
    gm <- smeta[smeta$group_name == g, , drop = FALSE]
    cols <- intersect(as.character(gm$sample_col), colnames(mat))
    if (length(cols) == 0) next

    grp_mat <- mat[, cols, drop = FALSE]
    raw_vals <- as.matrix(grp_mat)
    raw_vals[!is.finite(raw_vals) | raw_vals <= 0] <- NA_real_

    mean_raw <- rowMeans(raw_vals, na.rm = TRUE)
    sd_raw <- apply(raw_vals, 1, stats::sd, na.rm = TRUE)
    n_obs <- apply(raw_vals, 1, function(x) sum(is.finite(x)))
    cv_pct <- ifelse(is.finite(mean_raw) & mean_raw > 0 & n_obs >= min_replicates, 100 * sd_raw / mean_raw, NA_real_)

    stat_list[[g]] <- data.frame(
      protein_id = protein_id,
      group = g,
      cv_pct = cv_pct,
      mean_abundance_raw = mean_raw,
      n_detected = n_obs,
      stringsAsFactors = FALSE
    )
  }

  df <- if (length(stat_list) > 0) do.call(rbind, stat_list) else data.frame()
  if (nrow(df) == 0) {
    return(data.frame(
      protein_id = character(),
      group = character(),
      cv_pct = numeric(),
      mean_abundance_raw = numeric(),
      n_detected = integer(),
      cv_bin_label = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Build CV bin labels respecting num_bins
  all_thresholds <- c(cv_bin_1, cv_bin_2, cv_bin_3, cv_bin_4, cv_bin_5)
  n_thresholds <- min(num_bins - 1L, 5L)
  used_thresholds <- all_thresholds[seq_len(n_thresholds)]
  breaks <- c(0, used_thresholds, Inf)
  bin_labels <- character(num_bins)
  bin_labels[1] <- sprintf("0\u2013%.0f%%", used_thresholds[1])
  if (n_thresholds >= 2) {
    for (i in 2:n_thresholds) {
      bin_labels[i] <- sprintf("%.0f\u2013%.0f%%", used_thresholds[i - 1], used_thresholds[i])
    }
  }
  bin_labels[num_bins] <- sprintf("%.0f%%+", used_thresholds[n_thresholds])
  df$cv_bin_label <- as.character(cut(df$cv_pct, breaks = breaks, right = FALSE, include.lowest = TRUE, labels = bin_labels))

  # Filter to rows with valid CV
  df <- df[is.finite(df$cv_pct), , drop = FALSE]
  rownames(df) <- NULL

  df[, c("protein_id", "group", "cv_pct", "mean_abundance_raw", "n_detected", "cv_bin_label"), drop = FALSE]
}

#' Render IDQuant CV% child view
#'
#' Produces a CV% vs abundance scatter plot and a threshold-filtered table.
#' CV% is computed on raw intensities; transform (none/log2/log10) is applied for display.
#'
#' @param results Engine results (from stats_idquant_run)
#' @param style Viewer-time style overrides
#' @param meta Node metadata
#' @return list(plots, tables)
tb_render_idquant_cv <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  mat <- results$data$intensity_mat
  smeta <- results$data$sample_meta

  if (is.null(mat) || !is.matrix(mat) || is.null(smeta) || !is.data.frame(smeta)) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "IDQuant raw intensity data missing (legacy run)",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_cv_plot = p_empty),
      tables = list(idquant_cv_table = data.frame())
    ))
  }

  transform <- style$transform %||% "log10"
  cv_threshold <- suppressWarnings(as.numeric(style$cv_threshold %||% 30))
  if (!is.finite(cv_threshold)) cv_threshold <- 30
  show_labels <- isTRUE(style$show_labels %||% TRUE)
  show_avg_cv <- isTRUE(style$show_avg_cv %||% FALSE)
  n_labels <- suppressWarnings(as.integer(style$n_labels %||% 30L))
  if (!is.finite(n_labels) || n_labels < 0) n_labels <- 30L
  n_labels <- max(0L, min(200L, n_labels))

  # New style fields for plot mode, min replicates, and CV bins

  cv_plot_mode <- style$cv_plot_mode %||% "both"
  if (!cv_plot_mode %in% c("both", "scatter", "bar")) cv_plot_mode <- "both"

  min_replicates <- suppressWarnings(as.integer(style$min_replicates %||% 2L))
  if (!is.finite(min_replicates) || min_replicates < 1) min_replicates <- 2L
  min_replicates <- max(1L, min(10L, min_replicates))

  # Build CV bin thresholds from style fields (defaults: 10, 20, 30, 50, 100)
  cv_bin_1 <- suppressWarnings(as.numeric(style$cv_bin_1 %||% 10))
  cv_bin_2 <- suppressWarnings(as.numeric(style$cv_bin_2 %||% 20))
  cv_bin_3 <- suppressWarnings(as.numeric(style$cv_bin_3 %||% 30))
  cv_bin_4 <- suppressWarnings(as.numeric(style$cv_bin_4 %||% 50))
  cv_bin_5 <- suppressWarnings(as.numeric(style$cv_bin_5 %||% 100))
  if (!is.finite(cv_bin_1)) cv_bin_1 <- 10
  if (!is.finite(cv_bin_2)) cv_bin_2 <- 20
  if (!is.finite(cv_bin_3)) cv_bin_3 <- 30
  if (!is.finite(cv_bin_4)) cv_bin_4 <- 50
  if (!is.finite(cv_bin_5)) cv_bin_5 <- 100

  # Number of bins to display (2-6)
  num_bins <- suppressWarnings(as.integer(style$num_bins %||% 6L))
  if (!is.finite(num_bins) || num_bins < 2) num_bins <- 6L
  num_bins <- max(2L, min(6L, num_bins))

  tx <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (transform == "log2") return(ifelse(is.finite(x) & x > 0, log2(x), NA_real_))
    if (transform == "log10") return(ifelse(is.finite(x) & x > 0, log10(x), NA_real_))
    x
  }

  protein_id <- rownames(mat)
  rownames_were_null <- is.null(protein_id)
  if (rownames_were_null) {
    protein_id <- paste0("protein_", seq_len(nrow(mat)))
  } else {
    # Only replace empty/missing protein IDs with placeholders, not all of them
    empty_idx <- which(!nzchar(protein_id) | is.na(protein_id))
    if (length(empty_idx) > 0) {
      protein_id[empty_idx] <- paste0("protein_", empty_idx)
    }
  }

  label_id <- protein_id
  ids <- results$data$ids %||% NULL
  if (!is.null(ids) && is.data.frame(ids) && nrow(ids) > 0) {
    # Find primary ID column and gene symbol column
    # Look for common protein ID column names first
    primary_candidates <- c("protein_id", "proteinid", "uniprot", "uniprot_id", "accession",
                            "pg.proteingroups", "proteingroups")
    primary_col <- NULL
    for (cand in primary_candidates) {
      if (cand %in% tolower(names(ids))) {
        primary_col <- names(ids)[tolower(names(ids)) == cand][1]
        break
      }
    }
    # Fallback to first column
    if (is.null(primary_col)) primary_col <- names(ids)[1]

    # Look for gene symbol column
    cand_gene_cols <- c("gene_symbol", "gene", "symbol", "geneid", "gene_id", "gene_name",
                        "pg.genes", "genes")
    gene_col <- NULL
    for (cand in cand_gene_cols) {
      matches <- names(ids)[tolower(names(ids)) == cand]
      if (length(matches) > 0) {
        gene_col <- matches[1]
        break
      }
    }
    if (is.null(gene_col)) {
      # Fallback: look for columns containing "gene" or "symbol" (case-insensitive)
      gene_matches <- names(ids)[grepl("gene|symbol", names(ids), ignore.case = TRUE)]
      gene_col <- setdiff(gene_matches, primary_col)
      gene_col <- gene_col[1] %||% NULL
    }

    # FIX: If rownames were NULL/missing and row counts match, use direct 1:1 mapping first.
    # This handles cases where the matrix doesn't have rownames but ids table has the data.
    use_direct_mapping <- rownames_were_null && nrow(ids) == length(protein_id)

    if (use_direct_mapping && !is.null(gene_col) && gene_col %in% names(ids)) {
      # Direct 1:1 correspondence: row i of matrix corresponds to row i of ids
      cand <- as.character(ids[[gene_col]])
      ok <- !is.na(cand) & nzchar(cand)
      label_id[ok] <- cand[ok]
      # Also update protein_id from ids table for better display
      if (!is.null(primary_col) && primary_col %in% names(ids)) {
        prot_cand <- as.character(ids[[primary_col]])
        prot_ok <- !is.na(prot_cand) & nzchar(prot_cand)
        protein_id[prot_ok] <- prot_cand[prot_ok]
      }
    } else if (!is.null(gene_col) && gene_col %in% names(ids) &&
               !is.null(primary_col) && primary_col %in% names(ids)) {
      # Build lookup map: primary_id -> gene_symbol
      map <- stats::setNames(as.character(ids[[gene_col]]), as.character(ids[[primary_col]]))
      mapped <- unname(map[protein_id])
      ok <- !is.na(mapped) & nzchar(mapped)
      label_id[ok] <- mapped[ok]
    } else if (nrow(ids) == length(protein_id)) {
      # Fallback: if row counts match, assume 1:1 correspondence
      if (!is.null(gene_col) && gene_col %in% names(ids)) {
        cand <- as.character(ids[[gene_col]])
        ok <- !is.na(cand) & nzchar(cand)
        label_id[ok] <- cand[ok]
      }
    }
  }

  smeta$sample_col <- as.character(smeta$sample_col)
  smeta$group_name <- as.character(smeta$group_name %||% smeta$group)
  smeta$replicate <- suppressWarnings(as.integer(smeta$replicate %||% NA))
  smeta <- smeta[!is.na(smeta$sample_col) & nzchar(smeta$sample_col), , drop = FALSE]
  smeta <- smeta[smeta$sample_col %in% colnames(mat), , drop = FALSE]

  groups <- unique(smeta$group_name)
  groups <- groups[nzchar(groups)]

  stat_list <- list()
  for (g in groups) {
    gm <- smeta[smeta$group_name == g, , drop = FALSE]
    cols <- intersect(as.character(gm$sample_col), colnames(mat))
    if (length(cols) == 0) next

    grp_mat <- mat[, cols, drop = FALSE]
    raw_vals <- as.matrix(grp_mat)
    raw_vals[!is.finite(raw_vals) | raw_vals <= 0] <- NA_real_

    mean_raw <- rowMeans(raw_vals, na.rm = TRUE)
    sd_raw <- apply(raw_vals, 1, stats::sd, na.rm = TRUE)
    n_obs <- apply(raw_vals, 1, function(x) sum(is.finite(x)))
    # Apply min_replicates filter: proteins with n_obs < min_replicates get NA CV%
    cv_pct <- ifelse(is.finite(mean_raw) & mean_raw > 0 & n_obs >= min_replicates, 100 * sd_raw / mean_raw, NA_real_)

    stat_list[[g]] <- data.frame(
      protein_id = protein_id,
      label_id = label_id,
      group = g,
      abundance = tx(mean_raw),
      mean_abundance_raw = mean_raw,  # Raw numeric for export
      cv_pct = cv_pct,
      n_detected = n_obs,
      stringsAsFactors = FALSE
    )
  }

  df <- if (length(stat_list) > 0) do.call(rbind, stat_list) else data.frame()
  if (nrow(df) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No CV% data available",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_cv_plot = p_empty),
      tables = list(idquant_cv_table = data.frame())
    ))
  }

  df$high_cv <- is.finite(df$cv_pct) & df$cv_pct >= cv_threshold

  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"

  x_axis_mode <- style$x_axis_mode %||% "abundance"
  if (!x_axis_mode %in% c("abundance", "rank")) x_axis_mode <- "abundance"
  xlab <- tb_log_axis_label("Abundance", transform)

  df_plot <- df[is.finite(df$abundance) & is.finite(df$cv_pct), , drop = FALSE]
  df_plot$group <- factor(df_plot$group)

  # Check if all data was filtered out due to insufficient replicates
  if (nrow(df_plot) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = sprintf("No proteins with >= %d replicates\n(try lowering 'Minimum replicates' in style)", min_replicates),
                        size = 5, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_cv_plot = p_empty),
      tables = list(idquant_cv_table = data.frame())
    ))
  }

  x_col <- "abundance"
  if (x_axis_mode == "rank") {
    by_protein <- stats::aggregate(abundance ~ protein_id, data = df_plot, FUN = mean)
    by_protein <- by_protein[is.finite(by_protein$abundance) & nzchar(by_protein$protein_id), , drop = FALSE]
    # Rank increasing with abundance (highest abundance = highest rank)
    by_protein <- by_protein[order(by_protein$abundance, by_protein$protein_id), , drop = FALSE]
    by_protein$abundance_rank <- seq_len(nrow(by_protein))
    df_plot$abundance_rank <- by_protein$abundance_rank[match(df_plot$protein_id, by_protein$protein_id)]
    df_plot <- df_plot[is.finite(df_plot$abundance_rank), , drop = FALSE]
    x_col <- "abundance_rank"
    xlab <- "Abundance rank"
  }

  point_color_mode <- style$point_color_mode %||% "group"
  if (!point_color_mode %in% c("group", "flat")) point_color_mode <- "group"

  point_alpha <- suppressWarnings(as.numeric(style$point_alpha %||% 0.7))
  if (!is.finite(point_alpha)) point_alpha <- 0.7
  point_alpha <- max(0, min(1, point_alpha))

  point_size <- suppressWarnings(as.numeric(style$point_size %||% 1.5))
  if (!is.finite(point_size) || point_size <= 0) point_size <- 1.5

  vthreshold_show <- isTRUE(style$vthreshold_show %||% FALSE)
  vthreshold_x <- suppressWarnings(as.numeric(style$vthreshold_x %||% 3))
  if (!is.finite(vthreshold_x)) vthreshold_x <- 3
  smooth_show <- isTRUE(style$smooth_show %||% TRUE)
  show_below_count <- isTRUE(style$show_below_count %||% TRUE)
  threshold_color <- as.character(style$threshold_color %||% "gray60")
  threshold_width <- suppressWarnings(as.numeric(style$threshold_width %||% 0.5))
  if (!is.finite(threshold_width) || threshold_width < 0) threshold_width <- 0.5
  threshold_linetype <- "dashed"

  flat_color <- as.character(style$flat_color %||% "#808080")

  if (point_color_mode == "group") {
    group_colors <- results$data$group_colors %||% NULL
    if (is.null(group_colors) || length(group_colors) == 0) {
      group_colors <- scales::hue_pal()(nlevels(df_plot$group))
      names(group_colors) <- levels(df_plot$group)
    } else {
      missing <- levels(df_plot$group)[!levels(df_plot$group) %in% names(group_colors)]
      if (length(missing) > 0) {
        auto_colors <- grDevices::hcl.colors(length(missing), palette = "Dark 3")
        names(auto_colors) <- missing
        group_colors <- c(group_colors, auto_colors)
      }
    }
    group_colors <- group_colors[levels(df_plot$group)]

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = !!rlang::sym(x_col), y = cv_pct, color = group)) +
      ggplot2::geom_point(alpha = point_alpha, size = point_size) +
      ggplot2::scale_color_manual(values = group_colors, drop = FALSE)
  } else {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = !!rlang::sym(x_col), y = cv_pct)) +
      ggplot2::geom_point(alpha = point_alpha, size = point_size, color = flat_color, show.legend = FALSE)
  }

  if (vthreshold_show && identical(x_axis_mode, "abundance")) {
    p <- p + ggplot2::geom_vline(
      xintercept = vthreshold_x,
      color = threshold_color,
      linetype = threshold_linetype,
      linewidth = threshold_width
    )
  }

  if (smooth_show) {
    p <- p + ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      color = "black", linewidth = 0.7, show.legend = FALSE,
      inherit.aes = FALSE,
      mapping = ggplot2::aes(x = !!rlang::sym(x_col), y = cv_pct),
      data = df_plot
    )
  }

  if (show_below_count && vthreshold_show && identical(x_axis_mode, "abundance")) {
    n_below <- sum(df_plot$abundance < vthreshold_x, na.rm = TRUE)
    p <- p + ggplot2::annotate(
      "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.4,
      label = sprintf("n < %.2g: %d", vthreshold_x, n_below), size = 4
    )
  }

  x_min <- suppressWarnings(as.numeric(style$x_min %||% NA_real_))
  x_max <- suppressWarnings(as.numeric(style$x_max %||% NA_real_))
  use_manual_x <- is.finite(x_min) && is.finite(x_max)
  if (use_manual_x && x_min > x_max) {
    tmp <- x_min
    x_min <- x_max
    x_max <- tmp
  }
  x_scale <- if (use_manual_x) {
    ggplot2::scale_x_continuous(limits = c(x_min, x_max), expand = ggplot2::expansion(mult = c(0, 0.05)))
  } else {
    ggplot2::scale_x_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05)))
  }

  p <- p +
    x_scale +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = xlab, y = "CV%") +
    tb_theme_base(axis_text_size, axis_style = axis_style)

  if (show_labels) {
    hi <- df_plot[df_plot$high_cv, , drop = FALSE]
    if (nrow(hi) > 0) {
      hi <- hi[order(hi$cv_pct, decreasing = TRUE), , drop = FALSE]
      if (n_labels > 0) {
        hi <- hi[seq_len(min(n_labels, nrow(hi))), , drop = FALSE]
      } else {
        hi <- hi[0, , drop = FALSE]
      }
      p <- p + ggplot2::geom_text(
        data = hi,
        ggplot2::aes(label = label_id),
        size = 3,
        vjust = -0.4,
        check_overlap = TRUE,
        show.legend = FALSE
      )
    }
  }

  # Stacked bar: CV% bucket distribution per group
  # Build breaks and labels from cv_bin_1..5 style fields, respecting num_bins
  all_thresholds <- c(cv_bin_1, cv_bin_2, cv_bin_3, cv_bin_4, cv_bin_5)
  # Use only the first (num_bins - 1) thresholds to create num_bins bins
  n_thresholds <- min(num_bins - 1L, 5L)
  used_thresholds <- all_thresholds[seq_len(n_thresholds)]
  breaks <- c(0, used_thresholds, Inf)
  # Generate dynamic labels based on bin thresholds
  bin_labels <- character(num_bins)
  bin_labels[1] <- sprintf("0\u2013%.0f%%", used_thresholds[1])
  if (n_thresholds >= 2) {
    for (i in 2:n_thresholds) {
      bin_labels[i] <- sprintf("%.0f\u2013%.0f%%", used_thresholds[i - 1], used_thresholds[i])
    }
  }
  bin_labels[num_bins] <- sprintf("%.0f%%+", used_thresholds[n_thresholds])

  df_bucket <- df_plot
  df_bucket$bucket <- cut(df_bucket$cv_pct, breaks = breaks, right = FALSE, include.lowest = TRUE, labels = bin_labels)
  df_bucket <- df_bucket[is.finite(df_bucket$cv_pct) & !is.na(df_bucket$bucket), , drop = FALSE]
  df_bucket$n <- 1L
  # Store bin label for export
  df$cv_bin_label <- cut(df$cv_pct, breaks = breaks, right = FALSE, include.lowest = TRUE, labels = bin_labels)

  agg <- stats::aggregate(n ~ group + bucket, data = df_bucket, FUN = sum)
  agg$group <- factor(agg$group)
  # Reverse bin_labels so lowest CV% bin (0-10%) is at the bottom of stacked bars
  agg$bucket <- factor(agg$bucket, levels = rev(bin_labels))

  totals <- stats::aggregate(n ~ group, data = agg, FUN = sum)
  names(totals)[names(totals) == "n"] <- "n_total"
  agg <- merge(agg, totals, by = "group", all.x = TRUE)
  agg$pct <- ifelse(is.finite(agg$n_total) & agg$n_total > 0, 100 * agg$n / agg$n_total, NA_real_)

  # Determine Y-axis label based on data level (peptide vs protein)
  # Check analysis_level from node meta (most reliable), then fallback to other sources

  is_peptide_level <- identical(tolower(meta$analysis_level %||% ""), "peptide") ||
                      identical(tolower(meta$data_level %||% ""), "peptide") ||
                      isTRUE(meta$parent_engine_id %in% c("peptide_analysis", "dsilac_peptide_analysis")) ||
                      isTRUE(results$params$parent_engine_id %in% c("peptide_analysis", "dsilac_peptide_analysis")) ||
                      identical(tolower(results$params$data_level %||% ""), "peptide") ||
                      identical(tolower(results$data$analysis_level %||% ""), "peptide")
  data_level <- if (is_peptide_level) "Peptides" else "Proteins"
  y_axis_label <- sprintf("%% of %s", data_level)

  # Extract custom bin colors from style (with defaults)
  cv_bin_colors <- c(
    style$cv_bin_1_color %||% "#2166AC",
    style$cv_bin_2_color %||% "#67A9CF",
    style$cv_bin_3_color %||% "#D1E5F0",
    style$cv_bin_4_color %||% "#FDDBC7",
    style$cv_bin_5_color %||% "#EF8A62",
    style$cv_bin_6_color %||% "#B2182B"
  )
  # Use only the colors needed for the number of bins
  bin_colors <- cv_bin_colors[seq_len(num_bins)]
  names(bin_colors) <- bin_labels
  # Reverse to match the reversed factor levels (lowest CV at bottom)
  bin_colors_reversed <- rev(bin_colors)

  p_bar <- ggplot2::ggplot(agg, ggplot2::aes(x = group, y = pct, fill = bucket)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::scale_fill_manual(values = bin_colors_reversed) +
    ggplot2::labs(x = NULL, y = y_axis_label, fill = "CV%") +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    tb_theme_base(axis_text_size, axis_style = axis_style)

  if (show_avg_cv) {
    avg_cv <- stats::aggregate(cv_pct ~ group, data = df_bucket, FUN = mean, na.rm = TRUE)
    names(avg_cv) <- c("group", "avg_cv")
    avg_cv$label <- sprintf("%.1f%%", avg_cv$avg_cv)
    avg_cv$group <- factor(avg_cv$group)
    p_bar <- p_bar +
      ggplot2::geom_text(
        data = avg_cv,
        ggplot2::aes(x = group, y = 100, label = label),
        inherit.aes = FALSE,
        vjust = -0.3,
        size = axis_text_size / 5,
        fontface = "bold"
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # Compose plot(s) based on cv_plot_mode
  p_out <- if (cv_plot_mode == "scatter") {
    p
  } else if (cv_plot_mode == "bar") {
    p_bar
  } else {
    # "both" - combine scatter + stacked bar
    tb_require_pkg("ggplotify")
    g_scatter <- ggplot2::ggplotGrob(p)
    g_bar <- ggplot2::ggplotGrob(p_bar)
    p_combined <- suppressWarnings(ggplotify::as.ggplot(rbind(g_scatter, g_bar, size = "first")))
    attr(p_combined, "scatter_plot") <- p
    attr(p_combined, "bar_plot") <- p_bar
    p_combined
  }

  tbl <- df[df$high_cv, , drop = FALSE]
  tbl <- tbl[order(tbl$group, -tbl$cv_pct), , drop = FALSE]
  rownames(tbl) <- NULL

  if (nrow(tbl) > 0 && "abundance" %in% names(tbl)) {
    tbl$abundance <- format_k_suffix(tbl$abundance)
  }

  list(
    plots = list(idquant_cv_plot = p_out),
    tables = list(idquant_cv_table = tbl)
  )
}

tb_render_idquant_cv_bar <- function(results, style, meta) {
  style <- style %||% list()
  style$cv_plot_mode <- "bar"
  out <- tb_render_idquant_cv(results, style, meta)
  out$tables <- list()
  out
}

tb_render_idquant_cv_scatter <- function(results, style, meta) {
  style <- style %||% list()
  style$cv_plot_mode <- "scatter"
  tb_render_idquant_cv(results, style, meta)
}
#' Render IDQuant overlap child view
#'
#' Produces an overlap diagram across groups using either:
#' - "upset": an UpSet-style intersection bar plot (uses UpSetR if available for compatibility)
#' - "venn": a Venn diagram (ggVennDiagram)
#'
#' If neither optional package is available, returns an informative ggplot message.
#'
#' @param results Engine results (from stats_idquant_run)
#' @param style Viewer-time style overrides
#' @param meta Node metadata
#' @return list(plots, tables)
tb_render_idquant_overlap <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  mat <- results$data$intensity_mat
  smeta <- results$data$sample_meta

  if (is.null(mat) || !is.matrix(mat) || is.null(smeta) || !is.data.frame(smeta)) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "IDQuant raw intensity data missing (legacy run)",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_overlap_plot = p_empty),
      tables = list(idquant_overlap_table = data.frame())
    ))
  }

  protein_id <- rownames(mat)
  if (is.null(protein_id) || any(!nzchar(protein_id))) {
    protein_id <- paste0("protein_", seq_len(nrow(mat)))
  }

  smeta$sample_col <- as.character(smeta$sample_col)
  smeta$group_name <- as.character(smeta$group_name %||% smeta$group)
  smeta <- smeta[!is.na(smeta$sample_col) & nzchar(smeta$sample_col), , drop = FALSE]
  smeta <- smeta[smeta$sample_col %in% colnames(mat), , drop = FALSE]

  groups <- unique(smeta$group_name)
  groups <- groups[nzchar(groups)]

  overlap_metric <- tolower(as.character(results$params$overlap_metric %||% "detected"))
  use_all_reps <- identical(overlap_metric, "quantified")

  # Build per-group presence sets
  sets <- list()
  for (g in groups) {
    cols <- smeta$sample_col[smeta$group_name == g]
    cols <- intersect(as.character(cols), colnames(mat))
    if (length(cols) == 0) next

    grp_mat <- mat[, cols, drop = FALSE]
    present <- if (isTRUE(use_all_reps)) {
      apply(grp_mat, 1, function(x) all(is.finite(x) & !is.na(x) & x > 0))
    } else {
      apply(grp_mat, 1, function(x) any(is.finite(x) & !is.na(x) & x > 0))
    }
    sets[[g]] <- protein_id[present]
  }

  if (length(sets) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No overlap data available",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(idquant_overlap_plot = p_empty),
      tables = list(idquant_overlap_table = data.frame())
    ))
  }

  # Table: group set sizes
  size_tbl <- data.frame(
    group = names(sets),
    n_present = vapply(sets, length, integer(1)),
    stringsAsFactors = FALSE
  )
  size_tbl <- size_tbl[order(size_tbl$n_present, decreasing = TRUE), , drop = FALSE]
  rownames(size_tbl) <- NULL
  size_tbl$n_present <- format_k_suffix(size_tbl$n_present)

  make_msg_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, size = 5, color = "gray50") +
      ggplot2::theme_void()
  }

  plot_type <- tolower(as.character(style$overlap_plot_type %||% results$params$overlap_plot_type %||% "upset"))
  if (plot_type == "venn") {
    if (length(sets) > 6) {
      plot_type <- "upset"
    }
  }

  if (plot_type == "venn") {
    if (!requireNamespace("ggvenn", quietly = TRUE)) {
      p <- make_msg_plot("Venn diagram requires the 'ggvenn' package.")
      return(list(
        plots = list(idquant_overlap_plot = p),
        tables = list(idquant_overlap_table = size_tbl)
      ))
    }

    group_colors <- results$data$group_colors %||% NULL
    if (is.null(group_colors) || length(group_colors) == 0) {
      group_colors <- grDevices::hcl.colors(length(sets), palette = "Dark 3")
      names(group_colors) <- names(sets)
    }
    fill_colors <- group_colors[names(sets)]
    fill_colors[is.na(fill_colors) | !nzchar(fill_colors)] <- "#B0B0B0"

    venn_fill_alpha <- tb_num(style$venn_fill_alpha, 0.4)
    if (!is.finite(venn_fill_alpha)) venn_fill_alpha <- 0.4
    venn_fill_alpha <- max(0, min(1, venn_fill_alpha))
    venn_outline_size <- tb_num(style$venn_outline_size, 0.4)
    if (!is.finite(venn_outline_size) || venn_outline_size < 0) venn_outline_size <- 0.4
    venn_outline_color <- as.character(style$venn_outline_color %||% "#000000")
    venn_set_name_size <- tb_num(style$venn_set_name_size, 0)
    if (!is.finite(venn_set_name_size) || venn_set_name_size < 0) venn_set_name_size <- 0
    venn_text_size <- tb_num(style$venn_text_size, 4)
    if (!is.finite(venn_text_size) || venn_text_size < 0) venn_text_size <- 4

    show_percentage <- isTRUE(style$venn_show_percentage %||% FALSE)

    ggvenn_fun <- getFromNamespace("ggvenn", "ggvenn")
    ggvenn_args <- names(formals(ggvenn_fun))

    venn_args <- list(
      fill_color = unname(fill_colors),
      fill_alpha = venn_fill_alpha,
      stroke_size = venn_outline_size,
      stroke_color = venn_outline_color,
      stroke_colour = venn_outline_color,
      set_name_size = venn_set_name_size,
      text_size = venn_text_size,
      show_percentage = show_percentage,
      show_percent = show_percentage
    )
    venn_args <- venn_args[names(venn_args) %in% ggvenn_args]

    p <- do.call(ggvenn_fun, c(list(sets), venn_args))
    p <- p +
      ggplot2::scale_x_continuous(breaks = NULL, labels = NULL) +
      ggplot2::scale_y_continuous(breaks = NULL, labels = NULL)
    p <- p + ggplot2::theme_void() +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(0, 0, 0, 0)
      )

    return(list(
      plots = list(idquant_overlap_plot = p),
      tables = list(idquant_overlap_table = size_tbl)
    ))
  }

  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"
  count_labels_show <- isTRUE(style$count_labels_show %||% FALSE)
  count_labels_size <- suppressWarnings(as.numeric(style$count_labels_size %||% 3.5))
  if (!is.finite(count_labels_size) || count_labels_size <= 0) count_labels_size <- 3.5
  count_labels_color <- as.character(style$count_labels_color %||% "black")
  # show_title and transform are deprecated/removed fields - silently ignore for backward compatibility
  show_bar_outline <- isTRUE(style$show_bar_outline %||% FALSE)
  bar_outline_color <- as.character(style$bar_outline_color %||% "#000000")
  bar_outline_width <- tb_num(style$bar_outline_width, 0.8)
  if (!is.finite(bar_outline_width) || bar_outline_width < 0) bar_outline_width <- 0.8
  bar_fill_color <- as.character(style$bar_fill_color %||% "#4245FF")

  # UpSet-only (with matrix display)
  all_proteins <- unique(unlist(sets, use.names = FALSE))
  if (length(all_proteins) == 0) {
    p <- make_msg_plot("No proteins present in any group")
    return(list(
      plots = list(idquant_overlap_plot = p),
      tables = list(idquant_overlap_table = size_tbl)
    ))
  }

  group_names <- names(sets)
  M <- vapply(group_names, function(g) all_proteins %in% sets[[g]], logical(length(all_proteins)))
  if (is.null(dim(M))) {
    M <- matrix(M, ncol = length(group_names))
    colnames(M) <- group_names
  }

  keys <- apply(M, 1, function(r) paste0(ifelse(r, "1", "0"), collapse = ""))
  tab <- sort(table(keys), decreasing = TRUE)
  if (length(tab) == 0) {
    p <- make_msg_plot("No overlap intersections available")
    return(list(
      plots = list(idquant_overlap_plot = p),
      tables = list(idquant_overlap_table = size_tbl)
    ))
  }

  top_n <- min(20, length(tab))
  top_keys <- names(tab)[seq_len(top_n)]
  top_sizes <- as.integer(tab[top_keys])
  top_labels <- vapply(top_keys, function(k) {
    bits <- strsplit(k, "", fixed = TRUE)[[1]] == "1"
    on <- group_names[bits]
    if (length(on) == 0) "(none)" else paste(on, collapse = " & ")
  }, character(1))

  df_int <- data.frame(
    intersection = top_labels,
    n = top_sizes,
    key = top_keys,
    stringsAsFactors = FALSE
  )
  df_int$intersection <- factor(df_int$intersection, levels = rev(df_int$intersection))

  y_expand_mult <- if (count_labels_show) c(0, 0.15) else c(0, 0.05)
  bar_outline <- if (show_bar_outline) bar_outline_color else NA
  bar_outline_lw <- if (show_bar_outline) bar_outline_width else 0
  p_top <- ggplot2::ggplot(df_int, ggplot2::aes(x = intersection, y = n)) +
    ggplot2::geom_col(fill = bar_fill_color, color = bar_outline, linewidth = bar_outline_lw) +
    ggplot2::labs(x = NULL, y = "Count") +
    ggplot2::scale_y_continuous(labels = format_k_suffix, expand = ggplot2::expansion(mult = y_expand_mult)) +
    tb_theme_base(axis_text_size, axis_style = axis_style) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10))
  if (count_labels_show) {
    p_top <- p_top + ggplot2::geom_text(
      ggplot2::aes(label = n),
      vjust = -0.3,
      size = count_labels_size,
      color = count_labels_color
    )
    p_top <- p_top + ggplot2::coord_cartesian(clip = "off")
  }
  # Note: show_title removed from schema; old terpbooks with show_title field will have it silently ignored

  df_matrix <- do.call(rbind, lapply(seq_along(top_keys), function(i) {
    bits <- strsplit(top_keys[[i]], "", fixed = TRUE)[[1]] == "1"
    data.frame(
      intersection = top_labels[[i]],
      group = group_names,
      present = bits,
      stringsAsFactors = FALSE
    )
  }))
  df_matrix$intersection <- factor(df_matrix$intersection, levels = levels(df_int$intersection))
  df_matrix$group <- factor(df_matrix$group, levels = rev(group_names))
  df_matrix$y <- as.integer(df_matrix$group)

  df_present <- df_matrix[df_matrix$present, , drop = FALSE]
  segs <- NULL
  if (nrow(df_present) > 0) {
    segs <- do.call(rbind, lapply(split(df_present, df_present$intersection), function(d) {
      data.frame(
        intersection = unique(d$intersection),
        ymin = min(d$y),
        ymax = max(d$y),
        stringsAsFactors = FALSE
      )
    }))
  }

  p_matrix <- ggplot2::ggplot(df_matrix, ggplot2::aes(x = intersection, y = y)) +
    ggplot2::geom_segment(
      data = segs,
      ggplot2::aes(x = intersection, xend = intersection, y = ymin, yend = ymax),
      inherit.aes = FALSE,
      linewidth = 0.8,
      color = "gray40"
    ) +
    ggplot2::geom_point(ggplot2::aes(color = present), size = 3) +
    ggplot2::scale_color_manual(values = c("FALSE" = "gray85", "TRUE" = "black"), guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels(df_matrix$group)),
      labels = levels(df_matrix$group),
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    tb_theme_base(axis_text_size, axis_style = axis_style) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  tb_require_pkg("ggplotify")
  g_top <- ggplot2::ggplotGrob(p_top)
  g_mat <- ggplot2::ggplotGrob(p_matrix)

  # 58/42 height ratio (UpSet bars vs. matrix) using rbind for reliable stacking
  # Increased matrix space to prevent top cutoff with many groups
  # First, align the widths of both grobs
  max_widths <- grid::unit.pmax(g_top$widths, g_mat$widths)
  g_top$widths <- max_widths
  g_mat$widths <- max_widths

  # Combine vertically using rbind (S3 method for gtable class)
  g_combined <- rbind(g_top, g_mat, size = "first")

  # Set relative heights: 58% for bars, 42% for matrix
  # Find panel rows in the combined grob
  panel_rows_top <- which(grepl("panel", g_top$layout$name))
  panel_rows_mat <- which(grepl("panel", g_mat$layout$name))

  # Apply 58/42 split to the combined grob heights
  n_rows_top <- nrow(g_top)
  n_rows_mat <- nrow(g_mat)
  # Scale the heights proportionally
  g_combined$heights[seq_len(n_rows_top)] <- g_combined$heights[seq_len(n_rows_top)] * 0.58 / 0.5

  g_combined$heights[seq(n_rows_top + 1, n_rows_top + n_rows_mat)] <- g_combined$heights[seq(n_rows_top + 1, n_rows_top + n_rows_mat)] * 0.42 / 0.5

  p <- suppressWarnings(ggplotify::as.ggplot(g_combined))
  attr(p, "tb_skip_force_black_text") <- TRUE
  attr(p, "top_plot") <- p_top
  attr(p, "matrix_plot") <- p_matrix

  list(
    plots = list(idquant_overlap_plot = p),
    tables = list(idquant_overlap_table = size_tbl)
  )
}

tb_render_hor_dis <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  df <- results$data$values
  if (is.null(df) || !is.data.frame(df)) stop("hor_dis results$data$values missing.")
  if (is.null(df$value)) stop("hor_dis values require 'value' column.")

  summary_df <- results$data$summary
  df$group <- factor(df$group %||% "group")

  # log_transform: style is the single source of truth at render time.
  # If compute stored data under a transform (applied_transform), reverse it first,
  # then apply the style's choice. Legacy results (no flag) are assumed raw.
  applied_transform <- results$data$applied_transform %||% "none"
  log_transform <- style$log_transform %||% "log10"

  if (applied_transform == "log10") {
    df$value <- 10^(df$value)
  } else if (applied_transform == "log2") {
    df$value <- 2^(df$value)
  }
  if (log_transform == "log10") {
    df$value[df$value <= 0] <- NA
    df$value <- log10(df$value)
  } else if (log_transform == "log2") {
    df$value[df$value <= 0] <- NA
    df$value <- log2(df$value)
  }
  df <- df[is.finite(df$value), , drop = FALSE]

  # Get style options
  alpha_val <- tb_num(style$alpha, 0.8)
  show_mean <- isTRUE(style$show_mean %||% TRUE)
  show_mean_value <- isTRUE(style$show_mean_value %||% TRUE)
  mean_line_size <- tb_num(style$mean_line_size, 1)
  mean_text_size <- tb_num(style$mean_text_size, 10)
  plot_type <- style$plot_type %||% "density"
  layout_mode <- style$layout %||% "overlay"
  show_group_names <- isTRUE(style$show_group_names %||% TRUE)
  bins_n <- as.integer(tb_num(style$bins, 30))
  if (is.na(bins_n) || bins_n < 5) bins_n <- 30

  # Compare mode handling for individual replicate display
  compare_mode <- results$params$compare_mode %||% "avg_groups"
  selected_group <- style$selected_group %||% NULL

  # FIX: Store original group levels and get colors BEFORE filtering
  # This ensures within_groups mode preserves the original color for each group
  # Use engine-provided group_colors if available (from metadata), otherwise generate
  original_group_levels <- levels(df$group)
  n_original_groups <- length(original_group_levels)
  if (!is.null(results$data$group_colors) && length(results$data$group_colors) > 0) {
    # Use metadata-specified colors from engine output
    group_colors <- results$data$group_colors
    # Ensure all groups have colors (fill missing with auto-generated)
    missing <- original_group_levels[!original_group_levels %in% names(group_colors)]
    if (length(missing) > 0) {
      auto_colors <- grDevices::hcl.colors(length(missing), palette = "Dark 3")
      names(auto_colors) <- missing
      group_colors <- c(group_colors, auto_colors)
    }
  } else {
    # Fallback to auto-generated colors; guard against empty factor levels
    if (n_original_groups < 1 || length(original_group_levels) == 0) {
      group_colors <- c(`(all)` = "#808080")
    } else {
      group_colors <- scales::hue_pal()(n_original_groups)
      names(group_colors) <- original_group_levels
    }
  }

  # For all_reps and within_groups modes, create replicate-level series
  # Create a series_id column for grouping in the plot
  if (compare_mode == "all_reps") {
    # All replicates: show individual curves for every replicate
    # Label format: "GroupName ReplicateNumber"
    if (!is.null(df$replicate) && !all(is.na(df$replicate))) {
      df$series_id <- paste(df$group, df$replicate)
    } else {
      # Fallback: use sample column as series
      df$series_id <- df$sample %||% df$group
    }
    df$series_id <- factor(df$series_id)
  } else if (compare_mode == "within_groups" && !is.null(selected_group) && nzchar(selected_group)) {
    # Within groups: show only the selected group's replicates
    df <- df[df$group == selected_group, , drop = FALSE]
    # FIX: Also filter summary_df to match selected group for mean calculations
    if (!is.null(summary_df) && is.data.frame(summary_df)) {
      summary_df <- summary_df[summary_df$group == selected_group, , drop = FALSE]
    }
    if (nrow(df) == 0) {
      # No data for selected group - return empty plot
      p_empty <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste0("No data for group: ", selected_group),
                          size = 6, color = "gray50") +
        ggplot2::theme_void()
      return(list(plots = list(hor_dis_plot = p_empty), tables = list()))
    }
    # FIX: Drop unused factor levels so mean calculation uses only selected group
    df$group <- droplevels(df$group)
    if (!is.null(df$replicate) && !all(is.na(df$replicate))) {
      df$series_id <- paste(df$group, df$replicate)
    } else {
      df$series_id <- df$sample %||% df$group
    }
    df$series_id <- factor(df$series_id)
  } else {
    # avg_groups mode: use group as series
    df$series_id <- df$group
  }

  # Save original values BEFORE pooling for mean calculation
  # Mean should be calculated on all values, not just the capped ones
  original_values <- df$value

  # Pooling controls (viewer-time)
  pool_above <- isTRUE(style$pool_above %||% TRUE)
  pool_value <- tb_num(style$pool_value, 12)
  if (pool_above) {
    # Apply pooling: values above threshold are capped at pool_value
    # Use pool_value - small epsilon to ensure they render within axis bounds
    pool_cap <- pool_value - 0.01
    df$value[df$value > pool_value] <- pool_cap
  }

  # Color mode handling
  color_mode <- style$color_mode %||% "group"
  flat_color <- style$flat_color %||% "#B0B0B0"

  # Build plot based on plot_type
  # FIX: Always use 'group' for color mapping so replicates inherit their group's color
  # Use series_id only for separating curves/histograms (via linetype or position)
  # This ensures all_reps and within_groups modes show replicates in their parent group color

  if (plot_type == "histogram") {
    # Histogram mode with thin black outlines
    if (color_mode == "flat") {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(fill = flat_color, color = "black",
                                alpha = alpha_val, bins = bins_n, linewidth = 0.3)
    } else {
      # Use group for fill, but keep outline black
      p <- ggplot2::ggplot(df, ggplot2::aes(x = value, fill = group, group = series_id)) +
        ggplot2::geom_histogram(alpha = alpha_val, position = "identity", bins = bins_n, color = "black", linewidth = 0.3)
    }
    # Use larger y-axis expansion when mean value labels are shown to prevent clipping
    # Extra expansion needed for all_reps overlay mode with staggered labels
    if (show_mean_value) {
      n_series <- length(unique(df$series_id))
      if (compare_mode == "all_reps" && layout_mode == "overlay" && n_series > 1) {
        # Scale expansion based on number of staggered labels (0.10 per label + base 0.25)
        y_top_expand <- min(0.25 + (n_series - 1) * 0.10, 0.90)  # Cap at 90%
      } else {
        y_top_expand <- 0.22
      }
      y_expand <- ggplot2::expansion(mult = c(0, y_top_expand))
    } else {
      y_expand <- ggplot2::expansion(mult = c(0, 0.12))
    }
    p <- p + ggplot2::scale_y_continuous(labels = format_k_suffix, expand = y_expand)
    y_label <- "Count"
  } else {
    # Density mode (default) - always filled with matching color outline
    if (color_mode == "flat") {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
        ggplot2::geom_density(fill = flat_color, color = flat_color,
                              alpha = alpha_val, linewidth = 0.8)
    } else {
      # FIX: Use group for color, but group by series_id for separate density curves
      p <- ggplot2::ggplot(df, ggplot2::aes(x = value, fill = group, color = group, group = series_id)) +
        ggplot2::geom_density(alpha = alpha_val, linewidth = 0.8)
    }
    # Use larger y-axis expansion when mean value labels are shown to prevent clipping
    # Extra expansion needed for all_reps overlay mode with staggered labels
    if (show_mean_value) {
      n_series <- length(unique(df$series_id))
      if (compare_mode == "all_reps" && layout_mode == "overlay" && n_series > 1) {
        # Scale expansion based on number of staggered labels (0.10 per label + base 0.25)
        y_top_expand <- min(0.25 + (n_series - 1) * 0.10, 0.90)  # Cap at 90%
      } else {
        y_top_expand <- 0.22
      }
      y_expand <- ggplot2::expansion(mult = c(0, y_top_expand))
    } else {
      y_expand <- ggplot2::expansion(mult = c(0, 0.12))
    }
    p <- p + ggplot2::scale_y_continuous(labels = format_k_suffix, expand = y_expand)
    y_label <- "Density"
  }

  # Prepare mean values data if summary exists (used by both mean line and mean value label)
  # Support mean_type: "arithmetic" (default) or "harmonic" - now from style_schema
  mean_type <- style$mean_type %||% "arithmetic"

  mean_vals <- NULL
  # FIX: For all_reps mode, compute mean per series_id (each replicate gets its own mean)
  # For avg_groups/within_groups modes, compute mean per group
  if (compare_mode == "all_reps") {
    # all_reps mode: compute mean per series_id (replicate-level)
    current_series <- unique(as.character(df$series_id))
    if (length(current_series) > 0) {
      if (mean_type == "harmonic") {
        series_means <- vapply(current_series, function(s) {
          # Use original_values (before pooling) for mean calculation
          vals <- original_values[df$series_id == s]
          vals <- vals[is.finite(vals) & vals > 0]
          if (length(vals) == 0) return(NA_real_)
          length(vals) / sum(1 / vals)
        }, numeric(1))
      } else if (mean_type == "median") {
        series_means <- vapply(current_series, function(s) {
          vals <- original_values[df$series_id == s]
          vals <- vals[is.finite(vals)]
          if (length(vals) == 0) return(NA_real_)
          median(vals, na.rm = TRUE)
        }, numeric(1))
      } else {
        # Arithmetic mean per series
        series_means <- vapply(current_series, function(s) {
          # Use original_values (before pooling) for mean calculation
          vals <- original_values[df$series_id == s]
          vals <- vals[is.finite(vals)]
          if (length(vals) == 0) return(NA_real_)
          mean(vals, na.rm = TRUE)
        }, numeric(1))
      }
      mean_vals <- data.frame(
        series_id = current_series,
        mean_val = series_means,
        stringsAsFactors = FALSE
      )
      mean_vals$series_id <- factor(mean_vals$series_id, levels = levels(df$series_id))
    }
  } else if (!is.null(summary_df) && is.data.frame(summary_df) && nrow(summary_df) > 0) {
    # avg_groups or within_groups mode: compute mean per group
    current_groups <- unique(as.character(df$group))
    summary_df <- summary_df[summary_df$group %in% current_groups, , drop = FALSE]

    if (nrow(summary_df) > 0) {
      summary_df$group <- factor(summary_df$group, levels = current_groups)

      # For harmonic mean or median, recompute from raw data
      if (mean_type == "harmonic") {
        # Compute harmonic mean per group: n / sum(1/x) for positive values
        # Use original_values (before pooling) for mean calculation
        harmonic_means <- vapply(current_groups, function(g) {
          vals <- original_values[df$group == g]
          vals <- vals[is.finite(vals) & vals > 0]
          if (length(vals) == 0) return(NA_real_)
          length(vals) / sum(1 / vals)
        }, numeric(1))
        mean_vals <- data.frame(
          group = current_groups,
          mean_val = harmonic_means,
          stringsAsFactors = FALSE
        )
        mean_vals$group <- factor(mean_vals$group, levels = current_groups)
      } else if (mean_type == "median") {
        # Median per group from summary (precomputed) or raw data
        if ("median" %in% names(summary_df)) {
          mean_vals <- summary_df[, c("group", "median")]
          names(mean_vals) <- c("group", "mean_val")
        } else {
          median_vals <- vapply(current_groups, function(g) {
            vals <- original_values[df$group == g]
            vals <- vals[is.finite(vals)]
            if (length(vals) == 0) return(NA_real_)
            median(vals, na.rm = TRUE)
          }, numeric(1))
          mean_vals <- data.frame(
            group = current_groups,
            mean_val = median_vals,
            stringsAsFactors = FALSE
          )
        }
        mean_vals$group <- factor(mean_vals$group, levels = current_groups)
      } else {
        # Arithmetic mean (default) from summary
        mean_vals <- summary_df[, c("group", "mean")]
        names(mean_vals) <- c("group", "mean_val")
      }
    }
  }

  # Add mean lines if enabled (independent from mean value label)
  if (show_mean && !is.null(mean_vals) && nrow(mean_vals) > 0) {
    # For all_reps mode, mean_vals has series_id column; otherwise it has group column
    if (color_mode == "flat") {
      p <- p + ggplot2::geom_vline(
        data = mean_vals,
        ggplot2::aes(xintercept = mean_val),
        color = flat_color, linetype = "dashed",
        linewidth = mean_line_size, alpha = 0.8,
        inherit.aes = FALSE
      )
    } else if (compare_mode == "all_reps") {
      # all_reps mode: use dark dashed lines for mean (one per replicate)
      p <- p + ggplot2::geom_vline(
        data = mean_vals,
        ggplot2::aes(xintercept = mean_val),
        color = "#333333", linetype = "dashed",
        linewidth = mean_line_size, alpha = 0.8,
        inherit.aes = FALSE
      )
    } else if (compare_mode == "within_groups") {
      # within_groups mode: use dark dashed lines for mean (per group, but only selected group)
      p <- p + ggplot2::geom_vline(
        data = mean_vals,
        ggplot2::aes(xintercept = mean_val),
        color = "#333333", linetype = "dashed",
        linewidth = mean_line_size, alpha = 0.8,
        inherit.aes = FALSE
      )
    } else {
      # avg_groups mode: color by group
      p <- p + ggplot2::geom_vline(
        data = mean_vals,
        ggplot2::aes(xintercept = mean_val, color = group),
        linetype = "dashed", linewidth = mean_line_size, alpha = 0.8,
        inherit.aes = FALSE
      )
    }
  }

  # Add mean value text above curves/lines at max y + 5% if enabled (independent from mean line)
  if (show_mean_value && !is.null(mean_vals)) {
    mean_vals$mean_label <- signif(mean_vals$mean_val, 3)

    # Calculate max y for label positioning
    # For all_reps mode, iterate over series_id; otherwise iterate over group
    mean_vals$label_y <- NA_real_

    # Compute consistent bin edges that match ggplot2's geom_histogram(bins = bins_n)
    all_vals <- df$value[!is.na(df$value)]
    data_range <- range(all_vals, na.rm = TRUE)
    bin_width <- diff(data_range) / bins_n
    bin_edges <- seq(data_range[1], data_range[2] + bin_width * 0.001, by = bin_width)

    # Determine which column to use for iteration based on compare_mode
    if (compare_mode == "all_reps") {
      iter_col <- "series_id"
      iter_levels <- levels(df$series_id)
    } else {
      iter_col <- "group"
      iter_levels <- levels(df$group)
    }

    if (plot_type == "histogram") {
      if (layout_mode == "separate") {
        # Separated layout: each facet shows one series/group, compute per-series max
        for (lvl in iter_levels) {
          lvl_vals <- df$value[df[[iter_col]] == lvl]
          lvl_vals <- lvl_vals[!is.na(lvl_vals)]
          if (length(lvl_vals) > 0) {
            h <- graphics::hist(lvl_vals, breaks = bin_edges, plot = FALSE)
            lvl_max_y <- max(h$counts, na.rm = TRUE)
            mean_vals$label_y[mean_vals[[iter_col]] == lvl] <- lvl_max_y * 1.05
          } else {
            mean_vals$label_y[mean_vals[[iter_col]] == lvl] <- 1.05
          }
        }
      } else {
        # Overlay mode: all series on same panel, use combined max
        overall_max_y <- 0
        for (lvl in iter_levels) {
          lvl_vals <- df$value[df[[iter_col]] == lvl]
          lvl_vals <- lvl_vals[!is.na(lvl_vals)]
          if (length(lvl_vals) > 0) {
            h <- graphics::hist(lvl_vals, breaks = bin_edges, plot = FALSE)
            lvl_max_y <- max(h$counts, na.rm = TRUE)
            if (lvl_max_y > overall_max_y) overall_max_y <- lvl_max_y
          }
        }
        # Stagger labels vertically when multiple series to avoid overlap
        n_labels <- nrow(mean_vals)
        if (n_labels > 1 && compare_mode == "all_reps") {
          # Sort by mean value and assign staggered y positions
          mean_vals <- mean_vals[order(mean_vals$mean_val), ]
          stagger_step <- overall_max_y * 0.08
          mean_vals$label_y <- overall_max_y * 1.08 + (seq_len(n_labels) - 1) * stagger_step
        } else {
          mean_vals$label_y <- overall_max_y * 1.05
        }
      }
    } else {
      # Density mode
      if (layout_mode == "separate") {
        # Separated layout: each facet shows one series/group, compute per-series max
        for (lvl in iter_levels) {
          lvl_vals <- df$value[df[[iter_col]] == lvl]
          lvl_vals <- lvl_vals[!is.na(lvl_vals)]
          if (length(lvl_vals) > 1) {
            dens <- stats::density(lvl_vals, na.rm = TRUE)
            lvl_max_y <- max(dens$y, na.rm = TRUE)
            mean_vals$label_y[mean_vals[[iter_col]] == lvl] <- lvl_max_y * 1.05
          } else {
            mean_vals$label_y[mean_vals[[iter_col]] == lvl] <- 0.1
          }
        }
      } else {
        # Overlay mode: all series on same panel, use combined max
        overall_max_y <- 0
        for (lvl in iter_levels) {
          lvl_vals <- df$value[df[[iter_col]] == lvl]
          lvl_vals <- lvl_vals[!is.na(lvl_vals)]
          if (length(lvl_vals) > 1) {
            dens <- stats::density(lvl_vals, na.rm = TRUE)
            lvl_max_y <- max(dens$y, na.rm = TRUE)
            if (lvl_max_y > overall_max_y) overall_max_y <- lvl_max_y
          }
        }
        # Stagger labels vertically when multiple series to avoid overlap
        n_labels <- nrow(mean_vals)
        if (n_labels > 1 && compare_mode == "all_reps") {
          # Sort by mean value and assign staggered y positions
          mean_vals <- mean_vals[order(mean_vals$mean_val), ]
          stagger_step <- overall_max_y * 0.08
          mean_vals$label_y <- overall_max_y * 1.08 + (seq_len(n_labels) - 1) * stagger_step
        } else {
          mean_vals$label_y <- overall_max_y * 1.05
        }
      }
    }

    # Shift mean value label along X-axis so it doesn't overlap the vertical mean line
    x_range <- diff(range(df$value, na.rm = TRUE))
    x_offset <- x_range * 0.03
    mean_vals$label_x <- mean_vals$mean_val + x_offset

    # Render mean value labels
    if (color_mode == "flat") {
      p <- p + ggplot2::geom_text(
        data = mean_vals,
        ggplot2::aes(x = label_x, y = label_y, label = mean_label),
        color = flat_color, vjust = 0, hjust = 0,
        size = mean_text_size / 3, show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else if (compare_mode == "all_reps") {
      # all_reps mode: use dark color for text (one label per replicate)
      p <- p + ggplot2::geom_text(
        data = mean_vals,
        ggplot2::aes(x = label_x, y = label_y, label = mean_label),
        color = "#333333", vjust = 0, hjust = 0,
        size = mean_text_size / 3, show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else if (compare_mode == "within_groups") {
      # within_groups mode: use dark color for text
      p <- p + ggplot2::geom_text(
        data = mean_vals,
        ggplot2::aes(x = label_x, y = label_y, label = mean_label),
        color = "#333333", vjust = 0, hjust = 0,
        size = mean_text_size / 3, show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else {
      # avg_groups mode: color by group
      p <- p + ggplot2::geom_text(
        data = mean_vals,
        ggplot2::aes(x = label_x, y = label_y, label = mean_label, color = group),
        vjust = 0, hjust = 0, size = mean_text_size / 3, show.legend = FALSE,
        inherit.aes = FALSE
      )
    }
  }

  # Build x-axis title with log annotation
  x_title <- style$x_axis_title %||% "Intensity"
  x_label <- tb_log_axis_label(x_title, log_transform)

  p <- p +
    ggplot2::labs(x = x_label, y = y_label) +
    tb_theme_base(tb_num(style$axis_text_size, 20), axis_style = style$axis_style %||% "clean") +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL),
                    fill = ggplot2::guide_legend(title = NULL))

  # FIX: Apply manual color scales to preserve original group colors in within_groups mode
  if (color_mode != "flat") {
    p <- p +
      ggplot2::scale_color_manual(values = group_colors, drop = FALSE) +
      ggplot2::scale_fill_manual(values = group_colors, drop = FALSE)
  }

  # Apply layout mode (overlay vs separated)
  if (layout_mode == "separate") {
    # FIX: Use fixed y-axis scales across panels (not free_y) for shared range
    # Use series_id for faceting when in all_reps or within_groups mode
    facet_col <- if (compare_mode %in% c("all_reps", "within_groups")) "series_id" else "group"
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "fixed", ncol = 1)

    # FIX: Minimal strip styling - remove large boxes
    if (show_group_names) {
      p <- p + ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0)
      )
    } else {
      # Hide group names entirely
      p <- p + ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank()
      )
    }
  }

  # Apply x-axis range if in manual mode
  x_range_mode <- style$x_range_mode %||% "auto"
  if (x_range_mode == "manual") {
    x_min <- style$x_min %||% 0
    x_max <- style$x_max %||% 12
    p <- p + ggplot2::coord_cartesian(xlim = c(x_min, x_max))
  }

  list(plots = list(hor_dis_plot = p), tables = list())
}

tb_render_vert_dis <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  df <- results$data$values
  if (is.null(df) || !is.data.frame(df)) stop("vert_dis results$data$values missing.")
  if (is.null(df$value)) stop("vert_dis values require 'value' column.")

  summary_df <- results$data$summary
  df$group <- factor(df$group %||% "group")

  # log_transform: always use style (viewer-time) so user can change at render time
  # Reverse any compute-time transform and re-apply the style's choice
  applied_transform <- results$data$applied_transform %||% "legacy"
  log_transform <- style$log_transform %||% "log10"

  if (identical(applied_transform, "legacy")) {
    # Old results without applied_transform flag: data was pre-transformed, use params for label
    # Cannot reverse, so honour what was applied at compute time
    log_transform <- results$params$log_transform %||% "none"
  } else if (applied_transform != "none") {
    # Data was pre-transformed at compute time — reverse it to get raw values,
    # then apply the style's requested transform so viewer changes take effect
    if (applied_transform == "log10") {
      df$value <- 10^(df$value)
    } else if (applied_transform == "log2") {
      df$value <- 2^(df$value)
    }
    # Now apply the style-requested transform
    if (log_transform == "log10") {
      df$value[df$value <= 0] <- NA
      df$value <- log10(df$value)
    } else if (log_transform == "log2") {
      df$value[df$value <= 0] <- NA
      df$value <- log2(df$value)
    }
    df <- df[is.finite(df$value), , drop = FALSE]
  } else {
    # New results: data is raw, apply style$log_transform at render time
    if (log_transform == "log10") {
      df$value[df$value <= 0] <- NA
      df$value <- log10(df$value)
    } else if (log_transform == "log2") {
      df$value[df$value <= 0] <- NA
      df$value <- log2(df$value)
    }
    df <- df[is.finite(df$value), , drop = FALSE]
  }

  # Compare mode handling for individual replicate display
  compare_mode <- results$params$compare_mode %||% "avg_groups"
  selected_group <- style$selected_group %||% NULL

  # FIX: Store original group levels and get colors BEFORE filtering
  # This ensures within_groups mode preserves the original color for each group
  # Use engine-provided group_colors if available (from metadata), otherwise generate
  original_group_levels <- levels(df$group)
  n_original_groups <- length(original_group_levels)
  if (!is.null(results$data$group_colors) && length(results$data$group_colors) > 0) {
    # Use metadata-specified colors from engine output
    group_colors <- results$data$group_colors
    # Ensure all groups have colors (fill missing with auto-generated)
    missing <- original_group_levels[!original_group_levels %in% names(group_colors)]
    if (length(missing) > 0) {
      auto_colors <- grDevices::hcl.colors(length(missing), palette = "Dark 3")
      names(auto_colors) <- missing
      group_colors <- c(group_colors, auto_colors)
    }
  } else {
    # Fallback to auto-generated colors; guard against empty factor levels
    if (n_original_groups < 1 || length(original_group_levels) == 0) {
      group_colors <- c(`(all)` = "#808080")
    } else {
      group_colors <- scales::hue_pal()(n_original_groups)
      names(group_colors) <- original_group_levels
    }
  }

  # For all_reps and within_groups modes, create replicate-level series
  if (compare_mode == "all_reps") {
    # All replicates: show individual boxes/violins for every replicate
    # Label format: "GroupName ReplicateNumber"
    if (!is.null(df$replicate) && !all(is.na(df$replicate))) {
      df$series_id <- paste(df$group, df$replicate)
    } else {
      df$series_id <- df$sample %||% df$group
    }
    df$series_id <- factor(df$series_id)
  } else if (compare_mode == "within_groups" && !is.null(selected_group) && nzchar(selected_group)) {
    # Within groups: show only the selected group's replicates
    df <- df[df$group == selected_group, , drop = FALSE]
    # FIX: Also filter summary_df to match selected group for mean calculations
    if (!is.null(summary_df) && is.data.frame(summary_df)) {
      summary_df <- summary_df[summary_df$group == selected_group, , drop = FALSE]
    }
    if (nrow(df) == 0) {
      p_empty <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste0("No data for group: ", selected_group),
                          size = 6, color = "gray50") +
        ggplot2::theme_void()
      return(list(plots = list(vert_dis_plot = p_empty), tables = list()))
    }
    # FIX: Drop unused factor levels so mean calculation uses only selected group
    df$group <- droplevels(df$group)
    if (!is.null(df$replicate) && !all(is.na(df$replicate))) {
      df$series_id <- paste(df$group, df$replicate)
    } else {
      df$series_id <- df$sample %||% df$group
    }
    df$series_id <- factor(df$series_id)
  } else {
    # avg_groups mode: use group as series
    df$series_id <- df$group
  }

  # Get style options
  alpha_val <- tb_num(style$alpha, 0.3)
  show_global_mean <- isTRUE(style$show_global_mean %||% TRUE)
  mean_line_color <- style$mean_line_color %||% "#000000"
  mean_line_size <- tb_num(style$mean_line_size, 1)
  plot_type <- style$plot_type %||% "box"
  show_n <- isTRUE(style$show_n %||% TRUE)
  label_rotation <- as.numeric(style$label_rotation %||% "0")

  # Color mode handling
  color_mode <- style$color_mode %||% "group"
  flat_color <- style$flat_color %||% "#B0B0B0"

  # Use series_id as the x-axis grouping for all_reps and within_groups modes
  x_col <- if (compare_mode %in% c("all_reps", "within_groups")) "series_id" else "group"

  # Compute sample counts per x-axis category for Show N
  x_counts <- table(df[[x_col]])

  # Build x-axis labels with n if show_n enabled
  if (show_n) {
    # Format: "Label\n(n=####)"
    x_levels <- levels(df[[x_col]])
    new_labels <- vapply(x_levels, function(g) {
      n <- as.integer(x_counts[g])
      paste0(g, "\n(n=", n, ")")
    }, character(1))
    df$x_label <- factor(df[[x_col]], levels = x_levels, labels = new_labels)
  } else {
    df$x_label <- df[[x_col]]
  }

  # Build plot based on plot_type
  # FIX: Always use 'group' for color/fill mapping so replicates inherit their group's color
  # x_label already handles the x-axis positioning (series_id for all_reps/within_groups, group for avg_groups)

  if (color_mode == "flat") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x_label, y = value))
  } else {
    # Use group for fill color, regardless of compare_mode
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x_label, y = value, fill = group))
  }

  if (plot_type == "violin") {
    # Violin plot with enhancement: thin dark-grey boxplot + white median dot
    if (color_mode == "flat") {
      p <- p + ggplot2::geom_violin(fill = flat_color, alpha = alpha_val,
                                    trim = TRUE, scale = "width")
    } else {
      p <- p + ggplot2::geom_violin(alpha = alpha_val, trim = TRUE, scale = "width")
    }
    # Add thin dark-grey boxplot inside violin with whiskers
    # FIX: Use visible color for whiskers instead of color = NA
    p <- p + ggplot2::geom_boxplot(width = 0.1, fill = "#4a4a4a", color = "#4a4a4a",
                                   outlier.shape = NA, alpha = 0.8)
    # Add white median dot
    p <- p + ggplot2::stat_summary(fun = median, geom = "point",
                                   color = "white", size = 2)
  } else {
    # Box plot (default)
    if (color_mode == "flat") {
      p <- p + ggplot2::geom_boxplot(fill = flat_color, alpha = alpha_val,
                                     outlier.size = 0.6)
    } else {
      p <- p + ggplot2::geom_boxplot(alpha = alpha_val, outlier.size = 0.6)
    }
  }

  # FIX: Global mean line should be truly global (across all data)
  # Support mean_type: "arithmetic" (default) or "harmonic" - now from style_schema
  mean_type <- style$mean_type %||% "arithmetic"

  if (show_global_mean) {
    vals <- df$value[is.finite(df$value) & df$value > 0]
    if (mean_type == "harmonic" && length(vals) > 0) {
      # Harmonic mean: n / sum(1/x)
      global_mean <- length(vals) / sum(1 / vals)
    } else {
      # Arithmetic mean (default)
      global_mean <- mean(df$value, na.rm = TRUE)
    }
    p <- p + ggplot2::geom_hline(
      yintercept = global_mean,
      color = mean_line_color,
      linetype = "dashed",
      linewidth = mean_line_size
    )
  }

  # Per-group average labels: annotate each box/violin with a numeric value
  show_avg_label <- isTRUE(style$show_avg_label %||% FALSE)
  if (show_avg_label && nrow(df) > 0) {
    avg_label_type <- style$avg_label_type %||% "median"
    avg_label_color <- style$avg_label_color %||% "#333333"
    avg_label_size <- tb_num(style$avg_label_size, 3.5)

    # Compute per-group (or per-series) averages
    x_groups <- levels(df$x_label)
    avg_df <- do.call(rbind, lapply(x_groups, function(g) {
      vals <- df$value[df$x_label == g]
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0) return(NULL)

      avg_val <- if (avg_label_type == "harmonic") {
        pos <- vals[vals > 0]
        if (length(pos) > 0) length(pos) / sum(1 / pos) else NA_real_
      } else if (avg_label_type == "arithmetic") {
        mean(vals, na.rm = TRUE)
      } else {
        # median (default)
        median(vals, na.rm = TRUE)
      }

      if (!is.finite(avg_val)) return(NULL)
      data.frame(
        x_label = g,
        avg_val = avg_val,
        avg_label = format(signif(avg_val, 3), big.mark = ","),
        stringsAsFactors = FALSE
      )
    }))

    if (!is.null(avg_df) && nrow(avg_df) > 0) {
      avg_df$x_label <- factor(avg_df$x_label, levels = x_groups)
      p <- p + ggplot2::geom_text(
        data = avg_df,
        ggplot2::aes(x = x_label, y = avg_val, label = avg_label),
        color = avg_label_color,
        size = avg_label_size,
        hjust = -0.3, vjust = 0.5,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    }
  }

  # Build y-axis title with log annotation
  y_title <- style$y_axis_title %||% "Intensity"
  y_label <- tb_log_axis_label(y_title, log_transform)

  p <- p +
    ggplot2::labs(x = NULL, y = y_label) +
    tb_theme_base(tb_num(style$axis_text_size, 20), axis_style = style$axis_style %||% "clean") +
    ggplot2::guides(fill = "none")

  # FIX: Apply manual color scale to preserve original group colors in within_groups mode
  if (color_mode != "flat") {
    p <- p + ggplot2::scale_fill_manual(values = group_colors, drop = FALSE)
  }

  # Apply label rotation
  if (label_rotation == 45) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
  } else if (label_rotation == 90) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  }

  # Apply y-axis range if in manual mode
  y_range_mode <- style$y_range_mode %||% "auto"
  if (y_range_mode == "manual") {
    y_min <- style$y_min %||% 0
    y_max <- style$y_max %||% 12
    p <- p + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
  }

  list(plots = list(vert_dis_plot = p), tables = list())
}


tb_render_pca <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  # PCA engine produces pre-computed scores, loadings, variance
  scores <- results$data$scores
  variance <- results$data$variance
  loadings <- results$data$loadings

  if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0) {
    stop("pca results$data$scores missing or empty.")
  }

  # Ensure group column exists and is a factor
  if (!"group" %in% names(scores)) {
    scores$group <- sub("_.*$", "", scores$sample)
  }
  scores$group <- factor(scores$group)

  # Extract variance explained for axis labels
  var_expl <- if (!is.null(variance) && "variance_explained" %in% names(variance)) {
    variance$variance_explained
  } else {
    c(NA_real_, NA_real_)
  }

  pc1_label <- if (!is.na(var_expl[1])) {
    sprintf("PC1 (%.1f%%)", 100 * var_expl[1])
  } else {
    "PC1"
  }
  pc2_label <- if (length(var_expl) >= 2 && !is.na(var_expl[2])) {
    sprintf("PC2 (%.1f%%)", 100 * var_expl[2])
  } else {
    "PC2"
  }

  # Compute fixed axis limits from points, expanding to include ellipses if shown
  x_range <- range(scores$PC1, na.rm = TRUE)
  y_range <- range(scores$PC2, na.rm = TRUE)

  # If ellipses will be shown, expand limits to include ellipse extents
  if (isTRUE(style$show_ellipse %||% TRUE)) {
    group_counts <- table(scores$group)
    ellipse_level <- style$ellipse_level %||% 0.95

    # Helper to compute confidence ellipse bounds matching ggplot2::stat_ellipse type="norm"
    compute_ellipse_bounds <- function(x, y, level) {
      n <- length(x)
      if (n < 4) return(NULL)
      tryCatch({
        # Remove NAs
        valid <- !is.na(x) & !is.na(y)
        x <- x[valid]
        y <- y[valid]
        n <- length(x)
        if (n < 4) return(NULL)

        # Compute center and covariance (same as stat_ellipse)
        center <- c(mean(x), mean(y))
        cov_mat <- cov(cbind(x, y))
        if (any(!is.finite(cov_mat))) return(NULL)

        # Cholesky decomposition approach (matches ggplot2 stat_ellipse)
        chol_decomp <- tryCatch(chol(cov_mat), error = function(e) NULL)
        if (is.null(chol_decomp)) return(NULL)

        # Radius for confidence level
        radius <- sqrt(2 * qf(level, 2, n - 1))

        # Generate unit circle and transform
        theta <- seq(0, 2 * pi, length.out = 100)
        unit_circle <- cbind(cos(theta), sin(theta))

        # Transform: scale by radius, apply Cholesky, translate to center
        ellipse_pts <- unit_circle %*% (radius * chol_decomp)
        ellipse_pts[, 1] <- ellipse_pts[, 1] + center[1]
        ellipse_pts[, 2] <- ellipse_pts[, 2] + center[2]

        list(x = range(ellipse_pts[, 1]), y = range(ellipse_pts[, 2]))
      }, error = function(e) NULL)
    }

    # Expand bounds for stat_ellipse groups (>=4 points)
    groups_with_ellipse <- names(group_counts[group_counts >= 4])
    for (grp in groups_with_ellipse) {
      grp_data <- scores[scores$group == grp, , drop = FALSE]
      bounds <- compute_ellipse_bounds(grp_data$PC1, grp_data$PC2, ellipse_level)
      if (!is.null(bounds)) {
        x_range <- range(c(x_range, bounds$x), na.rm = TRUE)
        y_range <- range(c(y_range, bounds$y), na.rm = TRUE)
      }
    }

    # Expand bounds for custom ellipses (2-3 points)
    groups_with_hull <- names(group_counts[group_counts >= 2 & group_counts < 4])
    if (length(groups_with_hull) > 0) {
      pt_size <- tb_num(style$point_size, 5)
      data_range <- max(diff(range(scores$PC1)), diff(range(scores$PC2)))
      point_radius <- ((pt_size + 0.3) / 2) * (data_range * 0.01)

      for (grp in groups_with_hull) {
        grp_data <- scores[scores$group == grp, , drop = FALSE]
        n_pts <- nrow(grp_data)
        if (n_pts >= 2) {
          cx <- mean(grp_data$PC1)
          cy <- mean(grp_data$PC2)
          centered_x <- grp_data$PC1 - cx
          centered_y <- grp_data$PC2 - cy

          cov_matrix <- matrix(c(
            sum(centered_x^2), sum(centered_x * centered_y),
            sum(centered_x * centered_y), sum(centered_y^2)
          ), nrow = 2) / n_pts

          eig <- eigen(cov_matrix)
          angle <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])
          cos_a <- cos(-angle)
          sin_a <- sin(-angle)
          rotated_x <- centered_x * cos_a - centered_y * sin_a
          rotated_y <- centered_x * sin_a + centered_y * cos_a

          a <- max(abs(rotated_x)) + point_radius
          b <- max(abs(rotated_y)) + point_radius
          min_b <- a * 0.15
          if (b < min_b) b <- min_b

          # Generate ellipse points to get actual x/y extents
          theta <- seq(0, 2 * pi, length.out = 100)
          ellipse_x <- a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle)
          ellipse_y <- a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle)
          x_range <- range(c(x_range, cx + ellipse_x), na.rm = TRUE)
          y_range <- range(c(y_range, cy + ellipse_y), na.rm = TRUE)
        }
      }
    }
  }

  # Apply padding after including ellipse bounds
  x_pad <- diff(x_range) * 0.1
  y_pad <- diff(y_range) * 0.1
  fixed_xlim <- c(x_range[1] - x_pad, x_range[2] + x_pad)
  fixed_ylim <- c(y_range[1] - y_pad, y_range[2] + y_pad)

  # Create scores plot
  # FIX: Use consistent color and fill scales so ellipse outline and fill match point colors
  group_colors <- results$data$group_colors %||% NULL
  if (is.null(group_colors) || length(group_colors) == 0) {
    n_groups <- nlevels(scores$group)
    group_colors <- scales::hue_pal()(n_groups)
    names(group_colors) <- levels(scores$group)
  } else {
    missing <- levels(scores$group)[!levels(scores$group) %in% names(group_colors)]
    if (length(missing) > 0) {
      auto_colors <- grDevices::hcl.colors(length(missing), palette = "Dark 3")
      names(auto_colors) <- missing
      group_colors <- c(group_colors, auto_colors)
    }
  }
  group_colors <- group_colors[levels(scores$group)]

  # Add hover text for plotly interactivity
  scores$hover_text <- sprintf(
    "%s\nGroup: %s\nPC1: %.3f\nPC2: %.3f",
    scores$sample, scores$group, scores$PC1, scores$PC2
  )

  # Use the computed limits directly - no need to force square for PCA plots
  # since PC1 and PC2 have different variance explained anyway

  point_stroke <- 0.3
  p_scores <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, text = hover_text)) +
    ggplot2::geom_point(
      ggplot2::aes(fill = group),
      shape = 21,
      size = tb_num(style$point_size, 5),
      alpha = tb_num(style$point_alpha, 0.6),
      color = "black",
      stroke = point_stroke
    ) +
    ggplot2::scale_color_manual(values = group_colors) +
    ggplot2::scale_fill_manual(values = group_colors) +
    ggplot2::labs(x = pc1_label, y = pc2_label) +
    tb_theme_base(tb_num(style$axis_text_size, 20), axis_style = style$axis_style %||% "clean") +
    # FIX: Add panel border for full square frame - linewidth matches axis_style
    # IMPORTANT: Remove axis.line to prevent double-drawing (panel.border draws all 4 sides)
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = if (tolower(style$axis_style %||% "clean") == "bold") 1.5 else 0.5),
      axis.line = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL), color = "none")
    # NOTE: coord_fixed is added AFTER ellipse layers to ensure ellipses aren't clipped

  # Add ellipse/convex hull if requested
  # >=4 points: confidence ellipse (filled with outline same color)
  # <4 points: convex hull polygon
  if (isTRUE(style$show_ellipse %||% TRUE)) {
    group_counts <- table(scores$group)
    ellipse_alpha <- tb_num(style$ellipse_alpha, 0.25)

    # Groups with >=4 points get confidence ellipse
    groups_with_ellipse <- names(group_counts[group_counts >= 4])
    # Groups with <4 but >=2 points get convex hull
    groups_with_hull <- names(group_counts[group_counts >= 2 & group_counts < 4])

    # Add confidence ellipses for groups with >=4 points
    # Pass all eligible groups at once and let stat_ellipse handle grouping
    if (length(groups_with_ellipse) > 0) {
      ellipse_level <- style$ellipse_level %||% 0.95
      scores_for_ellipse <- scores[scores$group %in% groups_with_ellipse, , drop = FALSE]

      if (nrow(scores_for_ellipse) > 0) {
        tryCatch({
          p_scores <- p_scores +
            ggplot2::stat_ellipse(
              data = scores_for_ellipse,
              ggplot2::aes(x = PC1, y = PC2, color = group, fill = group),
              type = "norm",
              level = ellipse_level,
              linewidth = 0.8,
              linetype = "solid",
              alpha = ellipse_alpha,
              geom = "polygon",
              show.legend = FALSE,
              inherit.aes = FALSE  # Don't inherit 'text' aesthetic from main plot
            )
        }, error = function(e) {
          # Silently skip if ellipse fails
        })
      }
    }

    # Add minimal enclosing ellipse for groups with 2-3 points
    # Creates an ellipse that encapsulates the OUTER EDGE of points (not just centers)
    if (length(groups_with_hull) > 0) {
      # Estimate point radius in data coordinates
      # Point size is in mm, we need to convert to data units
      # Use ~2% of data range per mm of point size as approximation
      pt_size <- tb_num(style$point_size, 5)
      data_range <- max(diff(range(scores$PC1)), diff(range(scores$PC2)))
      # Each mm of point size ~ 0.5% of data range (tuned for typical PCA plots)
      point_radius <- ((pt_size + point_stroke) / 2) * (data_range * 0.01)

      for (grp in groups_with_hull) {
        grp_data <- scores[scores$group == grp, , drop = FALSE]
        n_pts <- nrow(grp_data)
        if (n_pts >= 2) {
          # Compute centroid
          cx <- mean(grp_data$PC1)
          cy <- mean(grp_data$PC2)

          # Center the points
          centered_x <- grp_data$PC1 - cx
          centered_y <- grp_data$PC2 - cy

          # Compute the principal axis direction using SVD/eigendecomposition
          # This gives us the optimal ellipse orientation
          cov_matrix <- matrix(c(
            sum(centered_x^2), sum(centered_x * centered_y),
            sum(centered_x * centered_y), sum(centered_y^2)
          ), nrow = 2) / n_pts

          eig <- eigen(cov_matrix)
          # Principal direction (major axis)
          angle <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])

          # Rotate points to align with principal axes
          cos_a <- cos(-angle)
          sin_a <- sin(-angle)
          rotated_x <- centered_x * cos_a - centered_y * sin_a
          rotated_y <- centered_x * sin_a + centered_y * cos_a

          # Find the extent along each axis (this is the minimal bounding box in rotated coords)
          # The ellipse must touch the most extreme points
          a <- max(abs(rotated_x))  # Semi-major axis (along principal direction)
          b <- max(abs(rotated_y))  # Semi-minor axis (perpendicular)

          # EXPAND by point radius to encapsulate outer edge of points, not just centers
          a <- a + point_radius
          b <- b + point_radius

          # Ensure minimum visible ellipse width (at least 15% of major axis)
          min_b <- a * 0.15
          if (b < min_b) b <- min_b

          # Generate ellipse polygon points
          theta <- seq(0, 2 * pi, length.out = 101)
          ellipse_x <- cx + a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle)
          ellipse_y <- cy + a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle)

          ellipse_data <- data.frame(
            PC1 = ellipse_x,
            PC2 = ellipse_y,
            group = grp,
            stringsAsFactors = FALSE
          )
          ellipse_data$group <- factor(ellipse_data$group, levels = levels(scores$group))

          p_scores <- p_scores +
            ggplot2::geom_polygon(
              data = ellipse_data,
              ggplot2::aes(x = PC1, y = PC2, fill = group, color = group),
              alpha = ellipse_alpha,
              linewidth = 0.8,
              show.legend = FALSE,
              inherit.aes = FALSE
            )
        }
      }
    }
  }

  # Group centroid labels (replaces legend when enabled)
  if (isTRUE(style$show_group_labels %||% FALSE)) {
    centroids <- stats::aggregate(
      cbind(PC1, PC2) ~ group,
      data = scores,
      FUN = mean
    )

    label_color_mode <- style$group_label_color_mode %||% "group"
    label_alpha <- tb_num(style$group_label_alpha, 1)
    label_size  <- tb_num(style$group_label_size, 5)
    use_bg      <- isTRUE(style$group_label_bg %||% FALSE)
    geom_fn     <- if (use_bg) ggplot2::geom_label else ggplot2::geom_text

    if (label_color_mode == "flat") {
      flat_col <- style$group_label_flat_color %||% "#000000"
      label_layer <- geom_fn(
        data = centroids,
        ggplot2::aes(x = PC1, y = PC2, label = group),
        color = flat_col,
        size = label_size,
        alpha = label_alpha,
        fontface = "bold",
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else {
      # "group" mode: color text by group color
      label_layer <- geom_fn(
        data = centroids,
        ggplot2::aes(x = PC1, y = PC2, label = group, color = group),
        size = label_size,
        alpha = label_alpha,
        fontface = "bold",
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    }

    p_scores <- p_scores + label_layer +
      ggplot2::theme(legend.position = "none")
  }

  # FIX: Use coord_cartesian instead of coord_fixed to allow flexible aspect ratio
  # This lets users control the plot proportions via width/height settings
  # The xlim/ylim are still computed symmetrically for visual balance
  p_scores <- p_scores + ggplot2::coord_cartesian(xlim = fixed_xlim, ylim = fixed_ylim)

  # Create scree plot from variance data (updated with percent y-axis, labels, and customizable color)
  if (!is.null(variance) && is.data.frame(variance) && nrow(variance) > 0) {
    df_scree <- data.frame(
      pc = seq_len(nrow(variance)),
      var = variance$variance_explained * 100,  # Convert to percent
      stringsAsFactors = FALSE
    )
    # Compute label position: raise by 5% relative to avoid dot-line overlap
    df_scree$label_y <- df_scree$var + 5
  } else {
    df_scree <- data.frame(pc = integer(0), var = numeric(0), label_y = numeric(0),
                           stringsAsFactors = FALSE)
  }

  # FIX: Separate color options for scree bars and line/dots
  # Support legacy scree_color for backwards compatibility
  scree_bar_color <- style$scree_bar_color %||% style$scree_color %||% "#4682B4"
  scree_line_color <- style$scree_line_color %||% "#E74C3C"

  p_scree <- ggplot2::ggplot(df_scree, ggplot2::aes(x = pc, y = var)) +
    ggplot2::geom_col(fill = scree_bar_color, alpha = 0.8) +
    ggplot2::geom_point(color = scree_line_color, size = 3) +
    ggplot2::geom_line(color = scree_line_color, linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", var), y = label_y),
      vjust = 0,
      size = tb_num(style$scree_text_size, 3.5)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      expand = ggplot2::expansion(mult = c(0, 0.15))  # More room for raised labels
    ) +
    ggplot2::scale_x_continuous(
      breaks = df_scree$pc,
      labels = paste0("PC", df_scree$pc)
    ) +
    ggplot2::labs(x = "Principal Component", y = "Variance Explained (%)") +
    tb_theme_base(tb_num(style$axis_text_size, 20), axis_style = style$axis_style %||% "clean")

  list(
    plots = list(pca_scores = p_scores, pca_scree = p_scree),
    tables = list(pca_loadings = loadings)
  )
}

# ============================================================
# Gene UMAP renderer (interactive plotly scatter)
# ------------------------------------------------------------
# Each point = one protein/gene. Position computed by stats
# engine (uwot::umap on log-abundance patterns across samples).
# Recoloring / resizing via viewer_schema fields never triggers
# a UMAP recompute — we just re-render from results$data$embedding.
# ============================================================
tb_render_umap_gene <- function(results, style, meta) {
  tb_require_pkg("plotly")

  emb <- results$data$embedding
  groups_all <- results$data$groups %||% unique(emb$group %||% "combined")

  if (is.null(emb) || !is.data.frame(emb) || nrow(emb) == 0) {
    p_empty <- plotly::plot_ly(type = "scatter", mode = "markers") %>%
      plotly::layout(
        title = list(text = "No embedding available", x = 0.5),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
    return(list(plots = list(umap_gene = p_empty),
                tables = list(gene_embedding = emb %||% data.frame())))
  }

  if (!"group" %in% names(emb)) emb$group <- "combined"

  color_by <- style$color_by %||% "subloc"
  size_by  <- style$size_by %||% "flat"
  pt_size  <- tb_num(style$point_size, 2)
  pt_alpha <- tb_num(style$point_alpha, 0.7)
  flat_col <- style$flat_color %||% "#4682B4"

  # Palettes shared across groups so colors are comparable between panels
  subloc_levels_all <- results$data$subloc_levels %||% unique(emb$subloc)
  if (length(subloc_levels_all) == 0) subloc_levels_all <- unique(emb$subloc)
  subloc_palette <- grDevices::hcl.colors(max(length(subloc_levels_all), 3),
                                          palette = "Dark 3")
  names(subloc_palette) <- subloc_levels_all

  # ---- Per-group rendering helper ----
  render_one <- function(df_g, group_tag, show_legend) {
    # Size mapping
    if (size_by == "mean_abundance" && "mean_abundance" %in% names(df_g)) {
      v <- df_g$mean_abundance
      v <- (v - min(v, na.rm = TRUE)) / max(1e-9, diff(range(v, na.rm = TRUE)))
      msize <- pt_size * (2 + 6 * v)
    } else if (size_by == "variance" && "variance" %in% names(df_g)) {
      v <- df_g$variance
      v <- (v - min(v, na.rm = TRUE)) / max(1e-9, diff(range(v, na.rm = TRUE)))
      msize <- pt_size * (2 + 6 * v)
    } else {
      msize <- pt_size * 3
    }

    cluster_txt <- if ("cluster" %in% names(df_g)) {
      ifelse(df_g$cluster == 0L, "noise", as.character(df_g$cluster))
    } else {
      rep("-", nrow(df_g))
    }
    df_g$hover_text <- sprintf(
      "%s\nUniProt: %s\nGroup: %s\nSubloc: %s\nCluster: %s\nMean: %.2f\nVar: %.3f",
      df_g$gene_symbol, df_g$protein_id, df_g$group, df_g$subloc, cluster_txt,
      df_g$mean_abundance, df_g$variance
    )

    p <- plotly::plot_ly(type = "scatter", mode = "markers",
                         source = paste0("umap_gene_", group_tag))

    if (color_by == "subloc") {
      for (lvl in subloc_levels_all) {
        mask <- df_g$subloc == lvl
        if (!any(mask)) next
        p <- p %>% plotly::add_trace(
          data = df_g[mask, , drop = FALSE],
          x = ~UMAP1, y = ~UMAP2, customdata = ~protein_id,
          marker = list(color = subloc_palette[[lvl]], size = msize[mask],
                        opacity = pt_alpha,
                        line = list(color = "#333333", width = 0.2)),
          text = ~hover_text, hoverinfo = "text",
          name = lvl, legendgroup = lvl, showlegend = show_legend
        )
      }
    } else if (color_by == "cluster" && "cluster" %in% names(df_g)) {
      cl_levels <- sort(unique(df_g$cluster))
      cl_palette <- grDevices::hcl.colors(max(length(cl_levels), 3), palette = "Dark 3")
      for (i in seq_along(cl_levels)) {
        cl <- cl_levels[i]
        mask <- df_g$cluster == cl
        if (!any(mask)) next
        col <- if (cl == 0L) "#BBBBBB" else cl_palette[i]
        nm <- if (cl == 0L) "noise" else paste0("cluster ", cl)
        p <- p %>% plotly::add_trace(
          data = df_g[mask, , drop = FALSE],
          x = ~UMAP1, y = ~UMAP2, customdata = ~protein_id,
          marker = list(color = col, size = msize[mask], opacity = pt_alpha,
                        line = list(color = "#333333", width = 0.2)),
          text = ~hover_text, hoverinfo = "text",
          name = nm, showlegend = show_legend
        )
      }
    } else {
      p <- p %>% plotly::add_trace(
        data = df_g,
        x = ~UMAP1, y = ~UMAP2, customdata = ~protein_id,
        marker = list(color = flat_col, size = msize, opacity = pt_alpha,
                      line = list(color = "#333333", width = 0.2)),
        text = ~hover_text, hoverinfo = "text", showlegend = FALSE
      )
    }

    if (isTRUE(style$show_labels %||% FALSE) && nrow(df_g) > 0) {
      top_n <- max(1, min(as.integer(style$label_top_n %||% 10), nrow(df_g)))
      top_idx <- order(df_g$variance, decreasing = TRUE)[seq_len(top_n)]
      p <- p %>% plotly::add_annotations(
        data = df_g[top_idx, , drop = FALSE],
        x = ~UMAP1, y = ~UMAP2, text = ~gene_symbol,
        showarrow = FALSE,
        font = list(size = tb_num(style$label_size, 3) * 3, color = "#000000"),
        yshift = 10
      )
    }

    title_text <- if (group_tag == "combined") "Gene UMAP (all samples)" else paste0("Group: ", group_tag)
    p %>%
      plotly::layout(
        title = list(text = title_text, x = 0.5, font = list(size = 14)),
        xaxis = list(title = "UMAP1", zeroline = FALSE,
                     showgrid = TRUE, gridcolor = "#e8e8e8"),
        yaxis = list(title = "UMAP2", zeroline = FALSE,
                     showgrid = TRUE, gridcolor = "#e8e8e8"),
        dragmode = "pan", plot_bgcolor = "white", paper_bgcolor = "white",
        legend = list(x = 1.02, y = 1, xanchor = "left"),
        showlegend = show_legend
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }

  groups_present <- unique(emb$group)
  # Order per results$data$groups if provided, else alphabetically
  if (length(groups_all) > 0) {
    groups_present <- intersect(groups_all, groups_present)
    extras <- setdiff(unique(emb$group), groups_present)
    groups_present <- c(groups_present, extras)
  }

  if (length(groups_present) <= 1) {
    # Combined mode (or single-group fallback)
    g <- groups_present[1] %||% "combined"
    p <- render_one(emb[emb$group == g, , drop = FALSE], g, show_legend = TRUE)
    return(list(
      plots = list(umap_gene = p),
      tables = list(gene_embedding = emb[, setdiff(names(emb), "hover_text"), drop = FALSE])
    ))
  }

  # Per-group: one plot per group + one table per group + tabs
  # Normalize group name to a safe token (spaces/punct -> underscore) so the
  # result viewer's tab-keyed plot lookup (regex "(^|_)<tab>(_|$)") matches.
  plots_out <- list()
  tables_out <- list()
  tab_keys <- character(length(groups_present))
  for (i in seq_along(groups_present)) {
    g <- groups_present[i]
    g_key <- gsub("[^A-Za-z0-9]+", "_", g)
    g_key <- gsub("^_+|_+$", "", g_key)
    if (!nzchar(g_key)) g_key <- sprintf("group%d", i)
    tab_keys[i] <- g_key
    df_g <- emb[emb$group == g, , drop = FALSE]
    plots_out[[paste0(g_key, "_plot")]] <- render_one(df_g, g, show_legend = (i == 1))
    tables_out[[paste0(g_key, "_table")]] <- df_g[, setdiff(names(df_g), "hover_text"), drop = FALSE]
  }

  list(
    plots = plots_out,
    tables = tables_out,
    tabs = tab_keys
  )
}

tb_render_1dgofcs <- function(results, style, meta) {
  # Check if data is organized by tabs
  data_obj <- results$data %||% list()

  # --- Ontology Filter (v1.5 migration) ---
  # style$ontology_filter takes precedence; fall back to params$ontology for legacy terpbooks
  ontology_filter <- style$ontology_filter %||% results$params$ontology %||% "all"

  # Auto-reset ontology filter for complex mode
  if (!is.null(data_obj$terms) && is.data.frame(data_obj$terms) &&
      "ontology" %in% names(data_obj$terms) &&
      tb_detect_complex_mode(data_obj$terms) &&
      ontology_filter %in% c("BP", "MF", "CC")) {
    ontology_filter <- "all"
  }

  # Apply ontology filter to terms if column exists
  if (!identical(ontology_filter, "all") && !is.null(data_obj$terms) &&
      is.data.frame(data_obj$terms) && "ontology" %in% names(data_obj$terms)) {
    data_obj$terms <- data_obj$terms[grepl(ontology_filter, data_obj$terms$ontology, ignore.case = TRUE), , drop = FALSE]
  }
  # Also filter multi-comparison analyses if present
  if (!identical(ontology_filter, "all") && !is.null(data_obj$analyses)) {
    for (comp in names(data_obj$analyses)) {
      df <- data_obj$analyses[[comp]]$terms
      if (!is.null(df) && is.data.frame(df) && "ontology" %in% names(df)) {
        data_obj$analyses[[comp]]$terms <- df[grepl(ontology_filter, df$ontology, ignore.case = TRUE), , drop = FALSE]
      }
    }
  }
  # --- End ontology filter ---

  available_tabs <- character()

  # FIX: Handle multi-comparison structure from standalone mode
  # When control_only is off, engine returns data$analyses with per-comparison results
  analyses <- data_obj$analyses %||% NULL
  if (!is.null(analyses) && is.list(analyses) && length(analyses) > 0) {
    plot_type <- style$plot_type %||% "bar"
    all_plots <- list()
    all_tables <- list()

    for (comp_name in names(analyses)) {
      analysis <- analyses[[comp_name]]
      comp_label <- analysis$score_label %||% {
        parts <- unlist(strsplit(comp_name, "_vs_", fixed = TRUE))
        paste0("log2(", paste(parts, collapse = "/"), ")")
      }
      if (isTRUE(style$flip_fc)) comp_label <- tb_flip_comparison_label(comp_label)
      df <- analysis$terms

      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        rendered <- tb_render_go_tab(comp_name, list(terms = df), style, plot_type, meta,
                                     is_fcs = TRUE, score_label = comp_label)

        for (pn in names(rendered$plots)) {
          all_plots[[paste0(comp_name, "_plot")]] <- rendered$plots[[pn]]
        }
        for (tn in names(rendered$tables)) {
          all_tables[[paste0(comp_name, "_table")]] <- rendered$tables[[tn]]
        }
      }
    }

    return(list(
      plots = all_plots,
      tables = all_tables,
      comparisons = names(analyses)  # Signal to UI that comparisons are present
    ))
  }

  # FIX: Extract score_label for dynamic axis labeling (e.g., "PC1" or "log2(BafA1/Control)")
  score_label <- data_obj$score_label %||% results$params$score_label %||% NULL
  if (isTRUE(style$flip_fc) && !is.null(score_label)) {
    score_label <- tb_flip_comparison_label(score_label)
  }

  # Detect which tabs are present (BP, MF, CC, or complex sources) in explicit structure
  for (tab in c("BP", "MF", "CC", "CORUM", "ComplexPortal")) {
    if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
      available_tabs <- c(available_tabs, tab)
    }
  }

  # FIX: Check if terms has an 'ontology' column we can split on
  df <- data_obj$terms
  if (length(available_tabs) == 0 && !is.null(df) && is.data.frame(df) && "ontology" %in% names(df)) {
    # Detect if this is complex enrichment (CORUM/ComplexPortal) or GO enrichment (BP/MF/CC)
    is_complex_mode <- tb_detect_complex_mode(df)

    if (is_complex_mode) {
      # Split by complex source (CORUM, ComplexPortal)
      complex_sources <- tb_get_complex_sources(df)
      for (src in complex_sources) {
        src_df <- df[df$ontology == src, , drop = FALSE]
        if (nrow(src_df) > 0) {
          data_obj[[src]] <- list(terms = src_df)
          available_tabs <- c(available_tabs, src)
        }
      }
    } else {
      # Split terms by GO ontology column to create tabs
      ontologies_present <- unique(df$ontology)
      for (ont in c("BP", "MF", "CC")) {
        if (ont %in% ontologies_present) {
          ont_df <- df[df$ontology == ont, , drop = FALSE]
          if (nrow(ont_df) > 0) {
            data_obj[[ont]] <- list(terms = ont_df)
            available_tabs <- c(available_tabs, ont)
          }
        }
      }
    }
  }

  # If no tabs found, fall back to single-tab structure
  if (length(available_tabs) == 0) {
    # Old format: single dataset
    df <- data_obj$terms %||% data_obj
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(list(plots = list(), tables = list()))
    }

    plot_type <- style$plot_type %||% "bar"
    rendered <- tb_render_go_tab("main", list(terms = df), style, plot_type, meta,
                                 is_fcs = TRUE, score_label = score_label)
    if (length(rendered$plots) == 0) {
      return(list(plots = list(), tables = list()))
    }
    return(list(
      plots = list(`1dgofcs_plot` = rendered$plots[[1]]),
      tables = list(`1dgofcs_table` = rendered$tables[[1]])
    ))
  }

  # Multi-tab structure
  plot_type <- style$plot_type %||% "bar"
  all_plots <- list()
  all_tables <- list()

  for (tab in available_tabs) {
    tab_data <- data_obj[[tab]]
    rendered <- tb_render_go_tab(tab, tab_data, style, plot_type, meta, is_fcs = TRUE,
                                 score_label = score_label)

    # Merge into all_plots/all_tables with tab prefix
    for (pn in names(rendered$plots)) {
      all_plots[[pn]] <- rendered$plots[[pn]]
    }
    for (tn in names(rendered$tables)) {
      all_tables[[tn]] <- rendered$tables[[tn]]
    }
  }

  list(
    plots = all_plots,
    tables = all_tables,
    tabs = available_tabs  # Signal to UI that tabs are present
  )
}

tb_render_2dgofcs <- function(results, style, meta) {
  # 2D GO-FCS renders scatter plots comparing fold-changes between comparisons
  # X-axis: score_x (fold change from comparison 1)
  # Y-axis: score_y (fold change from comparison 2)
  # Point size: # genes in term
  # Point color: FDR

  tb_require_pkg("ggplot2")

  data_obj <- results$data %||% list()

  # --- Ontology Filter (v1.5 migration) ---
  # style$ontology_filter takes precedence; fall back to params$ontology for legacy terpbooks
  ontology_filter <- style$ontology_filter %||% results$params$ontology %||% "all"

  # Auto-reset ontology filter for complex mode
  if (!is.null(data_obj$terms) && is.data.frame(data_obj$terms) &&
      "ontology" %in% names(data_obj$terms) &&
      tb_detect_complex_mode(data_obj$terms) &&
      ontology_filter %in% c("BP", "MF", "CC")) {
    ontology_filter <- "all"
  }

  # Apply ontology filter to terms if column exists
  if (!identical(ontology_filter, "all") && !is.null(data_obj$terms) &&
      is.data.frame(data_obj$terms) && "ontology" %in% names(data_obj$terms)) {
    data_obj$terms <- data_obj$terms[grepl(ontology_filter, data_obj$terms$ontology, ignore.case = TRUE), , drop = FALSE]
  }
  # Also filter multi-analysis structure if present
  if (!identical(ontology_filter, "all") && !is.null(data_obj$analyses)) {
    for (ana in names(data_obj$analyses)) {
      df <- data_obj$analyses[[ana]]$terms
      if (!is.null(df) && is.data.frame(df) && "ontology" %in% names(df)) {
        data_obj$analyses[[ana]]$terms <- df[grepl(ontology_filter, df$ontology, ignore.case = TRUE), , drop = FALSE]
      }
    }
  }
  # --- End ontology filter ---

  # Check for analyses structure (multiple comparison pairs)
  analyses <- data_obj$analyses %||% NULL

  if (!is.null(analyses) && is.list(analyses) && length(analyses) > 0) {
    # Multi-analysis structure: render each comparison pair as a scatter plot
    all_plots <- list()
    all_tables <- list()

    for (analysis_name in names(analyses)) {
      analysis <- analyses[[analysis_name]]
      df <- analysis$terms

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) next

      # Get axis labels from the analysis info
      x_comp <- analysis$x_comparison %||% "Comparison X"
      y_comp <- analysis$y_comparison %||% "Comparison Y"

      # Format comparison names for nicer labels (BafA1_vs_Control -> BafA1/Control)
      x_label <- gsub("_vs_", "/", x_comp)
      y_label <- gsub("_vs_", "/", y_comp)

      # FIX: Strip "Top N" suffix for cleaner axis labels (e.g., "PC1 Top 50" -> "PC1")
      x_label <- sub("\\s*Top\\s*\\d+\\s*$", "", x_label, ignore.case = TRUE)
      y_label <- sub("\\s*Top\\s*\\d+\\s*$", "", y_label, ignore.case = TRUE)

      # Flip axis labels (score negation handled in scatter_xy)
      if (isTRUE(style$flip_x)) x_label <- tb_flip_comparison_label(x_label)
      if (isTRUE(style$flip_y)) y_label <- tb_flip_comparison_label(y_label)

      rendered <- tb_render_2dgofcs_scatter_xy(df, style, meta, x_label, y_label, plot_key = analysis_name)

      if (!is.null(rendered$plot)) {
        all_plots[[analysis_name]] <- rendered$plot
      }
      if (!is.null(rendered$table)) {
        all_tables[[analysis_name]] <- rendered$table
      }
    }

    if (length(all_plots) == 0) {
      return(list(plots = list(), tables = list()))
    }

    return(list(plots = all_plots, tables = all_tables))
  }

  # FIX: Extract score labels from data if available (reflects input data source)
  # Strip "Top N" suffix for cleaner display (e.g., "PC1 Top 50" -> "PC1")
  x_score_label <- data_obj$x_score_label %||% results$params$x_score_label %||% NULL
  y_score_label <- data_obj$y_score_label %||% results$params$y_score_label %||% NULL
  if (!is.null(x_score_label)) x_score_label <- sub("\\s*Top\\s*\\d+\\s*$", "", x_score_label, ignore.case = TRUE)  # "PC1 Top 50" -> "PC1"
  if (!is.null(y_score_label)) y_score_label <- sub("\\s*Top\\s*\\d+\\s*$", "", y_score_label, ignore.case = TRUE)  # "PC2 Top 50" -> "PC2"
  default_x_label <- if (!is.null(x_score_label) && nzchar(x_score_label)) x_score_label else "Score X"
  default_y_label <- if (!is.null(y_score_label) && nzchar(y_score_label)) y_score_label else "Score Y"

  # Flip axis labels for non-analysis paths
  if (isTRUE(style$flip_x)) default_x_label <- tb_flip_comparison_label(default_x_label)
  if (isTRUE(style$flip_y)) default_y_label <- tb_flip_comparison_label(default_y_label)

  # Fallback: check for BP/MF/CC or complex source tabs (older format)
  available_tabs <- character()
  for (tab in c("BP", "MF", "CC", "CORUM", "ComplexPortal")) {
    if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
      available_tabs <- c(available_tabs, tab)
    }
  }

  if (length(available_tabs) > 0) {
    all_plots <- list()
    all_tables <- list()

    for (tab in available_tabs) {
      tab_data <- data_obj[[tab]]
      df <- tab_data$terms %||% tab_data$data %||% tab_data

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) next

      plot_key <- paste0(tolower(tab), "_plot")
      rendered <- tb_render_2dgofcs_scatter_xy(df, style, meta, default_x_label, default_y_label, plot_key = plot_key)

      if (!is.null(rendered$plot)) {
        all_plots[[plot_key]] <- rendered$plot
      }
      if (!is.null(rendered$table)) {
        all_tables[[paste0(tolower(tab), "_table")]] <- rendered$table
      }
    }

    return(list(plots = all_plots, tables = all_tables, tabs = available_tabs))
  }

  # Simplest fallback: single terms data.frame
  df <- data_obj$terms %||% data_obj
  if (is.null(df) || !is.data.frame(df)) {
    return(list(plots = list(), tables = list()))
  }

  rendered <- tb_render_2dgofcs_scatter_xy(df, style, meta, default_x_label, default_y_label, plot_key = "2dgofcs_plot")

  list(
    plots = if (!is.null(rendered$plot)) list(`2dgofcs_plot` = rendered$plot) else list(),
    tables = if (!is.null(rendered$table)) list(`2dgofcs_table` = rendered$table) else list()
  )
}

# Helper: render a single 2D GO-FCS scatter plot (X vs Y)
tb_render_2dgofcs_scatter_xy <- function(df, style, meta, x_label, y_label, plot_key = "2dgofcs_plot") {
  tb_require_pkg("ggplot2")

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(plot = NULL, table = NULL))
  }

  # Detect complex mode for label adjustments
  is_complex_mode <- tb_detect_complex_mode(df)
  size_legend_name <- if (is_complex_mode) "# Subunits" else "# Genes"

  # Normalize column names
  if (!"term_id" %in% names(df)) {
    for (col in c("TermID", "termID", "go_id", "GO", "ID", "complex_id")) {
      if (col %in% names(df)) { df$term_id <- df[[col]]; break }
    }
  }
  if (!"term" %in% names(df)) {
    for (col in c("term_name", "Term", "pathway", "Pathway", "term_id")) {
      if (col %in% names(df)) { df$term <- df[[col]]; break }
    }
  }
  if (!"fdr" %in% names(df)) {
    for (col in c("FDR", "p.adjust", "padj", "pval")) {
      if (col %in% names(df)) { df$fdr <- df[[col]]; break }
    }
    if (!"fdr" %in% names(df)) df$fdr <- 0.05
  }
  if (!"n" %in% names(df)) {
    for (col in c("n_genes", "count", "Count", "GeneCount", "size")) {
      if (col %in% names(df)) { df$n <- df[[col]]; break }
    }
    if (!"n" %in% names(df)) df$n <- 5
  }

  # Use score_x and score_y for the scatter plot axes
  if (!"score_x" %in% names(df)) {
    df$score_x <- 0
  }
  if (!"score_y" %in% names(df)) {
    df$score_y <- 0
  }

  # Flip axis scores (negate values to reverse enrichment direction)
  if (isTRUE(style$flip_x)) df$score_x <- -df$score_x
  if (isTRUE(style$flip_y)) df$score_y <- -df$score_y

  # Filter by ontology (BP/CC/MF toggle) - apply to both table and plot
  ontology_filter <- style$ontology_filter %||% "all"
  if (ontology_filter != "all" && "ontology" %in% names(df)) {
    df <- df[toupper(df$ontology) == toupper(ontology_filter), , drop = FALSE]
  }

  # Keep full df for table (with all terms after ontology filter), filter only for plot
  df_all <- df
  hidden_terms <- character(0)
  term_labels <- list()
  if (!is.null(meta) && !is.null(meta$visibility)) {
    hidden_terms <- meta$visibility$hidden_terms %||% character(0)
    term_labels <- meta$visibility$term_labels %||% list()
  }
  df_plot <- df_all[!(df_all$term %in% hidden_terms), , drop = FALSE]

  # Apply custom term labels to plot data
  if (length(term_labels) > 0 && nrow(df_plot) > 0) {
    df_plot$term_original <- df_plot$term
    for (i in seq_len(nrow(df_plot))) {
      orig <- df_plot$term[i]
      if (!is.null(term_labels[[orig]]) && nzchar(term_labels[[orig]])) {
        df_plot$term[i] <- term_labels[[orig]]
      }
    }
  } else if (nrow(df_plot) > 0) {
    df_plot$term_original <- df_plot$term
  }

  if (nrow(df_all) == 0) {
    return(list(plot = NULL, table = NULL))
  }

  # Add hover text for plotly interactivity
  # Use "Subunits" for complex mode, "Genes" for GO mode
  size_hover_label <- if (is_complex_mode) "Subunits" else "Genes"
  if (nrow(df_plot) > 0) {
    df_plot$hover_text <- sprintf(
      "%s\nScore X: %.3f\nScore Y: %.3f\nFDR: %.2e\n%s: %d",
      df_plot$term, df_plot$score_x, df_plot$score_y, df_plot$fdr, size_hover_label, df_plot$n
    )
  }

  # Style parameters
  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"
  if (identical(tolower(style$view_mode %||% ""), "interactive")) {
    axis_text_size <- 12
    axis_style <- "clean"
  }
  alpha <- tb_num(style$dot_alpha %||% style$alpha, 0.8)
  fdr_palette <- style$fdr_palette %||% "yellow_cap"

  # Axis ranges (defaults preserve existing [-1, 1] behavior)
  xlim <- suppressWarnings(as.numeric(c(style$x_min %||% -1, style$x_max %||% 1)))
  ylim <- suppressWarnings(as.numeric(c(style$y_min %||% -1, style$y_max %||% 1)))
  if (length(xlim) != 2 || any(!is.finite(xlim))) xlim <- c(-1, 1)
  if (length(ylim) != 2 || any(!is.finite(ylim))) ylim <- c(-1, 1)
  xlim <- sort(xlim)
  ylim <- sort(ylim)

  x_span <- diff(xlim)
  y_span <- diff(ylim)
  if (!is.finite(x_span) || x_span <= 0) x_span <- 2
  if (!is.finite(y_span) || y_span <= 0) y_span <- 2

  # Color mode
  cm <- style$color_mode %||% "fdr"
  use_flat_color <- (cm == "flat")
  flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

  # Create true 2D scatter plot: X = score_x, Y = score_y
  # FIX: Derive panel border thickness from axis_style to match axis appearance (clean=0.5, bold=1.5)
  axis_line_size <- if (tolower(axis_style %||% "clean") == "bold") 1.5 else 0.5

  # FIX: Reference line (x=0, y=0) controls - independent thickness, dotted, hide
  show_ref_lines <- isTRUE(style$show_ref_lines %||% TRUE)
  ref_line_size <- tb_num(style$ref_line_size, 1)
  ref_line_dotted <- isTRUE(style$ref_line_dotted %||% FALSE)
  ref_line_type <- if (ref_line_dotted) "dotted" else "solid"

  # Generate plot only if there are visible terms
  p <- NULL
  if (nrow(df_plot) > 0) {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = score_x, y = score_y))

    # FIX: Reference lines (x=0, y=0) with independent controls
    if (show_ref_lines) {
      p <- p +
        ggplot2::geom_hline(yintercept = 0, linetype = ref_line_type,
                           color = "black", linewidth = ref_line_size) +
        ggplot2::geom_vline(xintercept = 0, linetype = ref_line_type,
                           color = "black", linewidth = ref_line_size)
    }

    # FIX: Add diagonal guidelines (y=x and y=-x) when enabled
    show_diagonal_guides <- isTRUE(style$show_diagonal_guides %||% FALSE)
    if (show_diagonal_guides) {
      diagonal_guide_size <- style$diagonal_guide_size %||% 1
      p <- p +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dotted",
                            color = "gray60", linewidth = diagonal_guide_size, alpha = 0.7) +
        ggplot2::geom_abline(intercept = 0, slope = -1, linetype = "dotted",
                            color = "gray60", linewidth = diagonal_guide_size, alpha = 0.7)
    }

    # Add points with proper FDR color scale and legend (text aes for plotly hover)
    if (use_flat_color) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(size = n), color = flat_color, alpha = alpha) +
        ggplot2::scale_size_continuous(name = size_legend_name, range = c(3, 12))
    } else {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(size = n, color = fdr), alpha = alpha) +
        tb_fdr_scale("color", df_plot$fdr, palette = fdr_palette) +
        ggplot2::scale_size_continuous(name = size_legend_name, range = c(3, 12))
    }

    # FIX: Use label_font_size for axis titles as well for consistent text-size control
    label_font_size_val <- style$label_font_size %||% 12

    p <- p +
      ggplot2::coord_fixed(xlim = xlim, ylim = ylim, ratio = 1) +
      ggplot2::labs(x = x_label, y = y_label) +
      tb_theme_base(axis_text_size, axis_style = axis_style) +
      ggplot2::theme(
        # FIX: Panel border linewidth matches axis_style (clean=0.5, bold=1.5)
        # IMPORTANT: Remove axis.line to prevent double-drawing (panel.border draws all 4 sides)
        panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = axis_line_size),
        axis.line = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = label_font_size_val),
        axis.title.y = ggplot2::element_text(size = label_font_size_val)
      )

    # Saved plotly label positions (reflected in ggplot)
    saved <- meta$plotly$labels_by_plot[[plot_key]] %||%
      meta$plotly$labels_by_plot$default %||%
      meta$plotly$labels %||% list()

    # For 2dgofcs: ALL visible points get labels (visibility controlled via table checkbox)
    # Labels are tied to point visibility - if a point is visible, it has a label
    labs <- if (nrow(df_plot) > 0) {
      df_plot$term
    } else {
      character(0)
    }

    if (length(labs) > 0) {
      df_lab <- df_plot[df_plot$term %in% labs, , drop = FALSE]

      if (nrow(df_lab) > 0) {
        df_lab$lx <- NA_real_
        df_lab$ly <- NA_real_

        for (i in seq_len(nrow(df_lab))) {
          id <- as.character(df_lab$term_id[[i]] %||% df_lab$term_original[[i]] %||% df_lab$term[[i]])
          s <- saved[[id]] %||% list()

          # FIX: Improved default offset logic to place labels within plot area
          # Push labels away from edges and toward center when near boundaries
          px <- df_lab$score_x[[i]]
          py <- df_lab$score_y[[i]]

           # Calculate default offset direction based on point position (scaled to axis range)
           x_mid <- mean(xlim)
           x_right_edge <- xlim[[1]] + 0.85 * x_span
           x_left_edge <- xlim[[1]] + 0.15 * x_span
           y_top_edge <- ylim[[1]] + 0.85 * y_span

           default_x_offset <- if (px > x_right_edge) {
             -0.06 * x_span
           } else if (px < x_left_edge) {
             0.06 * x_span
           } else if (px < x_mid) {
             -0.06 * x_span
           } else {
             0.06 * x_span
           }
           default_y_offset <- if (py > y_top_edge) -0.04 * y_span else 0.04 * y_span

          pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)
          lx <- pos$x
          ly <- pos$y

          if (!is.finite(lx)) lx <- px
          if (!is.finite(ly)) ly <- py

          if (is.finite(lx) && is.finite(ly) && is.null(s$x_range) && is.null(s$y_range)) {
            if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) || !is.finite(suppressWarnings(as.numeric(s$y)))) {
              lx <- px + default_x_offset
              ly <- py + default_y_offset
            }
          }

           # Clamp to visible plot window with padding so labels can't "disappear" via clipping
           pad_x <- 0.05 * x_span
           pad_y <- 0.05 * y_span
           lx <- max(xlim[[1]] + pad_x, min(xlim[[2]] - pad_x, lx))
           ly <- max(ylim[[1]] + pad_y, min(ylim[[2]] - pad_y, ly))

          df_lab$lx[[i]] <- lx
          df_lab$ly[[i]] <- ly
        }

        label_font_size <- style$label_font_size %||% 12

        p <- p +
          ggplot2::geom_segment(
            data = df_lab,
            ggplot2::aes(x = score_x, y = score_y, xend = lx, yend = ly),
            linewidth = 0.3, color = "black"
          ) +
          ggplot2::geom_text(
            data = df_lab,
            ggplot2::aes(x = lx, y = ly, label = term),
            size = label_font_size / 3, color = "black", hjust = 0.5, vjust = 0
          )
      }
    }
  }

  # Format display columns - use ALL terms (including hidden) for reversible hide/show
  df_display <- df_all
  if ("fdr" %in% names(df_display)) {
    df_display$fdr <- tb_format_fdr(df_display$fdr)
  }
  if ("n" %in% names(df_display)) {
    df_display$n <- suppressWarnings(as.integer(df_display$n))
  }
  if ("score_x" %in% names(df_display)) {
    df_display$score_x <- tb_format_sig(df_display$score_x, 2)
  }
  if ("score_y" %in% names(df_display)) {
    df_display$score_y <- tb_format_sig(df_display$score_y, 2)
  }

  list(plot = p, table = df_display)
}

# ---- Normalization & Validation Helpers --------------------------------------

tb_assert_named_list <- function(x, what = "object") {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list, got %s", what, class(x)[[1]]))
  }
  nms <- names(x)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(sprintf("%s must be a named list with all non-empty names", what))
  }
  invisible(x)
}

tb_require_names_present <- function(x, required_names, what = "object", engine_id = "") {
  if (length(required_names) == 0) return(invisible(x))

  actual_names <- names(x) %||% character()
  missing <- setdiff(required_names, actual_names)

  if (length(missing) > 0) {
    msg <- sprintf(
      "Engine '%s': %s missing required names: %s",
      engine_id,
      what,
      paste(missing, collapse = ", ")
    )
    stop(msg)
  }
  invisible(x)
}

tb_normalize_plots <- function(x, default_name = "main") {
  if (is.null(x)) return(list())

  # Single ggplot object -> named list
  if (inherits(x, "ggplot")) {
    return(stats::setNames(list(x), default_name))
  }

  # Single htmlwidget (e.g., visNetwork) -> named list
  if (inherits(x, "htmlwidget")) {
    return(stats::setNames(list(x), default_name))
  }

  # Already a list
  if (!is.list(x)) return(list())

  # Remove NULLs
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) return(list())

  # Keep ggplot objects and htmlwidgets (e.g., visNetwork)
  is_plot <- vapply(x, function(p) inherits(p, "ggplot") || inherits(p, "htmlwidget"), logical(1))
  x <- x[is_plot]
  if (length(x) == 0) return(list())

  # Ensure names
  nms <- names(x)
  if (is.null(nms) || length(nms) != length(x)) {
    nms <- rep("", length(x))
  }
  bad <- !nzchar(nms)
  if (any(bad)) {
    if (length(x) == 1) {
      nms[bad] <- default_name
    } else {
      nms[bad] <- paste0(default_name, "_", which(bad))
    }
  }
  names(x) <- nms
  x
}

tb_normalize_tables <- function(x, default_name = "table") {
  if (is.null(x)) return(list())

  # Single data.frame/matrix -> named list
  if (is.data.frame(x) || is.matrix(x)) {
    return(stats::setNames(list(x), default_name))
  }

  # Already a list
  if (!is.list(x)) return(list())

  # Remove NULLs
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) return(list())

  # Ensure all are data.frame or matrix
  is_tbl <- vapply(x, function(obj) is.data.frame(obj) || is.matrix(obj), logical(1))
  x <- x[is_tbl]
  if (length(x) == 0) return(list())

  # Ensure names
  nms <- names(x)
  if (is.null(nms) || length(nms) != length(x)) {
    nms <- rep("", length(x))
  }
  bad <- !nzchar(nms)
  if (any(bad)) {
    if (length(x) == 1) {
      nms[bad] <- default_name
    } else {
      nms[bad] <- paste0(default_name, "_", which(bad))
    }
  }
  names(x) <- nms
  x
}

tb_normalize_render_output <- function(engine_def, rendered) {
  engine_id <- engine_def$engine_id %||% "unknown"
  render_spec <- engine_def$render_spec %||% list()

  # Extract and normalize components
  plots <- tb_normalize_plots(rendered$plots %||% rendered$plot)
  tables <- tb_normalize_tables(rendered$tables %||% rendered$table)
  tabs <- rendered$tabs %||% NULL

  # Validate against render_spec if defined
  spec_plots <- render_spec$plots %||% character()
  spec_tables <- render_spec$tables %||% character()
  spec_tabs <- render_spec$tabs %||% NULL

  # For engines with optional tabs (goora, 1dgofcs, 2dgofcs),
  # only validate the plots/tables that actually exist
  # Don't require all tab-specific plots if tabs aren't present
  if (!is.null(tabs) && length(tabs) > 0) {
    # Multi-tab output: validate that we have plots/tables
    # but don't enforce exact names since they're dynamic
  } else {
    # Single-tab output: validate required plots/tables
    # Only check non-tab-specific names
    if (length(spec_plots) > 0) {
      # Filter out tab-specific plot names (bp_plot, mf_plot, cc_plot)
      required_plots <- spec_plots[!grepl("^(bp|mf|cc)_", spec_plots)]
      if (length(required_plots) > 0 && length(plots) > 0) {
        # Soft validation: warn if missing but don't error
        # since some engines may return empty results legitimately
      }
    }
  }

  list(
    plots = plots,
    tables = tables,
    tabs = tabs
  )
}

# ---- Metabolite Enrichment renderers -----------------------------------------

# MSEA - Metabolite Set Enrichment Analysis (Pathway ORA)
tb_render_msea <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  data_obj <- results$data %||% list()

  # Pathway database filter
  db_filter <- style$pathway_db_filter %||% "all"

  # Get terms dataframe
  df <- data_obj$terms
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(plots = list(), tables = list()))
  }

  # Apply pathway database filter if not "all"
  if (!identical(db_filter, "all") && "pathway_type" %in% names(df)) {
    df <- df[grepl(db_filter, df$pathway_type, ignore.case = TRUE), , drop = FALSE]
    if (nrow(df) == 0) {
      return(list(plots = list(), tables = list()))
    }
  }

  # Normalize column names
  if (!"pathway_id" %in% names(df)) {
    for (col in c("term_id", "ID", "pathway", "Pathway")) {
      if (col %in% names(df)) { df$pathway_id <- df[[col]]; break }
    }
  }
  if (!"pathway_name" %in% names(df)) {
    for (col in c("term", "term_name", "Name", "name", "pathway_id")) {
      if (col %in% names(df)) { df$pathway_name <- df[[col]]; break }
    }
  }
  if (!"fdr" %in% names(df)) {
    for (col in c("FDR", "p.adjust", "padj", "pvalue")) {
      if (col %in% names(df)) { df$fdr <- df[[col]]; break }
    }
    if (!"fdr" %in% names(df)) df$fdr <- 1
  }
  if (!"n" %in% names(df)) {
    for (col in c("n_metabolites", "count", "Count", "n_overlap", "overlap_count", "size")) {
      if (col %in% names(df)) { df$n <- df[[col]]; break }
    }
    if (!"n" %in% names(df)) df$n <- 1
  }
  if (!"metabolite_ids" %in% names(df)) {
    for (col in c("metabolites", "compounds", "hits")) {
      if (col %in% names(df)) { df$metabolite_ids <- df[[col]]; break }
    }
    if (!"metabolite_ids" %in% names(df)) df$metabolite_ids <- ""
  }
  if (!"fold_enrichment" %in% names(df)) {
    for (col in c("FoldEnrichment", "foldEnrichment", "enrichment_ratio")) {
      if (col %in% names(df)) { df$fold_enrichment <- df[[col]]; break }
    }
    if (!"fold_enrichment" %in% names(df)) df$fold_enrichment <- 1
  }

  # Store original name for lookups
  df$pathway_name_original <- df$pathway_name

  # Handle visibility (hidden terms, custom labels)
  hidden_terms <- character(0)
  term_labels <- list()
  if (!is.null(meta) && !is.null(meta$visibility)) {
    hidden_terms <- meta$visibility$hidden_terms %||% character(0)
    term_labels <- meta$visibility$term_labels %||% list()
  }

  df_all <- df
  df_plot <- df_all[!(df_all$pathway_id %in% hidden_terms) & !(df_all$pathway_name_original %in% hidden_terms), , drop = FALSE]

  # Apply custom labels
  if (length(term_labels) > 0 && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      pid <- df_plot$pathway_id[i]
      orig <- df_plot$pathway_name_original[i]
      custom_label <- term_labels[[pid]] %||% term_labels[[orig]]
      if (!is.null(custom_label) && nzchar(custom_label)) {
        df_plot$pathway_name[i] <- custom_label
      }
    }
  }

  # Show pathway ID in labels if requested
  show_pathway_id <- isTRUE(style$show_pathway_id %||% FALSE)
  if (show_pathway_id && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      name <- df_plot$pathway_name[i]
      pid <- df_plot$pathway_id[i]
      if (!is.na(pid) && !grepl(pid, name, fixed = TRUE)) {
        df_plot$pathway_name[i] <- paste0(name, " (", pid, ")")
      }
    }
  }

  if (nrow(df_plot) == 0) {
    return(list(plots = list(), tables = list(msea_table = df_all)))
  }

  # Style parameters
  plot_type <- style$plot_type %||% "bar"
  axis_text_size <- tb_num(style$axis_text_size, 20)
  font_size <- tb_num(style$font_size, 14)
  alpha <- tb_num(style$alpha, 0.8)
  fdr_palette <- style$fdr_palette %||% "yellow_cap"
  flip_axis <- isTRUE(style$flip_axis %||% FALSE)
  x_axis_metric <- style$x_axis_metric %||% "neglog10_fdr"

  cm <- style$color_mode %||% "fdr"
  use_flat_color <- (cm == "flat")
  flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

  # Prepare for plotting
  df_plot$neglog10_fdr <- -log10(pmax(df_plot$fdr, 1e-300))

  # Limit to max_terms
  max_terms <- tb_num(style$max_terms %||% results$params$max_terms, 20)
  if (nrow(df_plot) > max_terms) {
    df_plot <- df_plot[order(df_plot$fdr), , drop = FALSE]
    df_plot <- df_plot[seq_len(max_terms), , drop = FALSE]
  }

  # Compute x-axis value based on selected metric
  if (x_axis_metric == "neglog10_fdr") {
    df_plot$x_value <- df_plot$neglog10_fdr
    x_label <- "-log10(FDR)"
  } else {
    df_plot$x_value <- df_plot$fold_enrichment
    x_label <- "Fold Enrichment"
  }

  # Order by x_value for display
  df_plot$pathway_name <- factor(df_plot$pathway_name, levels = unique(df_plot$pathway_name[order(df_plot$x_value)]))

  # Build plot
  if (plot_type == "dot") {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = pathway_name, size = n))
    if (use_flat_color) {
      p <- p + ggplot2::geom_point(color = flat_color, alpha = alpha)
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = fdr), alpha = alpha) +
        tb_fdr_scale("color", df_plot$fdr, palette = fdr_palette)
    }
    p <- p + ggplot2::labs(x = x_label, y = NULL, size = "Count", color = "-log10(FDR)")
  } else {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x_value, y = pathway_name))
    if (use_flat_color) {
      p <- p + ggplot2::geom_col(fill = flat_color, alpha = alpha)
    } else {
      p <- p + ggplot2::geom_col(ggplot2::aes(fill = fdr), alpha = alpha) +
        tb_fdr_scale("fill", df_plot$fdr, palette = fdr_palette)
    }
    p <- p + ggplot2::labs(x = x_label, y = NULL, fill = "-log10(FDR)")
  }

  # Query count subtitle
  show_query_count <- isTRUE(style$show_query_count %||% TRUE)
  n_query <- results$data$query_count %||% results$data$query_info$n_query %||% NA_integer_
  if (show_query_count && is.finite(as.numeric(n_query))) {
    p <- p + ggplot2::labs(subtitle = sprintf("n = %d", as.integer(n_query)))
  }

  p <- p + ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.text.x = ggplot2::element_text(size = axis_text_size * 0.8),
      legend.position = "right"
    )

  if (flip_axis) {
    p <- p + ggplot2::scale_x_reverse()
  }

  # Determine output key based on filter
  plot_key <- paste0(tolower(db_filter), "_plot")
  table_key <- paste0(tolower(db_filter), "_table")

  list(
    plots = setNames(list(p), plot_key),
    tables = setNames(list(df_all), table_key)
  )
}

# Chemical Class Enrichment
tb_render_class_enrichment <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  data_obj <- results$data %||% list()

  df <- data_obj$terms
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(plots = list(), tables = list()))
  }

  # Normalize column names
  if (!"class_name" %in% names(df)) {
    for (col in c("class", "Class", "term", "term_name")) {
      if (col %in% names(df)) { df$class_name <- df[[col]]; break }
    }
  }
  if (!"fdr" %in% names(df)) {
    for (col in c("FDR", "p.adjust", "padj", "pvalue")) {
      if (col %in% names(df)) { df$fdr <- df[[col]]; break }
    }
    if (!"fdr" %in% names(df)) df$fdr <- 1
  }
  if (!"n" %in% names(df)) {
    for (col in c("n_metabolites", "count", "Count", "n_overlap", "overlap_count")) {
      if (col %in% names(df)) { df$n <- df[[col]]; break }
    }
    if (!"n" %in% names(df)) df$n <- 1
  }
  if (!"metabolite_ids" %in% names(df)) {
    for (col in c("metabolites", "compounds", "hits")) {
      if (col %in% names(df)) { df$metabolite_ids <- df[[col]]; break }
    }
    if (!"metabolite_ids" %in% names(df)) df$metabolite_ids <- ""
  }
  if (!"fold_enrichment" %in% names(df)) {
    for (col in c("FoldEnrichment", "foldEnrichment", "enrichment_ratio")) {
      if (col %in% names(df)) { df$fold_enrichment <- df[[col]]; break }
    }
    if (!"fold_enrichment" %in% names(df)) df$fold_enrichment <- 1
  }

  df$class_name_original <- df$class_name

  # Handle visibility
  hidden_terms <- character(0)
  term_labels <- list()
  if (!is.null(meta) && !is.null(meta$visibility)) {
    hidden_terms <- meta$visibility$hidden_terms %||% character(0)
    term_labels <- meta$visibility$term_labels %||% list()
  }

  df_all <- df
  df_plot <- df_all[!(df_all$class_name_original %in% hidden_terms), , drop = FALSE]

  # Apply custom labels
  if (length(term_labels) > 0 && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      orig <- df_plot$class_name_original[i]
      custom_label <- term_labels[[orig]]
      if (!is.null(custom_label) && nzchar(custom_label)) {
        df_plot$class_name[i] <- custom_label
      }
    }
  }

  if (nrow(df_plot) == 0) {
    return(list(plots = list(), tables = list(class_table = df_all)))
  }

  # Style parameters
  plot_type <- style$plot_type %||% "bar"
  axis_text_size <- tb_num(style$axis_text_size, 20)
  font_size <- tb_num(style$font_size, 14)
  alpha <- tb_num(style$alpha, 0.8)
  fdr_palette <- style$fdr_palette %||% "yellow_cap"
  flip_axis <- isTRUE(style$flip_axis %||% FALSE)

  cm <- style$color_mode %||% "fdr"
  use_flat_color <- (cm == "flat")
  flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

  df_plot$neglog10_fdr <- -log10(pmax(df_plot$fdr, 1e-300))

  max_terms <- tb_num(style$max_terms %||% results$params$max_terms, 20)
  if (nrow(df_plot) > max_terms) {
    df_plot <- df_plot[order(df_plot$fdr), , drop = FALSE]
    df_plot <- df_plot[seq_len(max_terms), , drop = FALSE]
  }

  df_plot$class_name <- factor(df_plot$class_name, levels = unique(df_plot$class_name[order(df_plot$fold_enrichment)]))

  if (plot_type == "dot") {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fold_enrichment, y = class_name, size = n))
    if (use_flat_color) {
      p <- p + ggplot2::geom_point(color = flat_color, alpha = alpha)
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = neglog10_fdr), alpha = alpha) +
        tb_fdr_color_scale(fdr_palette, "continuous")
    }
    p <- p + ggplot2::labs(x = "Fold Enrichment", y = NULL, size = "Count", color = "-log10(FDR)")
  } else {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fold_enrichment, y = class_name))
    if (use_flat_color) {
      p <- p + ggplot2::geom_col(fill = flat_color, alpha = alpha)
    } else {
      p <- p + ggplot2::geom_col(ggplot2::aes(fill = neglog10_fdr), alpha = alpha) +
        tb_fdr_color_scale(fdr_palette, "fill")
    }
    p <- p + ggplot2::labs(x = "Fold Enrichment", y = NULL, fill = "-log10(FDR)")
  }

  p <- p + ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.text.x = ggplot2::element_text(size = axis_text_size * 0.8),
      legend.position = "right"
    )

  if (flip_axis) {
    p <- p + ggplot2::scale_x_reverse()
  }

  list(
    plots = list(class_plot = p),
    tables = list(class_table = df_all)
  )
}

# Pathway FCS (Functional Class Scoring) for Metabolites
tb_render_pathway_fcs <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  data_obj <- results$data %||% list()

  # Handle multi-comparison structure from standalone FC mode
  analyses <- data_obj$analyses %||% NULL
  if (!is.null(analyses) && is.list(analyses) && length(analyses) > 0) {
    all_plots <- list()
    all_tables <- list()

    for (comp_name in names(analyses)) {
      analysis <- analyses[[comp_name]]
      comp_label <- analysis$score_label %||% {
        parts <- unlist(strsplit(comp_name, "_vs_", fixed = TRUE))
        paste0("log2(", paste(parts, collapse = "/"), ")")
      }
      if (isTRUE(style$flip_fc)) comp_label <- tb_flip_comparison_label(comp_label)
      comp_results <- list(
        engine_id = "pathway_fcs",
        params = results$params,
        data = list(
          terms = analysis$terms,
          score_label = comp_label
        )
      )

      rendered <- tb_render_pathway_fcs(comp_results, style, meta)

      for (pn in names(rendered$plots)) {
        all_plots[[paste0(comp_name, "_plot")]] <- rendered$plots[[pn]]
      }
      for (tn in names(rendered$tables)) {
        all_tables[[paste0(comp_name, "_table")]] <- rendered$tables[[tn]]
      }
    }

    return(list(
      plots = all_plots,
      tables = all_tables,
      comparisons = names(analyses)
    ))
  }

  # Pathway database filter
  db_filter <- style$pathway_db_filter %||% "all"

  # Score label for x-axis
  score_label <- data_obj$score_label %||% results$params$score_label %||% "Normalized Enrichment Score"

  df <- data_obj$terms
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(plots = list(), tables = list()))
  }

  # Apply pathway database filter if not "all"
  if (!identical(db_filter, "all") && "pathway_type" %in% names(df)) {
    df <- df[grepl(db_filter, df$pathway_type, ignore.case = TRUE), , drop = FALSE]
    if (nrow(df) == 0) {
      return(list(plots = list(), tables = list()))
    }
  }

  # Normalize column names
  if (!"pathway_id" %in% names(df)) {
    for (col in c("term_id", "ID", "pathway", "Pathway")) {
      if (col %in% names(df)) { df$pathway_id <- df[[col]]; break }
    }
  }
  if (!"pathway_name" %in% names(df)) {
    for (col in c("term", "term_name", "Name", "name", "pathway_id")) {
      if (col %in% names(df)) { df$pathway_name <- df[[col]]; break }
    }
  }
  if (!"fdr" %in% names(df)) {
    for (col in c("FDR", "p.adjust", "padj", "pvalue")) {
      if (col %in% names(df)) { df$fdr <- df[[col]]; break }
    }
    if (!"fdr" %in% names(df)) df$fdr <- 1
  }
  if (!"score" %in% names(df)) {
    for (col in c("NES", "nes", "enrichment_score", "ES")) {
      if (col %in% names(df)) { df$score <- df[[col]]; break }
    }
    if (!"score" %in% names(df)) df$score <- 0
  }
  if (!"metabolite_ids" %in% names(df)) {
    for (col in c("metabolites", "compounds", "hits", "leading_edge")) {
      if (col %in% names(df)) { df$metabolite_ids <- df[[col]]; break }
    }
    if (!"metabolite_ids" %in% names(df)) df$metabolite_ids <- ""
  }
  if (!"n" %in% names(df)) {
    for (col in c("n_metabolites", "count", "Count", "n_overlap", "overlap_count", "size")) {
      if (col %in% names(df)) { df$n <- df[[col]]; break }
    }
    if (!"n" %in% names(df)) df$n <- 1
  }

  # Flip score direction if enabled (per-plot, viewer-only)
  flip_fc <- isTRUE(style$flip_fc %||% FALSE)
  if (flip_fc && !is.null(df$score)) {
    df$score <- -df$score
  }

  df$pathway_name_original <- df$pathway_name

  # Handle visibility
  hidden_terms <- character(0)
  term_labels <- list()
  if (!is.null(meta) && !is.null(meta$visibility)) {
    hidden_terms <- meta$visibility$hidden_terms %||% character(0)
    term_labels <- meta$visibility$term_labels %||% list()
  }

  df_all <- df
  df_plot <- df_all[!(df_all$pathway_id %in% hidden_terms) & !(df_all$pathway_name_original %in% hidden_terms), , drop = FALSE]

  # Apply custom labels
  if (length(term_labels) > 0 && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      pid <- df_plot$pathway_id[i]
      orig <- df_plot$pathway_name_original[i]
      custom_label <- term_labels[[pid]] %||% term_labels[[orig]]
      if (!is.null(custom_label) && nzchar(custom_label)) {
        df_plot$pathway_name[i] <- custom_label
      }
    }
  }

  show_pathway_id <- isTRUE(style$show_pathway_id %||% FALSE)
  if (show_pathway_id && nrow(df_plot) > 0) {
    for (i in seq_len(nrow(df_plot))) {
      name <- df_plot$pathway_name[i]
      pid <- df_plot$pathway_id[i]
      if (!is.na(pid) && !grepl(pid, name, fixed = TRUE)) {
        df_plot$pathway_name[i] <- paste0(name, " (", pid, ")")
      }
    }
  }

  if (nrow(df_plot) == 0) {
    return(list(plots = list(), tables = list(pathway_fcs_table = df_all)))
  }

  # Style parameters
  axis_text_size <- tb_num(style$axis_text_size, 20)
  font_size <- tb_num(style$font_size, 14)
  alpha <- tb_num(style$alpha, 0.8)
  fdr_palette <- style$fdr_palette %||% "yellow_cap"
  flip_axis <- isTRUE(style$flip_axis %||% FALSE)

  cm <- style$color_mode %||% "fdr"
  use_flat_color <- (cm == "flat")
  flat_color <- if (use_flat_color) (style$flat_color %||% "#B0B0B0") else NULL

  df_plot$neglog10_fdr <- -log10(pmax(df_plot$fdr, 1e-300))

  max_terms <- tb_num(style$max_terms %||% results$params$max_terms, 20)
  if (nrow(df_plot) > max_terms) {
    df_plot <- df_plot[order(df_plot$fdr), , drop = FALSE]
    df_plot <- df_plot[seq_len(max_terms), , drop = FALSE]
  }

  # Order by score for FCS
  df_plot$pathway_name <- factor(df_plot$pathway_name, levels = unique(df_plot$pathway_name[order(df_plot$score)]))

  # Build plot: branch on plot_type (bar vs dot) and color_mode (score vs fdr vs flat)
  plot_type <- style$plot_type %||% "bar"

  if (plot_type == "dot") {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = score, y = pathway_name, size = n))
    if (use_flat_color) {
      p <- p + ggplot2::geom_point(color = flat_color, alpha = alpha)
    } else if (cm == "score") {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = score), alpha = alpha) +
        ggplot2::scale_color_gradient2(
          low = "#2e66a0", mid = "#e0e0e0", high = "#d14f58", midpoint = 0,
          name = "Score"
        )
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = fdr), alpha = alpha) +
        tb_fdr_scale("color", df_plot$fdr, palette = fdr_palette)
    }
    p <- p + ggplot2::labs(x = score_label, y = NULL, size = "Count",
                           color = if (cm == "score") "Score" else "-log10(FDR)")
  } else {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = score, y = pathway_name))
    if (use_flat_color) {
      p <- p + ggplot2::geom_col(fill = flat_color, alpha = alpha)
    } else if (cm == "score") {
      p <- p + ggplot2::geom_col(ggplot2::aes(fill = score), alpha = alpha) +
        ggplot2::scale_fill_gradient2(
          low = "#2e66a0", mid = "#e0e0e0", high = "#d14f58", midpoint = 0,
          name = "Score"
        )
    } else {
      p <- p + ggplot2::geom_col(ggplot2::aes(fill = fdr), alpha = alpha) +
        tb_fdr_scale("fill", df_plot$fdr, palette = fdr_palette)
    }
    p <- p + ggplot2::labs(x = score_label, y = NULL,
                           fill = if (cm == "score") "Score" else "-log10(FDR)")
  }

  p <- p +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.text.x = ggplot2::element_text(size = axis_text_size * 0.8),
      legend.position = "right"
    )

  if (flip_axis) {
    p <- p + ggplot2::scale_x_reverse()
  }

  plot_key <- paste0(tolower(db_filter), "_plot")
  table_key <- paste0(tolower(db_filter), "_table")

  list(
    plots = setNames(list(p), plot_key),
    tables = setNames(list(df_all), table_key)
  )
}

# ---- Data Processor renderer -------------------------------------------------

tb_render_dataprocessor <- function(results, style, meta) {
  # Data Processor shows a summary table of substeps with before/after/changed values.
  # Prefer structured substep summary data; fall back to parsing log entries.

  substep_summary <- results$data$substep_summary %||% NULL
  if (!is.null(substep_summary) && is.data.frame(substep_summary) && nrow(substep_summary) > 0) {
    summary_df <- data.frame(
      Step = as.character(substep_summary$Step %||% seq_len(nrow(substep_summary))),
      Operation = as.character(substep_summary$Operation %||% ""),
      `Rows before` = as.integer(substep_summary$rows_before %||% NA_integer_),
      `Rows after` = as.integer(substep_summary$rows_after %||% NA_integer_),
      `Cells before` = as.integer(substep_summary$cells_before_non_na %||% NA_integer_),
      `Cells after` = as.integer(substep_summary$cells_after_non_na %||% NA_integer_),
      Changed = as.integer(substep_summary$cells_changed %||% NA_integer_),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    return(list(
      plots = list(),
      tables = list(summary = summary_df)
    ))
  }

  log_df <- results$data$log
  if (is.null(log_df) || !is.data.frame(log_df) || nrow(log_df) == 0) {
    return(list(
      plots = list(),
      tables = list(summary = data.frame(
        Step = "No log data",
        Operation = "",
        `Rows before` = NA_integer_,
        `Rows after` = NA_integer_,
        `Cells before` = NA_integer_,
        `Cells after` = NA_integer_,
        Changed = NA_integer_,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    ))
  }

  # Extract messages from log
  messages <- log_df$message
  if (is.null(messages)) {
    messages <- character()
  }

  # Parse substep information from log messages
  substep_info <- list()

  current_substep <- NULL
  before_rows <- NULL
  before_cells <- NULL

  for (msg in messages) {
    # Check for substep start
    start_match <- regmatches(msg, regexec("--- Substep (\\d+): ([^-]+) ---", msg))[[1]]
    if (length(start_match) >= 3) {
      # Save previous substep if exists
      if (!is.null(current_substep) && !is.null(current_substep$name)) {
        substep_info[[length(substep_info) + 1]] <- current_substep
      }

      # Start new substep
      current_substep <- list(
        step = as.integer(start_match[2]),
        name = trimws(start_match[3]),
        before = before_rows,
        after = NA_integer_,
        cells_before = before_cells,
        cells_after = NA_integer_,
        changed = NA_integer_
      )
      next
    }

    # Check for "Cells before: X, Cells after: Y, Changed: Z"
    cells_match <- regmatches(msg, regexec("Cells before: (\\d+), Cells after: (\\d+), Changed: (\\d+)", msg))[[1]]
    if (length(cells_match) >= 4 && !is.null(current_substep)) {
      current_substep$cells_before <- as.integer(cells_match[2])
      current_substep$cells_after <- as.integer(cells_match[3])
      current_substep$changed <- as.integer(cells_match[4])
      before_cells <- current_substep$cells_after
      next
    }

    # Check for "After substep N: X rows x Y columns"
    after_match <- regmatches(msg, regexec("After substep (\\d+): (\\d+) rows", msg))[[1]]
    if (length(after_match) >= 3 && !is.null(current_substep)) {
      current_substep$after <- as.integer(after_match[3])
      before_rows <- current_substep$after  # Next substep starts with this row count
      next
    }

    # Check for initial row count "Data loaded: X rows x Y columns"
    init_match <- regmatches(msg, regexec("Data loaded: (\\d+) rows", msg))[[1]]
    if (length(init_match) >= 2) {
      before_rows <- as.integer(init_match[2])
      next
    }

    # Check for initial cell count, if logged
    init_cells_match <- regmatches(msg, regexec("Cells before: (\\d+), Cells after: (\\d+), Changed: (\\d+)", msg))[[1]]
    if (length(init_cells_match) >= 4 && is.null(before_cells)) {
      before_cells <- as.integer(init_cells_match[2])
      next
    }
  }

  # Save last substep
  if (!is.null(current_substep) && !is.null(current_substep$name)) {
    substep_info[[length(substep_info) + 1]] <- current_substep
  }

  # Build summary table
  if (length(substep_info) == 0) {
    summary_df <- data.frame(
      Step = "No substeps found",
      Operation = "",
      `Rows before` = NA_integer_,
      `Rows after` = NA_integer_,
      `Cells before` = NA_integer_,
      `Cells after` = NA_integer_,
      Changed = NA_integer_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    summary_df <- data.frame(
      Step = vapply(substep_info, function(x) as.character(x$step %||% ""), character(1)),
      Operation = vapply(substep_info, function(x) x$name %||% "", character(1)),
      `Rows before` = vapply(substep_info, function(x) x$before %||% NA_integer_, integer(1)),
      `Rows after` = vapply(substep_info, function(x) x$after %||% NA_integer_, integer(1)),
      `Cells before` = vapply(substep_info, function(x) x$cells_before %||% NA_integer_, integer(1)),
      `Cells after` = vapply(substep_info, function(x) x$cells_after %||% NA_integer_, integer(1)),
      Changed = vapply(substep_info, function(x) x$changed %||% NA_integer_, integer(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  list(
    plots = list(),
    tables = list(summary = summary_df)
  )
}

# ---- (Old 2D GO-FCS Scatter Plot renderer removed - replaced by tb_render_2dgofcs_scatter_xy) ----

# ---- Half-Life (dSILAC) renderer ------------------------------------

tb_render_half_life <- function(results, style, meta) {
  data <- results$data
  if (is.null(data)) {
    return(list(
      plots = list(),
      tables = list(half_life_log = data.frame(
        Metric = "Error", Value = "No data", stringsAsFactors = FALSE, check.names = FALSE
      ))
    ))
  }

  half_life_log <- data$half_life_log
  if (is.null(half_life_log) || !is.data.frame(half_life_log)) {
    half_life_log <- data.frame(
      Metric = "No summary data", Value = "", stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  list(
    plots = list(),
    tables = list(half_life_log = half_life_log)
  )
}

# ---- dSILAC QC renderers ------------------------------------------------------

tb_render_dsilac_isotope_density <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  df <- results$data$values
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(
      plots = list(dsilac_isotope_density_plot =
                     ggplot2::ggplot() + ggplot2::labs(title = "No SILAC intensity data")),
      tables = list()
    ))
  }
  if (!all(c("value", "group", "channel") %in% names(df))) {
    stop("dsilac_isotope_density: values must have columns value, group, channel.")
  }

  log_transform <- style$log_transform %||% "log10"
  if (log_transform == "log10") {
    df$value[df$value <= 0] <- NA
    df$value <- log10(df$value)
  } else if (log_transform == "log2") {
    df$value[df$value <= 0] <- NA
    df$value <- log2(df$value)
  }
  df <- df[is.finite(df$value), , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(
      plots = list(dsilac_isotope_density_plot =
                     ggplot2::ggplot() + ggplot2::labs(title = "No finite values after transform")),
      tables = list()
    ))
  }

  if (isTRUE(style$pool_above %||% FALSE)) {
    pv <- tb_num(style$pool_value, 12)
    df$value[df$value > pv] <- pv * 0.99
  }

  # One row per biological sample (group + replicate), two columns (Light | Heavy).
  if ("replicate" %in% names(df) && !all(is.na(df$replicate))) {
    df$rep_label <- ifelse(is.na(df$replicate),
                           as.character(df$group),
                           paste(df$group, df$replicate))
  } else {
    df$rep_label <- df$sample %||% as.character(df$group)
  }
  rep_order <- unique(df[, c("group", "replicate", "rep_label"), drop = FALSE])
  rep_order <- rep_order[order(as.character(rep_order$group),
                               suppressWarnings(as.numeric(rep_order$replicate))),
                         , drop = FALSE]
  df$rep_label <- factor(df$rep_label, levels = unique(rep_order$rep_label))

  df$channel <- factor(df$channel, levels = c("light", "heavy"),
                       labels = c("Light", "Heavy"))
  df$group <- factor(df$group)

  plot_type <- style$plot_type %||% "density"
  layout_mode <- style$layout %||% "separate"
  alpha_val <- tb_num(style$alpha, 0.7)
  bins_n <- as.integer(tb_num(style$bins, 30))
  if (is.na(bins_n) || bins_n < 5) bins_n <- 30

  group_levels <- levels(df$group)
  if (length(group_levels) > 0) {
    group_colors <- scales::hue_pal()(length(group_levels))
    names(group_colors) <- group_levels
  } else {
    group_colors <- c(`(all)` = "#808080")
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = value, fill = group, color = group))
  if (identical(plot_type, "histogram")) {
    p <- p + ggplot2::geom_histogram(
      bins = bins_n, alpha = alpha_val, position = "identity",
      color = "black", linewidth = 0.3
    )
    y_label <- "Count"
  } else {
    p <- p + ggplot2::geom_density(alpha = alpha_val, linewidth = 0.6)
    y_label <- "Density"
  }

  p <- p +
    ggplot2::scale_fill_manual(values = group_colors, drop = FALSE) +
    ggplot2::scale_color_manual(values = group_colors, drop = FALSE) +
    ggplot2::labs(
      x = tb_log_axis_label(style$x_axis_title %||% "Intensity", log_transform),
      y = y_label, fill = "Group", color = "Group"
    ) +
    tb_theme_base(tb_num(style$axis_text_size, 18),
                  axis_style = style$axis_style %||% "clean")

  if (identical(layout_mode, "separate")) {
    # Ridge-style: each biological sample on its own row, Light | Heavy as columns.
    p <- p + ggplot2::facet_grid(
      rep_label ~ channel,
      scales = "free_y",
      switch = "y"
    ) + ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement  = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 11),
      strip.text.x      = ggplot2::element_text(size = 13, face = "bold"),
      panel.spacing.y   = grid::unit(0.2, "lines")
    )
  } else {
    # Overlay all samples within each channel column.
    p <- p + ggplot2::facet_wrap(~ channel, scales = "free_y", nrow = 1)
  }

  if (isTRUE(style$show_mean %||% TRUE)) {
    mean_type <- style$mean_type %||% "arithmetic"
    mean_fn <- switch(mean_type,
                      "harmonic" = function(x) {
                        x <- x[is.finite(x) & x > 0]
                        if (length(x) == 0) NA_real_ else length(x) / sum(1 / x)
                      },
                      "median" = function(x) stats::median(x, na.rm = TRUE),
                      function(x) mean(x, na.rm = TRUE))

    if (identical(layout_mode, "separate")) {
      mean_df <- do.call(rbind, lapply(
        split(df, list(df$rep_label, df$channel), drop = TRUE),
        function(d) data.frame(
          rep_label = d$rep_label[1], channel = d$channel[1],
          group = d$group[1], m = mean_fn(d$value),
          stringsAsFactors = FALSE
        )))
    } else {
      mean_df <- do.call(rbind, lapply(
        split(df, list(df$group, df$channel), drop = TRUE),
        function(d) data.frame(
          group = d$group[1], channel = d$channel[1],
          m = mean_fn(d$value), stringsAsFactors = FALSE
        )))
    }

    p <- p + ggplot2::geom_vline(
      data = mean_df,
      ggplot2::aes(xintercept = m, color = group),
      linetype = "dashed", linewidth = tb_num(style$mean_line_size, 1),
      show.legend = FALSE
    )

    if (isTRUE(style$show_mean_value %||% FALSE)) {
      p <- p + ggplot2::geom_text(
        data = mean_df,
        ggplot2::aes(x = m, y = Inf, label = sprintf("%.2f", m), color = group),
        vjust = 1.5, hjust = -0.1,
        size = tb_num(style$mean_text_size, 14) / .pt,
        show.legend = FALSE, inherit.aes = FALSE
      )
    }
  }

  if (identical(style$x_range_mode %||% "auto", "manual")) {
    p <- p + ggplot2::coord_cartesian(
      xlim = c(tb_num(style$x_min, 0), tb_num(style$x_max, 12))
    )
  }

  list(
    plots = list(dsilac_isotope_density_plot = p),
    tables = list(dsilac_isotope_density_summary = results$data$summary %||% data.frame())
  )
}

tb_render_dsilac_isotope_density_peptide <- function(results, style, meta) {
  tb_render_dsilac_isotope_density(results, style, meta)
}

tb_render_dsilac_isotope_density_protein <- function(results, style, meta) {
  tb_render_dsilac_isotope_density(results, style, meta)
}

tb_render_dsilac_ratio_box <- function(results, style, meta) {
  # Reshape results so tb_render_vert_dis can consume it, then rename plot/table keys.
  vert_results <- list(
    engine_id = "vert_dis",
    params = results$params,
    data = list(
      values = results$data$values,
      summary = results$data$summary,
      group_colors = results$data$group_colors,
      applied_transform = results$data$applied_transform %||% "none"
    )
  )
  out <- tb_render_vert_dis(vert_results, style, meta)
  plots <- list()
  tables <- list()
  if (length(out$plots) > 0) {
    plots$dsilac_ratio_box_plot <- out$plots[[1]]
  }
  if (length(out$tables) > 0) {
    tables$dsilac_ratio_box_summary <- out$tables[[1]]
  } else if (!is.null(results$data$summary)) {
    tables$dsilac_ratio_box_summary <- results$data$summary
  }
  list(plots = plots, tables = tables)
}

tb_render_dsilac_ratio_box_peptide <- function(results, style, meta) {
  tb_render_dsilac_ratio_box(results, style, meta)
}

tb_render_dsilac_ratio_box_protein <- function(results, style, meta) {
  tb_render_dsilac_ratio_box(results, style, meta)
}

# ---- Peptide Aggregate to Protein renderer ------------------------------------

tb_render_peptide_aggregate_to_protein <- function(results, style, meta) {
  # Simple renderer that shows the aggregation_log table with rows before/after

  data <- results$data
  if (is.null(data)) {
    return(list(
      plots = list(),
      tables = list(aggregation_log = data.frame(
        Operation = "Error",
        Method = "",
        `Rows Before` = NA_integer_,
        `Rows After` = NA_integer_,
        Proteins = NA_integer_,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    ))
  }

  # Return the aggregation_log table from the engine output
  aggregation_log <- data$aggregation_log
  if (is.null(aggregation_log) || !is.data.frame(aggregation_log)) {
    # Fallback: try to construct from summary data
    summary_df <- data$summary
    mat <- data$mat

    rows_after <- if (!is.null(mat)) nrow(mat) else NA_integer_
    proteins <- if (!is.null(summary_df)) nrow(summary_df) else rows_after

    aggregation_log <- data.frame(
      Operation = "Aggregate Peptides \u2192 Proteins",
      Method = "median",
      `Rows Before` = NA_integer_,
      `Rows After` = rows_after,
      Proteins = proteins,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  list(
    plots = list(),
    tables = list(aggregation_log = aggregation_log)
  )
}

# ---- Rank Plot --------------------------------------------------------------

#' Render rank plot (value vs rank scatter)
#'
#' @param results Engine results from stats_rankplot_run
#' @param style Viewer-time style overrides
#' @param meta Node metadata
#' @return list(plots, tables, sets)
tb_render_rankplot <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  # Extract data
  df <- results$data$points
  available_groups <- results$data$groups %||% character(0)

  # Handle empty data

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    error_msg <- results$data$error %||% "No data available for rank plot"
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = error_msg, size = 5, color = "gray50") +
      ggplot2::theme_void()
    return(list(
      plots = list(rankplot = p),
      tables = list(rankplot_data = data.frame()),
      sets = list()
    ))
  }

  # Style settings
  y_axis_title <- style$y_axis_title %||% "Intensity"
  highlight_mode <- style$highlight_mode %||% "none"
  point_color <- style$point_color %||% "#B0B0B0"
  highlight_color_top <- style$highlight_color_top %||% "#FF4242"
  highlight_color_bottom <- style$highlight_color_bottom %||% "#4245FF"
  point_size <- tb_num(style$point_size, 2)
  point_alpha <- tb_num(style$point_alpha, 0.7)
  axis_text_size <- tb_num(style$axis_text_size, 20)
  axis_style <- style$axis_style %||% "clean"

  # Rank axis mode: absolute rank or percent rank

  rank_axis_mode <- style$rank_axis_mode %||% "rank"
  if (!rank_axis_mode %in% c("rank", "percent_rank")) rank_axis_mode <- "rank"

  if (identical(rank_axis_mode, "percent_rank")) {
    # Convert rank to percent rank per group
    df <- do.call(rbind, lapply(split(df, df$group), function(gdf) {
      max_rank <- max(gdf$rank, na.rm = TRUE)
      if (is.finite(max_rank) && max_rank > 0) {
        gdf$rank <- gdf$rank / max_rank * 100
      }
      gdf
    }))
    rownames(df) <- NULL
  }

  # Compute global axis limits across ALL groups for consistency
  global_max_rank <- max(df$rank, na.rm = TRUE)
  global_max_value <- max(df$value, na.rm = TRUE)
  if (!is.finite(global_max_rank)) global_max_rank <- 1
  if (!is.finite(global_max_value)) global_max_value <- 1

  # Filter by selected group (viewer_schema)
  selected_group <- style$selected_group %||% ""
  if (nzchar(selected_group) && selected_group %in% df$group) {
    df <- df[df$group == selected_group, , drop = FALSE]
  } else if (length(available_groups) > 0) {
    # Default to first group if none selected
    df <- df[df$group == available_groups[1], , drop = FALSE]
  }

  # Filter by minimum replicates (style_schema)
  min_reps <- tb_num(style$min_replicates, 1)
  if (min_reps > 1 && "n_reps" %in% names(df)) {
    df <- df[df$n_reps >= min_reps, , drop = FALSE]
    # Re-compute ranks after filtering
    if (nrow(df) > 0) {
      df$rank <- rank(df$value, ties.method = "average", na.last = "keep")
      if (identical(rank_axis_mode, "percent_rank")) {
        max_rank <- max(df$rank, na.rm = TRUE)
        if (is.finite(max_rank) && max_rank > 0) {
          df$rank <- df$rank / max_rank * 100
        }
      }
    }
  }

  # Initialize highlighting
  df$highlight <- "none"
  highlighted_top <- character(0)
  highlighted_bottom <- character(0)

  if (highlight_mode == "threshold") {
    above <- suppressWarnings(as.numeric(style$threshold_highlight_above))
    below <- suppressWarnings(as.numeric(style$threshold_highlight_below))

    if (!is.na(above) && is.finite(above)) {
      mask <- df$value > above
      df$highlight[mask] <- "top"
      highlighted_top <- df$protein_id[mask]
    }
    if (!is.na(below) && is.finite(below)) {
      mask <- df$value < below
      df$highlight[mask] <- "bottom"
      highlighted_bottom <- df$protein_id[mask]
    }
  } else if (highlight_mode == "topn") {
    top_n <- suppressWarnings(as.integer(style$topn_top %||% 0))
    bottom_n <- suppressWarnings(as.integer(style$topn_bottom %||% 0))

    if (!is.na(top_n) && top_n > 0) {
      # Top N = lowest rank numbers (rank 1, 2, 3... = highest values)
      mask <- df$rank <= top_n
      df$highlight[mask] <- "top"
      highlighted_top <- df$protein_id[mask]
    }
    if (!is.na(bottom_n) && bottom_n > 0) {
      # Bottom N = highest rank numbers (lowest values)
      max_rank <- max(df$rank, na.rm = TRUE)
      mask <- df$rank > (max_rank - bottom_n)
      df$highlight[mask] <- "bottom"
      highlighted_bottom <- df$protein_id[mask]
    }
  }

  # Build color mapping
  color_map <- c(
    "none" = point_color,
    "top" = highlight_color_top,
    "bottom" = highlight_color_bottom
  )

  # Order factor for consistent layering (highlighted points on top)
  df$highlight <- factor(df$highlight, levels = c("none", "bottom", "top"))
  df <- df[order(df$highlight), , drop = FALSE]

  # Compute plot limits using GLOBAL max values (consistent across all groups)
  # 5% padding on right, y starts at 0
  xlim <- c(0, global_max_rank * 1.05)
  y_pad <- global_max_value * 0.05
  ylim <- c(0, global_max_value + y_pad)

  # Create the plot with explicit axis limits
  # clip = "on" to prevent points spilling past axis boundaries
  p <- ggplot2::ggplot(df, ggplot2::aes(x = rank, y = value, color = highlight)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(values = color_map, guide = "none") +
    ggplot2::scale_x_continuous(labels = if (identical(rank_axis_mode, "percent_rank")) waiver() else format_k_suffix, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, clip = "on") +
    ggplot2::labs(x = if (identical(rank_axis_mode, "percent_rank")) "% Rank" else "Rank", y = y_axis_title) +
    tb_theme_base(axis_text_size, axis_style = axis_style)

  # ---- Highlight Groups Rendering (ggplot2) ----
  # Parse highlight groups from style and overlay colored points
  highlight_map_json <- style$highlight_groups_map %||% "{}"
  highlight_map <- tryCatch(jsonlite::fromJSON(highlight_map_json, simplifyVector = FALSE), error = function(e) list())
  highlight_groups <- highlight_map[["rankplot"]] %||% list()

  if (length(highlight_groups) > 0) {
    # Build gene column if not present
    if (is.null(df$gene)) {
      df$gene <- df$gene_id %||% df$protein_id %||% ""
    }
    df$gene <- as.character(df$gene)

    # Build gene -> group mapping (last group wins)
    gene_to_group <- list()
    for (g in highlight_groups) {
      genes <- trimws(unlist(strsplit(as.character(g$genes %||% ""), "\n", fixed = TRUE)))
      genes <- genes[nzchar(genes)]
      for (gene in genes) {
        gene_to_group[[gene]] <- g
      }
    }

    # Reverse mapping: group_id -> genes
    group_to_genes <- list()
    for (gene in names(gene_to_group)) {
      gid <- gene_to_group[[gene]]$id
      group_to_genes[[gid]] <- c(group_to_genes[[gid]], gene)
    }

    # Track which groups have data for legend
    groups_with_data <- character()

    # Add layer for each group
    for (g in highlight_groups) {
      gid <- g$id
      genes_in_group <- group_to_genes[[gid]]
      if (length(genes_in_group) == 0) next

      df_hl <- df[df$gene %in% genes_in_group, , drop = FALSE]
      if (nrow(df_hl) == 0) next

      group_name <- g$name %||% gid
      fill_color <- g$color %||% "#FF6600"
      has_outline <- isTRUE(g$outline)
      outline_width <- as.numeric(g$outline_width %||% 1)

      df_hl$highlight_group <- factor(group_name, levels = group_name)
      groups_with_data <- c(groups_with_data, group_name)

      if (has_outline) {
        p <- p + ggplot2::geom_point(
          data = df_hl,
          ggplot2::aes(x = rank, y = value, fill = highlight_group),
          shape = 21,
          size = point_size + 0.5,
          color = "black",
          stroke = outline_width,
          inherit.aes = FALSE
        )
      } else {
        p <- p + ggplot2::geom_point(
          data = df_hl,
          ggplot2::aes(x = rank, y = value, fill = highlight_group),
          shape = 21,
          size = point_size + 0.5,
          color = fill_color,
          stroke = 0.5,
          inherit.aes = FALSE
        )
      }
    }

    # Add fill scale for legend
    if (length(groups_with_data) > 0) {
      fill_values <- stats::setNames(
        sapply(highlight_groups, function(g) g$color %||% "#FF6600"),
        sapply(highlight_groups, function(g) g$name %||% g$id)
      )
      fill_values <- fill_values[names(fill_values) %in% groups_with_data]

      p <- p + ggplot2::scale_fill_manual(
        values = fill_values,
        name = "Highlight",
        guide = ggplot2::guide_legend(order = 2)
      )
    }
  }

  # Add gene labels from label_genes_map (for export/preview mode)
  # Use saved plotly positions for label placement (same pattern as volcano)
  label_font_size <- tb_num(style$label_font_size, 12)
  label_map_json <- style$label_genes_map %||% "{}"
  label_map <- tryCatch(jsonlite::fromJSON(label_map_json, simplifyVector = FALSE), error = function(e) list())
  label_genes <- label_map[["rankplot"]] %||% ""
  label_genes <- trimws(unlist(strsplit(as.character(label_genes), "\n", fixed = TRUE)))
  label_genes <- label_genes[nzchar(label_genes)]

  # Get saved plotly label positions (reflected in ggplot export)
  # Use per-group composite key for position lookup, with fallback to legacy "rankplot"
  effective_group <- if (nzchar(selected_group) && selected_group %in% available_groups) {
    selected_group
  } else if (length(available_groups) > 0) {
    available_groups[1]
  } else {
    ""
  }
  position_key <- if (nzchar(effective_group)) paste0("rankplot::", effective_group) else "rankplot"
  saved <- meta$plotly$labels_by_plot[[position_key]] %||%
    meta$plotly$labels_by_plot[["rankplot"]] %||%
    meta$plotly$labels_by_plot$default %||%
    meta$plotly$labels %||% list()

  if (length(label_genes) > 0) {
    message("[DEBUG-RENDER] tb_render_rankplot: rendering ", length(label_genes), " label(s), saved positions: ", length(saved))
    # Build gene column if not present
    if (is.null(df$gene)) {
      df$gene <- df$gene_id %||% df$protein_id %||% ""
    }
    df$gene <- as.character(df$gene)

    df_labels <- df[df$gene %in% label_genes, , drop = FALSE]
    if (nrow(df_labels) > 0) {
      # Compute label positions from saved state (same pattern as volcano)
      df_labels$lx <- NA_real_
      df_labels$ly <- NA_real_

      for (i in seq_len(nrow(df_labels))) {
        id <- as.character(df_labels$gene[[i]])
        s <- saved[[id]] %||% list()

        pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)

        lx <- pos$x
        ly <- pos$y

        if (!is.finite(lx)) lx <- df_labels$rank[[i]]
        if (!is.finite(ly)) ly <- df_labels$value[[i]]

        # Default offset if no saved position
        if (is.null(s$x_range) && is.null(s$y_range)) {
          if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) ||
              !is.finite(suppressWarnings(as.numeric(s$y)))) {
            x_offset <- 0.07 * diff(xlim)
            y_offset <- 0.07 * diff(ylim)
            lx <- df_labels$rank[[i]] + x_offset
            ly <- df_labels$value[[i]] + y_offset
          }
        }

        # Clamp to visible plot window
        lx <- max(xlim[[1]], min(xlim[[2]], lx))
        ly <- max(ylim[[1]], min(ylim[[2]], ly))

        df_labels$lx[[i]] <- lx
        df_labels$ly[[i]] <- ly
      }
      message("[DEBUG-RENDER] tb_render_rankplot: adding geom_segment + geom_text for ", nrow(df_labels), " labels")

      # Draw connector lines and labels (same pattern as volcano)
      label_size <- label_font_size / 3  # Convert points to ggplot mm scale
      p <- p +
        ggplot2::geom_segment(
          data = df_labels,
          ggplot2::aes(x = rank, y = value, xend = lx, yend = ly),
          linewidth = 0.3, color = "black",
          inherit.aes = FALSE
        ) +
        ggplot2::geom_text(
          data = df_labels,
          ggplot2::aes(x = lx, y = ly, label = gene),
          size = label_size, color = "black", hjust = 0.5, vjust = 0,
          inherit.aes = FALSE
        )
    }
  }

  # Build frozen sets for GO-ORA integration
  sets <- list()
  if (length(highlighted_top) > 0) {
    sets$highlighted_top <- unique(highlighted_top)
  }
  if (length(highlighted_bottom) > 0) {
    sets$highlighted_bottom <- unique(highlighted_bottom)
  }

  list(
    plots = list(rankplot = p),
    tables = list(),
    sets = sets
  )
}

# ============================================================
# Plotly-based interactive rank plot for label editing
# - Hover: shows gene name, rank, value
# - Click on point: triggers uniprot search (handled by Shiny)
# - Drag annotations: repositions labels (captured via relayout events)
# ============================================================
tb_rankplot_plotly <- function(df, style, meta, xlim, ylim, labs, saved, y_axis_title = "Intensity") {
  tb_require_pkg("plotly")

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Normalize gene column
  if (is.null(df$gene)) {
    df$gene <- df$gene_id %||% df$protein_id %||% ""
  }
  df$gene <- as.character(df$gene)

  # Colors from style
  point_color <- style$point_color %||% "#B0B0B0"
  highlight_color_top <- style$highlight_color_top %||% "#FF4242"
  highlight_color_bottom <- style$highlight_color_bottom %||% "#4245FF"
  point_size <- tb_num(style$point_size, 2)

  # Map highlight to colors
  color_map <- c(
    "none" = point_color,
    "top" = highlight_color_top,
    "bottom" = highlight_color_bottom
  )
  df$color <- color_map[as.character(df$highlight)]
  df$color[is.na(df$color)] <- point_color

  # Build annotations with arrows for labels
  annotations <- list()
  label_size <- suppressWarnings(as.numeric(style$label_font_size %||% 12))
  if (!is.finite(label_size) || label_size <= 0) label_size <- 12

  if (length(labs) > 0) {
    df_lab <- df[df$gene %in% labs, , drop = FALSE]

    if (nrow(df_lab) > 0) {
      for (i in seq_len(nrow(df_lab))) {
        id <- as.character(df_lab$gene[[i]])
        s <- saved[[id]] %||% list()

        # Data point coordinates
        data_x <- df_lab$rank[[i]]
        data_y <- df_lab$value[[i]]

        # Get saved position or compute default for label placement
        lx <- suppressWarnings(as.numeric(s$x %||% NA_real_))
        ly <- suppressWarnings(as.numeric(s$y %||% NA_real_))

        # Apply range-normalized positions if available
        if (!is.null(s$x_range) && !is.null(s$y_range)) {
          pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)
          lx <- pos$x
          ly <- pos$y
        }

        if (!is.finite(lx)) lx <- data_x
        if (!is.finite(ly)) ly <- data_y

        # Default offset if no saved position - offset away from data point
        if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) ||
            !is.finite(suppressWarnings(as.numeric(s$y)))) {
          if (is.null(s$x_range) && is.null(s$y_range)) {
            # Use 7% of range for offset (similar to volcano)
            x_offset <- 0.07 * diff(xlim)
            y_offset <- 0.07 * diff(ylim)
            lx <- data_x + x_offset
            ly <- data_y + y_offset
          }
        }

        # Clamp to visible plot window
        lx <- max(xlim[[1]], min(xlim[[2]], lx))
        ly <- max(ylim[[1]], min(ylim[[2]], ly))

        # Label annotation with arrow pointing to data point
        # x, y = arrowhead position (data point)
        # ax, ay = text position (with axref/ayref = "x"/"y" for data coordinates)
        # The arrow draws from text (ax, ay) to data point (x, y)
        annotations <- c(annotations, list(list(
          x = data_x,
          y = data_y,
          ax = lx,
          ay = ly,
          xref = "x",
          yref = "y",
          axref = "x",
          ayref = "y",
          text = id,
          showarrow = TRUE,
          arrowhead = 0,
          arrowwidth = 1,
          arrowcolor = "#333333",
          font = list(size = label_size, color = "#000000"),
          xanchor = "center",
          yanchor = "bottom",
          captureevents = TRUE,
          name = id
        )))
      }
    }
  }

  # Create plotly
  p <- plotly::plot_ly(
    data = df,
    x = ~rank,
    y = ~value,
    customdata = ~gene,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = ~color,
      size = point_size * 2.5,
      opacity = 0.9
    ),
    text = ~paste0(gene, "\nRank: ", rank, "\n", y_axis_title, ": ", round(value, 3)),
    hoverinfo = "text",
    source = "rankplot_plotly"
  )

  # ---- Highlight Groups Rendering (Plotly) ----
  highlight_map_json <- style$highlight_groups_map %||% "{}"
  highlight_map <- tryCatch(jsonlite::fromJSON(highlight_map_json, simplifyVector = FALSE), error = function(e) list())
  highlight_groups <- highlight_map[["rankplot"]] %||% list()

  if (length(highlight_groups) > 0) {
    # Build gene -> group mapping (last group wins)
    gene_to_group <- list()
    for (g in highlight_groups) {
      genes <- trimws(unlist(strsplit(as.character(g$genes %||% ""), "\n", fixed = TRUE)))
      genes <- genes[nzchar(genes)]
      for (gene in genes) {
        gene_to_group[[gene]] <- g
      }
    }

    # Reverse mapping: group_id -> genes
    group_to_genes <- list()
    for (gene in names(gene_to_group)) {
      gid <- gene_to_group[[gene]]$id
      group_to_genes[[gid]] <- c(group_to_genes[[gid]], gene)
    }

    # Add traces for each highlight group
    for (g in highlight_groups) {
      gid <- g$id
      genes_in_group <- group_to_genes[[gid]]
      if (length(genes_in_group) == 0) next

      df_hl <- df[df$gene %in% genes_in_group, , drop = FALSE]
      if (nrow(df_hl) == 0) next

      group_name <- g$name %||% gid
      fill_color <- g$color %||% "#FF6600"
      has_outline <- isTRUE(g$outline)
      outline_width <- as.numeric(g$outline_width %||% 1)

      marker_config <- if (has_outline) {
        list(
          color = fill_color,
          size = point_size * 2.5 + 2,
          opacity = 1,
          line = list(color = "black", width = outline_width)
        )
      } else {
        list(
          color = fill_color,
          size = point_size * 2.5 + 2,
          opacity = 1,
          line = list(color = fill_color, width = 0.5)
        )
      }

      p <- p %>% plotly::add_trace(
        data = df_hl,
        x = ~rank,
        y = ~value,
        customdata = ~gene,
        type = "scatter",
        mode = "markers",
        marker = marker_config,
        text = ~paste0(gene, "\nRank: ", rank, "\n", y_axis_title, ": ", round(value, 3)),
        hoverinfo = "text",
        name = group_name,
        legendgroup = "highlight",
        showlegend = TRUE
      )
    }
  }

  # Show legend only if there are highlight groups
  has_hl_groups <- length(highlight_groups) > 0

  p <- p %>%
    plotly::layout(
      xaxis = list(
        title = "Rank",
        range = xlim,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        automargin = TRUE
      ),
      yaxis = list(
        title = y_axis_title,
        range = ylim,
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        automargin = TRUE
      ),
      annotations = annotations,
      showlegend = has_hl_groups,
      dragmode = "pan",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 60, r = 40, t = 20, b = 50)
    ) %>%
    plotly::config(
      edits = list(
        annotationPosition = TRUE,
        annotationTail = TRUE
      ),
      displayModeBar = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
    )

  p <- plotly::event_register(p, "plotly_click")
  p <- plotly::event_register(p, "plotly_relayout")

  p
}

# ---- Heatmap ----------------------------------------------------------------

# NOTE: We calculate extra space needed for custom gtable additions (group bars,
# cluster bars) inline before calling pheatmap, and reduce pheatmap's width/height
# accordingly. This ensures the final gtable size matches the user's target dimensions.

tb_render_heatmap <- function(results, style, context = NULL) {
  tb_require_pkg("ggplot2")
  tb_require_pkg("pheatmap")
  tb_require_pkg("ggplotify")

  params <- list()
  data <- results
  if (is.list(results) && !is.null(results$data) && is.list(results$data)) {
    params <- results$params %||% list()
    data <- results$data
  }

  color_mode <- as.character(style$color_mode %||% "zscore")[1]
  if (!color_mode %in% c("zscore", "abundance")) color_mode <- "zscore"

  mat <- if (identical(color_mode, "abundance")) data$mat_log else data$mat_zscore
  dendro <- if (identical(color_mode, "abundance")) data$dendro_abundance else data$dendro_zscore

  # Early return if no valid matrix (before trying to filter NAs)
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = "No heatmap data available",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    out <- list(
      plots = list(heatmap = p_empty),
      figures = list(heatmap = p_empty),
      tables = list(heatmap_data = data.frame())
    )
    return(out)
  }

  exclude_na_rows <- isTRUE(style$exclude_na_rows %||% params$exclude_na_rows %||% FALSE)
  na_row_mask <- NULL  # Track which rows are kept for cluster mapping
  if (exclude_na_rows) {
    keep <- stats::complete.cases(mat)
    na_row_mask <- keep  # Save for cluster color mapping
    mat <- mat[keep, , drop = FALSE]
    # Don't null the dendro - we'll use it for cluster colors with the mask
  }

  # Check again after NA filtering in case all rows were removed
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = "All rows filtered out (all contain NAs)",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    out <- list(
      plots = list(heatmap = p_empty),
      figures = list(heatmap = p_empty),
      tables = list(heatmap_data = data.frame())
    )
    return(out)
  }

  cluster_rows_requested <- isTRUE(params$cluster_rows %||% TRUE)
  cluster_rows_arg <- FALSE
  cluster_method <- style$cluster_method %||% "hierarchical"
  kmeans_clusters <- NULL  # Store k-means/k-medians cluster assignments
  kmeans_row_order <- NULL  # Row order for k-means/k-medians

  if (cluster_rows_requested && nrow(mat) >= 2) {
    if (cluster_method %in% c("kmeans", "kmedians")) {
      # K-means or K-medians clustering (computed at render time)
      k <- as.integer(style$cluster_k %||% 2)
      k <- min(k, nrow(mat))
      if (k < 2) k <- 2

      # Compute clustering on the current matrix
      m_complete <- mat[stats::complete.cases(mat), , drop = FALSE]
      if (nrow(m_complete) >= k) {
        if (cluster_method == "kmeans") {
          set.seed(42)
          km <- stats::kmeans(m_complete, centers = k, nstart = 25)
          cluster_assignments <- km$cluster
        } else {
          # K-medians using cluster::pam
          if (requireNamespace("cluster", quietly = TRUE)) {
            set.seed(42)
            pam_result <- cluster::pam(m_complete, k = k)
            cluster_assignments <- pam_result$clustering
          } else {
            # Fallback to k-means
            set.seed(42)
            km <- stats::kmeans(m_complete, centers = k, nstart = 25)
            cluster_assignments <- km$cluster
          }
        }
        names(cluster_assignments) <- rownames(m_complete)

        # Map back to original matrix rows (NA for rows that were excluded)
        kmeans_clusters <- rep(NA_integer_, nrow(mat))
        names(kmeans_clusters) <- rownames(mat)
        kmeans_clusters[names(cluster_assignments)] <- cluster_assignments

        # Reorder matrix rows by cluster assignment
        valid_rows <- !is.na(kmeans_clusters)
        row_order <- order(kmeans_clusters[valid_rows])
        valid_row_names <- rownames(mat)[valid_rows][row_order]
        na_row_names <- rownames(mat)[!valid_rows]

        # Combine: clustered rows first, then rows with NAs
        new_order <- c(valid_row_names, na_row_names)
        mat <- mat[new_order, , drop = FALSE]
        kmeans_clusters <- kmeans_clusters[new_order]
        kmeans_row_order <- new_order
      }
      cluster_rows_arg <- FALSE  # No hierarchical clustering for pheatmap
    } else {
      # Hierarchical clustering - computed at render time with configurable options
      distance_method <- style$distance_method %||% "correlation"
      corr_type <- style$correlation_type %||% "spearman"
      linkage <- style$linkage_method %||% "average"

      # Compute dendrogram from scratch based on style parameters
      clust_rows <- if (distance_method == "correlation") {
        which(rowSums(!is.na(mat)) >= 2)
      } else {
        which(rowSums(is.na(mat)) == 0)
      }

      if (length(clust_rows) >= 2) {
        m_sub <- mat[clust_rows, , drop = FALSE]

        # Compute distance matrix
        if (distance_method == "correlation") {
          cmat <- suppressWarnings(stats::cor(t(m_sub), use = "pairwise.complete.obs", method = corr_type))
          cmat[is.na(cmat)] <- 0
          diag(cmat) <- 1
          d <- stats::as.dist(1 - cmat)
        } else if (distance_method == "euclidean") {
          d <- stats::dist(m_sub, method = "euclidean")
        } else {
          d <- stats::dist(m_sub, method = "manhattan")
        }

        # Compute hierarchical clustering
        hc <- stats::hclust(d, method = linkage)
        if (is.null(hc$labels)) hc$labels <- rownames(m_sub)

        # Use computed dendrogram if it matches the matrix dimensions
        if (length(hc$order) == nrow(mat)) {
          cluster_rows_arg <- hc
          dendro <- hc  # Update dendro for cluster color bar computation
        } else {
          # Subset mismatch - let pheatmap handle clustering
          cluster_rows_arg <- if (any(!is.finite(mat))) FALSE else TRUE
        }
      } else {
        # Not enough rows to cluster - let pheatmap handle it
        cluster_rows_arg <- if (any(!is.finite(mat))) FALSE else TRUE
      }
    }
  }

  # Transpose option: genes become columns, samples become rows
  transpose <- isTRUE(style$transpose %||% FALSE)

  # Dendrogram visibility (independent of clustering, but hidden for k-means/k-medians)
  show_dendrogram <- isTRUE(style$show_dendrogram %||% TRUE)
  if (cluster_method %in% c("kmeans", "kmedians")) {
    show_dendrogram <- FALSE  # No dendrogram for k-means/k-medians
  }

  show_row_labels <- isTRUE(style$show_row_labels %||% TRUE)
  row_font_size <- suppressWarnings(as.numeric(style$row_font_size %||% 8))
  if (!is.finite(row_font_size) || row_font_size <= 0) row_font_size <- 8

  # Cluster color bar visibility (extracted early for margin calculation)
  show_cluster_colors <- isTRUE(style$show_cluster_colors %||% FALSE)

  na_color <- as.character(style$na_color %||% "grey50")[1]
  width <- suppressWarnings(as.numeric(style$width %||% 10))
  height <- suppressWarnings(as.numeric(style$height %||% 8))
  if (!is.finite(width) || width <= 0) width <- 10
  if (!is.finite(height) || height <= 0) height <- 8

  palette_name <- as.character(style$color_palette %||% "viridis")[1]
  get_palette <- function(name, n = 100) {
    name <- as.character(name %||% "viridis")[1]
    if (identical(name, "viridis")) {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        return(viridisLite::viridis(n))
      }
      return(grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(n))
    }
    # Convention: blue = low, red = high for signed diverging palettes
    if (identical(name, "RdBu")) {
      return(grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n))
    }
    if (identical(name, "RdYlBu")) {
      return(grDevices::colorRampPalette(c("#4575B4", "#FFFFBF", "#D73027"))(n))
    }
    if (identical(name, "Blues")) {
      return(grDevices::colorRampPalette(c("#EFF3FF", "#2171B5"))(n))
    }
    if (identical(name, "Reds")) {
      return(grDevices::colorRampPalette(c("#FEE0D2", "#A50F15"))(n))
    }
    if (identical(name, "PuOr")) {
      return(grDevices::colorRampPalette(c("#542788", "#F7F7F7", "#B35806"))(n))
    }
    grDevices::colorRampPalette(c("#2c7bb6", "#ffffbf", "#d7191c"))(n)
  }

  group_annotations <- data$group_annotations
  group_colors <- data$group_colors
  annotation_col <- NULL
  annotation_colors <- NULL
  col_labels <- colnames(mat)
  if (is.data.frame(group_annotations) && nrow(group_annotations) > 0 &&
      all(c("sample", "group") %in% names(group_annotations))) {
    ann <- data.frame(
      group = as.character(group_annotations$group),
      stringsAsFactors = FALSE,
      row.names = as.character(group_annotations$sample)
    )
    ann <- ann[colnames(mat), , drop = FALSE]

    unknown_idx <- is.na(ann$group) | !nzchar(ann$group)
    if (any(unknown_idx)) {
      ann$group[unknown_idx] <- "Unknown"
    }

    # Use group names only for display (no replicate numbers)
    col_labels <- as.character(ann$group)

    # Keep unique internal IDs for annotation_col row names (required by data.frame)
    # These match the original column names of the matrix
    annotation_col_rownames <- colnames(mat)

    annotation_col <- ann

    if (!is.null(group_colors) && is.atomic(group_colors)) {
      # Preserve names when converting to character (as.character strips names)
      group_colors <- stats::setNames(as.character(group_colors), names(group_colors))
      used_levels <- unique(ann$group)

      if (is.null(names(group_colors)) || anyNA(names(group_colors)) || any(!nzchar(names(group_colors)))) {
        group_colors <- character(0)
      }

      colors <- group_colors[used_levels]
      missing_groups <- names(colors)[is.na(colors) | !nzchar(colors)]
      if (length(missing_groups) > 0) {
        new_cols <- grDevices::hcl.colors(length(missing_groups), palette = "Dark 3")
        names(new_cols) <- missing_groups
        colors[missing_groups] <- new_cols[missing_groups]
      }
      if ("Unknown" %in% names(colors) && (is.na(colors[["Unknown"]]) || !nzchar(colors[["Unknown"]]))) {
        colors[["Unknown"]] <- "grey70"
      }

      ann$group <- factor(ann$group, levels = names(colors))
      annotation_col <- ann
      annotation_colors <- list(group = colors)
    }
  }

  # Handle transpose: genes become columns, samples become rows
  # Store original values for annotation bar logic
  annotation_row <- NULL
  original_mat <- mat
  original_cluster_rows_arg <- cluster_rows_arg
  original_col_names <- colnames(mat)  # Keep original sample names for annotation matching

  if (transpose) {
    # For transposed mode: keep unique sample IDs as row names (after transpose)
    # We'll display group-only labels via gtable manipulation for the row labels
    mat <- t(mat)
    # When transposed:
    # - Original rows (genes) become columns -> cluster columns using original row dendrogram
    # - Original columns (samples) become rows -> don't cluster these rows
    # - Group annotation (was for columns) now applies to rows
    if (!is.null(annotation_col)) {
      annotation_row <- annotation_col
      annotation_col <- NULL
    }
  } else {
    # For non-transposed mode: use group-only labels as column names (duplicates OK for colnames)
    if (!is.null(col_labels) && length(col_labels) == ncol(mat)) {
      colnames(mat) <- col_labels
    }
  }

  # Determine cluster settings for dimension calculation
  cluster_k_val <- suppressWarnings(as.integer(style$cluster_k %||% 0))
  will_show_clusters <- isTRUE(style$show_cluster_colors %||% FALSE) &&
                        !is.na(cluster_k_val) && cluster_k_val >= 2

  # Pre-calculate extra space needed for custom annotations (added via gtable)
  extra_width_inches <- 0
  extra_height_inches <- 0

  # Group color bar: 8pt height (bottom in normal mode, left in transposed)
  group_bar_size <- 8 / 72  # 8pt in inches
  has_group_annotation <- !is.null(annotation_col) && !is.null(annotation_colors)
  has_group_annotation_transposed <- !is.null(annotation_row) && !is.null(annotation_colors)

  # Cluster color bar: 12pt width/height with numbers
  cluster_bar_size <- 12 / 72  # 12pt in inches

  if (transpose) {
    # Transposed: group bar on LEFT (width), cluster bar at TOP (height)
    if (has_group_annotation_transposed) {
      # Group bar + labels on left
      group_col <- annotation_row[[1]]
      unique_groups <- unique(as.character(group_col))
      max_label_chars <- max(nchar(unique_groups), na.rm = TRUE)
      if (!is.finite(max_label_chars)) max_label_chars <- 5
      group_label_width <- max_label_chars * 6 / 72 + 0.1  # ~6pt per char + padding
      extra_width_inches <- extra_width_inches + group_bar_size + group_label_width
    }
    if (will_show_clusters) {
      extra_height_inches <- extra_height_inches + cluster_bar_size
    }
  } else {
    # Normal: group bar at BOTTOM (height), cluster bar on LEFT (width)
    if (has_group_annotation) {
      extra_height_inches <- extra_height_inches + group_bar_size
    }
    if (will_show_clusters) {
      extra_width_inches <- extra_width_inches + cluster_bar_size
    }
  }

  # Reduce pheatmap dimensions to leave room for custom annotations
  # Note: Legend spacing is handled dynamically after gtable is built
  pheatmap_width <- width - extra_width_inches
  pheatmap_height <- height - extra_height_inches

  # Ensure minimum dimensions
  if (pheatmap_width < 2) pheatmap_width <- 2
  if (pheatmap_height < 2) pheatmap_height <- 2

  # Truncate long row labels (gene/metabolite names) for display
  max_label_chars <- 15L
  rn <- rownames(mat)
  if (!is.null(rn)) {
    long <- nchar(rn) > max_label_chars
    if (any(long)) {
      rn[long] <- paste0(substr(rn[long], 1, max_label_chars - 3L), "...")
      rownames(mat) <- rn
    }
  }

  # Generate pheatmap WITHOUT custom annotations (we add them via gtable)
  # When transposed: cluster_cols uses the gene dendrogram, cluster_rows = FALSE
  # When normal: cluster_rows uses the gene dendrogram, cluster_cols = FALSE
  hm <- pheatmap::pheatmap(
    mat,
    cluster_rows = if (transpose) FALSE else cluster_rows_arg,
    cluster_cols = if (transpose) cluster_rows_arg else FALSE,
    treeheight_row = if (!transpose && show_dendrogram) 50 else 0,
    treeheight_col = if (transpose && show_dendrogram) 50 else 0,
    annotation_col = NULL,  # We add custom annotations via gtable
    annotation_row = NULL,
    annotation_colors = NULL,
    annotation_legend = FALSE,
    legend = TRUE,
    na_col = na_color,
    scale = "none",
    show_rownames = if (transpose) TRUE else show_row_labels,
    show_colnames = if (transpose) show_row_labels else TRUE,
    fontsize_row = if (transpose) 8 else row_font_size,
    fontsize_col = if (transpose) row_font_size else 8,
    angle_col = if (transpose) 90 else 270,
    color = get_palette(palette_name, n = 100),
    border_color = "black",
    silent = TRUE,
    width = pheatmap_width,
    height = pheatmap_height
  )

  # Helper to get gtable element position
  get_gt_pos <- function(gt, name) {
    idx <- which(gt$layout$name == name)
    if (length(idx) == 0) return(NULL)
    list(idx = idx, t = gt$layout$t[idx], b = gt$layout$b[idx],
         l = gt$layout$l[idx], r = gt$layout$r[idx])
  }

  # === NORMAL MODE: Add group bar at BOTTOM, cluster bar on LEFT ===
  if (!transpose && !is.null(hm$gtable)) {
    tb_require_pkg("grid")
    tb_require_pkg("gtable")
    gt <- hm$gtable

    # Add group color bar at BOTTOM
    if (has_group_annotation) {
      tryCatch({
        n_cols <- ncol(mat)
        col_groups <- annotation_col$group[match(original_col_names, rownames(annotation_col))]
        col_colors <- annotation_colors$group[as.character(col_groups)]

        bar_height <- grid::unit(group_bar_size, "inches")
        rect_grobs <- lapply(seq_len(n_cols), function(i) {
          grid::rectGrob(
            x = grid::unit((i - 0.5) / n_cols, "npc"),
            y = grid::unit(0.5, "npc"),
            width = grid::unit(1 / n_cols, "npc"),
            height = grid::unit(1, "npc"),
            gp = grid::gpar(fill = col_colors[i], col = "black", lwd = 0.5),
            name = paste0("group_rect_", i)
          )
        })
        colorbar_grob <- grid::gTree(children = do.call(grid::gList, rect_grobs), name = "group_colorbar")

        matrix_pos <- get_gt_pos(gt, "matrix")
        if (!is.null(matrix_pos)) {
          # Insert row WITHIN existing space (replace a null row or use reserved space)
          gt <- gtable::gtable_add_rows(gt, heights = bar_height, pos = matrix_pos$b)
          gt <- gtable::gtable_add_grob(gt, colorbar_grob,
            t = matrix_pos$b + 1, l = matrix_pos$l, r = matrix_pos$r, name = "group_colorbar")
        }
      }, error = function(e) {
        # Silently skip group color bar on error
      })
    }

    # Add cluster color bar on LEFT with numbered labels
    # Support both hierarchical and k-means/k-medians cluster coloring
    has_kmeans_clusters <- !is.null(kmeans_clusters) && any(!is.na(kmeans_clusters))
    has_hierarchical <- !is.null(dendro) && inherits(dendro, "hclust")

    if (will_show_clusters && (has_kmeans_clusters || has_hierarchical)) {
      tryCatch({
        n_display_rows <- nrow(mat)

        if (has_kmeans_clusters) {
          # Use pre-computed k-means/k-medians clusters
          ordered_clusters <- kmeans_clusters
          # Filter NAs and adjust cluster_k_val to actual number of clusters
          actual_k <- max(kmeans_clusters, na.rm = TRUE)
          if (actual_k > 0) cluster_k_val <- actual_k
        } else {
          # Use hierarchical clustering
          all_clusters <- stats::cutree(dendro, k = cluster_k_val)
          row_order <- if (!is.null(hm$tree_row)) hm$tree_row$order else dendro$order
          ordered_clusters <- all_clusters[row_order]
        }

        # Handle NA filtering mismatch
        if (length(ordered_clusters) != n_display_rows) {
          ordered_clusters <- ordered_clusters[seq_len(n_display_rows)]
        }

        cluster_colors <- grDevices::hcl.colors(cluster_k_val, palette = "Set2")
        row_colors <- cluster_colors[ordered_clusters]
        row_colors[is.na(ordered_clusters)] <- "grey80"  # Color for unclustered rows
        bar_width <- grid::unit(cluster_bar_size, "inches")

        # Create rectangles
        rect_grobs <- lapply(seq_len(n_display_rows), function(i) {
          y_pos <- (n_display_rows - i + 0.5) / n_display_rows
          grid::rectGrob(x = grid::unit(0.5, "npc"), y = grid::unit(y_pos, "npc"),
            width = grid::unit(1, "npc"), height = grid::unit(1 / n_display_rows, "npc"),
            gp = grid::gpar(fill = row_colors[i], col = NA), name = paste0("cluster_rect_", i))
        })

        # Create numbered labels at cluster centers
        cluster_rle <- rle(ordered_clusters)
        label_grobs <- list()
        cumsum_lens <- c(0, cumsum(cluster_rle$lengths))
        for (j in seq_along(cluster_rle$values)) {
          center <- (cumsum_lens[j] + cumsum_lens[j + 1] + 1) / 2
          y_pos <- (n_display_rows - center + 0.5) / n_display_rows
          label_grobs[[j]] <- grid::textGrob(
            label = as.character(cluster_rle$values[j]),
            x = grid::unit(0.5, "npc"), y = grid::unit(y_pos, "npc"),
            gp = grid::gpar(col = "white", fontsize = 9, fontface = "bold"),
            name = paste0("cluster_label_", j))
        }

        cluster_grob <- grid::gTree(children = do.call(grid::gList, c(rect_grobs, label_grobs)),
          name = "cluster_colorbar")

        matrix_pos <- get_gt_pos(gt, "matrix")
        if (!is.null(matrix_pos)) {
          gt <- gtable::gtable_add_cols(gt, widths = bar_width, pos = matrix_pos$l - 1)
          # Re-find matrix after column addition
          matrix_pos <- get_gt_pos(gt, "matrix")
          gt <- gtable::gtable_add_grob(gt, cluster_grob,
            t = matrix_pos$t, b = matrix_pos$b, l = matrix_pos$l - 1, name = "cluster_colorbar")
        }
      }, error = function(e) {
        # Silently skip cluster color bar on error
      })
    }

    hm$gtable <- gt
  }

  # === TRANSPOSED MODE: Add group bar on LEFT, cluster bar at TOP ===
  if (transpose && !is.null(hm$gtable)) {
    tb_require_pkg("grid")
    tb_require_pkg("gtable")
    gt <- hm$gtable

    # Hide default row names (sample IDs) - we show group labels instead
    row_names_idx <- which(gt$layout$name == "row_names")
    if (length(row_names_idx) > 0) {
      gt$grobs[[row_names_idx]] <- grid::nullGrob()
    }

    # Add group color bar and labels on LEFT
    if (has_group_annotation_transposed) {
      tryCatch({
        n_rows <- nrow(mat)
        group_col <- annotation_row[[1]]
        grp_colors <- annotation_colors[[names(annotation_row)[1]]]
        unique_groups <- unique(as.character(group_col))

        bar_width <- grid::unit(group_bar_size, "inches")
        max_label_chars <- max(nchar(unique_groups), na.rm = TRUE)
        label_width <- grid::unit(max_label_chars * 6 + 10, "pt")

        # Color rectangles
        rect_grobs <- lapply(seq_len(n_rows), function(i) {
          y_pos <- (n_rows - i + 0.5) / n_rows
          grp <- as.character(group_col[i])
          fill_col <- grp_colors[grp]
          grid::rectGrob(x = grid::unit(0.5, "npc"), y = grid::unit(y_pos, "npc"),
            width = grid::unit(1, "npc"), height = grid::unit(1 / n_rows, "npc"),
            gp = grid::gpar(fill = fill_col, col = NA), name = paste0("group_rect_", i))
        })
        group_bar_grob <- grid::gTree(children = do.call(grid::gList, rect_grobs), name = "group_colorbar")

        # Group labels (one per group, centered)
        label_grobs <- lapply(unique_groups, function(grp) {
          grp_indices <- which(group_col == grp)
          center_idx <- mean(grp_indices)
          y_pos <- (n_rows - center_idx + 0.5) / n_rows
          grid::textGrob(label = grp, x = grid::unit(1, "npc") - grid::unit(4, "pt"),
            y = grid::unit(y_pos, "npc"), just = c("right", "center"),
            gp = grid::gpar(fontsize = 8), name = paste0("group_label_", grp))
        })
        group_labels_grob <- grid::gTree(children = do.call(grid::gList, label_grobs), name = "group_labels")

        matrix_pos <- get_gt_pos(gt, "matrix")
        if (!is.null(matrix_pos)) {
          # Add label column first (further left)
          gt <- gtable::gtable_add_cols(gt, widths = label_width, pos = matrix_pos$l - 1)
          matrix_pos <- get_gt_pos(gt, "matrix")
          gt <- gtable::gtable_add_grob(gt, group_labels_grob,
            t = matrix_pos$t, b = matrix_pos$b, l = matrix_pos$l - 1, name = "group_labels")

          # Add color bar column (between labels and matrix)
          gt <- gtable::gtable_add_cols(gt, widths = bar_width, pos = matrix_pos$l - 1)
          matrix_pos <- get_gt_pos(gt, "matrix")
          gt <- gtable::gtable_add_grob(gt, group_bar_grob,
            t = matrix_pos$t, b = matrix_pos$b, l = matrix_pos$l - 1, name = "group_colorbar")
        }
      }, error = function(e) {
        # Silently skip transposed group bar on error
      })
    }

    # Add cluster color bar at TOP with numbered labels
    if (will_show_clusters && !is.null(dendro) && inherits(dendro, "hclust")) {
      tryCatch({
        n_display_cols <- ncol(mat)
        all_clusters <- stats::cutree(dendro, k = cluster_k_val)
        col_order <- if (!is.null(hm$tree_col)) hm$tree_col$order else dendro$order
        ordered_clusters <- all_clusters[col_order]

        if (length(ordered_clusters) != n_display_cols) {
          ordered_clusters <- ordered_clusters[seq_len(n_display_cols)]
        }

        cluster_colors <- grDevices::hcl.colors(cluster_k_val, palette = "Set2")
        col_colors <- cluster_colors[ordered_clusters]
        bar_height <- grid::unit(cluster_bar_size, "inches")

        # Rectangles
        rect_grobs <- lapply(seq_len(n_display_cols), function(i) {
          x_pos <- (i - 0.5) / n_display_cols
          grid::rectGrob(x = grid::unit(x_pos, "npc"), y = grid::unit(0.5, "npc"),
            width = grid::unit(1 / n_display_cols, "npc"), height = grid::unit(1, "npc"),
            gp = grid::gpar(fill = col_colors[i], col = NA), name = paste0("cluster_rect_col_", i))
        })

        # Numbered labels
        cluster_rle <- rle(ordered_clusters)
        label_grobs <- list()
        cumsum_lens <- c(0, cumsum(cluster_rle$lengths))
        for (j in seq_along(cluster_rle$values)) {
          center <- (cumsum_lens[j] + cumsum_lens[j + 1] + 1) / 2
          x_pos <- (center - 0.5) / n_display_cols
          label_grobs[[j]] <- grid::textGrob(
            label = as.character(cluster_rle$values[j]),
            x = grid::unit(x_pos, "npc"), y = grid::unit(0.5, "npc"),
            gp = grid::gpar(col = "white", fontsize = 9, fontface = "bold"),
            name = paste0("cluster_label_col_", j))
        }

        cluster_grob <- grid::gTree(children = do.call(grid::gList, c(rect_grobs, label_grobs)),
          name = "cluster_colorbar_top")

        matrix_pos <- get_gt_pos(gt, "matrix")
        if (!is.null(matrix_pos)) {
          gt <- gtable::gtable_add_rows(gt, heights = bar_height, pos = matrix_pos$t - 1)
          matrix_pos <- get_gt_pos(gt, "matrix")
          gt <- gtable::gtable_add_grob(gt, cluster_grob,
            t = matrix_pos$t - 1, l = matrix_pos$l, r = matrix_pos$r, name = "cluster_colorbar_top")
        }
      }, error = function(e) {
        # Silently skip transposed cluster bar on error
      })
    }

    hm$gtable <- gt
  }

  force_text_black <- function(g) {
    if (is.null(g)) return(g)

    if (inherits(g, "text")) {
      if (is.null(g$gp)) {
        g$gp <- grid::gpar(col = "black")
      } else {
        g$gp$col <- "black"
      }
      return(g)
    }

    if (!is.null(g$children) && length(g$children) > 0) {
      g$children <- lapply(g$children, force_text_black)
    }
    if (!is.null(g$grobs) && length(g$grobs) > 0) {
      g$grobs <- lapply(g$grobs, force_text_black)
    }
    if (!is.null(g$grob)) {
      g$grob <- force_text_black(g$grob)
    }
    g
  }

  if (!is.null(hm$gtable) && !is.null(hm$gtable$grobs)) {
    hm$gtable$grobs <- lapply(hm$gtable$grobs, force_text_black)
  }

  # Check if pheatmap produced a valid gtable before converting
  if (is.null(hm) || is.null(hm$gtable) || !inherits(hm$gtable, "gtable")) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = "Heatmap rendering failed (pheatmap returned invalid output)",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    tbl <- data.frame(gene = rownames(mat), mat, check.names = FALSE, stringsAsFactors = FALSE)
    return(list(
      plots = list(heatmap = p_empty),
      figures = list(heatmap = p_empty),
      tables = list(heatmap_data = tbl),
      tabs = NULL
    ))
  }

  # === FIT GTABLE TO TARGET DIMENSIONS ===
  # After all gtable manipulations, ensure proper spacing and scale to fit
  gt <- hm$gtable

  # Helper to safely convert units to inches (needs graphics device context)
  safe_convert_inches <- function(unit_obj) {
    tryCatch({
      current_dev <- grDevices::dev.cur()
      if (current_dev == 1) {  # null device
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off(), add = TRUE)
      }
      grid::convertUnit(unit_obj, "inches", valueOnly = TRUE)
    }, error = function(e) {
      # Fallback estimation
      vals <- numeric(length(unit_obj))
      for (i in seq_along(unit_obj)) {
        u <- unit_obj[[i]]
        unit_type <- attr(u, "unit")
        val <- as.numeric(u)
        if (is.null(unit_type)) unit_type <- "null"
        vals[i] <- switch(unit_type,
          "inches" = val,
          "cm" = val / 2.54,
          "mm" = val / 25.4,
          "points" = val / 72,
          "pt" = val / 72,
          "npc" = 0.5,
          "null" = 0.5,
          0.5
        )
      }
      vals
    })
  }

  # Helper to calculate text width in inches based on string and font size
  calc_text_width_inches <- function(text_strings, fontsize_pt) {
    if (length(text_strings) == 0) return(0)
    max_chars <- max(nchar(as.character(text_strings)), na.rm = TRUE)
    if (!is.finite(max_chars)) max_chars <- 0
    # Approximate: average character width is ~0.6 * font height
    # Font height in inches = fontsize_pt / 72
    char_width <- (fontsize_pt / 72) * 0.6
    max_chars * char_width
  }

  # === ENSURE LEGEND DOESN'T OVERLAP WITH ROW NAMES (normal mode) ===
  # In normal mode, row_names (gene names) are on the right side of the matrix,
  # and the legend is further right. We need to ensure proper spacing.
  if (!transpose) {
    row_names_pos <- get_gt_pos(gt, "row_names")
    legend_pos <- get_gt_pos(gt, "legend")

    if (!is.null(row_names_pos) && !is.null(legend_pos)) {
      row_names_col <- row_names_pos$r
      legend_col <- legend_pos$l

      # Calculate required width for row names based on actual text
      labels <- rownames(mat)
      fontsize <- row_font_size
      row_names_width_in <- calc_text_width_inches(labels, fontsize) + 0.15  # text + small padding

      # Ensure the row_names column has enough width
      current_row_names_width <- safe_convert_inches(gt$widths[row_names_col])
      if (length(current_row_names_width) == 0 || !is.finite(current_row_names_width)) {
        current_row_names_width <- 0
      }

      if (row_names_width_in > current_row_names_width) {
        gt$widths[row_names_col] <- grid::unit(row_names_width_in, "inches")
      }

      # Add a gap column between row_names and legend for visual separation
      # Insert after row_names column (before legend)
      gap_size <- grid::unit(0.2, "inches")
      gt <- gtable::gtable_add_cols(gt, widths = gap_size, pos = row_names_col)
    }
  }

  # === ENSURE COL_NAMES (gene names in transposed mode) HAVE ENOUGH SPACE ===
  col_names_pos <- get_gt_pos(gt, "col_names")
  if (!is.null(col_names_pos) && transpose) {
    # In transposed mode, col_names are the gene names at the bottom
    labels <- rownames(original_mat)  # Original gene names
    fontsize <- row_font_size
    # Rotated text: width becomes height. Add generous padding for the full text
    col_names_height_in <- calc_text_width_inches(labels, fontsize) + 0.25

    col_names_row <- col_names_pos$b
    current_height <- safe_convert_inches(gt$heights[col_names_row])
    if (length(current_height) == 0) current_height <- 0

    if (col_names_height_in > current_height) {
      gt$heights[col_names_row] <- grid::unit(col_names_height_in, "inches")
    }
  }

  # === SCALE TO FIT TARGET DIMENSIONS ===
  current_width_in <- sum(safe_convert_inches(gt$widths))
  current_height_in <- sum(safe_convert_inches(gt$heights))

  scale_x <- if (current_width_in > width && current_width_in > 0) width / current_width_in else 1
  scale_y <- if (current_height_in > height && current_height_in > 0) height / current_height_in else 1
  scale_factor <- min(scale_x, scale_y)

  if (scale_factor < 1) {
    width_inches <- safe_convert_inches(gt$widths) * scale_factor
    height_inches <- safe_convert_inches(gt$heights) * scale_factor
    gt$widths <- grid::unit(width_inches, "inches")
    gt$heights <- grid::unit(height_inches, "inches")
  }

  hm$gtable <- gt

  p <- suppressWarnings(ggplotify::as.ggplot(hm$gtable)) + ggplot2::theme_void()
  attr(p, "tb_skip_force_black_text") <- TRUE
  tbl <- data.frame(gene = rownames(mat), mat, check.names = FALSE, stringsAsFactors = FALSE)

  list(
    plots = list(heatmap = p),
    figures = list(heatmap = p),
    tables = list(heatmap_data = tbl),
    tabs = NULL
  )
}

#' Render F-test Heatmap
#'
#' Renders a heatmap for statistically significant genes with optional
#' significance star labels (*, **, ***).
#'
#' @param results Engine results from stats_ftest_heatmap_run
#' @param style Style parameters
#' @param context Render context
#' @return list(plots, figures, tables)
tb_render_ftest_heatmap <- function(results, style, context = NULL) {
  data <- results$data %||% results

  # Check for empty results (no significant genes)
  if (!is.null(data$message) && length(data$gene_order) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = data$message %||% "No significant genes found",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    return(list(
      plots = list(heatmap = p_empty),
      figures = list(heatmap = p_empty),
      tables = list(heatmap_data = data.frame()),
      tabs = NULL
    ))
  }

  show_sig_labels <- isTRUE(style$show_sig_labels %||% TRUE)

  # Append significance stars to row names if requested
  if (show_sig_labels && !is.null(data$stats_table) && nrow(data$stats_table) > 0) {
    sig_map <- stats::setNames(
      as.character(data$stats_table$sig_label),
      as.character(data$stats_table$gene)
    )

    # Modify row names in matrices
    if (!is.null(data$mat_log) && nrow(data$mat_log) > 0) {
      current_names <- rownames(data$mat_log)
      new_names <- paste0(current_names, sig_map[current_names])
      new_names[is.na(new_names)] <- current_names[is.na(new_names)]
      rownames(data$mat_log) <- new_names
    }
    if (!is.null(data$mat_zscore) && nrow(data$mat_zscore) > 0) {
      current_names <- rownames(data$mat_zscore)
      new_names <- paste0(current_names, sig_map[current_names])
      new_names[is.na(new_names)] <- current_names[is.na(new_names)]
      rownames(data$mat_zscore) <- new_names
    }

    # Update results$data for the heatmap renderer
    results$data <- data
  }

  # Delegate to existing heatmap renderer
  out <- tb_render_heatmap(results, style, context)

  list(
    plots = out$plots %||% list(),
    figures = out$figures %||% list(),
    tables = list(heatmap_data = data.frame()),
    tabs = NULL
  )
}

# ---- FC Heatmap Renderers ----------------------------------------------------

#' Render FC Targeted Heatmap
#'
#' Renders a fold change heatmap for a user-provided gene list.
#' Columns represent comparisons (e.g., KO1/Control), rows represent genes.
#'
#' @param results Engine results from stats_fc_heatmap_run
#' @param style Style parameters
#' @param context Optional context
#' @return Render output list with plots and tables
tb_render_fc_heatmap <- function(results, style, context = NULL) {
  tb_require_pkg("ggplot2")
  tb_require_pkg("pheatmap")
  tb_require_pkg("ggplotify")

  params <- list()
  data <- results
  if (is.list(results) && !is.null(results$data) && is.list(results$data)) {
    params <- results$params %||% list()
    data <- results$data
  }

  # Determine color mode: zscore or fc
  color_mode <- as.character(style$color_mode %||% "zscore")[1]
  if (!color_mode %in% c("zscore", "fc")) color_mode <- "zscore"

  mat <- if (identical(color_mode, "fc")) data$mat_fc else data$mat_zscore
  dendro <- if (identical(color_mode, "fc")) data$dendro_fc else data$dendro_zscore

  # Early return if no valid matrix
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = "No heatmap data available",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    return(list(
      plots = list(fc_heatmap = p_empty),
      figures = list(fc_heatmap = p_empty),
      tables = list(fc_heatmap_data = data.frame())
    ))
  }

  exclude_na_rows <- isTRUE(style$exclude_na_rows %||% params$exclude_na_rows %||% FALSE)
  if (exclude_na_rows) {
    keep <- stats::complete.cases(mat)
    mat <- mat[keep, , drop = FALSE]
  }

  # Check again after NA filtering
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = "All rows filtered out (all contain NAs)",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    return(list(
      plots = list(fc_heatmap = p_empty),
      figures = list(fc_heatmap = p_empty),
      tables = list(fc_heatmap_data = data.frame())
    ))
  }

  cluster_rows_requested <- isTRUE(params$cluster_rows %||% TRUE)
  cluster_rows_arg <- FALSE
  cluster_method <- style$cluster_method %||% "hierarchical"
  kmeans_clusters <- NULL
  kmeans_row_order <- NULL

  if (cluster_rows_requested && nrow(mat) >= 2) {
    if (cluster_method %in% c("kmeans", "kmedians")) {
      # K-means or K-medians clustering (computed at render time)
      k <- as.integer(style$cluster_k %||% 2)
      k <- min(k, nrow(mat))
      if (k < 2) k <- 2

      m_complete <- mat[stats::complete.cases(mat), , drop = FALSE]
      if (nrow(m_complete) >= k) {
        if (cluster_method == "kmeans") {
          set.seed(42)
          km <- stats::kmeans(m_complete, centers = k, nstart = 25)
          cluster_assignments <- km$cluster
        } else {
          if (requireNamespace("cluster", quietly = TRUE)) {
            set.seed(42)
            pam_result <- cluster::pam(m_complete, k = k)
            cluster_assignments <- pam_result$clustering
          } else {
            set.seed(42)
            km <- stats::kmeans(m_complete, centers = k, nstart = 25)
            cluster_assignments <- km$cluster
          }
        }
        names(cluster_assignments) <- rownames(m_complete)

        kmeans_clusters <- rep(NA_integer_, nrow(mat))
        names(kmeans_clusters) <- rownames(mat)
        kmeans_clusters[names(cluster_assignments)] <- cluster_assignments

        valid_rows <- !is.na(kmeans_clusters)
        row_order <- order(kmeans_clusters[valid_rows])
        valid_row_names <- rownames(mat)[valid_rows][row_order]
        na_row_names <- rownames(mat)[!valid_rows]

        new_order <- c(valid_row_names, na_row_names)
        mat <- mat[new_order, , drop = FALSE]
        kmeans_clusters <- kmeans_clusters[new_order]
        kmeans_row_order <- new_order
      }
      cluster_rows_arg <- FALSE
    } else {
      # Hierarchical clustering - computed at render time with configurable options
      distance_method <- style$distance_method %||% "correlation"
      corr_type <- style$correlation_type %||% "spearman"
      linkage <- style$linkage_method %||% "average"

      clust_rows <- if (distance_method == "correlation") {
        which(rowSums(!is.na(mat)) >= 2)
      } else {
        which(rowSums(is.na(mat)) == 0)
      }

      if (length(clust_rows) >= 2) {
        m_sub <- mat[clust_rows, , drop = FALSE]

        if (distance_method == "correlation") {
          cmat <- suppressWarnings(stats::cor(t(m_sub), use = "pairwise.complete.obs", method = corr_type))
          cmat[is.na(cmat)] <- 0
          diag(cmat) <- 1
          d <- stats::as.dist(1 - cmat)
        } else if (distance_method == "euclidean") {
          d <- stats::dist(m_sub, method = "euclidean")
        } else {
          d <- stats::dist(m_sub, method = "manhattan")
        }

        hc <- stats::hclust(d, method = linkage)
        if (is.null(hc$labels)) hc$labels <- rownames(m_sub)

        if (length(hc$order) == nrow(mat)) {
          cluster_rows_arg <- hc
          dendro <- hc
        } else {
          cluster_rows_arg <- if (any(!is.finite(mat))) FALSE else TRUE
        }
      } else {
        cluster_rows_arg <- if (any(!is.finite(mat))) FALSE else TRUE
      }
    }
  }

  transpose <- isTRUE(style$transpose %||% FALSE)
  show_dendrogram <- isTRUE(style$show_dendrogram %||% TRUE)
  if (cluster_method %in% c("kmeans", "kmedians")) {
    show_dendrogram <- FALSE
  }
  show_row_labels <- isTRUE(style$show_row_labels %||% TRUE)
  row_font_size <- suppressWarnings(as.numeric(style$row_font_size %||% 8))
  if (!is.finite(row_font_size) || row_font_size <= 0) row_font_size <- 8

  na_color <- as.character(style$na_color %||% "grey50")[1]
  width <- suppressWarnings(as.numeric(style$width %||% 10))
  height <- suppressWarnings(as.numeric(style$height %||% 8))
  if (!is.finite(width) || width <= 0) width <- 10
  if (!is.finite(height) || height <= 0) height <- 8

  palette_name <- as.character(style$color_palette %||% "RdBu")[1]
  get_palette <- function(name, n = 100) {
    name <- as.character(name %||% "RdBu")[1]
    if (identical(name, "viridis")) {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        return(viridisLite::viridis(n))
      }
      return(grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(n))
    }
    if (identical(name, "RdBu")) {
      return(grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n))
    }
    if (identical(name, "RdYlBu")) {
      return(grDevices::colorRampPalette(c("#4575B4", "#FFFFBF", "#D73027"))(n))
    }
    if (identical(name, "PuOr")) {
      return(grDevices::colorRampPalette(c("#542788", "#F7F7F7", "#B35806"))(n))
    }
    grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n)
  }

  # For FC heatmaps, columns are comparisons (not samples), so no group annotation bar
  if (transpose) {
    mat <- t(mat)
  }

  # Truncate long row labels (gene/metabolite names) for display
  max_label_chars <- 15L
  rn <- rownames(mat)
  if (!is.null(rn)) {
    long <- nchar(rn) > max_label_chars
    if (any(long)) {
      rn[long] <- paste0(substr(rn[long], 1, max_label_chars - 3L), "...")
      rownames(mat) <- rn
    }
  }

  hm <- pheatmap::pheatmap(
    mat,
    cluster_rows = if (transpose) FALSE else cluster_rows_arg,
    cluster_cols = if (transpose) cluster_rows_arg else FALSE,
    treeheight_row = if (!transpose && show_dendrogram) 50 else 0,
    treeheight_col = if (transpose && show_dendrogram) 50 else 0,
    annotation_col = NULL,
    annotation_row = NULL,
    annotation_colors = NULL,
    annotation_legend = FALSE,
    legend = TRUE,
    na_col = na_color,
    scale = "none",
    show_rownames = if (transpose) TRUE else show_row_labels,
    show_colnames = if (transpose) show_row_labels else TRUE,
    fontsize_row = if (transpose) 8 else row_font_size,
    fontsize_col = if (transpose) row_font_size else 8,
    angle_col = if (transpose) 90 else 270,
    color = get_palette(palette_name, n = 100),
    border_color = "black",
    silent = TRUE,
    width = width,
    height = height
  )

  p <- ggplotify::as.ggplot(hm)
  attr(p, "tb_skip_force_black_text") <- TRUE

  list(
    plots = list(fc_heatmap = p),
    figures = list(fc_heatmap = p),
    tables = list(fc_heatmap_data = data.frame())
  )
}

#' Render Pathway FC Heatmap
#'
#' Renders a heatmap of GO/complex enrichment scores (Perseus-style)
#' across group-vs-control comparisons.
#' Rows = terms, columns = comparisons, values in [-1, 1].
#'
#' @param results Engine results from stats_pathway_fc_heatmap_run
#' @param style Style parameters from registry style_schema + viewer_schema
#' @param context Rendering context
#' @return list(plots, figures, tables)
tb_render_pathway_fc_heatmap <- function(results, style, context = NULL) {
  tb_require_pkg("ggplot2")
  tb_require_pkg("pheatmap")
  tb_require_pkg("ggplotify")

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  params <- list()
  data <- results
  if (is.list(results) && !is.null(results$data) && is.list(results$data)) {
    params <- results$params %||% list()
    data <- results$data
  }

  mat <- data$mat_score
  term_info_df <- data$term_info

  # Early return if no valid matrix
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No heatmap data available",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(pathway_fc_heatmap = p_empty),
      figures = list(pathway_fc_heatmap = p_empty),
      tables = list(pathway_fc_heatmap_data = data.frame())
    ))
  }

  # Apply flip_fc from viewer_schema
  if (isTRUE(style$flip_fc)) {
    mat <- -mat
  }

  # Build row labels from term_info
  show_go_id <- isTRUE(style$show_go_id %||% FALSE)
  if (!is.null(term_info_df) && is.data.frame(term_info_df) && nrow(term_info_df) > 0) {
    label_map <- stats::setNames(term_info_df$term_name, term_info_df$term_id)
    row_labels <- label_map[rownames(mat)]
    # Fallback for missing names
    row_labels[is.na(row_labels)] <- rownames(mat)[is.na(row_labels)]

    if (show_go_id) {
      row_labels <- paste0(rownames(mat), " | ", row_labels)
    }
    rownames(mat) <- row_labels
  }

  # Filter NA rows
  exclude_na_rows <- isTRUE(style$exclude_na_rows %||% TRUE)
  if (exclude_na_rows) {
    keep <- rowSums(!is.na(mat)) > 0  # Keep rows with at least one non-NA value
    mat <- mat[keep, , drop = FALSE]
  }

  # Check again after filtering
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "All rows filtered out (no overlap with data)",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()

    return(list(
      plots = list(pathway_fc_heatmap = p_empty),
      figures = list(pathway_fc_heatmap = p_empty),
      tables = list(pathway_fc_heatmap_data = data.frame())
    ))
  }

  # --- Clustering ---
  cluster_rows_requested <- isTRUE(params$cluster_rows %||% TRUE)
  cluster_rows_arg <- FALSE
  cluster_method <- style$cluster_method %||% "hierarchical"
  kmeans_clusters <- NULL

  if (cluster_rows_requested && nrow(mat) >= 2) {
    if (cluster_method %in% c("kmeans", "kmedians")) {
      k <- as.integer(style$cluster_k %||% 2)
      k <- min(k, nrow(mat))
      if (k < 2) k <- 2

      m_complete <- mat[stats::complete.cases(mat), , drop = FALSE]
      if (nrow(m_complete) >= k) {
        if (cluster_method == "kmeans") {
          set.seed(42)
          km <- stats::kmeans(m_complete, centers = k, nstart = 25)
          cluster_assignments <- km$cluster
        } else {
          if (requireNamespace("cluster", quietly = TRUE)) {
            set.seed(42)
            pam_result <- cluster::pam(m_complete, k = k)
            cluster_assignments <- pam_result$clustering
          } else {
            set.seed(42)
            km <- stats::kmeans(m_complete, centers = k, nstart = 25)
            cluster_assignments <- km$cluster
          }
        }
        names(cluster_assignments) <- rownames(m_complete)

        kmeans_clusters <- rep(NA_integer_, nrow(mat))
        names(kmeans_clusters) <- rownames(mat)
        kmeans_clusters[names(cluster_assignments)] <- cluster_assignments

        valid_rows <- !is.na(kmeans_clusters)
        row_order <- order(kmeans_clusters[valid_rows])
        valid_row_names <- rownames(mat)[valid_rows][row_order]
        na_row_names <- rownames(mat)[!valid_rows]

        new_order <- c(valid_row_names, na_row_names)
        mat <- mat[new_order, , drop = FALSE]
        kmeans_clusters <- kmeans_clusters[new_order]
      }
      cluster_rows_arg <- FALSE
    } else {
      # Hierarchical clustering
      distance_method <- style$distance_method %||% "correlation"
      corr_type <- style$correlation_type %||% "spearman"
      linkage <- style$linkage_method %||% "average"

      clust_rows <- if (distance_method == "correlation") {
        which(rowSums(!is.na(mat)) >= 2)
      } else {
        which(rowSums(is.na(mat)) == 0)
      }

      if (length(clust_rows) >= 2) {
        m_sub <- mat[clust_rows, , drop = FALSE]

        if (distance_method == "correlation") {
          cmat <- suppressWarnings(stats::cor(t(m_sub), use = "pairwise.complete.obs", method = corr_type))
          cmat[is.na(cmat)] <- 0
          diag(cmat) <- 1
          d <- stats::as.dist(1 - cmat)
        } else if (distance_method == "euclidean") {
          d <- stats::dist(m_sub, method = "euclidean")
        } else {
          d <- stats::dist(m_sub, method = "manhattan")
        }

        hc <- stats::hclust(d, method = linkage)
        if (is.null(hc$labels)) hc$labels <- rownames(m_sub)

        if (length(hc$order) == nrow(mat)) {
          cluster_rows_arg <- hc
        } else {
          cluster_rows_arg <- if (any(!is.finite(mat))) FALSE else TRUE
        }
      } else {
        cluster_rows_arg <- if (any(!is.finite(mat))) FALSE else TRUE
      }
    }
  }

  # --- Style parameters ---
  transpose <- isTRUE(style$transpose %||% FALSE)
  show_dendrogram <- isTRUE(style$show_dendrogram %||% TRUE)
  if (cluster_method %in% c("kmeans", "kmedians")) show_dendrogram <- FALSE
  show_row_labels <- isTRUE(style$show_row_labels %||% TRUE)
  row_font_size <- suppressWarnings(as.numeric(style$row_font_size %||% 8))
  if (!is.finite(row_font_size) || row_font_size <= 0) row_font_size <- 8

  na_color <- as.character(style$na_color %||% "grey50")[1]
  width <- suppressWarnings(as.numeric(style$width %||% 10))
  height <- suppressWarnings(as.numeric(style$height %||% 8))
  if (!is.finite(width) || width <= 0) width <- 10
  if (!is.finite(height) || height <= 0) height <- 8

  palette_name <- as.character(style$color_palette %||% "RdBu")[1]
  get_palette <- function(name, n = 100) {
    name <- as.character(name %||% "RdBu")[1]
    if (identical(name, "viridis")) {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        return(viridisLite::viridis(n))
      }
      return(grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(n))
    }
    if (identical(name, "RdBu")) {
      return(grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n))
    }
    if (identical(name, "RdYlBu")) {
      return(grDevices::colorRampPalette(c("#4575B4", "#FFFFBF", "#D73027"))(n))
    }
    if (identical(name, "PuOr")) {
      return(grDevices::colorRampPalette(c("#542788", "#F7F7F7", "#B35806"))(n))
    }
    grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n)
  }

  # Transpose if requested
  if (transpose) {
    mat <- t(mat)
  }

  # Symmetric color breaks centered at 0 for [-1, 1] score range
  max_abs <- max(abs(mat), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
  max_abs <- max(max_abs, 0.01)  # Avoid degenerate breaks
  breaks <- seq(-max_abs, max_abs, length.out = 101)

  hm <- pheatmap::pheatmap(
    mat,
    cluster_rows = if (transpose) FALSE else cluster_rows_arg,
    cluster_cols = if (transpose) cluster_rows_arg else FALSE,
    treeheight_row = if (!transpose && show_dendrogram) 50 else 0,
    treeheight_col = if (transpose && show_dendrogram) 50 else 0,
    annotation_col = NULL,
    annotation_row = NULL,
    annotation_colors = NULL,
    annotation_legend = FALSE,
    legend = TRUE,
    na_col = na_color,
    scale = "none",
    show_rownames = if (transpose) TRUE else show_row_labels,
    show_colnames = if (transpose) show_row_labels else TRUE,
    fontsize_row = if (transpose) 8 else row_font_size,
    fontsize_col = if (transpose) row_font_size else 8,
    angle_col = if (transpose) 90 else 270,
    color = get_palette(palette_name, n = 100),
    breaks = breaks,
    border_color = "black",
    silent = TRUE,
    width = width,
    height = height
  )

  p <- ggplotify::as.ggplot(hm)
  attr(p, "tb_skip_force_black_text") <- TRUE

  # Build data table for export
  tbl <- data.frame(
    term_id = data$term_order,
    stringsAsFactors = FALSE
  )
  if (!is.null(term_info_df) && is.data.frame(term_info_df)) {
    tbl <- merge(tbl, term_info_df, by = "term_id", all.x = TRUE, sort = FALSE)
  }
  # Append score columns from the original (non-flipped) matrix
  score_mat <- data$mat_score
  if (!is.null(score_mat) && nrow(score_mat) > 0) {
    for (cn in colnames(score_mat)) {
      tbl[[cn]] <- score_mat[tbl$term_id, cn]
    }
  }

  list(
    plots = list(pathway_fc_heatmap = p),
    figures = list(pathway_fc_heatmap = p),
    tables = list(pathway_fc_heatmap_data = tbl)
  )
}

#' Render FC F-test Heatmap
#'
#' Renders a fold change heatmap for statistically significant genes.
#'
#' @param results Engine results from stats_fc_ftest_heatmap_run
#' @param style Style parameters
#' @param context Optional context
#' @return Render output list with plots and tables
tb_render_fc_ftest_heatmap <- function(results, style, context = NULL) {
  data <- results$data %||% results

  # Check for empty results (no significant genes)
  if (!is.null(data$message) && length(data$gene_order) == 0) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5, y = 0.5,
        label = data$message %||% "No significant genes found",
        size = 6, color = "gray50"
      ) +
      ggplot2::theme_void()

    return(list(
      plots = list(fc_heatmap = p_empty),
      figures = list(fc_heatmap = p_empty),
      tables = list(fc_heatmap_data = data.frame(), stats_table = data.frame()),
      tabs = NULL
    ))
  }

  show_sig_labels <- isTRUE(style$show_sig_labels %||% FALSE)

  # Append significance stars to row names if requested
  if (show_sig_labels && !is.null(data$stats_table) && nrow(data$stats_table) > 0) {
    sig_map <- stats::setNames(
      as.character(data$stats_table$sig_label),
      as.character(data$stats_table$gene)
    )

    # Modify row names in matrices
    if (!is.null(data$mat_fc) && nrow(data$mat_fc) > 0) {
      current_names <- rownames(data$mat_fc)
      new_names <- paste0(current_names, sig_map[current_names])
      new_names[is.na(new_names)] <- current_names[is.na(new_names)]
      rownames(data$mat_fc) <- new_names
    }
    if (!is.null(data$mat_zscore) && nrow(data$mat_zscore) > 0) {
      current_names <- rownames(data$mat_zscore)
      new_names <- paste0(current_names, sig_map[current_names])
      new_names[is.na(new_names)] <- current_names[is.na(new_names)]
      rownames(data$mat_zscore) <- new_names
    }

    # Update results$data for the heatmap renderer
    results$data <- data
  }

  # Delegate to FC heatmap renderer
  out <- tb_render_fc_heatmap(results, style, context)

  list(
    plots = out$plots %||% list(),
    figures = out$figures %||% list(),
    tables = list(fc_heatmap_data = data.frame(), stats_table = data$stats_table %||% data.frame()),
    tabs = NULL
  )
}

# ---- Gene Bar Chart Renderer -------------------------------------------------

tb_render_gene_barchart <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  data <- results$data
  if (is.null(data)) {
    stop("gene_barchart: results$data is missing")
  }

  genes <- data$genes
  if (is.null(genes) || length(genes) == 0) {
    stop("gene_barchart: no genes in results$data")
  }

  # Get selected gene from viewer_schema (or default to first)
  selected_gene <- as.character(style$selected_gene %||% "")[1]
  if (!nzchar(selected_gene) || !selected_gene %in% genes) {
    selected_gene <- genes[1]
  }

  data_type <- as.character(data$data_type %||% "zscore")[1]

  # Style parameters
  y_range_mode <- as.character(style$y_range_mode %||% "auto")[1]
  y_min <- tb_num(style$y_min, 0)
  y_max <- tb_num(style$y_max, 10)
  log_transform <- as.character(style$log_transform %||% "none")[1]
  bar_color_mode <- as.character(style$bar_color_mode %||% "group")[1]
  bar_color <- as.character(style$bar_color %||% "#4285F4")[1]
  if (!grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", bar_color)) bar_color <- "#4285F4"
  show_bar_outline <- isTRUE(style$show_bar_outline %||% FALSE)
  bar_outline_width <- tb_num(style$bar_outline_width, 0.5)
  show_error_bars <- isTRUE(style$show_error_bars %||% TRUE)
  show_significance <- isTRUE(style$show_significance %||% TRUE)
  sig_display_mode <- as.character(style$sig_display_mode %||% "stars")[1]
  sig_text_size <- tb_num(style$sig_text_size, 3)
  title_text_size <- tb_num(style$title_text_size, 16)
  axis_text_size <- tb_num(style$axis_text_size, 14)
  group_label_size <- tb_num(style$group_label_size, 12)
  axis_style <- as.character(style$axis_style %||% "clean")[1]

  outline_color <- if (show_bar_outline) "#000000" else NA_character_
  outline_width <- if (show_bar_outline) bar_outline_width else 0

  # Get data (bars are groups, averaged)
  group_order <- data$group_order
  group_colors <- data$group_colors

  # Recompute group stats and significance from sample-level data when log-transformed
  if (data_type == "raw" && log_transform != "none" && !is.null(data$mat_sub) && !is.null(data$group_annotations)) {
    mat_transformed <- data$mat_sub
    if (log_transform == "log2") {
      mat_transformed <- log2(mat_transformed + 1)
    } else if (log_transform == "log10") {
      mat_transformed <- log10(mat_transformed + 1)
    }
    recomputed <- compute_group_stats(mat_transformed, data$group_annotations)
    group_means <- recomputed$means
    group_sem <- recomputed$sem
    significance_data <- compute_pairwise_significance(mat_transformed, data$group_annotations, data$genes)
  } else {
    group_means <- data$group_means
    group_sem <- data$group_sem
    significance_data <- data$significance
  }

  if (is.null(group_means) || !selected_gene %in% rownames(group_means)) {
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = sprintf("%s not found in data",
                                        tools::toTitleCase(data$entity_label %||% "Feature")),
                        size = 6, color = "gray50") +
      ggplot2::theme_void()
    return(list(
      plots = list(gene_barchart = p),
      tables = list(gene_barchart_data = data.frame())
    ))
  }

  means <- group_means[selected_gene, ]
  sems <- group_sem[selected_gene, ]

  df <- data.frame(
    x = group_order,
    y = as.numeric(means[group_order]),
    sem = as.numeric(sems[group_order]),
    stringsAsFactors = FALSE
  )
  df$x <- factor(df$x, levels = group_order)


  # Build plot - handle color mode
  if (bar_color_mode == "flat") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_col(fill = bar_color, color = outline_color, linewidth = outline_width)
  } else {
    # Use group colors
    if (is.null(group_colors) || length(group_colors) == 0) {
      group_colors <- grDevices::hcl.colors(length(group_order), palette = "Dark 3")
      names(group_colors) <- group_order
    }
    # Ensure group_colors is a named character vector
    if (is.list(group_colors)) {
      group_colors <- unlist(group_colors)
    }
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = x)) +
      ggplot2::geom_col(color = outline_color, linewidth = outline_width) +
      ggplot2::scale_fill_manual(values = group_colors, guide = "none")
  }

  # Add error bars if enabled
  if (show_error_bars && !all(is.na(df$sem))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = y - sem, ymax = y + sem),
      width = 0.2,
      linewidth = 0.5
    )
  }

  # Y-axis label with proper subscript formatting
  # Effective log state: user's additional log_transform takes precedence;
  # otherwise inherit from upstream (e.g., heatmap mat_log is log10-transformed)
  already_log <- as.character(data$already_log_transform %||% "none")[1]
  effective_log <- if (log_transform != "none") log_transform else already_log
  y_label <- switch(data_type,
    "zscore" = "Z-score",
    "raw" = tb_log_axis_label("Intensity", effective_log),
    "Z-score"
  )

  # Add significance bars if enabled
  if (show_significance && !is.null(significance_data) && !is.null(significance_data[[selected_gene]])) {
    sig_data <- significance_data[[selected_gene]]
    if (length(sig_data) > 0) {
      # Calculate y positions for significance bars
      y_max_data <- max(df$y + df$sem, na.rm = TRUE)
      if (!is.finite(y_max_data)) y_max_data <- max(df$y, na.rm = TRUE)
      if (!is.finite(y_max_data)) y_max_data <- 1

      y_step <- y_max_data * 0.08
      current_y <- y_max_data * 1.05

      for (comp_name in names(sig_data)) {
        sig_info <- sig_data[[comp_name]]
        pval <- sig_info$pval
        if (is.na(pval) || !is.finite(pval)) next

        grp_a <- sig_info$group_a
        grp_b <- sig_info$group_b

        # Get x positions
        x_a <- which(group_order == grp_a)
        x_b <- which(group_order == grp_b)
        if (length(x_a) == 0 || length(x_b) == 0) next

        # Draw bracket - show n.s. for non-significant comparisons
        if (pval >= 0.05) {
          label_text <- "n.s."
        } else if (sig_display_mode == "pvalue") {
          label_text <- sig_info$pval_formatted
        } else {
          label_text <- sig_info$stars
        }
        if (!nzchar(label_text)) next

        # Add bracket line and text
        p <- p +
          ggplot2::annotate("segment", x = x_a, xend = x_a, y = current_y - y_step * 0.3, yend = current_y,
                            linewidth = 0.4) +
          ggplot2::annotate("segment", x = x_a, xend = x_b, y = current_y, yend = current_y,
                            linewidth = 0.4) +
          ggplot2::annotate("segment", x = x_b, xend = x_b, y = current_y, yend = current_y - y_step * 0.3,
                            linewidth = 0.4) +
          ggplot2::annotate("text", x = (x_a + x_b) / 2, y = current_y + y_step * 0.2,
                            label = label_text, size = sig_text_size)

        current_y <- current_y + y_step
      }
    }
  }

  # Build table
  tbl <- df[, c("x", "y", "sem"), drop = FALSE]
  names(tbl) <- c("Group", "Mean", "SEM")

  # Determine title face based on axis_style

  title_face <- if (axis_style == "bold") "bold" else "plain"

  # Apply theming
  p <- p +
    ggplot2::labs(
      title = selected_gene,
      x = "",
      y = y_label
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_text_size, face = title_face, hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = group_label_size, color = "black"),
      axis.text.y = ggplot2::element_text(size = axis_text_size, color = "black"),
      axis.title = ggplot2::element_text(size = axis_text_size, color = "black"),
      panel.grid = ggplot2::element_blank()
    )

  # For intensity (raw) data in auto mode, ensure bars start from y=0
  if (y_range_mode == "auto" && data_type == "raw") {
    p <- p +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
  }

  # Apply manual Y limits if set
  if (y_range_mode == "manual") {
    p <- p +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::coord_cartesian(ylim = c(y_min, y_max))
  }

  list(
    plots = list(gene_barchart = p),
    tables = list(gene_barchart_data = tbl)
  )
}

# ---- PPI Network Renderer ----------------------------------------------------

#' Render PPI network as interactive visNetwork
#'
#' @param data Engine output data (nodes, edges, graph, stats, hubs)
#' @param style Style parameters (color_by, size_by, show_confidence, layout)
#' @param viewer_state Viewer-only state (selected_cluster)
#' @param width Plot width in inches
#' @param height Plot height in inches
#'
#' @return visNetwork htmlwidget or error message
tb_ppi_network <- function(data, style = list(), viewer_state = list(), width = 10, height = 8) {

  # Validate data
  if (is.null(data$nodes) || nrow(data$nodes) == 0) {
    return(shiny::div(
      class = "alert alert-warning",
      shiny::icon("exclamation-triangle"),
      " No network data available. Try adjusting parameters or check that proteins were found."
    ))
  }

  # Load visNetwork
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    return(shiny::div(
      class = "alert alert-danger",
      "Package 'visNetwork' is required but not installed."
    ))
  }

  # Get style parameters with defaults
  color_by <- style$color_by %||% "cluster"
  size_by <- style$size_by %||% "degree"
  show_confidence <- style$show_confidence %||% TRUE
  show_labels <- isTRUE(style$show_labels %||% TRUE)
  label_font_size <- as.integer(style$label_font_size %||% 24)
  layout_algo <- style$layout %||% "fr"  # Default to pre-computed Fruchterman-Reingold (no jiggly physics)

  # Get nodes and edges from data
  nodes <- data$nodes
  edges <- data$edges

  # Re-apply colors and sizes if style changed (viewer-time adjustment)
  # This allows dynamic updates without re-running the engine
  if (!is.null(data$graph)) {
    tryCatch({
      # Source ppi_utils if functions not already available
      if (!exists("ppi_apply_node_colors", mode = "function")) {
        utils_path <- "R/engines/ppi_utils.R"
        if (!file.exists(utils_path)) {
          # Try absolute path from app root
          utils_path <- file.path(getwd(), "R", "engines", "ppi_utils.R")
        }
        if (file.exists(utils_path)) {
          source(utils_path, local = FALSE)
        }
      }

      # Re-apply styling if functions available
      if (exists("ppi_apply_node_colors", mode = "function")) {
        nodes$color <- ppi_apply_node_colors(nodes, color_by)
        nodes$size <- ppi_apply_node_sizes(nodes, size_by)
      }

      # Re-apply edge styling
      if (exists("ppi_confidence_to_color", mode = "function") && isTRUE(show_confidence)) {
        edges$color <- ppi_confidence_to_color(edges$weight)
        edges$width <- 1 + edges$weight * 4
      } else if (!isTRUE(show_confidence)) {
        edges$color <- "#848484"
        edges$width <- 1
      }
    }, error = function(e) {
      warning(sprintf("PPI style application error: %s", e$message))
      # Fallback: use existing node/edge colors from engine output
    })
  }

  # Hide node labels if toggled off
  if (!show_labels) {
    nodes$label <- NA
  }

  # Filter by cluster if selected
  selected_cluster <- viewer_state$selected_cluster
  if (!is.null(selected_cluster) && nzchar(selected_cluster) && selected_cluster != "") {
    cluster_num <- suppressWarnings(as.integer(selected_cluster))
    if (!is.na(cluster_num)) {
      cluster_nodes <- nodes$id[nodes$cluster == cluster_num]
      nodes <- nodes[nodes$cluster == cluster_num, , drop = FALSE]
      edges <- edges[edges$from %in% cluster_nodes & edges$to %in% cluster_nodes, , drop = FALSE]
    }
  }

  # Convert dimensions to pixels (96 dpi)
  width_px <- round(tb_num(width, 10) * 96)
  height_px <- round(tb_num(height, 8) * 96)

  # Compute pre-computed layout coordinates (default: no jiggly physics)
  # This makes the network stable on load - nodes can still be dragged but won't bounce
  use_physics <- FALSE
  if (!is.null(data$graph) && layout_algo %in% c("fr", "kk", "circle", "grid")) {
    tryCatch({
      if (exists("ppi_compute_layout", mode = "function")) {
        coords <- ppi_compute_layout(data$graph, layout_algo)
        if (!is.null(coords) && nrow(coords) == nrow(nodes)) {
          nodes$x <- coords[, 1] * 500  # Scale for visNetwork
          nodes$y <- coords[, 2] * 500
        }
      }
    }, error = function(e) {
      # Fall back to physics if layout computation fails
      use_physics <- TRUE
    })
  } else {
    use_physics <- TRUE
  }

  # Build visNetwork
  network <- visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    width = paste0(width_px, "px"),
    height = paste0(height_px, "px")
  )

  # Configure interactivity and options
  # Note: nodesIdSelection and selectedBy removed — filtering is handled by the style panel
  network <- network %>%
    visNetwork::visNodes(
      font = list(size = label_font_size, face = "arial", strokeWidth = 3, strokeColor = "#ffffff")
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      navigationButtons = FALSE,
      keyboard = TRUE,
      hover = TRUE,
      hoverConnectedEdges = TRUE,
      tooltipDelay = 200
    ) %>%
    visNetwork::visLayout(randomSeed = 42)

  # Physics: disabled by default (stable layout), enabled only if pre-computation failed
  if (use_physics) {
    network <- network %>%
      visNetwork::visPhysics(
        enabled = TRUE,
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength = 100,
          springConstant = 0.08
        ),
        stabilization = list(
          enabled = TRUE,
          iterations = 200,
          updateInterval = 25
        )
      )
  } else {
    network <- network %>%
      visNetwork::visPhysics(enabled = FALSE)
  }

  # Constrain tooltip positioning to prevent off-screen overflow
  network <- network %>%
    visNetwork::visEvents(
      type = "on",
      hoverNode = htmlwidgets::JS("
        function(params) {
          setTimeout(function() {
            var tooltip = document.querySelector('.vis-tooltip');
            if (!tooltip) return;
            var rect = tooltip.getBoundingClientRect();
            var vw = window.innerWidth;
            var vh = window.innerHeight;
            if (rect.right > vw) tooltip.style.left = (parseFloat(tooltip.style.left) - (rect.right - vw) - 10) + 'px';
            if (rect.bottom > vh) tooltip.style.top = (parseFloat(tooltip.style.top) - (rect.bottom - vh) - 10) + 'px';
            if (rect.left < 0) tooltip.style.left = '10px';
            if (rect.top < 0) tooltip.style.top = '10px';
          }, 210);
        }
      ")
    )
  network <- htmlwidgets::prependContent(network,
    htmltools::tags$style("
      .vis-tooltip {
        max-width: 300px;
        word-wrap: break-word;
        white-space: normal;
        border-radius: 6px;
        padding: 8px 12px;
        font-size: 13px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        pointer-events: none;
      }
    ")
  )

  network
}


#' Render PPI network statistics table
#'
#' @param data Engine output data
#' @param style Style parameters (unused for table)
#'
#' @return data.frame for DT rendering
tb_ppi_stats_table <- function(data, style = list()) {

  if (is.null(data$stats)) {
    return(data.frame(
      Metric = "No data",
      Value = NA,
      stringsAsFactors = FALSE
    ))
  }

  stats <- data$stats

  # Build display table
  df <- data.frame(
    Metric = c(
      "Nodes (proteins)",
      "Edges (interactions)",
      "Network density",
      "Average degree",
      "Avg. path length",
      "Diameter",
      "Components",
      "Clusters",
      "Modularity"
    ),
    Value = c(
      as.character(stats$n_nodes %||% 0),
      as.character(stats$n_edges %||% 0),
      sprintf("%.4f", stats$density %||% 0),
      sprintf("%.2f", stats$avg_degree %||% 0),
      if (is.na(stats$avg_path_length)) "N/A" else sprintf("%.2f", stats$avg_path_length),
      if (is.na(stats$diameter)) "N/A" else as.character(stats$diameter),
      as.character(stats$n_components %||% 0),
      as.character(data$n_clusters %||% stats$n_clusters %||% "N/A"),
      if (is.null(stats$modularity) || is.na(stats$modularity)) "N/A" else sprintf("%.4f", stats$modularity)
    ),
    stringsAsFactors = FALSE
  )

  df
}


#' Render PPI hub proteins table
#'
#' @param data Engine output data
#' @param style Style parameters (unused for table)
#'
#' @return data.frame for DT rendering
tb_ppi_hubs_table <- function(data, style = list()) {

  if (is.null(data$hubs) || nrow(data$hubs) == 0) {
    return(data.frame(
      Protein = character(0),
      Degree = integer(0),
      Betweenness = numeric(0),
      Closeness = numeric(0),
      Eigenvector = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  hubs <- data$hubs

  # Rename columns for display
  display_df <- data.frame(
    Protein = hubs$name,
    Degree = hubs$degree,
    Betweenness = round(hubs$betweenness, 4),
    Closeness = round(hubs$closeness, 4),
    Eigenvector = round(hubs$eigenvector, 4),
    stringsAsFactors = FALSE
  )

  # Add cluster if available
  if ("cluster" %in% names(hubs)) {
    display_df$Cluster <- hubs$cluster
  }

  # Add log2fc/padj if available (from upstream volcano)
  if ("log2fc" %in% names(hubs)) {
    display_df$log2FC <- round(hubs$log2fc, 3)
  }
  if ("padj" %in% names(hubs)) {
    display_df$FDR <- signif(hubs$padj, 3)
  }

  display_df
}


#' Render PPI nodes table (all nodes with attributes)
#'
#' @param data Engine output data
#' @param style Style parameters
#'
#' @return data.frame for DT rendering
tb_ppi_nodes_table <- function(data, style = list()) {

  if (is.null(data$centrality) || nrow(data$centrality) == 0) {
    # Fall back to nodes if centrality not computed
    if (is.null(data$nodes) || nrow(data$nodes) == 0) {
      return(data.frame(
        Protein = character(0),
        Degree = integer(0),
        Cluster = integer(0),
        stringsAsFactors = FALSE
      ))
    }
    nodes <- data$nodes
    return(data.frame(
      Protein = nodes$id,
      Degree = nodes$degree,
      Cluster = nodes$cluster,
      stringsAsFactors = FALSE
    ))
  }

  centrality <- data$centrality

  # Build display table
  display_df <- data.frame(
    Protein = centrality$name,
    Degree = centrality$degree,
    Betweenness = round(centrality$betweenness, 4),
    Closeness = round(centrality$closeness, 4),
    Eigenvector = round(centrality$eigenvector, 4),
    stringsAsFactors = FALSE
  )

  # Add cluster from nodes if available
  if (!is.null(data$nodes) && "cluster" %in% names(data$nodes)) {
    cluster_map <- stats::setNames(data$nodes$cluster, data$nodes$id)
    display_df$Cluster <- cluster_map[centrality$name]
  }

  # Sort by degree descending
  display_df <- display_df[order(display_df$Degree, decreasing = TRUE), , drop = FALSE]

  display_df
}


#' Render PPI network engine output
#'
#' Main dispatcher function for PPI network visualization and tables.
#'
#' @param results Engine output (from stats_ppi_network_run)
#' @param style Style parameters
#' @param meta Metadata and viewer state
#'
#' @return List with plots (ppi_network) and tables (stats, hubs, nodes)
tb_render_ppi_network <- function(results, style, meta) {

  data <- results$data

  # Get viewer state for cluster filtering
  viewer_state <- list(
    selected_cluster = style$selected_cluster %||% meta$viewer_state$selected_cluster %||% NULL
  )

  # Render network visualization
  network <- tb_ppi_network(
    data = data,
    style = style,
    viewer_state = viewer_state,
    width = style$width %||% 10,
    height = style$height %||% 8
  )

  # Render tables
  stats_table <- tb_ppi_stats_table(data, style)
  hubs_table <- tb_ppi_hubs_table(data, style)
  nodes_table <- tb_ppi_nodes_table(data, style)

  list(
    plots = list(ppi_network = network),
    tables = list(
      ppi_stats = stats_table,
      ppi_hubs = hubs_table,
      ppi_nodes = nodes_table
    )
  )
}

# ---- Multi-Dataset Renderers ------------------------------------------------

#' Render multi_scatter plot
#' @export
render_multi_scatter <- function(data, style, ...) {
  if (is.null(data$scatter_data)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No scatter data"))
  }

  df <- data$scatter_data
  x_label <- data$x_label %||% "X"
  y_label <- data$y_label %||% "Y"

  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))

  # Color mode
  color_mode <- style$color_mode %||% "quadrant"
  if (color_mode == "quadrant") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = quadrant),
      size = style$point_size %||% 2,
      alpha = style$point_alpha %||% 0.7
    )
    p <- p + ggplot2::scale_color_manual(
      values = c("Q1 (+/+)" = "#2ecc71", "Q2 (-/+)" = "#3498db",
                 "Q3 (-/-)" = "#9b59b6", "Q4 (+/-)" = "#e74c3c"),
      name = "Quadrant"
    )
  } else {
    p <- p + ggplot2::geom_point(
      color = style$point_color %||% "#1f77b4",
      size = style$point_size %||% 2,
      alpha = style$point_alpha %||% 0.7
    )
  }

  # Reference lines at origin
  p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)
  p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)

  # Regression line
  if (isTRUE(style$show_regression_line)) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "solid")
  }

  # Correlation annotation
  if (isTRUE(style$show_correlation %||% TRUE)) {
    cor_info <- data$correlation
    label_text <- sprintf("r = %.3f (n = %d)", cor_info$pearson, cor_info$n)
    p <- p + ggplot2::annotate(
      "text",
      x = min(df$x, na.rm = TRUE),
      y = max(df$y, na.rm = TRUE),
      label = label_text,
      hjust = 0, vjust = 1,
      size = 4, fontface = "bold"
    )
  }

  # Labels for top outliers
  if (isTRUE(style$show_labels)) {
    tb_require_pkg("ggrepel")
    top_n <- style$label_top_n %||% 10
    # Calculate distance from origin
    df$distance <- sqrt(df$x^2 + df$y^2)
    top_df <- df[order(-df$distance), ][1:min(top_n, nrow(df)), ]
    p <- p + ggrepel::geom_text_repel(
      data = top_df,
      ggplot2::aes(label = gene_symbol),
      size = 3, max.overlaps = 20
    )
  }

  # Axis labels
  p <- p + ggplot2::labs(x = x_label, y = y_label)

  # Theme
  p <- p + ggplot2::theme_minimal(base_size = style$axis_text_size %||% 16)
  p <- p + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "right"
  )

  p
}

#' Render multi_correlation plot
#' @export
render_multi_correlation <- function(data, style, ...) {
  if (is.null(data$correlation_data)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No correlation data"))
  }

  df <- data$correlation_data
  x_label <- data$x_label %||% "X"
  y_label <- data$y_label %||% "Y"

  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))

  # Points (with outlier highlighting)
  if (isTRUE(style$highlight_outliers %||% TRUE) && "is_outlier" %in% names(df)) {
    p <- p + ggplot2::geom_point(
      data = df[!df$is_outlier, ],
      color = style$point_color %||% "#2c3e50",
      size = style$point_size %||% 2,
      alpha = style$point_alpha %||% 0.6
    )
    p <- p + ggplot2::geom_point(
      data = df[df$is_outlier, ],
      color = style$outlier_color %||% "#e74c3c",
      size = (style$point_size %||% 2) * 1.2,
      alpha = 0.8
    )
  } else {
    p <- p + ggplot2::geom_point(
      color = style$point_color %||% "#2c3e50",
      size = style$point_size %||% 2,
      alpha = style$point_alpha %||% 0.6
    )
  }

  # Regression line with CI band
  if (isTRUE(style$show_regression_line %||% TRUE)) {
    if (isTRUE(style$show_ci_band %||% TRUE)) {
      p <- p + ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        color = style$regression_line_color %||% "#e74c3c",
        fill = style$regression_line_color %||% "#e74c3c",
        alpha = style$ci_band_alpha %||% 0.2
      )
    } else {
      p <- p + ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        color = style$regression_line_color %||% "#e74c3c"
      )
    }
  }

  # Statistics annotation
  if (isTRUE(style$show_stats_annotation %||% TRUE)) {
    stats_text <- sprintf(
      "r = %.3f, p = %.2e\nR%s = %.3f",
      data$pearson$r,
      data$pearson$p,
      "\u00B2",  # superscript 2
      data$regression$r_squared
    )
    p <- p + ggplot2::annotate(
      "text",
      x = min(df$x, na.rm = TRUE),
      y = max(df$y, na.rm = TRUE),
      label = stats_text,
      hjust = 0, vjust = 1,
      size = 4, fontface = "bold"
    )
  }

  # Axis labels
  p <- p + ggplot2::labs(x = x_label, y = y_label)

  # Theme
  p <- p + ggplot2::theme_minimal(base_size = style$axis_text_size %||% 16)
  p <- p + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  p
}

#' Render multi_scatter_data table
#' @export
render_multi_scatter_data_table <- function(data, style, ...) {
  df <- data$scatter_data
  if (is.null(df)) return(data.frame())

  # Format for display
  df$x <- round(df$x, 4)
  df$y <- round(df$y, 4)

  df[, c("protein_id", "gene_symbol", "x", "y", "quadrant")]
}

#' Render multi_correlation_data table
#' @export
render_multi_correlation_data_table <- function(data, style, ...) {
  df <- data$correlation_data
  if (is.null(df)) return(data.frame())

  df$x <- round(df$x, 4)
  df$y <- round(df$y, 4)
  df$residual <- round(df$residual, 4)

  df[, c("protein_id", "gene_symbol", "x", "y", "residual", "is_outlier")]
}

#' Render multi_correlation_stats table
#' @export
render_multi_correlation_stats_table <- function(data, style, ...) {
  data$statistics %||% data.frame()
}

#' Render multi_scatter engine output
#'
#' Main dispatcher function for multi-scatter visualization and tables.
#'
#' @param results Engine output (from run_multi_scatter)
#' @param style Style parameters
#' @param meta Metadata and viewer state
#'
#' @return List with plots and tables
tb_render_multi_scatter <- function(results, style, meta) {
  data <- results$data

  # Render scatter plot
  scatter_plot <- render_multi_scatter(data, style)

  # Render data table
  scatter_table <- render_multi_scatter_data_table(data, style)

  list(
    plots = list(multi_scatter = scatter_plot),
    tables = list(multi_scatter_data = scatter_table)
  )
}

#' Render multi_correlation engine output
#'
#' Main dispatcher function for multi-correlation visualization and tables.
#'
#' @param results Engine output (from run_multi_correlation)
#' @param style Style parameters
#' @param meta Metadata and viewer state
#'
#' @return List with plots and tables
tb_render_multi_correlation <- function(results, style, meta) {
  data <- results$data

  # Render correlation plot
  correlation_plot <- render_multi_correlation(data, style)

  # Render data table
  correlation_table <- render_multi_correlation_data_table(data, style)

  # Render stats table
  stats_table <- render_multi_correlation_stats_table(data, style)

  list(
    plots = list(multi_correlation = correlation_plot),
    tables = list(
      multi_correlation_data = correlation_table,
      multi_correlation_stats = stats_table
    )
  )
}

# ---- Replicate Clustering ---------------------------------------------------

tb_render_replicate_clustering <- function(results, style, meta) {
  tb_require_pkg("ggplot2")
  tb_require_pkg("ggplotify")

  data <- results$data %||% results
  hc <- data$hc
  group_map <- data$group_map %||% character(0)
  group_colors <- data$group_colors %||% character(0)

  # Early return if no hclust object

  if (is.null(hc) || !inherits(hc, "hclust")) {
    p_empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No clustering data available",
                        size = 6, color = "gray50") +
      ggplot2::theme_void()
    return(list(
      plots = list(dendrogram = p_empty),
      tables = list(cluster_info = data.frame())
    ))
  }

  label_size <- as.numeric(style$label_size %||% 3)
  color_branches <- isTRUE(style$color_branches %||% FALSE)
  hang_val <- as.numeric(style$hang %||% 0.1)

  # Convert to dendrogram and color leaves by group
  dend <- as.dendrogram(hc, hang = hang_val)

  # Color leaf labels by group
  label_col_fn <- function(n) {
    if (is.leaf(n)) {
      lbl <- attr(n, "label")
      grp <- group_map[lbl]
      col <- if (!is.na(grp) && grp %in% names(group_colors)) {
        group_colors[grp]
      } else {
        "black"
      }
      attr(n, "nodePar") <- list(
        lab.col = col,
        lab.cex = label_size / 3,
        pch = NA
      )
    }
    n
  }
  dend <- dendrapply(dend, label_col_fn)

  # Color branches by majority group if requested
  if (color_branches) {
    color_branch_fn <- function(n) {
      if (!is.leaf(n)) {
        leaves <- labels(n)
        grps <- group_map[leaves]
        grps <- grps[!is.na(grps)]
        if (length(grps) > 0) {
          majority <- names(sort(table(grps), decreasing = TRUE))[1]
          col <- group_colors[majority] %||% "gray50"
          attr(n, "edgePar") <- list(col = col, lwd = 1.5)
        }
      }
      n
    }
    dend <- dendrapply(dend, color_branch_fn)
  }

  # Build horizontal dendrogram using base R wrapped via ggplotify
  plot_fn <- function() {
    # Right margin accommodates horizontal labels; no y-axis needed
    max_label_len <- max(nchar(hc$labels), na.rm = TRUE)
    right_margin <- max(5, min(15, max_label_len * 0.6))

    par(mar = c(2, 1, 2, right_margin))
    plot(dend, horiz = TRUE, main = "", xlab = "", ylab = "",
         axes = FALSE)
  }

  # Temporarily switch to a writable directory so ggplotify / base R

  # does not try to create Rplots.pdf in a non-writable working dir
  old_wd <- getwd()
  setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  p <- suppressWarnings(ggplotify::as.ggplot(plot_fn))
  attr(p, "tb_skip_force_black_text") <- TRUE

  # Build cluster_info table
  cluster_info <- data$cluster_info
  if (is.null(cluster_info)) {
    cluster_info <- data.frame(
      sample = hc$labels,
      group = as.character(group_map[hc$labels]),
      stringsAsFactors = FALSE
    )
  }

  list(
    plots = list(dendrogram = p),
    tables = list(cluster_info = cluster_info)
  )
}

# ---- Peptide Region ----------------------------------------------------------

tb_render_peptide_region <- function(results, style, meta) {
  tb_require_pkg("ggplot2")

  data <- results$data
  if (is.null(data)) stop("peptide_region: results$data is missing")

  mode <- data$mode %||% "overview"

  if (identical(mode, "detail")) {
    return(.pr_render_detail(data, style))
  }

  # ---- OVERVIEW MODE ----
  scores <- data$protein_scores
  if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0) {
    p_msg <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No proteins scored (check min_peptides threshold)",
                        size = 5, color = "grey50") +
      ggplot2::theme_void()
    return(list(plots = list(peptide_region = p_msg), tables = list(), tabs = NULL))
  }

  is_single <- isTRUE(data$single_group)

  # ---- Build display table ----
  display <- data.frame(
    Gene              = scores$gene_symbol,
    Protein           = scores$protein_id,
    Biology           = scores$biology_tag %||% "Other",
    Peptides          = scores$n_peptides,
    `Unique Peptides` = scores$n_unique_peptides,
    `Peptide CV`      = round(scores$peptide_cv, 3),
    `Replicate CV`    = round(scores$mean_replicate_cv, 3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!is_single) {
    display[["Best FC Disparity"]] <- round(scores$best_fc_disparity, 3)
    display[["Max Log2 FC"]]       <- round(scores$max_log2fc, 3)

    # Per-comparison columns (kept set from engine)
    fc_mean_cols <- data$fc_mean_cols %||% character(0)
    fc_disp_cols <- data$fc_disp_cols %||% character(0)
    comp_keys    <- data$comp_keys_kept %||% character(0)
    for (i in seq_along(comp_keys)) {
      mc <- fc_mean_cols[i]
      dc <- fc_disp_cols[i]
      if (mc %in% names(scores)) {
        display[[paste0("FC Mean (", comp_keys[i], ")")]] <- round(scores[[mc]], 3)
      }
      if (dc %in% names(scores)) {
        display[[paste0("FC Disparity (", comp_keys[i], ")")]] <- round(scores[[dc]], 3)
      }
    }
  }

  display[["Score"]] <- round(scores$composite_score, 3)

  if ("is_target" %in% names(scores)) {
    display[["Target"]] <- ifelse(scores$is_target, "\u2605", "")
  }

  # GO column last (full string is searchable; renderer can ellipsis-truncate)
  display[["GO Terms"]] <- scores$go_terms_str %||% ""

  # ---- Resolve scatter axes from viewer/style state ----
  # Note: viewer_schema fields are merged into style at the page level
  numeric_cols <- .pr_overview_numeric_cols(scores)
  x_col <- .pr_resolve_axis(style$pr_x_axis, numeric_cols, default = "peptide_cv")
  y_col <- .pr_resolve_axis(style$pr_y_axis, numeric_cols,
                            default = if (is_single) "mean_replicate_cv" else "best_fc_disparity")

  # ---- Build static ggplot scatter (used for export / non-interactive view) ----
  p_static <- .pr_build_overview_scatter_static(scores, style, x_col, y_col)

  # Always include the protein scores table in the rendered output. The
  # viewer toggles between graph and table on the client via conditionalPanel,
  # so the render pipeline doesn't need to react to the choice — gating this
  # on a style field previously caused a render death loop.
  list(
    plots  = list(peptide_region = p_static),
    tables = list(protein_scores = display),
    tabs   = NULL
  )
}

# ---- Peptide Region overview helpers ----

#' Fixed biology-tag colour palette. Every tag gets a distinct, saturated
#' colour — no near-whites or pale tans. Tags not in the named map fall
#' through to the cycling fallback which also avoids white.
.pr_biology_palette <- function(levels) {
  named <- c(
    "Proteasome"    = "#E15759",
    "Transmembrane" = "#4E79A7",
    "Lysosomal"     = "#59A14F",
    "Secreted"      = "#F28E2B",
    "Mitochondrial" = "#B07AA1",
    "Protease"      = "#FF9DA7",
    "Nuclear"       = "#9C755F",
    "Cytoplasmic"   = "#76B7B2",
    "Membrane"      = "#EDC948",
    "Other"         = "#BAB0AC"
  )
  out <- character(length(levels))
  names(out) <- levels
  # Assign fixed colours for known tags
  known <- levels %in% names(named)
  out[known] <- named[levels[known]]
  # Cycle through a fallback palette for any unknown tags
  if (any(!known)) {
    fallback <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
                  "#A6D854", "#E6AB02", "#D95F02", "#7570B3")
    unknown_levels <- levels[!known]
    out[!known] <- fallback[((seq_along(unknown_levels) - 1) %% length(fallback)) + 1]
  }
  out
}

#' Numeric columns from protein_scores eligible for scatter axes
.pr_overview_numeric_cols <- function(scores) {
  if (is.null(scores) || !is.data.frame(scores)) return(character(0))
  num_idx <- vapply(scores, is.numeric, logical(1))
  cols <- names(scores)[num_idx]
  # Drop columns that are uninteresting for plotting
  drop <- c("is_target")
  setdiff(cols, drop)
}

#' Resolve a viewer-provided axis name against the available numeric columns
.pr_resolve_axis <- function(requested, available, default) {
  if (length(available) == 0L) return(default)
  if (is.null(requested) || !nzchar(as.character(requested))) {
    return(if (default %in% available) default else available[1])
  }
  req <- as.character(requested)[1]
  if (req %in% available) return(req)
  if (default %in% available) return(default)
  available[1]
}

#' Pretty axis label for a protein_scores column
.pr_axis_label <- function(col) {
  switch(
    col,
    "peptide_cv"        = "Peptide CV",
    "mean_replicate_cv" = "Mean replicate CV",
    "best_fc_disparity" = "Best FC disparity (SD of log2FC)",
    "max_log2fc"        = "Max |log2 FC|",
    "composite_score"   = "Composite score",
    "n_peptides"        = "# Peptides",
    "n_unique_peptides" = "# Unique peptides",
    {
      pretty <- col
      if (startsWith(col, "fc_mean_")) {
        pretty <- paste0("Mean log2 FC (", sub("^fc_mean_", "", col), ")")
      } else if (startsWith(col, "fc_disp_")) {
        pretty <- paste0("FC disparity (", sub("^fc_disp_", "", col), ")")
      }
      pretty
    }
  )
}

#' Static ggplot scatter (for PNG/PDF export)
.pr_build_overview_scatter_static <- function(scores, style, x_col, y_col) {
  tb_require_pkg("ggplot2")
  point_alpha       <- tb_num(style$point_alpha, 0.8)
  point_size_scale  <- tb_num(style$point_size_scale, 1)
  highlight_targets <- isTRUE(style$highlight_targets %||% TRUE)
  axis_text_size    <- tb_num(style$axis_text_size, 14)
  title_text_size   <- tb_num(style$title_text_size, 16)

  df <- data.frame(
    x        = scores[[x_col]],
    y        = scores[[y_col]],
    biology  = scores$biology_tag %||% "Other",
    n_pep    = scores$n_peptides,
    gene     = scores$gene_symbol,
    is_tgt   = isTRUE(highlight_targets) & (scores$is_target %||% FALSE),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]

  if (nrow(df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No data points to plot",
                          size = 5, color = "grey50") +
        ggplot2::theme_void()
    )
  }

  size_range <- c(1.5, 6) * point_size_scale

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$biology, size = .data$n_pep),
      alpha = point_alpha
    ) +
    ggplot2::scale_size(range = size_range, name = "# peptides") +
    ggplot2::labs(
      x = .pr_axis_label(x_col),
      y = .pr_axis_label(y_col),
      color = "Biology"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text  = ggplot2::element_text(size = axis_text_size * 0.75),
      axis.title = ggplot2::element_text(size = axis_text_size * 0.85),
      plot.title = ggplot2::element_text(face = "bold", size = title_text_size)
    ) +
    ggplot2::scale_color_manual(
      values = .pr_biology_palette(sort(unique(df$biology))),
      na.value = "#888888"
    )

  # Highlight targets with a black outline ring
  if (any(df$is_tgt)) {
    p <- p +
      ggplot2::geom_point(
        data = df[df$is_tgt, , drop = FALSE],
        ggplot2::aes(size = .data$n_pep),
        shape = 21, color = "black", fill = NA, stroke = 1
      )
  }

  p
}

#' Interactive plotly scatter (used by result viewer)
#' @param style merged style + viewer state from active_effective_state()$style
tb_peptide_region_overview_plotly <- function(results, style) {
  tb_require_pkg("plotly")

  data <- results$data
  if (is.null(data)) return(NULL)
  scores <- data$protein_scores
  if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0) return(NULL)

  is_single <- isTRUE(data$single_group)

  numeric_cols <- .pr_overview_numeric_cols(scores)
  x_col <- .pr_resolve_axis(style$pr_x_axis, numeric_cols, default = "peptide_cv")
  y_col <- .pr_resolve_axis(style$pr_y_axis, numeric_cols,
                            default = if (is_single) "mean_replicate_cv" else "best_fc_disparity")

  point_alpha       <- tb_num(style$point_alpha, 0.8)
  point_size_scale  <- tb_num(style$point_size_scale, 1)
  highlight_targets <- isTRUE(style$highlight_targets %||% TRUE)

  # Build the data frame, filtering to finite x/y
  df <- data.frame(
    x        = scores[[x_col]],
    y        = scores[[y_col]],
    biology  = scores$biology_tag %||% "Other",
    n_pep    = scores$n_peptides,
    gene     = scores$gene_symbol,
    protein  = scores$protein_id,
    cv       = round(scores$peptide_cv %||% NA_real_, 3),
    disp     = round(scores$best_fc_disparity %||% NA_real_, 3),
    composite = round(scores$composite_score %||% NA_real_, 3),
    is_tgt   = isTRUE(highlight_targets) & (scores$is_target %||% FALSE),
    go_short = .pr_truncate(scores$go_terms_str %||% "", 80L),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  # Marker size in plotly pixels: scale by sqrt(n_pep) for readability
  pep_sz <- sqrt(pmax(df$n_pep, 1))
  size_min <- 6 * point_size_scale
  size_max <- 22 * point_size_scale
  rng <- range(pep_sz, na.rm = TRUE)
  if (diff(rng) < 1e-6) {
    sizes <- rep((size_min + size_max) / 2, length(pep_sz))
  } else {
    sizes <- size_min + (pep_sz - rng[1]) / diff(rng) * (size_max - size_min)
  }

  # Biology color palette (shared with static scatter)
  bio_levels <- sort(unique(df$biology))
  color_map <- .pr_biology_palette(bio_levels)
  pal <- unname(color_map)
  df$color  <- color_map[df$biology]

  hover_text <- sprintf(
    "<b>%s</b> (%s)<br>Biology: %s<br># peptides: %d<br>Peptide CV: %s<br>FC disparity: %s<br>Score: %s<br>%s",
    df$gene, df$protein, df$biology, df$n_pep,
    format(df$cv, nsmall = 2), format(df$disp, nsmall = 2),
    format(df$composite, nsmall = 2),
    ifelse(nzchar(df$go_short), paste0("GO: ", df$go_short), "")
  )

  p <- plotly::plot_ly(
    data    = df,
    x       = ~x,
    y       = ~y,
    type    = "scatter",
    mode    = "markers",
    color   = ~biology,
    colors  = pal[seq_along(bio_levels)],
    text    = hover_text,
    hoverinfo = "text",
    marker  = list(
      size    = sizes,
      sizemode = "diameter",
      opacity = point_alpha,
      line    = list(width = 0.5, color = "rgba(0,0,0,0.4)")
    ),
    source  = "peptide_region_overview"
  )

  # Target highlight: overlay black-ringed markers
  if (any(df$is_tgt)) {
    df_t <- df[df$is_tgt, , drop = FALSE]
    sizes_t <- sizes[df$is_tgt]
    p <- plotly::add_markers(
      p,
      data = df_t,
      x = ~x, y = ~y,
      marker = list(
        size = sizes_t + 4,
        color = "rgba(0,0,0,0)",
        line = list(width = 2, color = "black")
      ),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )
  }

  plotly::layout(
    p,
    xaxis  = list(title = .pr_axis_label(x_col), zeroline = FALSE),
    yaxis  = list(title = .pr_axis_label(y_col), zeroline = FALSE),
    legend = list(title = list(text = "Biology")),
    hovermode = "closest",
    margin = list(l = 60, r = 20, t = 30, b = 60)
  )
}

#' Truncate a string with an ellipsis
.pr_truncate <- function(x, n) {
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)
}

# ---- Peptide Region: Shared detail components ----

.pr_feature_category_map <- function() {
  list(
    "Domain"              = list(cat = "Domains",      color = "#4E79A7", ymin = 0.75, ymax = 1.0, alpha = 0.45),
    "Repeat"              = list(cat = "Domains",      color = "#4E79A7", ymin = 0.75, ymax = 1.0, alpha = 0.40),
    "Region"              = list(cat = "Domains",      color = "#76B7B2", ymin = 0.75, ymax = 1.0, alpha = 0.35),
    "Motif"               = list(cat = "Domains",      color = "#59A14F", ymin = 0.75, ymax = 1.0, alpha = 0.40),
    "Coiled coil"         = list(cat = "Domains",      color = "#EDC948", ymin = 0.75, ymax = 1.0, alpha = 0.40),
    "Transmembrane"       = list(cat = "Topology",     color = "#E15759", ymin = 0.56, ymax = 0.72, alpha = 0.85),
    "Topological domain"  = list(cat = "Topology",     color = "#F28E2B", ymin = 0.56, ymax = 0.72, alpha = 0.55),
    "Intramembrane"       = list(cat = "Topology",     color = "#B07AA1", ymin = 0.56, ymax = 0.72, alpha = 0.85),
    "Signal"              = list(cat = "Processing",   color = "#FF9DA7", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Transit peptide"     = list(cat = "Processing",   color = "#FF9DA7", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Propeptide"          = list(cat = "Processing",   color = "#FFBE7D", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Chain"               = list(cat = "Processing",   color = "#D4A6C8", ymin = 0.39, ymax = 0.53, alpha = 0.50),
    "Peptide"             = list(cat = "Processing",   color = "#D4A6C8", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Initiator methionine" = list(cat = "Processing",  color = "#FF9DA7", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Active site"         = list(cat = "Binding",      color = "#E15759", ymin = 0.22, ymax = 0.36, alpha = 0.90),
    "Binding site"        = list(cat = "Binding",      color = "#F28E2B", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Site"                = list(cat = "Binding",      color = "#86BCB6", ymin = 0.22, ymax = 0.36, alpha = 0.80),
    "DNA binding"         = list(cat = "Binding",      color = "#499894", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Nucleotide binding"  = list(cat = "Binding",      color = "#59A14F", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Metal binding"       = list(cat = "Binding",      color = "#B6992D", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Calcium binding"     = list(cat = "Binding",      color = "#B6992D", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Zinc finger"         = list(cat = "Binding",      color = "#B6992D", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Modified residue"    = list(cat = "Modification", color = "#B07AA1", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Disulfide bond"      = list(cat = "Modification", color = "#E15759", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Glycosylation"       = list(cat = "Modification", color = "#59A14F", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Lipidation"          = list(cat = "Modification", color = "#EDC948", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Cross-link"          = list(cat = "Modification", color = "#FF9DA7", ymin = 0.05, ymax = 0.19, alpha = 0.80)
  )
}

.pr_feature_rectangles <- function(features_df, protein_length) {
  empty <- data.frame(
    type = character(0),
    category = character(0),
    color = character(0),
    alpha = numeric(0),
    xmin = numeric(0),
    xmax = numeric(0),
    ymin = numeric(0),
    ymax = numeric(0),
    start = numeric(0),
    end = numeric(0),
    description = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(features_df) || !is.data.frame(features_df) || nrow(features_df) == 0) {
    return(empty)
  }

  category_map <- .pr_feature_category_map()
  site_width <- max(2, protein_length * 0.006)
  rows <- list()

  for (i in seq_len(nrow(features_df))) {
    ft_type <- as.character(features_df$type[i] %||% "")
    mapping <- category_map[[ft_type]]
    if (is.null(mapping)) next

    s <- suppressWarnings(as.numeric(features_df$start[i]))
    e <- suppressWarnings(as.numeric(features_df$end[i]))
    if (!is.finite(s) || !is.finite(e)) next

    xmin <- if (identical(s, e)) s - site_width else s - 0.5
    xmax <- if (identical(s, e)) e + site_width else e + 0.5
    desc <- as.character(features_df$description[i] %||% "")
    if (!nzchar(desc)) desc <- ft_type

    rows[[length(rows) + 1L]] <- data.frame(
      type = ft_type,
      category = mapping$cat,
      color = mapping$color,
      alpha = mapping$alpha,
      xmin = xmin,
      xmax = xmax,
      ymin = mapping$ymin,
      ymax = mapping$ymax,
      start = s,
      end = e,
      description = desc,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(empty)

  rect_df <- do.call(rbind, rows)
  rect_df$data_id <- paste0("pr_feature_", seq_len(nrow(rect_df)))
  rect_df$tooltip <- sprintf(
    "<b>%s</b><br>Type: %s<br>Position: %s-%s<br>%s",
    rect_df$category,
    rect_df$type,
    as.integer(rect_df$start),
    as.integer(rect_df$end),
    rect_df$description
  )
  rect_df$label_x <- (rect_df$xmin + rect_df$xmax) / 2
  rect_df$label_y <- (rect_df$ymin + rect_df$ymax) / 2
  rect_df$label_color <- ifelse(rect_df$category == "Domains", "black", "white")
  rect_df$label <- rect_df$description

  min_w <- protein_length * 0.05
  for (i in seq_len(nrow(rect_df))) {
    seg_w <- rect_df$xmax[i] - rect_df$xmin[i]
    if (!nzchar(rect_df$label[i]) || seg_w <= min_w) {
      rect_df$label[i] <- ""
      next
    }
    max_chars <- max(3L, as.integer(seg_w / (protein_length * 0.012)))
    if (nchar(rect_df$label[i]) > max_chars) {
      rect_df$label[i] <- paste0(substr(rect_df$label[i], 1, max_chars - 1L), "\u2026")
    }
  }

  rect_df
}

.pr_group_track_id <- function(group_name) {
  paste0("group::", as.character(group_name %||% ""))
}

.pr_fc_track_id <- function(group_a, group_b) {
  paste0("fc::", as.character(group_a %||% ""), "::", as.character(group_b %||% ""))
}

.pr_group_track_label <- function(group_name, log_transform = "none",
                                  format = c("unicode", "expression", "plain")) {
  format <- match.arg(format)
  title <- as.character(group_name %||% "Value")

  if (identical(format, "expression")) {
    if (identical(log_transform, "log10")) {
      return(as.expression(bquote(log[10](.(title)))))
    }
    if (identical(log_transform, "log2")) {
      return(as.expression(bquote(log[2](.(title)))))
    }
    return(title)
  }

  if (identical(format, "unicode")) {
    if (identical(log_transform, "log10")) {
      return(paste0("log\u2081\u2080(", title, ")"))
    }
    if (identical(log_transform, "log2")) {
      return(paste0("log\u2082(", title, ")"))
    }
    return(title)
  }

  if (identical(log_transform, "log10")) return(paste0("log10(", title, ")"))
  if (identical(log_transform, "log2")) return(paste0("log2(", title, ")"))
  title
}

.pr_fc_track_label <- function(group_a, group_b, flip_fc = FALSE,
                               format = c("unicode", "expression", "plain")) {
  format <- match.arg(format)
  denom <- as.character(group_a %||% "Ctrl")
  numer <- as.character(group_b %||% "Treatment")
  if (isTRUE(flip_fc)) {
    tmp <- numer
    numer <- denom
    denom <- tmp
  }

  if (identical(format, "expression")) {
    as.expression(bquote(log[2](.(numer) / .(denom))))
  } else if (identical(format, "unicode")) {
    paste0("log\u2082(", numer, "/", denom, ")")
  } else {
    paste0("log2(", numer, "/", denom, ")")
  }
}

.pr_track_catalog_from_data <- function(data, style = list()) {
  if (is.null(data) || !is.list(data)) return(list())

  flip_fc <- isTRUE(style$flip_fc %||% FALSE)
  value_transform <- as.character(style$value_transform %||% "none")[1]
  catalog <- list()

  append_track <- function(track) {
    catalog[[length(catalog) + 1L]] <<- track
  }

  is_detail <- identical(as.character(data$mode %||% ""), "detail")

  if (is_detail) {
    group_coverages <- data$group_coverages %||% list()
    pairwise <- data$pairwise %||% list()
    comparisons <- data$comparisons %||% list()

    if (length(group_coverages) > 0) {
      group_names <- data$group_names %||% names(group_coverages)
      for (grp in group_names) {
        gc <- group_coverages[[grp]]
        if (is.null(gc) || is.null(gc$coverage)) next
        append_track(list(
          id = .pr_group_track_id(grp),
          type = "group",
          group_name = grp,
          label_plain = .pr_group_track_label(grp, value_transform, format = "plain"),
          label_unicode = .pr_group_track_label(grp, value_transform, format = "unicode"),
          label_expression = .pr_group_track_label(grp, value_transform, format = "expression")
        ))
      }

      for (pk in names(pairwise)) {
        pw <- pairwise[[pk]]
        if (is.null(pw) || is.null(pw$fc_coverage)) next
        append_track(list(
          id = .pr_fc_track_id(pw$group_a, pw$group_b),
          type = "fc",
          group_a = pw$group_a,
          group_b = pw$group_b,
          label_plain = .pr_fc_track_label(pw$group_a, pw$group_b, flip_fc = flip_fc, format = "plain"),
          label_unicode = .pr_fc_track_label(pw$group_a, pw$group_b, flip_fc = flip_fc, format = "unicode"),
          label_expression = .pr_fc_track_label(pw$group_a, pw$group_b, flip_fc = flip_fc, format = "expression")
        ))
      }

      return(catalog)
    }

    if (length(comparisons) > 0 && !is.null(comparisons[[1]]$treatment_group)) {
      first_comp <- comparisons[[1]]
      ctrl_group <- first_comp$ctrl_group %||% "Ctrl"
      append_track(list(
        id = .pr_group_track_id(ctrl_group),
        type = "group",
        group_name = ctrl_group,
        label_plain = .pr_group_track_label(ctrl_group, value_transform, format = "plain"),
        label_unicode = .pr_group_track_label(ctrl_group, value_transform, format = "unicode"),
        label_expression = .pr_group_track_label(ctrl_group, value_transform, format = "expression")
      ))

      seen_groups <- ctrl_group
      for (comp_key in names(comparisons)) {
        comp <- comparisons[[comp_key]]
        if (is.null(comp)) next
        treat_group <- comp$treatment_group %||% "Treatment"
        if (!treat_group %in% seen_groups) {
          seen_groups <- c(seen_groups, treat_group)
          append_track(list(
            id = .pr_group_track_id(treat_group),
            type = "group",
            group_name = treat_group,
            label_plain = .pr_group_track_label(treat_group, value_transform, format = "plain"),
            label_unicode = .pr_group_track_label(treat_group, value_transform, format = "unicode"),
            label_expression = .pr_group_track_label(treat_group, value_transform, format = "expression")
          ))
        }

        append_track(list(
          id = .pr_fc_track_id(comp$ctrl_group, comp$treatment_group),
          type = "fc",
          group_a = comp$ctrl_group,
          group_b = comp$treatment_group,
          label_plain = .pr_fc_track_label(comp$ctrl_group, comp$treatment_group, flip_fc = flip_fc, format = "plain"),
          label_unicode = .pr_fc_track_label(comp$ctrl_group, comp$treatment_group, flip_fc = flip_fc, format = "unicode"),
          label_expression = .pr_fc_track_label(comp$ctrl_group, comp$treatment_group, flip_fc = flip_fc, format = "expression")
        ))
      }
    }

    return(catalog)
  }

  groups <- unique(as.character(data$samples$group_name %||% character(0)))
  groups <- groups[nzchar(groups)]
  if (length(groups) == 0) {
    groups <- names(data$group_colors %||% list())
  }

  for (grp in groups) {
    append_track(list(
      id = .pr_group_track_id(grp),
      type = "group",
      group_name = grp,
      label_plain = .pr_group_track_label(grp, value_transform, format = "plain"),
      label_unicode = .pr_group_track_label(grp, value_transform, format = "unicode"),
      label_expression = .pr_group_track_label(grp, value_transform, format = "expression")
    ))
  }

  comparisons_info <- data$comparisons_info %||% list()
  for (comp_key in names(comparisons_info)) {
    comp <- comparisons_info[[comp_key]]
    if (is.null(comp)) next
    append_track(list(
      id = .pr_fc_track_id(comp$ctrl_group, comp$treatment_group),
      type = "fc",
      group_a = comp$ctrl_group,
      group_b = comp$treatment_group,
      label_plain = .pr_fc_track_label(comp$ctrl_group, comp$treatment_group, flip_fc = flip_fc, format = "plain"),
      label_unicode = .pr_fc_track_label(comp$ctrl_group, comp$treatment_group, flip_fc = flip_fc, format = "unicode"),
      label_expression = .pr_fc_track_label(comp$ctrl_group, comp$treatment_group, flip_fc = flip_fc, format = "expression")
    ))
  }

  catalog
}

.pr_resolve_selected_track_ids <- function(track_catalog, style = list()) {
  track_ids <- vapply(track_catalog, function(x) as.character(x$id %||% ""), character(1))
  if (length(track_ids) == 0) return(character(0))

  if ("visible_tracks" %in% names(style)) {
    raw <- style$visible_tracks
    if (is.null(raw)) return(track_ids)
    raw <- as.character(raw %||% character(0))
    raw <- raw[!is.na(raw)]
    if (any(raw == "__all__")) return(track_ids)
    if (length(raw) == 1 && !nzchar(raw[[1]])) raw <- character(0)
    return(intersect(raw, track_ids))
  }

  # Backward compatibility for older saved views that used visible_groups/show_fc_tracks.
  group_defaults <- vapply(Filter(function(x) identical(as.character(x$type %||% ""), "group"), track_catalog),
                           function(x) as.character(x$group_name %||% ""), character(1))
  visible_raw <- style$visible_groups
  visible_groups <- if (!is.null(visible_raw) && is.character(visible_raw) && any(nzchar(visible_raw))) {
    visible_raw[nzchar(visible_raw)]
  } else {
    group_defaults
  }
  show_fc_tracks <- isTRUE(style$show_fc_tracks %||% TRUE)

  keep <- vapply(track_catalog, function(track) {
    track_type <- as.character(track$type %||% "")
    if (identical(track_type, "group")) {
      as.character(track$group_name %||% "") %in% visible_groups
    } else if (identical(track_type, "fc")) {
      isTRUE(show_fc_tracks) &&
        as.character(track$group_a %||% "") %in% visible_groups &&
        as.character(track$group_b %||% "") %in% visible_groups
    } else {
      FALSE
    }
  }, logical(1))

  track_ids[keep]
}

tb_peptide_region_track_state <- function(results, style = list()) {
  data <- results$data %||% results
  catalog <- .pr_track_catalog_from_data(data, style)
  selected_ids <- .pr_resolve_selected_track_ids(catalog, style)
  selected_tracks <- Filter(function(track) {
    as.character(track$id %||% "") %in% selected_ids
  }, catalog)

  list(
    catalog = catalog,
    selected_ids = selected_ids,
    selected_tracks = selected_tracks
  )
}

.pr_detail_left_margin <- function(axis_text_size = 14) {
  ats <- tb_num(axis_text_size, 14)
  max(64, as.integer(round(40 + ats * 2.2)))
}

.pr_track_y_breaks <- function(y_min, y_max, is_fc = FALSE) {
  y_min <- suppressWarnings(as.numeric(y_min))
  y_max <- suppressWarnings(as.numeric(y_max))
  if (!all(is.finite(c(y_min, y_max)))) {
    return(scales::breaks_pretty(n = 2))
  }

  if (isTRUE(is_fc)) {
    # Pull edge breaks inward so labels don't collide with adjacent stacked tracks.
    inner_max <- signif(y_max * 0.9, 3)
    inner_min <- signif(y_min * 0.9, 3)
    brks <- unique(c(inner_min, 0, inner_max))
    brks[is.finite(brks) & brks >= y_min & brks <= y_max]
  } else {
    scales::breaks_pretty(n = 2)(c(y_min, y_max))
  }
}

.pr_build_detail_header <- function(gene_symbol, uniprot_id, protein_length,
                                    coverage_pct = NULL, title_size = 16,
                                    features_df = NULL) {
  header_title <- paste0(gene_symbol, " (", uniprot_id, ")")
  header_subtitle <- paste0(protein_length, " aa")
  if (!is.null(coverage_pct)) {
    header_subtitle <- paste0(header_subtitle, "  ", coverage_pct, "% covered")
  }

  title_size <- tb_num(title_size, 16)
  # Panel alignment across header/bar/tracks is handled by patchwork's gtable
  # width unification when they are combined into a single ggplot for the
  # interactive viewer — so this helper can stay visually minimal.
  p_title <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.68, label = header_title,
                      hjust = 0.5, vjust = 0.5, fontface = "bold",
                      size = title_size / 3.7, color = "black") +
    ggplot2::annotate("text", x = 0.5, y = 0.30, label = header_subtitle,
                      hjust = 0.5, vjust = 0.5,
                      size = max(title_size - 4, 10) / 3.7, color = "black") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 10, 4, 0))
}

#' Build the interactive protein feature bar used in detail view.
.pr_build_protein_bar_girafe <- function(features_df, protein_length, feature_text_size = 2) {
  tb_require_pkg("ggiraph")

  rect_df <- .pr_feature_rectangles(features_df, protein_length)
  x_lo <- -protein_length * 0.03
  x_hi <- protein_length * 1.03 + 0.5

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0.5, xmax = protein_length + 0.5, ymin = 0, ymax = 1),
      fill = "grey92", color = "black", linewidth = 0.3, inherit.aes = FALSE
    )

  if (nrow(rect_df) > 0) {
    p <- p +
      ggiraph::geom_rect_interactive(
        data = rect_df,
        ggplot2::aes(
          xmin = .data$xmin, xmax = .data$xmax,
          ymin = .data$ymin, ymax = .data$ymax,
          fill = .data$color, alpha = .data$alpha,
          tooltip = .data$tooltip, data_id = .data$data_id
        ),
        color = NA,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_alpha_identity()

    label_df <- rect_df[nzchar(rect_df$label), , drop = FALSE]
    if (nrow(label_df) > 0 && feature_text_size > 0) {
      p <- p +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x = .data$label_x, y = .data$label_y,
                       label = .data$label, color = .data$label_color),
          inherit.aes = FALSE,
          fontface = "bold",
          size = feature_text_size
        ) +
        ggplot2::scale_color_identity()
    }
  }

  p +
    ggplot2::annotate("text", x = -protein_length * 0.015, y = 0.5,
                      label = "N", fontface = "bold.italic", size = 3,
                      hjust = 1, color = "black") +
    ggplot2::annotate("text", x = protein_length + protein_length * 0.015 + 0.5, y = 0.5,
                      label = "C", fontface = "bold.italic", size = 3,
                      hjust = 0, color = "black") +
    ggplot2::scale_x_continuous(limits = c(x_lo, x_hi), expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(-0.02, 1.02), clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 10, 0, 0))
}

tb_peptide_region_detail_components <- function(results, style, interactive_bar = FALSE) {
  tb_require_pkg("patchwork")

  data <- results$data
  if (is.null(data) || !identical(data$mode %||% "", "detail")) return(NULL)
  .pr_build_detail_components(data, style, interactive_bar = interactive_bar)
}

tb_peptide_region_detail_girafe <- function(results, style) {
  tb_require_pkg("ggiraph")
  tb_require_pkg("patchwork")

  comps <- tb_peptide_region_detail_components(results, style, interactive_bar = TRUE)
  if (is.null(comps) || is.null(comps$combined_interactive)) return(NULL)

  width_svg  <- tb_num(style$width,  7)
  height_svg <- tb_num(style$height, 5)

  ggiraph::girafe(
    ggobj = comps$combined_interactive,
    width_svg = width_svg,
    height_svg = height_svg,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 0.85),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:1px;"),
      ggiraph::opts_tooltip(css = paste(
        "background-color: rgba(30,30,30,0.96);",
        "color: #FFFFFF;",
        "padding: 8px 10px;",
        "border-radius: 4px;",
        "font-size: 12px;"
      ))
    )
  )
}

# ---- Peptide Region: Detail Mode ----
.pr_build_detail_components <- function(data, style, interactive_bar = FALSE) {
  tb_require_pkg("patchwork")

  prot <- data$protein
  if (is.null(prot)) stop("peptide_region detail: protein data missing")

  # ---- Color scheme ----
  # Two modes: "group" uses input-defined per-group colors, "manual" uses user-set
  # value/FC track colors. Backwards compat: legacy fields ctrl_color/treatment_color
  # /fc_color still honored if present.
  color_mode_pr     <- as.character(style$color_mode %||% "group")[1]
  manual_value_color <- as.character(style$manual_value_color %||% "#CCCCCC")[1]
  manual_fc_color    <- as.character(style$manual_fc_color    %||% "#FFCC99")[1]
  # Legacy color overrides (only used if explicitly set)
  legacy_ctrl       <- style$ctrl_color
  legacy_treatment  <- style$treatment_color
  legacy_fc         <- style$fc_color

  title_text_size   <- tb_num(style$title_text_size, 16)
  axis_text_size    <- tb_num(style$axis_text_size, 14)
  track_alpha       <- tb_num(style$track_alpha, 0.85)
  show_markers      <- isTRUE(style$show_peptide_markers %||% TRUE)
  feature_text_size <- tb_num(style$feature_text_size, 2)
  feature_track_height <- tb_num(style$feature_track_height, 1.2)
  flip_fc           <- isTRUE(style$flip_fc %||% FALSE)
  show_significance <- isTRUE(style$show_significance %||% TRUE)
  sig_display       <- as.character(style$sig_display %||% "stars")[1]
  sig_threshold     <- tb_num(style$sig_threshold, 0.05)
  value_transform   <- as.character(style$value_transform %||% "none")[1]
  value_y_range     <- as.character(style$value_y_range %||% "auto")[1]
  value_y_max       <- tb_num(style$value_y_max, 10)
  fc_y_range        <- as.character(style$fc_y_range %||% "auto")[1]
  fc_y_abs          <- tb_num(style$fc_y_abs, 3)

  n_groups         <- data$n_groups %||% 1
  gene_symbol      <- prot$gene_symbol %||% prot$uniprot_id
  uniprot_id       <- prot$uniprot_id
  protein_length   <- prot$seq_length
  group_coverages  <- data$group_coverages %||% list()
  pairwise         <- data$pairwise %||% list()
  all_group_names  <- data$group_names %||% names(group_coverages)
  group_colors_map <- data$group_colors %||% list()
  track_state      <- tb_peptide_region_track_state(data, style)
  selected_tracks  <- track_state$selected_tracks %||% list()

  pr_lookup_group_coverage <- function(group_name) {
    group_name <- as.character(group_name %||% "")
    if (!nzchar(group_name)) return(NULL)

    gc <- group_coverages[[group_name]]
    if (!is.null(gc)) return(gc)

    for (item in group_coverages) {
      if (identical(as.character(item$group_name %||% ""), group_name)) {
        return(item)
      }
    }

    NULL
  }

  # Helper: resolve a value-track fill color for a given group
  pr_value_color <- function(group_name) {
    if (identical(color_mode_pr, "manual")) return(manual_value_color)
    # group mode: try input-defined color, fall back to a stable palette by index
    gc_color <- group_colors_map[[group_name]]
    if (!is.null(gc_color) && nzchar(gc_color)) return(gc_color)
    palette_fallback <- c("#2166AC", "#B2182B", "#4DAF4A", "#FF7F00",
                          "#984EA3", "#E41A1C", "#377EB8", "#A65628")
    idx <- match(group_name, all_group_names)
    if (is.na(idx)) idx <- 1L
    palette_fallback[((idx - 1L) %% length(palette_fallback)) + 1L]
  }

  # Helper: FC track color (single color in either mode by design)
  pr_fc_color <- function(comp_idx = 1L) {
    if (identical(color_mode_pr, "manual")) return(manual_fc_color)
    if (!is.null(legacy_fc) && nzchar(as.character(legacy_fc))) return(as.character(legacy_fc)[1])
    manual_fc_color
  }

  # Backward compat: old data stored as comparisons
  comparisons <- data$comparisons %||% list()

  has_new_format <- length(group_coverages) > 0

  if (!isTRUE(prot$has_coverage) ||
      (length(group_coverages) == 0 && length(comparisons) == 0)) {
    p_msg <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = paste0("No peptides mapped for ", gene_symbol,
                                       " (", uniprot_id, ")"),
                        size = 6, color = "black") +
      ggplot2::theme_void()
    return(list(
      header = NULL,
      bar_static = NULL,
      bar_interactive = NULL,
      tracks = p_msg,
      combined_static = p_msg,
      combined_interactive = p_msg,
      tables = list(),
      layout_weights = list(header = 0.9, bar = feature_track_height, tracks = 1.3)
    ))
  }

  # Peptide marker data (with tryptic classification)
  marker_df <- NULL
  if (show_markers && !is.null(prot$peptide_positions) && nrow(prot$peptide_positions) > 0) {
    marker_df <- data.frame(
      xmin = prot$peptide_positions$prot_start - 0.5,
      xmax = prot$peptide_positions$prot_end + 0.5,
      tryptic_class = if (!is.null(prot$peptide_positions$tryptic_class))
                        prot$peptide_positions$tryptic_class
                      else "fully_tryptic",
      stringsAsFactors = FALSE
    )
    marker_df <- unique(marker_df)
  }

  tables_out <- list()
  cov_pct <- NULL
  track_plots <- list()
  track_heights <- numeric(0)

  if (has_new_format) {
    # ---- New per-group data model ----
    selected_group_names <- unique(vapply(
      Filter(function(track) identical(as.character(track$type %||% ""), "group"), selected_tracks),
      function(track) as.character(track$group_name %||% ""),
      character(1)
    ))
    selected_group_names <- selected_group_names[nzchar(selected_group_names)]
    coverage_group_candidates <- unique(c(selected_group_names, all_group_names))
    first_gc <- NULL
    for (grp in coverage_group_candidates) {
      first_gc <- pr_lookup_group_coverage(grp)
      if (!is.null(first_gc) && !is.null(first_gc$coverage)) break
    }
    if (is.null(first_gc) && length(group_coverages) > 0) {
      first_gc <- group_coverages[[1]]
    }
    if (!is.null(first_gc) && !is.null(first_gc$coverage)) {
      first_col <- if ("group_mean" %in% names(first_gc$coverage)) "group_mean"
                   else names(first_gc$coverage)[2]
      n_covered <- sum(!is.na(first_gc$coverage[[first_col]]))
      cov_pct   <- round(100 * n_covered / protein_length, 1)
    }

    pairwise_by_id <- list()
    for (pk in names(pairwise)) {
      pw <- pairwise[[pk]]
      if (is.null(pw)) next
      pairwise_by_id[[.pr_fc_track_id(pw$group_a, pw$group_b)]] <- pw
    }

    all_stats <- list()
    for (ti in seq_along(selected_tracks)) {
      track <- selected_tracks[[ti]]
      track_type <- as.character(track$type %||% "")
      is_last <- ti == length(selected_tracks)

      if (identical(track_type, "group")) {
        grp <- as.character(track$group_name %||% "")
        gc <- pr_lookup_group_coverage(grp)
        if (is.null(gc) || is.null(gc$coverage)) next
        group_col <- if ("group_mean" %in% names(gc$coverage)) "group_mean" else names(gc$coverage)[2]

        track_plots[[paste0("grp_", ti)]] <- .pr_build_track(
          gc$coverage, group_col, pr_value_color(grp), grp,
          protein_length, axis_text_size, show_x_axis = is_last,
          alpha = track_alpha, marker_df = marker_df,
          log_transform = value_transform,
          manual_y_max = if (identical(value_y_range, "manual")) value_y_max else NULL
        )
        track_heights <- c(track_heights, 1.3)

        ps <- gc$peptide_stats
        if (!is.null(ps) && nrow(ps) > 0) {
          ps$group <- grp
          all_stats[[grp]] <- ps
        }
      } else if (identical(track_type, "fc")) {
        pw <- pairwise_by_id[[as.character(track$id %||% "")]]
        if (is.null(pw) || is.null(pw$fc_coverage)) next

        track_plots[[paste0("fc_", ti)]] <- .pr_build_track(
          pw$fc_coverage, "fc", pr_fc_color(ti), NULL,
          protein_length, axis_text_size, show_x_axis = is_last,
          alpha = track_alpha, is_fc = TRUE, flip_fc = flip_fc,
          ctrl_group = pw$group_a, treatment_group = pw$group_b,
          manual_y_max = if (identical(fc_y_range, "manual")) fc_y_abs else NULL,
          show_significance = show_significance && isTRUE(pw$has_statistics),
          sig_display = sig_display, sig_threshold = sig_threshold
        )
        track_heights <- c(track_heights, 1.3)
      }
    }

    if (length(all_stats) > 0) {
      tables_out[["peptide_stats"]] <- do.call(rbind, all_stats)
    }
  } else if (n_groups >= 2 && length(comparisons) > 0 &&
             !is.null(comparisons[[1]]$treatment_group)) {
    # ---- Legacy pairwise format (backward compat) ----
    comp_keys <- names(comparisons)
    first_comp <- comparisons[[comp_keys[1]]]
    group_specs <- list()
    fc_specs <- list()

    ctrl_group <- first_comp$ctrl_group %||% "Ctrl"
    group_specs[[.pr_group_track_id(ctrl_group)]] <- list(
      coverage = first_comp$coverage,
      col_name = if ("ctrl_mean" %in% names(first_comp$coverage)) "ctrl_mean" else names(first_comp$coverage)[2],
      group_name = ctrl_group
    )

    for (comp_key in comp_keys) {
      comp <- comparisons[[comp_key]]
      if (is.null(comp)) next

      treat_group <- comp$treatment_group %||% "Treatment"
      treat_id <- .pr_group_track_id(treat_group)
      if (is.null(group_specs[[treat_id]])) {
        group_specs[[treat_id]] <- list(
          coverage = comp$coverage,
          col_name = if ("treatment_mean" %in% names(comp$coverage)) "treatment_mean" else names(comp$coverage)[2],
          group_name = treat_group
        )
      }

      fc_specs[[.pr_fc_track_id(comp$ctrl_group, comp$treatment_group)]] <- comp
    }

    first_group_track <- Filter(function(track) identical(as.character(track$type %||% ""), "group"), selected_tracks)
    first_group_track <- if (length(first_group_track) > 0) first_group_track[[1]] else NULL
    first_group_spec <- if (!is.null(first_group_track)) {
      group_specs[[as.character(first_group_track$id %||% "")]]
    } else {
      group_specs[[1]]
    }
    if (!is.null(first_group_spec) && !is.null(first_group_spec$coverage)) {
      first_col <- first_group_spec$col_name
      n_covered <- sum(!is.na(first_group_spec$coverage[[first_col]]))
      cov_pct   <- round(100 * n_covered / protein_length, 1)
    }

    for (ti in seq_along(selected_tracks)) {
      track <- selected_tracks[[ti]]
      track_type <- as.character(track$type %||% "")
      is_last <- ti == length(selected_tracks)

      if (identical(track_type, "group")) {
        spec <- group_specs[[as.character(track$id %||% "")]]
        if (is.null(spec) || is.null(spec$coverage) || is.null(spec$col_name)) next

        track_plots[[paste0("grp_", ti)]] <- .pr_build_track(
          spec$coverage, spec$col_name, pr_value_color(spec$group_name), spec$group_name,
          protein_length, axis_text_size, show_x_axis = is_last,
          alpha = track_alpha, marker_df = marker_df,
          log_transform = value_transform,
          manual_y_max = if (identical(value_y_range, "manual")) value_y_max else NULL
        )
        track_heights <- c(track_heights, 1.3)
      } else if (identical(track_type, "fc")) {
        comp <- fc_specs[[as.character(track$id %||% "")]]
        if (is.null(comp) || is.null(comp$coverage)) next

        track_plots[[paste0("fc_", ti)]] <- .pr_build_track(
          comp$coverage, "fc", pr_fc_color(ti), NULL,
          protein_length, axis_text_size, show_x_axis = is_last,
          alpha = track_alpha, is_fc = TRUE, flip_fc = flip_fc,
          ctrl_group = comp$ctrl_group, treatment_group = comp$treatment_group,
          manual_y_max = if (identical(fc_y_range, "manual")) fc_y_abs else NULL
        )
        track_heights <- c(track_heights, 1.3)
      }
    }

    if (!is.null(first_comp$peptide_stats) && length(selected_tracks) > 0) {
      tables_out[["peptide_stats"]] <- first_comp$peptide_stats
    }
  } else {
    # ---- Single-group (from new or old format) ----
    gc <- if (has_new_format) group_coverages[[1]]
          else comparisons[[1]]
    coverage <- gc$coverage
    grp_name <- gc$group_name %||% gc$ctrl_group %||% "Value"

    mean_col <- if ("group_mean" %in% names(coverage)) "group_mean"
                else if ("ctrl_mean" %in% names(coverage)) "ctrl_mean"
                else names(coverage)[2]
    n_covered <- sum(!is.na(coverage[[mean_col]]))
    cov_pct   <- round(100 * n_covered / protein_length, 1)

    p_val <- .pr_build_track(
      coverage, mean_col, pr_value_color(grp_name), grp_name,
      protein_length, axis_text_size, alpha = track_alpha,
      marker_df = marker_df,
      log_transform = value_transform,
      manual_y_max = if (identical(value_y_range, "manual")) value_y_max else NULL
    )
    cv_col <- if ("group_cv" %in% names(coverage)) "group_cv"
              else if ("ctrl_cv" %in% names(coverage)) "ctrl_cv"
              else NULL
    p_cv <- if (!is.null(cv_col)) {
      .pr_build_track(
        coverage, cv_col, "#E69F00", "CV",
        protein_length, axis_text_size, alpha = 0.75,
        marker_df = marker_df
      )
    } else {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "CV requires > 1 replicate",
                          size = 4, color = "black") +
        ggplot2::theme_void()
    }
    p_depth <- .pr_build_track(
      coverage, "depth", "#999999", "Depth",
      protein_length, axis_text_size, show_x_axis = TRUE, alpha = 0.7
    )
    track_plots <- list(value = p_val, cv = p_cv, depth = p_depth)
    track_heights <- c(1.3, 1.3, 1.3)

    if (!is.null(gc$peptide_stats)) {
      tables_out[["peptide_stats"]] <- gc$peptide_stats
    }
  }

  header_plot <- .pr_build_detail_header(
    gene_symbol = gene_symbol,
    uniprot_id = uniprot_id,
    protein_length = protein_length,
    coverage_pct = cov_pct,
    title_size = title_text_size,
    features_df = prot$features
  )
  bar_static <- .pr_build_protein_bar(
    features_df = prot$features,
    protein_length = protein_length,
    feature_text_size = feature_text_size
  )
  bar_interactive <- if (isTRUE(interactive_bar)) {
    .pr_build_protein_bar_girafe(
      features_df = prot$features,
      protein_length = protein_length,
      feature_text_size = feature_text_size
    )
  } else {
    NULL
  }

  if (length(track_plots) == 0) {
    tracks_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "All data tracks hidden",
                        size = 4, color = "black") +
      ggplot2::theme_void()
    track_weight <- 1.3
  } else if (length(track_plots) == 1) {
    tracks_plot <- track_plots[[1]]
    track_weight <- track_heights[[1]]
  } else {
    tracks_plot <- patchwork::wrap_plots(track_plots, ncol = 1, heights = track_heights)
    track_weight <- sum(track_heights)
  }

  combined_static <- (header_plot / bar_static / tracks_plot) +
    patchwork::plot_layout(heights = c(0.9, feature_track_height, track_weight))

  # Combined interactive version for the live viewer. patchwork auto-aligns
  # panel regions across subplots (using gtable widths), which is how we
  # guarantee that the header, feature bar, and data tracks share the same
  # x-axis. This cannot be achieved across three independent Shiny outputs.
  combined_interactive <- if (!is.null(bar_interactive)) {
    (header_plot / bar_interactive / tracks_plot) +
      patchwork::plot_layout(heights = c(0.9, feature_track_height, track_weight))
  } else NULL

  list(
    header = header_plot,
    bar_static = bar_static,
    bar_interactive = bar_interactive,
    tracks = tracks_plot,
    combined_static = combined_static,
    combined_interactive = combined_interactive,
    tables = tables_out,
    layout_weights = list(header = 0.9, bar = feature_track_height, tracks = track_weight)
  )
}

.pr_render_detail <- function(data, style) {
  comps <- .pr_build_detail_components(data, style, interactive_bar = FALSE)
  list(
    plots = list(peptide_region = comps$combined_static),
    tables = comps$tables,
    tabs = NULL,
    components = list(peptide_region_detail = comps)
  )
}

# ---- Peptide Region: Multi-layer Protein Feature Bar ----
.pr_build_protein_bar_legacy <- function(features_df, protein_length, gene_symbol,
                                         uniprot_id, title_size = 16,
                                         coverage_pct = NULL, feature_text_size = 2) {
  # Feature category definitions: type → (category, color, y_min, y_max)
  # Domains get their own track row at the top (0.75–1.0), other categories share
  # the lower portion as thin strips at different y-heights
  category_map <- list(
    # Domains: own track row at top, light fill
    "Domain"           = list(cat = "Domains",      color = "#4E79A7", ymin = 0.75, ymax = 1.0, alpha = 0.45),
    "Repeat"           = list(cat = "Domains",      color = "#4E79A7", ymin = 0.75, ymax = 1.0, alpha = 0.40),
    "Region"           = list(cat = "Domains",      color = "#76B7B2", ymin = 0.75, ymax = 1.0, alpha = 0.35),
    "Motif"            = list(cat = "Domains",      color = "#59A14F", ymin = 0.75, ymax = 1.0, alpha = 0.40),
    "Coiled coil"      = list(cat = "Domains",      color = "#EDC948", ymin = 0.75, ymax = 1.0, alpha = 0.40),
    # Topology
    "Transmembrane"    = list(cat = "Topology",     color = "#E15759", ymin = 0.56, ymax = 0.72, alpha = 0.85),
    "Topological domain" = list(cat = "Topology",   color = "#F28E2B", ymin = 0.56, ymax = 0.72, alpha = 0.55),
    "Intramembrane"    = list(cat = "Topology",     color = "#B07AA1", ymin = 0.56, ymax = 0.72, alpha = 0.85),
    # Processing
    "Signal"           = list(cat = "Processing",   color = "#FF9DA7", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Transit peptide"  = list(cat = "Processing",   color = "#FF9DA7", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Propeptide"       = list(cat = "Processing",   color = "#FFBE7D", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Chain"            = list(cat = "Processing",   color = "#D4A6C8", ymin = 0.39, ymax = 0.53, alpha = 0.50),
    "Peptide"          = list(cat = "Processing",   color = "#D4A6C8", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    "Initiator methionine" = list(cat = "Processing", color = "#FF9DA7", ymin = 0.39, ymax = 0.53, alpha = 0.80),
    # Binding / Sites
    "Active site"      = list(cat = "Binding",      color = "#E15759", ymin = 0.22, ymax = 0.36, alpha = 0.90),
    "Binding site"     = list(cat = "Binding",      color = "#F28E2B", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Site"             = list(cat = "Binding",      color = "#86BCB6", ymin = 0.22, ymax = 0.36, alpha = 0.80),
    "DNA binding"      = list(cat = "Binding",      color = "#499894", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Nucleotide binding" = list(cat = "Binding",    color = "#59A14F", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Metal binding"    = list(cat = "Binding",      color = "#B6992D", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Calcium binding"  = list(cat = "Binding",      color = "#B6992D", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    "Zinc finger"      = list(cat = "Binding",      color = "#B6992D", ymin = 0.22, ymax = 0.36, alpha = 0.85),
    # Modifications
    "Modified residue" = list(cat = "Modification", color = "#B07AA1", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Disulfide bond"   = list(cat = "Modification", color = "#E15759", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Glycosylation"    = list(cat = "Modification", color = "#59A14F", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Lipidation"       = list(cat = "Modification", color = "#EDC948", ymin = 0.05, ymax = 0.19, alpha = 0.80),
    "Cross-link"       = list(cat = "Modification", color = "#FF9DA7", ymin = 0.05, ymax = 0.19, alpha = 0.80)
  )

  # Use same x-limits as coverage tracks so everything aligns
  x_lo <- -protein_length * 0.03
  x_hi <- protein_length * 1.03 + 0.5

  p <- ggplot2::ggplot() +
    # Background bar
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.5, xmax = protein_length + 0.5,
                                     ymin = 0, ymax = 1),
                       fill = "grey92", color = "black", linewidth = 0.3)

  site_width <- max(2, protein_length * 0.006)
  fts <- feature_text_size

  for (i in seq_len(nrow(features_df))) {
    ft_type <- features_df$type[i]
    mapping <- category_map[[ft_type]]
    if (is.null(mapping)) next

    s <- features_df$start[i]
    e <- features_df$end[i]
    if (is.na(s) || is.na(e)) next

    xmin <- if (s == e) s - site_width else s - 0.5
    xmax <- if (s == e) e + site_width else e + 0.5

    p <- p +
      ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
                        ymin = mapping$ymin, ymax = mapping$ymax,
                        fill = mapping$color, alpha = mapping$alpha,
                        color = NA)

    # Text labels inside features (if wide enough to fit)
    seg_w <- xmax - xmin
    label_y <- (mapping$ymin + mapping$ymax) / 2
    min_w <- protein_length * 0.05

    if (seg_w > min_w && fts > 0) {
      label <- features_df$description[i]
      if (!nzchar(label)) label <- ft_type
      if (nzchar(label)) {
        max_chars <- max(3L, as.integer(seg_w / (protein_length * 0.012)))
        if (nchar(label) > max_chars) label <- paste0(substr(label, 1, max_chars - 1), "\u2026")
        # Domain row: dark text on light fill; thin strips: white text
        is_domain <- identical(mapping$cat, "Domains")
        lbl_color <- if (is_domain) "black" else "white"
        p <- p +
          ggplot2::annotate("text", x = (xmin + xmax) / 2, y = label_y,
                            label = label, size = fts, color = lbl_color,
                            fontface = "bold")
      }
    }
  }

  # N/C terminals
  p <- p +
    ggplot2::annotate("text", x = -protein_length * 0.015, y = 0.5,
                      label = "N", fontface = "bold.italic", size = 3,
                      hjust = 1, color = "black") +
    ggplot2::annotate("text", x = protein_length + protein_length * 0.015 + 0.5, y = 0.5,
                      label = "C", fontface = "bold.italic", size = 3,
                      hjust = 0, color = "black")

  # Title + subtitle
  title_text <- paste0(gene_symbol, "  (", uniprot_id, ")")
  sub_parts <- paste0(protein_length, " aa")
  if (!is.null(coverage_pct)) sub_parts <- paste0(sub_parts, "  \u2022  ", coverage_pct, "% covered")

  p <- p +
    ggplot2::labs(title = title_text, subtitle = sub_parts) +
    ggplot2::scale_x_continuous(
      limits = c(x_lo, x_hi),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.02, 1.02), clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5,
                                             size = title_size,
                                             margin = ggplot2::margin(b = 1)),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = title_size - 4,
                                             color = "black",
                                             margin = ggplot2::margin(b = 3)),
      plot.margin   = ggplot2::margin(4, 10, 0, 10)
    )

  p
}

# Override the legacy title-bearing bar with a bare feature bar so detail mode
# can place the header and legend outside the plotting area.
.pr_build_protein_bar <- function(features_df, protein_length, feature_text_size = 2) {
  rect_df <- .pr_feature_rectangles(features_df, protein_length)
  x_lo <- -protein_length * 0.03
  x_hi <- protein_length * 1.03 + 0.5

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0.5, xmax = protein_length + 0.5, ymin = 0, ymax = 1),
      fill = "grey92", color = "black", linewidth = 0.3, inherit.aes = FALSE
    )

  if (nrow(rect_df) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = rect_df,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                     ymin = .data$ymin, ymax = .data$ymax,
                     fill = .data$color, alpha = .data$alpha),
        color = NA,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_alpha_identity()

    label_df <- rect_df[nzchar(rect_df$label), , drop = FALSE]
    if (nrow(label_df) > 0 && feature_text_size > 0) {
      p <- p +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x = .data$label_x, y = .data$label_y,
                       label = .data$label, color = .data$label_color),
          inherit.aes = FALSE,
          fontface = "bold",
          size = feature_text_size
        ) +
        ggplot2::scale_color_identity()
    }
  }

  p +
    ggplot2::annotate("text", x = -protein_length * 0.015, y = 0.5,
                      label = "N", fontface = "bold.italic", size = 3,
                      hjust = 1, color = "black") +
    ggplot2::annotate("text", x = protein_length + protein_length * 0.015 + 0.5, y = 0.5,
                      label = "C", fontface = "bold.italic", size = 3,
                      hjust = 0, color = "black") +
    ggplot2::scale_x_continuous(limits = c(x_lo, x_hi), expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(-0.02, 1.02), clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 10, 0, 0))
}

# ---- Peptide Region: Coverage track ----
.pr_build_track <- function(coverage, col_name, fill_color, y_label,
                             protein_length, axis_text_size = 14,
                             show_x_axis = FALSE, alpha = 0.85,
                             is_fc = FALSE, flip_fc = FALSE, marker_df = NULL,
                             ctrl_group = "Ctrl", treatment_group = "Treatment",
                             log_transform = "none", manual_y_max = NULL,
                             show_significance = FALSE, sig_display = "stars",
                             sig_threshold = 0.05) {
  vals <- coverage[[col_name]]
  if (is.null(vals)) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = paste("No data for", col_name),
                               size = 4, color = "black") +
             ggplot2::theme_void())
  }

  covered <- coverage[!is.na(vals), , drop = FALSE]
  covered$val <- vals[!is.na(vals)]

  if (nrow(covered) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = paste("No data for", col_name),
                               size = 4, color = "black") +
             ggplot2::theme_void())
  }

  if (is_fc) {
    covered$val <- log2(covered$val)
    if (flip_fc) covered$val <- -covered$val
    covered$val[!is.finite(covered$val)] <- NA_real_
    covered <- covered[!is.na(covered$val), , drop = FALSE]
    if (nrow(covered) == 0) {
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0.5, y = 0.5,
                                 label = "No finite fold-change values",
                                 size = 4, color = "black") +
               ggplot2::theme_void())
    }
    if (!is.null(manual_y_max)) {
      y_max <- abs(manual_y_max)
    } else {
      y_max <- max(abs(covered$val), na.rm = TRUE) * 1.2
    }
    y_min <- -y_max
    y_label <- .pr_fc_track_label(ctrl_group, treatment_group,
                                  flip_fc = flip_fc, format = "expression")
  } else {
    # Apply log transform to value tracks
    log_transform <- log_transform %||% "none"
    if (identical(log_transform, "log2")) {
      covered$val <- log2(covered$val)
      covered$val[!is.finite(covered$val)] <- NA_real_
      covered <- covered[!is.na(covered$val), , drop = FALSE]
    } else if (identical(log_transform, "log10")) {
      covered$val <- log10(covered$val)
      covered$val[!is.finite(covered$val)] <- NA_real_
      covered <- covered[!is.na(covered$val), , drop = FALSE]
    }

    if (!is.null(y_label)) {
      y_label <- .pr_group_track_label(y_label, log_transform, format = "expression")
    }

    if (nrow(covered) == 0) {
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0.5, y = 0.5,
                                 label = "No finite values after transform",
                                 size = 4, color = "black") +
               ggplot2::theme_void())
    }

    if (!is.null(manual_y_max)) {
      y_max <- manual_y_max
    } else {
      y_max <- max(covered$val, na.rm = TRUE) * 1.15
    }
    y_min <- if (any(covered$val < 0, na.rm = TRUE)) min(covered$val, na.rm = TRUE) * 1.15 else 0
  }

  ats <- tb_num(axis_text_size, 14)
  y_breaks <- .pr_track_y_breaks(y_min, y_max, is_fc = is_fc)

  p <- ggplot2::ggplot(covered, ggplot2::aes(x = .data$residue, y = .data$val))

  # Peptide markers (tryptic-class-dependent styling)
  if (!is.null(marker_df) && nrow(marker_df) > 0) {
    if (!"tryptic_class" %in% names(marker_df)) {
      marker_df$tryptic_class <- "fully_tryptic"
    }
    tc_styles <- list(
      fully_tryptic = list(fill = "#000000", alpha = 0.03),
      semi_tryptic  = list(fill = "#E69F00", alpha = 0.08),
      non_tryptic   = list(fill = "#D55E00", alpha = 0.14)
    )
    for (tc in unique(marker_df$tryptic_class)) {
      sub_df <- marker_df[marker_df$tryptic_class == tc, , drop = FALSE]
      sty <- tc_styles[[tc]] %||% tc_styles$fully_tryptic
      p <- p +
        ggplot2::geom_rect(
          data = sub_df,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE, fill = sty$fill, alpha = sty$alpha
        )
    }
  }

  p <- p +
    ggplot2::geom_col(width = 1, fill = fill_color, color = NA, alpha = alpha)

  if (is_fc) {
    if (y_max > 1) {
      p <- p + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1,
                                  fill = "grey90", alpha = 0.25)
    }
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "black")

    # Significance annotations on FC track
    if (show_significance && "min_pvalue" %in% names(coverage)) {
      sig_vals <- coverage$min_pvalue
      sig_pos  <- which(!is.na(sig_vals) & sig_vals < sig_threshold)
      if (length(sig_pos) > 0) {
        # Group into contiguous runs
        breaks <- which(diff(sig_pos) > 1)
        run_starts <- sig_pos[c(1, breaks + 1)]
        run_ends   <- sig_pos[c(breaks, length(sig_pos))]

        sig_annot <- data.frame(
          x = (run_starts + run_ends) / 2,
          stringsAsFactors = FALSE
        )
        run_min_p <- vapply(seq_along(run_starts), function(ri) {
          min(sig_vals[run_starts[ri]:run_ends[ri]], na.rm = TRUE)
        }, numeric(1))

        if (identical(sig_display, "pvalue")) {
          sig_annot$label <- format(signif(run_min_p, 2), scientific = TRUE)
        } else {
          sig_annot$label <- ifelse(run_min_p < 0.001, "***",
                             ifelse(run_min_p < 0.01, "**", "*"))
        }

        p <- p +
          ggplot2::geom_text(
            data = sig_annot,
            ggplot2::aes(x = .data$x, y = y_max * 0.85, label = .data$label),
            inherit.aes = FALSE, size = 3, color = "black", fontface = "bold"
          )
      }
    }
  }

  p <- p +
    ggplot2::labs(y = y_label) +
    ggplot2::scale_x_continuous(
      limits = c(-protein_length * 0.03, protein_length * 1.03 + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = scales::label_number(trim = TRUE),
      guide = ggplot2::guide_axis(check.overlap = TRUE)
    ) +
    ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey93", linetype = "dashed",
                                                  linewidth = 0.25),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.line          = ggplot2::element_line(color = "black", linewidth = 0.25),
      axis.ticks         = ggplot2::element_line(color = "black", linewidth = 0.25),
      axis.title.x       = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      # Horizontal Y axis title (angle = 0) so multi-character group labels stay readable.
      # vjust = 0.5 keeps it vertically centered against the plot panel.
      axis.title.y       = ggplot2::element_text(size = max(ats - 4, 8), color = "black",
                                                  angle = 0, hjust = 1,
                                                  vjust = 0.5,
                                                  margin = ggplot2::margin(r = 10)),
      axis.text.y        = ggplot2::element_text(size = max(ats - 6, 7), color = "black",
                                                  lineheight = 0.9),
      plot.margin        = ggplot2::margin(8, 10, 8, 34)
    )

  if (show_x_axis) {
    p <- p +
      ggplot2::scale_x_continuous(
        name = "Residue",
        limits = c(-protein_length * 0.03, protein_length * 1.03 + 0.5),
        expand = c(0, 0),
        breaks = scales::breaks_pretty(n = 5),
        guide = ggplot2::guide_axis(check.overlap = TRUE)
      ) +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(size = max(ats - 6, 7), color = "black"),
        axis.ticks.x = ggplot2::element_line(color = "black", linewidth = 0.25),
        axis.title.x = ggplot2::element_text(size = max(ats - 4, 8), color = "black",
                                              margin = ggplot2::margin(t = 3)),
        plot.margin  = ggplot2::margin(8, 10, 8, 34)
      )
  }

  p
}

# ---- Dispatcher --------------------------------------------------------------

terpbook_render_node <- function(engine_id, results, effective_state, registry = NULL, node_meta = NULL) {
  tb_require_pkg("ggplot2")

  engine_id <- tolower(as.character(engine_id %||% results$engine_id %||% ""))
  if (exists("migrate_legacy_engine_name", mode = "function")) {
    engine_id <- tolower(migrate_legacy_engine_name(engine_id))
  }
  style <- effective_state$style %||% list()

  # Build meta object for render functions
  # Include node_meta fields (like analysis_level) at top level for easy access
  meta  <- list(
    plotly = effective_state$plotly %||% list(),
    visibility = effective_state$visibility %||% list(),
    meta = effective_state$meta %||% list()
  )

  # Merge node_meta fields into meta for render functions to access
  if (!is.null(node_meta) && is.list(node_meta)) {
    meta$analysis_level <- node_meta$analysis_level %||% NULL
    meta$output_level <- node_meta$output_level %||% NULL
    meta$parent_engine_id <- node_meta$parent_engine_id %||% NULL
    meta$data_level <- node_meta$analysis_level %||% NULL  # alias for convenience
  }

  if (is.null(results) || !is.list(results)) {
    stop("results.rds is missing or not a list for engine: ", engine_id)
  }

  # Get engine definition from registry
  engine_def <- NULL
  if (!is.null(registry) && is.list(registry$engines)) {
    engine_def <- registry$engines[[engine_id]]
  }
  # If no registry or engine not found, create minimal definition
  if (is.null(engine_def)) {
    engine_def <- list(
      engine_id = engine_id,
      render_spec = list(plots = c("main"), tables = character(0), tabs = NULL)
    )
  }

  # Dispatch to engine-specific renderer
  out <- switch(
    engine_id,
    "dataprocessor" = tb_render_dataprocessor(results, style, meta),
    "half_life" = tb_render_half_life(results, style, meta),
    "peptide_aggregate_to_protein" = tb_render_peptide_aggregate_to_protein(results, style, meta),
    "idquant"  = tb_render_idquant(results, style, meta),
    "idquant_id_quant" = tb_render_idquant_id_quant(results, style, meta),
    "idquant_average_value" = tb_render_idquant_average_value(results, style, meta),
    "idquant_group" = tb_render_idquant_group(results, style, meta),
    "idquant_replicate" = tb_render_idquant_replicate(results, style, meta),
    "idquant_cv_scatter" = tb_render_idquant_cv_scatter(results, style, meta),
    "idquant_cv_bar" = tb_render_idquant_cv_bar(results, style, meta),
    "idquant_overlap" = tb_render_idquant_overlap(results, style, meta),
    "idquant_overlap_detected" = tb_render_idquant_overlap(results, style, meta),
    "idquant_overlap_quantified" = tb_render_idquant_overlap(results, style, meta),
    "scatter_correlation" = tb_render_scatter_correlation(results, style, meta),
    "spearman" = tb_render_scatter_correlation(results, style, meta),  # Legacy alias
    "hor_dis"  = tb_render_hor_dis(results, style, meta),
    "vert_dis" = tb_render_vert_dis(results, style, meta),
    "dsilac_isotope_density_peptide" = tb_render_dsilac_isotope_density_peptide(results, style, meta),
    "dsilac_isotope_density_protein" = tb_render_dsilac_isotope_density_protein(results, style, meta),
    "dsilac_ratio_box_peptide" = tb_render_dsilac_ratio_box_peptide(results, style, meta),
    "dsilac_ratio_box_protein" = tb_render_dsilac_ratio_box_protein(results, style, meta),
    "pca"      = tb_render_pca(results, style, meta),
    "umap_gene" = tb_render_umap_gene(results, style, meta),
    "heatmap"  = tb_render_heatmap(results, style, meta),
    "ftest_heatmap" = tb_render_ftest_heatmap(results, style, meta),
    "fc_heatmap" = tb_render_fc_heatmap(results, style, meta),
    "pathway_fc_heatmap" = tb_render_pathway_fc_heatmap(results, style, meta),
    "fc_ftest_heatmap" = tb_render_fc_ftest_heatmap(results, style, meta),
    "gene_barchart" = tb_render_gene_barchart(results, style, meta),
    "volcano"  = tb_render_volcano(results, style, meta),
    "ma_plot"  = tb_render_ma_plot(results, style, meta),
    "rankplot" = tb_render_rankplot(results, style, meta),
    "goora"    = tb_render_goora(results, style, meta),
    "1dgofcs"  = tb_render_1dgofcs(results, style, meta),
    "2dgofcs"  = tb_render_2dgofcs(results, style, meta),
    "subloc"   = tb_render_subloc(results, style, meta),
    "msea"     = tb_render_msea(results, style, meta),
    # "class_enrichment" = tb_render_class_enrichment(results, style, meta),
    "pathway_fcs" = tb_render_pathway_fcs(results, style, meta),
    "ppi_network" = tb_render_ppi_network(results, style, meta),
    "multi_scatter" = tb_render_multi_scatter(results, style, meta),
    "multi_correlation" = tb_render_multi_correlation(results, style, meta),
    "replicate_clustering" = tb_render_replicate_clustering(results, style, meta),
    "peptide_region" = tb_render_peptide_region(results, style, meta),
    {
      # Fallback for unknown engines
      plots <- list()
      if (inherits(results, "ggplot")) plots <- list(plot = results)
      if (inherits(results$plot, "ggplot")) plots <- list(plot = results$plot)
      if (is.list(results$plots)) plots <- results$plots
      list(plots = plots, tables = results$tables %||% list(), tabs = NULL)
    }
  )

  # Normalize and validate output against registry spec
  out <- tb_normalize_render_output(engine_def, out)

  # Ensure plots and tables are always lists (defensive)
  out$plots <- out$plots %||% list()
  out$tables <- out$tables %||% list()

  # Force black text defaults across all ggplot outputs (axes/titles/labels)
  out$plots <- lapply(out$plots, function(p) {
    if (isTRUE(attr(p, "tb_skip_force_black_text") %||% FALSE)) return(p)
    tb_force_black_text(p)
  })
  out
}
