# R/pages/tools/tool_upset.R
# UpSet Plot Tool - Visualize intersections between 2+ lists of items.
# Mirrors the Venn tool I/O (paste lists, style, PNG/PDF/Excel export) but
# scales past the 6-set limit of a Venn diagram via an UpSet matrix.

# ============================================================
# Helpers
# ============================================================

# Parse pasted text into a deduplicated character vector.
# Handles newline, tab, comma, and semicolon separators (covers Excel paste).
.parse_upset_items <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character(0))
  items <- unlist(strsplit(text, "[\n\r\t,;]+"))
  items <- trimws(items)
  unique(items[nzchar(items)])
}

# Default palette for up to 10 groups
.upset_default_colors <- function() {
  c("#E64B35", "#4DBBD5", "#00A087", "#F39B7F", "#8491B4",
    "#91D1C2", "#DC0000", "#7E6148", "#B09C85", "#3C5488")
}

# Compact integer formatter for axis labels (e.g. 1500 -> "1.5k").
.upset_format_k <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v)) return("")
    if (abs(v) >= 1000) paste0(formatC(v / 1000, format = "f", digits = 1), "k") else as.character(round(v))
  }, character(1))
}

# Compute exclusive-intersection regions from a named list of character vectors.
# Each item belongs to exactly one region (its exact combination of sets) — the
# same buckets an UpSet bar represents. Returns a list of region objects, each:
#   id, group_indices, group_names, group_colors, count, items
.compute_upset_regions <- function(sets, colors) {
  group_names <- names(sets)
  all_items <- unique(unlist(sets))
  if (length(all_items) == 0) return(list())

  # Determine exclusive membership for every item
  membership <- vapply(all_items, function(item) {
    belongs <- vapply(sets, function(s) item %in% s, logical(1))
    paste(sort(which(belongs)), collapse = ",")
  }, character(1))

  buckets <- split(all_items, membership)

  region_list <- lapply(names(buckets), function(key) {
    idx <- as.integer(strsplit(key, ",")[[1]])
    list(
      group_indices = idx,
      group_names   = group_names[idx],
      group_colors  = colors[idx],
      count         = length(buckets[[key]]),
      items         = sort(buckets[[key]])
    )
  })

  # Default sort: count descending, then deepest intersection first
  depths <- vapply(region_list, function(r) length(r$group_indices), integer(1))
  counts <- vapply(region_list, function(r) r$count, integer(1))
  region_list <- region_list[order(-counts, -depths)]

  for (i in seq_along(region_list)) region_list[[i]]$id <- i
  region_list
}

# Build a two-panel UpSet plot (intersection bars on top, dot matrix below)
# from a list of regions. Active dots are colored by their set color; inactive
# dots are light gray. Returns a ggplot (via ggplotify) or NULL on failure.
.build_upset_plot <- function(regions, group_names, group_colors, opts) {
  if (length(regions) == 0) return(NULL)

  # Labels: deepest-intersection groups joined with " & "
  labels <- vapply(regions, function(r) {
    if (length(r$group_names) == 0) "(none)" else paste(r$group_names, collapse = " & ")
  }, character(1))
  ids    <- vapply(regions, function(r) LETTERS[r$id], character(1))
  counts <- vapply(regions, function(r) r$count, integer(1))

  # Tag each bar with its region ID so it maps to the breakdown table
  bar_labels <- paste0("[", ids, "] ", labels)
  bar_labels <- factor(bar_labels, levels = rev(bar_labels))

  df_int <- data.frame(intersection = bar_labels, n = counts, stringsAsFactors = FALSE)

  y_expand_mult <- if (isTRUE(opts$count_labels_show)) c(0, 0.15) else c(0, 0.05)
  bar_outline    <- if (isTRUE(opts$show_bar_outline)) opts$bar_outline_color else NA
  bar_outline_lw <- if (isTRUE(opts$show_bar_outline)) opts$bar_outline_width else 0

  p_top <- ggplot2::ggplot(df_int, ggplot2::aes(x = intersection, y = n)) +
    ggplot2::geom_col(fill = opts$bar_fill_color, color = bar_outline, linewidth = bar_outline_lw) +
    ggplot2::labs(x = NULL, y = "Count") +
    ggplot2::scale_y_continuous(labels = .upset_format_k,
                                expand = ggplot2::expansion(mult = y_expand_mult)) +
    ggplot2::theme_bw(base_size = opts$axis_text_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  if (isTRUE(opts$count_labels_show)) {
    p_top <- p_top +
      ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.3,
                         size = opts$count_labels_size, color = opts$count_labels_color) +
      ggplot2::coord_cartesian(clip = "off")
  }

  # Dot matrix: one row per group, one column per intersection
  df_matrix <- do.call(rbind, lapply(seq_along(regions), function(i) {
    present <- group_names %in% regions[[i]]$group_names
    data.frame(
      intersection = as.character(bar_labels[i]),
      group   = group_names,
      color   = group_colors,
      present = present,
      stringsAsFactors = FALSE
    )
  }))
  df_matrix$intersection <- factor(df_matrix$intersection, levels = levels(bar_labels))
  df_matrix$group <- factor(df_matrix$group, levels = rev(group_names))
  df_matrix$y <- as.integer(df_matrix$group)

  # Color active dots per their set; inactive dots light gray
  df_matrix$dot_color <- ifelse(df_matrix$present, df_matrix$color, "#E0E0E0")

  # Connecting segments per intersection (top-most to bottom-most active group)
  df_present <- df_matrix[df_matrix$present, , drop = FALSE]
  segs <- NULL
  if (nrow(df_present) > 0) {
    segs <- do.call(rbind, lapply(split(df_present, df_present$intersection), function(d) {
      if (nrow(d) == 0) return(NULL)
      data.frame(
        intersection = unique(d$intersection),
        ymin = min(d$y), ymax = max(d$y),
        stringsAsFactors = FALSE
      )
    }))
  }

  p_matrix <- ggplot2::ggplot(df_matrix, ggplot2::aes(x = intersection, y = y))
  if (!is.null(segs) && nrow(segs) > 0) {
    p_matrix <- p_matrix + ggplot2::geom_segment(
      data = segs,
      ggplot2::aes(x = intersection, xend = intersection, y = ymin, yend = ymax),
      inherit.aes = FALSE, linewidth = 0.8, color = "gray40"
    )
  }
  p_matrix <- p_matrix +
    ggplot2::geom_point(ggplot2::aes(color = dot_color), size = opts$dot_size) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels(df_matrix$group)),
      labels = levels(df_matrix$group),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = opts$axis_text_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  if (!requireNamespace("ggplotify", quietly = TRUE)) {
    # Fallback: bars only if ggplotify is unavailable
    return(p_top)
  }

  g_top <- ggplot2::ggplotGrob(p_top)
  g_mat <- ggplot2::ggplotGrob(p_matrix)

  max_widths <- grid::unit.pmax(g_top$widths, g_mat$widths)
  g_top$widths <- max_widths
  g_mat$widths <- max_widths

  g_combined <- rbind(g_top, g_mat, size = "first")

  n_rows_top <- nrow(g_top)
  n_rows_mat <- nrow(g_mat)
  g_combined$heights[seq_len(n_rows_top)] <-
    g_combined$heights[seq_len(n_rows_top)] * 0.60 / 0.5
  g_combined$heights[seq(n_rows_top + 1, n_rows_top + n_rows_mat)] <-
    g_combined$heights[seq(n_rows_top + 1, n_rows_top + n_rows_mat)] * 0.40 / 0.5

  suppressWarnings(ggplotify::as.ggplot(g_combined))
}

# ============================================================
# UI
# ============================================================
tools_upset_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_upset_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("UpSet Plot", style = "margin: 0;")
    ),
    tags$p("Visualize intersections between 2–10 lists. Paste items (one per line, or tab/comma separated). Scales past the 6-set limit of a Venn diagram."),
    two_panel_ui(
      left_ui = tagList(
        numericInput("tools_upset_n_groups", "Number of lists", value = 3, min = 2, max = 10, step = 1),
        # Static group inputs — conditionalPanel keeps them alive so values survive
        # when the user changes the list count.
        local({
          dc <- .upset_default_colors()
          tagList(lapply(seq_len(10), function(i) {
            conditionalPanel(
              condition = paste0("input.tools_upset_n_groups >= ", i),
              div(
                style = "border: 1px solid #ddd; border-radius: 4px; padding: 8px; margin-bottom: 8px;",
                div(
                  style = "display: flex; gap: 8px; align-items: flex-end;",
                  div(
                    style = "flex: 1;",
                    textInput(paste0("tools_upset_title_", i),
                              paste0("List ", LETTERS[i], " title"),
                              value = "",
                              placeholder = "e.g. Treated, Control, KO ...")
                  ),
                  div(
                    style = "width: 120px;",
                    colourpicker::colourInput(paste0("tools_upset_color_", i),
                                              "Color",
                                              value = dc[i])
                  )
                ),
                textAreaInput(paste0("tools_upset_items_", i),
                              NULL,
                              placeholder = "Paste items here (one per line, or tab/comma separated)",
                              rows = 4,
                              resize = "vertical")
              )
            )
          }))
        }),
        div(
          style = "display: flex; gap: 8px; margin-top: 10px;",
          actionButton("tools_upset_generate", "Generate", class = "btn-primary btn-tool-action"),
          actionButton("tools_upset_reset", "Reset", class = "btn btn-default btn-tool-action")
        ),
        hr(),
        tools_collapse_section_ui(
          "tools_upset_style_section", "Style Options", open = FALSE,
          selectInput("tools_upset_sort_by", "Sort intersections by",
                      choices = c("Size (frequency)" = "size", "Degree (# of sets)" = "degree"),
                      selected = "size"),
          numericInput("tools_upset_max_int", "Max intersections shown", value = 40, min = 1, max = 200, step = 1),
          numericInput("tools_upset_min_size", "Min intersection size", value = 1, min = 1, max = 1e6, step = 1),
          hr(),
          colourpicker::colourInput("tools_upset_bar_fill", "Bar color", value = "#4245FF"),
          checkboxInput("tools_upset_show_bar_outline", "Bar outline", value = FALSE),
          colourpicker::colourInput("tools_upset_bar_outline_color", "Bar outline color", value = "#000000"),
          numericInput("tools_upset_bar_outline_width", "Bar outline width", value = 0.8, min = 0, max = 5, step = 0.1),
          checkboxInput("tools_upset_count_labels", "Show count labels", value = TRUE),
          numericInput("tools_upset_count_size", "Count label size", value = 3.5, min = 0, max = 12, step = 0.5),
          colourpicker::colourInput("tools_upset_count_color", "Count label color", value = "#000000"),
          numericInput("tools_upset_dot_size", "Matrix dot size", value = 3, min = 0.5, max = 10, step = 0.5),
          numericInput("tools_upset_axis_size", "Axis text size", value = 14, min = 6, max = 30, step = 1),
          hr(),
          numericInput("tools_upset_export_w", "Export width (in)", value = 10, min = 2, max = 30, step = 0.5),
          numericInput("tools_upset_export_h", "Export height (in)", value = 7, min = 2, max = 30, step = 0.5)
        )
      ),
      right_ui = tagList(
        uiOutput("tools_upset_status"),
        div(
          class = "tool-plot-box",
          plotOutput("tools_upset_plot", height = "500px")
        ),
        div(
          style = "display: flex; gap: 6px; margin: 8px 0;",
          actionButton("tools_upset_download_png", "Download PNG", class = "btn btn-sm btn-default", icon = icon("download")),
          actionButton("tools_upset_download_pdf", "Download PDF", class = "btn btn-sm btn-default", icon = icon("file-pdf")),
          downloadButton("tools_upset_download_excel", "Download Excel", class = "btn btn-sm btn-default")
        ),
        tags$h4("Intersection Breakdown"),
        tags$p(class = "text-muted", style = "font-size: 0.85em;",
               "Region IDs [A], [B], … label each bar. Hover a color dot to see the group name."),
        DT::dataTableOutput("tools_upset_intersection_table")
      )
    )
  )
}

# ============================================================
# Server
# ============================================================
tools_upset_server <- function(input, output, session, app_state, rv) {

  default_colors <- .upset_default_colors()

  # ---- Helper: collect current group data ----
  collect_groups <- function() {
    n <- input$tools_upset_n_groups %||% 3
    n <- max(2, min(10, as.integer(n)))
    titles <- character(n)
    colors <- character(n)
    sets <- vector("list", n)
    for (i in seq_len(n)) {
      raw_title <- input[[paste0("tools_upset_title_", i)]] %||% ""
      titles[i] <- if (nzchar(trimws(raw_title))) trimws(raw_title) else paste("List", LETTERS[i])
      colors[i] <- input[[paste0("tools_upset_color_", i)]] %||% default_colors[i]
      sets[[i]] <- .parse_upset_items(input[[paste0("tools_upset_items_", i)]])
    }
    names(sets) <- titles
    list(sets = sets, colors = colors, titles = titles)
  }

  # ---- Generate ----
  observeEvent(input$tools_upset_generate, {
    grp <- collect_groups()
    sets <- grp$sets
    colors <- grp$colors
    titles <- grp$titles

    # Validate: at least 2 populated groups
    populated <- vapply(sets, function(s) length(s) > 0, logical(1))
    if (sum(populated) < 2) {
      rv$status_msg <- "Please provide items in at least 2 lists."
      rv$status_level <- "error"
      rv$plot <- NULL
      rv$regions <- NULL
      return()
    }

    # Warn about empty groups
    empty_names <- titles[!populated]
    warn_msg <- if (length(empty_names) > 0) {
      paste0("Note: ", paste(empty_names, collapse = ", "), " empty — excluded.")
    } else NULL

    # Filter to populated groups only
    sets <- sets[populated]
    colors <- colors[populated]
    titles <- titles[populated]

    # Display group labels include set size, e.g. "Treated (123)"
    group_labels <- paste0(titles, " (", vapply(sets, length, integer(1)), ")")

    # Compute exclusive intersection regions (uses raw titles)
    all_regions <- .compute_upset_regions(sets, colors)
    if (length(all_regions) == 0) {
      rv$status_msg <- "No items found in the provided lists."
      rv$status_level <- "error"
      rv$plot <- NULL
      rv$regions <- NULL
      return()
    }

    # Apply min-size filter
    min_size <- suppressWarnings(as.integer(input$tools_upset_min_size %||% 1))
    if (!is.finite(min_size) || min_size < 1) min_size <- 1
    kept <- Filter(function(r) r$count >= min_size, all_regions)
    if (length(kept) == 0) {
      rv$status_msg <- paste0("No intersections with at least ", min_size, " items.")
      rv$status_level <- "error"
      rv$plot <- NULL
      rv$regions <- NULL
      return()
    }

    # Sort
    sort_by <- input$tools_upset_sort_by %||% "size"
    if (identical(sort_by, "degree")) {
      ord <- order(
        -vapply(kept, function(r) length(r$group_indices), integer(1)),
        -vapply(kept, function(r) r$count, integer(1))
      )
    } else {
      ord <- order(
        -vapply(kept, function(r) r$count, integer(1)),
        -vapply(kept, function(r) length(r$group_indices), integer(1))
      )
    }
    kept <- kept[ord]

    # Limit to top-N intersections
    max_int <- suppressWarnings(as.integer(input$tools_upset_max_int %||% 40))
    if (!is.finite(max_int) || max_int < 1) max_int <- 40
    truncated <- length(kept) > max_int
    if (truncated) kept <- kept[seq_len(max_int)]

    # Re-assign sequential IDs in display order (so [A] is the first bar)
    for (i in seq_along(kept)) kept[[i]]$id <- i

    opts <- list(
      bar_fill_color    = input$tools_upset_bar_fill %||% "#4245FF",
      show_bar_outline  = isTRUE(input$tools_upset_show_bar_outline),
      bar_outline_color = input$tools_upset_bar_outline_color %||% "#000000",
      bar_outline_width = { v <- suppressWarnings(as.numeric(input$tools_upset_bar_outline_width %||% 0.8)); if (!is.finite(v) || v < 0) 0.8 else v },
      count_labels_show = isTRUE(input$tools_upset_count_labels),
      count_labels_size = { v <- suppressWarnings(as.numeric(input$tools_upset_count_size %||% 3.5)); if (!is.finite(v) || v <= 0) 3.5 else v },
      count_labels_color = input$tools_upset_count_color %||% "#000000",
      dot_size          = { v <- suppressWarnings(as.numeric(input$tools_upset_dot_size %||% 3)); if (!is.finite(v) || v <= 0) 3 else v },
      axis_text_size    = { v <- suppressWarnings(as.numeric(input$tools_upset_axis_size %||% 14)); if (!is.finite(v) || v <= 0) 14 else v }
    )

    p <- tryCatch(
      .build_upset_plot(kept, group_labels, colors, opts),
      error = function(e) {
        rv$status_msg <- paste("Error generating UpSet plot:", e$message)
        rv$status_level <- "error"
        NULL
      }
    )
    if (is.null(p)) return()

    rv$plot <- p
    rv$regions <- kept

    total <- length(unique(unlist(sets)))
    msg <- paste0("UpSet plot generated — ", total, " unique items across ",
                  length(sets), " lists, ", length(kept), " intersections shown.")
    if (truncated) msg <- paste0(msg, " (top ", max_int, " by ", sort_by, ".)")
    if (!is.null(warn_msg)) msg <- paste(msg, warn_msg)
    rv$status_msg  <- msg
    rv$status_level <- "success"
  }, ignoreInit = TRUE)

  # ---- Reset ----
  observeEvent(input$tools_upset_reset, {
    rv$plot     <- NULL
    rv$regions  <- NULL
    rv$status_msg   <- NULL
    rv$status_level <- NULL
    updateNumericInput(session, "tools_upset_n_groups", value = 3)
    for (i in seq_len(10)) {
      updateTextInput(session, paste0("tools_upset_title_", i), value = "")
      updateTextAreaInput(session, paste0("tools_upset_items_", i), value = "")
      colourpicker::updateColourInput(session, paste0("tools_upset_color_", i),
                                      value = default_colors[i])
    }
  }, ignoreInit = TRUE)

  # ---- Render plot ----
  output$tools_upset_plot <- renderPlot({
    req(rv$plot)
    rv$plot
  })

  # ---- Render status ----
  output$tools_upset_status <- renderUI({
    msg <- rv$status_msg
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    cls <- switch(rv$status_level %||% "info",
                  error   = "text-danger",
                  success = "text-success",
                  "text-muted")
    tags$div(class = cls, style = "margin-bottom: 8px;", msg)
  })

  # ---- Render intersection table (colored dots, no text group names) ----
  output$tools_upset_intersection_table <- DT::renderDataTable({
    req(rv$regions)
    regions <- rv$regions

    df <- data.frame(
      ID = vapply(regions, function(r) LETTERS[r$id], character(1)),
      Groups = vapply(regions, function(r) {
        dots <- vapply(seq_along(r$group_colors), function(k) {
          sprintf(
            '<span title="%s" style="display:inline-block;width:14px;height:14px;border-radius:50%%;background:%s;margin-right:3px;border:1px solid #888;cursor:help;"></span>',
            htmltools::htmlEscape(r$group_names[k]),
            htmltools::htmlEscape(r$group_colors[k])
          )
        }, character(1))
        paste(dots, collapse = "")
      }, character(1)),
      Count = vapply(regions, function(r) r$count, integer(1)),
      Items = vapply(regions, function(r) paste(r$items, collapse = ", "), character(1)),
      stringsAsFactors = FALSE
    )

    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 20,
                                 columnDefs = list(
                                   list(width = "40px", targets = 0),
                                   list(width = "80px", targets = 1)
                                 )))
  })

  # ---- Export: PNG ----
  observeEvent(input$tools_upset_download_png, {
    if (is.null(rv$plot)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PNG",
      selectInput("tools_upset_png_dpi", "DPI", choices = c(150, 300, 600), selected = 300),
      downloadButton("tools_upset_png_confirm", "Download", class = "btn-primary"),
      actionButton("tools_upset_png_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_upset_png_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_upset_png_confirm <- downloadHandler(
    filename = function() {
      paste0("upset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      p <- rv$plot
      if (is.null(p)) return()
      dpi <- as.numeric(input$tools_upset_png_dpi %||% 300)
      w <- input$tools_upset_export_w %||% 10
      h <- input$tools_upset_export_h %||% 7
      png_type <- if (capabilities("cairo")) "cairo-png" else NULL
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", dpi = dpi,
                      device = grDevices::png, type = png_type)
      removeModal()
    }
  )

  # ---- Export: PDF ----
  observeEvent(input$tools_upset_download_pdf, {
    if (is.null(rv$plot)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PDF",
      downloadButton("tools_upset_pdf_confirm", "Download", class = "btn-primary"),
      actionButton("tools_upset_pdf_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_upset_pdf_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_upset_pdf_confirm <- downloadHandler(
    filename = function() {
      paste0("upset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      p <- rv$plot
      if (is.null(p)) return()
      w <- input$tools_upset_export_w %||% 10
      h <- input$tools_upset_export_h %||% 7
      pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", device = pdf_device)
      removeModal()
    }
  )

  # ---- Export: Excel (Key + Items sheets, color-coded) ----
  output$tools_upset_download_excel <- downloadHandler(
    filename = function() {
      paste0("upset_intersections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      regions <- rv$regions
      if (is.null(regions) || length(regions) == 0) {
        writeLines("No intersection data to export.", file)
        return()
      }

      wb <- openxlsx::createWorkbook()

      # ---- Sheet 1: Region Key ----
      key_df <- data.frame(
        Region = vapply(regions, function(r) LETTERS[r$id], character(1)),
        Groups = vapply(regions, function(r) paste(r$group_names, collapse = " + "), character(1)),
        Count  = vapply(regions, function(r) r$count, integer(1)),
        Items  = vapply(regions, function(r) paste(r$items, collapse = ", "), character(1)),
        stringsAsFactors = FALSE
      )
      openxlsx::addWorksheet(wb, "Key")
      openxlsx::writeData(wb, "Key", key_df)

      header_style <- openxlsx::createStyle(textDecoration = "bold")
      openxlsx::addStyle(wb, "Key", header_style, rows = 1, cols = seq_len(ncol(key_df)))
      for (i in seq_along(regions)) {
        bg <- regions[[i]]$group_colors[1]
        rgb_vals <- grDevices::col2rgb(bg)
        lum <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
        fg <- if (lum > 0.5) "#000000" else "#FFFFFF"
        cell_style <- openxlsx::createStyle(fgFill = bg, fontColour = fg, textDecoration = "bold")
        openxlsx::addStyle(wb, "Key", cell_style, rows = i + 1, cols = 1)
      }
      openxlsx::setColWidths(wb, "Key", cols = c(1, 2, 3, 4), widths = c(8, 30, 8, 60))

      # ---- Sheet 2: Items (one row per item) ----
      items_rows <- lapply(regions, function(r) {
        if (length(r$items) == 0) return(NULL)
        data.frame(
          Item   = r$items,
          Region = LETTERS[r$id],
          Groups = paste(r$group_names, collapse = " + "),
          stringsAsFactors = FALSE
        )
      })
      items_df <- do.call(rbind, Filter(function(x) !is.null(x), items_rows))
      if (is.null(items_df)) {
        items_df <- data.frame(Item = character(), Region = character(),
                               Groups = character(), stringsAsFactors = FALSE)
      }
      openxlsx::addWorksheet(wb, "Items")
      openxlsx::writeData(wb, "Items", items_df)
      openxlsx::addStyle(wb, "Items", header_style, rows = 1, cols = seq_len(ncol(items_df)))
      openxlsx::setColWidths(wb, "Items", cols = c(1, 2, 3), widths = c(25, 8, 30))

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}
