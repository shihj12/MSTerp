# R/pages/tools/tool_venn.R
# Venn Diagram Tool - Compare overlaps between 2-6 lists of items

# ============================================================
# Helpers
# ============================================================

# Parse pasted text into a deduplicated character vector.
# Handles newline, tab, comma, and semicolon separators (covers Excel paste).
.parse_venn_items <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character(0))
  items <- unlist(strsplit(text, "[\n\r\t,;]+"))
  items <- trimws(items)
  unique(items[nzchar(items)])
}

# Default palette for up to 6 groups
.venn_default_colors <- function() {
  c("#E64B35", "#4DBBD5", "#00A087", "#F39B7F", "#8491B4", "#91D1C2")
}

# Compute intersection regions from a named list of character vectors.
# Returns a list of region objects, each with:
#   id, group_indices, group_names, group_colors, count, items
.compute_venn_regions <- function(sets, colors) {
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

  # Sort: deepest intersection first (most groups), then count descending
  depths <- vapply(region_list, function(r) length(r$group_indices), integer(1))
  counts <- vapply(region_list, function(r) r$count, integer(1))
  region_list <- region_list[order(-depths, -counts)]

  # Assign IDs
  for (i in seq_along(region_list)) region_list[[i]]$id <- i
  region_list
}

# Try to add region-ID labels ([A], [B], ...) to a ggvenn plot by extracting
# the positions of count-text from the built plot data and overlaying tags.
# Falls back silently if extraction fails.
.annotate_venn_regions <- function(p, regions) {
  tryCatch({
    pb <- ggplot2::ggplot_build(p)

    # Locate the text layer whose labels are numeric (the count text)
    count_data <- NULL
    for (d in pb$data) {
      if (!"label" %in% names(d) || nrow(d) == 0) next
      labels_clean <- sub("\\n.*", "", as.character(d$label))
      if (all(grepl("^\\d+$", labels_clean))) {
        count_data <- d
        count_data$.count <- as.integer(labels_clean)
        break
      }
    }
    if (is.null(count_data)) return(p)

    # Match build-data rows to computed regions by count.
    # For tied counts, first-come-first-served (acceptable since the table
    # also shows color-coded group dots for disambiguation).
    region_counts <- vapply(regions, function(r) r$count, integer(1))
    region_ids    <- vapply(regions, function(r) r$id, integer(1))
    available     <- seq_along(region_counts)
    matched_ids   <- integer(nrow(count_data))

    for (j in seq_len(nrow(count_data))) {
      bc <- count_data$.count[j]
      cands <- available[region_counts[available] == bc]
      if (length(cands) > 0) {
        matched_ids[j] <- region_ids[cands[1]]
        available <- setdiff(available, cands[1])
      }
    }

    valid <- matched_ids > 0
    if (!any(valid)) return(p)

    annot_df <- data.frame(
      x     = count_data$x[valid],
      y     = count_data$y[valid],
      label = paste0("[", LETTERS[matched_ids[valid]], "]")
    )
    p + ggplot2::geom_text(
      data = annot_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3, fontface = "bold", vjust = 2.8, color = "#555555"
    )
  }, error = function(e) p)
}

# ============================================================
# UI
# ============================================================
tools_venn_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_venn_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("Venn Diagram", style = "margin: 0;")
    ),
    tags$p("Compare overlaps between 2\u20136 lists. Paste items (one per line, or tab/comma separated)."),
    two_panel_ui(
      left_ui = tagList(
        numericInput("tools_venn_n_groups", "Number of lists", value = 3, min = 2, max = 6, step = 1),
        # Static group inputs — conditionalPanel keeps them alive so values survive
        # when the user changes the list count.
        local({
          dc <- .venn_default_colors()
          tagList(lapply(seq_len(6), function(i) {
            conditionalPanel(
              condition = paste0("input.tools_venn_n_groups >= ", i),
              div(
                style = "border: 1px solid #ddd; border-radius: 4px; padding: 8px; margin-bottom: 8px;",
                div(
                  style = "display: flex; gap: 8px; align-items: flex-end;",
                  div(
                    style = "flex: 1;",
                    textInput(paste0("tools_venn_title_", i),
                              paste0("List ", LETTERS[i], " title"),
                              value = "",
                              placeholder = "e.g. Treated, Control, KO ...")
                  ),
                  div(
                    style = "width: 120px;",
                    colourpicker::colourInput(paste0("tools_venn_color_", i),
                                              "Color",
                                              value = dc[i])
                  )
                ),
                textAreaInput(paste0("tools_venn_items_", i),
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
          actionButton("tools_venn_generate", "Generate", class = "btn-primary btn-tool-action"),
          actionButton("tools_venn_reset", "Reset", class = "btn btn-default btn-tool-action")
        ),
        hr(),
        tools_collapse_section_ui(
          "tools_venn_style_section", "Style Options", open = FALSE,
          sliderInput("tools_venn_fill_alpha", "Fill opacity", min = 0, max = 1, value = 0.4, step = 0.05),
          numericInput("tools_venn_outline_size", "Outline width", value = 0.4, min = 0, max = 5, step = 0.1),
          colourpicker::colourInput("tools_venn_outline_color", "Outline color", value = "#000000"),
          numericInput("tools_venn_label_size", "Label font size", value = 5, min = 0, max = 20, step = 0.5),
          numericInput("tools_venn_text_size", "Count font size", value = 4, min = 0, max = 20, step = 0.5),
          checkboxInput("tools_venn_show_pct", "Show percentages", value = FALSE),
          hr(),
          numericInput("tools_venn_export_w", "Export width (in)", value = 8, min = 2, max = 30, step = 0.5),
          numericInput("tools_venn_export_h", "Export height (in)", value = 8, min = 2, max = 30, step = 0.5)
        )
      ),
      right_ui = tagList(
        uiOutput("tools_venn_status"),
        div(
          class = "tool-plot-box",
          plotOutput("tools_venn_plot", height = "500px")
        ),
        div(
          style = "display: flex; gap: 6px; margin: 8px 0;",
          actionButton("tools_venn_download_png", "Download PNG", class = "btn btn-sm btn-default", icon = icon("download")),
          actionButton("tools_venn_download_pdf", "Download PDF", class = "btn btn-sm btn-default", icon = icon("file-pdf")),
          downloadButton("tools_venn_download_excel", "Download Excel", class = "btn btn-sm btn-default")
        ),
        tags$h4("Intersection Breakdown"),
        tags$p(class = "text-muted", style = "font-size: 0.85em;",
               "Region IDs [A], [B], \u2026 are overlaid on the diagram. Hover a color dot to see the group name."),
        DT::dataTableOutput("tools_venn_intersection_table")
      )
    )
  )
}

# ============================================================
# Server
# ============================================================
tools_venn_server <- function(input, output, session, app_state, rv) {

  default_colors <- .venn_default_colors()

  # ---- Helper: collect current group data ----
  collect_groups <- function() {
    n <- input$tools_venn_n_groups %||% 3
    n <- max(2, min(6, as.integer(n)))
    titles <- character(n)
    colors <- character(n)
    sets <- vector("list", n)
    for (i in seq_len(n)) {
      raw_title <- input[[paste0("tools_venn_title_", i)]] %||% ""
      titles[i] <- if (nzchar(trimws(raw_title))) trimws(raw_title) else paste("List", LETTERS[i])
      colors[i] <- input[[paste0("tools_venn_color_", i)]] %||% default_colors[i]
      sets[[i]] <- .parse_venn_items(input[[paste0("tools_venn_items_", i)]])
    }
    names(sets) <- titles
    list(sets = sets, colors = colors, titles = titles)
  }

  # ---- Generate ----
  observeEvent(input$tools_venn_generate, {
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
      paste0("Note: ", paste(empty_names, collapse = ", "), " empty \u2014 excluded from diagram.")
    } else NULL

    # Filter to populated groups only
    sets <- sets[populated]
    colors <- colors[populated]
    titles <- titles[populated]

    # Build display sets with "Title (n)" labels
    display_sets <- sets
    names(display_sets) <- paste0(titles, " (", vapply(sets, length, integer(1)), ")")

    # Build Venn diagram
    ggvenn_fun <- getFromNamespace("ggvenn", "ggvenn")
    ggvenn_args <- names(formals(ggvenn_fun))

    fill_alpha   <- input$tools_venn_fill_alpha   %||% 0.4
    outline_size <- input$tools_venn_outline_size  %||% 0.4
    outline_color <- input$tools_venn_outline_color %||% "#000000"
    label_size   <- input$tools_venn_label_size    %||% 5
    text_size    <- input$tools_venn_text_size     %||% 4
    show_pct     <- isTRUE(input$tools_venn_show_pct)

    venn_args <- list(
      fill_color      = unname(colors),
      fill_alpha      = fill_alpha,
      stroke_size     = outline_size,
      stroke_color    = outline_color,
      stroke_colour   = outline_color,
      set_name_size   = label_size,
      text_size       = text_size,
      show_percentage = show_pct,
      show_percent    = show_pct
    )
    venn_args <- venn_args[names(venn_args) %in% ggvenn_args]

    p <- tryCatch({
      pl <- do.call(ggvenn_fun, c(list(display_sets), venn_args))
      pl + ggplot2::theme_void() +
        ggplot2::theme(
          axis.text   = ggplot2::element_blank(),
          axis.title  = ggplot2::element_blank(),
          axis.ticks  = ggplot2::element_blank(),
          panel.grid  = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(5, 5, 5, 5)
        )
    }, error = function(e) {
      rv$status_msg <- paste("Error generating Venn diagram:", e$message)
      rv$status_level <- "error"
      NULL
    })

    if (is.null(p)) return()

    # Compute regions (uses raw titles, not display labels)
    regions <- .compute_venn_regions(sets, colors)
    rv$regions <- regions

    # Annotate diagram with region IDs [A], [B], ...
    rv$plot <- .annotate_venn_regions(p, regions)

    total <- length(unique(unlist(sets)))
    msg <- paste0("Venn diagram generated \u2014 ", total, " unique items across ",
                  length(sets), " lists.")
    if (!is.null(warn_msg)) msg <- paste(msg, warn_msg)
    rv$status_msg  <- msg
    rv$status_level <- "success"
  }, ignoreInit = TRUE)

  # ---- Reset ----
  observeEvent(input$tools_venn_reset, {
    rv$plot     <- NULL
    rv$regions  <- NULL
    rv$status_msg   <- NULL
    rv$status_level <- NULL
    updateNumericInput(session, "tools_venn_n_groups", value = 3)
    # Clear all 6 group inputs (static panels, so values persist unless cleared)
    for (i in seq_len(6)) {
      updateTextInput(session, paste0("tools_venn_title_", i), value = "")
      updateTextAreaInput(session, paste0("tools_venn_items_", i), value = "")
      colourpicker::updateColourInput(session, paste0("tools_venn_color_", i),
                                      value = default_colors[i])
    }
  }, ignoreInit = TRUE)

  # ---- Render plot ----
  output$tools_venn_plot <- renderPlot({
    req(rv$plot)
    rv$plot
  })

  # ---- Render status ----
  output$tools_venn_status <- renderUI({
    msg <- rv$status_msg
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    cls <- switch(rv$status_level %||% "info",
                  error   = "text-danger",
                  success = "text-success",
                  "text-muted")
    tags$div(class = cls, style = "margin-bottom: 8px;", msg)
  })

  # ---- Render intersection table (colored dots, no text group names) ----
  output$tools_venn_intersection_table <- DT::renderDataTable({
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
  observeEvent(input$tools_venn_download_png, {
    if (is.null(rv$plot)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PNG",
      selectInput("tools_venn_png_dpi", "DPI", choices = c(150, 300, 600), selected = 300),
      downloadButton("tools_venn_png_confirm", "Download", class = "btn-primary"),
      actionButton("tools_venn_png_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_venn_png_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_venn_png_confirm <- downloadHandler(
    filename = function() {
      paste0("venn_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      p <- rv$plot
      if (is.null(p)) return()
      dpi <- as.numeric(input$tools_venn_png_dpi %||% 300)
      w <- input$tools_venn_export_w %||% 8
      h <- input$tools_venn_export_h %||% 8
      png_type <- if (capabilities("cairo")) "cairo-png" else NULL
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", dpi = dpi,
                      device = grDevices::png, type = png_type)
      removeModal()
    }
  )

  # ---- Export: PDF ----
  observeEvent(input$tools_venn_download_pdf, {
    if (is.null(rv$plot)) {
      showNotification("No plot to download.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Download PDF",
      downloadButton("tools_venn_pdf_confirm", "Download", class = "btn-primary"),
      actionButton("tools_venn_pdf_cancel", "Cancel", class = "btn-secondary"),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$tools_venn_pdf_cancel, removeModal(), ignoreInit = TRUE)

  output$tools_venn_pdf_confirm <- downloadHandler(
    filename = function() {
      paste0("venn_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      p <- rv$plot
      if (is.null(p)) return()
      w <- input$tools_venn_export_w %||% 8
      h <- input$tools_venn_export_h %||% 8
      pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      ggplot2::ggsave(file, p, width = w, height = h, units = "in", device = pdf_device)
      removeModal()
    }
  )

  # ---- Export: Excel (Key + Items sheets, color-coded) ----
  output$tools_venn_download_excel <- downloadHandler(
    filename = function() {
      paste0("venn_intersections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
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

      # Color-code Region column cells with the first participating group's color
      header_style <- openxlsx::createStyle(textDecoration = "bold")
      openxlsx::addStyle(wb, "Key", header_style, rows = 1, cols = seq_len(ncol(key_df)))
      for (i in seq_along(regions)) {
        bg <- regions[[i]]$group_colors[1]
        # Make font white or black depending on background brightness
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
        items_df <- data.frame(Item = character(), Region = integer(),
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
