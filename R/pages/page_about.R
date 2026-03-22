# page_about.R

page_about_ui <- function(linkedin_logo = "static/img/linkedin.png",
                          scholar_logo  = "static/img/scholar.png") {
  # Icons (YOU)
  scholar_profile_url <- "https://scholar.google.com/citations?user=c8TjxpQAAAAJ&hl=en"
  linkedin_url        <- "https://www.linkedin.com/in/jamison-shih"
  
  # Publications list (LAB works page)
  scholar_works_url <- "https://scholar.google.com/citations?hl=en&user=IgEvNuQAAAAJ&view_op=list_works&sortby=pubdate"
  
  about_card <- function(title, ..., class = NULL) {
    tags$div(
      class = paste(c("a-card", class), collapse = " "),
      tags$div(class = "a-card-h", title),
      tags$div(class = "a-card-b", ...)
    )
  }

  cite_item <- function(title, detail, href) {
    tags$li(
      tags$a(
        href = href,
        target = "_blank",
        rel = "noopener",
        title
      ),
      if (!is.null(detail) && nzchar(detail)) tags$div(class = "muted", detail) else NULL
    )
  }
  
  msterp_page(
    title = NULL,
    tags$div(
      id = "about-page",
      
      # Scoped styling for this page only (no dependency on bslib cards)
      tags$style(htmltools::HTML("
        #about-page {
          height: 100%;
          overflow: auto;
        }
        #about-page .muted { color: var(--text-muted); }

        /* Layout: scrollable container */
        #about-page .about-wrap {
          display: flex;
          flex-direction: column;
          gap: 14px;
          padding-bottom: 20px;
        }

        #about-page .about-main {
          display: flex;
          flex-direction: column;
          gap: 14px;
        }

        #about-page .about-grid {
          display: grid;
          grid-template-columns: minmax(340px, 1.2fr) minmax(320px, 0.8fr);
          gap: 16px;
          align-items: stretch;
        }

        #about-page .right-col {
          display: flex;
          flex-direction: column;
          gap: 16px;
          height: 100%;
        }

        /* Custom cards */
        #about-page .a-card {
          border: 1px solid var(--md-card-border);
          border-radius: 14px;
          background: var(--bg-card);
          box-shadow: var(--md-card-shadow);
          overflow: hidden;
          display: flex;
          flex-direction: column;
        }

        #about-page .a-card-h {
          background-color: var(--md-red);
          color: #fff;
          font-weight: 600;
          padding: 10px 14px;
          border-bottom: 1px solid var(--md-border);
        }

        #about-page .a-card-b {
          padding: 12px 14px;
          display: flex;
          flex-direction: column;
          gap: 10px;
        }

        /* Fixed height map */
        #about-page .map-card { height: 100%; }
        #about-page .map-card .a-card-b { flex: 1 1 auto; }
        #about-page .map-wrap {
          flex: 1 1 auto;
          min-height: 420px;
        }

        #about-page .pub-list-wrap {
          max-height: 200px;
          overflow: auto;
          padding-right: 6px;
        }

        /* Footer icons centered */
        #about-page .about-footer {
          display: flex;
          justify-content: center;
          align-items: center;
          gap: 14px;
          padding: 10px 0 2px;
        }
        #about-page .about-footer img { height: 30px; width: 30px; }
        #about-page .about-footer a { text-decoration: none; }

        /* Responsive */
        @media (max-width: 992px) {
          #about-page .about-grid { grid-template-columns: 1fr; }
          #about-page .map-wrap { min-height: 420px; }
        }

        #about-page hr {
          border: none;
          border-top: 1px solid var(--md-border);
        }
      ")),
      
      tags$div(
        class = "about-wrap",

        tags$div(
          class = "about-main",
          
          tags$div(
            class = "about-grid",
            
            # LEFT: Map (expands)
            about_card(
              "Location",
              tags$div(
                tags$strong("The Hao Research Group"),
                tags$div(class = "muted", "University of Maryland, College Park, Department of Chemistry & Biochemistry")
              ),
              tags$div(
                class = "map-wrap",
                leaflet::leafletOutput("about_dept_map", height = "100%")
              ),
              class = "map-card"
            ),
            
            # RIGHT: Cards stacked, pubs card grows to fill remaining height
            tags$div(
              class = "right-col",

              about_card(
                "Lab Website",
                tags$a(
                  href = "https://blog.umd.edu/haolab/",
                  target = "_blank",
                  rel = "noopener",
                  "The Hao Research Group"
                )
              ),
              
              about_card(
                "Our recent publications",
                tags$div(
                  class = "pub-list-wrap",
                  shiny::uiOutput("about_recent_pubs_ui")
                ),
                tags$div(
                  tags$a(
                    class = "btn btn-outline-primary btn-sm",
                    href = scholar_works_url,
                    target = "_blank",
                    rel = "noopener",
                    "See more"
                  )
                ),
                class = "pub-card"
              ),
              
              about_card(
                "Citations",
                tags$ol(
                  cite_item(
                    "Protein Contaminants Matter: Building Universal Protein Contaminant Libraries for DDA and DIA Proteomics",
                    paste(
                      "Ashley M. Frankenfield, Jiawei Ni, Mustafa Ahmed, and Ling Hao.",
                      "Journal of Proteome Research (2022) 21(9): 2104-2113. DOI: 10.1021/acs.jproteome.2c00145"
                    ),
                    "https://doi.org/10.1021/acs.jproteome.2c00145"
                  ),
                  cite_item(
                    "Ritchie ME, Phipson B, Wu D, et al. limma powers differential expression analyses for RNA-sequencing and microarray studies.",
                    "Nucleic Acids Research (2015). DOI: 10.1093/nar/gkv007",
                    "https://doi.org/10.1093/nar/gkv007"
                  ),
                  cite_item(
                    "The UniProt Consortium. UniProt: the Universal Protein Knowledgebase in 2023.",
                    "Nucleic Acids Research (2023). DOI: 10.1093/nar/gkac1052",
                    "https://doi.org/10.1093/nar/gkac1052"
                  ),
                  cite_item(
                    "The Gene Ontology Consortium. The Gene Ontology knowledgebase in 2023.",
                    "Nucleic Acids Research (2023). DOI: 10.1093/nar/gkac1055",
                    "https://doi.org/10.1093/nar/gkac1055"
                  )
                )
              )
            )
          )
        ),
        
        tags$hr(style = "margin: 0;"),
        
        tags$div(
        class = "about-footer",
        tags$div("Coded by Jamison Shih"),
        tags$hr(style = "margin: 0;"),
          tags$a(
            href = linkedin_url,
            target = "_blank",
            rel = "noopener",
            title = "LinkedIn (Jamison Shih)",
            tags$img(src = linkedin_logo, alt = "LinkedIn")
          ),
          tags$a(
            href = scholar_profile_url,
            target = "_blank",
            rel = "noopener",
            title = "Google Scholar (Jamison Shih)",
            tags$img(src = scholar_logo, alt = "LinkedIn")
          )
        )
      )
    )
  )
}

page_about_server <- function(session) {
  scholar_works_url <- "https://scholar.google.com/citations?hl=en&user=IgEvNuQAAAAJ&view_op=list_works&sortby=pubdate"

  # Static fallback publications (update periodically)
  # Used when Google Scholar blocks automated requests (common on servers)
  static_pubs <- data.frame(
    title = c(
      "CHIP protects lysosomes from CLN4 mutant-induced membrane damages",
      "Maintenance of neuronal TDP-43 expression requires axonal lysosome transport",
      "sPLA2-IIA modifies progranulin deficiency phenotypes in mouse models",
      "Benchmarking SILAC Proteomics Workflows and Data Analysis Platforms",
      "Overcoming Analytical Challenges in Proximity Labeling Proteomics"
    ),
    link = c(
      "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=IgEvNuQAAAAJ&sortby=pubdate",
      "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=IgEvNuQAAAAJ&sortby=pubdate",
      "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=IgEvNuQAAAAJ&sortby=pubdate",
      "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=IgEvNuQAAAAJ&sortby=pubdate",
      "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=IgEvNuQAAAAJ&sortby=pubdate"
    ),
    venue = c(
      "Nature Cell Biology 27, 1465-1481, 2025",
      "eLife, 2024.09.30.615241, 2025",
      "Molecular Neurodegeneration 20 (1), 72, 2025",
      "Molecular & Cellular Proteomics 24 (6), 2025",
      "Journal of Mass Spectrometry 60 (5), e5134, 2025"
    ),
    year = c(2025L, 2025L, 2025L, 2025L, 2025L),
    stringsAsFactors = FALSE
  )

  fetch_recent_pubs <- function(works_url, n = 5) {
    if (!requireNamespace("rvest", quietly = TRUE)) return(NULL)
    if (!requireNamespace("xml2", quietly = TRUE)) return(NULL)
    
    # Encourage a fuller first page
    if (!grepl("pagesize=", works_url, fixed = TRUE)) {
      works_url <- paste0(works_url, "&cstart=0&pagesize=100")
    }
    
    ua <- paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/120 Safari/537.36"
    )
    
    doc <- tryCatch({
      sess <- rvest::session(works_url, user_agent = ua)
      rvest::read_html(sess)
    }, error = function(e) NULL, warning = function(w) NULL)
    
    if (is.null(doc)) return(NULL)
    
    rows <- rvest::html_elements(doc, ".gsc_a_tr")
    if (length(rows) == 0) return(NULL)
    
    get_text <- function(node, css) {
      x <- rvest::html_element(node, css)
      if (is.na(x) || length(x) == 0) return(NA_character_)
      txt <- rvest::html_text2(x)
      if (!nzchar(txt)) NA_character_ else txt
    }
    
    get_href <- function(node, css) {
      x <- rvest::html_element(node, css)
      if (is.na(x) || length(x) == 0) return(NA_character_)
      href <- rvest::html_attr(x, "href")
      if (!nzchar(href)) NA_character_ else href
    }
    
    pubs <- lapply(rows, function(r) {
      title <- get_text(r, ".gsc_a_at")
      href  <- get_href(r, ".gsc_a_at")
      year  <- get_text(r, ".gsc_a_y span")
      
      # Gray lines: authors then journal/venue (best-effort)
      gray_lines <- rvest::html_elements(r, ".gsc_a_t .gs_gray") |>
        rvest::html_text2()
      venue <- if (length(gray_lines) >= 2 && nzchar(gray_lines[2])) gray_lines[2] else NA_character_
      
      link <- if (!is.na(href)) paste0("https://scholar.google.com", href) else NA_character_
      
      data.frame(
        title = title,
        link  = link,
        venue = venue,
        year  = suppressWarnings(as.integer(year)),
        stringsAsFactors = FALSE
      )
    })
    
    df <- do.call(rbind, pubs)
    df <- df[!is.na(df$title) & nzchar(df$title), , drop = FALSE]
    
    # Works page is already sorted by pubdate; keep that order.
    head(df, n)
  }
  
  # Map
  lat <- 38.9899144
  lng <- -76.9400382
  
  session$output$about_dept_map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
      leaflet::setView(lng = lng, lat = lat, zoom = 10) |>
      leaflet::addCircleMarkers(
        lng = lng, lat = lat,
        radius = 9,
        stroke = TRUE, weight = 2,
        color = "#d50032",
        fillColor = "#d50032",
        fillOpacity = 0.9,
        popup = htmltools::HTML("<b>The Hao Research Group</b><br/>Department of Chemistry &amp; Biochemistry"),
        label = "The Hao Research Group - Department of Chemistry & Biochemistry",
        labelOptions = leaflet::labelOptions(direction = "top", offset = c(0, -10), textsize = "13px")
      ) |>
      leaflet::addControl(
        html = htmltools::HTML(
          "<b>The Hao Research Group</b><br/><span class='muted'>Department of Chemistry &amp; Biochemistry</span>"
        ),
        position = "topright"
      )
  })
  
  # Publications
  pubs_rv <- shiny::reactiveVal(NULL)
  
  shiny::observeEvent(TRUE, {
    pubs_rv(fetch_recent_pubs(scholar_works_url, n = 5))
  }, once = TRUE)
  
  session$output$about_recent_pubs_ui <- shiny::renderUI({
    df <- pubs_rv()

    # Use static fallback if scraping failed (common on servers due to Google blocking)
    if (is.null(df) || nrow(df) == 0) {
      df <- static_pubs
    }

    # Format: Title (Venue, Year) with no "Year:" label and no citation count
    tags$ul(
      lapply(seq_len(nrow(df)), function(i) {
        title_i <- df$title[i]
        link_i  <- if (!is.na(df$link[i]) && nzchar(df$link[i])) df$link[i] else scholar_works_url
        venue_i <- df$venue[i]
        year_i  <- df$year[i]

        has_year_in_venue <- !is.na(venue_i) && grepl("\\b(19|20)\\d{2}\\b", venue_i)
        meta <- if (!is.na(venue_i) && nzchar(venue_i)) {
          if (has_year_in_venue || is.na(year_i)) venue_i else paste0(venue_i, ", ", year_i)
        } else {
          if (!is.na(year_i)) as.character(year_i) else ""
        }

        tags$li(
          tags$a(
            href = link_i,
            target = "_blank",
            rel = "noopener",
            paste0(title_i, if (nzchar(meta)) paste0(" (", meta, ")") else "")
          )
        )
      })
    )
  })
  
  invisible(NULL)
}
