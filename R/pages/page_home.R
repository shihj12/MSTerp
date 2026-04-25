# R/pages/page_home.R
page_home_ui <- function() {
  tags$div(
    id = "home-landing",
    tags$head(
      tags$link(
        rel = "preload",
        href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@700;900&display=swap",
        as = "style",
        onload = "this.rel='stylesheet'"
      ),
      tags$style(htmltools::HTML("
        :root {
          --home-red: var(--primary, #c9414d);
          --home-red-dark: var(--primary-dark, #a33540);
          --home-ink: var(--text-primary, #1a1a1a);
          --home-muted: var(--text-secondary, #5a5a5a);
          --home-line: var(--border-light, #e8e4df);
        }

        #home-landing {
          min-height: 100vh;
          position: fixed;
          top: var(--titlebar-h, 0px);
          left: 0;
          right: 0;
          bottom: 0;
          overflow: auto;
          display: flex;
          flex-direction: column;
          z-index: 100;
          color: var(--home-ink);
          background: linear-gradient(180deg, #fadcdf 0%, #ffffff 30%, #ffffff 70%, #fadcdf 100%) !important;
        }
        [data-theme='dark'] #home-landing {
          background: linear-gradient(180deg, #2a1a1c 0%, var(--bg-page, #1a1a1a) 30%, var(--bg-page, #1a1a1a) 70%, #2a1a1c 100%) !important;
          color: var(--text-primary, #e8e8e8);
        }

        /* === Banner bar (matches shell topbar) === */
        #home-landing .home-banner {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 0 16px;
          height: var(--topbar-h, 56px);
          flex-shrink: 0;
          background: #1b1b1b;
          border-bottom: 1px solid rgba(201, 65, 77, 0.15);
          gap: 12px;
        }
        #home-landing .home-banner-left {
          display: flex;
          align-items: center;
          gap: 10px;
          flex-shrink: 0;
        }
        #home-landing .home-banner-left img {
          height: 45px;
          width: auto;
          display: block;
        }

        /* Theme toggle (in-flow inside dark banner — always dark-styled) */
        .home-theme-toggle {
          background: #2a2a2a;
          border: 1px solid #3a3a3a;
          color: #e8e8e8;
          width: 36px;
          height: 36px;
          border-radius: 8px;
          cursor: pointer;
          font-size: 18px;
          display: flex;
          align-items: center;
          justify-content: center;
          box-shadow: none;
          transition: all 0.15s ease;
          padding: 0;
          line-height: 1;
          flex-shrink: 0;
        }
        .home-theme-toggle:hover {
          border-color: var(--primary, #c9414d);
        }

        #home-landing .home-container {
          position: relative;
          z-index: 1;
          max-width: 1280px;
          margin: 0 auto;
          padding: 48px 48px 28px;
          width: 100%;
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
        }

        #home-landing .main-stack {
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
          justify-content: center;
          gap: 28px;
        }

        #home-landing .hero {
          display: grid;
          grid-template-columns: 1fr;
          gap: 24px;
          align-items: center;
          justify-items: center;
          text-align: center;
        }

        #home-landing .hero-title {
          font-family: 'Source Sans Pro', sans-serif;
          font-weight: 900;
          font-size: 64px;
          white-space: nowrap;
          line-height: 1.1;
          margin: 0 0 12px 0;
          letter-spacing: 0.3px;
        }
    
        #home-landing .hero-tagline {
          font-family: var(--mono, 'JetBrains Mono', monospace);
          font-size: 18px;
          font-weight: 600;
          color: var(--home-red);
          letter-spacing: 0.5px;
          margin-bottom: 8px;
        }

        #home-landing .hero-desc {
          font-size: 14px;
          line-height: 1.7;
          color: var(--home-muted);
          max-width: 540px;
          margin: 0 auto 4px;
        }
        [data-theme='dark'] #home-landing .hero-desc {
          color: var(--text-secondary, #aaaaaa);
        }

        #home-landing .cta-row {
          display: flex;
          gap: 12px;
          flex-wrap: wrap;
          align-items: center;
          justify-content: center;
          margin-top: 6px;
        }

        #home-landing .btn {
          border-radius: 10px;
          font-weight: 700;
          padding: 14px 22px;
          border: 1px solid transparent;
          cursor: pointer;
          text-decoration: none;
          display: inline-block;
        }

        #home-landing .btn-primary {
          background: var(--home-red);
          color: #ffffff;
          border-color: var(--home-red-dark);
          box-shadow: 0 10px 24px rgba(201, 65, 77, 0.22);
          transition: all var(--duration-fast, 150ms) ease;
        }

        #home-landing .btn-primary:hover { background: var(--home-red-dark); }

        #home-landing .btn-ghost {
          background: transparent;
          border-color: var(--home-line);
          color: var(--home-ink);
          transition: all var(--duration-fast, 150ms) ease;
        }

        #home-landing .btn-ghost:hover {
          border-color: var(--home-red);
          color: var(--home-red);
        }

        /* === Highlight pills === */
        #home-landing .highlights {
          display: flex;
          flex-wrap: wrap;
          gap: 8px;
          justify-content: center;
        }
        #home-landing .recovery-banner {
          background: rgba(201, 65, 77, 0.08);
          border: 1px solid rgba(201, 65, 77, 0.35);
          border-radius: 12px;
          padding: 12px 16px;
          display: flex;
          align-items: center;
          gap: 12px;
          flex-wrap: wrap;
          color: var(--home-ink);
        }
        [data-theme='dark'] #home-landing .recovery-banner {
          background: rgba(201, 65, 77, 0.15);
          color: var(--text-primary, #e8e8e8);
        }
        #home-landing .recovery-banner .rb-msg { flex: 1 1 auto; font-size: 13px; }
        #home-landing .recovery-banner .rb-msg b { color: var(--home-red); }
        #home-landing .recovery-banner .rb-actions { display: flex; gap: 8px; }
        #home-landing .recovery-list { display: flex; flex-direction: column; gap: 8px; }
        #home-landing .recovery-row {
          display: flex; align-items: center; justify-content: space-between;
          gap: 12px; padding: 10px 12px;
          border: 1px solid var(--home-line); border-radius: 10px;
          background: var(--bg-card, #ffffff);
        }
        [data-theme='dark'] #home-landing .recovery-row {
          background: var(--bg-card, #242424); border-color: var(--border-light, #3a3a3a);
        }
        #home-landing .recovery-row .rr-meta { font-size: 12px; color: var(--home-muted); }

        #home-landing .hi-pill {
          display: inline-block;
          padding: 6px 14px;
          font-size: 12px;
          font-weight: 600;
          border-radius: 999px;
          border: 1px solid rgba(201, 65, 77, 0.25);
          color: var(--home-red);
          background: rgba(201, 65, 77, 0.06);
          letter-spacing: 0.3px;
          white-space: nowrap;
        }
        [data-theme='dark'] #home-landing .hi-pill {
          background: rgba(201, 65, 77, 0.12);
          border-color: rgba(201, 65, 77, 0.3);
          color: #e8a0a6;
        }

        #home-landing .section {
          margin-top: 0;
        }

        #home-landing .section-head {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
        }

        #home-landing .section-title {
          font-size: 22px;
          margin: 0;
        }

        #home-landing .workflow {
          display: grid;
          grid-template-columns: repeat(4, minmax(180px, 1fr));
          gap: 14px;
          margin-top: 18px;
        }

        #home-landing .wf-card {
          background: var(--bg-card, #ffffff);
          border: 1px solid rgba(201, 65, 77, 0.35);
          border-radius: 16px;
          padding: 16px;
          min-height: 200px;
          display: flex;
          flex-direction: column;
          gap: 8px;
          box-shadow: var(--shadow-md, 0 4px 12px rgba(26, 26, 26, 0.06));
          transition: transform var(--duration-normal, 250ms) var(--ease-out, cubic-bezier(0.16, 1, 0.3, 1)), box-shadow var(--duration-normal, 250ms) ease, border-color var(--duration-fast, 150ms) ease;
        }
        #home-landing .wf-card:hover {
          transform: translateY(-4px);
          box-shadow: var(--shadow-lg, 0 8px 24px rgba(26, 26, 26, 0.08));
          border-color: rgba(201, 65, 77, 0.6);
        }

        #home-landing .wf-step {
          font-weight: 800;
          color: var(--home-red);
          font-size: 14px;
        }
        #home-landing .wf-title { font-weight: 700; font-size: 16px; }
        #home-landing .wf-desc { color: var(--home-muted); font-size: 13px; }
        #home-landing .wf-link {
          margin-top: auto;
          color: var(--home-ink);
          font-weight: 700;
          text-decoration: none;
        }
        #home-landing .wf-button {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          padding: 10px 14px;
          border: 1px solid var(--home-line);
          border-radius: 10px;
          background: var(--bg-card, #ffffff);
          font-weight: 700;
          transition: all var(--duration-fast, 150ms) ease;
        }
        #home-landing .wf-button:hover {
          border-color: var(--home-red);
          color: #ffffff;
          background: var(--home-red);
        }

        #home-landing .footer {
          margin-top: 46px;
          padding-top: 18px;
          border-top: 1px solid var(--home-line);
          color: var(--home-muted);
          font-size: 13px;
          display: flex;
          flex-direction: column;
          gap: 10px;
          flex-wrap: wrap;
        }
        #home-landing .footer-row {
          display: flex;
          justify-content: space-between;
          align-items: center;
          gap: 12px;
          width: 100%;
        }
        #home-landing .footer-links {
          display: flex;
          gap: 14px;
          flex-wrap: wrap;
          justify-content: center;
        }
        #home-landing .footer-links a {
          color: var(--home-ink);
          text-decoration: none;
          font-weight: 700;
        }
        #home-landing .footer-links a:hover {
          color: var(--home-red);
        }

        #home-landing .fade-in {
          animation: rise var(--duration-slow, 400ms) var(--ease-out, cubic-bezier(0.16, 1, 0.3, 1)) both;
        }
        #home-landing .delay-1 { animation-delay: 80ms; }
        #home-landing .delay-2 { animation-delay: 160ms; }
        #home-landing .delay-3 { animation-delay: 240ms; }
        #home-landing .delay-4 { animation-delay: 320ms; }

        @keyframes rise {
          0% { opacity: 0; transform: translateY(16px); }
          100% { opacity: 1; transform: translateY(0); }
        }

        @media (max-width: 980px) {
          #home-landing .workflow { grid-template-columns: repeat(2, minmax(160px, 1fr)); }
          #home-landing .home-container { padding: 36px 28px 22px; }
        }

        @media (max-width: 640px) {
          #home-landing .hero-title { font-size: 44px;}
          #home-landing .hero-tagline { font-size: 15px; }
          #home-landing .hero-desc { font-size: 13px; max-width: 100%; }
          #home-landing .workflow { grid-template-columns: 1fr; }
          #home-landing .footer-row { flex-direction: column; align-items: flex-start; }
          #home-landing .footer-links { justify-content: flex-start; }
          #home-landing .home-banner { padding: 0 10px; height: 48px; }
          #home-landing .home-banner-left img { height: 34px; }
          #home-landing .highlights { gap: 6px; }
          #home-landing .hi-pill { font-size: 11px; padding: 5px 10px; }
        }

        /* Dark mode overrides for home page */
        [data-theme='dark'] #home-landing .wf-card {
          background: var(--bg-card, #242424);
          border-color: rgba(201, 65, 77, 0.4);
        }
        [data-theme='dark'] #home-landing .wf-card:hover {
          border-color: rgba(201, 65, 77, 0.7);
        }
        [data-theme='dark'] #home-landing .wf-button {
          background: var(--bg-card, #242424);
          border-color: var(--border-light, #3a3a3a);
          color: var(--text-primary, #e8e8e8);
        }
        [data-theme='dark'] #home-landing .wf-button:hover {
          background: var(--primary, #c9414d);
          color: #ffffff;
        }
        [data-theme='dark'] #home-landing .btn-ghost {
          border-color: var(--border-light, #3a3a3a);
          color: var(--text-primary, #e8e8e8);
        }
        [data-theme='dark'] #home-landing .footer {
          border-top-color: var(--border-light, #3a3a3a);
          color: var(--text-secondary, #aaaaaa);
        }
        [data-theme='dark'] #home-landing .footer-links a {
          color: var(--text-primary, #e8e8e8);
        }
        [data-theme='dark'] #home-landing .wf-desc {
          color: var(--text-secondary, #aaaaaa);
        }
        [data-theme='dark'] #home-landing .wf-link {
          color: var(--text-primary, #e8e8e8);
        }
      "))
    ),
    # Banner bar: [logo_left] [logo_right] --- spacer --- [theme_toggle]
    div(
      class = "home-banner fade-in",
      div(
        class = "home-banner-left",
        tags$img(src = "static/img/logo_left.png", alt = "University of Maryland"),
        tags$img(src = "static/img/logo_right.png", alt = "Hao Research Group")
      ),
      tags$button(
        id = "home_theme_toggle",
        class = "home-theme-toggle",
        title = "Toggle dark mode",
        onclick = "msterpToggleTheme()",
        HTML("&#9790;")
      )
    ),
    div(
      class = "home-container",
      div(
        class = "main-stack",
        uiOutput("home_recovery_banner"),
        # Hero section
        div(
          class = "hero fade-in delay-1",
          div(
            class = "hero-left",
            div(class = "hero-title", "MS Terp"),
            div(class = "hero-tagline", "Interactive Multi-omics Analysis Platform"),
            div(
              class = "hero-desc",
              "Pipeline based platform for data analysis and figure generation.",
              tags$br(),
              "Intensity data to publication ready figures, no coding required."
            ),
            div(
              class = "cta-row",
              actionButton("home_go_newrun", "Start new run", class = "btn btn-primary"),
              actionButton("home_go_results", "Open results", class = "btn btn-ghost"),
              actionButton("home_go_tutorial", "Walkthrough", class = "btn btn-ghost")
            )
          )
        ),
        # Feature highlight pills
        div(
          class = "highlights delay-2 fade-in",
          tags$span(class = "hi-pill", "Proteomics + Metabolomics"),
          tags$span(class = "hi-pill", "Reproducible pipelines"),
          tags$span(class = "hi-pill", "Interactive Data Exploration"),
          tags$span(class = "hi-pill", "30+ analysis engines")
        ),
        # Workflow cards
        div(
          class = "section delay-3 fade-in",
          div(
            class = "section-head",
            tags$h3(class = "section-title", "")
          ),
          div(
            class = "workflow",
            div(
              class = "wf-card",
              div(class = "wf-step", "01"),
              div(class = "wf-title", "Prepare data"),
              div(class = "wf-desc", "Import data in CSV/Excel form and define experimental groups."),
              actionButton("home_go_format", "Format data", class = "wf-link wf-button")
            ),
            div(
              class = "wf-card",
              div(class = "wf-step", "02"),
              div(class = "wf-title", "Build TerpBase"),
              div(class = "wf-desc", "Build custom proteomics databases from UniProt."),
              actionButton("home_go_db", "Open TerpBase", class = "wf-link wf-button")
            ),
            div(
              class = "wf-card",
              div(class = "wf-step", "03"),
              div(class = "wf-title", "Design pipeline"),
              div(class = "wf-desc", "Compose analysis steps into reproducible pipelines."),
              actionButton("home_go_pipeline", "Open TerpFlow", class = "wf-link wf-button")
            ),
            div(
              class = "wf-card",
              div(class = "wf-step", "04"),
              div(class = "wf-title", "Run"),
              div(class = "wf-desc", "Generate interactive plots and tables. Export publication-ready figures."),
              actionButton("home_go_newrun2", "Start run", class = "wf-link wf-button")
            )
          )
        )
      ),
      div(
        class = "footer delay-4 fade-in",
        div(
          class = "footer-row",
          tags$span("The Hao Research Group @ The University of Maryland"),
          tags$span("Version 1.0")
        ),
        div(
          class = "footer-links",
          actionLink("home_go_tools", "Tools"),
          actionLink("home_go_about", "About us")
        )
      )
    )
  )
}

page_home_server <- function(input, output, session, app_state) {
  if (is.null(app_state)) return(invisible(NULL))

  # Refresh recovery candidates on demand (after the user dismisses or recovers).
  refresh_candidates <- function() {
    app_state$recovery_candidates <- tryCatch(tb_workspace_scan_dirty(),
                                              error = function(e) list())
  }

  # Modal generation counter — bumps on every modal open so per-row input IDs
  # are unique across opens. Prevents stale observers from double-firing.
  modal_gen <- 0L

  output$home_recovery_banner <- renderUI({
    cands <- app_state$recovery_candidates
    if (is.null(cands) || length(cands) == 0) return(NULL)
    n <- length(cands)
    div(
      class = "recovery-banner fade-in",
      div(class = "rb-msg",
          tags$b(sprintf("%d unsaved terpbook edit%s", n, if (n == 1) "" else "s")),
          tags$span(" found from a previous session.")),
      div(class = "rb-actions",
          actionButton("home_recovery_open", "Review",
                       class = "btn btn-primary btn-sm"),
          actionButton("home_recovery_dismiss", "Dismiss",
                       class = "btn btn-ghost btn-sm"))
    )
  })

  observeEvent(input$home_recovery_dismiss, {
    # Hide the banner this session without deleting workspace data.
    app_state$recovery_candidates <- list()
  }, ignoreInit = TRUE)

  observeEvent(input$home_recovery_open, {
    cands <- app_state$recovery_candidates %||% list()
    if (length(cands) == 0) { removeModal(); return() }
    modal_gen <<- modal_gen + 1L
    gen <- modal_gen
    rows <- lapply(seq_along(cands), function(i) {
      c1 <- cands[[i]]
      name <- c1$source_basename %||% "(unknown)"
      src  <- c1$source_path
      le   <- c1$last_edit_at
      le_str <- if (!is.null(le) && !is.na(le)) format(le, "%Y-%m-%d %H:%M") else "unknown"
      meta <- if (!is.null(src) && !is.na(src)) {
        if (file.exists(src)) sprintf("Source: %s · last edit %s", src, le_str)
        else sprintf("Source missing: %s · last edit %s", src, le_str)
      } else {
        sprintf("Browser-uploaded · last edit %s", le_str)
      }
      div(
        class = "recovery-row",
        div(
          tags$div(tags$b(name)),
          tags$div(class = "rr-meta", meta)
        ),
        div(
          style = "display: flex; gap: 6px;",
          actionButton(sprintf("home_recovery_recover_%d_%d", gen, i), "Recover",
                       class = "btn btn-primary btn-sm"),
          actionButton(sprintf("home_recovery_drop_%d_%d", gen, i), "Discard",
                       class = "btn btn-warning btn-sm")
        )
      )
    })
    showModal(modalDialog(
      title = "Recover unsaved edits",
      size = "l",
      easyClose = TRUE,
      div(class = "recovery-list", rows),
      footer = modalButton("Close")
    ))

    # Bind one observer per row. Input IDs include the modal generation so
    # observers from prior opens cannot double-fire on a new open.
    for (i in seq_along(cands)) {
      local({
        idx <- i
        cand <- cands[[idx]]
        gen_local <- gen
        observeEvent(input[[sprintf("home_recovery_recover_%d_%d", gen_local, idx)]], {
          app_state$pending_recovery <- list(
            workspace_dir = cand$dir,
            source_path = if (!is.null(cand$source_path) && !is.na(cand$source_path)) cand$source_path else NULL,
            display_name = cand$source_basename
          )
          removeModal()
          # Trigger the same navigation that home_go_results fires (observed
          # in app.R server). Random value ensures it always re-fires.
          session$sendCustomMessage("msterp_trigger_input",
                                    list(id = "home_go_results", value = stats::runif(1)))
          refresh_candidates()
        }, ignoreInit = TRUE, once = TRUE)
        observeEvent(input[[sprintf("home_recovery_drop_%d_%d", gen_local, idx)]], {
          tryCatch(unlink(cand$dir, recursive = TRUE, force = TRUE), error = function(e) NULL)
          refresh_candidates()
          removeModal()
          if (length(app_state$recovery_candidates) > 0) {
            session$sendCustomMessage("msterp_trigger_input",
                                      list(id = "home_recovery_open", value = stats::runif(1)))
          }
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  }, ignoreInit = TRUE)

  invisible(NULL)
}
