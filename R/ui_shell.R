# R/ui_shell.R
msterp_theme_head <- function() {
  tags$head(
    tags$link(
      rel = "preload",
      href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@300;400;500;600;700;800&display=swap",
      as = "style",
      onload = "this.rel='stylesheet'"
    ),
    tags$noscript(tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@300;400;500;600;700;800&display=swap"
    )),
    tags$script(HTML("
      // Apply saved theme immediately to prevent flash of wrong theme
      (function(){
        var t = localStorage.getItem('msterp-theme') || 'light';
        if (t === 'dark') document.documentElement.setAttribute('data-theme','dark');
      })();
    ")),
    tags$style(HTML("
      /* === Global CSS Variables (Spec) === */
      :root {
        /* Softer color palette */
        --primary: #c9414d;           /* Muted burgundy (was #d50032) */
        --primary-dark: #a33540;
        --primary-light: #f8e8ea;

        --accent-gold: #d4a84b;       /* Muted amber (was #FFD200) */
        --accent-gold-light: #faf3e3;

        --charcoal: #3d3d3d;          /* Soft black (was #000000) */
        --charcoal-light: #5a5a5a;

        /* Neutral backgrounds - warm cream tones */
        --bg-page: #faf9f7;
        --bg-card: #ffffff;
        --bg-muted: #f5f3f0;
        --bg-hover: #f0eeeb;

        /* Text */
        --text-primary: #1a1a1a;
        --text-secondary: #5a5a5a;
        --text-muted: #8a8a8a;

        /* Borders */
        --border-light: #e8e4df;
        --border-medium: #d4cfc7;

        /* Shadows */
        --shadow-sm: 0 1px 3px rgba(26, 26, 26, 0.04);
        --shadow-md: 0 4px 12px rgba(26, 26, 26, 0.06);
        --shadow-lg: 0 8px 24px rgba(26, 26, 26, 0.08);

        /* Animation tokens */
        --ease-out: cubic-bezier(0.16, 1, 0.3, 1);
        --ease-in-out: cubic-bezier(0.65, 0, 0.35, 1);
        --duration-fast: 150ms;
        --duration-normal: 250ms;
        --duration-slow: 400ms;

        /* Legacy aliases for backward compatibility */
        --md-red: var(--primary);
        --md-gold: var(--accent-gold);
        --md-gold-light: var(--accent-gold-light);
        --md-green: #0f8a3a;
        --md-bg: var(--bg-page);
        --md-text: var(--text-primary);
        --md-border: var(--border-medium);
        --md-card-bg: var(--bg-card);
        --md-card-border: var(--border-medium);
        --md-card-shadow: var(--shadow-md);
        --md-nav-bg: #1b1b1b;
        --md-nav-item-bg: #292929;
        --md-nav-item-border: #383838;
        --md-nav-hover: #2f2f2f;

        /* Tabbed cardset variables */
        --tab-bg: var(--bg-card);
        --tab-border: var(--border-medium);
        --tab-hover-bg: var(--bg-muted);
        --tab-active-bg: var(--bg-muted);
        --tab-accent: var(--primary);
        --tab-accent-strong: var(--primary-dark);
        --tab-text: var(--text-primary);
        --tab-text-hover: var(--primary);
        --tab-text-active: var(--primary);
        --tab-text-muted: var(--text-secondary);
        --tab-radius: 12px;
        --tab-pill-pad: 4px;
        --mono: 'JetBrains Mono', monospace;

        /* Semantic colors for dark-mode toggling */
        --color-white: #ffffff;
        --color-text-hint: #666666;
        --color-text-subtle: #555555;
        --color-text-meta: #444444;
        --color-text-pct: #333333;
        --status-success-bg: #e8f5e9;
        --status-success-text: #2e7d32;
        --status-success-dot: #4caf50;
        --status-error-bg: #ffebee;
        --status-error-text: #c62828;
        --status-error-dot: #ef5350;
        --status-warn-bg: #fef3cd;
        --status-warn-text: #856404;
        --status-warn-border: #ffc107;
        --status-info-bg: #d1ecf1;
        --status-info-text: #17a2b8;
        --progress-fill: #0d6efd;
        --progress-fill-dark: #0a58ca;
        --btn-default-bg: #ffffff;
        --btn-default-hover-bg: #f7f4f0;
        --btn-default-hover-border: #d3c8b8;
        --btn-primary-border: #b30028;
        --btn-primary-hover: #b30028;
        --btn-primary-hover-border: #9a0022;
        --scrollbar-thumb: #c0c0c0;
        --scrollbar-hover: #999999;
        --progress-track: #e9e2d7;
        --logs-bg: #1f1f1f;
        --logs-border: #383838;
        --logs-text: #e0e0e0;
        --pill-text: #9c6a00;
        --notification-error-bg: #fff5f5;
        --modal-backdrop: #1a1a1a;
      }

      /* === Dark Mode Overrides === */
      [data-theme='dark'] {
        --primary-light: #3d1f22;
        --accent-gold-light: #3a3020;

        --bg-page: #1a1a1a;
        --bg-card: #242424;
        --bg-muted: #2a2a2a;
        --bg-hover: #333333;

        --text-primary: #e8e8e8;
        --text-secondary: #aaaaaa;
        --text-muted: #777777;

        --border-light: #404040;
        --border-medium: #555555;

        --shadow-sm: 0 1px 3px rgba(0, 0, 0, 0.2);
        --shadow-md: 0 4px 12px rgba(0, 0, 0, 0.3);
        --shadow-lg: 0 8px 24px rgba(0, 0, 0, 0.4);

        --md-bg: var(--bg-page);
        --md-text: var(--text-primary);
        --md-border: var(--border-medium);
        --md-card-bg: var(--bg-card);
        --md-card-border: var(--border-medium);
        --md-card-shadow: var(--shadow-md);
        --md-nav-bg: #111111;
        --md-nav-item-bg: #1a1a1a;
        --md-nav-item-border: #333333;
        --md-nav-hover: #2a2a2a;

        --tab-bg: var(--bg-card);
        --tab-border: var(--border-medium);
        --tab-hover-bg: var(--bg-muted);
        --tab-active-bg: var(--bg-muted);
        --tab-text: var(--text-primary);
        --tab-text-hover: var(--primary);
        --tab-text-active: var(--primary);
        --tab-text-muted: var(--text-secondary);

        --color-white: var(--bg-card);
        --color-text-hint: #999999;
        --color-text-subtle: #999999;
        --color-text-meta: #aaaaaa;
        --color-text-pct: #cccccc;
        --status-success-bg: #1a3a1f;
        --status-success-text: #66bb6a;
        --status-error-bg: #3a1a1a;
        --status-error-text: #ef5350;
        --status-warn-bg: #3a3020;
        --status-warn-text: #d4a84b;
        --status-info-bg: #1a2a30;
        --status-info-text: #4dd0e1;
        --progress-fill: #4d8eff;
        --progress-fill-dark: #3a6fd8;
        --btn-default-bg: var(--bg-card);
        --btn-default-hover-bg: var(--bg-hover);
        --btn-default-hover-border: var(--border-medium);
        --scrollbar-thumb: #555555;
        --scrollbar-hover: #777777;
        --progress-track: #3a3a3a;
        --logs-bg: #111111;
        --logs-border: #444444;
        --logs-text: #e0e0e0;
        --pill-text: #d4a84b;
        --notification-error-bg: #3a1a1a;
        --modal-backdrop: #000000;
      }

      [data-theme='dark'] .msterp-wrap,
      [data-theme='dark'] .shell {
        --banner: #111111;
        --panel: var(--bg-card);
        --panel2: var(--bg-muted);
        --border: var(--border-medium);
      }

      /* Dark mode: make the outer content wrapper darker so panels/cards inside float above it */
      [data-theme='dark'] .msterp-content,
      [data-theme='dark'] .main {
        background: var(--bg-page);
      }

      /* Slim scrollbars (global) */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: transparent; }
      ::-webkit-scrollbar-thumb { background: var(--scrollbar-thumb); border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: var(--scrollbar-hover); }
      * { scrollbar-width: thin; scrollbar-color: var(--scrollbar-thumb) transparent; }

      /* Selectize: prevent caret arrow from covering text */
      .selectize-input { padding-right: 28px !important; }
      .selectize-input::after {
        right: 8px !important;
        border-width: 4px 4px 0 4px !important;
      }

      /* === Animation Keyframes === */
      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(16px); }
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      @keyframes shimmer {
        0% { background-position: -200% 0; }
        100% { background-position: 200% 0; }
      }
      @keyframes spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      @keyframes indeterminate {
        0% { transform: translateX(-100%); }
        100% { transform: translateX(200%); }
      }

      /* === Animation Utility Classes === */
      .animate-fade-in {
        animation: fadeInUp var(--duration-slow) var(--ease-out) both;
      }
      .animate-delay-1 { animation-delay: 50ms; }
      .animate-delay-2 { animation-delay: 100ms; }
      .animate-delay-3 { animation-delay: 150ms; }
      .animate-delay-4 { animation-delay: 200ms; }
      .animate-delay-5 { animation-delay: 250ms; }
      .stagger-item {
        animation: fadeInUp var(--duration-normal) var(--ease-out) both;
      }
      .stagger-item:nth-child(1) { animation-delay: 0ms; }
      .stagger-item:nth-child(2) { animation-delay: 50ms; }
      .stagger-item:nth-child(3) { animation-delay: 100ms; }
      .stagger-item:nth-child(4) { animation-delay: 150ms; }
      .stagger-item:nth-child(5) { animation-delay: 200ms; }

      /* === Skeleton Loaders === */
      .skeleton {
        background: linear-gradient(90deg, var(--bg-muted) 25%, var(--bg-hover) 50%, var(--bg-muted) 75%);
        background-size: 200% 100%;
        animation: shimmer 1.5s infinite;
        border-radius: 4px;
      }
      .skeleton-text {
        height: 1em;
        margin-bottom: 0.5em;
        border-radius: 4px;
      }
      .skeleton-text:last-child { width: 60%; }
      .skeleton-circle {
        width: 40px;
        height: 40px;
        border-radius: 50%;
      }
      .skeleton-rect {
        height: 100px;
        border-radius: 8px;
      }

      /* === Spinner === */
      .spinner-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 12px;
        padding: 24px;
      }
      .spinner {
        width: 32px;
        height: 32px;
        border: 3px solid var(--border-light);
        border-top-color: var(--primary);
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
      }
      .spinner-text {
        font-size: 14px;
        color: var(--text-secondary);
      }

      /* === Progress Bar === */
      .progress-bar-container {
        width: 100%;
        height: 6px;
        background: var(--bg-muted);
        border-radius: 999px;
        overflow: hidden;
      }
      .progress-bar-fill {
        height: 100%;
        background: var(--primary);
        border-radius: 999px;
        transition: width var(--duration-normal) var(--ease-out);
      }
      .progress-bar-fill.indeterminate {
        width: 40%;
        animation: indeterminate 1.2s var(--ease-in-out) infinite;
      }

      html, body {
        height: 100%;
        font-family: var(--mono, 'JetBrains Mono', monospace);
        background: var(--md-bg);
        color: var(--md-text);
      }

      /* Important for grid/scroll correctness */
      *, *::before, *::after { box-sizing: border-box; }

      /* === Shell wrapper (.shell alias) === */
      .msterp-wrap, .shell {
        --topbar-h: 56px;
        --bg: var(--md-bg);
        --banner: #1b1b1b;
        --panel: #ffffff;
        --panel2: var(--bg-muted);
        --border: var(--md-border);
        --gap: 0px;

        background: var(--bg);
        min-height: 100vh;
        padding: 0;
        border-radius: 0;
      }

      /* Top banner */
      .msterp-topbar {
        height: var(--topbar-h);
        background: var(--banner);
        color: #fff;
        border-radius: 0;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 16px;
        margin-bottom: 0;
        gap: 12px;
      }
      .msterp-topbar-left {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-shrink: 0;
      }
      .msterp-topbar-left img { height: 45px; width: auto; display: block; }
      .msterp-topbar-title {
        font-weight: 800;
        font-size: 25px;
        letter-spacing: 0.3px;
        white-space: nowrap;
        font-family: var(--mono, 'JetBrains Mono', monospace);
        color: #fff;
        margin-left: 10px;
      }
      .msterp-topbar-nav {
        display: flex;
        align-items: center;
        gap: 4px;
        overflow-x: auto;
      }
      .msterp-topbar-nav a.action-button {
        display: flex;
        align-items: center;
        gap: 6px;
        padding: 6px 10px;
        color: #fff;
        background: transparent;
        border: 1px solid transparent;
        border-radius: 6px;
        text-decoration: none;
        white-space: nowrap;
        font-weight: 600;
        font-size: 13px;
        transition: background 0.15s ease;
        margin: 0;
      }
      .msterp-topbar-nav a:hover {
        background: var(--md-nav-hover);
      }
      .msterp-topbar-nav a.active {
        background: var(--primary);
        color: #fefefe;
        border-color: var(--primary-dark);
      }
      .msterp-topbar-nav .nav-icon {
        width: 18px;
        height: 18px;
        min-width: 18px;
        min-height: 18px;
        object-fit: contain;
        filter: invert(1);
      }
      .msterp-topbar-nav .nav-text {
        font-size: 13px;
        line-height: 1.2;
      }

      /* === Shell layout: sidebar + content (.shell > .nav + .main) === */
      .msterp-shell {
        display: flex;
        flex-direction: row;
        gap: 0;
        height: calc(100vh - var(--topbar-h));
        min-height: 0;
      }

      /* === Sidebar (hidden - nav moved to topbar) === */
      .msterp-sidebar, .nav {
        display: none;
      }

      /* === Main content (.main alias) === */
      .msterp-content, .main {
        flex: 1;
        background: var(--panel);
        border: none;
        border-radius: 0;
        padding: 16px;
        overflow: auto;
        min-height: 0;
        min-width: 0;
        display: flex;
        flex-direction: column;
        gap: 16px;
        height: 100%;
        width: 100%;
      }
      .msterp-content > * {
        flex-shrink: 0;
      }
      .msterp-content > .two-panel,
      .msterp-content > .bslib-card {
        flex: 1 1 auto;
        min-height: 0;
      }

      /* === Page wrapper === */
      .msterp-page {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
        gap: 12px;
        width: 100%;
        max-width: 1200px;
        margin: 0 auto;
      }
      .msterp-page.full-bleed {
        max-width: none;
        margin: 0;
      }
      .msterp-page-header {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
        padding: 6px 0 8px;
        border-bottom: 1px solid var(--md-border);
      }
      .msterp-page-title {
        font-size: 18px;
        font-weight: 800;
        margin: 0;
      }
      .msterp-page-actions {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
      }
      .msterp-page-body {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
        gap: 16px;
      }
      .msterp-page-body > .two-panel,
      .msterp-page-body > .bslib-card {
        flex: 1 1 auto;
        min-height: 0;
      }

      /* === Top row pattern for pages === */
      .top {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 12px;
        margin-bottom: 8px;
      }

      /* Typography hierarchy (shell pages only) */
      .msterp-wrap h1,
      .msterp-wrap h2 {
        font-weight: 800;
      }
      .msterp-wrap h3,
      .msterp-wrap h4 {
        font-weight: 700;
      }
      .msterp-wrap h5,
      .msterp-wrap h6 {
        font-weight: 600;
      }
      .msterp-wrap p,
      .msterp-wrap li,
      .msterp-wrap small {
        font-weight: 400;
      }
      .msterp-wrap label,
      .msterp-wrap .control-label {
        font-weight: 600;
      }
      .msterp-wrap strong {
        font-weight: 700;
      }

      .msterp-nav-title { font-weight: 700; margin: 4px 0 10px; color: #fff; }

      /* === Nav items (dark theme per spec) === */
      .msterp-nav {
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        gap: 8px;
        padding-top: 4px;
        padding-bottom: 8px;
      }
      .msterp-nav a.action-button {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: flex-start;
        gap: 10px;
        text-align: left;
        padding: 10px 12px;
        text-decoration: none;
        color: #fff;
        background: var(--md-nav-item-bg);
        border: 1px solid var(--md-nav-item-border);
        border-radius: 8px;
        margin: 0;
        transition: background 0.15s ease;
      }
      .msterp-nav .nav-row {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: 10px;
        width: 100%;
        min-width: 0;
      }
      .msterp-nav a:hover {
        background: var(--md-nav-hover);
      }
      .msterp-nav a.active, .msterp-nav a[aria-current='page'] {
        background: var(--primary);
        color: #fefefe;
        border-color: var(--primary-dark);
      }

      .msterp-nav .nav-icon {
        width: 22px;
        height: 22px;
        min-width: 22px;
        min-height: 22px;
        object-fit: contain;
        flex: 0 0 22px;
        display: block;
        border-radius: 4px;
        background: transparent;
        padding: 0;
        filter: invert(1);
      }
      .msterp-nav .nav-text, .msterp-nav .label {
        flex: 1;
        font-weight: 600;
        font-size: 14px;
        line-height: 1.2;
        white-space: nowrap;
        overflow: hidden;
        transition: opacity var(--duration-fast) ease, width var(--duration-fast) ease;
      }

      .msterp-nav .hint { font-size: 12px; color: var(--text-muted); margin-top: 10px; }

      /* === Sidebar collapsed rules removed — sidebar moved to topbar === */

      /* === Nav header with toggle button === */
      .nav-header {
        display: flex;
        justify-content: flex-end;
        margin-bottom: 10px;
      }
      .sidebar-toggle { display: none; }

      /* Two-panel inner layout (for embedded tools later) */
      .two-panel {
        display: grid;
        grid-template-columns: minmax(320px, 36%) minmax(0, 1fr);
        gap: 16px;
        flex: 1;
        min-height: 0;
      }
      .two-panel > .panel-left  {
        background: var(--panel);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 16px;
        padding-bottom: 16px;
        overflow: auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      .two-panel > .panel-right {
        background: var(--panel);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 16px;
        padding-bottom: 16px;
        overflow: auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      /* Tool page plot containers - mirrors results viewer */
      .tool-plot-box {
        width: 100%;
        max-width: 100%;
        aspect-ratio: var(--tool-plot-ar, 7/5);
        overflow: hidden;
        position: relative;
      }
      .tool-plot-box .shiny-plot-output,
      .tool-plot-box .shiny-bound-output {
        width: 100% !important;
        height: 100% !important;
        min-height: 0 !important;
      }
      .tool-plot-box .shiny-plot-output img {
        width: 100% !important;
        height: 100% !important;
        object-fit: contain;
        object-position: center;
      }

      /* Fallback for browsers without aspect-ratio */
      @supports not (aspect-ratio: 1/1) {
        .tool-plot-box {
          height: 0;
          padding-bottom: calc(100% / var(--tool-plot-ar, 1.4));
        }
        .tool-plot-box > * {
          position: absolute;
          top: 0; left: 0;
          width: 100%; height: 100%;
        }
      }

      /* Tool results structure */
      .tool-results {
        display: flex;
        flex-direction: column;
        gap: 16px;
        flex: 1;
        min-height: 0;
      }
      .tool-results .card {
        flex-shrink: 0;
      }

      /* Tool progress bar */
      .tool-progress-wrap { margin: 10px 0; }
      .tool-progress-bar {
        height: 12px; background: var(--bg-muted); border-radius: 6px; overflow: hidden; border: 1px solid var(--border-light);
      }
      .tool-progress-fill {
        height: 100%; background: var(--progress-fill); width: 0%;
        transition: width 0.3s ease;
        border-radius: 6px;
      }
      .tool-progress-fill.active {
        background: repeating-linear-gradient(
          -45deg,
          var(--progress-fill),
          var(--progress-fill) 10px,
          var(--progress-fill-dark) 10px,
          var(--progress-fill-dark) 20px
        );
        background-size: 28px 28px;
        animation: tool-progress-stripe 0.6s linear infinite;
      }
      @keyframes tool-progress-stripe {
        0% { background-position: 0 0; }
        100% { background-position: 28px 0; }
      }
      .tool-status-row {
        display: flex; align-items: center; gap: 10px; margin-bottom: 8px;
      }
      .tool-status-pill {
        display: inline-block; padding: 4px 8px; border-radius: 999px;
        border: 1px solid var(--border); background: var(--color-white); font-weight: 700; font-size: 11px;
      }
      .tool-status-pill.running {
        background-color: var(--status-warn-bg); border-color: var(--status-warn-border);
        animation: tool-pulse-running 1.5s infinite;
      }
      @keyframes tool-pulse-running {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.7; }
      }
      .tool-status-msg { color: var(--color-text-hint); font-size: 12px; }

      /* Collapsible sections for tools pages */
      .tools-collapse-section {
        border: 1px solid var(--border-light, #e8e4df);
        border-radius: 8px;
        margin-bottom: 12px;
        background: var(--bg-card, #ffffff);
        overflow: hidden;
      }
      .tools-collapse-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 10px 12px;
        cursor: pointer;
        background: var(--bg-muted, #f5f3f0);
        transition: background var(--duration-fast, 150ms) ease;
        user-select: none;
      }
      .tools-collapse-header:hover {
        background: var(--bg-hover, #f0eeeb);
      }
      .tools-collapse-title {
        display: flex;
        align-items: center;
        gap: 8px;
        font-weight: 700;
        font-size: 14px;
      }
      .tools-collapse-icon {
        display: inline-block;
        font-size: 10px;
        color: var(--text-secondary, #5a5a5a);
        transition: transform var(--duration-fast, 150ms) ease;
      }
      .tools-collapse-section.open .tools-collapse-icon {
        transform: rotate(90deg);
      }
      .tools-collapse-body {
        max-height: 0;
        overflow: hidden;
        transition: max-height var(--duration-normal, 250ms) ease, padding var(--duration-normal, 250ms) ease;
        padding: 0 12px;
      }
      .tools-collapse-section.open .tools-collapse-body {
        max-height: 1200px;
        padding: 12px;
      }

      /* Busy overlay */
      .msterp-busy {
        display: none;
        position: fixed;
        inset: 0;
        background: rgba(0,0,0,0.55);
        z-index: 9999;
        align-items: center;
        justify-content: center;
        padding: 24px;
      }
      .msterp-busy-card {
        width: min(520px, 95vw);
        background: var(--color-white);
        border-radius: 16px;
        border: 1px solid var(--border);
        padding: 18px;
      }
      .msterp-busy-row {
        display: flex;
        gap: 14px;
        align-items: center;
      }
      .msterp-busy-media {
        width: 96px;
        height: 96px;
        border-radius: 12px;
        border: 1px solid var(--border);
        background: var(--color-white);
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
        flex: 0 0 96px;
      }
      .msterp-busy-media img { width: 100%; height: 100%; object-fit: cover; }
      .msterp-busy-text { flex: 1; }
      .msterp-busy-text .msg { font-weight: 700; margin-bottom: 6px; }
      .msterp-progress {
        width: 100%;
        height: 12px;
        background: var(--bg-muted);
        border-radius: 999px;
        border: 1px solid var(--border);
        overflow: hidden;
      }
      .msterp-progress > .bar {
        height: 100%;
        width: 0%;
        background: var(--banner);
        border-radius: 999px;
        transition: width 160ms linear;
      }
      .msterp-percent { margin-top: 8px; font-size: 12px; color: var(--color-text-pct); }

      /* === Shared Components (Spec) === */

      /* Buttons (brand-aligned) */
      .btn.btn-primary,
      .btn-primary,
      .action-button.btn-primary {
        background: var(--md-red);
        border-color: var(--btn-primary-border);
        color: #fff;
        font-weight: 700;
      }
      .btn.btn-primary:hover,
      .btn-primary:hover,
      .action-button.btn-primary:hover {
        background: var(--btn-primary-hover);
        border-color: var(--btn-primary-hover-border);
      }
      .btn.btn-default,
      .btn-default {
        background: var(--btn-default-bg);
        border: 1px solid var(--md-border);
        color: var(--text-primary);
        font-weight: 600;
      }
      .btn.btn-default:hover,
      .btn-default:hover {
        background: var(--btn-default-hover-bg);
        border-color: var(--btn-default-hover-border);
      }
      .btn.btn-outline-primary,
      .btn-outline-primary {
        color: var(--md-red);
        border-color: var(--md-red);
        background: transparent;
        font-weight: 600;
      }
      .btn.btn-outline-primary:hover,
      .btn-outline-primary:hover {
        background: var(--md-red);
        color: #fff;
      }

      /* === Refined UI Components === */

      /* Soft buttons */
      .btn-soft {
        background: var(--bg-muted);
        border: 1px solid var(--border-light);
        color: var(--text-primary);
        font-weight: 600;
        border-radius: 8px;
        padding: 8px 14px;
        transition: all var(--duration-fast) ease;
      }
      .btn-soft:hover {
        background: var(--bg-hover);
        border-color: var(--border-medium);
      }
      .btn-primary-soft {
        background: var(--primary-light);
        border: 1px solid transparent;
        color: var(--primary);
        font-weight: 600;
        border-radius: 8px;
        padding: 8px 14px;
        transition: all var(--duration-fast) ease;
      }
      .btn-primary-soft:hover {
        background: var(--primary);
        color: #fff;
      }
      .btn-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 32px;
        height: 32px;
        padding: 0;
        border-radius: 8px;
        background: var(--bg-muted);
        border: 1px solid var(--border-light);
        color: var(--text-secondary);
        transition: all var(--duration-fast) ease;
      }
      .btn-icon:hover {
        background: var(--bg-hover);
        color: var(--text-primary);
      }

      /* Status badges */
      .status-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
      }
      .status-badge::before {
        content: '';
        width: 8px;
        height: 8px;
        border-radius: 50%;
      }
      .status-saved {
        background: var(--status-success-bg);
        color: var(--status-success-text);
      }
      .status-saved::before {
        background: var(--status-success-dot);
      }
      .status-unsaved {
        background: var(--accent-gold-light);
        color: #8b6914;
      }
      .status-unsaved::before {
        background: var(--accent-gold);
        animation: pulse 1.5s infinite;
      }
      .status-error {
        background: var(--status-error-bg);
        color: var(--status-error-text);
      }
      .status-error::before {
        background: var(--status-error-dot);
        animation: pulse 1s infinite;
      }

      /* Toggle switches (refined) */
      .toggle-switch-track {
        width: 44px;
        height: 24px;
        background: var(--bg-muted);
        border: 1px solid var(--border-medium);
        border-radius: 12px;
        position: relative;
        cursor: pointer;
        transition: all var(--duration-fast) ease;
      }
      .toggle-switch-track.active {
        background: var(--primary);
        border-color: var(--primary-dark);
      }
      .toggle-switch-knob {
        width: 18px;
        height: 18px;
        background: var(--color-white);
        border-radius: 50%;
        position: absolute;
        top: 2px;
        left: 2px;
        box-shadow: var(--shadow-sm);
        transition: transform var(--duration-fast) var(--ease-out);
      }
      .toggle-switch-track.active .toggle-switch-knob {
        transform: translateX(20px);
      }

      /* Card variants */
      .card-minimal {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-radius: 10px;
        padding: 14px;
      }
      .card-accent {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-left: 3px solid var(--primary);
        border-radius: 10px;
        padding: 14px;
      }
      .card-accent-gold {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-left: 3px solid var(--accent-gold);
        border-radius: 10px;
        padding: 14px;
      }

      /* Form controls (minimal style) */
      .form-group-minimal {
        margin-bottom: 12px;
      }
      .form-label-minimal {
        display: block;
        font-size: 12px;
        font-weight: 600;
        color: var(--text-secondary);
        margin-bottom: 4px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .form-input-minimal {
        width: 100%;
        padding: 8px 12px;
        border: 1px solid var(--border-light);
        border-radius: 8px;
        font-size: 14px;
        background: var(--bg-card);
        color: var(--text-primary);
        transition: border-color var(--duration-fast) ease, box-shadow var(--duration-fast) ease;
      }
      .form-input-minimal:focus {
        outline: none;
        border-color: var(--primary);
        box-shadow: 0 0 0 3px var(--primary-light);
      }

      /* Global form-control baseline */
      .msterp-wrap .form-control {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-radius: 8px;
        color: var(--text-primary);
        font-size: 13px;
        padding: 8px 12px;
        box-shadow: none;
        transition: border-color var(--duration-fast) ease, box-shadow var(--duration-fast) ease;
      }
      .msterp-wrap .form-control:focus {
        border-color: var(--primary);
        box-shadow: 0 0 0 2px var(--primary-light);
        outline: none;
      }
      .msterp-wrap .control-label {
        font-size: 12px;
        font-weight: 600;
        color: var(--text-secondary);
        margin-bottom: 4px;
      }

      /* File input — custom themed */
      .msterp-wrap .btn-file,
      .msterp-wrap .shiny-input-container .input-group-btn .btn-file {
        background: var(--primary-light) !important;
        border: 1px solid var(--border-light) !important;
        color: var(--primary) !important;
        font-weight: 600;
        border-radius: 8px 0 0 8px !important;
        height: 38px;
        padding: 8px 14px;
        line-height: 20px;
        transition: all var(--duration-fast) ease;
      }
      .msterp-wrap .btn-file:hover,
      .msterp-wrap .shiny-input-container .input-group-btn .btn-file:hover {
        background: var(--primary) !important;
        color: #fff !important;
        border-color: var(--primary-dark) !important;
      }
      .msterp-wrap .shiny-input-container .input-group .form-control[readonly] {
        background: var(--bg-card) !important;
        border: 1px solid var(--border-light) !important;
        border-left: none !important;
        border-radius: 0 8px 8px 0 !important;
        color: var(--text-secondary);
        font-size: 13px;
        height: 38px;
        padding: 8px 12px;
        box-shadow: none !important;
      }
      .msterp-wrap .shiny-input-container .input-group {
        border: none;
      }

      /* verbatimTextOutput — white card style */
      .msterp-wrap pre.shiny-text-output,
      .msterp-wrap .shiny-text-output {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-radius: 10px;
        padding: 12px;
        font-family: var(--mono);
        font-size: 12px;
        color: var(--text-primary);
        white-space: pre-wrap;
        word-wrap: break-word;
        max-height: 400px;
        overflow: auto;
      }

      /* DT DataTables — hide default chrome, keep pagination */
      .msterp-wrap .dataTables_length,
      .msterp-wrap .dataTables_filter,
      .msterp-wrap .dataTables_info {
        display: none !important;
      }

      /* Card component */
      .card {
        background: var(--md-card-bg);
        border: 1px solid var(--md-card-border);
        border-radius: 12px;
        padding: 16px;
        box-shadow: var(--md-card-shadow);
      }

      /* Pill component (gold) */
      .pill {
        display: inline-block;
        background: var(--md-gold-light);
        border: 1px solid var(--md-gold);
        color: var(--pill-text);
        font-weight: 700;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
      }

      /* Validation lights */
      .light {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 6px 10px;
        border: 1px solid var(--md-border);
        border-radius: 8px;
        background: var(--color-white);
        font-weight: 600;
      }
      .dot {
        width: 10px;
        height: 10px;
        border-radius: 999px;
        display: inline-block;
      }
      .dot.red { background: var(--md-red); }
      .dot.green { background: var(--md-green); }
      .dot.gray { background: var(--text-muted); }

      /* Progress bar (spec) */
      .bar {
        width: 100%;
        height: 10px;
        background: var(--progress-track);
        border-radius: 999px;
        border: 1px solid var(--md-border);
        overflow: hidden;
      }
      .bar .fill {
        height: 100%;
        background: var(--md-red);
        border-radius: 999px;
        transition: width 160ms linear;
      }

      /* Logs container (dark) */
      .logs {
        background: var(--logs-bg);
        border: 1px solid var(--logs-border);
        border-radius: 12px;
        padding: 12px;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-size: 12px;
        color: var(--logs-text);
        overflow-x: auto;
        overflow-y: auto;
        white-space: pre-wrap;
        min-height: 100px;
        max-height: 400px;
      }

      /* Gate pattern (shared) */
      .msterp-gate {
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 16px;
      }
      .msterp-gate-card {
        width: min(520px, 92vw);
        background: var(--md-card-bg);
        border: 1px solid var(--md-card-border);
        border-radius: 14px;
        box-shadow: var(--md-card-shadow);
        overflow: hidden;
      }
      .msterp-gate-body {
        padding: 16px;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      .msterp-gate-title {
        font-weight: 800;
        font-size: 18px;
      }
      .msterp-gate-help {
        font-size: 12px;
        color: var(--color-text-subtle);
      }
      .msterp-gate-actions {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      /* Responsive grid */
      .grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 14px;
      }

      /* === bslib Card Tab Styling (Centered, shrink-wrapped, spec-matching) === */
      .bslib-card {
        border: 1px solid var(--md-card-border);
        border-radius: 12px;
        background: var(--md-card-bg);
        box-shadow: var(--md-card-shadow);
        overflow: hidden;
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-height: 0;
        width: 100%;
      }

      /* Center the whole tab group in the card header */
      .bslib-card > .card-header {
        background: var(--tab-bg);
        border-bottom: 1px solid var(--tab-border);
        padding: 12px 8px;
        display: flex;
        justify-content: center;
        flex-shrink: 0;
      }

      /* Shrink-wrap the nav so it doesn't span full width (and add capsule border) */
      .bslib-card .nav-tabs,
      .bslib-card .nav.nav-tabs {
        background: transparent;
        border-bottom: 0;
        gap: 0;
        display: inline-flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        width: auto;
        margin: 0 auto;
        padding: var(--tab-pill-pad);
        border: 1px solid var(--tab-border);
        border-radius: calc(var(--tab-radius) + 2px);
      }

      .bslib-card .nav-tabs .nav-item {
        flex: 0 0 auto;
        display: inline-block;
      }

      /* Typography + dividers + hover/active */
      .bslib-card .nav-tabs .nav-link {
        background: transparent;
        color: var(--tab-text);
        font-family: var(--mono, 'JetBrains Mono', monospace);
        font-weight: 700;
        font-size: 15px;
        line-height: 1.15;
        padding: 12px 24px;
        border: 0;
        border-right: 1px solid var(--tab-border);
        border-radius: var(--tab-radius);
        white-space: nowrap;
        text-align: center;
      }

      .bslib-card .nav-tabs .nav-item:last-child .nav-link {
        border-right: 0;
      }

      .bslib-card .nav-tabs .nav-link:hover {
        background: var(--tab-hover-bg);
        color: var(--tab-text-hover);
      }

      .bslib-card .nav-tabs .nav-link.active,
      .bslib-card .nav-tabs .nav-item.show .nav-link {
        background: var(--tab-active-bg);
        color: var(--tab-text-active);
        position: relative;
      }

      /* Active underline accent */
      .bslib-card .nav-tabs .nav-link.active::after {
        content: '';
        position: absolute;
        left: 12px;
        right: 12px;
        bottom: 0;
        height: 3px;
        background: var(--tab-accent);
        border-radius: 2px 2px 0 0;
      }

      .bslib-card > .card-body,
      .bslib-card > .tab-content {
        padding: 16px;
        flex: 1 1 auto;
        min-height: 0;
        overflow: auto;
        width: 100%;
      }
      .bslib-card .tab-pane {
        padding: 0;
        width: 100%;
        min-height: 0;
      }
      .bslib-card .tab-pane.active {
        display: block;
        width: 100%;
      }

      /* Narrow sizing for tabs */
      @media (max-width: 700px) {
        .bslib-card .nav-tabs .nav-link {
          font-size: 14px;
          padding: 10px 16px;
        }
      }

      /* === Modal dialog — de-Shiny === */
      .modal-content {
        background: var(--bg-card);
        border: 1px solid var(--border-medium);
        border-radius: 14px;
        box-shadow: var(--shadow-lg);
        overflow: hidden;
      }
      .modal-header {
        background: var(--bg-card);
        border-bottom: 1px solid var(--border-light);
        padding: 16px 20px;
      }
      .modal-header .modal-title,
      .modal-header h4 {
        font-weight: 800;
        font-size: 16px;
        color: var(--text-primary);
      }
      .modal-header .close {
        color: var(--text-muted);
        opacity: 0.6;
        font-size: 20px;
      }
      .modal-header .close:hover {
        opacity: 1;
        color: var(--text-primary);
      }
      .modal-body {
        padding: 16px 20px;
        color: var(--text-primary);
      }
      .modal-footer {
        background: var(--bg-card);
        border-top: 1px solid var(--border-light);
        padding: 12px 20px;
      }
      .modal-backdrop.in {
        opacity: 0.4;
        background: var(--modal-backdrop);
      }

      /* === Notification toasts — de-Shiny === */
      #shiny-notification-panel {
        top: 64px;
        right: 16px;
        bottom: auto;
        left: auto;
        width: 360px;
      }
      .shiny-notification {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-radius: 10px;
        box-shadow: var(--shadow-md);
        font-size: 13px;
        color: var(--text-primary);
        padding: 12px 16px;
        opacity: 1;
      }
      .shiny-notification-message {
        border-left: 3px solid var(--primary);
      }
      .shiny-notification-warning {
        border-left: 3px solid var(--accent-gold);
        background: var(--accent-gold-light);
      }
      .shiny-notification-error {
        border-left: 3px solid var(--status-error-text);
        background: var(--notification-error-bg);
      }
      .shiny-notification .shiny-notification-close {
        color: var(--text-muted);
        font-size: 16px;
      }
      .shiny-notification .shiny-notification-close:hover {
        color: var(--text-primary);
      }
      .shiny-notification .progress {
        background: var(--bg-muted);
        border-radius: 6px;
        height: 6px;
      }
      .shiny-notification .progress-bar {
        background: var(--primary);
        border-radius: 6px;
      }

      /* === Theme Toggle Button === */
      .theme-toggle-btn {
        background: transparent;
        border: 1px solid rgba(255,255,255,0.2);
        color: #fff;
        width: 36px;
        height: 36px;
        border-radius: 8px;
        cursor: pointer;
        font-size: 18px;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
        transition: background 0.15s ease;
        padding: 0;
        line-height: 1;
      }
      .theme-toggle-btn:hover {
        background: rgba(255,255,255,0.1);
      }

      /* === DT DataTables Dark Mode === */
      [data-theme='dark'] .dataTables_wrapper {
        color: var(--text-primary);
      }
      [data-theme='dark'] .dataTables_wrapper .dataTable {
        color: var(--text-primary);
        background: var(--bg-card);
      }
      [data-theme='dark'] .dataTables_wrapper .dataTable thead th,
      [data-theme='dark'] .dataTables_wrapper .dataTable thead td {
        border-bottom-color: var(--border-medium);
        color: var(--text-primary);
        background: var(--bg-muted);
      }
      [data-theme='dark'] .dataTables_wrapper .dataTable tbody td {
        border-top-color: var(--border-light);
      }
      [data-theme='dark'] .dataTables_wrapper .dataTable tbody tr:hover {
        background: var(--bg-hover) !important;
      }
      [data-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: var(--text-primary) !important;
        background: var(--bg-card);
        border-color: var(--border-light);
      }
      [data-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--primary) !important;
        color: #fff !important;
        border-color: var(--primary-dark);
      }
      [data-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: var(--bg-hover) !important;
        color: var(--text-primary) !important;
        border-color: var(--border-medium);
      }

      /* === Selectize Dark Mode === */
      [data-theme='dark'] .selectize-input {
        background: var(--bg-card) !important;
        border-color: var(--border-light) !important;
        color: var(--text-primary) !important;
      }
      [data-theme='dark'] .selectize-input.focus {
        border-color: var(--primary) !important;
        box-shadow: 0 0 0 2px var(--primary-light) !important;
      }
      [data-theme='dark'] .selectize-dropdown {
        background: var(--bg-card) !important;
        border-color: var(--border-medium) !important;
        color: var(--text-primary) !important;
      }
      [data-theme='dark'] .selectize-dropdown .option {
        color: var(--text-primary) !important;
      }
      [data-theme='dark'] .selectize-dropdown .option:hover,
      [data-theme='dark'] .selectize-dropdown .active {
        background: var(--bg-hover) !important;
        color: var(--text-primary) !important;
      }
      [data-theme='dark'] .selectize-control.single .selectize-input::after {
        border-top-color: var(--text-secondary) !important;
      }

      /* === Responsive Behavior === */
      @media (max-width: 1200px) {
        .msterp-topbar-nav .nav-text {
          display: none;
        }
        .msterp-topbar-nav a.action-button {
          padding: 6px 8px;
        }
      }

      @media (max-width: 900px) {
        .msterp-topbar {
          flex-wrap: wrap;
          height: auto;
          min-height: 56px;
          padding: 8px 16px;
        }
        .grid {
          grid-template-columns: 1fr;
        }
        .logs {
          overflow-x: scroll;
        }
      }
    ")),
    tags$style(HTML("
      /* Pipeline editor layout */
      .pipe-layout {
        display: grid;
        grid-template-columns: 280px minmax(0, 1fr) 320px;
        gap: var(--gap);
        height: 100%;
        min-height: 0; /* allow inner panels to scroll */
      }
      .pipe-left, .pipe-center, .pipe-right {
        border-radius: 12px;
        border: 1px solid var(--border);
        overflow: auto;
        min-height: 0; /* grid + overflow fix */
      }
      .pipe-left  { background: var(--panel); padding: 12px; }
      .pipe-center{ background: var(--color-white);       padding: 12px; }
      .pipe-right { background: var(--panel); padding: 12px; }

      .pipe-section {
        border: 1px solid var(--border);
        border-radius: 12px;
        background: var(--panel);
        padding: 10px;
        margin-bottom: 12px;
      }
      .pipe-section-title {
        font-weight: 800;
        margin-bottom: 8px;
      }
      .pipe-step-card {
        border: 1px solid var(--md-card-border);
        border-radius: 10px;
        background: var(--md-card-bg);
        padding: 8px 10px;
        margin-bottom: 8px;
        display: flex;
        justify-content: space-between;
        gap: 10px;
        align-items: center;
        box-shadow: var(--md-card-shadow);
      }
      .pipe-step-meta {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      .pipe-step-meta .small {
        font-size: 12px;
        color: var(--color-text-meta);
      }

      /* Tools page action buttons - constrained width */
      .btn-tool-action {
        width: auto !important;
        max-width: 160px;
        display: inline-block;
      }
      .panel-left .btn-tool-action {
        margin-bottom: 8px;
      }
    ")),
    tags$script(HTML("
      // Sidebar removed — no-op stubs to prevent errors
      function msterpSyncSidebarToggle() {}

      // Theme toggle
      function msterpToggleTheme() {
        var html = document.documentElement;
        var current = html.getAttribute('data-theme') || 'light';
        var next = current === 'dark' ? 'light' : 'dark';
        html.setAttribute('data-theme', next);
        localStorage.setItem('msterp-theme', next);
        // Update toggle button icon (moon for light, sun for dark)
        var btn = document.getElementById('theme_toggle');
        if (btn) btn.innerHTML = next === 'dark' ? '\\u2600' : '\\u263E';
        // Also update home page toggle if it exists
        var homeBtn = document.getElementById('home_theme_toggle');
        if (homeBtn) homeBtn.innerHTML = next === 'dark' ? '\\u2600' : '\\u263E';
      }

      // Sync theme toggle icon when DOM updates (for dynamic UI)
      $(document).on('shiny:connected', function() {
        var current = document.documentElement.getAttribute('data-theme') || 'light';
        var btn = document.getElementById('theme_toggle');
        if (btn) btn.innerHTML = current === 'dark' ? '\\u2600' : '\\u263E';
        var homeBtn = document.getElementById('home_theme_toggle');
        if (homeBtn) homeBtn.innerHTML = current === 'dark' ? '\\u2600' : '\\u263E';
      });

      // Busy overlay handler
      Shiny.addCustomMessageHandler('msterp_busy', function(payload) {
        var active  = !!payload.active;
        var msg     = payload.message || '';
        var percent = payload.percent;

        var $overlay = $('#msterp_busy');
        var $msg     = $('#msterp_busy_msg');
        var $bar     = $('#msterp_busy_bar');
        var $pct     = $('#msterp_busy_pct');

        $msg.text(msg);

        if (percent === null || typeof percent === 'undefined' || isNaN(percent)) {
          $bar.css('width', '0%');
          $pct.text('');
        } else {
          var p = Math.max(0, Math.min(100, Number(percent)));
          $bar.css('width', p + '%');
          $pct.text(p.toFixed(0) + '% complete');
        }

        if (active) $overlay.css('display', 'flex');
        else $overlay.css('display', 'none');
      });

      // Sidebar collapse handler (no-op — sidebar removed)
      Shiny.addCustomMessageHandler('msterp_sidebar_collapse', function(payload) {});

      // Reset file inputs (used by Format/New Run reset actions)
      Shiny.addCustomMessageHandler('msterp_reset_file_input', function(payload) {
        if (!payload || !payload.id) return;
        var id = payload.id;
        var el = document.getElementById(id);
        var container = null;
        var input = null;

        // Find the container and input element
        if (el && el.tagName === 'INPUT' && el.type === 'file') {
          input = el;
          container = el.closest('.shiny-input-container');
        } else if (el && el.querySelector) {
          container = el;
          input = el.querySelector('input[type=\"file\"]');
        } else {
          container = document.querySelector('#' + id);
          if (container) {
            input = container.querySelector('input[type=\"file\"]');
          }
        }

        if (!input) return;

        // Clear the actual file input
        input.value = '';

        // Reset the displayed filename text (Shiny fileInput shows filename in a text input)
        if (container) {
          var textInput = container.querySelector('input[type=\"text\"].form-control');
          if (textInput) {
            textInput.value = '';
            textInput.placeholder = 'No file selected';
          }
        }

        // Notify Shiny that the input has been cleared
        try { Shiny.setInputValue(id, null, {priority: 'event'}); } catch (e) {}
      });

      // Open URL in a new tab (used by Results viewer link-outs)
      Shiny.addCustomMessageHandler('msterp_open_url', function(payload) {
        if (!payload || !payload.url) return;
        var url = payload.url || payload;
         window.open(payload.url, '_blank', 'noopener');
      });

      // Set active nav item based on current page
      var pendingNavUpdate = null;
      var navPollTimer = null;

      var navIdMap = {
        'home': 'nav_home',
        'results': 'nav_results',
        'format': 'nav_format',
        'new_run': 'nav_newrun',
        'pipeline': 'nav_pipeline',
        'tools': 'nav_tools',
        'database': 'nav_db',
        'tutorial': 'nav_tutorial',
        'about': 'nav_about'
      };

      function applyNavUpdate(pageId) {
        var navId = navIdMap[pageId];
        if (!navId) return false;

        var $navItem = $('#' + navId);
        if ($navItem.length === 0) return false;

        // Remove active class from all nav items
        $('.msterp-topbar-nav a').removeClass('active');
        // Add active class to the current nav item
        $navItem.addClass('active');
        return true;
      }

      function stopNavPoll() {
        if (navPollTimer) {
          clearTimeout(navPollTimer);
          navPollTimer = null;
        }
      }

      function startNavPoll(pageId) {
        stopNavPoll();
        var retries = 0;
        var maxRetries = 40; // 40 * 50ms = 2s max

        function poll() {
          navPollTimer = null;
          if (applyNavUpdate(pageId)) {
            pendingNavUpdate = null;
            return;
          }
          retries++;
          if (retries < maxRetries) {
            navPollTimer = setTimeout(poll, 50);
          }
        }
        poll();
      }

      // Immediately highlight nav item on click (before server roundtrip)
      $(document).on('click', '.msterp-topbar-nav a', function(e) {
        $('.msterp-topbar-nav a').removeClass('active');
        $(this).addClass('active');
      });

      Shiny.addCustomMessageHandler('msterp_set_active_nav', function(payload) {
        var pageId = payload.page_id;

        // Try to apply immediately
        if (applyNavUpdate(pageId)) {
          pendingNavUpdate = null;
          stopNavPoll();
          return;
        }

        // If nav doesn't exist yet, poll until it appears
        pendingNavUpdate = pageId;
        startNavPoll(pageId);
      });

      // Scroll to element handler
      Shiny.addCustomMessageHandler('msterp_scroll_to', function(payload) {
        if (!payload || !payload.selector) return;
        var el = document.querySelector(payload.selector);
        if (el) {
          el.scrollIntoView({ behavior: 'smooth', block: 'center' });
          el.focus();
        }
      });

      // Collapsible section toggle for tools pages
      $(document).on('click', '.tools-collapse-header', function(e) {
        e.stopPropagation();
        $(this).closest('.tools-collapse-section').toggleClass('open');
      });

    "))
  )
}

topbar_ui <- function(left_logo  = "static/img/logo_left.png",
                      right_logo = "static/img/logo_right.png",
                      left_alt   = "Left logo",
                      right_alt  = "Right logo",
                      about_logo = "static/img/about.svg",
                      flow_logo = "static/img/flow.svg",
                      format_logo = "static/img/format.svg",
                      home_logo = "static/img/home.svg",
                      new_run_logo = "static/img/newrun.svg",
                      results_logo = "static/img/results.svg",
                      terpbase_logo = "static/img/terpbase.svg",
                      tools_logo = "static/img/tools.svg",
                      tutorial_logo = "static/img/tutorial.svg") {

  nav_item <- function(id, text, icon_src) {
    actionLink(
      id,
      label = tags$span(
        class = "nav-row",
        tags$img(src = icon_src, class = "nav-icon", alt = text),
        span(class = "nav-text", text)
      ),
      title = text
    )
  }

  div(
    class = "msterp-topbar",
    div(
      class = "msterp-topbar-left",
      tags$img(src = left_logo,  alt = left_alt),
      tags$img(src = right_logo, alt = right_alt),
      span(class = "msterp-topbar-title", "MS Terp")
    ),
    div(
      class = "msterp-topbar-nav",
      nav_item("nav_home",     "Home",     home_logo),
      nav_item("nav_results",  "TerpBook",  results_logo),
      nav_item("nav_format",   "Format",   format_logo),
      nav_item("nav_newrun",   "New run",  new_run_logo),
      nav_item("nav_pipeline", "TerpFlow", flow_logo),
      nav_item("nav_tools",    "Tools",    tools_logo),
      nav_item("nav_db",       "TerpBase", terpbase_logo),
      nav_item("nav_tutorial", "Tutorial", tutorial_logo),
      nav_item("nav_about",    "About",    about_logo)
    ),
    tags$button(
      id = "theme_toggle",
      class = "theme-toggle-btn",
      title = "Toggle dark mode",
      onclick = "msterpToggleTheme()",
      HTML("&#9790;")
    )
  )
}

msterp_sidebar_ui <- function(...) {
  # Sidebar removed — nav items now live in topbar_ui()
  tagList()
}


msterp_busy_overlay_ui <- function() {
  div(
    id = "msterp_busy",
    class = "msterp-busy",
    div(
      class = "msterp-busy-card",
      div(
        class = "msterp-busy-row",
        div(
          class = "msterp-busy-media",
          tags$img(src = "static/media/loading.gif", alt = "Loading")
        ),
        div(
          class = "msterp-busy-text",
          div(id = "msterp_busy_msg", class = "msg", "Working…"),
          div(class = "msterp-progress", div(id = "msterp_busy_bar", class = "bar")),
          div(id = "msterp_busy_pct", class = "msterp-percent", "")
        )
      )
    )
  )
}

single_panel_ui <- function(...) {
  tagList(...)
}

msterp_page <- function(title = NULL, actions = NULL, ..., full_bleed = FALSE, class = NULL) {
  header <- NULL
  if (!is.null(title) || !is.null(actions)) {
    header <- div(
      class = "msterp-page-header",
      if (!is.null(title)) tags$h2(class = "msterp-page-title", title) else NULL,
      if (!is.null(actions)) div(class = "msterp-page-actions", actions) else NULL
    )
  }

  div(
    class = paste(c("msterp-page", if (isTRUE(full_bleed)) "full-bleed", class), collapse = " "),
    header,
    div(class = "msterp-page-body", ...)
  )
}

two_panel_ui <- function(left_ui, right_ui) {

  div(
    class = "two-panel",
    div(class = "panel-left",  left_ui),
    div(class = "panel-right", right_ui)
  )
}

# Collapsible section helper for tools pages
# @param id Unique ID for the section
# @param title Section header title
# @param open Whether section starts open (default FALSE)
# @param ... Contents of the collapsible section
tools_collapse_section_ui <- function(id, title, open = FALSE, ...) {
  div(
    class = paste("tools-collapse-section", if (open) "open" else ""),
    id = id,
    div(
      class = "tools-collapse-header",
      div(
        class = "tools-collapse-title",
        span(class = "tools-collapse-icon", HTML("&#9654;")),
        title
      )
    ),
    div(class = "tools-collapse-body", ...)
  )
}

