# R/utils/viewer_status_helpers.R
# Shared helper for Result Viewer status indicator.
# Extracted from page_results.R to centralise status text/class logic.

#' Compute status descriptor for the Result Viewer save-status indicator
#'
#' Pure function — no Shiny reactivity, no side effects.
#'
#' @param status     Character scalar: one of "saved", "dirty", "saving", "failed".
#' @param save_error Optional error message string (used when status == "failed").
#' @return Named list with \code{css_class}, \code{text}, and \code{title}.
res_status_descriptor <- function(status, save_error = NULL) {
  css_class <- switch(status,
    "saved"  = "saved",
    "dirty"  = "unsaved",
    "saving" = "saving",
    "failed" = "failed",
    "unsaved"
  )

  text <- switch(status,
    "saved"  = "Up to date",
    "dirty"  = "Unsaved (session only)",
    "saving" = "Saving...",
    "failed" = "Save failed",
    "Unsaved changes"
  )

  title <- switch(status,
    "saved"  = "All changes exported to .terpbook",
    "dirty"  = "Changes exist in this session only. Download Updated .terpbook to persist.",
    "saving" = "Saving changes...",
    "failed" = paste("Save failed:", save_error %||% "Unknown error"),
    "Unsaved changes"
  )

  list(css_class = css_class, text = text, title = title)
}
