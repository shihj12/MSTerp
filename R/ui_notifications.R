msterp_set_busy <- function(session, active, message = "", percent = NULL) {
  session$sendCustomMessage("msterp_busy", list(
    active  = isTRUE(active),
    message = as.character(message %||% ""),
    percent = if (is.null(percent)) NULL else as.numeric(percent)
  ))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
