# R/utils/palettes.R
# Centralized color-palette definitions for MSTerp.
# Single source of truth for palette names, colors, and choice lists.
# Replaces the three duplicate get_palette() copies in terpbook.R and the
# inline fdr color stops in tb_fdr_scale().
#
# Convention: the first stop = LOW value, last stop = HIGH value.
# For FDR scales (which use -log10 transform with low-FDR at one end), the
# caller may reverse the palette via `direction = -1`.

# Hex stops for each palette. Keep ordered low -> high.
.msterp_palette_stops <- list(
  # Sequential yellow -> red; red is HIGH (user's new palette).
  yellow_red = c("#FFFFCC", "#FFEDA0", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026"),
  # Existing FCS palette: significant -> not significant as yellow -> black via pink/purple/blue.
  # Kept in its original low-index-is-yellow orientation so callers reversing for
  # high-FDR semantics continue to work unchanged.
  yellow_cap = c("yellow", "#ff7ab0", "#881bff", "#2727d6", "black"),
  # Diverging blue -> gray -> red. Red is HIGH.
  blue_red = c("#2e66a0", "#6a8ab8", "#b0b0b0", "#c88080", "#d14f58"),
  # Diverging blue -> white -> red. Red is HIGH.
  RdBu = c("#2166AC", "#F7F7F7", "#B2182B"),
  # Diverging blue -> yellow -> red. Red is HIGH.
  RdYlBu = c("#4575B4", "#FFFFBF", "#D73027"),
  # Diverging purple -> white -> orange. Orange is HIGH.
  PuOr = c("#542788", "#F7F7F7", "#B35806"),
  # Sequential light -> dark blue. Dark blue is HIGH.
  Blues = c("#EFF3FF", "#2171B5"),
  # Sequential light -> dark red. Dark red is HIGH.
  Reds = c("#FEE0D2", "#A50F15")
)

# Classification of palettes for choice-list filtering.
# "fdr"        = sensible for coloring by FDR / log-scaled p-values in enrichment plots.
# "sequential" = sensible for one-sided numeric ranges (intensity, counts).
# "diverging"  = sensible for signed values (log fold-change, z-scores).
# "heatmap"    = full union for heatmaps (all of the above + viridis).
.msterp_palette_kinds <- list(
  yellow_red = c("fdr", "sequential", "heatmap"),
  yellow_cap = c("fdr"),
  blue_red   = c("fdr", "diverging", "heatmap"),
  viridis    = c("fdr", "sequential", "heatmap"),
  RdBu       = c("diverging", "heatmap"),
  RdYlBu     = c("fdr", "diverging", "heatmap"),
  PuOr       = c("diverging", "heatmap"),
  Blues      = c("sequential", "heatmap"),
  Reds       = c("sequential", "heatmap")
)

#' Available palette names for a given context.
#'
#' @param kind One of "fdr" (enrichment FDR scales), "diverging", "sequential",
#'   or "heatmap" (full heatmap union including viridis). Defaults to all.
#' @return Character vector of palette names in a stable preferred order.
msterp_palette_choices <- function(kind = c("all", "fdr", "diverging", "sequential", "heatmap")) {
  kind <- match.arg(kind)
  # Stable order: new palette first, then historical defaults, then the rest.
  preferred_order <- c("yellow_red", "yellow_cap", "blue_red", "RdYlBu",
                       "viridis", "RdBu", "PuOr", "Blues", "Reds")
  if (kind == "all") return(preferred_order)
  matches <- vapply(preferred_order, function(nm) {
    kinds <- .msterp_palette_kinds[[nm]] %||% character(0)
    kind %in% kinds
  }, logical(1))
  preferred_order[matches]
}

#' Return a vector of n interpolated colors for a named palette.
#'
#' @param name      Palette name (see msterp_palette_choices()). Unknown names
#'   fall back to a sensible diverging palette.
#' @param n         Number of colors to return (interpolated across stops).
#' @param direction 1 (default) keeps palette in low->high order; -1 reverses.
#' @return Character vector of hex colors, length n.
msterp_palette_colors <- function(name, n = 100, direction = 1) {
  name <- as.character(name %||% "viridis")[1]
  direction <- if (isTRUE(direction == -1)) -1 else 1

  # viridis is handled specially to preserve perceptual-uniformity when the
  # viridisLite package is present.
  if (identical(name, "viridis")) {
    cols <- if (requireNamespace("viridisLite", quietly = TRUE)) {
      viridisLite::viridis(n)
    } else {
      grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(n)
    }
    if (direction == -1) cols <- rev(cols)
    return(cols)
  }

  stops <- .msterp_palette_stops[[name]]
  if (is.null(stops)) {
    # Unknown palette name — fall back to RdYlBu-like default and warn once.
    stops <- .msterp_palette_stops[["RdYlBu"]]
  }
  if (direction == -1) stops <- rev(stops)
  grDevices::colorRampPalette(stops)(n)
}

#' Raw palette stops (for callers that need discrete stops, e.g. ggplot
#' scale_*_gradientn() which interpolates internally).
#'
#' @param name Palette name.
#' @param direction 1 or -1.
#' @return Character vector of hex stops in low->high order (after direction).
msterp_palette_stops <- function(name, direction = 1) {
  name <- as.character(name %||% "viridis")[1]
  direction <- if (isTRUE(direction == -1)) -1 else 1
  stops <- .msterp_palette_stops[[name]]
  if (is.null(stops)) {
    if (identical(name, "viridis")) {
      stops <- c("#440154", "#21908C", "#FDE725")
    } else {
      stops <- .msterp_palette_stops[["RdYlBu"]]
    }
  }
  if (direction == -1) stops <- rev(stops)
  stops
}
