# =========================================================
# engines/registry.R Engine Registry (single source of truth)
#
# Drives:
#  - pipeline editor UI (params + style controls)
#  - validation (requirements; sequential capability)
#  - results rendering expectations (outputs contract)
#
# Engine metadata flags (registry-level, not params/style):
#  - picker_hidden: hide engine from step/substep pickers (system/internal engines)
#  - locked_parent: for container engines, renders the parent node with reduced opacity in Results Viewer
#
# =========================================================
# THREE-SCHEMA CONTRACT
# =========================================================
#
# params_schema (COMPUTE-TIME - Pipeline only):
#   - Fields that affect computed output data
#   - Require re-running the pipeline to change
#   - Shown in Pipeline editor (basic section)
#   - NOT shown in Results Viewer
#   - Examples:
#     - Statistical mode (t-test vs ANOVA)
#     - Log transform choice
#     - Significance thresholds that define downstream gene lists
#     - FDR cutoffs that filter terms
#     - Compare mode (avg_groups vs within_groups)
#   - CRITICAL: Any threshold that determines child view content (e.g., GO gene lists)
#     MUST be in params_schema
#
# style_schema (DEFAULT STYLE - Both Pipeline and Results Viewer):
#   - Fields that affect aesthetics/presentation
#   - Can be changed in Results Viewer without re-running
#   - Shown in Pipeline editor under "Show more options" (as defaults)
#   - Shown in Results Viewer for live adjustment
#   - Stored in render_state.json overrides
#   - Examples:
#     - Colors (point, line, fill)
#     - Point/line sizes
#     - Opacity/alpha values
#     - Font sizes
#     - Plot dimensions (width/height)
#     - Show/hide toggles for annotations (labels, legends)
#
# viewer_schema (VIEWER-ONLY - Results Viewer only):
#   - Fields that only make sense in the Results Viewer context
#   - NOT shown in Pipeline editor
#   - Examples:
#     - Group selection dropdowns (populated from results data)
#     - Ontology filters (BP/MF/CC selection)
#     - Other runtime-only controls
#
# hidden = TRUE on any field: Not shown anywhere (internal use)
#
# =========================================================
# Alignment:
#  - Per-engine "Options" are style_schema keys
#  - mean_type for hor_dis + vert_dis moved to params_schema
#  - conditional UI relies on specific key names (color_mode, *_range_mode, show_ellipse, etc.)
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Session-level registry cache (avoids rebuilding 1800+ line registry on every call)
.registry_cache <- NULL

msterp_schema_field <- function(
    name,
    type = c("choice", "bool", "int", "num", "string", "range"),
    label = name,
    default = NULL,
    choices = NULL,
    choice_labels = NULL,
    min = NULL,
    max = NULL,
    advanced = FALSE,
    hidden = FALSE,
    help = NULL,
    viewer_only = FALSE  # DEPRECATED: use viewer_schema instead; kept for backward compatibility
) {
  type <- match.arg(type)
  list(
    name = name,
    type = type,
    label = label,
    default = default,
    choices = choices,
    choice_labels = choice_labels,
    min = min,
    max = max,
    advanced = isTRUE(advanced),
    hidden = isTRUE(hidden),
    help = help,
    viewer_only = isTRUE(viewer_only)  # DEPRECATED: kept for backward compatibility
  )
}

msterp_engine_registry <- function(force_rebuild = FALSE) {
  if (!is.null(.registry_cache) && !isTRUE(force_rebuild)) {
    return(.registry_cache)
  }

  registry_version <- 2L
  
  mk_style <- function(width = 7, height = 6, axis_text_size = 20) {
    list(
      msterp_schema_field(
        "axis_style", "choice", "Axis style",
        default = "clean", choices = c("clean", "bold"),
        advanced = TRUE  # Per-engine style control (no longer global toggle)
      ),
      msterp_schema_field(
        "axis_text_size", "int", "Axis text size",
        default = axis_text_size, min = 6, max = 40, advanced = TRUE
      ),
      msterp_schema_field(
        "width", "num", "Plot width (in)",
        default = width, min = 2, max = 24, advanced = TRUE
      ),
      msterp_schema_field(
        "height", "num", "Plot height (in)",
        default = height, min = 2, max = 24, advanced = TRUE
      )
    )
  }

  common_style <- mk_style(width = 7, height = 6, axis_text_size = 20)
  
  engines <- list(
    dataprocessor = list(
      engine_id = "dataprocessor",
      label = "Data Processor",
      category = "processing",
      supported_data_types = c("proteomics"),
      description = "Preprocessing utilities (filters, aggregation, contaminants, imputation).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "operation", "choice", "Operation",
          default = "filter_non_numeric",
          choices = c(
            "filter_threshold", "filter_prefix", "filter_keyword",
            "filter_non_numeric", "aggregate_rows", "average_rows",
            "tag_remove_contaminants", "impute"
          ),
          advanced = TRUE
        ),
        msterp_schema_field("plan_json", "string", "Plan (JSON)", default = "", advanced = TRUE)
      ),
      style_schema = list(),
      outputs = list(figures = c(), tables = c("dataprocessor_log"), interactive = FALSE),
      render_spec = list(plots = character(0), tables = c("dataprocessor_log"), tabs = NULL)
    ),

    peptide_analysis = list(
      engine_id = "peptide_analysis",
      type = "container",
      label = "Peptide Analysis",
      category = "processing",
      supported_data_types = c("proteomics"),
      description = "Container for peptide-level steps; auto-aggregates to protein-level before continuing.",
      supports_sequential = FALSE,
      accepted_input_levels = c("peptide"),
      locked_parent = TRUE,
      allowed_child_engines = c(
        "dataprocessor",
        "idquant_id_quant",
        "idquant_average_value",
        "idquant_cv_bar",
        "idquant_overlap",
        "idquant_overlap_detected",
        "idquant_overlap_quantified"
      ),
      # Auto-appended, non-removable final substep within this container:
      forced_final_substep_engine_id = "peptide_aggregate_to_protein",
      exit_level = "protein",
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("peptide")
      ),
      params_schema = list(),
      style_schema = list(),
      outputs = list(figures = c(), tables = c(), interactive = FALSE),
      render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    ),

    # ----------------------------
    # dSILAC Peptide Analysis Container
    # ----------------------------
    dsilac_peptide_analysis = list(
      engine_id = "dsilac_peptide_analysis",
      type = "container",
      label = "dSILAC Peptide Analysis",
      category = "processing",
      supported_data_types = c("proteomics"),
      description = "Container for peptide-level dynamic SILAC half-life calculation; aggregates to protein-level via harmonic mean.",
      supports_sequential = FALSE,
      accepted_input_levels = c("peptide"),
      locked_parent = TRUE,
      allowed_child_engines = c("dataprocessor", "half_life"),
      forced_final_substep_engine_id = "peptide_aggregate_to_protein",
      forced_final_substep_params = list(method = "harmonic_mean"),
      exit_level = "protein",
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("peptide"),
        requires_silac = TRUE
      ),
      params_schema = list(),
      style_schema = list(),
      outputs = list(figures = c(), tables = c(), interactive = FALSE),
      render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    ),

    peptide_aggregate_to_protein = list(
      engine_id = "peptide_aggregate_to_protein",
      label = "Aggregate Peptides → Proteins",
      category = "processing",
      supported_data_types = c("proteomics"),
      description = "Aggregates peptide-level data into protein-level by combining peptide values per protein ID.",
      supports_sequential = FALSE,
      accepted_input_levels = c("peptide"),
      output_level = "protein",
      # Always used as a forced container substep; hide from independent Add Steps
      picker_hidden = TRUE,
      # Keep visible in Results Viewer when system-generated (shows before/after table)
      results_hidden_system_generated = FALSE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "method", "choice", "Aggregation method",
          default = "sum",
          choices = c("sum", "arithmetic_mean", "harmonic_mean"),
          choice_labels = c("Sum", "Arithmetic Mean", "Harmonic Mean")
        )
      ),
      style_schema = list(),
      outputs = list(figures = c(), tables = c("aggregation_log"), interactive = FALSE),
      render_spec = list(plots = character(0), tables = c("aggregation_log"), tabs = NULL)
    ),

    # ----------------------------
    # Dynamic SILAC Half-Life Engine
    # ----------------------------
    half_life = list(
      engine_id = "half_life",
      label = "Half-Life (dSILAC)",
      category = "processing",
      supported_data_types = c("proteomics"),
      description = "Compute half-lives from dynamic SILAC Heavy/Light ratios at a single time point.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide"),
        requires_silac = TRUE
      ),
      params_schema = list(
        msterp_schema_field(
          "time_value", "num", "Pulse time",
          default = 24,
          min = 0.01,
          max = 100000,
          help = "Duration of the SILAC pulse (labeling time)."
        ),
        msterp_schema_field(
          "time_unit", "choice", "Time unit",
          default = "hour",
          choices = c("min", "hour", "day"),
          choice_labels = c("Minutes", "Hours", "Days")
        ),
        msterp_schema_field(
          "normalize_ratios", "bool", "Median-normalize H/L ratios",
          default = TRUE,
          help = "Median-normalize Heavy/Light ratios to correct for unequal SILAC mixing. Caution: median normalization removes global shifts in turnover — disable if a global half-life change is expected."
        ),
        msterp_schema_field(
          "ratio_min", "num", "Ratio filter: minimum H/L",
          default = 0.02,
          min = 0,
          max = 100,
          help = "Remove H/L ratios below this threshold before computing half-lives. Set to 0 to disable."
        ),
        msterp_schema_field(
          "ratio_max", "num", "Ratio filter: maximum H/L",
          default = 100,
          min = 1,
          max = 1000000,
          help = "Remove H/L ratios above this threshold before computing half-lives."
        )
      ),
      style_schema = c(
        mk_style(width = 7, height = 5, axis_text_size = 14),
        list(
          msterp_schema_field(
            "ratio_hist_bins", "int", "Ratio histogram bins",
            default = 50, min = 10, max = 200
          ),
          msterp_schema_field(
            "halflife_hist_bins", "int", "Half-life histogram bins",
            default = 50, min = 10, max = 200
          )
        )
      ),
      viewer_schema = list(),
      outputs = list(
        figures = c("ratio_distribution", "halflife_histogram", "replicate_concordance"),
        tables = c("half_life_log"),
        interactive = FALSE
      ),
      render_spec = list(
        plots = c("ratio_distribution", "halflife_histogram", "replicate_concordance"),
        tables = c("half_life_log"),
        tabs = NULL
      )
    ),

    # ----------------------------
    # Metabolite Analysis Container - Hidden for v1 release
    # ----------------------------
    # metabolite_analysis = list(
    #   engine_id = "metabolite_analysis",
    #   type = "container",
    #   label = "Metabolite Analysis",
    #   category = "processing",
    #   description = "Container for metabolite-level analysis steps. Unlike peptides, metabolites do not aggregate.",
    #   supports_sequential = FALSE,
    #   accepted_input_levels = c("metabolite"),
    #   locked_parent = TRUE,
    #   allowed_child_engines = c(
    #     "dataprocessor",
    #     "idquant_id_quant",
    #     "idquant_average_value",
    #     "idquant_cv_scatter",
    #     "idquant_cv_bar",
    #     "idquant_overlap",
    #     "idquant_overlap_detected",
    #     "idquant_overlap_quantified"
    #   ),
    #   # NO forced_final_substep - metabolites don't aggregate like peptides
    #   exit_level = "metabolite",
    #   requirements = list(
    #     min_groups = 1,
    #     requires_terpbase = FALSE,
    #     requires_metabobase = FALSE,
    #     required_ids = c("metabolite"),
    #     analysis_levels = c("metabolite")
    #   ),
    #   params_schema = list(),
    #   style_schema = list(),
    #   outputs = list(figures = c(), tables = c(), interactive = FALSE),
    #   render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    # ),

    # ----------------------------
    # QC
    # ----------------------------
    idquant = list(
      engine_id = "idquant",
      type = "container",
      label = "Quantitative QC",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Container for ID quantification summary views (protein, peptide, or metabolite).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      locked_parent = TRUE,
      allowed_child_engines = c(
        "idquant_id_quant",
        "idquant_average_value",
        "idquant_cv_scatter",
        "idquant_cv_bar",
        "idquant_overlap",
        "idquant_overlap_detected",
        "idquant_overlap_quantified"
      ),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = list(),
      outputs = list(figures = c(), tables = c(), interactive = FALSE),
      render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    ),

    # IDQuant container child engines (substeps)
    idquant_id_quant = list(
      engine_id = "idquant_id_quant",
      label = "ID & Quantification",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Quantification summary view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "y_limit_mode", "choice", "Y-limit mode",
            default = "auto", choices = c("auto", "manual"), advanced = TRUE
          ),
          msterp_schema_field("ymax_protein", "int", "Y max (protein)", default = 10000, min = 1, advanced = TRUE),
          msterp_schema_field(
            "show_values", "bool", "Show values",
            default = TRUE,
            help = "Display integer counts above bars."
          ),
          msterp_schema_field(
            "value_label_size", "num", "Value label size",
            default = 4, min = 1, max = 12,
            help = "Text size for value labels.",
            advanced = TRUE
          ),
          msterp_schema_field("color_quantified", "string", "Reproducibly Quantified color", default = "#1f77b4", advanced = TRUE),
          msterp_schema_field("color_identified", "string", "Quantified color", default = "#ff7f0e", advanced = TRUE),
          msterp_schema_field("show_bar_outline", "bool", "Show bar outline", default = FALSE, advanced = TRUE),
          msterp_schema_field("bar_outline_color", "string", "Outline color", default = "#000000", advanced = TRUE),
          msterp_schema_field("bar_outline_width", "num", "Outline width", default = 0.5, min = 0, max = 5, advanced = TRUE),
          msterp_schema_field(
            "show_significance", "bool", "Show significance",
            default = FALSE,
            help = "Display pairwise t-test significance brackets between groups."
          ),
          msterp_schema_field(
            "sig_display_mode", "choice", "Significance display",
            default = "stars", choices = c("stars", "pvalue"),
            help = "Show stars (*, **, ***) or p-values on brackets.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "sig_text_size", "num", "Significance text size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for significance labels.",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("idquant_group", "idquant_replicate"), tables = c("idquant_group", "idquant_replicate"), interactive = TRUE),
      render_spec = list(plots = c("idquant_group", "idquant_replicate"), tables = c("idquant_group", "idquant_replicate"), tabs = NULL)
    ),

    idquant_average_value = list(
      engine_id = "idquant_average_value",
      label = "Average Intensity",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Average value summary view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to values for plots/tables."
          ),
          msterp_schema_field(
            "bar_color_mode", "choice", "Bar color mode",
            default = "group", choices = c("group", "flat"),
            help = "Color bars by group, or use a single flat color.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_color", "string", "Bar color",
            default = "#1f77b4",
            help = "Flat bar color (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = TRUE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_width", "num", "Bar outline width",
            default = 0.5, min = 0, max = 5,
            help = "Outline width for bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_values", "bool", "Show values",
            default = TRUE,
            help = "Display values above bars."
          ),
          msterp_schema_field(
            "value_label_size", "num", "Value label size",
            default = 5, min = 1, max = 12,
            help = "Text size for value labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "y_axis_title", "string", "Y-axis title",
            default = "Intensity",
            help = "Custom label for Y-axis.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_error_bars", "bool", "Show error bars",
            default = FALSE,
            help = "Display SEM error bars on each group bar."
          ),
          msterp_schema_field(
            "show_significance", "bool", "Show significance",
            default = FALSE,
            help = "Display pairwise t-test significance brackets between groups."
          ),
          msterp_schema_field(
            "sig_display_mode", "choice", "Significance display",
            default = "stars", choices = c("stars", "pvalue"),
            help = "Show stars (*, **, ***) or p-values on brackets.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "sig_text_size", "num", "Significance text size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for significance labels.",
            advanced = TRUE
          )
        ),
        mk_style(width = 4, height = 5, axis_text_size = 20)
      ),
      outputs = list(
        figures = c("idquant_average_value", "idquant_average_value_replicate"),
        tables = c("idquant_average_value", "idquant_average_value_replicate"),
        interactive = TRUE
      ),
      render_spec = list(
        plots = c("idquant_average_value", "idquant_average_value_replicate"),
        tables = c("idquant_average_value", "idquant_average_value_replicate"),
        tabs = NULL
      )
    ),

    # Legacy IDQuant child views (viewer-only; retained for backward compatibility)
    idquant_group = list(
      engine_id = "idquant_group",
      label = "IDQuant: Group",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "IDQuant group-level summaries (legacy child view).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables."
          ),
          msterp_schema_field(
            "show_count_labels", "bool", "Show count labels",
            default = FALSE,
            help = "Display count values above bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_label_size", "num", "Count label size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("idquant_group_plot"), tables = c("idquant_group_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_group_plot"), tables = c("idquant_group_table"), tabs = NULL)
    ),

    idquant_replicate = list(
      engine_id = "idquant_replicate",
      label = "IDQuant: Replicate",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "IDQuant replicate-level summaries (legacy child view).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables."
          ),
          msterp_schema_field(
            "show_count_labels", "bool", "Show count labels",
            default = FALSE,
            help = "Display count values above bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_label_size", "num", "Count label size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("idquant_replicate_plot"), tables = c("idquant_replicate_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_replicate_plot"), tables = c("idquant_replicate_table"), tabs = NULL)
    ),

    # IDQuant: CV scatter (split engine)
    idquant_cv_scatter = list(
      engine_id = "idquant_cv_scatter",
      label = "Coef. Of Variation (CV) (scatter)",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "CV% scatter/table view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables."
          ),
          msterp_schema_field(
            "min_replicates", "int", "Minimum replicates",
            default = 2, min = 1, max = 10,
            help = "Minimum number of replicates required per group for CV calculation.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "x_axis_mode", "choice", "X-axis mode",
            default = "abundance", choices = c("abundance", "rank"),
            help = "X-axis shows (abundance) log-transformed abundance values, or (rank) abundance rank ordering.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "x_min", "num", "X min",
            default = 0,
            help = "Manual X-axis minimum (only used when set).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "x_max", "num", "X max",
            default = 12,
            help = "Manual X-axis maximum (only used when set).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_threshold", "num", "CV% threshold",
            default = 30, min = 0, max = 200,
            help = "Highlight/filter proteins with CV% above this threshold.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "threshold_show", "bool", "Show threshold line",
            default = FALSE,
            help = "Show the horizontal CV% threshold line on the scatter plot.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_labels", "bool", "Show labels",
            default = FALSE,
            help = "Show labels for highlighted proteins.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "threshold_color", "string", "Threshold line color",
            default = "gray60",
            help = "Color for the CV% threshold line.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "threshold_width", "num", "Threshold line width",
            default = 0.5, min = 0, max = 10,
            help = "Line width for the CV% threshold line.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "n_labels", "int", "# labels",
            default = 30, min = 0, max = 200,
            help = "Number of points to label (display names when available).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "point_color_mode", "choice", "Point color mode",
            default = "group", choices = c("group", "flat"),
            help = "Color points by group, or use a single flat color.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "flat_color", "string", "Flat point color",
            default = "#808080",
            help = "Color for scatter points when point color mode is 'flat' (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "point_size", "num", "Point size",
            default = 1.5, min = 0.1, max = 10,
            help = "Point size.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "point_alpha", "num", "Point alpha",
            default = 0.7, min = 0, max = 1,
            help = "Point opacity (0=transparent, 1=opaque).",
            advanced = TRUE
          )
        ),
        mk_style(width = 6, height = 5, axis_text_size = 20),
        list(
          msterp_schema_field(
            "cv_plot_mode", "choice", "Plot mode",
            default = "scatter", choices = c("scatter"),
            help = "Scatter plot only (split engine).",
            hidden = TRUE
          )
        )
      ),
      outputs = list(figures = c("idquant_cv_plot"), tables = c("idquant_cv_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_cv_plot"), tables = character(0), tabs = NULL)
    ),

    # Legacy IDQuant CV (combined scatter+bar; retained for backward compatibility)
    # Peptide Analysis: IDQuant CV bar-only variant (system-generated substep)
    idquant_cv_bar = list(
      engine_id = "idquant_cv_bar",
      label = "Coef. Of Variation (CV) (bar)",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "CV% bar chart view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("peptide", "protein")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "show_avg_cv", "bool", "Show average CV%",
            default = FALSE,
            help = "Display the average CV% value on top of each bar.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_threshold", "num", "CV% threshold",
            default = 30, min = 0, max = 200,
            help = "Highlight/filter proteins with CV% above this threshold.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "min_replicates", "int", "Minimum replicates",
            default = 2, min = 1, max = 10,
            help = "Minimum number of replicates required per group for CV calculation.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "num_bins", "int", "Number of bins",
            default = 4, min = 2, max = 6,
            help = "Number of CV% bins to display (2-6). Uses thresholds from the bin settings below.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_1", "num", "CV bin 1 threshold (%)",
            default = 10, min = 0, max = 500,
            help = "First CV% bin boundary (e.g., 0-10%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_1_color", "string", "CV bin 1 color",
            default = "#5C5C5C",
            help = "Color for the first CV% bin (0-X%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_2", "num", "CV bin 2 threshold (%)",
            default = 30, min = 0, max = 500,
            help = "Second CV% bin boundary (e.g., 10-20%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_2_color", "string", "CV bin 2 color",
            default = "#9C9C9C",
            help = "Color for the second CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_3", "num", "CV bin 3 threshold (%)",
            default = 100, min = 0, max = 500,
            help = "Third CV% bin boundary (e.g., 20-30%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_3_color", "string", "CV bin 3 color",
            default = "#C7C8C9",
            help = "Color for the third CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_4", "num", "CV bin 4 threshold (%)",
            default = 50, min = 0, max = 500,
            help = "Fourth CV% bin boundary (e.g., 30-50%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_4_color", "string", "CV bin 4 color",
            default = "#FC0000",
            help = "Color for the fourth CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_5", "num", "CV bin 5 threshold (%)",
            default = 100, min = 0, max = 500,
            help = "Fifth CV% bin boundary (e.g., 50-100%). Values above this threshold go to 'X%+' bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_5_color", "string", "CV bin 5 color",
            default = "#EF8A62",
            help = "Color for the fifth CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_6_color", "string", "CV bin 6 color",
            default = "#B2182B",
            help = "Color for the sixth CV% bin (highest CV%).",
            advanced = TRUE
          )
        ),
        mk_style(width = 6, height = 5, axis_text_size = 20),
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables.",
            hidden = TRUE
          )
        )
      ),
      outputs = list(figures = c("idquant_cv_plot"), tables = c("idquant_cv_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_cv_plot"), tables = character(0), tabs = NULL)
    ),

    idquant_overlap = list(
      engine_id = "idquant_overlap",
      label = "Overlaps",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Overlap view (UpSet).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "overlap_metric", "choice", "Overlap metric",
          default = "detected", choices = c("detected", "quantified"),
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "count_labels_show", "bool", "Show count labels",
            default = FALSE,
            help = "Show numeric count labels above intersection bars (UpSet).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_size", "num", "Count label size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_color", "string", "Count label color",
            default = "black",
            help = "Text color for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = FALSE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_fill_color", "string", "Bar fill color",
            default = "#4245FF",
            help = "Fill color for intersection bars (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_fill_alpha", "num", "Venn fill opacity",
            default = 0.4, min = 0, max = 1,
            help = "Opacity for Venn fills.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_show_percentage", "bool", "Show percentage",
            default = FALSE,
            help = "Show percentages inside the Venn diagram instead of counts.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_text_size", "num", "Venn value text size",
            default = 4, min = 1, max = 12,
            help = "Text size for values inside the Venn diagram.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_set_name_size", "num", "Venn set label size",
            default = 0, min = 0, max = 12,
            help = "Text size for set labels around the Venn diagram (0 hides labels).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_size", "num", "Venn outline size",
            default = 0.4, min = 0, max = 5,
            help = "Outline thickness for Venn circles.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_color", "string", "Venn outline color",
            default = "#000000",
            help = "Outline color for Venn circles (hex).",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "overlap_plot_type", "choice", "Plot type",
          default = "upset", choices = c("upset", "venn"),
          choice_labels = c("UpSet plot", "Venn diagram")
        )
      ),
      outputs = list(figures = c("idquant_overlap_plot"), tables = c("idquant_overlap_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_overlap_plot"), tables = character(0), tabs = NULL)
    ),

    idquant_overlap_detected = list(
      engine_id = "idquant_overlap_detected",
      label = "Overlaps (Quantified)",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Overlap view using quantified entries.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "overlap_metric", "choice", "Overlap metric",
          default = "detected", choices = c("detected", "quantified"),
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "count_labels_show", "bool", "Show count labels",
            default = TRUE,
            help = "Show numeric count labels above intersection bars (UpSet).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_size", "num", "Count label size",
            default = 4, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_color", "string", "Count label color",
            default = "black",
            help = "Text color for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = FALSE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_fill_color", "string", "Bar fill color",
            default = "#9E9E9E",
            help = "Fill color for intersection bars (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_fill_alpha", "num", "Venn fill opacity",
            default = 0.4, min = 0, max = 1,
            help = "Opacity for Venn fills.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_show_percentage", "bool", "Show percentage",
            default = FALSE,
            help = "Show percentages inside the Venn diagram instead of counts.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_text_size", "num", "Venn value text size",
            default = 4, min = 1, max = 12,
            help = "Text size for values inside the Venn diagram.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_set_name_size", "num", "Venn set label size",
            default = 5, min = 0, max = 12,
            help = "Text size for set labels around the Venn diagram (0 hides labels).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_size", "num", "Venn outline size",
            default = 0.5, min = 0, max = 5,
            help = "Outline thickness for Venn circles.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_color", "string", "Venn outline color",
            default = "#000000",
            help = "Outline color for Venn circles (hex).",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 18)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "overlap_plot_type", "choice", "Plot type",
          default = "venn", choices = c("upset", "venn"),
          choice_labels = c("UpSet plot", "Venn diagram")
        )
      ),
      outputs = list(figures = c("idquant_overlap_plot"), tables = c("idquant_overlap_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_overlap_plot"), tables = character(0), tabs = NULL)
    ),

    idquant_overlap_quantified = list(
      engine_id = "idquant_overlap_quantified",
      label = "Overlaps (Reproducibly Quantified)",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Overlap view using reproducibly quantified entries.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "overlap_metric", "choice", "Overlap metric",
          default = "quantified", choices = c("detected", "quantified"),
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "count_labels_show", "bool", "Show count labels",
            default = TRUE,
            help = "Show numeric count labels above intersection bars (UpSet).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_size", "num", "Count label size",
            default = 4, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_color", "string", "Count label color",
            default = "black",
            help = "Text color for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = FALSE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_fill_color", "string", "Bar fill color",
            default = "#9E9E9E",
            help = "Fill color for intersection bars (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_fill_alpha", "num", "Venn fill opacity",
            default = 0.4, min = 0, max = 1,
            help = "Opacity for Venn fills.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_show_percentage", "bool", "Show percentage",
            default = FALSE,
            help = "Show percentages inside the Venn diagram instead of counts.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_text_size", "num", "Venn value text size",
            default = 4, min = 1, max = 12,
            help = "Text size for values inside the Venn diagram.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_set_name_size", "num", "Venn set label size",
            default = 5, min = 0, max = 12,
            help = "Text size for set labels around the Venn diagram (0 hides labels).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_size", "num", "Venn outline size",
            default = 0.5, min = 0, max = 5,
            help = "Outline thickness for Venn circles.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_color", "string", "Venn outline color",
            default = "#000000",
            help = "Outline color for Venn circles (hex).",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 18)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "overlap_plot_type", "choice", "Plot type",
          default = "venn", choices = c("upset", "venn"),
          choice_labels = c("UpSet plot", "Venn diagram")
        )
      ),
      outputs = list(figures = c("idquant_overlap_plot"), tables = c("idquant_overlap_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_overlap_plot"), tables = character(0), tabs = NULL)
    ),
    scatter_correlation = list(
      engine_id = "scatter_correlation",
      label = "Scatter Correlation Plot",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Pairwise scatter plots with correlation statistics (Pearson, Spearman, Kendall).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      # params: things that actually change computed data
      params_schema = list(
        msterp_schema_field("compare_mode", "choice", "Compare", default = "avg_groups",
                            choices = c("avg_groups", "within_groups")),
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons against control (skip non-control pairwise comparisons)"
        ),
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none"))
      ),
      # style: all PDF "Options" live here
      style_schema = c(
        list(
          msterp_schema_field("point_color_mode", "choice", "Point color", default = "density",
                              choices = c("density", "flat"), advanced = TRUE),
          msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("point_size", "num", "Point size", default = 2, min = 0.5, max = 10, advanced = TRUE),
          msterp_schema_field("point_alpha", "num", "Point opacity", default = 1, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("line_type", "choice", "Best-fit line", default = "y_equals_x",
                              choices = c("none", "y_equals_x", "best_fit", "best_fit_zero_intercept"),
                              choice_labels = c("None", "y = x", "Best fit (y = mx + b)", "Best fit (y = mx)"),
                              advanced = TRUE),
          msterp_schema_field("line_color", "string", "Line color (hex)", default = "#FF0000", advanced = TRUE),
          msterp_schema_field("line_size", "num", "Line width", default = 1, min = 0.1, max = 3, advanced = TRUE),
          msterp_schema_field("show_correlation", "bool", "Show correlation", default = TRUE, advanced = TRUE),
          msterp_schema_field("show_equation", "bool", "Show equation", default = FALSE,
                              help = "Display line equation below correlation (only when a best-fit line is selected)",
                              advanced = TRUE),
          msterp_schema_field("corr_text_size", "int", "Correlation text size", default = 20, min = 6, max = 30, advanced = TRUE),
          msterp_schema_field("corr_color", "string", "Correlation color (hex)", default = "#000000", advanced = TRUE),
          msterp_schema_field("corr_position_x", "num", "Correlation position X (0-1)", default = 0.05, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("corr_position_y", "num", "Correlation position Y (0-1)", default = 0.95, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("axis_title", "string", "Axis title", default = "Intensity", advanced = TRUE),
          msterp_schema_field("xy_range_mode", "choice", "XY range", default = "auto",
                              choices = c("auto", "manual"), advanced = TRUE),
          msterp_schema_field("xy_min", "num", "XY min", default = 0, advanced = TRUE),
          msterp_schema_field("xy_max", "num", "XY max", default = 12, advanced = TRUE),
          msterp_schema_field("axis_style", "choice", "Axis style", default = "clean", choices = c("clean", "bold"), advanced = TRUE),
          msterp_schema_field("axis_text_size", "int", "Axis text size", default = 20, min = 6, max = 40, advanced = TRUE),
          msterp_schema_field("width",  "num", "Plot width (in)",  default = 6, min = 2, max = 24, advanced = TRUE),
          msterp_schema_field("height", "num", "Plot height (in)", default = 6, min = 2, max = 24, advanced = TRUE)
        )
      ),
      # viewer_schema: only shown in Results Viewer (not Pipeline editor)
      viewer_schema = list(
        msterp_schema_field("show_pearson", "bool", "Show Pearson (r)", default = FALSE),
        msterp_schema_field("show_spearman", "bool", "Show Spearman (\u03C1)", default = TRUE),
        msterp_schema_field("show_kendall", "bool", "Show Kendall (\u03C4)", default = FALSE),
        # Swap X/Y axes: swap which sample is on X vs Y axis
        msterp_schema_field(
          "swap_axes", "bool", "Swap X/Y axes",
          default = FALSE,
          help = "Swap which sample is on X vs Y axis"
        )
      ),
      outputs = list(figures = c("scatter_correlation"), tables = c("correlations"), interactive = TRUE),
      render_spec = list(plots = c("scatter_correlation"), tables = character(0), tabs = NULL)
    ),

    # Legacy alias for backwards compatibility
    spearman = list(
      engine_id = "scatter_correlation",
      label = "Scatter Correlation Plot",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Legacy alias for scatter_correlation.",
      picker_hidden = TRUE,
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(),
      style_schema = list(),
      viewer_schema = list(),
      outputs = list(figures = c("scatter_correlation"), tables = c("correlations"), interactive = TRUE),
      render_spec = list(plots = c("scatter_correlation"), tables = character(0), tabs = NULL),
      legacy_alias_for = "scatter_correlation"
    ),
    
    hor_dis = list(
      engine_id = "hor_dis",
      label = "Histogram / Density",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Distribution plots across groups / replicates.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("compare_mode", "choice", "Compare", default = "avg_groups",
                            choices = c("avg_groups", "within_groups", "all_reps"),
                            choice_labels = c("Average groups", "Within groups", "All replicates"))
      ),
      style_schema = list(
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none")),
        msterp_schema_field("plot_type", "choice", "Plot type", default = "histogram",
                            choices = c("density", "histogram"),
                            choice_labels = c("Density", "Histogram"), advanced = TRUE),
        msterp_schema_field("layout", "choice", "Layout", default = "separate",
                            choices = c("overlay", "separate"),
                            choice_labels = c("Overlay", "Separated"), advanced = TRUE),
        msterp_schema_field("x_axis_title", "string", "X-axis title", default = "Intensity", advanced = TRUE),
        msterp_schema_field("show_group_names", "bool", "Show group names", default = TRUE, advanced = TRUE),
        msterp_schema_field("color_mode", "choice", "Color", default = "group",
                            choices = c("group", "flat"), advanced = TRUE),
        msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity", default = 0.7, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("show_mean", "bool", "Show mean line", default = TRUE, advanced = TRUE),
        # Mean type: only visible when show_mean is true (conditional UI in page_results.R)
        msterp_schema_field("mean_type", "choice", "Mean type", default = "arithmetic",
                            choices = c("arithmetic", "harmonic"),
                            choice_labels = c("Arithmetic", "Harmonic"), advanced = TRUE),
        msterp_schema_field("show_mean_value", "bool", "Show mean value", default = TRUE, advanced = TRUE),
        msterp_schema_field("mean_line_size", "num", "Mean line thickness", default = 1, min = 0.2, max = 5, advanced = TRUE),
        msterp_schema_field("mean_text_size", "int", "Mean text size", default = 17, min = 6, max = 24, advanced = TRUE),
        # Pooling controls (viewer-time override) - applies pool_value * 0.8 safeguard
        msterp_schema_field("pool_above", "bool", "Pool above threshold", default = TRUE, advanced = TRUE),
        msterp_schema_field("pool_value", "num", "Pool threshold", default = 12, min = 1, max = 50, advanced = TRUE),
        msterp_schema_field("x_range_mode", "choice", "X range", default = "auto",
                            choices = c("auto", "manual"), advanced = TRUE),
        msterp_schema_field("x_min", "num", "X min", default = 0, advanced = TRUE),
        msterp_schema_field("x_max", "num", "X max", default = 12, advanced = TRUE),
        msterp_schema_field("axis_style", "choice", "Axis style", default = "clean", choices = c("clean", "bold"), advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size", default = 20, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width",  "num", "Plot width (in)",  default = 7, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)", default = 14, min = 2, max = 24, advanced = TRUE)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Group selector for within_groups mode (dynamically populated from results)
        msterp_schema_field("selected_group", "string", "Group", default = "")
      ),
      outputs = list(figures = c("hor_dis_plot"), tables = c("hor_dis_means"), interactive = FALSE),
      render_spec = list(plots = c("hor_dis_plot"), tables = character(0), tabs = NULL)
    ),

    vert_dis = list(
      engine_id = "vert_dis",
      label = "Box / Violin",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Vertical distribution plot across groups / replicates.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("compare_mode", "choice", "Compare", default = "avg_groups",
                            choices = c("avg_groups", "within_groups", "all_reps"),
                            choice_labels = c("Average groups", "Within groups", "All replicates"))
      ),
      style_schema = list(
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none")),
        msterp_schema_field("plot_type", "choice", "Plot type", default = "violin",
                            choices = c("box", "violin"),
                            choice_labels = c("Box plot", "Violin plot"), advanced = TRUE),
        msterp_schema_field("y_axis_title", "string", "Y-axis title", default = "Intensity", advanced = TRUE),
        msterp_schema_field("label_rotation", "choice", "Label rotation",
                            default = "0", choices = c("0", "45", "90"),
                            choice_labels = c("Horizontal", "45 degrees", "Vertical"), advanced = TRUE),
        msterp_schema_field("show_n", "bool", "Show n", default = TRUE, advanced = TRUE),
        msterp_schema_field("show_global_mean", "bool", "Show global mean",
                            default = TRUE, advanced = TRUE),
        # Mean type: only visible when show_global_mean is true (conditional UI in page_results.R)
        msterp_schema_field("mean_type", "choice", "Mean type", default = "arithmetic",
                            choices = c("arithmetic", "harmonic"),
                            choice_labels = c("Arithmetic", "Harmonic"), advanced = TRUE),
        msterp_schema_field("mean_line_color", "string", "Mean line color (hex)",
                            default = "#FF0000", advanced = TRUE),
        msterp_schema_field("mean_line_size", "num", "Mean line thickness",
                            default = 1, min = 0.2, max = 5, advanced = TRUE),
        # Per-group average label: annotate each box/violin with a numeric value
        msterp_schema_field("show_avg_label", "bool", "Show average label",
                            default = FALSE, advanced = TRUE),
        msterp_schema_field("avg_label_type", "choice", "Average type",
                            default = "median",
                            choices = c("median", "arithmetic", "harmonic"),
                            choice_labels = c("Median", "Arithmetic mean", "Harmonic mean"),
                            advanced = TRUE),
        msterp_schema_field("avg_label_color", "string", "Label color (hex)",
                            default = "#333333", advanced = TRUE),
        msterp_schema_field("avg_label_size", "num", "Label text size",
                            default = 3.5, min = 1, max = 12, advanced = TRUE),
        msterp_schema_field("color_mode", "choice", "Color", default = "group",
                            choices = c("group", "flat"), advanced = TRUE),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#B0B0B0", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity", default = 0.8,
                            min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("y_range_mode", "choice", "Y range", default = "auto",
                            choices = c("auto", "manual"), advanced = TRUE),
        msterp_schema_field("y_min", "num", "Y min", default = 0, advanced = TRUE),
        msterp_schema_field("y_max", "num", "Y max", default = 12, advanced = TRUE),
        msterp_schema_field("axis_style", "choice", "Axis style", default = "clean",
                            choices = c("clean", "bold"), advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size", default = 20,
                            min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)", default = 5,
                            min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)", default = 6,
                            min = 2, max = 24, advanced = TRUE)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Group selector for within_groups mode (dynamically populated from results)
        msterp_schema_field("selected_group", "string", "Group", default = "")
      ),
      outputs = list(figures = c("vert_dis_plot"), tables = c("vert_dis_means"), interactive = FALSE),
      render_spec = list(plots = c("vert_dis_plot"), tables = character(0), tabs = NULL)
    ),

    replicate_clustering = list(
      engine_id = "replicate_clustering",
      label = "Replicate Clustering",
      category = "qc",
      supported_data_types = c("proteomics", "metabolomics", "multi"),
      description = "Hierarchical clustering dendrogram of all replicates to assess group reproducibility.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("distance_method", "choice", "Distance metric",
          default = "pearson",
          choices = c("pearson", "spearman", "euclidean"),
          help = "Pearson/Spearman use 1-correlation as distance; Euclidean uses raw distances."
        ),
        msterp_schema_field("linkage_method", "choice", "Linkage method",
          default = "complete",
          choices = c("complete", "average", "ward.D2", "single")
        ),
        msterp_schema_field("log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "log2", "none")
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field("label_size", "num", "Label size",
            default = 3, min = 1, max = 10),
          msterp_schema_field("color_branches", "bool", "Color branches by group",
            default = FALSE, advanced = TRUE),
          msterp_schema_field("show_group_bar", "bool", "Show group color bar",
            default = TRUE),
          msterp_schema_field("hang", "num", "Leaf hang",
            default = 0.1, min = -1, max = 1, advanced = TRUE)
        ),
        mk_style(width = 8, height = 5, axis_text_size = 20)
      ),
      viewer_schema = list(),
      outputs = list(figures = c("dendrogram"), tables = c("cluster_info"), interactive = FALSE),
      render_spec = list(plots = c("dendrogram"), tables = c("cluster_info"), tabs = NULL)
    ),

    # ----------------------------
    # Trends
    # ----------------------------
    pca = list(
      engine_id = "pca",
      label = "PCA",
      category = "trends",
      supported_data_types = c("proteomics", "metabolomics", "multi"),
      description = "PCA scores + scree; optional loadings correlation analysis.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "log2", "none")
        ),
        msterp_schema_field(
          "scale_method", "choice", "Scaling method",
          default = "zscore",
          choices = c("zscore", "rank", "none"),
          help = "Z-score for standard PCA; rank-based is robust for non-normal distributions"
        ),
        msterp_schema_field("n_pcs", "int", "Number of PCs", default = 3, min = 2, max = 20),
        msterp_schema_field("top_n", "int", "Top N IDs (pos/neg)", default = 50, min = 1, max = 500),
        msterp_schema_field("loadings_corr", "bool", "Loadings correlation analysis", default = FALSE)
      ),
      style_schema = c(
        list(
          msterp_schema_field("point_size", "num", "Point size",
                              default = 5, min = 0.5, max = 10, advanced = TRUE),
          msterp_schema_field("point_alpha", "num", "Point opacity",
                              default = 0.6, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("show_ellipse", "bool", "Display ellipse",
                              default = TRUE, advanced = TRUE),
          msterp_schema_field("ellipse_alpha", "num", "Ellipse opacity",
                              default = 0.25, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("scree_bar_color", "string", "Scree bar color (hex)",
                              default = "#4682B4", advanced = TRUE),
          msterp_schema_field("scree_line_color", "string", "Scree line color (hex)",
                              default = "#E74C3C", advanced = TRUE),
          msterp_schema_field("scree_text_size", "num", "Scree text size",
                              default = 3.5, min = 1, max = 10, advanced = TRUE)
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("pca_scores", "pca_scree"),
                     tables = c("pca_loadings"), interactive = FALSE),
      render_spec = list(plots = c("pca_scores", "pca_scree"),
                         tables = character(0), tabs = NULL)
    ),

    heatmap = list(
      engine_id = "heatmap",
      label = "Targeted Heatmap",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Standalone heatmap for a user-provided target list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "target_list", "string", "Target list (one per line)",
          default = "",
          help = "Paste IDs one per line (max 2000). Unmatched entries are silently skipped."
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "none")
        ),
        msterp_schema_field(
          "cluster_rows", "bool", "Cluster rows",
          default = TRUE,
          help = "If TRUE, rows are clustered; if FALSE, preserves input order.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "autoscale", "bool", "Autoscale (z-score)",
          default = TRUE,
          hidden = TRUE,
          help = "Always compute z-scores to enable zscore/abundance toggle in viewer."
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "color_mode", "choice", "Color mode",
            default = "zscore", choices = c("zscore", "abundance"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "color_palette", "choice", "Color palette",
            default = "PuOr",
            choices = c("viridis", "RdBu", "RdYlBu", "Blues", "Reds", "PuOr"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_row_labels", "bool", "Show row labels",
            default = TRUE
          ),
          msterp_schema_field(
            "row_font_size", "int", "Row font size",
            default = 8, min = 4, max = 20, advanced = TRUE
          ),
          msterp_schema_field(
            "exclude_na_rows", "bool", "Hide rows with NAs",
            default = TRUE,
            help = "If TRUE, hides rows with any NA before clustering/plotting (viewer-time)."
          ),
          msterp_schema_field(
            "na_color", "string", "NA color (hex or named)",
            default = "grey50", advanced = TRUE
          ),
          msterp_schema_field(
            "width", "num", "Plot width (in)",
            default = 10, min = 2, max = 24, advanced = TRUE
          ),
          msterp_schema_field(
            "height", "num", "Plot height (in)",
            default = 8, min = 2, max = 24, advanced = TRUE
          ),
          msterp_schema_field(
            "cluster_k", "int", "Cluster preview (k)",
            default = 2, min = 2, max = 20,
            help = "Number of clusters to show.",
            hidden = TRUE  # Controlled by cluster analysis panel
          ),
          msterp_schema_field(
            "show_cluster_colors", "bool", "Show cluster colors",
            default = FALSE,
            help = "Show cluster membership color bar on left side of heatmap.",
            hidden = TRUE  # Controlled by cluster analysis panel
          ),
          # Clustering method configuration - placed before transpose/dendrogram for dropdown visibility
          msterp_schema_field(
            "cluster_method", "choice", "Clustering method",
            default = "hierarchical",
            choices = c("hierarchical", "kmeans", "kmedians"),
            choice_labels = c("Hierarchical", "K-means", "K-medians"),
            help = "K-means/K-medians replace dendrogram with cluster color bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "distance_method", "choice", "Distance method",
            default = "correlation",
            choices = c("correlation", "euclidean", "manhattan"),
            choice_labels = c("Correlation-based", "Euclidean", "Manhattan"),
            help = "Only used for hierarchical clustering.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "correlation_type", "choice", "Correlation type",
            default = "spearman",
            choices = c("spearman", "pearson", "kendall"),
            choice_labels = c("Spearman", "Pearson", "Kendall"),
            help = "Only used when distance method is correlation-based.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "linkage_method", "choice", "Linkage method",
            default = "average",
            choices = c("average", "complete", "single", "ward.D2"),
            choice_labels = c("Average", "Complete", "Single", "Ward"),
            help = "Only used for hierarchical clustering.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "transpose", "bool", "Transpose (features as columns)",
            default = FALSE,
            help = "Flip orientation so features become columns and samples become rows.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_dendrogram", "bool", "Show dendrogram",
            default = TRUE,
            help = "Show or hide the dendrogram visualization (only for hierarchical clustering).",
            advanced = TRUE
          )
        )
      ),
      outputs = list(figures = c("heatmap"), tables = c("heatmap_data"), interactive = FALSE),
      render_spec = list(plots = c("heatmap"), tables = character(0), tabs = NULL)
    ),

    ftest_heatmap = list(
      engine_id = "ftest_heatmap",
      label = "F-test Heatmap",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Heatmap of statistically significant features from multi-group F-test.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "stat_test", "choice", "Statistical test",
          default = "welch_anova",
          choices = c("welch_anova", "kruskal_wallis", "limma"),
          choice_labels = c("Welch's ANOVA", "Kruskal-Wallis", "limma (moderated F)")
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "none")
        ),
        msterp_schema_field(
          "padj_threshold", "num", "FDR threshold",
          default = 0.05, min = 0, max = 1,
          help = "Benjamini-Hochberg adjusted p-value threshold for significance."
        ),
        msterp_schema_field(
          "top_n", "int", "Max features",
          default = 100, min = 1, max = 300,
          help = "Maximum number of significant features to display (sorted by significance)."
        ),
        msterp_schema_field(
          "max_missingness", "num", "Max % Missing Replicates",
          default = 60, min = 0, max = 100,
          help = "Maximum percentage of missing values allowed per row (e.g., 60% means row needs at least 40% valid values)."
        ),
        msterp_schema_field(
          "normalize", "bool", "Median centering",
          default = FALSE,
          help = "Center each sample to the global median before statistical testing.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "cluster_rows", "bool", "Cluster rows",
          default = TRUE,
          help = "If TRUE, rows are hierarchically clustered.",
          advanced = TRUE
        )
      ),
      style_schema = list(
        msterp_schema_field(
          "color_mode", "choice", "Color mode",
          default = "zscore", choices = c("zscore", "abundance"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "color_palette", "choice", "Color palette",
          default = "PuOr",
          choices = c("viridis", "RdBu", "RdYlBu", "Blues", "Reds", "PuOr"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_sig_labels", "bool", "Show significance stars",
          default = FALSE,
          help = "Append *, **, *** to row labels based on adjusted p-value.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_row_labels", "bool", "Show row labels",
          default = TRUE
        ),
        msterp_schema_field(
          "row_font_size", "int", "Row font size",
          default = 7, min = 4, max = 20, advanced = TRUE
        ),
        msterp_schema_field(
          "exclude_na_rows", "bool", "Exclude rows with any NA",
          default = TRUE
        ),
        msterp_schema_field(
          "na_color", "string", "NA color (hex or named)",
          default = "grey50", advanced = TRUE
        ),
        msterp_schema_field(
          "width", "num", "Plot width (in)",
          default = 7, min = 2, max = 24, advanced = TRUE
        ),
        msterp_schema_field(
          "height", "num", "Plot height (in)",
          default = 15, min = 2, max = 24, advanced = TRUE
        ),
        msterp_schema_field(
          "cluster_k", "int", "Cluster preview (k)",
          default = 2, min = 2, max = 20,
          help = "Number of clusters to show.",
          hidden = TRUE  # Controlled by cluster analysis panel
        ),
        msterp_schema_field(
          "show_cluster_colors", "bool", "Show cluster colors",
          default = FALSE,
          help = "Show cluster membership color bar on left side of heatmap.",
          hidden = TRUE  # Controlled by cluster analysis panel
        ),
        # Clustering method configuration - placed before transpose/dendrogram for dropdown visibility
        msterp_schema_field(
          "cluster_method", "choice", "Clustering method",
          default = "hierarchical",
          choices = c("hierarchical", "kmeans", "kmedians"),
          choice_labels = c("Hierarchical", "K-means", "K-medians"),
          help = "K-means/K-medians replace dendrogram with cluster color bars.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "distance_method", "choice", "Distance method",
          default = "correlation",
          choices = c("correlation", "euclidean", "manhattan"),
          choice_labels = c("Correlation-based", "Euclidean", "Manhattan"),
          help = "Only used for hierarchical clustering.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "correlation_type", "choice", "Correlation type",
          default = "spearman",
          choices = c("spearman", "pearson", "kendall"),
          choice_labels = c("Spearman", "Pearson", "Kendall"),
          help = "Only used when distance method is correlation-based.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "linkage_method", "choice", "Linkage method",
          default = "average",
          choices = c("average", "complete", "single", "ward.D2"),
          choice_labels = c("Average", "Complete", "Single", "Ward"),
          help = "Only used for hierarchical clustering.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "transpose", "bool", "Transpose (features as columns)",
          default = FALSE,
          help = "Flip orientation so features become columns and samples become rows.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_dendrogram", "bool", "Show dendrogram",
          default = TRUE,
          help = "Show or hide the dendrogram visualization (only for hierarchical clustering).",
          advanced = TRUE
        )
      ),
      outputs = list(figures = c("heatmap"), tables = c("heatmap_data", "stats_table"), interactive = FALSE),
      render_spec = list(plots = c("heatmap"), tables = c("stats_table"), tabs = NULL)
    ),

    fc_heatmap = list(
      engine_id = "fc_heatmap",
      label = "FC Targeted Heatmap",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Heatmap of fold changes for a user-provided target list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "target_list", "string", "Target list (one per line)",
          default = "",
          help = "Paste IDs one per line (max 2000). Unmatched entries are silently skipped."
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log FC transform",
          default = "log2", choices = c("log2", "log10", "none"),
          choice_labels = c("log2 FC", "log10 FC", "Raw FC ratio"),
          help = "Transform applied to fold change values."
        ),
        msterp_schema_field(
          "control_only", "bool", "Compare vs control only",
          default = FALSE,
          help = "Only generate comparisons against the control group (e.g., KO1/Control, KO2/Control). If unchecked, includes all pairwise comparisons."
        ),
        msterp_schema_field(
          "cluster_rows", "bool", "Cluster rows",
          default = TRUE,
          help = "If TRUE, rows are clustered; if FALSE, preserves input order.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "is_log_transformed", "bool", "Data is already log-transformed",
          default = FALSE,
          help = "Enable if your data is already log2-transformed. Skips log2(x+1) transform for fold change calculation.",
          advanced = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "color_mode", "choice", "Color mode",
            default = "zscore", choices = c("zscore", "fc"),
            choice_labels = c("Z-score", "Fold Change"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "color_palette", "choice", "Color palette",
            default = "RdBu",
            choices = c("viridis", "RdBu", "RdYlBu", "PuOr"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_row_labels", "bool", "Show row labels",
            default = TRUE
          ),
          msterp_schema_field(
            "row_font_size", "int", "Row font size",
            default = 8, min = 4, max = 20, advanced = TRUE
          ),
          msterp_schema_field(
            "exclude_na_rows", "bool", "Hide rows with NAs",
            default = TRUE,
            help = "If TRUE, hides rows with any NA before plotting."
          ),
          msterp_schema_field(
            "na_color", "string", "NA color (hex or named)",
            default = "grey50", advanced = TRUE
          ),
          msterp_schema_field(
            "width", "num", "Plot width (in)",
            default = 10, min = 2, max = 24, advanced = TRUE
          ),
          msterp_schema_field(
            "height", "num", "Plot height (in)",
            default = 8, min = 2, max = 24, advanced = TRUE
          ),
          msterp_schema_field(
            "cluster_k", "int", "Cluster preview (k)",
            default = 2, min = 2, max = 20,
            help = "Number of clusters to show.",
            hidden = TRUE
          ),
          msterp_schema_field(
            "show_cluster_colors", "bool", "Show cluster colors",
            default = FALSE,
            help = "Show cluster membership color bar on left side of heatmap.",
            hidden = TRUE
          ),
          # Clustering method configuration - placed before transpose/dendrogram for dropdown visibility
          msterp_schema_field(
            "cluster_method", "choice", "Clustering method",
            default = "hierarchical",
            choices = c("hierarchical", "kmeans", "kmedians"),
            choice_labels = c("Hierarchical", "K-means", "K-medians"),
            help = "K-means/K-medians replace dendrogram with cluster color bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "distance_method", "choice", "Distance method",
            default = "correlation",
            choices = c("correlation", "euclidean", "manhattan"),
            choice_labels = c("Correlation-based", "Euclidean", "Manhattan"),
            help = "Only used for hierarchical clustering.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "correlation_type", "choice", "Correlation type",
            default = "spearman",
            choices = c("spearman", "pearson", "kendall"),
            choice_labels = c("Spearman", "Pearson", "Kendall"),
            help = "Only used when distance method is correlation-based.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "linkage_method", "choice", "Linkage method",
            default = "average",
            choices = c("average", "complete", "single", "ward.D2"),
            choice_labels = c("Average", "Complete", "Single", "Ward"),
            help = "Only used for hierarchical clustering.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "transpose", "bool", "Transpose (features as columns)",
            default = FALSE,
            help = "Flip orientation so features become columns and comparisons become rows.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_dendrogram", "bool", "Show dendrogram",
            default = TRUE,
            help = "Show or hide the dendrogram visualization (only for hierarchical clustering).",
            advanced = TRUE
          )
        )
      ),
      outputs = list(figures = c("fc_heatmap"), tables = c("fc_heatmap_data"), interactive = FALSE),
      render_spec = list(plots = c("fc_heatmap"), tables = character(0), tabs = NULL)
    ),

    fc_ftest_heatmap = list(
      engine_id = "fc_ftest_heatmap",
      label = "FC F-test Heatmap",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Heatmap of fold changes for statistically significant features from multi-group F-test.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "stat_test", "choice", "Statistical test",
          default = "welch_anova",
          choices = c("welch_anova", "kruskal_wallis", "limma"),
          choice_labels = c("Welch's ANOVA", "Kruskal-Wallis", "limma (moderated F)")
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log FC transform",
          default = "log2", choices = c("log2", "log10", "none"),
          choice_labels = c("log2 FC", "log10 FC", "Raw FC ratio"),
          help = "Transform applied to fold change values."
        ),
        msterp_schema_field(
          "control_only", "bool", "Compare vs control only",
          default = FALSE,
          help = "Only generate comparisons against the control group (e.g., KO1/Control, KO2/Control). If unchecked, includes all pairwise comparisons."
        ),
        msterp_schema_field(
          "padj_threshold", "num", "FDR threshold",
          default = 0.05, min = 0, max = 1,
          help = "Benjamini-Hochberg adjusted p-value threshold for significance."
        ),
        msterp_schema_field(
          "top_n", "int", "Max features",
          default = 100, min = 1, max = 300,
          help = "Maximum number of significant features to display (sorted by significance)."
        ),
        msterp_schema_field(
          "max_missingness", "num", "Max % Missing Replicates",
          default = 60, min = 0, max = 100,
          help = "Maximum percentage of missing values allowed per row."
        ),
        msterp_schema_field(
          "normalize", "bool", "Median centering",
          default = FALSE,
          help = "Center each sample to the global median before statistical testing.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "cluster_rows", "bool", "Cluster rows",
          default = TRUE,
          help = "If TRUE, rows are hierarchically clustered.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "is_log_transformed", "bool", "Data is already log-transformed",
          default = FALSE,
          help = "Enable if your data is already log2-transformed. Skips log2(x+1) transform for fold change calculation.",
          advanced = TRUE
        )
      ),
      style_schema = list(
        msterp_schema_field(
          "color_mode", "choice", "Color mode",
          default = "zscore", choices = c("zscore", "fc"),
          choice_labels = c("Z-score", "Fold Change"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "color_palette", "choice", "Color palette",
          default = "RdBu",
          choices = c("viridis", "RdBu", "RdYlBu", "PuOr"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_sig_labels", "bool", "Show significance stars",
          default = FALSE,
          help = "Append *, **, *** to row labels based on adjusted p-value.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_row_labels", "bool", "Show row labels",
          default = TRUE
        ),
        msterp_schema_field(
          "row_font_size", "int", "Row font size",
          default = 7, min = 4, max = 20, advanced = TRUE
        ),
        msterp_schema_field(
          "exclude_na_rows", "bool", "Exclude rows with any NA",
          default = TRUE
        ),
        msterp_schema_field(
          "na_color", "string", "NA color (hex or named)",
          default = "grey50", advanced = TRUE
        ),
        msterp_schema_field(
          "width", "num", "Plot width (in)",
          default = 7, min = 2, max = 24, advanced = TRUE
        ),
        msterp_schema_field(
          "height", "num", "Plot height (in)",
          default = 15, min = 2, max = 24, advanced = TRUE
        ),
        msterp_schema_field(
          "cluster_k", "int", "Cluster preview (k)",
          default = 2, min = 2, max = 20,
          help = "Number of clusters to show.",
          hidden = TRUE
        ),
        msterp_schema_field(
          "show_cluster_colors", "bool", "Show cluster colors",
          default = FALSE,
          help = "Show cluster membership color bar on left side of heatmap.",
          hidden = TRUE
        ),
        # Clustering method configuration - placed before transpose/dendrogram for dropdown visibility
        msterp_schema_field(
          "cluster_method", "choice", "Clustering method",
          default = "hierarchical",
          choices = c("hierarchical", "kmeans", "kmedians"),
          choice_labels = c("Hierarchical", "K-means", "K-medians"),
          help = "K-means/K-medians replace dendrogram with cluster color bars.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "distance_method", "choice", "Distance method",
          default = "correlation",
          choices = c("correlation", "euclidean", "manhattan"),
          choice_labels = c("Correlation-based", "Euclidean", "Manhattan"),
          help = "Only used for hierarchical clustering.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "correlation_type", "choice", "Correlation type",
          default = "spearman",
          choices = c("spearman", "pearson", "kendall"),
          choice_labels = c("Spearman", "Pearson", "Kendall"),
          help = "Only used when distance method is correlation-based.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "linkage_method", "choice", "Linkage method",
          default = "average",
          choices = c("average", "complete", "single", "ward.D2"),
          choice_labels = c("Average", "Complete", "Single", "Ward"),
          help = "Only used for hierarchical clustering.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "transpose", "bool", "Transpose (features as columns)",
          default = FALSE,
          help = "Flip orientation so features become columns and comparisons become rows.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_dendrogram", "bool", "Show dendrogram",
          default = TRUE,
          help = "Show or hide the dendrogram visualization (only for hierarchical clustering).",
          advanced = TRUE
        )
      ),
      outputs = list(figures = c("fc_heatmap"), tables = c("fc_heatmap_data", "stats_table"), interactive = FALSE),
      render_spec = list(plots = c("fc_heatmap"), tables = c("stats_table"), tabs = NULL)
    ),

    gene_barchart = list(
      engine_id = "gene_barchart",
      label = "Feature Bar Chart",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Bar chart visualization of individual feature values across sample groups or comparisons.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "targets", "string", "Targets (JSON)",
          default = "[]",
          hidden = TRUE
        ),
        msterp_schema_field(
          "data_type", "choice", "Data type",
          default = "zscore",
          choices = c("zscore", "raw"),
          hidden = TRUE
        ),
        msterp_schema_field(
          "parent_engine_id", "string", "Parent engine",
          default = "",
          hidden = TRUE
        )
      ),
      style_schema = list(
        msterp_schema_field(
          "y_range_mode", "choice", "Y-axis range",
          default = "auto",
          choices = c("auto", "manual"),
          choice_labels = c("Auto", "Manual")
        ),
        msterp_schema_field(
          "y_min", "num", "Y min",
          default = 0,
          advanced = TRUE
        ),
        msterp_schema_field(
          "y_max", "num", "Y max",
          default = 10,
          advanced = TRUE
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "none",
          choices = c("none", "log2", "log10"),
          help = "Apply log transform to values (only for raw data type)."
        ),
        msterp_schema_field(
          "bar_color_mode", "choice", "Bar color mode",
          default = "group",
          choices = c("group", "flat"),
          choice_labels = c("Group colors", "Flat color")
        ),
        msterp_schema_field(
          "bar_color", "string", "Bar color",
          default = "#4285F4",
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_bar_outline", "bool", "Show bar outline",
          default = FALSE,
          advanced = TRUE
        ),
        msterp_schema_field(
          "bar_outline_width", "num", "Outline width",
          default = 0.5,
          min = 0.1,
          max = 3,
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_error_bars", "bool", "Show error bars (SEM)",
          default = TRUE,
          help = "Display standard error of mean (SEM) error bars."
        ),
        msterp_schema_field(
          "show_significance", "bool", "Show significance",
          default = TRUE,
          help = "Display pairwise significance bars with stars or p-values."
        ),
        msterp_schema_field(
          "sig_display_mode", "choice", "Significance format",
          default = "stars",
          choices = c("stars", "pvalue"),
          choice_labels = c("Stars (*, **, ***)", "P-value"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "sig_text_size", "num", "Significance text size",
          default = 3,
          min = 2,
          max = 8,
          advanced = TRUE,
          help = "Text size for significance stars or p-values."
        ),
        msterp_schema_field(
          "title_text_size", "int", "Title size",
          default = 16,
          min = 8,
          max = 32,
          help = "Feature name title size."
        ),
        msterp_schema_field(
          "axis_text_size", "int", "Axis text size",
          default = 14,
          min = 6,
          max = 24,
          advanced = TRUE
        ),
        msterp_schema_field(
          "group_label_size", "int", "Group label size",
          default = 12,
          min = 6,
          max = 24,
          advanced = TRUE
        ),
        msterp_schema_field(
          "axis_style", "choice", "Axis style",
          default = "clean",
          choices = c("clean", "bold"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "width", "num", "Plot width (in)",
          default = 6,
          min = 2,
          max = 24,
          advanced = TRUE
        ),
        msterp_schema_field(
          "height", "num", "Plot height (in)",
          default = 5,
          min = 2,
          max = 24,
          advanced = TRUE
        )
      ),
      viewer_schema = list(
        msterp_schema_field(
          "selected_gene", "choice", "Select feature",
          default = "",
          choices = c(""),
          help = "Browse between features in this bar chart set."
        )
      ),
      outputs = list(figures = c("gene_barchart"), tables = character(0), interactive = FALSE),
      render_spec = list(plots = c("gene_barchart"), tables = character(0), tabs = NULL)
    ),

    # ----------------------------
    # Comparison
    # ----------------------------
    volcano = list(
      engine_id = "volcano",
      label = "Volcano",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Pairwise volcano plots + summaries.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "metabolite")
      ),
      # COMPUTE-TIME: These params determine sig up/down feature lists for GO children
      params_schema = list(
        msterp_schema_field(
          "stat_mode", "choice", "Statistic",
          default = "ttest", choices = c("ttest", "limma"),
          help = "Default: ttest. limma runs an empirical Bayes moderated t-test."
        ),
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons against control (e.g., KO1 vs Control, KO2 vs Control). If no control group is defined, this is ignored."
        ),
        msterp_schema_field(
          "fc_transform", "choice", "Fold-change transform",
          default = "log2", choices = c("none", "log2", "log10")
        ),
        # CRITICAL: Significance semantics for paired GO children - MUST be compute-time
        # apply_fdr: Whether to use FDR-adjusted p-values (TRUE) or raw p-values (FALSE)
        msterp_schema_field(
          "apply_fdr", "bool", "Apply FDR correction",
          default = TRUE,
          help = "Use FDR-adjusted p-values (TRUE) or raw p-values (FALSE). Frozen for child GO analyses."
        ),
        msterp_schema_field(
          "fc_threshold", "range", "Fold-change threshold (min, max)",
          default = c(-1, 1), min = -10, max = 10,
          help = "Defines significant up/down boundaries for child GO analyses"
        ),
        msterp_schema_field(
          "p_threshold", "choice", "p threshold",
          default = "0.05",
          choices = c("0.05", "0.03", "0.01", "0.005", "0.001"),
          help = "p-value cutoff for significance classification (applies to FDR-adjusted or raw p-values based on apply_fdr)"
        ),
        msterp_schema_field("show_summary_table", "bool", "Show summary table", default = TRUE, hidden = TRUE),
        msterp_schema_field(
          "is_log_transformed", "bool", "Data is already log-transformed",
          default = FALSE,
          help = "Enable if your data is already log2-transformed. Skips log2(x+1) transform for fold change and limma input.",
          advanced = TRUE
        )
      ),
      # VIEWER-TIME: Pure aesthetics, changeable in Results Viewer
      style_schema = c(
        list(
          msterp_schema_field(
            "label_mode", "choice", "Label mode",
            default = "color_sig",
            choices = c("color_sig", "hide_nonsig"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_summary_cards", "bool", "Show summary info",
            default = TRUE,
            advanced = TRUE,
            help = "Display summary statistics as subtitle on plot"
          ),
          msterp_schema_field(
            "summary_text_size", "num", "Summary text size",
            default = 3.5,
            min = 1,
            max = 8,
            advanced = TRUE,
            help = "Text size for summary info cards"
          ),
          msterp_schema_field("show_cut_lines", "bool", "Show cutoff lines", default = FALSE, advanced = TRUE),
          msterp_schema_field("point_size", "num", "Point size", default = 3, min = 0.5, max = 10, advanced = TRUE),
          # FIX: Add label font size control for volcano feature labels
          msterp_schema_field("label_font_size", "int", "Label font size", default = 12, min = 6, max = 30, advanced = TRUE),

          msterp_schema_field("col_sig_up", "string", "Color: significant up (hex)", default = "#FF4242", advanced = TRUE),
          msterp_schema_field("col_sig_down", "string", "Color: significant down (hex)", default = "#4245FF", advanced = TRUE),
          msterp_schema_field("col_nonsig", "string", "Color: non-significant (hex)", default = "#B0B0B0", advanced = TRUE),

          # Axis range controls
          msterp_schema_field("x_range_mode", "choice", "X range", default = "auto",
                              choices = c("auto", "manual"), advanced = TRUE),
          msterp_schema_field("x_min", "num", "X min", default = -7, advanced = TRUE),
          msterp_schema_field("x_max", "num", "X max", default = 7, advanced = TRUE),
          msterp_schema_field("y_range_mode", "choice", "Y range", default = "auto",
                              choices = c("auto", "manual"), advanced = TRUE),
          msterp_schema_field("y_max", "num", "Y max", default = 7, advanced = TRUE)
        ),
        common_style,
        list(
          msterp_schema_field("y_min", "num", "Y min", default = 0, min = 0, hidden = TRUE)
        )
      ),
      viewer_schema = list(
        msterp_schema_field(
          "view_mode", "choice", "Viewer mode",
          default = "export_preview", choices = c("export_preview", "interactive"),
          choice_labels = c("Export preview", "Interactive")
        ),
        # Flip FC direction: negates log2fc values per-plot (swap numerator/denominator interpretation)
        msterp_schema_field(
          "flip_fc", "bool", "Flip fold change direction",
          default = FALSE,
          help = "Negate log2FC values to swap up/down interpretation (GroupA/GroupB becomes GroupB/GroupA)"
        ),
        # label_targets: per-plot map stored in label_targets_map (JSON); UI renders custom control
        # Moved from style_schema to viewer_schema - default is empty (no pre-filled labels)
        msterp_schema_field(
          "label_targets_map", "string", "Per-plot labels (JSON)",
          default = "{}"
        ),
        # highlight_groups: per-plot map of highlight groups (JSON); each group has color, outline, targets
        msterp_schema_field(
          "highlight_groups_map", "string", "Per-plot highlight groups (JSON)",
          default = "{}"
        )
      ),
      outputs = list(
        figures = c("volcano_plot"),
        tables = c("volcano_summary"),
        interactive = TRUE,
        plotly_allowed = TRUE,
        default_plot_mode = "ggplot",
        click_target = "target"
      ),
      render_spec = list(plots = c("volcano_plot"), tables = c("volcano_summary"), tabs = NULL)
    ),

    rankplot = list(
      engine_id = "rankplot",
      label = "Rank Plot",
      category = "comparison",
      supported_data_types = c("proteomics", "metabolomics"),
      description = "Scatter plot of values vs rank (lowest value = rank 1, ascending order).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide", "metabolite")
      ),
      params_schema = list(
        msterp_schema_field(
          "value_transform", "choice", "Value transform",
          default = "none", choices = c("none", "log2", "log10"),
          help = "Transform applied to values before ranking."
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "y_axis_title", "string", "Y-axis title",
            default = "Intensity",
            help = "Label for the Y-axis (e.g., Intensity, Half-life)."
          ),
          msterp_schema_field(
            "rank_axis_mode", "choice", "Rank axis mode",
            default = "rank", choices = c("rank", "percent_rank"),
            choice_labels = c("Rank", "% Rank"),
            help = "Display rank as absolute rank or as percent rank (rank / max rank * 100)."
          ),
          msterp_schema_field(
            "min_replicates", "int", "Min replicates",
            default = 1, min = 1, max = 100,
            help = "Minimum number of non-NA replicates required per feature. Features with fewer replicates are excluded from the plot."
          ),
          msterp_schema_field(
            "highlight_mode", "choice", "Highlight mode",
            default = "none", choices = c("none", "threshold", "topn"),
            choice_labels = c("None", "By threshold", "By top N"),
            help = "How to highlight points: by value threshold or by rank."
          ),
          # Threshold mode fields (shown when highlight_mode == "threshold")
          msterp_schema_field(
            "threshold_highlight_above", "num", "Highlight above threshold",
            default = NA,
            help = "Highlight points with values above this threshold."
          ),
          msterp_schema_field(
            "threshold_highlight_below", "num", "Highlight below threshold",
            default = NA,
            help = "Highlight points with values below this threshold."
          ),
          # Top-N mode fields (shown when highlight_mode == "topn")
          msterp_schema_field(
            "topn_top", "int", "Top N (lowest values)",
            default = 0, min = 0, max = 5000,
            help = "Number of lowest-ranked items (rank 1, 2, 3... = lowest values) to highlight."
          ),
          msterp_schema_field(
            "topn_bottom", "int", "Bottom N (highest values)",
            default = 0, min = 0, max = 5000,
            help = "Number of highest-ranked items (highest values) to highlight."
          ),
          # Colors
          msterp_schema_field(
            "point_color", "string", "Point color",
            default = "#B0B0B0", advanced = TRUE,
            help = "Color for non-highlighted points."
          ),
          msterp_schema_field(
            "highlight_color_top", "string", "Highlight color (top/above)",
            default = "#FF4242", advanced = TRUE,
            help = "Color for highlighted top/above-threshold points."
          ),
          msterp_schema_field(
            "highlight_color_bottom", "string", "Highlight color (bottom/below)",
            default = "#4245FF", advanced = TRUE,
            help = "Color for highlighted bottom/below-threshold points."
          ),
          msterp_schema_field(
            "point_size", "num", "Point size",
            default = 2, min = 0.5, max = 10, advanced = TRUE
          ),
          msterp_schema_field(
            "point_alpha", "num", "Point opacity",
            default = 0.7, min = 0, max = 1, advanced = TRUE
          ),
          msterp_schema_field(
            "label_font_size", "int", "Label font size",
            default = 12, min = 6, max = 30, advanced = TRUE
          )
        ),
        mk_style(width = 8, height = 6, axis_text_size = 20)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "view_mode", "choice", "Viewer mode",
          default = "export_preview", choices = c("export_preview", "interactive"),
          choice_labels = c("Export preview", "Interactive")
        ),
        msterp_schema_field(
          "selected_group", "choice", "Group",
          default = "", choices = c(""),
          help = "Select which group to display."
        ),
        msterp_schema_field(
          "label_targets_map", "string", "Per-plot labels (JSON)",
          default = "{}"
        ),
        msterp_schema_field(
          "highlight_groups_map", "string", "Per-plot highlight groups (JSON)",
          default = "{}"
        )
      ),
      outputs = list(
        figures = c("rankplot"),
        tables = character(0),
        interactive = TRUE,
        plotly_allowed = TRUE,
        default_plot_mode = "ggplot",
        click_target = "target"
      ),
      render_spec = list(plots = c("rankplot"), tables = character(0), tabs = NULL)
    ),

    # ----------------------------
    # Enrichment
    # ----------------------------
    goora = list(
      engine_id = "goora",
      label = "GO-ORA",
      category = "enrichment",
      supported_data_types = c("proteomics"),
      description = "GO over-representation analysis.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = TRUE,
        required_ids = c("gene"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "database", "choice", "Database",
          default = "go",
          choices = c("go", "complex"),
          choice_labels = c("Gene Ontology", "Protein Complexes"),
          help = "Select enrichment database: GO terms or protein complexes (CORUM/ComplexPortal)"
        ),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff", default = 0.05, min = 0, max = 1),
        msterp_schema_field("min_term_size", "int", "Min term size", default = 5, min = 1,
                            help = "Minimum proteins in database annotated to a GO term (filters small terms)"),
        msterp_schema_field("min_overlap", "int", "Min overlap", default = 3, min = 1,
                            help = "Minimum query proteins that must overlap with a GO term (filters weak hits)"),
        msterp_schema_field("max_terms", "int", "Terms to show", default = 20, min = 1, max = 200),
        msterp_schema_field(
          "ontology", "choice", "Ontology",
          default = "ALL", choices = c("ALL", "BP", "MF", "CC"),
          help = "Select which ontologies to compute: ALL (BP, MF, CC), or a single ontology.",
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "plot_type", "choice", "Plot type",
            default = "bar", choices = c("bar", "dot"), advanced = TRUE
          ),
          msterp_schema_field(
            "color_mode", "choice", "Coloring",
            default = "fdr", choices = c("fdr", "flat"), advanced = TRUE
          ),
          msterp_schema_field(
            "fdr_palette", "choice", "FDR color palette",
            default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
            choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
          ),
          msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("alpha", "num", "Opacity", default = 0.8, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("show_go_id", "bool", "Show term ID in labels", default = FALSE,
                              help = "Display term ID beside name (e.g., 'Term Name (GO:0000000)' or 'Complex Name (CORUM:123)')",
                              advanced = TRUE),
          msterp_schema_field("font_size", "int", "Font size", default = 14, min = 6, max = 30, advanced = TRUE),
          msterp_schema_field(
            "x_axis_metric", "choice", "X-axis metric",
            default = "fold_enrichment",
            choices = c("fold_enrichment", "neglog10_fdr"),
            choice_labels = c("Fold Enrichment", "-log10(FDR)"),
            advanced = TRUE
          )
        ),
        mk_style(width = 12, height = 6, axis_text_size = 20)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Ontology filter (BP/MF/CC for GO, CORUM/ComplexPortal for complex)
        msterp_schema_field("ontology_filter", "choice", "Ontology",
                            default = "BP", choices = c("BP", "MF", "CC")),
        msterp_schema_field(
          "flip_axis", "bool", "Flip horizontal axis",
          default = FALSE
        )
      ),
      outputs = list(
        figures = c("goora_plot"),
        tables = c("goora_table"),
        interactive = TRUE,
        default_plot_mode = "ggplot",
        click_target = "term"
      ),
      render_spec = list(
        plots = c("goora_plot", "bp_plot", "mf_plot", "cc_plot"),
        tables = c("goora_table", "bp_table", "mf_table", "cc_table"),
        tabs = c("BP", "MF", "CC")
      )
    ),

    `1dgofcs` = list(
      engine_id = "1dgofcs",
      label = "1D GO-FCS",
      category = "enrichment",
      supported_data_types = c("proteomics"),
      description = "1D functional class scoring on a ranked gene list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = TRUE,
        required_ids = c("gene"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "database", "choice", "Database",
          default = "go",
          choices = c("go", "complex"),
          choice_labels = c("Gene Ontology", "Protein Complexes"),
          help = "Select enrichment database: GO terms or protein complexes (CORUM/ComplexPortal)"
        ),
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons against control (e.g., log2(KO1/Control), log2(KO2/Control)). If no control group is defined, this is ignored."
        ),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff", default = 0.03, min = 0, max = 1),
        msterp_schema_field("min_term_size", "int", "Min term size", default = 5, min = 1),
        msterp_schema_field("min_overlap", "int", "Min overlap", default = 5, min = 1,
                            help = "Minimum query proteins that must overlap with a GO term (filters weak hits)"),
        msterp_schema_field("max_terms", "int", "Terms to show", default = 20, min = 1, max = 200),
        msterp_schema_field(
          "ontology", "choice", "Ontology",
          default = "ALL", choices = c("ALL", "BP", "MF", "CC"),
          help = "Select which ontologies to compute: ALL (BP, MF, CC), or a single ontology.",
          hidden = TRUE
        ),
        msterp_schema_field(
          "is_log_transformed", "bool", "Data is already log-transformed",
          default = FALSE,
          help = "Enable if your data is already log2-transformed. Skips log2(x+1) transform for fold change ranking.",
          advanced = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "plot_type", "choice", "Plot type",
            default = "bar", choices = c("bar", "dot"), advanced = TRUE
          ),
          msterp_schema_field(
            "color_mode", "choice", "Coloring",
            default = "fdr", choices = c("fdr", "flat"), advanced = TRUE
          ),
          msterp_schema_field(
            "fdr_palette", "choice", "FDR color palette",
            default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
            choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
          ),
          msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("alpha", "num", "Opacity", default = 0.8, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("show_go_id", "bool", "Show GO ID in labels", default = FALSE,
                              help = "Display GO ID beside term name (e.g., 'Term Name (GO:0000000)')",
                              advanced = TRUE),
          msterp_schema_field("font_size", "int", "Font size", default = 14, min = 6, max = 30, advanced = TRUE)
        ),
        mk_style(width = 14, height = 6, axis_text_size = 20)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Ontology filter (BP/MF/CC only - no ALL option)
        msterp_schema_field("ontology_filter", "choice", "Ontology",
                            default = "BP", choices = c("BP", "MF", "CC")),
        msterp_schema_field(
          "flip_axis", "bool", "Flip horizontal axis",
          default = FALSE
        ),
        # Flip FC direction: negates score values per-plot (swap enrichment direction)
        msterp_schema_field(
          "flip_fc", "bool", "Flip score direction",
          default = FALSE,
          help = "Negate score values to swap enrichment direction (GroupA/GroupB becomes GroupB/GroupA)"
        )
      ),
      outputs = list(
        figures = c("1dgofcs_plot"),
        tables = c("1dgofcs_table"),
        interactive = TRUE,
        default_plot_mode = "ggplot",
        click_target = "term"
      ),
      render_spec = list(
        plots = c("1dgofcs_plot", "bp_plot", "mf_plot", "cc_plot"),
        tables = c("1dgofcs_table", "bp_table", "mf_table", "cc_table"),
        tabs = c("BP", "MF", "CC")
      )
    ),

    `2dgofcs` = list(
      engine_id = "2dgofcs",
      label = "2D GO-FCS",
      category = "enrichment",
      supported_data_types = c("proteomics", "multi"),
      description = "2D functional class scoring on two ranked feature lists.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = TRUE,
        required_ids = c("gene"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "database", "choice", "Database",
          default = "go",
          choices = c("go", "complex"),
          choice_labels = c("Gene Ontology", "Protein Complexes"),
          help = "Select enrichment database: GO terms or protein complexes (CORUM/ComplexPortal)"
        ),
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons involving control (e.g., log2(KO1/Control) vs log2(KO2/Control)). If no control group is defined, this is ignored."
        ),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff", default = 0.03, min = 0, max = 1),
        msterp_schema_field("min_term_size", "int", "Min term size", default = 5, min = 1),
        msterp_schema_field("min_overlap", "int", "Min overlap", default = 5, min = 1,
                            help = "Minimum query proteins that must overlap with a GO term (filters weak hits)"),
        msterp_schema_field("max_terms", "int", "Terms to show", default = 20, min = 1, max = 200),
        msterp_schema_field(
          "ontology", "choice", "Ontology",
          default = "ALL", choices = c("ALL", "BP", "MF", "CC"),
          help = "Select which ontologies to compute: ALL (BP, MF, CC), or a single ontology.",
          hidden = TRUE
        ),
        msterp_schema_field(
          "is_log_transformed", "bool", "Data is already log-transformed",
          default = FALSE,
          help = "Enable if your data is already log2-transformed. Skips log2(x+1) transform for fold change ranking.",
          advanced = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "color_mode", "choice", "Coloring",
            default = "fdr", choices = c("fdr", "flat"), advanced = TRUE
          ),
          msterp_schema_field(
            "fdr_palette", "choice", "FDR color palette",
            default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
            choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
          ),
          msterp_schema_field("flat_color", "string", "Flat color (hex)",
                              default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("dot_alpha", "num", "Dot opacity",
                              default = 1, min = 0, max = 1, advanced = TRUE),
          # Reference lines (x=0, y=0) with independent controls for thickness, style, visibility
          msterp_schema_field("show_ref_lines", "bool", "Show x=0 and y=0 lines",
                              default = TRUE, advanced = TRUE),
          msterp_schema_field("ref_line_size", "num", "Reference line thickness",
                              default = 0.5, min = 0.2, max = 5, advanced = TRUE),
          msterp_schema_field("ref_line_dotted", "bool", "Dotted reference lines",
                              default = TRUE, advanced = TRUE),
          # Diagonal guideline controls (y=x and y=-x)
          msterp_schema_field("show_diagonal_guides", "bool", "Show y=x and y=-x guidelines",
                              default = TRUE, advanced = TRUE),
          msterp_schema_field("diagonal_guide_size", "num", "Diagonal guide thickness",
                              default = 1, min = 0.2, max = 5, advanced = TRUE),
          msterp_schema_field("label_font_size", "int", "Label font size",
                              default = 12, min = 6, max = 30, advanced = TRUE),
          msterp_schema_field("show_go_id", "bool", "Show GO ID in labels", default = FALSE,
                              help = "Display GO ID beside term name (e.g., 'Term Name (GO:0000000)')",
                              advanced = TRUE)
        ),
        mk_style(width = 8, height = 6, axis_text_size = 20),
        list(
          # Axis range controls - hidden (internal use only)
          msterp_schema_field("x_min", "num", "X min", default = -1, min = -100, max = 100, hidden = TRUE),
          msterp_schema_field("x_max", "num", "X max", default = 1,  min = -100, max = 100, hidden = TRUE),
          msterp_schema_field("y_min", "num", "Y min", default = -1, min = -100, max = 100, hidden = TRUE),
          msterp_schema_field("y_max", "num", "Y max", default = 1,  min = -100, max = 100, hidden = TRUE)
        )
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Ontology filter (BP/MF/CC only - no ALL option)
        msterp_schema_field(
          "view_mode", "choice", "Viewer mode",
          default = "export_preview", choices = c("export_preview", "interactive"),
          choice_labels = c("Export preview", "Interactive")
        ),
        msterp_schema_field("ontology_filter", "choice", "Ontology",
                            default = "BP", choices = c("BP", "MF", "CC"))
      ),
      outputs = list(
        figures = c("2dgofcs_plot"),
        tables = c("2dgofcs_table"),
        interactive = TRUE,
        plotly_allowed = TRUE,
        default_plot_mode = "ggplot",
        click_target = "term"
      ),
      render_spec = list(
        plots = c("2dgofcs_plot", "bp_plot", "mf_plot", "cc_plot"),
        tables = c("2dgofcs_table", "bp_table", "mf_table", "cc_table"),
        tabs = c("BP", "MF", "CC")
      )
    ),

    subloc = list(
      engine_id = "subloc",
      label = "Subcellular Localization",
      category = "enrichment",
      supported_data_types = c("proteomics"),
      description = "Subcellular localization group-wise distributions from terpbase.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = TRUE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none")),
        msterp_schema_field("mean_type", "choice", "Mean type", default = "arithmetic",
                            choices = c("arithmetic", "harmonic")),
        msterp_schema_field("orientation", "choice", "Orientation", default = "vertical",
                            choices = c("vertical", "horizontal", "sideways"),
                            choice_labels = c("Vertical", "(horizontal)", "Sideways (labels as strips)"),
                            hidden = TRUE)
      ),
      style_schema = list(
        # FIX: Global mean is now a style option (viewer-time) not compute-time param
        msterp_schema_field("y_axis_title", "string", "Y-axis title", default = "Intensity"),
        msterp_schema_field("label_rotation", "choice", "Label rotation",
                            default = "0", choices = c("0", "45", "90"),
                            choice_labels = c("Horizontal", "45 degrees", "Vertical"),
                            advanced = TRUE),
        msterp_schema_field("show_global_mean", "bool", "Show global mean line", default = FALSE,
                            help = "Display the global mean across all groups as a reference line",
                            advanced = TRUE),
        msterp_schema_field("global_mean_color", "string", "Global mean color (hex)", default = "#FF0000", advanced = TRUE),
        msterp_schema_field("global_mean_size", "num", "Global mean line thickness", default = 1, min = 0.5, max = 5, advanced = TRUE),
        msterp_schema_field("color_mode", "choice", "Color", default = "group",
                            choices = c("group", "flat"), advanced = TRUE),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#B0B0B0", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Box opacity",
                            default = 0.3, min = 0, max = 1, advanced = TRUE),
        # Y-axis range controls
        msterp_schema_field("y_range_mode", "choice", "Y-axis range", default = "auto",
                            choices = c("auto", "manual"), advanced = TRUE),
        msterp_schema_field("y_min", "num", "Y-axis min", default = 0, advanced = TRUE),
        msterp_schema_field("y_max", "num", "Y-axis max", default = 12, advanced = TRUE),
        msterp_schema_field("axis_style", "choice", "Axis style", default = "clean",
                            choices = c("clean", "bold"), advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 20, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width",  "num", "Plot width (in)",
                            default = 7, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 6, min = 2, max = 24, advanced = TRUE)
      ),
      outputs = list(figures = c("subloc_plot"), tables = c("subloc_counts"),
                     interactive = FALSE),
      render_spec = list(plots = c("subloc_plot"), tables = c("subloc_counts"), tabs = NULL)
    ),

    # ----------------------------
    # Network
    # ----------------------------
    ppi_network = list(
      engine_id = "ppi_network",
      label = "PPI Network",
      category = "network",
      supported_data_types = c("proteomics"),
      description = "Protein-protein interaction network from STRING-DB with clustering and centrality analysis.",
      picker_hidden = TRUE,
      supports_sequential = TRUE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("protein_source", "choice", "Protein source",
          default = "de_proteins",
          choices = c("de_proteins", "go_term", "custom"),
          choice_labels = c("DE Proteins", "GO Term Members", "Custom List"),
          help = "Source of proteins to include in network"
        ),
        msterp_schema_field("score_threshold", "int", "STRING score threshold",
          default = 400, min = 0, max = 1000,
          help = "Minimum confidence (0=low, 400=medium, 700=high, 900=highest)"
        ),
        msterp_schema_field("species", "choice", "Species",
          default = "9606",
          choices = c("9606", "10090", "10116"),
          choice_labels = c("Human", "Mouse", "Rat")
        ),
        msterp_schema_field("network_type", "choice", "Network type",
          default = "functional",
          choices = c("functional", "physical"),
          choice_labels = c("Functional", "Physical")
        ),
        msterp_schema_field("clustering_method", "choice", "Clustering algorithm",
          default = "louvain",
          choices = c("louvain", "walktrap", "leiden", "fast_greedy"),
          choice_labels = c("Louvain", "Walktrap", "Leiden", "Fast Greedy"),
          help = "Algorithm for grouping related proteins"
        ),
        msterp_schema_field("max_proteins", "int", "Max proteins",
          default = 500, min = 10, max = 2000,
          help = "Maximum proteins to include (for performance)",
          advanced = TRUE
        )
      ),
      style_schema = list(
        msterp_schema_field("color_by", "choice", "Color nodes by",
          default = "cluster",
          choices = c("cluster", "log2fc", "pval", "degree"),
          choice_labels = c("Cluster", "Fold Change", "P-value", "Connectivity")
        ),
        msterp_schema_field("size_by", "choice", "Size nodes by",
          default = "degree",
          choices = c("degree", "pval", "log2fc", "fixed"),
          choice_labels = c("Connectivity", "P-value", "Fold Change", "Fixed")
        ),
        msterp_schema_field("show_confidence", "bool", "Color edges by confidence",
          default = TRUE,
          help = "Darker edges indicate higher STRING confidence scores"
        ),
        msterp_schema_field("show_labels", "bool", "Show node labels",
          default = TRUE,
          help = "Display protein/gene name labels on network nodes"
        ),
        msterp_schema_field("label_font_size", "int", "Label font size",
          default = 24, min = 8, max = 60,
          help = "Font size for protein/gene name labels on nodes"
        ),
        msterp_schema_field("layout", "choice", "Layout algorithm",
          default = "fr",
          choices = c("fr", "kk", "circle", "grid"),
          choice_labels = c("Force-directed (FR)", "Kamada-Kawai", "Circle", "Grid"),
          advanced = TRUE
        ),
        msterp_schema_field("width", "num", "Width", default = 10, min = 4, max = 20, advanced = TRUE),
        msterp_schema_field("height", "num", "Height", default = 8, min = 4, max = 20, advanced = TRUE)
      ),
      viewer_schema = list(
        msterp_schema_field("selected_cluster", "choice", "Filter by cluster",
          default = "",
          choices = c(""),
          help = "Show only proteins in selected cluster (populated from results)"
        )
      ),
      outputs = list(
        figures = c("ppi_network"),
        tables = c("ppi_stats", "ppi_hubs", "ppi_nodes"),
        interactive = TRUE
      ),
      render_spec = list(
        plots = c("ppi_network"),
        tables = c("ppi_stats", "ppi_hubs", "ppi_nodes"),
        tabs = NULL
      )
    ),

    # ----------------------------
    # Multi-Dataset Comparison
    # ----------------------------
    multi_scatter = list(
      engine_id = "multi_scatter",
      label = "Multi-Dataset Scatter",
      category = "2d",
      supported_data_types = c("multi"),
      description = "Cross-dataset scatter plot comparing values from two linked datasets.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein"),
        requires_multi_dataset = TRUE
      ),
      params_schema = list(
        msterp_schema_field(
          "center_mode", "choice", "Center mode",
          default = "zero",
          choices = c("zero", "median"),
          choice_labels = c("Origin (0,0)", "Median"),
          help = "How to define quadrant boundaries"
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field("point_size", "num", "Point size", default = 2, min = 0.5, max = 8),
          msterp_schema_field("point_alpha", "num", "Point opacity", default = 0.7, min = 0.1, max = 1),
          msterp_schema_field("color_mode", "choice", "Color by",
            default = "quadrant",
            choices = c("quadrant", "flat"),
            choice_labels = c("Quadrant", "Single color")
          ),
          msterp_schema_field("point_color", "string", "Point color", default = "#1f77b4", advanced = TRUE),
          msterp_schema_field("show_correlation", "bool", "Show correlation", default = TRUE),
          msterp_schema_field("show_regression_line", "bool", "Show regression line", default = FALSE),
          msterp_schema_field("show_labels", "bool", "Label points", default = FALSE),
          msterp_schema_field("label_top_n", "int", "Label top N outliers", default = 10, min = 0, max = 50, advanced = TRUE)
        ),
        mk_style(width = 8, height = 8, axis_text_size = 16)
      ),
      outputs = list(
        figures = c("multi_scatter"),
        tables = c("multi_scatter_data"),
        interactive = TRUE
      ),
      render_spec = list(
        plots = c("multi_scatter"),
        tables = c("multi_scatter_data"),
        tabs = NULL
      )
    ),

    multi_correlation = list(
      engine_id = "multi_correlation",
      label = "Multi-Dataset Correlation",
      category = "2d",
      supported_data_types = c("multi"),
      description = "Correlation analysis between two linked datasets with Pearson/Spearman and regression.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein"),
        requires_multi_dataset = TRUE
      ),
      params_schema = list(
        msterp_schema_field(
          "method", "choice", "Primary method",
          default = "pearson",
          choices = c("pearson", "spearman"),
          choice_labels = c("Pearson (linear)", "Spearman (rank)"),
          help = "Primary correlation method (both are always computed)"
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field("point_size", "num", "Point size", default = 2, min = 0.5, max = 8),
          msterp_schema_field("point_alpha", "num", "Point opacity", default = 0.6, min = 0.1, max = 1),
          msterp_schema_field("point_color", "string", "Point color", default = "#2c3e50"),
          msterp_schema_field("show_regression_line", "bool", "Show regression line", default = TRUE),
          msterp_schema_field("regression_line_color", "string", "Regression line color", default = "#e74c3c", advanced = TRUE),
          msterp_schema_field("show_ci_band", "bool", "Show 95% CI band", default = TRUE),
          msterp_schema_field("ci_band_alpha", "num", "CI band opacity", default = 0.2, min = 0.05, max = 0.5, advanced = TRUE),
          msterp_schema_field("highlight_outliers", "bool", "Highlight outliers", default = TRUE),
          msterp_schema_field("outlier_color", "string", "Outlier color", default = "#e74c3c", advanced = TRUE),
          msterp_schema_field("show_stats_annotation", "bool", "Show stats annotation", default = TRUE)
        ),
        mk_style(width = 8, height = 8, axis_text_size = 16)
      ),
      outputs = list(
        figures = c("multi_correlation"),
        tables = c("multi_correlation_data", "multi_correlation_stats"),
        interactive = TRUE
      ),
      render_spec = list(
        plots = c("multi_correlation"),
        tables = c("multi_correlation_data", "multi_correlation_stats"),
        tabs = NULL
      )
    ),

    # Cross-Entity Correlation (protein-metabolomics)
    multi_cross_correlation = list(
      engine_id = "multi_cross_correlation",
      label = "Cross-Entity Correlation",
      category = "2d",
      supported_data_types = c("multi"),
      description = "Per-entity correlation between proteins and metabolites across paired samples.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "metabolite"),
        requires_multi_dataset = TRUE,
        requires_mixed_entities = TRUE
      ),
      params_schema = list(
        msterp_schema_field(
          "method", "choice", "Correlation method",
          default = "spearman",
          choices = c("spearman", "pearson"),
          choice_labels = c("Spearman (rank)", "Pearson (linear)"),
          help = "Correlation method for cross-entity comparisons"
        ),
        msterp_schema_field(
          "p_cutoff", "num", "P-value cutoff",
          default = 0.05, min = 0, max = 1,
          help = "P-value threshold for significant correlations"
        ),
        msterp_schema_field(
          "top_n", "int", "Top N per entity",
          default = 10, min = 1, max = 100,
          help = "Number of top correlations to report per entity"
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field("show_labels", "bool", "Show labels", default = TRUE),
          msterp_schema_field("color_mode", "choice", "Color by",
            default = "correlation",
            choices = c("correlation", "entity_type"),
            choice_labels = c("Correlation strength", "Entity type")
          )
        ),
        mk_style(width = 10, height = 8, axis_text_size = 14)
      ),
      outputs = list(
        figures = c("cross_correlation_heatmap"),
        tables = c("cross_correlations", "per_entity_a", "per_entity_b", "statistics"),
        interactive = TRUE
      ),
      render_spec = list(
        plots = c("cross_correlation_heatmap"),
        tables = c("cross_correlations", "per_entity_a", "per_entity_b"),
        tabs = NULL
      )
    ),

    # ----------------------------
    # Metabolite Enrichment Engines
    # ----------------------------
    msea = list(
      engine_id = "msea",
      label = "Pathway Enrichment (MSEA)",
      category = "enrichment",
      supported_data_types = c("metabolomics"),
      description = "Metabolite Set Enrichment Analysis - pathway over-representation using KEGG/Reactome/SMPDB.",
      supports_sequential = FALSE,
      accepted_input_levels = c("metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        requires_metabobase = TRUE,
        required_ids = c("metabolite"),
        analysis_levels = c("metabolite")
      ),
      params_schema = list(
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff",
                            default = 0.05, min = 0, max = 1),
        msterp_schema_field("min_pathway_size", "int", "Min pathway size",
                            default = 3, min = 1, max = 100),
        msterp_schema_field("min_overlap", "int", "Min overlap",
                            default = 2, min = 1, max = 50,
                            help = "Minimum number of query metabolites in pathway"),
        msterp_schema_field("max_terms", "int", "Max terms per database",
                            default = 20, min = 1, max = 200)
        # PubChem resolver is now always-on (no opt-in needed)
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type",
                            default = "bar",
                            choices = c("bar", "dot"),
                            choice_labels = c("Bar chart", "Dot plot")),
        msterp_schema_field("color_mode", "choice", "Color by",
                            default = "fdr",
                            choices = c("fdr", "flat"),
                            choice_labels = c("FDR value", "Flat color")),
        msterp_schema_field(
          "fdr_palette", "choice", "FDR color palette",
          default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
          choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
        ),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#4A90D9", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity",
                            default = 0.8, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("show_pathway_id", "bool", "Show pathway ID in labels",
                            default = FALSE, advanced = TRUE),
        msterp_schema_field("font_size", "int", "Font size",
                            default = 14, min = 6, max = 30, advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 20, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)",
                            default = 12, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 6, min = 2, max = 24, advanced = TRUE)
      ),
      viewer_schema = list(
        msterp_schema_field("pathway_db_filter", "choice", "Pathway database",
                            default = "all",
                            choices = c("all", "KEGG", "Reactome", "SMPDB"),
                            choice_labels = c("All databases", "KEGG only", "Reactome only", "SMPDB only")),
        msterp_schema_field("flip_axis", "bool", "Flip horizontal axis",
                            default = FALSE)
      ),
      outputs = list(figures = c("msea_plot"), tables = c("msea_results"), interactive = TRUE),
      render_spec = list(plots = c("all_plot"), tables = c("all_table"), tabs = NULL)
    ),

    # class_enrichment disabled — no guaranteed database (KEGG API produces empty class_mappings)
    # class_enrichment = list(
    #   engine_id = "class_enrichment",
    #   label = "Chemical Class Enrichment",
    #   category = "enrichment",
    #   supported_data_types = c("metabolomics"),
    #   description = "Enrichment analysis of chemical classes (lipids, amino acids, etc.).",
    #   supports_sequential = FALSE,
    #   accepted_input_levels = c("metabolite"),
    #   requirements = list(
    #     min_groups = 1,
    #     requires_terpbase = FALSE,
    #     requires_metabobase = TRUE,
    #     required_ids = c("metabolite"),
    #     analysis_levels = c("metabolite")
    #   ),
    #   params_schema = list(
    #     msterp_schema_field("class_level", "choice", "Classification level",
    #                         default = "class",
    #                         choices = c("superclass", "class", "subclass"),
    #                         choice_labels = c("Superclass", "Class", "Subclass")),
    #     msterp_schema_field("fdr_cutoff", "num", "FDR cutoff",
    #                         default = 0.05, min = 0, max = 1),
    #     msterp_schema_field("min_class_size", "int", "Min class size",
    #                         default = 3, min = 1, max = 100),
    #     msterp_schema_field("max_terms", "int", "Max terms to show",
    #                         default = 20, min = 1, max = 200)
    #   ),
    #   style_schema = list(
    #     msterp_schema_field("plot_type", "choice", "Plot type",
    #                         default = "bar",
    #                         choices = c("bar", "dot"),
    #                         choice_labels = c("Bar chart", "Dot plot")),
    #     msterp_schema_field("color_mode", "choice", "Color by",
    #                         default = "fdr",
    #                         choices = c("fdr", "flat"),
    #                         choice_labels = c("FDR value", "Flat color")),
    #     msterp_schema_field("flat_color", "string", "Flat color (hex)",
    #                         default = "#7B68EE", advanced = TRUE),
    #     msterp_schema_field("alpha", "num", "Opacity",
    #                         default = 0.8, min = 0, max = 1, advanced = TRUE),
    #     msterp_schema_field("font_size", "int", "Font size",
    #                         default = 14, min = 6, max = 30, advanced = TRUE),
    #     msterp_schema_field("axis_text_size", "int", "Axis text size",
    #                         default = 20, min = 6, max = 40, advanced = TRUE),
    #     msterp_schema_field("width", "num", "Plot width (in)",
    #                         default = 12, min = 2, max = 24, advanced = TRUE),
    #     msterp_schema_field("height", "num", "Plot height (in)",
    #                         default = 6, min = 2, max = 24, advanced = TRUE)
    #   ),
    #   viewer_schema = list(
    #     msterp_schema_field("flip_axis", "bool", "Flip horizontal axis",
    #                         default = FALSE)
    #   ),
    #   outputs = list(figures = c("class_enrichment_plot"), tables = c("class_enrichment_results"), interactive = TRUE),
    #   render_spec = list(plots = c("class_plot"), tables = c("class_table"), tabs = NULL)
    # ),

    pathway_fcs = list(
      engine_id = "pathway_fcs",
      label = "1D Pathway FCS",
      category = "enrichment",
      supported_data_types = c("metabolomics"),
      description = "1D Pathway Functional Class Scoring on a ranked metabolite list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        requires_metabobase = TRUE,
        required_ids = c("metabolite"),
        analysis_levels = c("metabolite")
      ),
      params_schema = list(
        msterp_schema_field("control_only", "bool", "Only compare against control",
                            default = FALSE),
        msterp_schema_field("is_log_transformed", "bool", "Data is already log-transformed",
                            default = FALSE, advanced = TRUE),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff",
                            default = 0.03, min = 0, max = 1),
        msterp_schema_field("min_pathway_size", "int", "Min pathway size",
                            default = 5, min = 1, max = 100),
        msterp_schema_field("min_overlap", "int", "Min overlap",
                            default = 3, min = 1, max = 50),
        msterp_schema_field("max_terms", "int", "Max terms per database",
                            default = 20, min = 1, max = 200)
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type",
                            default = "bar",
                            choices = c("bar", "dot"),
                            choice_labels = c("Bar chart", "Dot plot")),
        msterp_schema_field("color_mode", "choice", "Color by",
                            default = "score",
                            choices = c("score", "fdr", "flat"),
                            choice_labels = c("Enrichment score", "FDR value", "Flat color")),
        msterp_schema_field(
          "fdr_palette", "choice", "FDR color palette",
          default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
          choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
        ),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#20B2AA", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity",
                            default = 0.8, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("show_pathway_id", "bool", "Show pathway ID in labels",
                            default = FALSE, advanced = TRUE),
        msterp_schema_field("font_size", "int", "Font size",
                            default = 14, min = 6, max = 30, advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 20, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)",
                            default = 14, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 6, min = 2, max = 24, advanced = TRUE)
      ),
      viewer_schema = list(
        msterp_schema_field("pathway_db_filter", "choice", "Pathway database",
                            default = "all",
                            choices = c("all", "KEGG", "Reactome", "SMPDB"),
                            choice_labels = c("All databases", "KEGG only", "Reactome only", "SMPDB only")),
        msterp_schema_field("flip_axis", "bool", "Flip horizontal axis",
                            default = FALSE),
        msterp_schema_field(
          "flip_fc", "bool", "Flip score direction",
          default = FALSE,
          help = "Negate score values to swap enrichment direction (GroupA/GroupB becomes GroupB/GroupA)"
        )
      ),
      outputs = list(figures = c("pathway_fcs_plot"), tables = c("pathway_fcs_results"), interactive = TRUE),
      render_spec = list(plots = c("all_plot"), tables = c("all_table"), tabs = NULL)
    )

  )

  for (eid in names(engines)) {
    if (is.null(engines[[eid]]$type) || !nzchar(as.character(engines[[eid]]$type))) {
      engines[[eid]]$type <- "engine"
    }
    engines[[eid]]$picker_hidden <- isTRUE(engines[[eid]]$picker_hidden %||% FALSE)
    engines[[eid]]$locked_parent <- isTRUE(engines[[eid]]$locked_parent %||% FALSE)
    if (is.null(engines[[eid]]$viewer_schema)) engines[[eid]]$viewer_schema <- list()
  }

  .registry_cache <<- list(registry_version = registry_version, engines = engines)
  .registry_cache
}

msterp_engine_picker_hidden <- function(engine) {
  isTRUE(engine$picker_hidden %||% FALSE)
}

msterp_engine_locked_parent <- function(engine) {
  isTRUE(engine$locked_parent %||% FALSE)
}

msterp_engine_requirements <- function(engine) {
  req <- engine$requirements %||% list()
  req$min_groups <- req$min_groups %||% engine$min_groups %||% 1
  req$requires_terpbase <- req$requires_terpbase %||% engine$requires_terpbase %||% FALSE
  req$required_ids <- req$required_ids %||% engine$required_ids %||% character()
  req$analysis_levels <- req$analysis_levels %||% engine$analysis_levels %||% character()
  req
}

msterp_engine_outputs <- function(engine) {
  out <- engine$outputs %||% list()
  if (is.character(out)) out <- list()
  out$figures <- out$figures %||% character()
  out$tables <- out$tables %||% character()
  out$interactive <- isTRUE(out$interactive %||% FALSE)
  out
}

msterp_engine_get <- function(engine_id, registry = msterp_engine_registry()) {
  registry$engines[[engine_id]]
}

get_engine_def <- function(engine_id, registry = msterp_engine_registry()) {
  if (is.null(engine_id)) return(NULL)
  engine_id <- as.character(engine_id)
  if (!length(engine_id) || !nzchar(engine_id[[1]])) return(NULL)

  eid <- migrate_legacy_engine_name(engine_id[[1]])
  msterp_engine_get(tolower(eid), registry = registry)
}

msterp_engine_is_picker_hidden <- function(engine_id, registry = msterp_engine_registry()) {
  msterp_engine_picker_hidden(msterp_engine_get(engine_id, registry = registry) %||% list())
}

msterp_engine_is_locked_parent <- function(engine_id, registry = msterp_engine_registry()) {
  msterp_engine_locked_parent(msterp_engine_get(engine_id, registry = registry) %||% list())
}

msterp_engine_ids <- function(registry = msterp_engine_registry()) {
  names(registry$engines)
}

#' Get engine IDs filtered by data type
#' @param data_type Optional data type filter: "proteomics", "metabolomics", or "multi"
#' @param registry Engine registry
#' @return Character vector of engine IDs
msterp_engine_ids_by_data_type <- function(data_type = NULL, registry = msterp_engine_registry()) {
  all_ids <- names(registry$engines)
  if (is.null(data_type)) return(all_ids)

  data_type <- tolower(trimws(data_type))
  Filter(function(eid) {
    eng <- registry$engines[[eid]]
    supported <- eng$supported_data_types %||% c("proteomics")  # Default for backwards compat
    data_type %in% supported
  }, all_ids)
}

#' Get supported data types for an engine
#' @param engine_id Engine ID
#' @param registry Engine registry
#' @return Character vector of supported data types
msterp_engine_supported_data_types <- function(engine_id, registry = msterp_engine_registry()) {
  eng <- registry$engines[[engine_id]]
  if (is.null(eng)) return(character(0))
  eng$supported_data_types %||% c("proteomics")  # Default for backwards compat
}

# ---- Registry Sanity Check ---------------------------------------------------

tb_registry_render_spec_sanity <- function(registry = msterp_engine_registry()) {
  if (is.null(registry) || is.null(registry$engines)) {
    warning("Registry is NULL or has no engines")
    return(invisible(FALSE))
  }

  issues <- character()

  for (eng_id in names(registry$engines)) {
    eng <- registry$engines[[eng_id]]
    render_spec <- eng$render_spec

    if (is.null(render_spec)) {
      issues <- c(issues, sprintf("Engine '%s': missing render_spec", eng_id))
      next
    }

    # Check that plots is a character vector
    plots <- render_spec$plots
    if (!is.null(plots) && !is.character(plots)) {
      issues <- c(issues, sprintf(
        "Engine '%s': render_spec$plots must be character vector, got %s",
        eng_id, class(plots)[[1]]
      ))
    }

    # Check for duplicate plot names
    if (is.character(plots) && length(plots) > 0) {
      if (any(duplicated(plots))) {
        issues <- c(issues, sprintf(
          "Engine '%s': render_spec$plots has duplicates: %s",
          eng_id, paste(plots[duplicated(plots)], collapse = ", ")
        ))
      }
    }

    # Check that tables is a character vector
    tables <- render_spec$tables
    if (!is.null(tables) && !is.character(tables)) {
      issues <- c(issues, sprintf(
        "Engine '%s': render_spec$tables must be character vector, got %s",
        eng_id, class(tables)[[1]]
      ))
    }

    # Check for duplicate table names
    if (is.character(tables) && length(tables) > 0) {
      if (any(duplicated(tables))) {
        issues <- c(issues, sprintf(
          "Engine '%s': render_spec$tables has duplicates: %s",
          eng_id, paste(tables[duplicated(tables)], collapse = ", ")
        ))
      }
    }

    # Check tabs
    tabs <- render_spec$tabs
    if (!is.null(tabs) && !is.character(tabs)) {
      issues <- c(issues, sprintf(
        "Engine '%s': render_spec$tabs must be NULL or character vector, got %s",
        eng_id, class(tabs)[[1]]
      ))
    }
  }

  if (length(issues) > 0) {
    for (issue in issues) {
      warning(issue)
    }
    return(invisible(FALSE))
  }

  message(sprintf(
    "Registry render_spec sanity check passed for %d engines",
    length(registry$engines)
  ))
  invisible(TRUE)
}

# ---- Registry Metadata Validation --------------------------------------------
# Optional validator for engine metadata shape and enums.
# Not called automatically; intended for development/testing use.

msterp_validate_registry <- function(registry = msterp_engine_registry()) {

  # Canonical allowed values for input levels (lowercase only)
  valid_input_levels <- c("protein", "peptide", "metabolite")

  # Valid field types for schema fields
  valid_field_types <- c("choice", "bool", "int", "num", "string", "range")

  if (is.null(registry) || is.null(registry$engines)) {
    stop("Registry is NULL or has no engines")
  }

  issues <- character()
  warnings <- character()

  for (eng_id in names(registry$engines)) {
    eng <- registry$engines[[eng_id]]

    # -------------------------------------------------------
    # 1. Check accepted_input_levels exists and is valid
    # -------------------------------------------------------
    ail <- eng$accepted_input_levels
    if (is.null(ail)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing accepted_input_levels", eng_id
      ))
    } else if (!is.character(ail) || length(ail) == 0) {
      issues <- c(issues, sprintf(
        "Engine '%s': accepted_input_levels must be non-empty character vector", eng_id
      ))
    } else {
      invalid <- setdiff(ail, valid_input_levels)
      if (length(invalid) > 0) {
        issues <- c(issues, sprintf(
          "Engine '%s': accepted_input_levels contains invalid values: %s",
          eng_id, paste(invalid, collapse = ", ")
        ))
      }
    }

    # -------------------------------------------------------
    # 2. Check analysis_levels in requirements (existing field)
    # -------------------------------------------------------
    req <- eng$requirements
    if (!is.null(req$analysis_levels)) {
      al <- req$analysis_levels
      if (!is.character(al)) {
        issues <- c(issues, sprintf(
          "Engine '%s': requirements$analysis_levels must be character vector", eng_id
        ))
      }
    }

    # -------------------------------------------------------
    # 3. Validate params_schema exists (can be empty list)
    # -------------------------------------------------------
    if (is.null(eng$params_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing params_schema (use list() if empty)", eng_id
      ))
    } else if (!is.list(eng$params_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': params_schema must be a list", eng_id
      ))
    } else {
      # Validate each field in params_schema
      for (i in seq_along(eng$params_schema)) {
        field <- eng$params_schema[[i]]
        if (!is.list(field)) next
        fname <- field$name %||% sprintf("field[%d]", i)

        # Check field type is valid
        if (!is.null(field$type) && !field$type %in% valid_field_types) {
          issues <- c(issues, sprintf(
            "Engine '%s': params_schema$%s has invalid type '%s'",
            eng_id, fname, field$type
          ))
        }

        # Check default exists for compute params
        if (is.null(field$default) && !isTRUE(field$hidden)) {
          warnings <- c(warnings, sprintf(
            "Engine '%s': params_schema$%s missing default value",
            eng_id, fname
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 4. Validate style_schema exists (can be empty list)
    # -------------------------------------------------------
    if (is.null(eng$style_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing style_schema (use list() if empty)", eng_id
      ))
    } else if (!is.list(eng$style_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': style_schema must be a list", eng_id
      ))
    } else {
      # Validate each field in style_schema
      for (i in seq_along(eng$style_schema)) {
        field <- eng$style_schema[[i]]
        if (!is.list(field)) next
        fname <- field$name %||% sprintf("field[%d]", i)

        # Check field type is valid
        if (!is.null(field$type) && !field$type %in% valid_field_types) {
          issues <- c(issues, sprintf(
            "Engine '%s': style_schema$%s has invalid type '%s'",
            eng_id, fname, field$type
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 4b. Validate viewer_schema exists (can be empty list)
    # -------------------------------------------------------
    if (is.null(eng$viewer_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing viewer_schema (use list() if empty)", eng_id
      ))
    } else if (!is.list(eng$viewer_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': viewer_schema must be a list", eng_id
      ))
    } else {
      for (i in seq_along(eng$viewer_schema)) {
        field <- eng$viewer_schema[[i]]
        if (!is.list(field)) next
        fname <- field$name %||% sprintf("field[%d]", i)
        if (!is.null(field$type) && !field$type %in% valid_field_types) {
          issues <- c(issues, sprintf(
            "Engine '%s': viewer_schema$%s has invalid type '%s'",
            eng_id, fname, field$type
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 5. Check for known misplacements (compute fields in style)
    # -------------------------------------------------------
    # Known compute-time fields that should NOT be in style_schema
    compute_only_keywords <- c("threshold", "cutoff", "transform", "compare")

    # Known viewer-time mode suffixes (allowed in style_schema)
    allowed_style_modes <- c(
      "range_mode",   # axis range mode (auto/manual)
      "color_mode",   # coloring mode (group/flat/density)
      "label_mode",   # label display mode (color_sig/hide_nonsig)
      "limit_mode",   # axis limit mode (auto/manual)
      "plot_mode"     # plot rendering mode (ggplot/plotly)
    )

    # Fields explicitly allowed in style_schema despite containing compute keywords
    # These are viewer-time transforms applied at render time, not compute time
    allowed_style_fields <- c("log_transform", "transform")

    if (is.list(eng$style_schema)) {
      for (i in seq_along(eng$style_schema)) {
        field <- eng$style_schema[[i]]
        if (!is.list(field)) next
        fname <- tolower(field$name %||% "")

        # Check if field name suggests compute-time behavior
        for (kw in compute_only_keywords) {
          if (grepl(kw, fname, fixed = TRUE)) {
            # Check if it's an allowed style mode or explicitly allowed field
            is_allowed <- any(sapply(allowed_style_modes, function(am) grepl(am, fname, fixed = TRUE))) ||
              fname %in% allowed_style_fields
            if (!is_allowed) {
              warnings <- c(warnings, sprintf(
                "Engine '%s': style_schema field '%s' may be compute-time (contains '%s')",
                eng_id, field$name, kw
              ))
            }
          }
        }
      }
    }

    # -------------------------------------------------------
    # 5b. Check supported_data_types
    # -------------------------------------------------------
    sdt <- eng$supported_data_types
    valid_data_types <- c("proteomics", "metabolomics", "multi")
    if (!is.null(sdt)) {
      if (!is.character(sdt) || length(sdt) == 0) {
        issues <- c(issues, sprintf(
          "Engine '%s': supported_data_types must be non-empty character vector", eng_id
        ))
      } else {
        invalid <- setdiff(sdt, valid_data_types)
        if (length(invalid) > 0) {
          issues <- c(issues, sprintf(
            "Engine '%s': supported_data_types contains invalid values: %s",
            eng_id, paste(invalid, collapse = ", ")
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 6. Check render_spec exists
    # -------------------------------------------------------
    if (is.null(eng$render_spec)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing render_spec", eng_id
      ))
    }
  }

  # Report warnings (non-fatal)
  if (length(warnings) > 0) {
    for (w in warnings) {
      warning(w)
    }
  }

  # Report errors (fatal)
  if (length(issues) > 0) {
    stop(paste(c("Registry validation failed:", issues), collapse = "\n  "))
  }

  message(sprintf(
    "Registry validation passed: %d engines, %d warnings",
    length(registry$engines), length(warnings)
  ))
  invisible(TRUE)
}

#' Migrate legacy engine IDs to current engine IDs
#'
#' This is a narrow compatibility layer for older `.terpflow`/`.terpbook` files
#' that reference historical engine IDs.
#'
#' @param engine_id Character vector of engine IDs (any case)
#' @return Character vector of migrated engine IDs (lowercase)
migrate_legacy_engine_name <- function(engine_id) {
  if (is.null(engine_id)) return(engine_id)
  x <- as.character(engine_id)
  if (length(x) == 0) return(x)

    map <- c(
      # Scatter correlation legacy alias
      "spearman" = "scatter_correlation",
      # GO-FCS legacy IDs
      "gofcs_1d" = "1dgofcs",
      "gofcs1d" = "1dgofcs",
      "gofcs_2d" = "2dgofcs",
      "gofcs2d" = "2dgofcs",
      # Legacy IDQuant CV view
      "idquant_cv" = "idquant_cv_scatter"
    )

  y <- tolower(x)
  hit <- y %in% names(map)
  y[hit] <- unname(map[y[hit]])
  y
}
