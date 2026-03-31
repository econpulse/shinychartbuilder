# Zentrale Chart-Konfigurationsstruktur

`%||%` <- function(x, y) if (is.null(x)) y else x

assert_required_fields <- function(x, required_fields, context = "object") {
  missing <- setdiff(required_fields, names(x))
  if (length(missing) > 0) {
    stop(sprintf(
      "Fehlende Pflichtfelder in %s: %s",
      context,
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

new_chart_config <- function(chart_name = "Neuer Chart", created_by = "system") {
  now <- Sys.time()
  list(
    schema_version = 1L,
    chart_meta = list(
      chart_id = NULL,
      chart_name = chart_name,
      title = "",
      subtitle = "",
      caption = "",
      created_by = created_by,
      created_at = now,
      updated_at = now
    ),
    raw_series = list(),
    transformations = list(),
    plot_series = list(),
    x_axis = list(
      date_breaks = "3 months",
      date_labels = "%Y-%m",
      limits = NULL,
      minor_breaks = NULL
    ),
    y_axis = list(
      min = NULL,
      max = NULL,
      breaks = NULL,
      format = "number", # number | percent
      decimals = 2L
    ),
    facets = list(
      enabled = FALSE,
      facet_var = NULL,
      scales = "fixed",
      ncol = NULL,
      nrow = NULL
    ),
    theme_options = list(
      base_font_size = 12,
      legend_position = "right",
      use_minimal_theme = TRUE
    ),
    storage_meta = list(
      dirty = FALSE,
      revision = 0L,
      last_saved_at = NULL,
      deleted = FALSE
    )
  )
}

normalise_plot_series_config <- function(plot_series_item) {
  required <- c("id", "source_transformation_id", "display_name", "geoms")
  assert_required_fields(plot_series_item, required, context = "plot_series_item")

  if (!is.list(plot_series_item$geoms) || length(plot_series_item$geoms) == 0) {
    stop("plot_series_item$geoms muss eine nicht-leere Liste sein.", call. = FALSE)
  }

  plot_series_item
}
