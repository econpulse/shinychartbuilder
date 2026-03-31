# Strukturierte Validierung fuer UI-Integration

new_validation_result <- function() {
  list(ok = TRUE, errors = list(), warnings = list())
}

add_error <- function(v, code, message, field = NULL, context = NULL) {
  v$ok <- FALSE
  v$errors[[length(v$errors) + 1]] <- list(
    code = code,
    message = message,
    field = field,
    context = context
  )
  v
}

validate_chart_config <- function(cfg) {
  v <- new_validation_result()
  required_top <- c("chart_meta", "raw_series", "transformations", "plot_series", "x_axis", "y_axis", "facets", "theme_options", "storage_meta")
  missing <- setdiff(required_top, names(cfg))

  if (length(missing) > 0) {
    v <- add_error(v, "missing_top_level", paste("Fehlende Top-Level Felder:", paste(missing, collapse = ", ")))
  }

  tv <- validate_transformations(cfg$transformations, cfg$raw_series)
  if (!tv$ok) v$errors <- c(v$errors, tv$errors)

  pv <- validate_plot_series(cfg$plot_series, cfg$transformations, cfg$raw_series)
  if (!pv$ok) v$errors <- c(v$errors, pv$errors)

  av <- validate_axes_config(cfg$x_axis, cfg$y_axis)
  if (!av$ok) v$errors <- c(v$errors, av$errors)

  v$ok <- length(v$errors) == 0
  v
}

validate_transformations <- function(transformations, raw_series) {
  v <- new_validation_result()

  if (length(transformations) == 0) return(v)

  ids <- vapply(transformations, function(x) x$id %||% NA_character_, character(1))
  if (anyNA(ids) || anyDuplicated(ids) > 0) {
    v <- add_error(v, "invalid_transformation_ids", "Transformation IDs fehlen oder sind nicht eindeutig.")
  }

  raw_ids <- vapply(raw_series, function(x) x$id %||% NA_character_, character(1))
  tr_ids <- ids
  known <- c(raw_ids, tr_ids)
  allowed_types <- c("identity", "ratio", "difference", "sma", "ema", "yoy_pct")

  for (tr in transformations) {
    if (!(tr$type %in% allowed_types)) {
      v <- add_error(v, "invalid_transformation_type", sprintf("Ungueltiger Typ: %s", tr$type), context = tr$id)
    }
    for (inp in tr$inputs %||% character(0)) {
      if (!(inp %in% known)) {
        v <- add_error(v, "unknown_input", sprintf("Unbekannter Input: %s", inp), context = tr$id)
      }
    }

    if (tr$type %in% c("sma", "ema")) {
      w <- as.integer(tr$params$window %||% NA_integer_)
      if (is.na(w) || w < 1) {
        v <- add_error(v, "invalid_window", "window muss >= 1 sein", context = tr$id)
      }
    }
  }

  v$ok <- length(v$errors) == 0
  v
}

validate_plot_series <- function(plot_series, transformations, raw_series = list()) {
  v <- new_validation_result()

  tr_ids <- vapply(transformations, function(x) x$id %||% NA_character_, character(1))
  raw_ids <- vapply(raw_series, function(x) x$id %||% NA_character_, character(1))
  known_sources <- c(tr_ids, raw_ids)

  ids <- vapply(plot_series, function(x) x$id %||% NA_character_, character(1))
  if (length(ids) > 0 && (anyNA(ids) || anyDuplicated(ids) > 0)) {
    v <- add_error(v, "invalid_plot_series_ids", "Plot-Serie IDs fehlen oder sind nicht eindeutig.")
  }

  allowed_geoms <- c("line", "point", "col")

  for (ps in plot_series) {
    src <- ps$source_transformation_id %||% NA_character_
    if (is.na(src) || !(src %in% known_sources)) {
      v <- add_error(v, "unknown_plot_source", sprintf("Unbekannte Quelle fuer Plot-Serie: %s", src), context = ps$id)
    }

    geoms <- ps$geoms %||% list()
    if (length(geoms) == 0) {
      v <- add_error(v, "missing_geoms", "Plot-Serie braucht mindestens ein Geom.", context = ps$id)
    }

    for (g in geoms) {
      if (!(g$type %||% "" %in% allowed_geoms)) {
        v <- add_error(v, "invalid_geom", sprintf("Ungueltiger Geom-Typ: %s", g$type %||% ""), context = ps$id)
      }
    }
  }

  v$ok <- length(v$errors) == 0
  v
}

validate_axes_config <- function(x_axis, y_axis) {
  v <- new_validation_result()

  if (!is.null(y_axis$min) && !is.null(y_axis$max) && y_axis$min > y_axis$max) {
    v <- add_error(v, "invalid_y_limits", "y_axis min darf nicht groesser als max sein.")
  }

  if (!is.null(y_axis$decimals) && y_axis$decimals < 0) {
    v <- add_error(v, "invalid_decimals", "y_axis decimals muss >= 0 sein.")
  }

  if (!is.null(x_axis$limits) && length(x_axis$limits) == 2) {
    d1 <- suppressWarnings(as.Date(x_axis$limits[[1]]))
    d2 <- suppressWarnings(as.Date(x_axis$limits[[2]]))
    if (is.na(d1) || is.na(d2) || d1 > d2) {
      v <- add_error(v, "invalid_x_limits", "x_axis limits sind ungueltig.")
    }
  }

  v$ok <- length(v$errors) == 0
  v
}

`%||%` <- function(x, y) if (is.null(x)) y else x
