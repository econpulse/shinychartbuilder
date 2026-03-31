# Plot-Dataset und ggplot-Build

`%||%` <- function(x, y) if (is.null(x)) y else x

build_plot_dataset <- function(transformed_data, plot_series_cfg) {
  rows <- list()

  for (ps in plot_series_cfg) {
    src_id <- ps$source_transformation_id
    dat <- transformed_data[[src_id]]
    if (is.null(dat) || nrow(dat) == 0) next

    for (g in ps$geoms) {
      rows[[length(rows) + 1]] <- tibble::tibble(
        date = as.Date(dat$date),
        value = as.numeric(dat$value),
        plot_series_id = ps$id,
        display_name = ps$display_name %||% ps$id,
        facet_key = ps$facet_key %||% "default",
        geom_type = g$type,
        color = g$color %||% "#1f77b4",
        linewidth = g$linewidth %||% 1,
        alpha = g$alpha %||% 1,
        point_size = g$point_size %||% 2
      )
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      date = as.Date(character()), value = numeric(), plot_series_id = character(),
      display_name = character(), facet_key = character(), geom_type = character(),
      color = character(), linewidth = numeric(), alpha = numeric(), point_size = numeric()
    ))
  }

  dplyr::bind_rows(rows)
}

build_y_label_formatter <- function(y_cfg) {
  fmt <- y_cfg$format %||% "number"
  decimals <- as.integer(y_cfg$decimals %||% 2L)

  if (identical(fmt, "percent")) {
    function(x) sprintf(paste0("%.", decimals, "f%%"), x)
  } else {
    function(x) formatC(x, format = "f", digits = decimals, big.mark = "'")
  }
}

build_ggplot_object <- function(plot_data, chart_config) {
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$date, y = .data$value))

  for (gt in unique(plot_data$geom_type)) {
    d <- dplyr::filter(plot_data, .data$geom_type == !!gt)

    if (gt == "line") {
      p <- p + ggplot2::geom_line(
        data = d,
        ggplot2::aes(color = .data$display_name, group = .data$plot_series_id, linewidth = .data$linewidth, alpha = .data$alpha)
      )
    } else if (gt == "point") {
      p <- p + ggplot2::geom_point(
        data = d,
        ggplot2::aes(color = .data$display_name, group = .data$plot_series_id, size = .data$point_size, alpha = .data$alpha)
      )
    } else if (gt == "col") {
      p <- p + ggplot2::geom_col(
        data = d,
        ggplot2::aes(fill = .data$display_name, group = .data$plot_series_id),
        alpha = 0.9
      )
    }
  }

  meta <- chart_config$chart_meta
  p <- p + ggplot2::labs(
    title = meta$title %||% "",
    subtitle = meta$subtitle %||% "",
    caption = meta$caption %||% "",
    x = NULL,
    y = NULL
  )

  x_cfg <- chart_config$x_axis
  x_limits <- NULL
  if (!is.null(x_cfg$limits) && length(x_cfg$limits) == 2) {
    x_limits <- as.Date(x_cfg$limits)
  }

  p <- p + ggplot2::scale_x_date(
    date_breaks = x_cfg$date_breaks %||% "3 months",
    date_labels = x_cfg$date_labels %||% "%Y-%m",
    limits = x_limits
  )

  y_cfg <- chart_config$y_axis
  y_limits <- c(y_cfg$min, y_cfg$max)
  if (all(is.null(y_limits))) y_limits <- NULL
  y_breaks <- y_cfg$breaks
  y_labels <- build_y_label_formatter(y_cfg)

  p <- p + ggplot2::scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    labels = y_labels
  )

  f_cfg <- chart_config$facets
  if (isTRUE(f_cfg$enabled)) {
    facet_var <- f_cfg$facet_var %||% "facet_key"
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste("~", facet_var)),
      scales = f_cfg$scales %||% "fixed",
      ncol = f_cfg$ncol,
      nrow = f_cfg$nrow
    )
  }

  p + ggplot2::theme_minimal(base_size = chart_config$theme_options$base_font_size %||% 12)
}
