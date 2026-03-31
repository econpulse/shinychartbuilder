library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(RSQLite)
library(jsonlite)
library(dplyr)
library(tibble)

source("R/chart_config.R")
source("R/transformation_engine.R")
source("R/plot_builder.R")
source("R/storage.R")
source("R/db_query.R")
source("R/validators.R")
source("R/mod_data_source.R")
source("R/mod_transformations.R")
source("R/mod_plot_series.R")
source("R/mod_layout.R")
source("R/mod_storage.R")
source("R/mod_preview.R")

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$script(src = "unsaved_changes_guard.js")
  ),
  layout_columns(
    col_widths = c(4, 8),
    div(
      mod_data_source_ui("data_source"),
      mod_transformations_ui("transformations"),
      mod_plot_series_ui("plot_series"),
      mod_layout_ui("layout"),
      mod_storage_ui("storage")
    ),
    div(mod_preview_ui("preview"))
  )
)

server <- function(input, output, session) {
  con <- init_storage("data/charts.sqlite")
  onStop(function() DBI::dbDisconnect(con))

  cache <- new_data_cache()

  state <- reactiveValues(
    chart_config = new_chart_config(),
    dirty = FALSE
  )

  # Demo: ticker_meta initialisieren, falls leer
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ticker_meta (ticker TEXT PRIMARY KEY, frequency TEXT, description TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ticker_data (ticker TEXT, date TEXT, value REAL)")

  available_tickers_r <- reactive({
    list_available_tickers(con)
  })

  data_mod <- mod_data_source_server("data_source", available_tickers_r = available_tickers_r)
  tr_mod <- mod_transformations_server("transformations", raw_series_r = data_mod$raw_series_defs)
  ps_mod <- mod_plot_series_server("plot_series", transformations_r = tr_mod$transformations, raw_series_r = data_mod$raw_series_defs)
  layout_mod <- mod_layout_server("layout")
  storage_mod <- mod_storage_server("storage", con = con, chart_config_r = reactive(state$chart_config))

  observe({
    cfg <- state$chart_config
    cfg$raw_series <- data_mod$raw_series_defs()
    cfg$transformations <- tr_mod$transformations()
    cfg$plot_series <- ps_mod$plot_series()

    lay <- layout_mod$layout()
    cfg$chart_meta$title <- lay$chart_meta$title
    cfg$chart_meta$subtitle <- lay$chart_meta$subtitle
    cfg$chart_meta$caption <- lay$chart_meta$caption
    cfg$x_axis <- lay$x_axis
    cfg$y_axis <- lay$y_axis
    cfg$facets <- lay$facets
    cfg$chart_meta$updated_at <- Sys.time()

    state$chart_config <- cfg
    state$dirty <- TRUE
    shiny::session$sendCustomMessage("dirty-state", list(is_dirty = TRUE))
  })

  observeEvent(storage_mod$save_result(), {
    res <- storage_mod$save_result()
    cfg <- state$chart_config
    cfg$chart_meta$chart_id <- res$chart_id
    cfg$storage_meta$revision <- res$revision
    cfg$storage_meta$last_saved_at <- Sys.time()
    cfg$storage_meta$dirty <- FALSE
    state$chart_config <- cfg
    state$dirty <- FALSE
    shiny::session$sendCustomMessage("dirty-state", list(is_dirty = FALSE))
  })

  observeEvent(storage_mod$save_as_result(), {
    res <- storage_mod$save_as_result()
    cfg <- state$chart_config
    cfg$chart_meta$chart_id <- res$chart_id
    cfg$storage_meta$revision <- res$revision
    cfg$storage_meta$last_saved_at <- Sys.time()
    cfg$storage_meta$dirty <- FALSE
    state$chart_config <- cfg
    state$dirty <- FALSE
    shiny::session$sendCustomMessage("dirty-state", list(is_dirty = FALSE))
  })

  observeEvent(storage_mod$load_event(), {
    if (isTRUE(state$dirty)) {
      showModal(modalDialog(
        title = "Ungespeicherte Aenderungen",
        "Es gibt ungespeicherte Aenderungen. Trotzdem laden?",
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("confirm_load_anyway", "Trotzdem laden", class = "btn-warning")
        )
      ))
    } else {
      loaded <- storage_mod$loaded_config()
      if (!is.null(loaded) && is.null(loaded$.parse_error)) {
        state$chart_config <- loaded
        state$dirty <- FALSE
        shiny::session$sendCustomMessage("dirty-state", list(is_dirty = FALSE))
      }
    }
  })

  observeEvent(input$confirm_load_anyway, {
    removeModal()
    loaded <- storage_mod$loaded_config()
    if (!is.null(loaded) && is.null(loaded$.parse_error)) {
      state$chart_config <- loaded
      state$dirty <- FALSE
      shiny::session$sendCustomMessage("dirty-state", list(is_dirty = FALSE))
    }
  })

  raw_loader <- function(raw_series_cfg) {
    dr <- data_mod$date_range()
    load_raw_series_data(con, raw_series_cfg, date_from = dr$date_from, date_to = dr$date_to, cache = cache)
  }

  mod_preview_server("preview", chart_config_r = reactive(state$chart_config), raw_data_loader_fn = raw_loader)
}

shinyApp(ui, server)
