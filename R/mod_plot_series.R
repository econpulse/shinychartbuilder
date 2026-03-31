mod_plot_series_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Plot-Serien"),
    shiny::textInput(ns("ps_id"), "Plot-Serie ID"),
    shiny::textInput(ns("ps_name"), "Anzeigename"),
    shiny::selectInput(ns("source_id"), "Quelle", choices = NULL),
    shiny::selectInput(ns("geom_type"), "Geom", choices = c("line", "point", "col")),
    shiny::actionButton(ns("add_ps"), "Hinzufuegen")
  )
}

mod_plot_series_server <- function(id, transformations_r, raw_series_r) {
  shiny::moduleServer(id, function(input, output, session) {
    ps_store <- shiny::reactiveVal(list())

    shiny::observe({
      tr_ids <- vapply(transformations_r() %||% list(), function(x) x$id, character(1))
      raw_ids <- vapply(raw_series_r() %||% list(), function(x) x$id, character(1))
      shiny::updateSelectInput(session, "source_id", choices = c(raw_ids, tr_ids))
    })

    shiny::observeEvent(input$add_ps, {
      req(input$ps_id, input$source_id)
      ps <- list(
        id = input$ps_id,
        source_transformation_id = input$source_id,
        display_name = input$ps_name %||% input$ps_id,
        geoms = list(list(type = input$geom_type, color = "#1f77b4", linewidth = 1, alpha = 1)),
        group = NULL,
        facet_key = "default"
      )
      ps_store(c(ps_store(), list(ps)))
    })

    list(plot_series = shiny::reactive(ps_store()))
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x
