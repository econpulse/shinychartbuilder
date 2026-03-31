mod_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Preview"),
    shiny::actionButton(ns("refresh"), "Preview aktualisieren"),
    shiny::verbatimTextOutput(ns("status")),
    shiny::plotOutput(ns("plot"), height = "520px")
  )
}

mod_preview_server <- function(id, chart_config_r, raw_data_loader_fn) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::eventReactive(input$refresh, {
      cfg <- chart_config_r()

      v <- validate_chart_config(cfg)
      if (!v$ok) return(list(ok = FALSE, errors = v$errors, plot = NULL))

      raw_data <- raw_data_loader_fn(cfg$raw_series)
      tr <- apply_transformations(raw_data, cfg$transformations, freq_policy = list(target_freq = "daily", aggregation_method = "last"))
      if (!tr$ok) return(list(ok = FALSE, errors = tr$errors, plot = NULL))

      ds <- build_plot_dataset(tr$data, cfg$plot_series)
      p <- build_ggplot_object(ds, cfg)
      list(ok = TRUE, errors = list(), plot = p)
    })

    output$status <- shiny::renderPrint({
      r <- result()
      if (is.null(r)) "Noch keine Preview vorhanden." else if (!r$ok) r$errors else "OK"
    })

    output$plot <- shiny::renderPlot({
      r <- result()
      shiny::req(!is.null(r), r$ok, !is.null(r$plot))
      r$plot
    })

    list(result = result)
  })
}
