mod_data_source_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Datenquellen"),
    shiny::selectizeInput(ns("tickers"), "Ticker", choices = NULL, multiple = TRUE),
    shiny::dateInput(ns("date_from"), "Von", value = Sys.Date() - 365),
    shiny::dateInput(ns("date_to"), "Bis", value = Sys.Date()),
    shiny::actionButton(ns("apply_raw"), "Rohserien uebernehmen")
  )
}

mod_data_source_server <- function(id, available_tickers_r) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      tk <- available_tickers_r()
      choices <- if (is.data.frame(tk)) tk$ticker else tk
      shiny::updateSelectizeInput(session, "tickers", choices = choices, server = TRUE)
    })

    raw_series_defs <- shiny::eventReactive(input$apply_raw, {
      req(input$tickers)
      lapply(seq_along(input$tickers), function(i) {
        list(id = paste0("raw_", i), ticker = input$tickers[[i]], frequency = "daily", source = "ticker_data")
      })
    }, ignoreNULL = FALSE)

    date_range <- shiny::reactive({
      list(date_from = input$date_from, date_to = input$date_to)
    })

    list(
      raw_series_defs = raw_series_defs,
      date_range = date_range,
      event = shiny::reactive(input$apply_raw)
    )
  })
}
