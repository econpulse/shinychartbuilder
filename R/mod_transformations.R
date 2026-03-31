mod_transformations_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Transformationen"),
    shiny::textInput(ns("tr_id"), "Transformation ID"),
    shiny::selectInput(ns("tr_type"), "Typ", choices = c("identity", "ratio", "difference", "sma", "ema", "yoy_pct")),
    shiny::uiOutput(ns("inputs_ui")),
    shiny::uiOutput(ns("params_ui")),
    shiny::textInput(ns("output_label"), "Output Label"),
    shiny::actionButton(ns("add_tr"), "Hinzufuegen")
  )
}

mod_transformations_server <- function(id, raw_series_r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr_store <- shiny::reactiveVal(list())

    available_inputs <- shiny::reactive({
      raw_ids <- vapply(raw_series_r() %||% list(), function(x) x$id, character(1))
      tr_ids <- vapply(tr_store() %||% list(), function(x) x$id, character(1))
      c(raw_ids, tr_ids)
    })

    output$inputs_ui <- shiny::renderUI({
      n <- if (input$tr_type %in% c("ratio", "difference")) 2 else 1
      tagList(
        lapply(seq_len(n), function(i) {
          shiny::selectInput(ns(paste0("input_", i)), paste("Input", i), choices = available_inputs())
        })
      )
    })

    output$params_ui <- shiny::renderUI({
      if (input$tr_type %in% c("sma", "ema")) {
        shiny::numericInput(ns("param_window"), "Window", value = 20, min = 1, step = 1)
      } else if (input$tr_type == "yoy_pct") {
        shiny::numericInput(ns("param_lag_n"), "Lag N", value = 12, min = 1, step = 1)
      } else {
        shiny::helpText("Keine zusaetzlichen Parameter fuer diesen Typ.")
      }
    })

    shiny::observeEvent(input$add_tr, {
      req(input$tr_id, input$tr_type)

      n <- if (input$tr_type %in% c("ratio", "difference")) 2 else 1
      inputs <- vapply(seq_len(n), function(i) input[[paste0("input_", i)]], character(1))

      params <- switch(
        input$tr_type,
        sma = list(window = as.integer(input$param_window %||% 20L)),
        ema = list(window = as.integer(input$param_window %||% 20L)),
        yoy_pct = list(lag_n = as.integer(input$param_lag_n %||% 12L)),
        list()
      )

      new_tr <- list(
        id = input$tr_id,
        type = input$tr_type,
        inputs = inputs,
        params = params,
        output_label = input$output_label %||% input$tr_id
      )

      tr_store(c(tr_store(), list(new_tr)))
    })

    list(
      transformations = shiny::reactive(tr_store()),
      event = shiny::reactive(input$add_tr)
    )
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x
