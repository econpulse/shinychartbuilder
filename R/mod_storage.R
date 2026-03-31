mod_storage_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Speicher"),
    shiny::selectInput(ns("chart_id"), "Gespeicherte Charts", choices = NULL),
    shiny::actionButton(ns("load_btn"), "Laden"),
    shiny::actionButton(ns("save_btn"), "Speichern"),
    shiny::actionButton(ns("save_as_btn"), "Speichern unter"),
    shiny::actionButton(ns("delete_btn"), "Loeschen")
  )
}

mod_storage_server <- function(id, con, chart_config_r) {
  shiny::moduleServer(id, function(input, output, session) {
    chart_list <- shiny::reactive(list_chart_configs(con))

    shiny::observe({
      tbl <- chart_list()
      choices <- if (nrow(tbl) == 0) character() else setNames(tbl$chart_id, paste0(tbl$chart_name, " (#", tbl$chart_id, ")"))
      shiny::updateSelectInput(session, "chart_id", choices = choices)
    })

    loaded_config <- shiny::eventReactive(input$load_btn, {
      req(input$chart_id)
      load_chart_config(con, as.integer(input$chart_id))
    })

    save_res <- shiny::eventReactive(input$save_btn, {
      cfg <- chart_config_r()
      cid <- cfg$chart_meta$chart_id
      save_chart_config(con, cfg, chart_id = cid, save_as_new = FALSE)
    })

    save_as_res <- shiny::eventReactive(input$save_as_btn, {
      cfg <- chart_config_r()
      save_chart_config(con, cfg, chart_id = NULL, save_as_new = TRUE)
    })

    shiny::observeEvent(input$delete_btn, {
      req(input$chart_id)
      shiny::showModal(shiny::modalDialog(
        title = "Chart loeschen",
        sprintf("Moechtest du Chart-ID %s wirklich loeschen?", input$chart_id),
        footer = tagList(
          shiny::modalButton("Abbrechen"),
          shiny::actionButton(session$ns("confirm_delete"), "Ja, loeschen", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_delete, {
      req(input$chart_id)
      delete_chart_config(con, as.integer(input$chart_id), soft_delete = TRUE)
      shiny::removeModal()
    })

    list(
      loaded_config = loaded_config,
      save_result = save_res,
      save_as_result = save_as_res,
      chart_list = chart_list,
      load_event = shiny::reactive(input$load_btn)
    )
  })
}
