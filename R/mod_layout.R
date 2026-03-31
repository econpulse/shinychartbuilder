mod_layout_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Layout und Achsen"),
    shiny::textInput(ns("title"), "Titel"),
    shiny::textInput(ns("subtitle"), "Untertitel"),
    shiny::textInput(ns("caption"), "Caption"),
    shiny::textInput(ns("x_breaks"), "X date_breaks", value = "3 months"),
    shiny::textInput(ns("x_labels"), "X date_labels", value = "%Y-%m"),
    shiny::textInput(ns("x_limit_from"), "X Limit von (YYYY-MM-DD)", value = ""),
    shiny::textInput(ns("x_limit_to"), "X Limit bis (YYYY-MM-DD)", value = ""),
    shiny::numericInput(ns("y_min"), "Y min", value = NA),
    shiny::numericInput(ns("y_max"), "Y max", value = NA),
    shiny::textInput(ns("y_breaks"), "Y breaks (z.B. 0,10,20)", value = ""),
    shiny::selectInput(ns("y_format"), "Y Format", choices = c("number", "percent"), selected = "number"),
    shiny::numericInput(ns("y_decimals"), "Y Dezimalstellen", value = 2, min = 0, step = 1),
    shiny::checkboxInput(ns("facet_enabled"), "Facets aktivieren", FALSE),
    shiny::textInput(ns("facet_var"), "Facet Variable", value = "facet_key"),
    shiny::selectInput(ns("facet_scales"), "Facet Scales", choices = c("fixed", "free", "free_x", "free_y"), selected = "fixed")
  )
}

parse_numeric_breaks <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  vals <- suppressWarnings(as.numeric(trimws(strsplit(txt, ",")[[1]])))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) NULL else vals
}

mod_layout_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    layout <- shiny::reactive({
      x_limits <- NULL
      if (nzchar(trimws(input$x_limit_from)) && nzchar(trimws(input$x_limit_to))) {
        x_limits <- c(input$x_limit_from, input$x_limit_to)
      }

      list(
        chart_meta = list(
          title = input$title,
          subtitle = input$subtitle,
          caption = input$caption
        ),
        x_axis = list(
          date_breaks = input$x_breaks,
          date_labels = input$x_labels,
          limits = x_limits,
          minor_breaks = NULL
        ),
        y_axis = list(
          min = ifelse(is.na(input$y_min), NULL, input$y_min),
          max = ifelse(is.na(input$y_max), NULL, input$y_max),
          breaks = parse_numeric_breaks(input$y_breaks),
          format = input$y_format,
          decimals = as.integer(input$y_decimals)
        ),
        facets = list(
          enabled = isTRUE(input$facet_enabled),
          facet_var = input$facet_var,
          scales = input$facet_scales,
          ncol = NULL,
          nrow = NULL
        )
      )
    })

    list(layout = layout)
  })
}
