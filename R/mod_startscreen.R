#' startscreen UI Function
#'
#' @title mod_startscreen_ui and mod_startscreen_server
#'
#' @rdname mod_startscreen
#' @description A shiny Module to create the start screen.
#'
#' @param id Internal parameters for shiny.
#' @import shiny
#' @import htmltools
#' @export
#'
#' @importFrom shiny NS tagList
mod_startscreen_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(shiny::column(width=12,"")),
    shiny::fluidRow(shiny::column(width=12,style='margin-left: 0px;',
                                  div(htmltools::HTML('<b><h3>Welcome to the CLARITY app for an interactive exploration of breed-specific
                               genetic maps in cattle </h3></b>'))),

                    shiny::column(11, tags$img(src = 'www/start-picture.jpg',alt="Cow_pictures",width="72%", height="62%")),
                    shiny::column(1,""),
                    shiny::column(12,htmltools::HTML('<p style = "font-size:9px; margin-bottom: 0.5em"> &#169;FBN, Qualitas AG, ZuchtData </p>')),
    )
  )
}

#' startscreen Server Functions
#' @rdname mod_startscreen
#'
#' @export
mod_startscreen_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}


## To be copied in the UI
# mod_startscreen_ui("startscreen_1")

## To be copied in the server
# mod_startscreen_server("startscreen_1")
