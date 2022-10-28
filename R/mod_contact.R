# Module UI

#' @title mod_contact_ui and mod_contact_server
#' @description  A shiny module containing the contact information.
#'
#' @param id module id

#' @rdname mod_contact
#'
#' @keywords internal, data set
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @export


mod_contact_ui = function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::br(),htmltools::br(),
      shinydashboard::box(status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,
         shiny::fluidRow(
           shiny::column(width=8, htmltools::tags$h4(htmltools::HTML("Research Institute for Farm Animal Biology (FBN) <br>
                                  Wilhelm-Stahl-Allee 2 <br>
                                  18196 Dummerstorf <br> <br>
                                  Email: wittenburg@fbn-dummerstorf.de <br>
                                  URL link: <a href='https://www.fbn-dummerstorf.de/en/about-us/staff/mitarbeiter/doerte-wittenburg/' target='blank' >Wittenburg homepage</a>
                                  <br> <br>
                                  Email: melzer@fbn-dummerstorf.de <br>
                                  URL link: <a href='https://www.fbn-dummerstorf.de/en/about-us/staff/mitarbeiter/nina-melzer/' target='blank' >Melzer homepage</a>"
          ))),
          shiny::column(width=4,shiny::img(src="www/logo_I2.png",alt="institute_logo",width="188px"))
        )
      )
  )
}


#' @rdname mod_contact
#' @export
#' @keywords internal

mod_contact_server = function(id){
  shiny::moduleServer( id, function(input, output, session){
  })
}

## To be copied in the UI
# mod_contact_ui("mod_contact_1")

## To be copied in the server
# mod_contact_server("mod_contact_1")


