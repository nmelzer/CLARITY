# Module UI

#' @title mod_about_the_project_ui and mod_about_the_project_server
#' @description  A shiny module to provide all relevant information about the project.
#'
#' @param id module id
#'
#' @rdname mod_about_the_project
#' @keywords internal, information
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @export

mod_about_the_project_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      fluidRow(style='margin-left: 6px;',
               column(width=12), htmltools::h3("About the project")),
                shinydashboard::box(status="danger",width=12, solidHeader = TRUE,collapsible = FALSE,
                 shiny::fluidRow(
                   shiny::column(width=12,htmltools::tags$h4("Cattle breeds are strongly influenced by artificial selection. To manage a population under non-random mating conditions,
                                          monitoring genetic parameters such as effective population size as well as characterizing the recombination landscape are particularly important.",  htmltools::tags$br(),
                                               htmltools::tags$br(),
                                          "This project aimed at:",htmltools::tags$br(),htmltools::tags$br(),
                                          "(1) Constructing updated genetic map of cattle breeds by estimating recombination rates between pairs of molecular markers
                                          (e.g., single nucleotide polymorphisms) considering the family stratification. Establishing breed-specific genetic maps is crucial to
                                          advance cattle breeding and to increase the efficiency of use of our animal resources.", htmltools::tags$br(),  htmltools::tags$br(),
                                          "(2) Characterizing genomic regions with higher recombination frequency.  Identifying local phenomena like hot and cold spots of recombination activities helps elucidating the chromosome structure.", htmltools::tags$br(),htmltools::tags$br(),
                                          "Both steps are important in order to enhance our understanding of the underlying of recombination activity in cattle."),
                                htmltools::br(),htmltools::br(),htmltools::br(),htmltools::br(),htmltools::br(),htmltools::br(),htmltools::br(),
                                htmltools::tags$h4("The project", htmltools::em("'The construction of a combined physical-genetic map for a dairy cattle breed in Germany (CLARITY)'"), "was supported (2019 - 2022) by the grant from the",
                                                   htmltools::HTML("<a href = 'https://www.gesundheitsforschung-bmbf.de/de/clarity-die-entwicklung-einer-kombinierten-physisch-genetischen-karte-fur-eine-9212.php' target='_blank'><u>German Federal Ministry of Education and Research (BMBF, FKZ 031L0166 CompLS)</u></a>.")),
                                htmltools::tags$a(href = 'https://www.gesundheitsforschung-bmbf.de/de/clarity-die-entwicklung-einer-kombinierten-physisch-genetischen-karte-fur-eine-9212.php',
                                                  htmltools::tags$img(src = 'www/BMBF_logo_en.jpg',alt="BMBF-logo", title = "Company Home",width="180px",height="143px"), target="_blank",  ## target blank added also enables to open website in a new tab using a browser
                                                  style = "padding-top:10px; padding-bottom:10px;")
                      )
              )
      )
    )
  )
}


# Module Server
#' @rdname mod_about_the_project
#' @export

mod_about_the_project_server=function(id){
  shiny::moduleServer(id,function(input, output, session){
})
}

## mod_about_the_project_ui("mod_about_the_project_1")

## To be copied in the server
# mod_about_the_project_server("mod_about_the_project_1")
