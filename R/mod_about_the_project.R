# Module UI

#' @title mod_about_the_project_ui and mod_about_the_project_server
#' @description  A shiny Module to provide all the relevant information about the project.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about_the_project
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import shiny 
#' @import shinydashboard 


mod_about_the_project_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width=12,box(title = tags$b(tags$h3("CLARITY: The construction of a combined physical-genetic map for a dairy cattle breed in Germany")),status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,  
              br(),br(),
              fluidRow(
                column(width=12,tags$h4(tags$b("General aim")),br(),
                                tags$h4("Dairy cattle are strongly influenced by artificial selection. To manage a population under non-random mating conditions, 
                                          monitoring genetic parameters such as effective population size as well as characterizing the recombination landscape are particularly important.",  tags$br(),
                                          "This project aimed at:",tags$br(),tags$br(),
                                          "(1) Constructing an updated genetic map of dairy cattle by estimating recombination rates between pairs of molecular markers 
                                          (e.g., single nucleotide polymorphisms) considering the family stratification. Establishing a comprehensive genetic map is crucial to 
                                          advance cattle breeding and to increase the efficiency of use of our animal resources.", tags$br(),  tags$br(),
                                          "(2) Characterizing genomic regions with higher recombination frequency.  Identifying local phenomena like hot and cold spots of recombination activities helps elucidating the chromosome structure.", tags$br(),tags$br(),
                                          "Both steps are important in order to enhance our understanding of the underlying of recombination activity in cattle."), 
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                tags$h4("The project is supported by the grant from the", HTML("<a href = 'https://www.gesundheitsforschung-bmbf.de/de/clarity-die-entwicklung-einer-kombinierten-physisch-genetischen-karte-fur-eine-9212.php' target='_blank' >German Federal Ministry of Education and Research (BMBF, FKZ 031L0166 CompLS).</a>")),

                                tags$a(href = 'https://www.gesundheitsforschung-bmbf.de/de/clarity-die-entwicklung-einer-kombinierten-physisch-genetischen-karte-fur-eine-9212.php', ## div vor href setzt es ausser gefecht mit a wird es aktivieret
                                tags$img(src = 'www/BMBF_logo_en.jpg',
                                title = "Company Home", height =200, width=350), target="_blank",  ## target blank added also enables to open website in a new tab using a browser
                                style = "padding-top:10px; padding-bottom:10px;")
                      
                               #imageOutput(ns("bmbf"),width="100%",height="100%")
                )
              )
           )
        )
     )
  )
}


# Module Server
#' @rdname mod_about_the_project
#' 
#' @export
#' @keywords internal
mod_about_the_project_server=function(input, output, session){
  
  output$bmbf=shiny::renderImage({
    filename <- "inst/figures/BMBF_logo_en.jpg"
    list(src = filename,width="15%",height="15%")
  }, deleteFile = FALSE)
  
  
}




## To be copied in the UI
# mod_about_the_project_ui("mod_about_the_project_1")

## To be copied in the server
# callModule(mod_about_the_project_server,"mod_about_the_project_1")