#' contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_contact_ui <- function(id){
  ns <- NS(id)
  
#  tagList(
    fluidRow(
     br(),
     br(),
    
      box("", width=12,
          fluidRow(
          column(width=8,
          tags$h3(HTML("Research Institute for Farm Animal Biology (FBN) <br> 
                Institute of Genetics and Biometry <br>
                 Wilhelm-Stahl-Allee 2 <br>
                18196 Dummerstorf <br> <br>
                Email: wittenburg@fbn-dummerstorf.de <br>
                URL link: <a href='https://www.fbn-dummerstorf.de/en/about-us/staff/mitarbeiter/doerte-wittenburg/' target='blank' >Wittenburg homepage</a>
                <br> <br>  
                Email: melzer@fbn-dummerstorf.de <br>
                URL link: <a href='https://www.fbn-dummerstorf.de/en/about-us/staff/mitarbeiter/nina-melzer/' target='blank' >Melzer homepage</a>"
                
          ))),
          column(width=4,
          img(src="www/logo_I2.png",height="220px",width="220px"))
      ))
    )
 
 # )
}
    
#' contact Server Functions
#'
#' @noRd 
mod_contact_server <- function(id){
  moduleServer( id, function(input, output, session){
  })
}
    
## To be copied in the UI
# mod_contact_ui("mod_contact_1")
    
## To be copied in the server
# mod_contact_server("mod_contact_1")


