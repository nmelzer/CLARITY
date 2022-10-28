# Module UI

#' @title mod_datasets_ui and mod_datasets_server
#' @description  A shiny module containing the relevant information about the data sets and their external preprocessing.
#'
#' @param id module id
#'
#' @rdname mod_datasets
#'
#' @keywords internal, data set
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @export

mod_datasets_ui <- function(id){
  ns <-  shiny::NS(id)

 shiny::tagList(
#    shiny::fluidRow(id="back_methodology" ,  shiny::column(5,""),
#                  shiny::column(5,htmltools::tags$a(href="#info",htmltools::h3("Back to methodology"), onclick = "openTab('methodology')"))),
    shiny::fluidRow(
      htmltools::br(),htmltools::br(),
      shinydashboard::box(title =tags$b(tags$h3("The German Holstein data set")),status="primary",width=12,collapsible=F,solidHeader = TRUE,
          tags$h4(HTML("Data were provided by the German Evaluation Center <a href = 'https://www.vit.de' target='_blank' >vit (IT solutions for animal production)</a>.  Permission for data access was granted by the
                   <a href='https://www.fbf-forschung.de/' target='_blank' >Association for Bioeconomy Research (FBF, Bonn)</a> as representative of German cattle breeders.")),
            htmltools::br(),
            tags$h4("This study used a large pedigree including 367,056 German Holstein cattle (i.e., 1053 half-sib families with sires born between 1979-2017).",
            HTML("Data included genotypes of the Illumina Bovine SNP50 genotype array mapped to the coordinates of the <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank' >ARS-UCD1.2 assembly</a>.") ),
            htmltools::br(),
            tags$h4("After preprocessing, recombination rates were eventually estimated based on 44,631 SNPs and across 876 half-sib families
               with sires having a minimum number of 39 progeny.")
      ),
      htmltools::br(),htmltools::br(),
      shinydashboard::box(title =tags$b(tags$h3("The German/Austrian Fleckvieh data set")),status="primary",width=12,collapsible=F,solidHeader = TRUE,
                        htmltools::tags$h4(htmltools::HTML("Data were provided by <a href = 'https://www.rinderzucht.at/zuchtdata.html' target='_blank' >ZuchtData</a> (Vienna, Austria) with
                          permission granted by the representatives of cattle breeders.")),
                        htmltools::br(),
                        htmltools::tags$h4(htmltools::HTML("The data set included 298,850 genotyped Fleckvieh cattle belonging to 6,866 half-sib families. Genotyping was performed with the Illumina Bovine SNP50
                          genotype array. Genotype data were provided for 40,144 SNPs and physical coordinates were mapped to the
                          <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank' >ARS-UCD1.2 assembly</a>.")),
                        htmltools::br(),
                       htmltools::tags$h4("After preprocessing, recombination rates were estimated based on 40,003 SNPs and across 1,577 half-sib families with sires having a minimum number of 30 progeny.")
    ))
 )
}

# Module Server
#' @rdname mod_datasets
#' @export
#' @keywords internal

mod_datasets_server=function(id){
  shiny::moduleServer(id,function(input, output, session){

    output$try <- renderUI("")
  })
}

## To be copied in the UI
# mod_datasets_ui("mod_datasets_1")

## To be copied in the server
# mod_datasets_server("mod_datasets_1")
