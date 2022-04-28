# Module UI

#' @title mod_misplaced_ui and mod_misplaced_server
#' @description  A shiny Module to generate the outcome for Misplaced markers.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_misplaced
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import shiny
#' @rawNamespace import(shinyjs, except = runExample)
#'

mod_misplaced_ui <-function(id)
{
  ns <- NS(id)

  tagList(
    br(),
    br(),
    fluidRow(
      box(title = tags$b("Markers identified as misplaced or residing in problematic regions"),status="danger",width=12,
          solidHeader = TRUE,collapsible = FALSE,collapsed=FALSE,
          br(),
          br(),
          fluidRow(
            column(width=12,DT::dataTableOutput(ns("tableMis")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
          ),
          fluidRow(
            column(width=10,checkboxInput(ns("checkbox2"), "Show/hide legend", FALSE), p(id = "element2",
              tags$b("SNP"), "",br(),
              tags$b("Old_Chr"),"- Old chromosome number",br(),
              tags$b("Old_bp"),"- Old chromsomal position",br(),
              tags$b("New_Chr"), " - New chromosome number based on LD analysis. Negative values indicate inconclusive results and the analysis
                               continued on the original chromosome. ", br(),
              tags$b("New_bp")," - New chromosomal position denotes the coordinate of the SNP to which the minimum recombination rate was achieved.",br(),
              tags$b("Theta")," -  Minimum recombination rate on new chromosome",br(),
              tags$b("Clear_recrate"), "- Coded as 1 if the recombination rate < 0.01 at the new coordinate (bp), zero otherwise; in total 34",br(),
              tags$b( "BLAST_ProbeA_Chr.bp")," - The position of alternate match when the probe sequence of candidate SNP was aligned to the reference genome.
                                No entry implies a single hit coinciding with the map coordinate in ",
                                tags$a(href="https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2", "ARS-UCD1.2",target="_blank"),
                                " or no hit.", br(),
              tags$b( "Candidate"), "- Coded as 1 if the SNP was identified as candidate of misplacement based on estimates of recombination rate in the
                               original study", tags$a(href = "https://doi.org/10.1186/s12711-020-00593-z","Qanbari and Wittenburg (2020)"),"; in total 51",br(),
              tags$b("Problematic region")," -  Identifier of coherent problematic region ; in total 15", br(),
              tags$b("Start")," - Start  of position of problematic region",br(),
              tags$b("End")," - End of position of problematic region",br(),
              tags$b("n_heterozygous"), "- Number of heterozygous sires based on which recombination rates were estimated"))
          )
       )),
       fluidRow(
       box(title = span(icon("info-circle"), tags$b("General problematic regions")),status="danger",width=12,
           solidHeader = TRUE,collapsible = TRUE,collapsed=TRUE,
           fluidRow(
             br(),
             br(),
             column(10, "Currently, there are some problematic regions, that are recommended to remove regardless of the SNP panel used (please cite as follows:
                    Qanbari S, Schnabel RD, Wittenburg D (2022). Evidence of Rare Misassemblies in the Bovine Reference Genome Revealed by Population Genetic Metrics. Anim Genet (to appear))."
                  )
           ),
           br(),
           fluidRow(

             column(width=4,DT::dataTableOutput(ns("tableGenProReg")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
           )
       )
     )
    )
}


# Module Server
#' @rdname mod_misplaced

#' @export
#' @keywords internal
#' @import htmltools

mod_misplaced_server <- function(input, output, session, filter)
{

  misplacedMarkers<-generalProblematicRegions<-NULL

  load(system.file("extdata","misplacedMarkers.Rdata",package="CLARITY"))
  data=misplacedMarkers

  ##### all for table 1 - putative misplaced markers
  thead<-tr<-th<-NULL
  sketch = htmltools::withTags(
    table( ##
      class = 'display',
      thead(
        tr(
          th(colspan=1,"SNP "),
          th(colspan=1,"Old_Chr"),
          th(colspan=1,"Old_bp"),
          th(colspan=1,"New_Chr"),
          th(colspan=1,"New_bp"),
          th(colspan=1,"Theta"),
          th(colspan=1,"Clear_recrate"),
          th(colspan=1,"BLAST_ProbeA_Chr.bp"),
          th(colspan=1,"Candidate"),
          th(colspan=1,"Problematic region"),
          th(colspan=1,"Start"),
          th(colspan=1,"End"),
          th(colspan=1,"n_heterozygous"),
        )
      )
    )
  )

  if(filter=="All")
  {
      output$tableMis=DT::renderDataTable(
        data, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom='Bfrtip',
                columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf'),
                pagelength = 10, lengthMenu = list(c(10, 20,30, -1), c('10', '20','30','All')))
      )
  }
  if(filter!="All")
  {
    output$tableMis=DT::renderDataTable(
      data[data$old_chr==filter,],extensions = c("Buttons"), container=sketch,rownames=FALSE,
      options = list(columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt', buttons = c('copy', 'csv', 'excel'))
    )
  }

  ## show / hide legend
  observe({
    toggle(id = "element2", condition = input$checkbox2,asis=TRUE)
  })



  ##### all for general problematic regions
  load(system.file("extdata","generalProblematicRegions.Rdata",package="CLARITY"))
  data2=generalProblematicRegions

  sketch2 = htmltools::withTags(
    table( ##
      class = 'display',
      thead(
        tr(
          th(colspan=1,"Chromsome"),
          th(colspan=1,"Start (bp)"),
          th(colspan=1,"End (bp)"),
        )
      )
    )
  )
  output$tableGenProReg=DT::renderDataTable(
    data2[,1:3], extensions = c("Buttons"), container=sketch2,rownames=FALSE,options = list(searching=FALSE,dom='Bfrtip',
                                                                                    columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                                    buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf'),
                                                                                    pagelength = 10, lengthMenu = list(c(10, 20,30, -1), c('10', '20','30','All')))
  )

}


## To be copied in the UI
# mod_misplaced_ui("mod_misplaced_1")

## To be copied in the server
# callModule(mod_misplaced_server,"mod_misplaced_1")
