# Module UI

#' @title mod_misplaced_ui and mod_misplaced_server
#' @description  A shiny module to generate the outcome for the misplaced markers.
#'
#' @param id module id
#'
#' @rdname mod_misplaced
#'
#' @keywords internal, misplaced marker
#' @import shiny
#' @import htmltools
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @rawNamespace import(shinyjs, except = runExample)
#' @export

mod_misplaced_ui <-function(id)
{
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::br(),
    htmltools::br(),
    tags$h4(HTML("For any breed, the recombination rate analysis discarded markers being putatively misplaced in the bovine genome assembly <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank' >ARS-UCD1.2 assembly</a>.
                 For this, two sources of information were taken into account.")
    ),
   # shiny::fluidRow(id="back_methodology2" ,  shiny::column(5,""),
  #                  shiny::column(5,htmltools::tags$a(href="#info",htmltools::h3("Back to methodology"), onclick = "openTab('methodology')"))),
    htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(title=tags$b(tags$h3("1. General problematic regions")),status="primary",width=12,
          solidHeader = TRUE,collapsible = FALSE,collapsed=FALSE,
          shiny::fluidRow(
            htmltools::br(),htmltools::br(),
            shiny::column(10, tags$h4("Currently, there are some problematic regions, that are recommended to remove regardless of the SNP panel used (please cite
                    ",HTML("<a href = 'https://doi.org/10.1111/age.13205' target='_blank' >Qanbari et al. 2022</a>)."))
            )
          ),
          shiny:: fluidRow(
            htmltools::br(),htmltools::br(),
            shiny::column(width=6,DT::dataTableOutput(ns("tableGenProReg"),width="auto",height="auto")%>%shinycssloaders::withSpinner(color="#0dc5c1"),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
          )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(title = tags$b(tags$h3("2. Markers identified as misplaced or residing in problematic regions in Holstein")),status="primary",width=12,
          solidHeader = TRUE,collapsible = FALSE,collapsed=FALSE,
          htmltools::br(),htmltools::br(),
          shiny::fluidRow(
            shiny::column(10,htmltools::tags$h4("Specifically, we identified a panel of 65 SNP markers with strong evidence for being misplaced based on the analysis of linkage disequilibrium and recombination rate in Holstein cattle."))
          ),
          htmltools::br(),htmltools::br(),
          shiny:: fluidRow(
            shiny::column(5,shiny::selectInput(inputId = ns('old_chr'), label = 'Select based on Old_Chr:', choices= c(" ","All",1:29))),
            shiny::column(5,shiny::selectInput(inputId = ns('new_chr'), label = 'Select based on New_Chr:', choices= c(" ","All",1:29)))
          ),
          shiny::fluidRow(
            shiny::column(width=12,DT::dataTableOutput(ns("tableMis"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1"),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
          ),
          shiny::fluidRow(
            shiny::column(width=10,shiny::checkboxInput(ns("checkbox2"), "Show/hide legend", FALSE), htmltools::p(id = "element2",
            "SNP: SNP name",htmltools::br(),
            "Old_Chr: Old chromosome number",htmltools::br(),
            "Old_bp: Old chromsomal position",htmltools::br(),
             "New_Chr: New chromosome number based on LD analysis. Negative values indicate inconclusive results and the analysis
                               continued on the original chromosome. ", htmltools::br(),
            "New_bp: New chromosomal position denotes the coordinate of the SNP to which the minimum recombination rate was achieved.",htmltools::br(),
            "Theta:  Minimum recombination rate on new chromosome",htmltools::br(),
             "Clear_recrate: Coded as 1 if the recombination rate < 0.01 at the new coordinate (bp), zero otherwise; in total 34",htmltools::br(),
            "BLAST_ProbeA_Chr.bp: The position of alternate match when the probe sequence of candidate SNP was aligned to the reference genome.
                                No entry implies a single hit coinciding with the map coordinate in ",
            htmltools::tags$a(href="https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2", "ARS-UCD1.2",target="_blank"),
            " or no hit.", htmltools::br(),
             "Candidate: Coded as 1 if the SNP was identified as candidate of misplacement based on estimates of recombination rate in the
                               original study", tags$a(href = "https://doi.org/10.1186/s12711-020-00593-z","Qanbari and Wittenburg (2020)"),"; in total 51",htmltools::br(),
            "Problematic region: Identifier of coherent problematic region ; in total 15",htmltools::br(),
            "Start: Start  of position of problematic region",htmltools::br(),
            "End End of position of problematic region",htmltools::br(),
             "n_heterozygous: Number of heterozygous sires based on which recombination rates were estimated"))
          )
     ))
  )
}


# Module Server
#' @rdname mod_misplaced
#' @export

mod_misplaced_server <- function(id){
  shiny::moduleServer(id,function(input, output, session)
  {
    ##### all for table 1 -  general problematic regions
    thead<-tr<-th<-NULL
    sketch2 = htmltools::withTags(
      table(
        class = 'display',
        thead(
          tr(
            th(colspan=1,"Chromosome"),
            th(colspan=1,"Start (bp)"),
            th(colspan=1,"End (bp)"),
          )
        )
      )
    )

    load(system.file("extdata","general/generalProblematicRegions.Rdata",package="CLARITY"))
    data2=generalProblematicRegions

    output$tableGenProReg=DT::renderDataTable({
      DT::datatable(
      data2[,1:3], extensions = c("Buttons"), container=sketch2,rownames=FALSE,options = list(searching=FALSE,dom='Bfrtip',
                                                                                              columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                                              buttons = list('pageLength', 'copy',list(extend='csv',title="General_problematic_regions"),list(extend='excel',title="General_problematic_regions")),
                                                                                              pagelength = 10, lengthMenu = list(c(10, 20,30, -1), c('10', '20','30','All'))))
    },server=FALSE)

    ##### all for table 2 - putative misplaced markers
    misplacedMarkers<-generalProblematicRegions<-NULL

    load(system.file("extdata",paste0("general/misplacedMarkers.Rdata"),package="CLARITY"))   ## has to be changed since no breed specific onces
    data<-misplacedMarkers

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

   output$tableMis=DT::renderDataTable({
     DT::datatable(
     data, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom='Bfrtip',
                                                                                     columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                                     buttons = c('pageLength', 'copy',list(extend='csv',title="Misplaced_markers-all"),list(extend='excel',title="Misplaced_markers-all")),
                                                                                     pagelength = 10, lengthMenu = list(ll1, ll2)))
   },server=FALSE)

    shiny::observeEvent(input$old_chr,
    {
      req(input$old_chr)
      shiny::updateSelectInput(session,'old_chr',label="Select based on Old_Chr:",choices=c(" ",1:29,"All"),selected="")
      if(input$old_chr=="All" || input$old_chr==" "){
        data.mis=data
        ll1=c(10, 20,30, -1)
        ll2=c('10', '20','30','All')
      }
      else
      {
        data.mis=data[data[,2] %in% as.numeric(input$old_chr), ]
        ll1=-1
        ll2='All'
      }

      output$tableMis=DT::renderDataTable({
        DT::datatable(
          data.mis, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom='Bfrtip',
                                                                                       columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                                       buttons = list('pageLength', 'copy',list(extend='csv',title=paste0("Misplaced_markers-oldBTA-",input$old_chr)),list(extend='excel',title=paste0("Misplaced_markers-oldBTA-",input$old_chr))),
                                                                                       pagelength = 10, lengthMenu = list(ll1, ll2)))
      },server = FALSE)
    })

   shiny::observeEvent(input$new_chr,
   {
      req(input$new_chr)
      shiny::updateSelectInput(session,'new_chr',label="Select based on New_Chr:",choices=c(" ",1:29,"All"),selected="")
      if(input$new_chr=="All" || input$new_chr==" ")
      {
         data.mis=data
         ll1=c(10, 20,30, -1)
         ll2=c('10', '20','30','All')
      }
      else
      {
         ll1= -1
         ll2='All'
         data.mis=data[abs(data[,4]) %in% as.numeric(input$new_chr), ]
      }

      output$tableMis=DT::renderDataTable({
        DT::datatable(
          data.mis, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom='Bfrtip',
                                                                                      columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                                      buttons = list('pageLength', 'copy',list(extend='csv',title=paste0("Misplaced_markers-newBTA-",input$new_chr)),list(extend='excel',title=paste0("Misplaced_markers-newBTA-",input$new_chr))),
                                                                                      pagelength = 10, lengthMenu = list(ll1, ll2)))
       },server=FALSE)
    })

    ## show / hide legend
    shiny::observe({
      shinyjs::toggle(id = "element2", condition = input$checkbox2,asis=TRUE)
    })
  })
}

## To be copied in the UI
# mod_misplaced_ui("mod_misplaced_1")

## To be copied in the server
# mod_misplaced_server("mod_misplaced_1")
