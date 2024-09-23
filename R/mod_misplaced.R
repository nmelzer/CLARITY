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
#' @import magrittr
#' @rawNamespace import(dplyr, except = combine)
#' @rawNamespace import(shinyjs, except = runExample)
#' @export

mod_misplaced_ui <-function(id)
{
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::br(),
    htmltools::br(),
    tags$h4(HTML("For any breed, the recombination rate analysis discarded markers being putatively misplaced in the bovine genome assembly <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank' > <u>ARS-UCD1.2 assembly</u></a>.
                 For this, two sources of information were taken into account.")
    ),
    htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(title=tags$b(tags$b("1. General problematic regions")),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE,collapsed=TRUE,
          shiny::fluidRow(
            htmltools::br(),htmltools::br(),
            shiny::column(10, tags$h4("Currently, there are some problematic regions, that are recommended to remove regardless of the SNP panel used (please cite
                    ",HTML("<a href = 'https://doi.org/10.1111/age.13205' target='_blank'><u>Qanbari et al. 2022</u></a>)."))
            )
          ),
          shiny:: fluidRow(
            htmltools::br(),htmltools::br(),
            shiny::column(width=6,DT::DTOutput(ns("tableGenProReg"),width="auto",height="auto"),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
          )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(title = tags$b(tags$b("2. Markers identified as misplaced or residing in problematic regions in Holstein-DE")),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE,collapsed=TRUE,
          htmltools::br(),htmltools::br(),
          shiny::fluidRow(
            shiny::column(10,htmltools::tags$h4("Specifically, we identified a panel of 65 SNP markers with strong evidence for being misplaced based on the analysis of linkage disequilibrium and recombination rate in Holstein cattle."))
          ),
          htmltools::br(),htmltools::br(),
          shiny:: fluidRow(
            shiny::column(5,shiny::selectInput(inputId = ns('old_chr'), label = 'Select based on Old_Chr:', choices= c(" ","All",1,2,5:9,11,12, 14:16, 18, 21, 23, 26, 28, 29),multiple = FALSE, selected="All", selectize = FALSE)),
            shiny::column(5,shiny::selectInput(inputId = ns('new_chr'), label = 'Select based on New_Chr:', choices= c(" ","All",1,2,4:7,10,11,13:15,20,23,24,26,29),multiple = FALSE, selected=" ", selectize = FALSE)),
          ),
          shiny::fluidRow(
            shiny::column(width=12,DT::DTOutput(ns("tableMis"),width="auto",height="auto"),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
          ),
          shiny::fluidRow(
            shiny::column(width=10,shiny::checkboxInput(ns("checkbox22"), "Show/hide legend", TRUE), htmltools::p(id = "element22",
            "SNP: SNP name",htmltools::br(),
            "Old_Chr: Chromosome number according to",htmltools::tags$a(href="https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2", "ARS-UCD1.2",target="_blank",style='text-decoration-line: underline'),htmltools::br(),
            "Old_bp: Chromosomal position in bp according to",htmltools::tags$a(href="https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2", "ARS-UCD1.2",target="_blank",style='text-decoration-line: underline;'),htmltools::br(),
            "New_Chr: Chromosome number based on LD analysis. Negative values indicate inconclusive results and the analysis
                               continued on the original chromosome. ", htmltools::br(),
            "New_bp: Chromosomal position denotes the coordinate of the SNP to which the minimum recombination rate was achieved.",htmltools::br(),
            "Theta:  Minimum recombination rate on new chromosome",htmltools::br(),
             "Clear_recrate: Coded as 1 if the recombination rate < 0.01 at the new coordinate (bp), zero otherwise; in total 34",htmltools::br(),
            "BLAST_ProbeA_Chr.bp: The position of alternate match when the probe sequence of candidate SNP was aligned to the reference genome.
                                No entry implies a single hit coinciding with the map coordinate in ",
            htmltools::tags$a(href="https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2", "ARS-UCD1.2",target="_blank",style='text-decoration-line: underline;'),
            " or no hit.", htmltools::br(),
             "Candidate: Coded as 1 if the SNP was identified as candidate of misplacement based on estimates of recombination rate in the
                               original study", tags$a(href = "https://doi.org/10.1186/s12711-020-00593-z","Qanbari and Wittenburg (2020)", style='text-decoration-line: underline;'),"; in total 51",htmltools::br(),
            "Problematic region: Identifier of coherent problematic region; in total 15",htmltools::br(),
            "Start: Start  of position of problematic region",htmltools::br(),
            "End: End of position of problematic region",htmltools::br(),
             "n_heterozygous: Number of heterozygous sires based on which recombination rates were estimated"))
          )
     )
  ),
  shiny::fluidRow(
    shinydashboard::box(title = tags$b(tags$b("Additional markers identified as misplaced in all other breeds")),status="danger",width=12,
                        solidHeader = TRUE,collapsible = TRUE,collapsed=TRUE,
                       htmltools::br(),htmltools::br(),
                       shiny:: fluidRow(
                         shiny::column(width=5,shiny::selectInput(inputId = ns('breedSelect_misplaced'), label = 'Select breed(s):',choices= c("","All","Holstein-CH","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin","Unifying-CH"),
                                                                  selected=" ",multiple=TRUE)),
                         shiny::column(width=5,id=ns("show_hide_common"), checkboxInput(ns("common"),label="Show shared",value=FALSE))

                       ),
                        shiny::fluidRow(
                         shiny::column(width=5,DT::DTOutput(ns("tableMisAllBreeds"),width="auto",height="auto"),
                                        style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
                       )
                    )
    )
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

    output$tableGenProReg=DT::renderDT({
      DT::datatable(
      data2[,1:3], extensions = c("Buttons"), container=sketch2,rownames=FALSE,options = list(searching=FALSE,dom='Btfip',
                                                                                              columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                              buttons = list('pageLength', 'copy',list(extend='csv',title="General_problematic_regions"),list(extend='excel',title="General_problematic_regions")),
                                                                                              pagelength = 10, lengthMenu = list(c(10, 20,30, -1), c('10', '20','30','All'))))
    },server=FALSE)

    ##### all for table 2 - putative misplaced markers
    misplacedMarkers<-generalProblematicRegions<-NULL

    load(system.file("extdata",paste0("general/misplacedMarkers.Rdata"),package="CLARITY"))
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

   output$tableMis=DT::renderDT({
     DT::datatable(
     data, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom='Btfip',
                                                                                     columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                     buttons = c('pageLength', 'copy',list(extend='csv',title="Misplaced_markers-all"),list(extend='excel',title="Misplaced_markers-all")),
                                                                                     pagelength = 10, lengthMenu = list(ll1, ll2)))
   },server=FALSE)

    shiny::observeEvent(input$old_chr,
    {
      if(input$old_chr!=" ")
      {
        if(input$old_chr=="All")
        {
          data.mis=data
          ll1=c(10, 20,30, -1)
          ll2=c('10','20','30','All')
          title.old="Misplaced_markers-all"
        }
        else
        {
          data.mis=data[data$old_chr %in% as.numeric(input$old_chr), ]
          ll1=-1
          ll2='All'
          title.old=paste0("Misplaced_markers-oldBTA-",input$old_chr)
        }

      output$tableMis=DT::renderDT({
        DT::datatable(
          data.mis, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom='Btfip',
                                                                                       columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                       buttons = list('pageLength', 'copy',list(extend='csv',title=title.old),list(extend='excel',title=title.old)),
                                                                                       pagelength = 10, lengthMenu = list(ll1, ll2)))
      },server = FALSE)
      shiny::updateSelectInput(session,'new_chr',label="Select based on New_Chr:",choices= c("All",1,2,4:7,10,11,13:15,20,23,24,26,29),selected=" ")
      }
    })

   shiny::observeEvent(input$new_chr,
   {
     if(input$new_chr!=" ")
     {
      if(input$new_chr=="All")
      {
         data.mis=data
         ll1=c(10, 20,30, -1)
         ll2=c('10', '20','30','All')
         title.new="Misplaced_markers-all"
      }
      else
      {
         ll1= -1
         ll2='All'
         data.mis=data[abs(data$new_chr) %in% as.numeric(input$new_chr), ] # changed on 13.04.2023 and abs was taken out
         title.new=paste0("Misplaced_markers-newBTA-",input$new_chr)
      }

      output$tableMis=DT::renderDT({
        DT::datatable(
          data.mis, extensions = c("Buttons"), container=sketch,rownames=FALSE,options = list(searching=FALSE,dom= 'Btfip',
                                                                                      columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                      buttons = list('pageLength', 'copy',list(extend='csv',title=title.new),list(extend='excel',title=title.new)),
                                                                                      pagelength = 10, lengthMenu = list(ll1, ll2)))
       },server=FALSE)
      shiny::updateSelectInput(session,'old_chr',label="Select based on Old_Chr:",choices= c("All",1,2,5:9,11,12, 14:16, 18, 21, 23, 26, 28, 29),selected=" ")
     }
    })


   ## show / hide legend
   shiny::observe({
     shinyjs::toggle(id = "element22", condition = input$checkbox22,asis=TRUE)
   })


  #######  load and make table for all misplaced markers in all breeds
  ## part can be improved worked for now 14.04.2023
  tab<-Breed<-Name<-NULL

  load(system.file("extdata","general/misplaced_all_breeds.Rdata",package="CLARITY"))
  data3=tab


  thead<-tr<-th<-NULL

 sketch3 = htmltools::withTags(
    table( ##
      class = 'display',
      thead(
        tr(
          th(colspan=1,"Breed "),
          th(colspan=1,"Chromosome"),
          th(colspan=1,"Name"),
          th(colspan=1,"Position_Mbp")
        )
      )
    )
  )

  output$tableMisAllBreeds=DT::renderDT({
      DT::datatable(
      data3, extensions = c("Buttons"), container=sketch3,rownames=FALSE,
                            options = list(searching=FALSE,dom='Btfip',
                              columnDefs = list(list(className = 'dt-left', targets = "_all")),
                              buttons = list('pageLength', 'copy',list(extend='csv',title=paste0("Misplaced_markers-","All-breeds-except-HOL-DE")),list(extend='excel',title=paste0("Misplaced_markers-","All-breeds-except-HOL-DE"))),
                              pagelength = 10, lengthMenu = list(c(10, 30 ,50, -1), c('10', '20','30','All'))))
  },server=FALSE)


 shinyjs::hide(id="show_hide_common")

 shiny::observeEvent(input$breedSelect_misplaced,
   {
     req(input$breedSelect_misplaced)

     if(is.na(match("All",input$breedSelect_misplaced))==FALSE)
     {
       if(input$breedSelect_misplaced[1]!="All")
       {
          shiny::updateSelectInput(session,'breedSelect_misplaced',label="Select breed(s):",choices=c("All","Holstein-CH","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin","Unifying-CH"), selected="All")
          data4=data3
          shinyjs::hide(id="show_hide_common")
          title1.tab="Misplaced_markers-All-breeds-except-HOL-DE"
       }
       if(length(input$breedSelect_misplaced)==1)
       {
         shiny::updateSelectInput(session,'breedSelect_misplaced',label="Select breed(s):",choices=c("All","Holstein-CH","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin","Unifying-CH"), selected="All")
         data4=data3
         shinyjs::hide(id="show_hide_common")
         title1.tab="Misplaced_markers-All-breeds-except-HOL-DE"

       }
       if(input$breedSelect_misplaced[1]=="All" && length(input$breedSelect_misplaced)>1)
       {
         shiny::updateSelectInput(session,'breedSelect_misplaced',label="Select breed(s):",choices=c("All","Holstein-CH","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin","Unifying-CH"), selected=input$breedSelect_misplaced[2])
         data4=data3 %>% filter(Breed %in% input$breedSelect_misplaced[2])
         nam=c()
         nam=paste0(nam,"-",input$breedSelect_misplaced[2])
         title1.tab=paste0("Misplaced_markers",nam)
        }
     }
    if(is.na(match("All",input$breedSelect_misplaced))==TRUE)
    {
      data4=data3 %>% filter(Breed %in% input$breedSelect_misplaced)

      if(length(input$breedSelect_misplaced)>1) shinyjs::show(id="show_hide_common")
      if(length(input$breedSelect_misplaced)==1) shinyjs::hide(id="show_hide_common")
      nam=c()
      for(ik in 1:length(input$breedSelect_misplaced))nam=paste0(nam,"-",input$breedSelect_misplaced[ik])
      title1.tab=paste0("Misplaced_markers",nam)

      if(input$common==TRUE)
      {
        data4=data3 %>% filter(Breed %in% input$breedSelect_misplaced)
        data4=data4 %>% group_by(Name) %>%filter(n()==length(input$breedSelect_misplaced))
        nam=c()
        for(ik in 1:length(input$breedSelect_misplaced)) nam=paste0(nam,"-",input$breedSelect_misplaced[ik])
        title1.tab=paste0("Misplaced_markers",nam,"-shared")
      }
    }

     output$tableMisAllBreeds=DT::renderDT({
       DT::datatable(
         data4, extensions = c("Buttons"), container=sketch3,rownames=FALSE,
         options = list(searching=FALSE,dom='Btfip',
                        columnDefs = list(list(className = 'dt-left', targets = "_all")),
                        buttons = list('pageLength', 'copy',list(extend='csv',title=title1.tab),list(extend='excel',title=title1.tab)),
                        pagelength = 10, lengthMenu = list(c(10, 30 ,50, -1), c('10', '20','30','All'))))
     },server=FALSE)
   })


    shiny::observeEvent(input$common,
    {
      req(input$breedSelect_misplaced)

         if(input$common==TRUE)
         {
           data4=data3 %>% filter(Breed %in% input$breedSelect_misplaced)
           data4=data4 %>% group_by(Name) %>%filter(n()==length(input$breedSelect_misplaced))
           print(data4)
           nam=c()
           for(ik in 1:length(input$breedSelect_misplaced)) nam=paste0(nam,"-",input$breedSelect_misplaced[ik])
           title1.tab=paste0("Misplaced_markers",nam,"-shared")
         }
         else
         {
            data4=data3 %>% filter(Breed %in% input$breedSelect_misplaced)
            nam=c()
            for(ik in 1:length(input$breedSelect_misplaced)) nam=paste0(nam,"-",input$breedSelect_misplaced[ik])
            title1.tab=paste0("Misplaced_markers",nam)
       }

        output$tableMisAllBreeds=DT::renderDT({
         DT::datatable(
            data4, extensions = c("Buttons"), container=sketch3,rownames=FALSE,
            options = list(searching=FALSE,dom='Btfip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")),
                         buttons = list('pageLength', 'copy',list(extend='csv',title=title1.tab),list(extend='excel',title=title1.tab)),
                         pagelength = 10, lengthMenu = list(c(10, 30 ,50, -1), c('10', '20','30','All'))))
          },server=FALSE)

      })

  })

}

## To be copied in the UI
# mod_misplaced_ui("mod_misplaced_1")

## To be copied in the server
# mod_misplaced_server("mod_misplaced_1")
