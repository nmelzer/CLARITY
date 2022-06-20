# Module UI

#' @title mod_general_ui and mod_general_server
#' @description  A shiny Module to generate the outcome for the tabpanel General information.
#'
#' @param id shiny id
#'
#' @rdname mod_general
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import shiny
#' @import shinydashboard
#' @importFrom stats cor
#' @rawNamespace import(shinyjs, except = runExample)
#' @note The module uses the mod_general_fct_functions.R and the fct_general_hovering.R.

mod_general_ui<- function(id){
  ns <- NS(id)

  tagList(
    br(),br(),
    fluidRow(
      box(title = tags$b("Genetic map summary"),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE, color="black",
          br(),
          fluidRow(column(width=10,DT::dataTableOutput(ns("table0")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
          ),
          br(),
          fluidRow(column(width=10,checkboxInput(ns("checkbox"), "Show/hide legend", TRUE), ## tried to make the HTML as htmlOutput but it does not work - must be checked later for a better solution
                          p(id = "element",
                            HTML("Chr: chromosome<br>nSNP: number of SNPs<br> BP: chromosome length in base pairs<br>
                                  Gap: maximum gap size between pairs of adjacent markers in base pair (bp)<br>
                                  Space: inter-marker space in kilobase (kb) <br> nRec: number of cross-overs detected <br>
                                  D(M): genetic length in Morgan estimated based on deterministic approach <br>
                                  L(M): genetic length in Morgan estimated with the likelihood-based approach <br>
                                  D(cM/Mb): centiMorgan per megabase pair for the deterministic approach <br>
                                  L(cM/Mb): centiMorgan per megabase pair for the likelihood approach")))
         )
      )
    ),
    fluidRow(br(),br()),
    fluidRow(
      box(title=tags$b("Genetic versus physical length of the bovine autosomes"), status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
          fluidRow(column(width=8,
                   tags$b(tags$h5("Interactive graphic: moving the mouse over the points will show the corresponding information.")))
          ),
          br(),br(),
          ## layout for the two plots -  next to each others
          fluidRow(
            column(width=10,
                   plotOutput(ns("plot1"),hover = hoverOpts(ns("plot_hover2"), delay = 100, delayType = "debounce"),width="100%",height="100%"),
                   uiOutput(ns("hover_info2"))),
            column(width=2,downloadButton(outputId=ns("DownloadPlot"),"Save Plot"))
          ),
          fluidRow(
            br(),
            column(width=12, "cM: centiMorgan; Mbp: megabase pairs"),
            br(),
          ),
          fluidRow(
            br(),
            column(12, h4("The Pearson correlation (r\U00B2) between physical and genetic length  #
                           was r\U00B2=", textOutput(ns("detapproach"),inline=T)," for the ",tags$a(href="#","deterministic approach", onclick = "openTab('methodology')"), " and
                           r\U00B2=",textOutput(ns("likapproach"),inline=T)," for the ",tags$a(href="#","likelihood approach approach", onclick = "openTab('methodology')"),".")),
            br(),
          )
      )
    )
  )
}


# Module Server
#' @title mod_general_server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_general
#'
#' @keywords internal
#' @export
#' @import htmltools
#' @details Both use mod_general_fct_functions were all functions are included for the graphical outputs.

mod_general_server=function(input, output, session, filter){

  ns <- session$ns

  genetic_map_summary=NULL
  load(system.file("extdata", "genetic_map_summary.Rdata",package="CLARITY"))
  dt=genetic_map_summary


  ####################
  # a custom table container
  tr<-thead<-th<-NULL
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=1,""),
        th(colspan=5,"Physical Map"),
        th(colspan=4,"Genetic Map"),
      ),
      tr(
        th(colspan=1,""),
        th(colspan=5,""),
        th(colspan = 2,tags$a(href="#","Deterministic approach", onclick = "openTab('methodology')") ), ## previous - help
        th(colspan = 2, tags$a(href="#","Likelihood approach",onclick = "openTab('methodology')")) ### previous - help
      ),
      tr(
        th(colspan=1,"Chr"),
        th(colspan=1,"nSNP"),
        th(colspan=1,"BP"),
        th(colspan=1,"Gap"),
        th(colspan=1,"Space"),
        th(colspan=1,"nRec"),
        th(colspan=1,"D(M)"),
        th(colspan=1,"D(cM/Mb)"),
        th(colspan=1,"L(M)"),
        th(colspan=1,"L(cM/Mb)")
      )
    )
  ))

  if(filter=="All"){
    output$table0=DT::renderDataTable(
     dt,extensions = c("Buttons"), container=sketch, rownames=FALSE, options = list(searching=FALSE,dom='Bfrtip',
                    columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf'),
                    pagelength = 10, lengthMenu = list(c(5,10, 15, -1), c('5','10', '15','All')))
    )
  }
  else
  {
    fg=dt
    fg2=fg[which(fg$Chr==filter),]
    output$table0=DT::renderDataTable(
      fg2,extensions = c("Buttons"), container=sketch, rownames=FALSE, options = list(searching = FALSE,columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                    dom = 'Bt', buttons = c('copy', 'csv', 'excel', 'pdf'))
    )
  }

  ## show / hide legend
  observe({
    toggle(id = "element", condition = input$checkbox,asis=TRUE)
  })

  ######################## All regarding plots
  data=dt[-dim(dt)[1],]

  ## set color for the plot
  colo=c("dodgerblue2","cadetblue3")

  ## transform data for the plot
  data2=transformdata(data1 = cbind(data[,3],data[,7]))
  data3=transformdata(data1 = cbind(data[,3],data[,9]))

  ## combine data for the plot
  colo2=c(rep(colo[1],nrow(data2)),rep(colo[2],nrow(data3)))
  Approach=c(rep("deterministic",nrow(data2)),rep("likelihood",nrow(data3)))
  data5=rbind(data2,data3)
  data5=cbind(data5,colo2,Approach)

  ## perform plots
  if(filter=="All"){
    tt=scatterPlot(dat=data5,colo=colo)
    InputPlot2=shiny::reactive(tt)

    file.name="genetic_vs_physical_length.png"

    output$plot1 <- shiny::renderPlot({
      scatterPlot(dat=data5,colo=colo)
      },height=600 #, height = 800 ## here sizes can be changed of figures
    )
  }

  if(filter!="All"){
    tt=scatterPlot0(dat=data5,fil=as.numeric(as.character(filter)),colo=colo)
    InputPlot2=shiny::reactive(tt)

    file.name=paste0("genetic_vs_physical_length_Chr",filter,".png")

    output$plot1 <- shiny::renderPlot({
     scatterPlot0(dat=data5,fil=as.numeric(as.character(filter)),colo=colo)
    },height=600 #,
    )
  }

  ## add hover
  output$hover_info2 <- shiny::renderUI({
    if(!is.null(input$plot_hover2)){
      hovering(dat1=data5,hover=input$plot_hover2,what=1)
    }})

  ## render correlation coefficient for deterministic approach
  output$detapproach<-shiny::renderText({
    zz=paste0(round(cor(data2[,1],data2[,2],method="pearson"),3))
  })

  ## render correlation coefficient for likelihood approach
  output$likapproach<-shiny::renderText({
    zz=round(cor(data3[,1],data3[,2],method="pearson"),3)
  })

  ## handle download plot
  output$DownloadPlot <- shiny::downloadHandler(
    filename = file.name,
    content = function(file) {
      ggsave(file, plot = InputPlot2(), device = "png",width=20,height=10,units="in",dpi=300)
    }
  )
}


## To be copied in the UI
# mod_general_ui("mod_general_1")

## To be copied in the server
# callModule(mod_general_server,"mod_general_1")
