# Module UI

#' @title mod_general_ui and mod_general_server
#' @description  A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed analysis".
#'
#' @details Within the module the data are prepared for plotting using the function \link{transformdata_general}. Moreover,
#' the plots are created using the function \link{scatterPlot_general_all} or \link{scatterPlot_general_selected} depending on the user selected chromosome.
#' Corresponding hovering information and style are generated using \link{hovering}.
#'
#' @param id module id
#'
#' @rdname mod_general
#'
#' @keywords internal, general module
#' @export
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom stats cor
#' @import htmltools
#' @rawNamespace import(shinyjs, except = runExample)
#'
#' @seealso \link{transformdata_general},\link{scatterPlot_general_all}, \link{scatterPlot_general_selected} and \link{hovering}

mod_general_ui<- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::br(),htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(title = tags$b("Genetic map summary"),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE, color="black",
          htmltools::br(),
          shiny:: fluidRow(shiny::column(width=10,DT::dataTableOutput(ns("table0")),style = "overflow-y: scroll;overflow-x: scroll;"),
          ),
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=10,shiny::checkboxInput(ns("checkbox"), "Show/hide legend", TRUE), ## tried to make the HTML as htmlOutput but it does not work - must be checked later for a better solution
                                        htmltools::p(id = "element",
                            HTML("Chr: chromosome<br>nSNP: number of SNPs<br> bp: chromosome length in base pairs<br>
                                  Gap: maximum gap size between pairs of adjacent markers in bp<br>
                                  Space: inter-marker space in kilobase (kb) <br> nRec: number of cross-overs detected <br>
                                  D(M): genetic length in Morgan estimated based on deterministic approach <br>
                                  L(M): genetic length in Morgan estimated with the likelihood-based approach <br>
                                  D(cM/Mb): centiMorgan per megabase pair for the deterministic approach <br>
                                  L(cM/Mb): centiMorgan per megabase pair for the likelihood-based approach")))

      ))
    ),
    shiny::fluidRow(htmltools::br(),htmltools::br()),
    shiny::fluidRow(
      shinydashboard::box(title=tags$b("Genetic versus physical length of the bovine autosomes"), status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(shiny::column(width=8,
                   tags$b(tags$h5("Interactive graphic: moving the mouse over the points will show the corresponding information.")))
          ),
          htmltools::br(),htmltools::br(),
          shiny::fluidRow(shiny::column(width=12,shiny::downloadButton(outputId=ns("DownloadPlot"),"Save plot"))),
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=10,
                   shiny::plotOutput(ns("plot1"),hover = hoverOpts(ns("plot_hover1"),delay = 100, delayType = "debounce"),width="100%",height="auto"),
                   shiny::uiOutput(ns("hover_info1")))
          ),
          shiny::fluidRow(
            htmltools::br(),
            shiny::column(width=12, "cM: centiMorgan; Mbp: megabase pairs"),
            htmltools::br(),
          ),
          shiny::fluidRow(
            htmltools::br(),
            shiny::column(12, h4("The Pearson correlation (r) between physical and genetic length  #
                           was r=", shiny::textOutput(ns("detapproach"),inline=T)," for the ",tags$a(href="#","deterministic approach", onclick = "openTab('methodology')"), " and
                           r=",shiny::textOutput(ns("likapproach"),inline=T)," for the ",tags$a(href="#","likelihood-based approach", onclick = "openTab('methodology')"),"."))
          )
      )
    )
  )
}


# Module Server
#' @rdname mod_general
#'
#' @param filter selected chromosome
#' @param breed.select name of the selected breed
#' @param color.breed.dl vector containing the predefined colors
#'
#' @keywords internal, general module
#' @export

mod_general_server=function(id, filter,breed.select,color.breed.dl){
  shiny::moduleServer(id, function(input, output, session){

  ns <- session$ns

  genetic_map_summary<-NULL
  load(system.file("extdata", paste0(breed.select,"/genetic_map_summary.Rdata"),package="CLARITY"))
  dt=as.data.frame(genetic_map_summary)

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
        th(colspan = 2,htmltools::tags$a(href="#","Deterministic approach", onclick = "openTab('methodology')") ),
        th(colspan = 2, htmltools::tags$a(href="#","Likelihood-based approach",onclick = "openTab('methodology')"))
      ),
      tr(
        th(colspan=1,"Chr"),
        th(colspan=1,"nSNP"),
        th(colspan=1,"bp"),
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

  ### table preparation
  if(filter=="All"){
    title1=paste0(breed.select,"_genetic_map_summary-all")

    output$table0=DT::renderDataTable({
      DT::datatable(dt,extensions = c("Buttons"), container=sketch, rownames=FALSE, options = list(searching=FALSE,dom='Bfrtip',
                    columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                    pagelength = 10, lengthMenu = list(c(5,10, 15, -1), c('5','10', '15','All'))))
    }, server = FALSE)
  }
  else
  {
    fg2=dt[dt$Chr%in%filter,]
    title1=paste0(breed.select,"_genetic_map_summary-BTA-",filter)

    output$table0=DT::renderDataTable({
      DT::datatable(
      fg2,extensions = c("Buttons"), container=sketch, rownames=FALSE, options = list(searching = FALSE,columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                    dom = 'Bt', buttons = list('copy',list(extend='csv',title=title1),list(extend='excel',title=title1))))
    }, server = FALSE)
  }

  ## show / hide legend for the table
  shiny::observe({
    shinyjs::toggle(id = "element", condition = input$checkbox,asis=TRUE)
  })

  ######################## All regarding plots
  data=dt[-dim(dt)[1],] # take out last line which contains the total number of markers

  ## transform data for the plot
  data2=transformdata_general(data1 = cbind(data[,3],data[,7]))
  data3=transformdata_general(data1 = cbind(data[,3],data[,9]))

  ## combine data for the plot
  colo2=c(rep(color.breed.dl[1],nrow(data2)),rep(color.breed.dl[2],nrow(data3)))

  Approach=c(rep("deterministic",nrow(data2)),rep("likelihood",nrow(data3)))
  data5=rbind(data2,data3)
  data5=cbind(data5,colo2,Approach)

  ## make plots
  if(filter=="All"){
    file.name=paste0(breed.select,"-genetic_vs_physical_length.png")

    tt=scatterPlot_general_all(dat=data5,colo=color.breed.dl)
    InputPlot2=shiny::reactive(tt)

    output$plot1 <- shiny::renderPlot({
      InputPlot2()
      },height=600

    )
  }

  if(filter!="All"){
    tt=scatterPlot_general_selected(dat=data5,fil=as.numeric(as.character(filter)),colo=color.breed.dl)
    InputPlot2=shiny::reactive(tt)

    file.name=paste0(breed.select,"-genetic_vs_physical_length_BTA-",filter,"_all.png")

    output$plot1 <- shiny::renderPlot({
      InputPlot2()
    },height=600
    )
  }

  ## add hover
  output$hover_info1 <- shiny::renderUI({
    if(!is.null(input$plot_hover1)){
      hovering(dat1=data5,hover=input$plot_hover1,what=1)
    }
  })

  ## render correlation coefficient for deterministic approach
  output$detapproach<-shiny::renderText({
    zz=paste0(round(stats::cor(data2[,1],data2[,2],method="pearson"),3))
  })

  ## render correlation coefficient for likelihood approach
  output$likapproach<-shiny::renderText({
    zz=round(stats::cor(data3[,1],data3[,2],method="pearson"),3)
  })

  ## handle download plot
  output$DownloadPlot <- shiny::downloadHandler(
    filename = file.name,
    content = function(file) {
      ggsave(file, plot = InputPlot2(), device = "png",width=20,height=10,units="in",dpi=300)
    }
  )
})
}


## To be copied in the UI
# mod_general_ui("mod_general_1")

## To be copied in the server
# mod_general_server("mod_general_1")
