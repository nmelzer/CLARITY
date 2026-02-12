# Module UI

#' @title mod_general_ui and mod_general_server
#' @description  A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed analysis".
#'
#' @details Within the module the data are prepared for the table and for plotting using the function \link{transformdata_general}. Moreover,
#' the plots are created using the function \link{scatterPlot_general}.
#' The function \link{create.output} is used to create the outcome for the correlation depending on selected approaches.
#' Finally, if the likelihood-based approach is selected, the quality of the likelihood approach is also shown here by means of a traffic light
#' (also termed as likelihood quality signal (\link{make_traffic_light})).
#'
#' @param id module id
#'
#' @rdname mod_general
#'
#' @keywords internal, general module
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @importFrom plotly plotlyOutput toWebGL renderPlotly
#' @rawNamespace import(shinyjs, except = runExample)
#' @importFrom htmlwidgets onRender
#'
#' @seealso
#' * \link{transformdata_general} \cr
#' * \link{scatterPlot_general} \cr
#' * \link{make_traffic_light} \cr
#' * \link{create.output}
#' @export

mod_general_ui<- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::br(),htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(title = tags$b("Genetic map summary"),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE, color="black",
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=11,DT::dataTableOutput(ns("table0")),style = "overflow-y: scroll;overflow-x: scroll;")),
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=10,shiny::checkboxInput(ns("checkbox"), "Show/hide legend", TRUE),
                                    shiny::uiOutput(outputId =  ns("element"))))
      )
    ),
    shiny::fluidRow(htmltools::br(),htmltools::br()),
    shiny::fluidRow(
      shinydashboard::box(title=tags$b("Genetic versus physical length of the bovine autosomes"), status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(
            shiny::column(width=8, htmltools::tags$b(htmltools::tags$h5("Interactive graphic: moving the mouse over the points will show the corresponding information.")))
          ),
          htmltools::br(),htmltools::br(),
          shiny::fluidRow(id=ns("showhideSignal"),
                          shiny::column(width=9,""),
                          shiny::column(width=2,htmltools::tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'), ## new insert 23.08.2023 - only show when lik
                                        shiny::plotOutput(ns("TrafficLight"),width="80%",height="auto") )
          ),
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=11,plotly::plotlyOutput(ns("plot1"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1"))),
          shiny::fluidRow(
                          htmltools::br(),
                          shiny::column(width=12, "cM: centiMorgan; Mbp: megabase pairs"),
                          htmltools::br(),
          ),
          shiny::fluidRow(
                          htmltools::br(),
                          shiny::column(12, htmltools::h4(shiny::uiOutput(ns("calc.approaches"))))
         )
      )
    )
  )
}

# Module Server
#' @rdname mod_general
#'
#' @param filter character contains the selected chromosome
#' @param breed.select character contains the name of the selected breed
#' @param make.traff.light object containing the ggplot for the likelihood quality signal (\link{make_traffic_light})
#' @param approach.info data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#' @param dt data frame contains the genetic map summary for the selected breed
#' @param add.approach.names character containing the concatenated approach abbreviations and is used as an addition to the file name
#' @param figure_rendered_g reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed analysis" sidebar elements.
#'
#' @keywords internal, general module
#' @export

mod_general_server=function(id, filter,breed.select,make.traff.light,approach.info,dt,add.approach.names,figure_rendered_g){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    ## if the likelihood approach is selected than likelihood-quality signal is rendered and shown
    if(match("Likelihood_male",approach.info$Approach,0)!=0){
      shinyjs::show("showhideSignal")
      output$TrafficLight <- shiny::renderPlot({
        make.traff.light()
      }, width=80, height=30)
    }
    else shinyjs::hide("showhideSignal")

    ## determine columns which have to be used from the genetic map summary based on selected approaches
    use<-unlist(sapply(1:nrow(approach.info), function(x) (grep(paste0(approach.info$Abbreviation[x],"_"),colnames(dt)))))
    dt=dt[,c(1:5,use)]

    #### Table
    ## create header for table
    sketch<-create_table_header1(approach.info)

    ## create legend for table
    explanation<- create.legend.table(approach.info=approach.info)

    title1=paste0(breed.select,"_genetic_map_summary-BTA-",filter,"-",add.approach.names)

    if(filter=="All")dt.table<-dt
    else dt.table<-dt[dt$Chr%in%filter,]

    output$table0=DT::renderDataTable({
      DT::datatable(dt.table,extensions = c("Buttons"), container=sketch, rownames=FALSE,
                    options = list(searching=FALSE, columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                    buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                    pagelength = 10, lengthMenu = list(c(5,10, 15, -1), c('5','10', '15','All'))))
    }, server = FALSE)

    ## show / hide legend for the table
    output$element <-shiny::renderUI({
      if (input$checkbox) {
        htmltools::p(id = "element",HTML(explanation))
      }else{
        ""
      }
    })

    #### Plot
    ## transform data for the scatter plot
    data.trans=transformdata_general(data1 =dt[-nrow(dt),],approach.info=approach.info,filter=filter)# take out the last line in dt, which contains the total number of markers

    ## react to scatter plot (see output$plot1) when it is fully rendered
    ## and update the figure_rendered_g() to enable the "Breed analysis" sidebar elements
    shiny::observeEvent(input$plot1_rendered_appr, {
      figure_rendered_g(TRUE)
    })

    ## render scatter plot and send signal when plot is fully rendered
    output$plot1 <- plotly::renderPlotly({
      if(filter!="All")file.name=paste0(breed.select,"-genetic_vs_physical_length_BTA-",filter,"-",add.approach.names)
      else file.name=paste0(breed.select,"-genetic_vs_physical_length-all-",add.approach.names)

      plot<-scatterPlot_general(dat=data.trans,name.file=file.name)
      plot%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_appr', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
    })

    #### render UI output - correlations for selected approaches
    output$calc.approaches <- shiny::renderUI({
       output = htmltools::HTML(create.output(data.trans=data.trans,approach.info=approach.info))
       output
    })
  })
}

## To be copied in the UI
# mod_general_ui("mod_general_1")

## To be copied in the server
# mod_general_server("mod_general_1")
