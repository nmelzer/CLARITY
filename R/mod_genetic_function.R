# Module UI

#' @title mod_genetic_function_ui and mod_genetic_function_server
#' @description  A shiny module to generate the outcome for the tabpanel "Genetic-map functions" for the sidebar "Breed analysis".
#'
#' @details The module uses the function \link{makePlot_genetic_function} for plotting.
#'  Moreover, the quality of the likelihood approach is shown here by means of a traffic light (also termed as likelihood quality signal ((\link{make_traffic_light}))).
#'
#' @param id module id
#'
#' @rdname mod_genetic_function
#' @keywords internal, genetic function
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @importFrom htmlwidgets onRender
#' @rawNamespace import(shinyjs, except = runExample)
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @seealso
#' * \link{makePlot_genetic_function} \cr
#' * \link{make_traffic_light}
#' @export

mod_genetic_function_ui=function(id)
{
    ns=shiny::NS(id)

    shiny::tagList(
      useShinyjs(),
      shinyjs::hidden(
        shiny::fluidRow(id=ns("no.other.method"),
                      htmltools::br(),htmltools::br(),
                      shiny::column(width=12,htmltools::tags$h3(htmltools::HTML("<span style=color:red;margin-left: 6px;> Genetic-map functions are only available for the likelihood-based approach.</span>")))
      )),
      htmltools::br(),
      shinyjs::hidden(
        shiny::fluidRow(id=ns("only.likelihood1"),
            shinydashboard::box(title= tags$b("Interactive graphical visualization"),status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,

                ### show output when all chromosomes are selected
                shinyjs::hidden(shiny::fluidRow(id=ns("all_chromosome"),
                       shiny::column(12,"Only two chromosomes can be chosen and approximately 200,000 data points are plotted."),
                       shiny::column(width=8),
                       shiny::column(width=2,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                       plotOutput(ns("TrafficLight2"),width="80%",height="auto") ),
                       shiny::column(width = 12,style="padding-top:20px", ""),
                       shiny::column(12, "Remove a genetic map function in the figure by clicking on the corresponding name in the legend."),
                       shiny::column(width = 12,style="padding-top:20px", ""),
                       shiny::column(width=6, shiny::selectInput(inputId=ns('GenetMapchr1'),label = "Select chromosome",choices=c(seq(1,29,1)),selected=18,multiple=FALSE,selectize=FALSE),
                                    plotly::plotlyOutput(ns("genetic_functions3"))%>% shinycssloaders::withSpinner(color="#0dc5c1")),
                       shiny::column(width=6, shiny::selectInput(inputId=ns('GenetMapchr2'), label = "Select chromosome",choices=c(seq(1,29,1)),selected=28,multiple=FALSE,selectize=FALSE),
                            plotly::plotlyOutput(ns("genetic_functions4"))%>% shinycssloaders::withSpinner(color="#0dc5c1"))
                )),
                ### show output when specific chromosome is selected
                shinyjs::hidden(
                      shiny::fluidRow(id=ns("single_chromosome"),
                          shiny::column(12, "Only about 200,000 data points are plotted."),
                          shiny::column(width=8),
                          shiny::column(width=3,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                     shiny::plotOutput(ns("TrafficLight"),width="80%",height="auto")),
                          shiny::column(width = 12,style="padding-top:20px", ""),
                          shiny::column(8, "Remove a genetic-map function in the figure by clicking on the corresponding name in the legend."),
                          shiny::column(width = 12,style="padding-top:20px", ""),
                          shiny::column(width=8,plotly::plotlyOutput(ns("genetic_functions5"),width="auto",height="auto")%>% withSpinner(color="#0dc5c1")),
               ))
            )
      )),
      htmltools::br(),
      shinyjs::hidden(
          shiny::fluidRow(id=ns("only.likelihood2"),
              shinydashboard::box(title = tags$b("Genetic-map functions"),status="danger",width=12, solidHeader = TRUE,collapsible = TRUE, collapsed=FALSE,
                    shiny::column(width = 12,style="padding-top:30px", ""),
                    shiny::fluidRow(
                        shiny::column(width=5, "Genetic-map function that fits best is highlighed in orange.")),
                    shiny::column(width = 12,style="padding-top:30px", ""),
                    shiny::fluidRow(
                      shiny::column(width=8,DT::dataTableOutput(ns("tableBestmapFunction")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
                    shiny::fluidRow(
                      shiny::column(width=5,"MSE - mean squared error"))
              )
      ))
    )
}


# Module Server
#' @rdname mod_genetic_function
#' @param filter character contains the selected chromosome
#' @param breed.select character contains the name of the selected breed
#' @param make.traff.light object containing the ggplot for the likelihood quality signal(\link{make_traffic_light})
#' @param figure_rendered_gf reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed analysis" sidebar elements.
#'
#' @export
#' @keywords internal
mod_genetic_function_server=function(id, filter,breed.select,make.traff.light,approach.info,figure_rendered_gf){
  shiny::moduleServer(id,function(input, output, session){
    ns <- session$ns

    ## initialize
    out<-NULL

    ## A delay was added to ensure the DOM is ready before executing shinyjs show and hide functions.
    ## Initialize which UI parts are shown.
    shinyjs::delay(3,
         if(match("Likelihood_male", approach.info$Approach, 0) == 0){ ## likelihood approach is not selected
                    shinyjs::show(id = ns("no.other.method"), asis = TRUE)
                    shinyjs::hide(id = ns("only.likelihood1"), asis = TRUE)
                    shinyjs::hide(id = ns("only.likelihood2"), asis = TRUE)
         }else
         {
                    shinyjs::hide(id = ns("no.other.method"), asis = TRUE)
                    shinyjs::show(id = ns("only.likelihood1"), asis = TRUE)
                    shinyjs::show(id = ns("only.likelihood2"), asis = TRUE)
                    if(filter == "All"){
                      shinyjs::show(id = ns("all_chromosome"), asis = TRUE)
                      shinyjs::hide(id = ns("single_chromosome"), asis = TRUE)
                    }else
                    {
                      shinyjs::show(id = ns("single_chromosome"), asis = TRUE)
                      shinyjs::hide(id = ns("all_chromosome"), asis = TRUE)
                    }
         }
    )

    ## React to when the first plot for all chromosome is fully rendered (see output$genetic_functions3) and
    ## update the figure_rendered_gf() to enable the "Breed analysis" sidebar elements
    observeEvent(input$genetic_functions3_rendered_plot1, {
      figure_rendered_gf(TRUE)
    })

    ## React to when the second plot for all chromosome is fully rendered (see output$genetic_functions4) and
    ## update the figure_rendered_gf() to enable the "Breed analysis" sidebar elements
    observeEvent(input$genetic_functions4_rendered_plot2, {
      figure_rendered_gf(TRUE)
    })

    ## React to when plot for specific chromosome is fully rendered (see output$genetic_functions5) and
    ## update the figure_rendered_gf() to enable the "Breed analysis" sidebar elements
    observeEvent(input$genetic_functions5_rendered_breed, {
      figure_rendered_gf(TRUE)
    })


    if(match("Likelihood_male",approach.info$Approach,0)==0) ## likelihood approach not selected
    {
      figure_rendered_gf(TRUE) #  e enable the "Breed analysis" sidebar elements
    }else{

      # read.in data and prepare data
      load(system.file("extdata", paste0(breed.select,"/bestmapfun.Rdata"),package="CLARITY"))
      out.new=out

      use=c(2,4,6,8)
      out.new[,use]=round(out.new[,use],7)

      ## create matrix with "0" for coloring the columns in table
      pp=matrix(0,dim(out)[1],dim(out.new)[2])
      colnames(pp)=colnames(out.new)


      ## find the the best genetic-map function - it is necessary for coloring in the corresponding tables
      for(i in 1:dim(out.new)[1])
      {
        p=which(out.new[i,use]==min(out.new[i,use]))
        out.new[i,use[p]]=paste0(out.new[i,use[p]],"*") ## for table
        pp[i,use[p]]=out.new[i,use[p]] ## for coloring table
      }
      out.2<-shiny::reactive({as.data.frame(out.new)})
      pp=as.data.frame(pp)

      ## Table header with internal link to methodology --
      sketch1<-create_table_header4()

      ### header for table output csv and excel
      names= paste0("'",colnames(out.new),"'",collapse=",")
      names2=paste0("[",names,"];",collapse=",")

      ## create the header for csv or EXCEL output
      createHeader <- c(
        "function(data, columnIdx) {",
        "  var headers =",names2,
        "  return headers[columnIdx];",
        "}"
      )


      if(filter=="All")
      {
        ## render traffic light
        output$TrafficLight2 <- renderPlot({
          make.traff.light()
        }, width=80, height=30)


        #### react to the input of the first chromosome selection: load, prepare and plot data and send signal when plot is fully rendered
        shiny::observeEvent(input$GenetMapchr1,{
          figure_rendered_gf(FALSE)

          ## data
          chr <-input$GenetMapchr1
          store=NULL; outcome1=list()
          load(system.file("extdata",paste0(breed.select,"/curve-short-",chr,".Rdata"),package="CLARITY"))
          outcome1= store

          ## plot
          file.name=paste0(breed.select,"_genetic-map-funcions_BTA-",chr)
          output$genetic_functions3 <-plotly::renderPlotly({
            pp<-makePlot_genetic_function(chromo=chr,df.list=outcome1,name.file=file.name,breed.select=breed.select)
            pp%>% plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_plot1', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
          })%>%shiny::bindCache(breed.select,chr,"likelihood",cache="app")
        })

        #### react to the input of the second chromosome selection: load, prepare and plot data and send signal when plot is fully rendered
        shiny::observeEvent(input$GenetMapchr2,{
          figure_rendered_gf(FALSE)

          ## data
          chr=input$GenetMapchr2
          store=NULL; outcome2=list()
          load(system.file("extdata",paste0(breed.select,"/curve-short-",chr,".Rdata"),package="CLARITY"))
          outcome2= store

          ## plot
          file.name=paste0(breed.select,"_genetic-map-funcions_BTA-",chr)
          output$genetic_functions4 <-plotly::renderPlotly({
              pp<-makePlot_genetic_function(chromo=chr,df.list=outcome2,name.file=file.name,breed.select=breed.select)
              pp%>% plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_plot2', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
          })%>%shiny::bindCache(breed.select,chr,"likelihood",cache="app")
       })
        table.show <-out.2
      }
      else  ## when specific chromosome is selected
      {
        ## render traffic light
        output$TrafficLight <- renderPlot({
          make.traff.light()
        }, width=80, height=30)

        ## data
        store<-NULL
        load(system.file("extdata",paste0(breed.select,"/curve-short-",filter,".Rdata"),package="CLARITY"))
        outcome<-shiny::reactive({store})

        table.show <- shiny::reactive({out.2()[as.numeric(filter),]})

        figure_rendered_gf(FALSE)

        file.name=paste0(breed.select,"_genetic-map-funcions_BTA-",filter)

        ## create and render plot
        output$genetic_functions5 <-plotly::renderPlotly({
          pp<-makePlot_genetic_function(chromo=filter,df.list=outcome(),name.file=file.name,breed.select=breed.select)
          pp%>% plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
        })%>%shiny::bindCache(breed.select,filter,"likelihood",cache="app")
      }

      ## render table for specific chromosome and when all selected
      output$tableBestmapFunction=DT::renderDataTable({
        title1=paste0(breed.select,"_genetic-map-functions_BTA-",filter)
        DT::datatable(table.show(),filter="none", container=sketch1,options=list(dom='Bt',buttons = list('pageLength','copy',
                                                                                                         list(extend='csv',title=title1, exportOptions = list(columns = ":visible",format = list(header = DT::JS(createHeader)))),
                                                                                                         list(extend='excel',title=title1,exportOptions = list(columns = ":visible",format = list(header = DT::JS(createHeader))))),
                                                                                 pagelength = 10,lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),
                      escape=FALSE,rownames=FALSE)%>%
          DT::formatStyle(if(length(pp[which(pp$Haldane_scaled_mse!=0),2])!=0)"Haldane_scaled_mse",backgroundColor=DT::styleEqual(pp[which(pp[,2]!=0),2], rep("orange",length(pp[which(pp[,2]!=0),2]))))%>%
          DT::formatStyle(if(length(pp[which(pp$Rao_mse!=0),4])!=0)"Rao_mse",backgroundColor=DT::styleEqual(pp[which(pp[,4]!=0),4], rep("orange",length(pp[which(pp[,4]!=0),4]))))%>%
          DT::formatStyle(if(length(pp[which(pp$Felsenstein_mse!=0),6])!=0)"Felsenstein_mse",backgroundColor=DT::styleEqual(pp[which(pp[,6]!=0),6], rep("orange",length(pp[which(pp[,6]!=0),6])))) %>%
          DT::formatStyle(if(length(pp[which(pp$Karlin_mse!=0),8])!=0)"Karlin_mse",backgroundColor=DT::styleEqual(pp[which(pp[,8]!=0),8], rep("orange",length(pp[which(pp[,8]!=0),8]))))
      }, server = FALSE)

    }
  })
}## End server

## To be copied in the UI
# mod_genetic_function_ui("mod_genetic_function_1")

## To be copied in the server
# mod_genetic_function_server("mod_genetic_function_1")



