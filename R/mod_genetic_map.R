# Module UI

#' @title mod_genetic_map_ui and mod_genetic_map_server
#' @description  A shiny module to generate the outcome for the tabpanel "Genetic map" for the sidebar "Breed analysis".
#'
#' @details Independent from the user choice regarding chromosome selection, the data are prepared for the plot
#' using the function \link{transformdata_genetic_map}. Depending on the user's selection, when a specific chromosome is chosen, the function \link{makePlot_geneticMaps} is used to create the plot,
#' otherwise the function \link{makePlot_all_geneticMaps} is used to create the plot. In addition, when all chromosome is selected than the corresponding plot
#' is cached to increase the app performance.
#' Finally, the quality of the likelihood approach is also shown here by means of a traffic light
#' (also termed as likelihood quality signal (\link{make_traffic_light})).
#'
#' @param id module id
#'
#' @rdname mod_genetic_map
#'
#' @keywords internal, genetic map
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @rawNamespace import(shinyjs, except = runExample)
#' @import htmltools
#' @importFrom ggplot2 ggsave
#' @importFrom plotly plotlyOutput renderPlotly toWebGL ggplotly
#' @importFrom writexl write_xlsx
#' @importFrom htmlwidgets onRender
#'
#' @seealso
#' * \link{transformdata_genetic_map} \cr
#' * \link{makePlot_geneticMaps} \cr
#' * \link{makePlot_all_geneticMaps} \cr
#' * \link{make_traffic_light}
#' @export
#'
mod_genetic_map_ui<-function(id)
{
  ns<-shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    htmltools::br(),htmltools::br(),
    shinyjs::hidden(
      shiny::fluidRow(id=ns("output1"),  ## when specific chromosome is selected
             shinydashboard::box(title= tags$b("Interactive graphical visualization: Genetic vs. physical distance"),status="danger",width=12,
                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                 shiny::column(width=9,""),
                 shinyjs::hidden(
                    shiny::column(width=3,id=ns("showhideSignal2"),
                            tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                            shiny::plotOutput(ns("TrafficLight"),width="80%",height="auto" ))
                 ),
                 shiny::column(width=11,plotly::plotlyOutput(ns("genetic"),width="100%",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1"))
              ),
              htmltools::br(),htmltools::br(),
              shinydashboard::box(title= tags$b("Genetic distance in selected interval"),status="danger",width=12,
                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                 shiny::fluidRow(
                   shiny::column(width=12,"Use the slider to define the range of interest and then click on button",em('Apply selected interval'),". This accordingly changes the figure and table including corresponding filenames. Use the button", em('Reset to all')," to obtain corresponding results for whole chromosome again."),
                   shiny::column(width = 12,style="padding-top:30px", ""),
                   shiny::column(width=10,shiny::uiOutput(ns("sliderRangeMap"))),shiny::column(width=2,""),
                   shiny::column(width = 12,style="padding-top:30px", ""),
                   shiny::column(width=12,shiny::actionButton(ns("ButtonshowRangeMap"),"Apply selected interval"),
                                 shinyjs::hidden(shiny::actionButton(ns("ButtonAll"),"Reset to all")))
                 ),
                 htmltools::br(),
                 htmltools::hr(style = "border-top: 1px solid #68838B;"),htmltools::br(),htmltools::br(),
                 shiny::fluidRow(shiny::column(width=11,DT::dataTableOutput(outputId=ns("TableMaps")),style = "overflow-y: scroll;overflow-x: scroll;"))
              )
    )),
    shinyjs::hidden(
      shiny::fluidRow(id=ns("output2"),  ## when all chromosome is selected
                 shinyjs::hidden(
                    shiny::column(width=1,shiny::actionButton(inputId=ns("help_Button"),label="not shown"))
                  ),
                  shiny::column(width=2,shiny::downloadButton(outputId=ns("download11"),label="Save figure",class="butt1")),
                  shiny::column(width=1),
                  shiny::column(width=1,shiny::radioButtons(inputId=ns("filetype"), "Select file extension:", choices = c("CSV" = "csv", "Excel" = "xlsx"))),
                  shiny::column(width=2,shiny::downloadButton(outputId=ns("downloadData11"),label="Save genetic map data",class="butt1")),
                  shiny::column(width=1),
                  shiny::column(width=2,id=ns("showhideSignal1"),tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                  shiny::plotOutput(ns("TrafficLight2"),width="80%",height="auto") ),
                  shinydashboard::box(width=12,style='auto;overflow-x: scroll;height:1000px;overflow-y: scroll;',
                                        shinycssloaders::withSpinner(shiny::plotOutput(ns('GM_all_chromosomes'),width="1600px",height="2800px"),color="#0dc5c1",proxy.height = "200px"))
    ))
 )
}


# Module Server
#' @rdname mod_genetic_map
#' @param filter character contains the selected chromosome
#' @param breed.select character contains the name of the selected breed
#' @param geneticMap data frame containing the genetic map
#' @param make.traff.light object containing the ggplot for the likelihood quality signal (\link{make_traffic_light})
#' @param approach.info data frame containing the predefined settings for selected approaches (\link{table_approach_information})
#' @param add.approach.names character containing the concatenated approach abbreviations and is used as an addition to the file name
#' @param figure_rendered_gm reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed analysis" sidebar elements.
#' @param state numeric indicating if the threshold slider UI should be created (0; new module ID) or not (1; reused module ID and settings retained). State of the module is tracked on the main server side.
#'
#' @export

mod_genetic_map_server=function(id, filter,breed.select,geneticMap,make.traff.light,approach.info,add.approach.names,figure_rendered_gm,state){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    ## A delay was added to ensure the DOM is ready before executing shinyjs show, hide or click functions.
    ## Initialize which UI parts are shown.
    shinyjs::delay(3,
       if(filter!="All"){ ## when specific chromosome is selected
           shinyjs::show(id=ns("output1"),asis=TRUE)
           shinyjs::hide(id=ns("output2"),asis=TRUE)
           if(match("Likelihood_male",approach.info$Approach,0)!=0){
              shinyjs::show(id = ns("showhideSignal2"),asis=TRUE)
              output$TrafficLight <- shiny::renderPlot({
                   make.traff.light()
              }, width=80, height=30)
           }
       }else ## when all chromosome is selected
       {
           shinyjs::show(id=ns("output2"),asis=TRUE)
           shinyjs::hide(id=ns("output1"),asis=TRUE)
           if(length(grep("Likelihood_male",approach.info$Approach))!=0){
               shinyjs::show("showhideSignal1")
               output$TrafficLight2 <- shiny::renderPlot({
                     make.traff.light()
               }, width=80, height=30)
            }
            ## A help button was added to ensure correct access to the information when the ggplot (generated or cached) is fully rendered.
            ## This ensures the correct timing to show the corresponding sidebar elements for "Breed analysis".
            shinyjs::click(id=ns("help_Button"),asis=TRUE)
        }
    )


    if(filter!="All") ## when specific chromosome is selected
    {
        ## determine needed cols
        pos.list=lapply(1:nrow(approach.info),function(inner){
          grep(approach.info$Abbreviation[inner],colnames(geneticMap))
        })

        pos.use=do.call("c",pos.list)
        use.cols=c(1:5,pos.use)

        ## Table header with internal link to methodology - change depending on approach
        sketch2<-create_table_header2(approach.info=approach.info,no.cols=length(pos.use))

        # data
        genetMap<-geneticMap[geneticMap$Chr %in% filter,use.cols]

        ## create slider when state is set to 0
        if(state==0)
        {
          output$sliderRangeMap <- shiny::renderUI({
            shiny::sliderInput(ns("rangeMap"), "Range based on Mbp", min=genetMap$Mbp_position[1], max= genetMap$Mbp_position[length(genetMap$Mbp_position)], step=0.1, value=c(20,30))
          })
        }

        # determine once format for print-out table
        pos.col1=grep("_cM",colnames(genetMap))
        pos.col2=grep("_recrate_adjacent",colnames(genetMap))
        pos.col3=grep("Mbp_",colnames(genetMap))
        digit.cols=c(rep(8,length(pos.col1)),rep(8,length(pos.col2)),rep(6,length(pos.col3)))

        ## React to genetic map plot (see output$genetic) when it is fully rendered
        ## and update the figure_rendered_gm() to enable "Breed analysis" sidebar elements
        shiny::observeEvent(input$genetic_rendered_breed, {
          figure_rendered_gm(TRUE)
        })

        ## render table
        output$TableMaps=DT::renderDataTable({
          DT::datatable(genetMap,filter="none",container=sketch2, options=list(searching = TRUE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='csv',title=file.name),list(extend='excel',title=file.name)),
                                                                           pagelength = 10,lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),
                                                                           escape=FALSE,rownames=FALSE)%>%DT::formatRound(columns = c(pos.col1,pos.col2,pos.col3),digits=digit.cols)
        }, server = FALSE)

        #### plot
        plot.data<-transformdata_genetic_map(input=genetMap,approach.info=approach.info)

        file.name=paste0(breed.select,"_geneticMap_BTA-",filter,"_",add.approach.names)

        ## render plot and send signal when ready
        output$genetic <- plotly::renderPlotly({

          pp=makePlot_geneticMaps(dat=plot.data[[1]],name.file=file.name)
          pp%>%toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
        })

        ##### react of selected slider input and update corresponding elements
        shiny::observeEvent(input$ButtonshowRangeMap,{
          req(input$ButtonshowRangeMap)

          shinyjs::show("ButtonAll")

          file.name=paste0(breed.select,"_genetic-maps_BTA-",filter,"_range-",input$rangeMap[1],"-",input$rangeMap[2],"_",add.approach.names)

          range<-input$rangeMap
          range1 <<- which(as.numeric(genetMap$Mbp_position)>=input$rangeMap[1])
          range2 <<- which(as.numeric(genetMap$Mbp_position)<=input$rangeMap[2])

          if(length(range1)==0)range1=1
          if(length(range2)==0)range2=dim(genetMap)[1]

          ## data
          data=genetMap[range1[1]:range2[length(range2)],]

          ## render table
          output$TableMaps=DT::renderDataTable({
            DT::datatable(data,filter="none" , container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='csv',title=file.name), list(extend='excel',title=file.name)),
                                                                             pagelength = 10,lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)%>%DT::formatRound(columns = c(pos.col1,pos.col2,pos.col3),digits=digit.cols)
          }, server = FALSE)

          ## render plot and send signal when ready
          output$genetic <- plotly::renderPlotly({
             figure_rendered_gm(FALSE)

             plot.data1<-transformdata_genetic_map(input=data,approach.info=approach.info)
             pp=makePlot_geneticMaps(dat=plot.data1[[1]],name.file=file.name)
             pp%>% toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
           })
        })

        #### react to "Reset to all" button click and reset slider to all and update corresponding elements
        shiny::observeEvent(input$ButtonAll,{
          req(input$ButtonAll)

          shinyjs::hide("ButtonAll")

          file.name=paste0(breed.select,"_geneticMap_BTA-",filter,"_",add.approach.names)

          ## render table
          output$TableMaps=DT::renderDataTable({
            DT::datatable(genetMap,filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='csv',title=file.name), list(extend='excel',title=file.name)),
                                                                                                pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)%>%DT::formatRound(columns = c(pos.col1,pos.col2,pos.col3),digits=digit.cols)
          }, server = FALSE)

          ## reset slider
          output$sliderRangeMap <- shiny::renderUI({
            shiny::sliderInput(ns("rangeMap"), "Range based on Mbp", min= genetMap$Mbp_position[1], max= genetMap$Mbp_position[length( genetMap$Mbp_position)],
                               step=0.1, value=c(genetMap$Mbp_position[1], genetMap$Mbp_position[length( genetMap$Mbp_position)]))
          })

          ## render plot and send signal when ready
          output$genetic <- plotly::renderPlotly({
            figure_rendered_gm(FALSE)

            pp=makePlot_geneticMaps(dat=plot.data[[1]],name.file=file.name)
            pp%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
           })
        })

      }else ## when all chromosome is selected
      {
        ## a reactive value to track if the plot is ready
        plot_rend <- shiny::reactiveVal(FALSE)

        ## react to full genetic map plot (cached or generated) via reactive value plot_rend() when it is fully rendered
        ## and update the figure_rendered_gm() to enable the "Breed analysis" sidebar elements
        shiny::observe({
          req(plot_rend)

          if(plot_rend()==TRUE){
            figure_rendered_gm(TRUE)}
          else figure_rendered_gm(FALSE)
        })

        ## react to help button click: transform data, generate plot and send signal when plot is ready rendered
        shiny::observeEvent(input$help_Button,{
          req(input$help_Button)

          ## data
          ll.single=transformdata_genetic_map(input=geneticMap,approach.info=approach.info)
          filename.gm.single_all<-paste0(breed.select,"_geneticMaps_",add.approach.names,".png")

          ## create plot
          InputPlot_gm_single<-shiny::reactive({
            makePlot_all_geneticMaps(ll.gm.s=ll.single)
          })

          ## render plot
          output$GM_all_chromosomes <-shiny::renderPlot({
            InputPlot_gm_single()
          })%>%shiny::bindCache(breed.select,filter,add.approach.names)

          # change state of the reactive variable that the plot is fully rendered
          plot_rend(TRUE)

          ## download the figure
          output$download11 <- shiny::downloadHandler(
           filename =  filename.gm.single_all,
           content = function(file) {
             showModal(modalDialog("Loading", footer=NULL))
             on.exit(removeModal())
             ggplot2::ggsave(file, plot = InputPlot_gm_single(), device = "png",width=20,height=40,units="in",dpi=300)
           }
          )
        },ignoreInit = TRUE)

        ## All for table output
        pos.list=lapply(1:nrow(approach.info),function(inner){
          grep(approach.info$Abbreviation[inner],colnames(geneticMap))
        })
        pos.use=do.call("c",pos.list)
        use.cols=c(1:5,pos.use)

        ## create table header accordingly to selected approaches
        headerOutputTable<-create_table_header2_all(approach.info=approach.info,no.cols=length(pos.use))

        ## create table
        genetMap=geneticMap[,use.cols]
        colnames(genetMap)<-headerOutputTable
        genetMap2=shiny::reactive(genetMap)

        ## download table
        output$downloadData11 <- shiny::downloadHandler(
          filename = function(){
            paste0(breed.select,"_geneticMap_BTA-ALL_",add.approach.names, ifelse(input$filetype == "csv", ".csv", ".xlsx"))
          },
          content = function(file) {
            showModal(modalDialog("Loading", footer=NULL))
            on.exit(removeModal())
            if(input$filetype=="csv")utils::write.table(genetMap2(), file, row.names = FALSE,col.names = TRUE,sep=",")
            else if(input$filetype=="xlsx")writexl::write_xlsx(genetMap2(), file)
          }
        )
      }
  })
}

## To be copied in the UI
# mod_genetic_map_ui("mod_genetic_map_1")

## To be copied in the server
# callModule(mod_genetic_map_server,"mod_genetic_map_1")
