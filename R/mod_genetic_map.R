# Module UI

#' @title mod_genetic_map_ui and mod_genetic_map_server
#' @description  A shiny module to generate the outcome for the tabpanel "Genetic map" for the sidebar "Breed analysis".
#'
#' @details Depending on the user choice, when a specific chromosome is selected than the function \link{makePlot_geneticMaps} is used to create the plot.
#' If all chromosomes are selected than genetic map data are prepared for the plot using the function \link{transformdata_genetic_map}
#' and plotted using the function \link{makePlot_all_geneticMaps}. Moreover, the corresponding plot is cached to increase the app performance.
#' Finally, the quality of the likelihood approach is also shown here by means of a traffic light (also termed as likelihood quality signal).
#'
#' @param id module id
#'
#' @rdname mod_genetic_map
#'
#' @keywords internal, genetic map
#' @export
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @rawNamespace importFrom(utils,write.csv)
#' @rawNamespace importFrom(writexl,write_xlsx)
#'
#'@seealso  \link{transformdata_genetic_map}, \link{makePlot_geneticMaps}, \link{makePlot_all_geneticMaps} and \link{make_traffic_light}

mod_genetic_map_ui<-function(id)
{
  ns<-shiny::NS(id)

  shiny::tagList(
    htmltools::br(),htmltools::br(),
    shiny::fluidRow(id=ns("output1"),
             shinydashboard::box(title= tags$b("Interactive graphical visualization: Genetic vs. physical distance"),status="danger",width=12,
                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                 shiny::column(width=9,""),
                 shiny::column(width=2,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                               plotOutput(ns("TrafficLight"),width="80%",height="auto") ),
                 shiny::column(width=12,plotly::plotlyOutput(ns("genetic"),width="90%",height="90%")%>% shinycssloaders::withSpinner(color="#0dc5c1"))

              ),
              htmltools::br(),htmltools::br(),
              ########## new insert - take may out again
              shinydashboard::box(title= tags$b("Genetic distance in selected interval"),status="danger",width=12,
                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                 shiny::fluidRow(
                   shiny::column(width=12,"Use the slider to define the range of interest and then click on button",em('Apply selected intervall'),". This accordingly changes the figure and table including corresponding filenames. Use the button", em('Reset to all')," to obtain corresponding results for whole chromosome again."),
                   shiny::column(width = 12,style="padding-top:30px", ""),
                   shiny::column(width=10,shiny::uiOutput(ns("sliderRangeMap"))),shiny::column(width=2,""),
                   shiny::column(width = 12,style="padding-top:30px", ""),
                   shiny::column(width=12,shiny::actionButton(ns("ButtonshowRangeMap"),"Apply selected intervall"),shiny::actionButton(ns("ButtonAll"),"Reset to all"))
                 ),
                 htmltools::br(),
                 htmltools::hr(style = "border-top: 1px solid #68838B;"),htmltools::br(),htmltools::br(),
                 shiny::column(width=12,downloadButton(ns("csvstore1"),label="CSV",class="butt1",icon=NULL,style="margin-right: 5px; width=100px; position:absolute;left:0em;"),downloadButton(ns("excelstore1"),label="Excel",icon=NULL,class="butt1",style="margin-left: 40px")),
                 shiny::fluidRow(shiny::column(width=11,DT::dataTableOutput(outputId=ns("TableMaps")),style = "overflow-y: scroll;overflow-x: scroll;"))
              )
    ),
    shiny::fluidRow(id=ns("output3"),shiny::column(width=1),
                                    shiny::column(width=2,shiny::downloadButton(outputId=ns("download11"),label="Save figure",class="butt1")),
                                    shiny::column(width=9),
                                    shiny::column(width=9),
                                  shiny::column(width=2,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                  plotOutput(ns("TrafficLight2"),width="80%",height="auto") ),
                                      shinydashboard::box(width=12,style='auto;overflow-x: scroll;height:1000px;overflow-y: scroll;',
                                       shinycssloaders::withSpinner(shiny::plotOutput(ns('GM_all_chromosomes'),width="1600px",height="2800px"),color="#0dc5c1",proxy.height = "200px"))
    )
  )
}


# Module Server
#' @rdname mod_genetic_map
#' @param filter character contains the selected chromosome
#' @param breed.select character contains the name of the selected breed
#' @param geneticMap data frame containing the genetic map
#' @param color.breed.dl vector containing the predefined colors
#' @param make.traff.light object containing the ggplot for the likelihood quality signal
#'
#' @export

mod_genetic_map_server=function(id, filter,breed.select,geneticMap,color.breed.dl,make.traff.light){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    X<-Y<-Approach<-NULL


    ## Table header with internal link to methodology
    thead<-tr<-th<-NULL
    sketch2 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=2,""),
          th(colspan=3,"Physical Map "),
          th(colspan=3,"Genetic Map")
        ),
        tr(
          th(colspan=5,""),
          th(colspan=1,tags$a(href="#","Deterministic approach", onclick = "openTab('methodology')",style='text-decoration-line: underline;') ),
          th(colspan = 2,tags$a(href="#","Likelihood-based approach", onclick = "openTab('methodology')",style='text-decoration-line: underline;'))
        ),
        tr(
          th(colspan=1,"Chromosome"),
          th(colspan=1,"Marker"),
          th(colspan=1,"Position (Mbp)"),
          th(colspan=1,"Inter-marker distance (Mbp)"),
          th(colspan=1,"Position (BP)"),
          th(colspan=1,"Position (cM)"),
          th(colspan=1,"Position (cM)"),
          th(colspan=1,"Recombination rate adjacent")
        )
      )
    ))

    if(filter!="All")
    {
      shinyjs::show(id="output1")
      shinyjs::hide(id="output2")
      shinyjs::hide(id="output3")

      ## render traffic light
      output$TrafficLight <- renderPlot({
        make.traff.light()
      }, width=80, height=30)
      ##

      genetMap=geneticMap[geneticMap$Chr %in% filter,c(1:3,8,4,6,5,7)]
      file.name=paste0(breed.select,"_genetic-maps_BTA-",filter)

      ## initialize
      output$sliderRangeMap <- shiny::renderUI({
        shiny::sliderInput(ns("rangeMap"), "Range based on Mbp", min= genetMap$Mbp_position[1], max= genetMap$Mbp_position[length(genetMap$Mbp_position)], step=0.1, value=c(20,30))
      })
      output$genetic <- plotly::renderPlotly({
        pp=makePlot_geneticMaps(chr=filter,dat_maps=genetMap,name.file=file.name,colo=color.breed.dl)
        pp%>%toWebGL()
      })

      output$TableMaps=DT::renderDataTable({
        DT::datatable(genetMap,filter="none",container=sketch2, options=list(searching = TRUE,dom='Bfrtip',buttons = list('pageLength'), #,'copy', list(extend='csv',title=file.name),list(extend='excel',title=file.name)),
                                                                           pagelength = 10,lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),
                                                                           escape=FALSE,rownames=FALSE)
      }, server = FALSE)

      ## storing table information
      output$csvstore1<-shiny::downloadHandler(
        paste0(file.name,".csv") , content = function(file) {
          utils::write.csv(genetMap, file, row.names = FALSE
          )
        })

      output$excelstore1<-shiny::downloadHandler(
        paste0(file.name,".xlsx"), content = function(file) {
          writexl::write_xlsx(genetMap, file)
        })

      ### end initialize

      shiny::observeEvent(input$ButtonshowRangeMap,{
        req(input$ButtonshowRangeMap)

        file.name=paste0(breed.select,"_genetic-maps_BTA-",filter,"_range-",input$rangeMap[1],"-",input$rangeMap[2])

        range<-input$rangeMap
        range1 <<- which(as.numeric(genetMap$Mbp_position)>=input$rangeMap[1])
        range2 <<- which(as.numeric(genetMap$Mbp_position)<=input$rangeMap[2])

        if(length(range1)==0)range1=1
        if(length(range2)==0)range2=dim(genetMap)[1]

        data=genetMap[range1[1]:range2[length(range2)],]

        output$TableMaps=DT::renderDataTable({
          DT::datatable(data,filter="none" , container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength'),#'copy', list(extend='csv',title=file.name), list(extend='excel',title=file.name)),
                                                                             pagelength = 10,lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
        }, server = FALSE)

        output$genetic <- plotly::renderPlotly({
          pp=makePlot_geneticMaps(chr=filter,dat_maps=data,name.file=file.name,colo=color.breed.dl)
          pp%>% toWebGL()
         })

        output$csvstore1<-shiny::downloadHandler(
          paste0(file.name,".csv") , content = function(file) {
            utils::write.csv(data, file, row.names = FALSE
            )
          })

        output$excelstore1<-shiny::downloadHandler(
          paste0(file.name,".xlsx"), content = function(file) {
            writexl::write_xlsx(data, file)
          })

      })

       ### hier reset to all
      shiny::observeEvent(input$ButtonAll,{
        req(input$ButtonAll)

        file.name=paste0(breed.select,"_genetic-maps_BTA-",filter)

        output$TableMaps=DT::renderDataTable({
          DT::datatable(genetMap,filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength'),#'copy', list(extend='csv',title=file.name), list(extend='excel',title=file.name)),
                                                                                                pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
        }, server = FALSE)

        output$sliderRangeMap <- shiny::renderUI({
          shiny::sliderInput(ns("rangeMap"), "Range based on Mbp", min= genetMap$Mbp_position[1], max= genetMap$Mbp_position[length( genetMap$Mbp_position)],
                               step=0.1, value=c( genetMap$Mbp_position[1], genetMap$Mbp_position[length( genetMap$Mbp_position)]))
        })

        output$genetic <- plotly::renderPlotly({
          pp=makePlot_geneticMaps(chr=filter,dat_maps= genetMap,name.file=file.name,colo=color.breed.dl)
          pp%>%plotly::toWebGL()
        })

        output$csvstore1<-shiny::downloadHandler(
          paste0(file.name,".csv") , content = function(file) {
            utils::write.csv(genetMap, file, row.names = FALSE
            )
          })

        output$excelstore1<-shiny::downloadHandler(
          paste0(file.name,".xlsx"), content = function(file) {
            writexl::write_xlsx(genetMap, file)
          })
      })
    }

    if(filter=="All")
    {
      shinyjs::hide(id="output1")
      shinyjs::show(id="output2")
      shinyjs::show(id="output3")

      ## render traffic light
      output$TrafficLight2 <- renderPlot({
        make.traff.light()
      }, width=80, height=30)


      filename.gm.single_all=paste0(breed.select,"_genetic-maps_all.png")

      ll.single=transformdata_genetic_map(n=29,input=geneticMap)
      InputPlot_gm_single=shiny::reactive(makePlot_all_geneticMaps(ll.gm.s=ll.single,colo=color.breed.dl))

      output$GM_all_chromosomes <-shiny::renderPlot({
        InputPlot_gm_single()
      })%>%shiny::bindCache(breed.select,filter)

      output$download11 <- shiny::downloadHandler(
        filename =  filename.gm.single_all,
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          ggplot2::ggsave(file, plot = InputPlot_gm_single(), device = "png",width=20,height=40,units="in",dpi=300)
        }
      )
    }
  })
}


## To be copied in the UI
# mod_genetic_map_ui("mod_genetic_map_1")

## To be copied in the server
# callModule(mod_genetic_map_server,"mod_genetic_map_1")
