# Module UI

#' @title mod_hotspot_ui and mod_hotspot_server
#' @description A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed analysis".
#'
#' @details  Within the module the input data are prepared using the function \link{transformdata_hotspot}. To produce the hotspot plot, the function
#' \link{scatterPlot_hotspot} for a selected chromosome or \link{scatterPlot_hotspot_all} when all chromosomes are selected is used.
#'  This module also shows a Venn diagram of corresponding revealed hotspot SNP markers. The input data are prepared
#'  using the function \link{process_venn_data}, corresponding colors for Venn sets are created using the function \link{create.colors} and plotted using the function \link{creating_venn}. The user may select a Venn diagram subset and the
#'  table will be reduced accordingly using the function \link{prepare_table_venn}.
#'
#' @param id module id
#'
#' @rdname mod_hotspot
#'
#' @keywords internal, hotspot
#' @export
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @importFrom ggVennDiagram Venn
#'
#' @seealso
#' * \link{transformdata_hotspot},
#' * \link{scatterPlot_hotspot},
#' * \link{scatterPlot_hotspot_all}
#' * \link{process_venn_data}
#' * \link{create.colors} \cr
#' * \link{creating_venn}
#' * \link{prepare_table_venn}

mod_hotspot_ui=function(id)
{
  ns=shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(id=ns("no.likelihood"),
      htmltools::br(),htmltools::br(),
      shiny::column(width=12,htmltools::tags$h3(htmltools::HTML("<span style=color:red;margin-left: 6px;>  Hotspots cannot be detected for the likelihood-based approach.</span>")))
      ),

    shiny::fluidRow(id=ns("wo.likelihood1"),
      shiny::column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')",style='text-decoration-line: underline;')," intervals.")),
      htmltools::br(),htmltools::br(),
      shinydashboard::box(width=12, title=tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
        h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return."),
        shiny::column(width=11,plotly::plotlyOutput(ns("plothotspot2"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1"))#width="90%",height="90%",
      )
   ),
   htmltools::br(),
   shiny::fluidRow(id=ns("wo.likelihood2"),
     shinydashboard::box(title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",width=12,
      solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
      shiny::fluidRow(
        shiny::column(width=12, "Use the slider to define the threshold to hotspot region (standard deviation of recombination rate). This accordingly changes the figure and table. The default treshold is 2.5."),
        shiny::column(width=4, div(style="margin-bottom:30px"), sliderInput(ns("threshold"),"Threshold to define hotspot region",min = 0, max = 10, value = 2.5, step = 0.1),div(style="margin-bottom:60px")),column(width=8,"")
      ),
      shiny::fluidRow(id=ns("showhideBoxVenn"),
        shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")),
        shiny::column(width=12,shiny::downloadButton(outputId=ns("download.venn.diagram"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1"),
                    shiny::actionButton(ns("hideVenn"), "Hide venn diagram",style="background-color: #87CEFA"),
                    shiny::actionButton(ns("showVenn"), "Show interactive venn diagram",style="background-color: #87CEFA"),
                    shiny::actionButton(ns("ButtonResetVenn"),"Reset",style="background-color: #87CEFA")),
        shiny::column(width = 12,style="padding-top:30px", ""),
        shiny::column(width=8,"When you click on a specific subset of interest, only the markers for that set are listed in the table. To return to all sets please use the button '",em("Reset"),"'."),
        shiny::column(width=4,""),
        shiny::column(width=12,""),
        shiny::column(width=11,style='overflow-x: auto;',shiny::plotOutput(ns("venn_diagram_hot"),click = ns("venn_click"),width="20%",height="auto")),
      ),
      htmltools::hr(style = "border-top: 1px solid #68838B;"),
      shiny::fluidRow(
        shiny::column(width=12,DT::dataTableOutput(outputId=ns("tablehotspot"),height="auto",width="80%"),style = "overflow-y: auto;overflow-x: auto;")
        )
      )
    )
  )
}

# Module Server
#' @rdname mod_hotspot
#' @param filter character contains the selected chromosome
#' @param breed.select character contains the name of the selected breed
#' @param approach.info data.frame data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#' @param add.approach.names character containing the concatenated approach abbreviations and is used as an addition to the file name
#'
#' @keywords internal
#' @export
mod_hotspot_server=function(id, filter,breed.select,approach.info,add.approach.names){
   shiny::moduleServer(id, function(input, output, session){

     ns <- session$ns

      if(match("Likelihood_male",approach.info$Approach,0)!=0 && nrow(approach.info)==1)
      {
        shinyjs::show(id="no.likelihood")
        shinyjs::hide(id="wo.likelihood1")
        shinyjs::hide(id="wo.likelihood2")
      }else
      {
        shinyjs::show(id="wo.likelihood1")
        shinyjs::show(id="wo.likelihood2")
        shinyjs::hide("showhideBoxVenn")
        shinyjs::hide(id="ButtonResetVenn")
        shinyjs::show(id="hideVenn")
        shinyjs::hide(id="showVenn")
        shinyjs::hide(id="no.likelihood")
        adjacentRecRate<-venn_data<-NULL

        load(system.file("extdata",paste0(breed.select,"/adjacentRecRate.Rdata"),package="CLARITY"))

        ## show / hide legend
        shiny::observe({
           shinyjs::toggle(id = "element3", condition = input$checkbox3, asis=TRUE)
        })

        shiny::observeEvent(input$threshold,{
          InputPlot_hotspot_venn<-NULL

          ranges <- shiny::reactiveValues(x = NULL, y = NULL)

          ## take out the Likelihood approach when included
          if(match("Likelihood_male",approach.info$Approach,0)!=0)approach.info=approach.info[-match("Likelihood_male",approach.info$Approach),]

          #### create table header
          sketch3<-create_table_header3(approach.info=approach.info)

          dat=transformdata_hotspot(data.trans=adjacentRecRate,value=input$threshold,filter=filter,infos=approach.info)

          title1=paste0(breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_",add.approach.names)
          output$tablehotspot= DT::renderDataTable({
               DT::datatable(dat[[2]],  container=sketch3, extensions = c("Buttons"), rownames=FALSE ,options = list(searching=FALSE,dom='Bfrtip',
               columnDefs = list(list(className = 'dt-left', targets = "_all")),
               dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
               pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
          }, server = FALSE)

          if(nrow(approach.info)>=2)
          {
            shinyjs::show("showhideBoxVenn")

            colors<-create.colors(colo=approach.info$Color)
            venn <- ggVennDiagram::Venn(dat[[3]])
            venn_data <-process_venn_data(venn)

            if(nrow(approach.info)==2){
              width1= 400
              height1= 150
              width1.venn.plot=6
              height1.venn.plot=2.5 ## for print-out
            }else if(nrow(approach.info)==3)
            {
              width1= 600
              height1= 350
              width1.venn.plot=9
              height1.venn.plot=6
            }

            InputPlot_hotspot_venn=shiny::reactive(creating_venn(venn_data,long.names=approach.info$Approach, abbreviations=approach.info$Abbreviation,venn.colors=colors))

            output$venn_diagram_hot <- shiny::renderPlot({
              InputPlot_hotspot_venn()
            }, width=width1,height=height1)
          }else shinyjs::hide("showhideBoxVenn")

          shiny::observeEvent(input$venn_click, {
            req(input$venn_click)

            if(is.null(venn_data)==FALSE)
            {
              data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$venn_click, table.bc= dat[[2]])

              output$tablehotspot= DT::renderDataTable({
                title1=paste0(breed.select,"_hotspot-geneticMap_BTA-",filter,"_",input$threshold,"_set-",data2[[2]],"_",add.approach.names)

                DT::datatable(data2[[1]][order(data2[[1]][,3]),], container=sketch3, extensions = c("Buttons"),rownames=FALSE,
                              options = list(searching=FALSE,dom='Bfrtip', columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                             dom = 'Bt',buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                             pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All'))))

              },server=FALSE)

              shinyjs::show("ButtonResetVenn")
            }
          })

          shiny::observeEvent(input$hideVenn,{
           shinyjs::hide(id="hideVenn")
           shinyjs::show(id="showVenn")
           shinyjs::hide(id="venn_diagram_hot")
         })
          shiny::observeEvent(input$showVenn,{
           shinyjs::show(id="hideVenn")
           shinyjs::hide(id="showVenn")
           shinyjs::show(id="venn_diagram_hot")
         })

          shiny::observeEvent(input$ButtonResetVenn,{
            req(input$ButtonResetVenn)
            shinyjs::hide("ButtonResetVenn")

            output$tablehotspot= DT::renderDataTable({
              title1=paste0(breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_",add.approach.names)
              DT::datatable(dat[[2]], container=sketch3, extensions = c("Buttons"), rownames=FALSE,
                            options = list(searching=FALSE,dom='Bfrtip', columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                          dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                          pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
             }, server = FALSE)
           })

          output$plothotspot2 <- plotly::renderPlotly({
               if(filter!="All")pp=scatterPlot_hotspot(dat1=dat[[1]],file.name=paste0("Plot_",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_",add.approach.names))
               else  pp=scatterPlot_hotspot_all(dat1=dat[[1]],file.name=paste0("Plot_",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_",add.approach.names))
               pp%>%toWebGL()
          })

          output$download.venn.diagram <- shiny::downloadHandler(
              filename= paste0("Venn-Diagram-",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_",add.approach.names,".png"),## insert methods,
              content = function(file) {
              showModal(modalDialog("Loading", footer=NULL))
              on.exit(removeModal())
              ggsave(file, plot = InputPlot_hotspot_venn(), device = "png",bg="white",width=width1.venn.plot,height=height1.venn.plot,units="in",dpi=300)
            })
      })
    }
 })
}


## To be copied in the UI
# mod_hotspot_ui("mod_hotspot_1")

## To be copied in the server
# mod_hotspot_server("mod_hotspot_1")

