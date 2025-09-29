# bc_hotspot UI Function
#'
#' @title mod_bc_hotspot_ui and mod_bc_hotspot_server
#' @description A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed comparison".
#'
#' @details Within the module the input data are prepared using the function \link{transformdata_hotspot_bc}. To produce the hotspot plot, the function
#' \link{scatterPlot_hotspot} for a selected chromosome or \link{scatterPlot_hotspot_all} when all chromosomes are selected is used.
#'  This module also shows a Venn diagram of corresponding revealed hotspot SNP markers. The input data are prepared
#'  using the function \link{process_venn_data}, corresponding colors for Venn sets are created using the function \link{create.colors} and plotted using the function \link{creating_venn}. The user may select a Venn diagram subset and the
#'  table will be reduced accordingly using the function \link{prepare_table_venn}.
#'
#' @rdname mod_bc_hotspot
#'
#' @param id module id

#' @keywords internal
#' @import shiny
#' @importFrom shinydashboard box
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @rawNamespace import(shinyjs, except = runExample)
#' @importFrom rlang is_empty
#' @import htmltools
#'
#' @seealso
#' * \link{transformdata_hotspot_bc} \cr
#' * \link{scatterPlot_hotspot} \cr
#' * \link{scatterPlot_hotspot_all} \cr
#' * \link{process_venn_data} \cr
#' * \link{create.colors} \cr
#' * \link{creating_venn} \cr
#' * \link{prepare_table_venn}
#' @export
#'
mod_bc_hotspot_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
  shiny::fluidRow(id=ns("no.likelihood"),
                    htmltools::br(),htmltools::br(),
                    shiny::column(width=12,htmltools::tags$h3(htmltools::HTML("<span style=color:red;margin-left: 6px;> Hotspots cannot be detected for the likelihood-based approach.</span>")))
    ),
    shiny::fluidRow(id=ns("wo.likelihood1"),
      shiny::column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')",style='text-decoration-line: underline;')," intervals.")),
      htmltools::br(),
      htmltools::br(),
      shinydashboard::box(width=12, title=htmltools::tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
          h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return."),
          shiny::column(width= 12,plotly::plotlyOutput(ns("plothotspot"),height="100%") %>% shinycssloaders::withSpinner(color="#0dc5c1"))
      )
    ),

    htmltools::br(),
    shiny::fluidRow(id=ns("wo.likelihood2"),
      shinydashboard::box(width=12,title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",
          solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny:: fluidRow(shiny::column(width=12,"Use the slider to define the threshold to hotspot region (standard deviation of recombination rate). This accordingly changes the figure and table.
                                         The default threshold is 2.5.", div(style="margin-bottom:30px")),
                          shiny::column(width=4,
                              shiny::sliderInput(ns("threshold"),"Threshold to define hotspot region",min = 0, max = 10, value = 2.5, step = 0.1),
                          div(style="margin-bottom:60px"))),
          htmltools::hr(style = "border-top: 1px solid #68838B;"),
          shiny:: fluidRow(shinyjs::useShinyjs(),id=ns("venn_hot1"),shiny::column(width=6,shiny::actionButton(ns("venn_hotspot1"), "Show interactively venn diagram",style = "color: black;
                            background-color: #87CEFA"))),
          shiny::fluidRow(shinyjs::useShinyjs(),id=ns("venn_hot2"),
                            shiny::column(width=10,shiny::downloadButton(ns("downloadVenn"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1"),
                                          shiny::actionButton(ns("venn_hotspot2"), "Hide venn diagram",style="background-color: #87CEFA"),
                                          shiny::actionButton(ns("ButtonAll_bc_hotspot"),"Reset table to all",style="background-color: #87CEFA")),
                            shiny::column(width=10,style="padding-top:30px", ""),
                            shiny::column(width=10,"When you click on a specific subset of interest, only the markers for that set are listed in the table."),
                            shiny::column(width=5,shiny::plotOutput(ns("venn_diagram"), click = ns("plot_click"),width = "100%",
                                                                         height = "300px",inline=TRUE))
         ),
         htmltools:: br(),
         htmltools::hr(style = "border-top: 1px solid #68838B;"),
         htmltools::br(),
         shiny::fluidRow(shiny::column(width=7,DT::dataTableOutput(outputId=ns("tablehotspot")),style = "overflow-y: scroll;overflow-x: scroll;")),
         )
      )
    )
}

# bc_hotspot Server Functions
#' @rdname mod_bc_hotspot
#'
#' @param filter character contains the selected chromosome
#' @param breed.infos data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param names.files character containing the concatenate breed names for file naming
#' @param approach data frame containing the predefined settings and names for the selected approach (\link{table_approach_information})
#'
#'@export
mod_bc_hotspot_server <- function(id,filter, breed.infos,names.files,approach){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    adjacentRecRate<-NULL

    if(approach$Approach=="Likelihood_male"){
      shinyjs::show(id="no.likelihood")
      shinyjs::hide(id="wo.likelihood1")
      shinyjs::hide(id="wo.likelihood2")
    }else{

      shinyjs::hide(id="no.likelihood")
      shinyjs::show(id="wo.likelihood1")
      shinyjs::show(id="wo.likelihood2")

      shinyjs::hide("ButtonAll_bc_hotspot")
      shinyjs::hide(id="venn_hot1")
      shinyjs::show(id="venn_hot2")

      ## Table header with internal link to methodology
      sketch2<-create_table_header_bc3(breed.select.bc=breed.infos$Name)

      shiny::observeEvent(input$threshold,
      {
          req(input$threshold)

          rem<-NULL
          adjacent.mat <- mapply(function(x) {
            load(system.file("extdata",paste0(x,"/adjacentRecRate.Rdata"),package="CLARITY"))
            if(length(which(is.na(adjacentRecRate$cM)==TRUE))!=0){
              adjacentRecRate=adjacentRecRate[-which(is.na(adjacentRecRate$cM)==T),]
            }

            if(is.numeric(adjacentRecRate$Dis)==TRUE){
              adjacentRecRate$Dis=as.integer(adjacentRecRate$Dis)
            }
            adjacentRecRate
          },breed.infos$Name , SIMPLIFY = FALSE)

          dat<-transformdata_hotspot_bc(data.trans=adjacent.mat,value=input$threshold,filter=filter,infos=breed.infos,approach=approach)
          venn.data.all<-dat[[3]]

          ranges <- shiny::reactiveValues(x = NULL, y = NULL)
          #table
          data12.2<-dat[[2]]

          output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_",approach$Abbreviation)
          if(filter=="All"){
            output$tablehotspot= DT::renderDataTable({
             DT::datatable(data12.2,  container=sketch2,  extensions = c("Buttons"), rownames=FALSE ,options = list(searching=FALSE,dom='Bfrtip',
                                                                                columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                                pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
            },server=FALSE)
          }else
          {
             output$tablehotspot=DT::renderDataTable({
              DT::datatable(data12.2, container=sketch2, filter="none" , options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                  pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE) #
            },server=FALSE)
          }

          dat0<-as.data.frame(dat[[1]])
          if(filter!="All")
          {
              output$plothotspot <- plotly::renderPlotly({
                tt=scatterPlot_hotspot(dat0,file.name=paste0("Plot_",names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_",approach$Abbreviation))
                tt%>%toWebGL()
             })
          }else
          {
              output$plothotspot <- plotly::renderPlotly({
                  tt=scatterPlot_hotspot_all(dat0,file.name =paste0("Plot",names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_",approach$Abbreviation) )
                  tt%>%toWebGL()
              })
          }

          venn<-RVenn::Venn(venn.data.all)
          venn_data <-process_venn_data(venn)

          if(nrow(breed.infos)==2)
          {
            width1= 500
            height1= 200
            width1.venn.plot=6 ## for print-out
            height1.venn.plot=3
          }
          else
          {
            width1= 600 #700
            height1= 350 #750
            width1.venn.plot=9
            height1.venn.plot=6
          }

          col1<-create.colors(colo=breed.infos$Color)
          InputPlot5=shiny::reactive(creating_venn(venn_data,long.names=breed.infos$Name,abbreviations=breed.infos$Abbreviation,venn.colors=col1))
          output$venn_diagram <- shiny::renderPlot({
            InputPlot5()
          },width=width1, height=height1)

          ## select  specific set from venn diagram
          observeEvent(input$plot_click, {
            req(input$plot_click)
            data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$plot_click, table.bc=data12.2)

            output$tablehotspot <-DT::renderDataTable({
              output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_set-",data2[[2]],"_",approach$Abbreviation)
                DT::datatable(data2[[1]],container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                                                      options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                     dom = 'Bt', buttons = list('pageLength', 'copy',list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                     pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
            },server=FALSE)
            shinyjs::show("ButtonAll_bc_hotspot")
          })

          ### hier reset to all
          shiny::observeEvent(input$ButtonAll_bc_hotspot,{
            output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_",approach$Abbreviation)
            output$tablehotspot <-DT::renderDataTable({
              DT::datatable(data12.2,container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                                                      options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                     dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                     pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
            },server=FALSE)
            shinyjs::hide("ButtonAll_bc_hotspot")
          })


          #download Venn Plot
          output$downloadVenn <- shiny::downloadHandler(
            filename = paste0(names.files,"_Venn_hotspots_BTA-",filter,"_", input$threshold,"_",approach$Abbreviation,".png") ,
            content = function(file){
              ggplot2::ggsave(file, plot =  InputPlot5(), bg="white",device = "png",width= width1.venn.plot,height= height1.venn.plot,units="in",dpi=300)
            }
          )

      }) ## end observe sliderInput threshhold

      shiny::observeEvent(input$venn_hotspot1,{
        shinyjs::hide(id="venn_hot1")
        shinyjs::show(id="venn_hot2")
      })

      shiny::observeEvent(input$venn_hotspot2,{
        shinyjs::show(id="venn_hot1")
        shinyjs::hide(id="venn_hot2")
      })
    }
  })
}

## To be copied in the UI
# mod_bc_hotspot_ui("bc_hotspot_1")

## To be copied in the server
# mod_bc_hotspot_server("bc_hotspot_1")
