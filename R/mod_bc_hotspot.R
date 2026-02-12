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
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom rlang is_empty
#' @import htmltools
#' @importFrom htmlwidgets onRender
#' @rawNamespace import(dplyr, except = combine)
#' @importFrom ggVennDiagram Venn
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
  useShinyjs(),
  shinyjs::hidden(
    shiny::fluidRow(id=ns("no.likelihood_bc"),
                    htmltools::br(),htmltools::br(),
                    shiny::column(width=12,htmltools::tags$h3(htmltools::HTML("<span style=color:red;margin-left: 6px;> Hotspots cannot be detected for the likelihood-based approach.</span>")))
   )),
   shinyjs::hidden(
     shiny::fluidRow(id=ns("wo.likelihood1_bc"),
      shiny::column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')",style='text-decoration-line: underline;')," intervals.")),
      htmltools::br(),
      htmltools::br(),
      shinydashboard::box(width=12, title=htmltools::tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
          shiny::column(width=12, h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return.")),
          shiny::column(width=12, shiny::radioButtons(ns("plot_hotspot_choice_bc"),label="Plot hotspot only",inline=TRUE,selected="Yes",choices = c("yes"="Yes", "no"="No"))),
          shiny::column(width= 11,id=ns("showhidePLOT_bc"),style="padding-top:30px",plotly::plotlyOutput(ns("plothotspot"),height="auto", width="auto") %>% shinycssloaders::withSpinner(color="#0dc5c1")),
          shiny::column(width=11,style="padding-top:80px", id=ns("textforNoHotspot_bc"),htmltools::HTML("<b>No data to display - no regions meet the threshold criteria.</b>")),
          shinyjs::hidden(
            shiny::column(1,actionButton(ns("help_button4"),label="help_button"))
          )
      )
    )),
    htmltools::br(),
    shinyjs::hidden(
      shiny::fluidRow(id=ns("wo.likelihood2_bc"),
        shinydashboard::box(width=12,title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",
                                solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(shiny::column(width=12,"Use the slider to define the threshold to hotspot region (standard deviation of recombination rate). This accordingly changes the figure and table.
                                         The default threshold is 2.5.", div(style="margin-bottom:30px")),
                          shiny::column(width=4, shiny::sliderInput(ns("threshold"),"Threshold to define hotspot region",min = 0, max = 10, value = 2.5, step = 0.1),
                                          div(style="margin-bottom:60px"))
          ),
          shinyjs::hidden(
            shiny::fluidRow(id=ns("venn_hot1"),
                            shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")),
                            shiny::column(width=6,shiny::actionButton(ns("venn_hotspot1"), "Show interactively venn diagram",style = "color: black;
                                background-color: #87CEFA"))
            ),
            shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;"))
          ),
          shinyjs::hidden(
            shiny::fluidRow(id=ns("venn_hot2"),
                            shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")),
                            shiny::column(width=10,shiny::downloadButton(ns("downloadVenn"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1"),
                                          shiny::actionButton(ns("venn_hotspot2"), "Hide venn diagram",style="background-color: #87CEFA"),
                                          shinyjs::hidden(shiny::actionButton(ns("ButtonAll_bc_hotspot"),"Reset table to all",style="background-color: #87CEFA"))),
                            shiny::column(width=10,style="padding-top:30px", ""),
                            shiny::column(width=10,"When you click on a specific subset of interest, only the markers for that set are listed in the table."),
                            shiny::column(width=11,style='overflow-x: auto;',shiny::plotOutput(ns("venn_diagram"), click = ns("plot_click"),width = "20%", height = "auto")),
                            shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;"))
         )),
         htmltools::br(),
         shiny::fluidRow(
           shiny::column(width=11,DT::dataTableOutput(outputId=ns("tablehotspot"),width="60%",height="auto"),style = "overflow-y: auto;overflow-x: auto;")
         )
        )
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
#' @param figure_rend2 reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed comparison" sidebar elements.
#'
#'@export
mod_bc_hotspot_server <- function(id,filter, breed.infos,names.files,approach,figure_rend2){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## A delay was added to ensure the DOM is ready before executing shinyjs show and hide functions.
    ## Initialize which UI parts are shown.
    shinyjs::delay(3,
       if(approach$Approach=="Likelihood_male"){ # likelihood approach selected
             shinyjs::show(id=ns("no.likelihood_bc"),asis=TRUE)
             shinyjs::hide(id=ns("wo.likelihood1_bc"),asis=TRUE)
             shinyjs::hide(id=ns("wo.likelihood2_bc"),asis=TRUE)
       }else{
             shinyjs::hide(id=ns("no.likelihood_bc"),asis=TRUE)
             shinyjs::show(id=ns("wo.likelihood1_bc"),asis=TRUE)
             shinyjs::show(id=ns("wo.likelihood2_bc"),asis=TRUE)
       }
    )


    if(approach$Approach=="Likelihood_male"){ # likelihood approach selected
      figure_rend2(TRUE)
    }else{

      ## initialize
      adjacentRecRate<-NULL

      store.data.bc<-shiny::reactiveValues(adjacent.mat=NULL,data=list(),header=NULL)
      store.data.venn.bc<-shiny::reactiveValues(venn=NULL,venn_data=NULL)

      check.status.venn.bc<-shiny::reactiveVal(FALSE)
      check.reset.but<-shiny::reactiveVal(FALSE)

      ## data
      store.data.bc$adjacent.mat <- mapply(function(x) {
        load(system.file("extdata",paste0(x,"/adjacentRecRate.Rdata"),package="CLARITY"))
        if(length(which(is.na(adjacentRecRate$cM)==TRUE))!=0){
          adjacentRecRate=adjacentRecRate[-which(is.na(adjacentRecRate$cM)==T),]
        }
        if(is.numeric(adjacentRecRate$Dis)==TRUE){
          adjacentRecRate$Dis=as.integer(adjacentRecRate$Dis)
        }
        adjacentRecRate
      },breed.infos$Name , SIMPLIFY = FALSE)


      ## react to hotspot plot (see output$plothotspot) when it is fully rendered
      observeEvent(input$plothotspot_rendered, {
        shinyjs::enable(id="threshold")
        shinyjs::enable(id="plot_hotspot_choice_bc")
        figure_rend2(TRUE) #  enable the "Breed comparison" sidebar elements
      })

      ## create table header with internal link to methodology
      store.data.bc$header<-create_table_header_bc3(breed.select.bc=breed.infos$Name)

      ## react to selected hotspot threshold and update corresponding elements
      shiny::observeEvent(input$threshold,{
        req(input$threshold)

        ## initialize
        figure_rend2(FALSE)

        shinyjs::disable(id="threshold")
        shinyjs::disable(id="plot_hotspot_choice_bc")
        shinyjs::hide("ButtonAll_bc_hotspot")

        ## data
        store.data.bc$data<-transformdata_hotspot_bc(data.trans=store.data.bc$adjacent.mat,value=input$threshold,filter=filter,infos=breed.infos,approach=approach, choice=input$plot_hotspot_choice_bc)

        ## show/hide elements when data are not null
        if(nrow(store.data.bc$data[[1]])>0){
           shinyjs::show("showhidePLOT_bc")
           shinyjs::hide("textforNoHotspot_bc")
        }else{
            shinyjs::hide("showhidePLOT_bc")
            shinyjs::show("textforNoHotspot_bc")
            shinyjs::enable(id="plot_hotspot_choice_bc")
            shinyjs::enable(id="threshold")
            figure_rend2(TRUE)
        }

        ## table file name
        output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_",approach$Abbreviation)

        ## render table
        output$tablehotspot <-DT::renderDataTable({
          DT::datatable(store.data.bc$data[[2]],container=store.data.bc$header, extensions = c("Buttons"),  rownames=FALSE,
                        options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                       dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                       pagelength = 10, lengthMenu = list(c(10,20,30, -1), c(10,20,30,'All')) ))
        },server=TRUE)

        ## plot and send signal when plot is fully rendered
        output$plothotspot <- plotly::renderPlotly({
          if(filter!="All") tt=scatterPlot_hotspot(store.data.bc$data[[1]],file.name=paste0("Plot_",names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice_bc,"_",approach$Abbreviation),max.plot=store.data.bc$data[[4]])
          else  tt=scatterPlot_hotspot_all(store.data.bc$data[[1]],file.name =paste0("Plot",names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice_bc,"_",approach$Abbreviation), max.plot=store.data.bc$data[[4]])
          tt%>% plotly::toWebGL() %>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered', {
                    timestamp: new Date().toISOString()
                    });
                }
              ")
        })

        #### Venn diagram
        ## only generate Venn diagram when data is not zero
        if(nrow(store.data.bc$data[[2]])>0)
        {
            ## show or hide corresponding UI part
            if(check.status.venn.bc()==TRUE)shinyjs::click("venn_hotspot2")
            else shinyjs::click("venn_hotspot1")

            ## plot size for Venn diagram and print-out
            if(nrow(breed.infos)==2)
            {
               width1= 400
               height1= 150
               width1.venn.plot=6 ## for print-out
               height1.venn.plot=2.5
            }else
            {
              width1= 600
              height1= 350
              width1.venn.plot=9
              height1.venn.plot=6
            }

            ## create data for Venn
            venn.data.all<-store.data.bc$data[[3]]
            store.data.venn.bc$venn<-ggVennDiagram::Venn(venn.data.all)
            store.data.venn.bc$venn_data <-process_venn_data(store.data.venn.bc$venn)

            ## create Venn
            col1<-create.colors(colo=breed.infos$Color)
            InputPlot5=shiny::reactive(creating_venn(store.data.venn.bc$venn_data,long.names=breed.infos$Name,abbreviations=breed.infos$Abbreviation,venn.colors=col1))

            ## render Venn
            output$venn_diagram <- shiny::renderPlot({
              InputPlot5()
            },width=width1, height=height1)
        }else
        {
            ## hide corresponding UI part
            shinyjs::hide("venn_hot1")
            shinyjs::hide("venn_hot2")
        }

        #download Venn plot
        output$downloadVenn <- shiny::downloadHandler(
          filename = paste0(names.files,"_Venn_hotspots_BTA-",filter,"_", input$threshold,"_",approach$Abbreviation,".png") ,
          content = function(file){
            ggplot2::ggsave(file, plot =  InputPlot5(), bg="white",device = "png",width= width1.venn.plot,height= height1.venn.plot,units="in",dpi=300)
          }
        )
      })

      ## react to changes in plot hotspot choice (yes or no)
      shiny::observeEvent(input$plot_hotspot_choice_bc,{
        req(input$plot_hotspot_choice_bc)

        figure_rend2(FALSE)
        shinyjs::disable(id="threshold")
        shinyjs::disable(id="plot_hotspot_choice_bc")

        ## prepare data
        dat.choice.bc<-transformdata_hotspot_bc(data.trans=store.data.bc$adjacent.mat,value=input$threshold,filter=filter,infos=breed.infos,approach=approach, choice=input$plot_hotspot_choice_bc)#

        ## check that data is larger than 0
        if(nrow(dat.choice.bc[[1]])>0){
          shinyjs::show("showhidePLOT_bc")
          shinyjs::hide("textforNoHotspot_bc")

          output$plothotspot <- plotly::renderPlotly({
            if(filter!="All") tt=scatterPlot_hotspot(dat.choice.bc[[1]],file.name=paste0("Plot_",names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice_bc,"_",approach$Abbreviation),max.plot=dat.choice.bc[[4]])
            else  tt=scatterPlot_hotspot_all(dat.choice.bc[[1]],file.name =paste0("Plot",names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice_bc,"_",approach$Abbreviation), max.plot=dat.choice.bc[[4]])
            tt%>% plotly::toWebGL() %>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered', {
                    timestamp: new Date().toISOString()
                      });
                  }
                 ")
          })
        }else{
          shinyjs::enable(id="plot_hotspot_choice_bc")
          shinyjs::enable(id="threshold")
          figure_rend2(TRUE)
          shinyjs::hide("showhidePLOT_bc")   # hide the UI part for the plot
          shinyjs::show("textforNoHotspot_bc") # show the UI part that nothing exists for the hotspot threshold
        }
      },ignoreInit = TRUE)

      ## react to when Venn diagram is clicked
      shiny::observeEvent(input$plot_click, {
        req(input$plot_click)

        ## get data for the clicked Venn diagram subset
        data2=prepare_table_venn(venn.dat= store.data.venn.bc$venn_data,venn= store.data.venn.bc$venn,click=input$plot_click, table.bc=store.data.bc$data[[2]])

        output$tablehotspot <-DT::renderDataTable({
          output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_set-",data2[[2]],"_",approach$Abbreviation)
          DT::datatable(data2[[1]],container=store.data.bc$header, extensions = c("Buttons"),  rownames=FALSE,
                        options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                       dom = 'Bt', buttons = list('pageLength', 'copy',list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                       pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
        },server=TRUE)
        shinyjs::show("ButtonAll_bc_hotspot")
        check.reset.but(TRUE)
      },ignoreInit = TRUE)

      ## react to click on 'Reset table to all' button and reset table to show all data visible in the Venn diagram
      shiny::observeEvent(input$ButtonAll_bc_hotspot,{
        req(input$ButtonAll_bc_hotspot)

        if(check.reset.but()==TRUE){
          output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_",approach$Abbreviation)
          output$tablehotspot <-DT::renderDataTable({
            DT::datatable(store.data.bc$data[[2]],container=store.data.bc$header, extensions = c("Buttons"),  rownames=FALSE,
                          options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                         dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                         pagelength = 10, lengthMenu = list(c(10,20,30, -1), c(10,20,30,'All')) ))
          },server=TRUE)
        }
        check.reset.but(FALSE)
        shinyjs::hide("ButtonAll_bc_hotspot")
      },ignoreInit = TRUE)

      ## show Venn diagram
      shiny::observeEvent(input$venn_hotspot1,{
        shinyjs::hide(id="venn_hot1")
        shinyjs::show(id="venn_hot2")
        check.status.venn.bc(FALSE)
      },ignoreInit = TRUE)

      ## hide Venn diagram
      shiny::observeEvent(input$venn_hotspot2,{
        shinyjs::show(id="venn_hot1")
        shinyjs::hide(id="venn_hot2")
        check.status.venn.bc(TRUE)
      },ignoreInit = TRUE)
    }
  })
}

## To be copied in the UI
# mod_bc_hotspot_ui("bc_hotspot_1")

## To be copied in the server
# mod_bc_hotspot_server("bc_hotspot_1")
