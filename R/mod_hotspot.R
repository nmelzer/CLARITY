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
#' @importFrom htmlwidgets onRender
#' @import htmltools
#' @rawNamespace import(dplyr, except = combine)
#' @rawNamespace import(plotly, except = last_plot)
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
    useShinyjs(),
    shinyjs::hidden(
      shiny::fluidRow(id=ns("no.likelihood"),
          htmltools::br(),htmltools::br(),
          shiny::column(width=12,htmltools::tags$h3(htmltools::HTML("<span style=color:red;margin-left: 6px;>  Hotspots cannot be detected for the likelihood-based approach.</span>")))
    )),
    shinyjs::hidden(
      shiny::fluidRow(id=ns("wo.likelihood1"),
          shiny::column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')",style='text-decoration-line: underline;')," intervals.")),
          htmltools::br(),htmltools::br(),
          shinydashboard::box(width=12, title=tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
            shiny::column(width=12, h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return.")),
            shiny::column(width=12, shiny::radioButtons(ns("plot_hotspot_choice"),label="Plot hotspot only",inline=TRUE,selected="Yes",choices = c("yes"="Yes", "no"="No"))),
            shiny::column(width=11,style="padding-top:30px",id=ns("showhidePLOT"),plotly::plotlyOutput(ns("plothotspot2"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1")),#width="90%",height="90%",
            shiny::column(width=11,style="padding-top:80px", id=ns("textforNoHotspot"),htmltools::HTML("<b>No data to display - no regions meet the threshold criteria.</b>")),
          )
    )),
    htmltools::br(),
    shinyjs::hidden(
     shiny::fluidRow(id=ns("wo.likelihood2"),
        shinydashboard::box(title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",width=12,
                                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(id=ns("wo.likelihood2"),
            shiny::column(width=12, "Use the slider to define the threshold to hotspot region (standard deviation of recombination rate). This accordingly changes the figure and table. The default treshold is 2.5."),
            div(id=ns("showt1"),shiny::column(width=4, div(style="margin-bottom:30px"), sliderInput(ns("threshold"),"Threshold to define hotspot region",min = 0, max = 10, value = 2.5, step = 0.1),div(style="margin-bottom:60px"))),
            shiny::column(width=8,"")),
          shiny::fluidRow(id=ns("showhide_venn1"),
                      shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")),
                      shiny::column(width=6,shiny::actionButton(ns("showVenn"), "Show interactive venn diagram",style="background-color: #87CEFA")),
                      shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;"))),
          shiny::fluidRow(id=ns("showhide_venn2"),
                       shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")),
                       shiny::column(width=10,shiny::downloadButton(outputId=ns("download.venn.diagram"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1"),
                                          shiny::actionButton(ns("hideVenn"), "Hide venn diagram",style="background-color: #87CEFA"),
                                    shinyjs::hidden(shiny::actionButton(ns("ButtonResetVenn"),"Reset",style="background-color: #87CEFA"))),
                       shiny::column(width = 10,style="padding-top:30px", ""),
                       shiny::column(width=10,"When you click on a specific subset of interest, only the markers for that set are listed in the table. To return to all sets please use the button '",em("Reset"),"'."),
                       shiny::column(width=11,style='overflow-x: auto;',shiny::plotOutput(ns("venn_diagram_hot"),click = ns("venn_click"),width="20%",height="auto")),
                       shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")),
          ),
          htmltools::br(),
          shiny::fluidRow(
            shiny::column(width=12,DT::dataTableOutput(outputId=ns("tablehotspot"),height="auto",width="80%"),style = "overflow-y: auto;overflow-x: auto;")
          )
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
#' @param figure_rendered reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed analysis" sidebar elements.
#'
#' @keywords internal
#' @export
mod_hotspot_server=function(id, filter,breed.select,approach.info,add.approach.names,figure_rendered){
   shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    ## A delay was added to ensure the DOM is ready before executing shinyjs show and hide functions.
    ## Initialize which UI parts are shown.
    shinyjs::delay(3,
        if(match("Likelihood_male",approach.info$Approach,0)!=0 && nrow(approach.info)==1)# only likelihood approach selected
        {
           shinyjs::show(id=ns("no.likelihood"),asis=TRUE)
           shinyjs::hide(id=ns("wo.likelihood1"),asis=TRUE)
           shinyjs::hide(id=ns("wo.likelihood2"),asis=TRUE)
        }else{
           shinyjs::hide(id=ns("no.likelihood"),asis=TRUE)
           shinyjs::show(id=ns("wo.likelihood1"),asis=TRUE)
           shinyjs::show(id=ns("wo.likelihood2"),asis=TRUE)
        }
    )


    ####
    if(match("Likelihood_male",approach.info$Approach,0)!=0 && nrow(approach.info)==1) # only likelihood approach selected
    {
      figure_rendered(TRUE)
    }else
    {
      ## take out the Likelihood approach when included
      if(match("Likelihood_male",approach.info$Approach,0)!=0){
        approach.info<-approach.info[-match("Likelihood_male",approach.info$Approach),]
        start.lm=unlist(gregexpr("Lm",add.approach.names))
        if(start.lm==1)add.approach.names=substring(add.approach.names,4,nchar(add.approach.names))
        else if(nchar(add.approach.names)==5)add.approach.names=substring(add.approach.names,1,2)
        else add.approach.names=paste0(substring(add.approach.names,1,(start.lm-1)), substring(add.approach.names,start.lm+3,nchar(add.approach.names)))
      }

      # initialize
      adjacentRecRate<-venn_data<-keep<-NULL

      check.status.venn<-shiny::reactiveVal(FALSE)
      check.reset.button<-shiny::reactiveVal(FALSE)

      InputPlot_hotspot_venn<-NULL

      store<-shiny::reactiveValues(adjacentRecRate=NULL,header=TRUE)
      store.data<-shiny::reactiveValues(data=list())
      store.data.venn<-shiny::reactiveValues(venn=NULL,venn_data=NULL)

      # data
      load(system.file("extdata",paste0(breed.select,"/adjacentRecRate.Rdata"),package="CLARITY"))
      store$adjacentRecRate<-adjacentRecRate

      ## show / hide legend
      shiny::observe({
         shinyjs::toggle(id = "element3", condition = input$checkbox3, asis=TRUE)
      })

      ## plot size for Venn diagram and print-out
      if(nrow(approach.info)==2){
        width1= 460
        height1= 150
        width1.venn.plot=7
        height1.venn.plot=2.5 ## for print-out
      }else
      {
        width1= 600
        height1= 350
        width1.venn.plot=9
        height1.venn.plot=6
      }

      ## create table header
      store$header<-create_table_header3(approach.info=approach.info)

      ## react to hotspot plot (see output$plothotspot2) when it is fully rendered
      observeEvent(input$plothotspot2_rendered_breed, {
          shinyjs::enable(id="threshold")
          shinyjs::enable(id="plot_hotspot_choice")
          figure_rendered(TRUE)  # enable  "Breed analysis" sidebar elements
      })

      ## react to selected hotspot threshold and update corresponding elements
      shiny::observeEvent(input$threshold,{
        req(input$threshold)

        InputPlot_hotspot_venn<-NULL

        figure_rendered(FALSE)

        shinyjs::disable(id="threshold")
        shinyjs::disable(id="plot_hotspot_choice")
        shinyjs::hide(id="ButtonResetVenn")

        ### transform data
        store.data$data<-transformdata_hotspot(data.trans=store$adjacentRecRate,value=input$threshold,filter=filter,infos=approach.info,choice=input$plot_hotspot_choice)

        # render table
        output$tablehotspot= DT::renderDataTable({
           title1=paste0(breed.select,"_hotspot-geneticMap_BTA-",filter,"_",input$threshold,"_",add.approach.names)
           DT::datatable(store.data$data[[2]], container=store$header, extensions = c("Buttons"), rownames=FALSE,
                     options = list(searching=FALSE,dom='Bfrtip', columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                    dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                    pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
        }, server = TRUE)

        ## only plot data when they are not zero and send signal when plot is ready
        if(nrow(store.data$data[[1]])>0)
        {
          shinyjs::show("showhidePLOT")
          shinyjs::hide("textforNoHotspot")

          output$plothotspot2 <- plotly::renderPlotly({
              if(filter!="All")pp=scatterPlot_hotspot(dat1=store.data$data[[1]],file.name=paste0("Plot_",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice,"_",add.approach.names),max.plot=store.data$data[[4]])
              else  pp=scatterPlot_hotspot_all(dat1=store.data$data[[1]],file.name=paste0("Plot_",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice,"_",add.approach.names),max.plot=store.data$data[[4]])
              pp%>%toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                      // send info to shiny when the figure is fully rendered
                      Shiny.setInputValue(el.id + '_rendered_breed', {
                      timestamp: new Date().toISOString()
                  });
                }
              ")
          })
        }else
        {
           shinyjs::enable(id="threshold")
           shinyjs::enable(id="plot_hotspot_choice")
           figure_rendered(TRUE)
           shinyjs::hide("showhidePLOT")  # hide the UI part for the plot
           shinyjs::show("textforNoHotspot") # show the UI part that nothing exists for the hotspot threshold
        }

        ## part for venn diagram: venn diagram only generated when more than 2 approaches are selected and data are not zero
        if(nrow(store.data$data[[2]])>0 && nrow(approach.info)>=2)
        {
            ## show or hide corresponding UI part
            if(check.status.venn()==TRUE)shinyjs::click("hideVenn")
            else shinyjs::click("showVenn")

            colors<-create.colors(colo=approach.info$Color)

            ## create Venn data
            store.data.venn$venn <- ggVennDiagram::Venn(store.data$data[[3]])
            store.data.venn$venn_data <-process_venn_data(store.data.venn$venn)

            ## create Venn diagram
            InputPlot_hotspot_venn<-shiny::reactive(creating_venn(store.data.venn$venn_data,long.names=approach.info$Approach, abbreviations=approach.info$Abbreviation,venn.colors=colors))

            ## render Venn diagram
            output$venn_diagram_hot <- shiny::renderPlot({
              InputPlot_hotspot_venn()
            }, width=width1,height=height1)

            output$download.venn.diagram <- shiny::downloadHandler(
              filename= paste0("Venn-Diagram-",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_",add.approach.names,".png"),## insert methods,
              content = function(file) {
                shiny::showModal(modalDialog("Loading", footer=NULL))
                on.exit(removeModal())
              ggplot2::ggsave(file, plot = InputPlot_hotspot_venn(), device = "png",bg="white",width=width1.venn.plot,height=height1.venn.plot,units="in",dpi=300)
            })
       } else
        {
           # hide corresponding UI part
           shinyjs::hide("showhide_venn2")
           shinyjs::hide("showhide_venn1")
        }
      })

      ## react to changes in plot hotspot choice  (yes or no)
      shiny::observeEvent(input$plot_hotspot_choice,{
        req(input$plot_hotspot_choice)

        figure_rendered(FALSE)
        shinyjs::disable(id="threshold")
        shinyjs::disable(id="plot_hotspot_choice")

        ## create data
        dat.choice=transformdata_hotspot(data.trans=store$adjacentRecRate,value=input$threshold,filter=filter,infos=approach.info,choice=input$plot_hotspot_choice)

        ## check that data are not 0
        if(nrow(dat.choice[[1]])>0)
        {
          shinyjs::show("showhidePLOT")
          shinyjs::hide("textforNoHotspot")

          output$plothotspot2 <- plotly::renderPlotly({
            if(filter!="All")pp=scatterPlot_hotspot(dat1=dat.choice[[1]],file.name=paste0("Plot_",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice,"_",add.approach.names),max.plot=dat.choice[[4]])
            else  pp=scatterPlot_hotspot_all(dat1=dat.choice[[1]],file.name=paste0("Plot_",breed.select,"_hotspot_BTA-",filter,"_",input$threshold,"_","only-hotspot-",input$plot_hotspot_choice,"_",add.approach.names),max.plot=dat.choice[[4]])

            pp%>%toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
          })
        }else{
          shinyjs::enable(id="threshold")
          shinyjs::enable(id="plot_hotspot_choice")
          figure_rendered(TRUE)
          shinyjs::hide("showhidePLOT")
          shinyjs::show("textforNoHotspot")
        }
      },ignoreInit=TRUE)

      ## react to when the Venn diagram is clicked
      shiny::observeEvent(input$venn_click, {
          req(input$venn_click)

          if(is.null(store.data.venn$venn)==FALSE)
          {
              ## get data for the clicked Venn diagram subset
              data2=prepare_table_venn(venn.dat=store.data.venn$venn_data,venn=store.data.venn$venn,click=input$venn_click, table.bc=store.data$data[[2]])

              output$tablehotspot= DT::renderDataTable({
                title1=paste0(breed.select,"_hotspot-geneticMap_BTA-",filter,"_",input$threshold,"_set-",data2[[2]],"_",add.approach.names)

                DT::datatable(data2[[1]], container=store$header, extensions = c("Buttons"),rownames=FALSE,
                              options = list(searching=FALSE,dom='Bfrtip', columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                             dom = 'Bt',buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                             pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')))) #[order(data2()[[1]][,3]),]

              },server=TRUE)
              shinyjs::show("ButtonResetVenn")
              check.reset.button(TRUE)
          }
        },ignoreInit = TRUE)

      ## react to click on 'Reset' button and reset table to show all data visible in the Venn diagram
      shiny::observeEvent(input$ButtonResetVenn,{
        req(input$ButtonResetVenn)

        shinyjs::hide("ButtonResetVenn")

        if(check.reset.button()==TRUE)
        {
          output$tablehotspot= DT::renderDataTable({
            title1=paste0(breed.select,"_hotspot-geneticMap_BTA-",filter,"_",input$threshold,"_",add.approach.names)
            DT::datatable(store.data$data[[2]], container=store$header, extensions = c("Buttons"), rownames=FALSE,
                          options = list(searching=FALSE,dom='Bfrtip', columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                         dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                         pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
          }, server = TRUE)
        }

        check.reset.button(FALSE)
      },ignoreInit = TRUE)

      ## hide Venn diagram
      shiny::observeEvent(input$hideVenn,{
         shinyjs::show(id="showhide_venn1")
         shinyjs::hide(id="showhide_venn2")
        check.status.venn(TRUE)
      },ignoreInit = TRUE)

      ## show Venn diagram
      shiny::observeEvent(input$showVenn,{
        shinyjs::hide(id="showhide_venn1")
        shinyjs::show(id="showhide_venn2")
        check.status.venn(FALSE)
      },ignoreInit = TRUE)
   }
 })
}


## To be copied in the UI
# mod_hotspot_ui("mod_hotspot_1")

## To be copied in the server
# mod_hotspot_server("mod_hotspot_1")

