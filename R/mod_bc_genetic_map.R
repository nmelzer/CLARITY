# bc_genetic_map UI Function
#' @title mod_bc_genetic_map_ui and mod_bc_genetic_map_server
#' @description  A shiny module to generate the outcome for the tabpanel "Genetic map" for the sidebar "Breed comparison".
#'
#' @details  Independent from the user choice regarding chromosome selection, the data are prepared for the plot
#' using the function \link{transformdata_genetic_map_bc}.Depending on the user's selection, when a specific chromosome is chosen, the function \link{makePlot_geneticMaps} is used to create the plot,
#' otherwise the function \link{makePlot_all_geneticMaps} is used to create the plot. In addition, when all chromosome is selected than the corresponding plot
#' is cached to increase the app performance.
#' This module also shows a Venn diagram of corresponding markers. The input data are prepared
#' using the function \link{process_venn_data}, corresponding colors for Venn sets are created using the function \link{create.colors} and plotted using the function \link{creating_venn}.
#' The user may select a Venn diagram subset and the
#' table will be reduced accordingly using the function \link{prepare_table_venn}.
#' Finally, the quality of the likelihood approach is also shown here by means of a traffic light (also termed as likelihood quality signal (\link{make_traffic_light})).
#'
#' @rdname mod_bc_genetic_map
#'
#' @param id module id
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @importFrom ggplot2 ggsave
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput renderPlotly toWebGL
#' @importFrom writexl write_xlsx
#' @rawNamespace import(shinyjs, except = runExample)
#' @importFrom ggVennDiagram Venn
#' @importFrom htmlwidgets onRender
#' @importFrom dplyr semi_join
#'
#' @export
#' @seealso
#' *\link{transformdata_genetic_map_bc} \cr
#' * \link{makePlot_all_geneticMaps} \cr
#' * \link{makePlot_geneticMaps}
#' * \link{process_venn_data} \cr
#' * \link{create.colors}
#' * \link{creating_venn}\cr
#' * \link{prepare_table_venn} \cr
#' * \link{make_traffic_light}


mod_bc_genetic_map_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    useShinyjs(),
    htmltools::br(), htmltools::br(),
     shinyjs::hidden(
       shiny::fluidRow(id=ns("output1"),  ## when specific chromosome is selected
              shinydashboard::box(title= tags$b("Interactive graphical visualization: Genetic vs. physical distance"),status="danger",width=12,
                        solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                        shiny::column(width=9,""),
                        shinyjs::hidden(
                            shiny::column(width=2,id=ns("showhideSignal1"),tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                            shiny::plotOutput(ns("TrafficLight_1"),width="70%",height="auto"))
                        ),
                        shiny::column(width=11, plotly::plotlyOutput(ns("genetic1"),width="100%",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1")) #width="90%",height="10%"
                      ),
                      htmltools::br(),
                      htmltools::br(),
                      shinydashboard::box(title= tags$b("Genetic distance in selected interval"),status="danger",width=12,
                                           solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                          shiny::fluidRow(
                                    shiny::column(width=12,"Use the slider to define the range of interest and then click on button", em('Apply selected interval'),".
                                         This accordingly changes the venn diagram and table. Use the button",em('Reset to all'),"to obtain corresponding results for whole chromosome again."),
                                    shiny::column(width = 12,style="padding-top:30px", ""),
                                    shiny::column(width=10,shiny::uiOutput(ns("sliderRangeMap_bc")),shiny::column(width=2,"")),
                                    shiny::column(5,shiny::actionButton(ns("ButtonshowRangeMap_bc"),"Apply selected interval"),
                                                  shinyjs::hidden(shiny::actionButton(ns("ButtonAll"),"Reset to all")))
                          ),
                          htmltools::br(),
                          htmltools::hr(style = "border-top: 1px solid #68838B;"),
                          htmltools::br(),
                          shinyjs::hidden(
                            shiny::fluidRow(id=ns("venn_chr1"),column(width=6,actionButton(ns("venn_chr_bc1"), "Show interactive venn diagram",style = "color: black;
                            background-color: #87CEFA"))
                          )),
                          shinyjs::hidden(
                            shiny::fluidRow(id=ns("venn_chr2"),
                                   shiny::column(width=12,shiny::downloadButton(outputId=ns("download.venn.diagram_bc"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1"),
                                                 shiny::actionButton(ns("venn_chr_bc2"), "Hide venn diagram",style="background-color: #87CEFA"),
                                                 shinyjs::hidden(shiny::actionButton(ns("ButtonReset"),"Reset",style="background-color: #87CEFA"))),
                                   shiny::column(width = 12,style="padding-top:30px", ""),
                                   shiny::column(width=12,"When you click on a specific subset of interest, only the markers for that set are listed in the table. To return to all sets please use the button '",em("Reset"),"'."),
                                   shiny::column(width=11,style='overflow-x: auto;',shiny::plotOutput(ns("venn_diagram"), click = ns("plot_click"),height="auto",width = "20%"))
                          )),
                          htmltools::br(),
                          htmltools::hr(style = "border-top: 1px solid #68838B;"),
                          htmltools::br(),
                          shiny::fluidRow(
                            shiny::column(width=11,style='overflow-x: auto;',DT::dataTableOutput(outputId=ns("rangeMaps"),width="80%"))
                          )
                      )
      )),
      shinyjs::hidden(
        shiny::fluidRow(id=ns("output2"), ## when all chromosome is selected
                           shinyjs::hidden(shiny::column(width=1,shiny::actionButton(inputId=ns("help_button2"),label="not shown"))),
                           shiny::column(width=2,shiny::downloadButton(outputId=ns("download.gm.bc"),label="Save figure",class="butt1")),
                           shiny::column(width=1),
                           shiny::column(width=1,shiny::radioButtons(inputId=ns("filetype.bc"), "Select file extension:", choices = c("CSV" = "csv", "Excel" = "xlsx"))),
                           shiny::column(width=2,shiny::downloadButton(outputId=ns("downloadData_breeds"),label="Save genetic map data",class="butt1")),
                           shiny::column(1),
                           shinyjs::hidden(shiny::column(width=2,id=ns("showhideSignal2"),tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                                            shiny::plotOutput(ns("TrafficLight_2"),width="70%",height="auto"))
                           ),
                           shinydashboard::box(width=12,style='overflow-x: scroll;height:1000px;overflow-y:scroll;',
                                 shinycssloaders::withSpinner(shiny::plotOutput(ns("figure_breeds"),width="1600px",height="2800px"),color="#0dc5c1",proxy.height = "200px") ##
                           )
      ))
  )
}

# Module Server
#' @rdname mod_bc_genetic_map
#'
#' @param filter character contains the selected chromosome
#' @param geneticMap.bc list of genetic maps from the selected breeds (each list element contains a breed)
#' @param names.files character containing the concatenated breed names and is used as an addition to the file name
#' @param make.traff.light.bc object containing the ggplot for the likelihood quality signal (\link{make_traffic_light})
#' @param breed.infos data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param approach data frame containing the predefined settings and names for the selected approach (\link{table_approach_information})
#' @param figure_rend2_gm reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed comparison" sidebar elements.
#' @param state numeric indicating if the threshold slider UI should be created (0; new module ID) or not (1; reused module ID and settings retained). State of the module is tracked on the main server side.
#'
#' @export
mod_bc_genetic_map_server <- function(id, filter,geneticMap.bc,names.files,make.traff.light.bc,breed.infos,approach,figure_rend2_gm,state){
  moduleServer(id, function(input,output,session){
    ns <- session$ns

    ## A delay was added to ensure the DOM is ready before executing shinyjs show, hide or click functions.
    ## Initialize which UI parts are shown.
    shinyjs::delay(3,
      if(filter!="All") # when specific chromosome is selected
      {
        shinyjs::show(id=ns("output1"),asis=TRUE)
        shinyjs::hide(id=ns("output2"),asis=TRUE)
        shinyjs::show(id=ns("venn_chr2"),asis=TRUE)

        ## check for likelihood approach render traffic light
        if(match("Likelihood_male",approach$Approach,0)!=0)
        {
          shinyjs::show(id=ns("showhideSignal1"),asis=TRUE)

          height2=ifelse(nrow(breed.infos)==2,45,55)
          output$TrafficLight_1 <- shiny::renderPlot({
            make.traff.light.bc()
          }, width=165, height=height2)
        }
      }else    # when all chromosome is selected
      {
          shinyjs::hide(id=ns("output1"),asis=TRUE)
          shinyjs::show(id=ns("output2"),asis=TRUE)

          ## check for likelihood approach render traffic light
          if(match("Likelihood_male",approach$Approach,0)!=0)
          {
             shinyjs::show(id=ns("showhideSignal2"),asis=TRUE)
             height2=ifelse(nrow(breed.infos)==2,45,55)
             output$TrafficLight_2 <- shiny::renderPlot({
                    make.traff.light.bc()
             }, width=165, height=height2)
           }
           ## A help button was added to ensure correct access to the information when the ggplot (generated or cached) is fully rendered.
           ## This ensures the correct timing to show the corresponding sidebar elements for Breed comparison.
           shinyjs::click(id=ns("help_button2"),asis=TRUE)
      }
    )


    ## specific chromosome is selected
    if(filter!="All")
    {
      #### initialize for specific chromosome
      range.used <-table.complete<-filename.venn<-InputPlot4<-file.name.tab0<-change.filename<-Marker<-Name<-NULL

      #### data
      ## create table header
      sketch2<-create_table_header_bc2(breed.infos=breed.infos)

      ## filter for selected chromosome and selected approach over the selected breeds and store in a new list
      df.pp <- mapply(function(x) {
        rem=geneticMap.bc[[x]][geneticMap.bc[[x]]$Chr%in%filter,c(2,1,3,4,grep(paste0(approach$Abbreviation,"_"),colnames(geneticMap.bc[[x]])))]
        rem
      },breed.infos$Name , SIMPLIFY = FALSE)

      df.p.table=do.call("rbind",df.pp)
      min.pos<-min(df.p.table$Mbp_position, na.rm = TRUE)
      max.pos<-max(df.p.table$Mbp_position, na.rm = TRUE)

      # prepare data table
      if(approach$Approach!="Likelihood_male")
      {
        tab.bc=merge(as.data.frame(df.pp[[1]][,-c(grep("recrate",colnames(df.pp[[1]])))]),as.data.frame(df.pp[[2]][,-c(grep("recrate",colnames(df.pp[[2]])))]),by.x=2,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),6:8]
        if(nrow(breed.infos)==3)tab.bc=merge(tab.bc,as.data.frame(df.pp[[3]][,-c(grep("recrate",colnames(df.pp[[3]])))]),by.x=1,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),10:12]
      }
      else{
        tab.bc=merge(as.data.frame(df.pp[[1]]),as.data.frame(df.pp[[2]]),by.x=2,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),6:8]
        if(nrow(breed.infos)==3)tab.bc=merge(tab.bc,as.data.frame(df.pp[[3]]),by.x=1,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),10:12]
      }

      take.out1=grep("Chr",colnames(tab.bc))[-1]
      take.out2=grep("bp_position",colnames(tab.bc))[-c(1:2)]
      tab.bc=tab.bc[,-c(take.out1,take.out2)]
      tab.bc<-tab.bc[order(tab.bc$Mbp_position),]

      table.complete<<-shiny::reactive(tab.bc)

      ## create slider when state is set to 0
      if(state==0)
      {
        output$sliderRangeMap_bc <- shiny::renderUI({
          shiny::sliderInput(ns("rangeMap_bc"), "Range based on Mbp", min=min.pos, max=max.pos, step=0.1, value=c(20,30))  ## changed from 0.1 to 1
        })
      }

      #### transform data to plot data
      plot.dat1<-transformdata_genetic_map_bc(input=df.pp,breed.infos,approach)

      ## React to genetic map plot (see output$genetic1) when it is fully rendered
      ## and update the figure_rend2_gm() to enable the "Breed comparison" sidebar elements
      observeEvent(input$genetic1_rendered_breed2, {
        figure_rend2_gm(TRUE)
      })

      ## render plot and send signal when ready
      output$genetic1 <- plotly::renderPlotly({
        output1.filename=paste0(names.files,"_geneticMap_BTA-",filter,"-",approach$Abbreviation)
        pp=makePlot_geneticMaps(dat=plot.dat1[[1]],name.file=output1.filename)
        pp%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed2', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
      })

      #### rendering the table
      output$rangeMaps=DT::renderDataTable({
         file.name.tab=paste0(names.files,"-geneticMap_BTA-",filter,"-",approach$Abbreviation)
         DT::datatable(table.complete(),filter="none",container=sketch2,extensions = c("Buttons"),  options=list(searching = FALSE,dom='Bfrtip',
                       buttons = list('pageLength', 'copy',list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                      pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c(10, 20,30,'All'))),rownames=FALSE,escape=FALSE)
      })

      #### all for Venn diagram
      ## plot size for Venn diagram and print-out
      if(nrow(breed.infos)==2)
      {
        width1= 400
        height1= 150
        width1.venn.plot=6
        height1.venn.plot=2.5
      }else
      {
        width1= 600
        height1= 350
        width1.venn.plot=9
        height1.venn.plot=6
      }

      ## prepare data for Venn
      venn.data.chr<-mapply(function(x) {
        rem=geneticMap.bc[[x]][geneticMap.bc[[x]]$Chr%in%filter,1]
        rem
      }, breed.infos$Name, SIMPLIFY = FALSE)

      venn<-ggVennDiagram::Venn(venn.data.chr)
      venn_data <-process_venn_data(venn=venn)
      col1<-create.colors(colo=breed.infos$Color)

      ## create Venn plot
      InputPlot4 <- shiny::reactive(creating_venn(venn_data,long.names=breed.infos$Name, abbreviations=breed.infos$Abbreviation,venn.colors=col1))

      ## render Venn plot
      output$venn_diagram <- shiny::renderPlot({
        InputPlot4()
      },width=width1,height=height1)


      change.filename<<-paste0(names.files,"-geneticMap_BTA-",filter)

      ## react when Venn diagram is clicked
      shiny::observeEvent(input$plot_click, {
        req(input$plot_click)

        shinyjs::show("ButtonReset")

        ## get data for the clicked Venn diagram subset
        data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$plot_click, table.bc= table.complete())

        output$rangeMaps=DT::renderDataTable({
           file.name.tab2=paste0(change.filename,"-set-",data2[[2]],"-",approach$Abbreviation)
           DT::datatable(data2[[1]][order(data2[[1]][,3]),],container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                                    options = list(searching=FALSE,dom='Bfrtip',
                                      dom = 'Bt', buttons = list('pageLength', 'copy',list(extend='csv',title=file.name.tab2),list(extend='excel',title=file.name.tab2)),
                                      pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All'))))

        },server=FALSE)
      },ignoreInit = TRUE)

      #### react to selected slider input and update corresponding elements
      shiny::observeEvent(input$ButtonshowRangeMap_bc,{
         req(input$ButtonshowRangeMap_bc)

         df.p2<-venn.data.chr.2<-list()

         range<-input$rangeMap_bc

         shinyjs::show("ButtonAll")
         shinyjs::hide("ButtonReset")

         for(ij in 1:length(names(df.pp)))
         {
             range1 <- which(as.numeric(df.pp[[ij]]$Mbp_position)>=input$rangeMap_bc[1])
             range2 <- which(as.numeric(df.pp[[ij]]$Mbp_position)<=input$rangeMap_bc[2])

             if(length(range1)==0)range1=1
             if(length(range2)==0)range2=dim(df.pp[[1]])[1]

             df.p2[[names(df.pp)[ij]]]=df.pp[[ij]][range1[1]:range2[length(range2)],]
             venn.data.chr.2[[breed.infos$Abbreviation[ij]]]=df.p2[[names(df.pp)[ij]]]$Name
         }

         range1 <- which(as.numeric(tab.bc$Mbp_position)>=(input$rangeMap_bc[1]))
         range2 <- which(as.numeric(tab.bc$Mbp_position)<=(input$rangeMap_bc[2]))
         range.used<<-1

         table.complete<<-shiny::reactive(tab.bc[range1[1]:range2[length(range2)],])

         change.filename<<-paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2]) ## for click venn diagram -change title

         ## render table
         output$rangeMaps=DT::renderDataTable({
           file.name.tab<<-paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2],"-",approach$Abbreviation)
           DT::datatable(table.complete(),filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                                                                        pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
         },server=FALSE)

         output1.filename=paste0(names.files,"_geneticMap_BTA-",filter,"_range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2],"-",approach$Abbreviation)

         ## render plot and send signal when ready
         output$genetic1 <- plotly::renderPlotly({
             figure_rend2_gm(FALSE)

            data.plot.new<-dplyr::semi_join(plot.dat1[[1]], table.complete(),join_by(Marker==Name))
            pp=makePlot_geneticMaps(dat=data.plot.new,name.file=output1.filename)

            pp%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed2', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
          })

         file.name.tab0<<-paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2]) # may not working

         ## change venn diagram accordingly
         filename.venn<<-shiny::reactive(paste0("Venn-Diagram-",names.files,"_BTA-",filter,"_range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2],"-",approach$Abbreviation,".png"))
         venn  <- ggVennDiagram::Venn(venn.data.chr.2)
         venn_data <-process_venn_data(venn)

         ## create venn diagram
         col1<-create.colors(colo=breed.infos$Color)
         InputPlot4 <<- shiny::reactive(creating_venn(venn_data,long.names=breed.infos$Name, abbreviations=breed.infos$Abbreviation,venn.colors=col1))

         ## render venn diagram
         output$venn_diagram <- shiny::renderPlot({
           InputPlot4()
         },width=width1,height=height1)

      })

      ### react to "Reset to all" button click and reset slider to all and update corresponding elements
      shiny::observeEvent(input$ButtonAll,{
        req(input$ButtonAll)

        shinyjs::hide("ButtonAll")
        shinyjs::hide("ButtonReset")

        range.used<<-NULL
        table.complete<<-shiny::reactive(tab.bc)

        change.filename<<-paste0(names.files,"-geneticMap_BTA-",filter)

        ## render table
        output$rangeMaps=DT::renderDataTable({
          file.name.tab<-paste0(names.files,"-geneticMap_BTA-",filter,"-",approach$Abbreviation)
           DT::datatable(table.complete(),filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy',list(extend='csv',title=file.name.tab), list(extend='excel',title=file.name.tab)), ## changed
                                                                          pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
        },server=FALSE)

        ## reset slider
        output$sliderRangeMap_bc <- shiny::renderUI({
          shiny::sliderInput(inputId=ns("rangeMap_bc"), label= "Range based on Mbp",  min=min.pos, max=max.pos,step=0.1, value=c(min.pos,max.pos))
        })

        ## render plot and send signal when ready
        output$genetic1 <- plotly::renderPlotly({
          figure_rend2_gm(FALSE)

          output1.filename=paste0(names.files,"_geneticMap_BTA-",filter,"-",approach$Abbreviation)
          pp=makePlot_geneticMaps(dat=plot.dat1[[1]],name.file=output1.filename)
          pp%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed2', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
        })

        ## Venn diagram
        filename.venn <<- shiny::reactive(paste0("Venn-Diagram-",names.files,"_BTA-",filter,"-",approach$Abbreviation,".png"))
        venn <- ggVennDiagram::Venn(venn.data.chr)
        venn_data <-process_venn_data(venn) ## also changed

        ## create Venn diagram
        col1<-create.colors(colo=breed.infos$Color)
        InputPlot4 <<- shiny::reactive(creating_venn(venn_data,long.names=breed.infos$Name, abbreviations=breed.infos$Abbreviation,venn.colors=col1))

        ## render Venn diagram
        output$venn_diagram <- shiny::renderPlot({
            InputPlot4()
        },width=width1,height=height1)

     })

      ## show Venn diagram
      shiny::observeEvent(input$venn_chr_bc1,{
        shinyjs::hide(id="venn_chr1")
        shinyjs::show(id="venn_chr2")

        output$venn_diagram <- shiny::renderPlot({
          InputPlot4()
        },width=width1,height=height1)
      })

      ## hide Venn diagram
      shiny::observeEvent(input$venn_chr_bc2,{
        shinyjs::show(id="venn_chr1")
        shinyjs::hide(id="venn_chr2")
      })

      ## react to "Reset" button click and reset table to show all data visible in the Venn diagram
      shiny::observeEvent(input$ButtonReset,{
        req(input$ButtonReset)

        shinyjs::hide(id="ButtonReset")

        if(is.null(range.used)==TRUE)file.name.tab=paste0(names.files,"-geneticMap_BTA-",filter,"-",approach$Abbreviation)
        else file.name.tab=paste0(names.files,"-geneticMap_BTA-",filter,"_range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2],"-",approach$Abbreviation)

        ## render table
        output$rangeMaps=DT::renderDataTable({
           DT::datatable(table.complete(),filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)), ## changed
                                                                                      pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
        },server=FALSE)
      })

      ## download Venn diagram
      filename.venn<<-shiny::reactive(paste0("Venn-Diagram-",names.files,"_BTA-",filter,"-",approach$Abbreviation,".png"))

      ## download Venn diagram
      output$download.venn.diagram_bc <- shiny::downloadHandler(
         filename=function() filename.venn(),
         content = function(file) {
            showModal(modalDialog("Loading", footer=NULL))
            on.exit(removeModal())
            ggsave(file, plot = InputPlot4(), device = "png",bg="white",width=width1.venn.plot,height=height1.venn.plot,units="in",dpi=300)
      })
    }else  ## when all chromosome is selected
    {
      ## a reactive value to track if the plot is ready
      plot_rend_ready<-shiny::reactiveVal(FALSE)

      ## react to full genetic map plot (cached or generated) via reactive value plot_rend_ready() when it is fully rendered
      ## and update the figure_rend2_gm() to enable the "Breed comparison" sidebar elements
      shiny::observeEvent(plot_rend_ready(),{
        if(plot_rend_ready()==TRUE)figure_rend2_gm(TRUE)
        else figure_rend2_gm(FALSE)
      })

      ## react to help button2 click: transform data, generate plot and send signal when plot is ready rendered
      shiny::observeEvent(input$help_button2,{
        req(input$help_button2)

        ##  data
        ll2=transformdata_genetic_map_bc(input=geneticMap.bc,breed.infos=breed.infos,approach)

        ## create plot
        plot_gm_all_det<-shiny::reactive({
          makePlot_all_geneticMaps(ll.gm.s=ll2)
        })

        ## render plot
        output$figure_breeds<-shiny::renderPlot({
          plot_gm_all_det()
        })%>%shiny::bindCache(breed.infos$Name, filter,approach$Abbreviation)

        ## plot is ready change the state of the corresponding reactive value
        plot_rend_ready(TRUE)

        ## download the figure
        output$download.gm.bc <- shiny::downloadHandler(
          filename = paste0(names.files,"-geneticMaps-",approach$Abbreviation,".png"),
          content = function(file) {
            showModal(modalDialog("Loading", footer=NULL))
            on.exit(removeModal())
            ggplot2::ggsave(file, plot = plot_gm_all_det(), device = "png",width=20,height=40,units="in",dpi=300)
          })
      },ignoreInit = TRUE)

      #### data
      ## create table header
      header.breed.bc<-create_table_header_bc2_all(breed.infos=breed.infos)

      ## filter for selected approach over the selected breeds and store in a new list
      df.pp <- mapply(function(x) {
        rem=geneticMap.bc[[x]][,c(2,1,3,4,grep(paste0(approach$Abbreviation,"_"),colnames(geneticMap.bc[[x]])))]
        rem
      },breed.infos$Name , SIMPLIFY = FALSE)

      df.p.table=do.call("rbind",df.pp)

      # prepare data table
      if(approach$Approach!="Likelihood_male")
      {
        tab.bc=merge(as.data.frame(df.pp[[1]][,-c(grep("recrate",colnames(df.pp[[1]])))]),as.data.frame(df.pp[[2]][,-c(grep("recrate",colnames(df.pp[[2]])))]),by.x=2,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),6:8]
        if(nrow(breed.infos)==3)tab.bc=merge(tab.bc,as.data.frame(df.pp[[3]][,-c(grep("recrate",colnames(df.pp[[3]])))]),by.x=1,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),10:12]
      }
      else{
        tab.bc=merge(as.data.frame(df.pp[[1]]),as.data.frame(df.pp[[2]]),by.x=2,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),6:8]
        if(nrow(breed.infos)==3)tab.bc=merge(tab.bc,as.data.frame(df.pp[[3]]),by.x=1,by.y=2,all.x=TRUE,all.y=TRUE)
        if(length(which(is.na(tab.bc$Chr.x)==TRUE))!=0)tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),2:4]=tab.bc[which(is.na(tab.bc$Chr.x)==TRUE),10:12]
      }

      take.out1=grep("Chr",colnames(tab.bc))[-1]
      take.out2=grep("bp_position",colnames(tab.bc))[-c(1:2)]
      tab.bc=tab.bc[,-c(take.out1,take.out2)]
      tab.bc<-tab.bc[order(tab.bc$Mbp_position),]
      colnames(tab.bc)<-header.breed.bc

      table.complete<<-shiny::reactive(tab.bc)

      ## download table
      output$downloadData_breeds <- shiny::downloadHandler(
        filename = function(){
          paste0(names.files,"-geneticMap_BTA-ALL_",approach$Abbreviation, ifelse(input$filetype.bc == "csv", ".csv", ".xlsx"))
        },
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          if(input$filetype.bc=="csv")utils::write.table(table.complete(), file, row.names = FALSE,col.names = TRUE,sep=",")
          else if(input$filetype.bc=="xlsx")writexl::write_xlsx(table.complete(), file)
        }
      )
    }
  })
}

## To be copied in the UI
# mod_bc_genetic_map_ui("bc_genetic_map_1")

## To be copied in the server
# mod_bc_genetic_map_server("bc_genetic_map_1")
