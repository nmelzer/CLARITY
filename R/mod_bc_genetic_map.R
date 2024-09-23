# bc_genetic_map UI Function
#' @title mod_bc_genetic_map_ui and mod_bc_genetic_map_server
#' @description  A shiny module to generate the outcome for the tabpanel "Genetic map" for the sidebar "Breed comparison".
#'
#' @details When a specific chromosome is selected by the user then the function \link{makePlot_geneticMaps_bc} is used to create the plot.
#' If all chromosomes are selected then genetic map data are prepared for the plot using the function \link{transformdata_genetic_map_bc}
#' and plotted using the function \link{makePlot_all_geneticMaps_bc}. Moreover, the corresponding plot is cached to increase the app performance.
#'  This module also shows a Venn diagram of corresponding markers. The input data are prepared
#'  using the function \link{process_venn_data} and plotted using the function \link{creating_venn}. The user may select a Venn diagram subset and the
#'  table will be reduced accordingly using the function \link{prepare_table_venn}. Finally, the quality of the likelihood approach is also shown here by means of a traffic light (also termed as likelihood quality signal).
#'
#' @rdname mod_bc_genetic_map
#'
#' @param id module id
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @rawNamespace import(plotly, except = last_plot)
#' @rawNamespace import(shinyjs, except = runExample)
#' @rawNamespace importFrom(utils,write.csv)
#' @rawNamespace importFrom(writexl,write_xlsx)
#' @importFrom RVenn Venn
#' @export
#' @seealso \link{transformdata_genetic_map_bc}, \link{makePlot_geneticMaps_bc}, \link{makePlot_all_geneticMaps_bc}, \link{process_venn_data}, \link{creating_venn}, \link{prepare_table_venn} and \link{make_traffic_light}


mod_bc_genetic_map_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::br(), htmltools::br(),
     shiny::fluidRow(id=ns("output1"),  ## single chromosome output
                     shinydashboard::box(title= tags$b("Interactive graphical visualization: Genetic vs. physical distance"),status="danger",width=12,
                        solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                        shiny::column(width=9,""),
                        shiny::column(width=2,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                      plotOutput(ns("TrafficLight_2"),width="80%",height="auto")),
                        shiny::column(width=5, plotly::plotlyOutput(ns("genetic1"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1")),
                        shiny::column(width=5, plotly::plotlyOutput(ns("genetic2"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1"))
               ),
               htmltools::br(),
               htmltools::br(),
               ########## new insert - take may out again
               box(title= tags$b("Genetic distance in selected interval"),status="danger",width=12,
                   solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                   shiny::fluidRow( shiny::column(width=12,"Use the slider to define the range of interest and then click on button", em('Apply selected intervall'),".
                                         This accordingly changes the venn diagram and table. Use the button",em('Reset to all'),"to obtain corresponding results for whole chromosome again."),
                                    shiny::column(width = 12,style="padding-top:30px", ""),
                                    shiny::column(width=5,shiny::uiOutput(ns("sliderRangeMap_bc"))),
                                    shiny::column(5,shiny::actionButton(ns("ButtonshowRangeMap_bc"),"Apply selected intervall"),
                                                  shiny::actionButton(ns("ButtonAll"),"Reset to all"))
                   ),
                   htmltools::br(),
                   htmltools::hr(style = "border-top: 1px solid #68838B;"),
                   htmltools::br(),
                   shiny::fluidRow(shinyjs::useShinyjs(),id=ns("venn_chr1"),column(width=6,actionButton(ns("venn_chr_bc1"), "Show interactive venn diagram",style = "color: black;
                            background-color: #87CEFA"))),
                   shiny::fluidRow(shinyjs::useShinyjs(),id=ns("venn_chr2"),
                                   shiny::column(width=12,shiny::downloadButton(outputId=ns("download.venn.diagram_bc"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1"),
                                                 shiny::actionButton(ns("venn_chr_bc2"), "Hide venn diagram",style="background-color: #87CEFA"),
                                                 shiny::actionButton(ns("ButtonReset"),"Reset",style="background-color: #87CEFA")),
                                   shiny::column(width = 12,style="padding-top:30px", ""),
                                   shiny::column(width=8,"When you click on a specific subset of interest, only the markers for that set are listed in the table. To return to all sets please use the button '",em("Reset"),"'."),
                                   shiny::column(width=4,""),
                                   shiny::column(width=12,""),
                                   shiny::column(width=6,shiny::plotOutput(ns("venn_diagram"), click = ns("plot_click"),width = "100%",height = "500px",inline=TRUE)
                                                      ),column(width=6,""),
                   ),
                   htmltools::br(),
                   htmltools::hr(style = "border-top: 1px solid #68838B;"),
                   htmltools::br(),
                   shiny::column(width=12,downloadButton(ns("csvstore2"),label="CSV",class="butt1",icon=NULL,style="margin-right: 5px; width=100px; position:absolute;left:0em;"),
                                  downloadButton(ns("excelstore2"),label="Excel",icon=NULL,class="butt1",style="margin-left: 40px")),
                   shiny::fluidRow(
                            shiny::column(width=8,DT::dataTableOutput(outputId=ns("rangeMaps")))

                   ))
      ),
      shiny::fluidRow(id=ns("output2"), ## all chromosomes output
                      shiny::column(1),
                      ## trouble with radiobuttons - lighthouse always show error when using radiobuttons so changed to selectInput - 16.08.2024
                      #shiny::column(3,shiny::radioButtons(inputId=ns("show_method_bc"),label= "Select approach:",
                      #   choices=list("deterministic","likelihood-based"),selected="deterministic")),
                      shiny::column(3,shiny::selectInput(inputId=ns("show_method_bc"),label= "Select approach:",
                                                         choices=list("deterministic","likelihood-based"),selected = "deterministic",size=2,selectize=FALSE)),
                      shiny::fluidRow(id=ns("show_bc_method_1"),column(width=1,""),column(width=4,shiny::downloadButton(outputId=ns("download.gm.bc"),label="Save figure",class="butt1")),
                      shinydashboard::box(width=12,style='overflow-x: scroll;height:1000px;overflow-y:scroll;',
                         shinycssloaders::withSpinner(plotOutput(ns("figure_det"),width="1600px",height="2800px"),color="#0dc5c1",proxy.height = "200px")) ##
                  ),

                  shiny::fluidRow(id=ns("show_bc_method_2"),
                                  shiny::column(width=1,""),
                                  shiny::column(width=4,shiny::downloadButton(outputId=ns("download.gm.bc2"),label="Save figure",class="butt1")),
                                  shiny::column(width=2,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                               plotOutput(ns("TrafficLight_3"),width="80%",height="auto")),
                                  shinydashboard::box(width=12,style='overflow-x: scroll;height:1000px;overflow-y: scroll;',
                                      shinycssloaders::withSpinner(shiny::plotOutput(ns("figure_lik"),width="1600px",height="2800px"),color="#0dc5c1",proxy.height = "200px"))
                )
      )
  )
}


# Module Server
#' @rdname mod_bc_genetic_map
#'
#' @param filter character contains the selected chromosome
#' @param breed.select.bc vector containing the names of selected breeds
#' @param geneticMap.bc list of genetic maps from the selected breeds (each list element contains a breed)
#' @param color.shape.def data frame containing the definition for coloring, shapes, size for plots
#' @param names.bc.venn vector containing the first the letter of the selected breed names
#' @param names.files string concatenate breed names
#' @param make.traff.light.bc object containing the ggplot for the likelihood quality signal
#'
#' @export
mod_bc_genetic_map_server <- function(id, filter, breed.select.bc, geneticMap.bc,color.shape.def,names.bc.venn,names.files,make.traff.light.bc){
  moduleServer(id, function(input, output, session,mtl.bc){

    ns <- session$ns
    chr <-filter
    range.used <-NULL

    #### make Table colummn
    ## Table header with internal link to methodology
    thead<-tr<-th<-NULL
    sketch2 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=3,""),
          th(colspan=(3+2*length(breed.select.bc)),"Genetic Map")
        ),
        tr(
          th(colspan=3,""),
          th(colspan=length(breed.select.bc),tags$a(href="#","Deterministic approach", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan = length(breed.select.bc),tags$a(href="#","Likelihood-based approach", onclick = "openTab('methodology')",style='text-decoration-line: underline;'))
        ),
        tr(
          th(colspan=2,""),
          th(colspan=1,"Physical Map"),
          lapply(colspan=1,rep(breed.select.bc,2),th)
        ),
        tr(
          th(colspan=1,"Marker"),
          th(colspan=1,"Chr"),
          th(colspan=1,"Position (bp)"),
          lapply(colspan=1,rep("Position (cM)",length(breed.select.bc)*2),th)
        )
      )
    ))



    ## all for selected chromosome
    if(filter!="All")
    {
      shinyjs::show(id="output1")
      shinyjs::hide(id="output2")
      shinyjs::hide(id="output3")
      shinyjs::hide(id="venn_chr1")
      shinyjs::show(id="venn_chr2")
      shinyjs::hide(id="ButtonReset")

      ## render traffic light
      if(length(breed.select.bc)==2)height2=45
      else height2=55
      output$TrafficLight_2 <- renderPlot({
        make.traff.light.bc()
      }, width=165, height=height2) ## vorher 80
      ###########################

      df.p=list(); df.pp=list();venn.data.chr=list()
      min.pos<-max.pos<-NULL

      ## data preparation when specific chromosome is selected
      for(i in 1:length(breed.select.bc))
      {
        df.p[[breed.select.bc[i]]]=geneticMap.bc[[i]][which(geneticMap.bc[[i]]$Chr==chr),]
        df.pp[[i]]=df.p[[breed.select.bc[i]]][,c(2,1,4,6,5)]

        ### determination the limits for the sliderInput
        if(i==1){
          min.pos<-df.p[[i]]$Mbp_position[1]
          max.pos<-df.p[[i]]$Mbp_position[length(df.p[[i]]$Mbp_position)]
        }
        else{
          if(min.pos>df.p[[i]]$Mbp_position[1])min.pos<-df.p[[i]]$Mbp_position[1]
          if(max.pos<df.p[[i]]$Mbp_position[length(df.p[[i]]$Mbp_position)])max.pos<-df.p[[i]]$Mbp_position[length(df.p[[i]]$Mbp_position)]
        }
        venn.data.chr[[names.bc.venn[i]]]=df.p[[breed.select.bc[i]]]$Name
      }

      use1=4; use2=5
      for(ik in 1:(length(breed.select.bc)-1))
      {
        use1=use1+2
        use2=use2+2
        if(ik == 1)
        {
         tab.bc=merge(as.data.frame(df.pp[[1]]),as.data.frame(df.pp[[2]]),by.x=1,by.y=1,all.x=TRUE,all.y=TRUE)
         tab.bc[which(is.na(tab.bc[,use2])==F),3]=tab.bc[which(is.na(tab.bc[,use2])==F),use2]
         tab.bc[which(is.na(tab.bc[,use1])==F),2]=tab.bc[which(is.na(tab.bc[,use1])==F),use1]
         tab.bc=tab.bc[,-c(use1,use2)]
        }
        else
        {
          tab.bc=merge(tab.bc,as.data.frame(df.pp[[ik+1]]),by.x=1,by.y=1,all.x=TRUE,all.y=TRUE)
          tab.bc[which(is.na(tab.bc[,use2])==F),3]=tab.bc[which(is.na(tab.bc[,use2])==F),use2]
          tab.bc[which(is.na(tab.bc[,use1])==F),2]=tab.bc[which(is.na(tab.bc[,use1])==F),use1]
          tab.bc=tab.bc[,-c(use1,use2)]
        }
      }

      tab.bc=tab.bc[,c(1:4,seq(6,ncol(tab.bc),2),seq(5,ncol(tab.bc),2))]
      tab.bc=tab.bc[order(tab.bc[,3]),]

      ## make column names for table output - complex header not good at this point - 14.06.2024 - nice for online not for printing - quick solution
      if(ncol(tab.bc)==7)
      {
         colnames(tab.bc)=c("Marker","Chr", "bp_position", paste0("cM_deterministic.",breed.select.bc[1]),  paste0("cM_deterministic.",breed.select.bc[2]),
                            paste0("cM_likelihood.",breed.select.bc[1]), paste0("cM_likelihood.",breed.select.bc[2]))
      }
      else
      {
        colnames(tab.bc)=c("Marker","Chr", "bp_position", paste0("cM_deterministic.",breed.select.bc[1]),  paste0("cM_deterministic.",breed.select.bc[2]),paste0("cM_deterministic.",breed.select.bc[3]),
                           paste0("cM_likelihood.",breed.select.bc[1]), paste0("cM_likelihood.",breed.select.bc[2]), paste0("cM_likelihood.",breed.select.bc[3]))
      }

      data.table<-shiny::reactive(tab.bc) ## complete table

      ## initialize
      range.used<<-0

       output$sliderRangeMap_bc <- shiny::renderUI({
        shiny::sliderInput(ns("rangeMap_bc"), "Range based on Mbp", min=min.pos, max=max.pos, step=0.1, value=c(20,30))  ## changed from 0.1 to 1
      })

     output$genetic1 <- plotly::renderPlotly({
       output1.filename=paste0(names.files,"_deterministic_BTA-",filter)
       pp=makePlot_geneticMaps_bc(chr=chr,dat_maps.list=df.p,colo=color.shape.def$color1[1:length(names(geneticMap.bc))],plot.pos=6,what="Deterministic approach",name.file=output1.filename)
        pp%>%plotly::toWebGL()
      })

    output$genetic2 <- plotly::renderPlotly({
      output2.filename=paste0(names.files,"_likelihood_BTA-",filter)
      pp=makePlot_geneticMaps_bc(chr=chr,dat_maps.list=df.p,colo=color.shape.def$color1[1:length(names(geneticMap.bc))],plot.pos=5,what="Likelihood-based approach",name.file= output2.filename)
      pp%>%plotly::toWebGL()
    })

    output$rangeMaps=DT::renderDataTable({
         file.name.tab<<-paste0(names.files,"-geneticMap_BTA-",filter)
         DT::datatable(tab.bc,filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength'),#,'copy', list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                                                                          pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
    })

    output$csvstore2<-shiny::downloadHandler(
      paste0(file.name.tab,".csv") , content = function(file) {
        utils::write.csv(tab.bc, file, row.names = FALSE
        )
      })

    output$excelstore2<-shiny::downloadHandler(
      paste0(file.name.tab,".xlsx"), content = function(file) {
        writexl::write_xlsx(tab.bc, file)
      })




    ## all for venn diagram
    filename.venn <- paste0("Venn-Diagram-",names.files,"_BTA-",filter,".png")
    venn <- RVenn::Venn(venn.data.chr)
    venn_data <-process_venn_data(venn)

    InputPlot4 <- shiny::reactive(creating_venn(venn_data,breed.bc=breed.select.bc, bc.venn=names.bc.venn))
    if(length(breed.select.bc)==2) ## changed on 11.04.2023 ## changed 03.06.2024
    {
      width1= 500 #500
      height1= 200 #550
      width1.venn.plot=6 ## for print-out
      height1.venn.plot=3
    }
    else
    {
      width1= 700 #700
      height1= 560 #750
      width1.venn.plot=9
      height1.venn.plot=6
    }
    output$venn_diagram <- shiny::renderPlot({
      InputPlot4()},
      width=width1,# changed 11.04.2023 ## changed 03.06.2024
      height=height1
    )

    ## venn.diagramm is clicked
    shiny::observeEvent(input$plot_click, {
      req(input$plot_click)

      data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$plot_click, table.bc= data.table())
      output$rangeMaps=DT::renderDataTable({
        file.name.tab<<-paste0(names.files,"-geneticMap_BTA-",filter,"-set-",data2[[2]])

        DT::datatable(data2[[1]][order(data2[[1]][,3]),],container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                                               options = list(searching=FALSE,dom='Bfrtip',
                                                              dom = 'Bt', buttons = list('pageLength'),#, 'copy',list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                                                              pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All'))))
      },server=FALSE)
      shinyjs::show("ButtonReset")

      output$csvstore2<-shiny::downloadHandler(
        paste0(file.name.tab,".csv") , content = function(file) {
          utils::write.csv(data2[[1]][order(data2[[1]][,3]),], file, row.names = FALSE
          )
        })

      output$excelstore2<-shiny::downloadHandler(
        paste0(file.name.tab,".xlsx"), content = function(file) {
          writexl::write_xlsx(data2[[1]][order(data2[[1]][,3]),], file)
        })

    })

    #### Apply selected input
    shiny::observeEvent(input$ButtonshowRangeMap_bc,
    {
         req(input$ButtonshowRangeMap_bc)
         range<-input$rangeMap_bc

         shinyjs::hide("ButtonReset")

         df.p2=list();venn.data.chr.2=list()
         for(ij in 1:length(names(df.p)))
         {
             range1 <- which(as.numeric(df.p[[ij]]$Mbp_position)>=input$rangeMap_bc[1])
             range2 <- which(as.numeric(df.p[[ij]]$Mbp_position)<=input$rangeMap_bc[2])

             if(length(range1)==0)range1=1
             if(length(range2)==0)range2=dim(df.p[[1]])[1]

             df.p2[[names(df.p)[ij]]]=df.p[[ij]][range1[1]:range2[length(range2)],]
             venn.data.chr.2[[names.bc.venn[ij]]]=df.p2[[names(df.p)[ij]]]$Name

         }

         range1 <- which(as.numeric(tab.bc[,3])>=(input$rangeMap_bc[1]*1000000))
         range2 <- which(as.numeric(tab.bc[,3])<=(input$rangeMap_bc[2]*1000000))

         data.table<-shiny::reactive(tab.bc[range1[1]:range2[length(range2)],]) ## for reset


         output$rangeMaps=DT::renderDataTable({
           file.name.tab<<-paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2])
           DT::datatable(data.table(),filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='excel',title=file.name.tab)),
                                                                        pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
         },server=FALSE)

         output$csvstore2<-shiny::downloadHandler(
           paste0(file.name.tab,".csv") , content = function(file) {
             utils::write.csv(data.table(), file, row.names = FALSE
             )
           })

         output$excelstore2<-shiny::downloadHandler(
           paste0(file.name.tab,".xlsx"), content = function(file) {
             writexl::write_xlsx(data.table(), file)
           })

         output$genetic1 <- plotly::renderPlotly({
          output1.filename=paste0(names.files,"_deterministic_BTA-",filter,"_range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2])
          pp=makePlot_geneticMaps_bc(chr=chr,dat_maps.list=df.p2,colo=color.shape.def$color1[1:length(names(geneticMap.bc))],plot.pos=6,what="Deterministic approach",name.file=output1.filename)
          pp%>%plotly::toWebGL()
          })

         output$genetic2 <- plotly::renderPlotly({
           output2.filename=paste0(names.files,"_likelihood_BTA-",filter,"_range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2])
           pp=makePlot_geneticMaps_bc(chr=chr,dat_maps.list=df.p2,colo=color.shape.def$color1[1:length(names(geneticMap.bc))],plot.pos=5,what="Likelihood-based approach",name.file=output2.filename) ## colo=c("orange","red")
           pp%>%plotly::toWebGL()
         })

         filename.venn <-paste0("Venn-Diagram-",names.files,"_BTA-",filter,"_range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2],".png")

         venn  <- RVenn::Venn(venn.data.chr.2)
         venn_data <-process_venn_data(venn)

         InputPlot4 <-shiny::reactive(creating_venn(venn_data,breed.bc=breed.select.bc, bc.venn=names.bc.venn))
         output$venn_diagram <- shiny::renderPlot({
           InputPlot4()
         },width=width1,# changed 11.04.2023
         height=height1)

         output$download.venn.diagram_bc <- shiny::downloadHandler(
           filename=filename.venn,
           content = function(file) {
             showModal(modalDialog("Loading", footer=NULL))
             on.exit(removeModal())
             ggsave(file, plot = InputPlot4(), device = "png",width=width1.venn.plot,bg="white",height=height1.venn.plot,units="in",dpi=300)
           })


         shiny::observeEvent(input$plot_click, {
           req(input$plot_click)

           data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$plot_click, table.bc= data.table())
           output$rangeMaps=DT::renderDataTable({
             file.name.tab<<-paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2],"_unique-set-",data2[[2]])
             DT::datatable(data2[[1]][order(data2[[1]][,3]),],container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                           options = list(searching=FALSE,dom='Bfrtip',
                                          dom = 'Bt', buttons = list('pageLength'),# 'copy',list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                                          pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All'))))
           },server=FALSE)
           shinyjs::show("ButtonReset")

           output$csvstore2<-shiny::downloadHandler(
             paste0(file.name.tab,".csv") , content = function(file) {
               utils::write.csv(data2[[1]][order(data2[[1]][,3]),], file, row.names = FALSE
               )
             })

           output$excelstore2<-shiny::downloadHandler(
             paste0(file.name.tab,".xlsx"), content = function(file) {
               writexl::write_xlsx(data2[[1]][order(data2[[1]][,3]),], file)
             })

         })

         shiny::observeEvent(input$ButtonReset,{
           output$rangeMaps<-DT::renderDataTable({
             file.name.tab<<-paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2])
             DT::datatable(data.table(),container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                           options = list(searching=FALSE,dom='Bfrtip',
                                          dom = 'Bt', buttons = list('pageLength'),#, 'copy', list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                                          pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
           },server=FALSE)

           output$csvstore2<-shiny::downloadHandler(
             paste0(file.name.tab,".csv") , content = function(file) {
               utils::write.csv(data.table(), file, row.names = FALSE
               )
             })

           output$excelstore2<-shiny::downloadHandler(
             paste0(file.name.tab,".xlsx"), content = function(file) {
               writexl::write_xlsx(data.table(), file)
             })

           shinyjs::hide("ButtonReset")
         })
    })

    ### hier reset to all
    shiny::observeEvent(input$ButtonAll,{
      req(input$ButtonAll)

      shinyjs::hide("ButtonReset")
      range.used<<-0

      data.table<-shiny::reactive(tab.bc)

      output$rangeMaps=DT::renderDataTable({
        file.name.tab=paste0(names.files,"-geneticMap_BTA-",filter)
        DT::datatable(data.table(),filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='excel',title=file.name.tab)), ## changed
                                                                          pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
      },server=FALSE)

      output$sliderRangeMap_bc <- shiny::renderUI({
        sliderInput(ns("rangeMap_bc"), "Range based on Mbp",  min=min.pos, max=max.pos, ## sliderInput
                               step=0.1, value=c(df.p[[1]]$Mbp_position[1],df.p[[1]]$Mbp_position[length(df.p[[1]]$Mbp_position)]))
      })

      output$genetic1 <- plotly::renderPlotly({
        output1.filename=paste0(names.files,"_deterministic_BTA-",filter)
        pp=makePlot_geneticMaps_bc(chr=chr,dat_maps.list=df.p,colo=color.shape.def$color1[1:length(names(geneticMap.bc))],plot.pos=6,what="Deterministic approach",name.file=output1.filename)
        pp%>%toWebGL()
      })

      output$genetic2 <- plotly::renderPlotly({
        output2.filename=paste0(names.files,"_likelihood_BTA-",filter)
        pp=makePlot_geneticMaps_bc(chr=chr,dat_maps.list=df.p,colo=color.shape.def$color1[1:length(names(geneticMap.bc))],plot.pos=5,what="Likelihood-based approach",name.file=output2.filename)
        pp%>%toWebGL()
      })

                  #
      filename.venn <-paste0("Venn-Diagram-",names.files,"_BTA-",filter,".png")
      venn <- RVenn::Venn(venn.data.chr)
      venn_data <-process_venn_data(venn) ## also changed

      InputPlot4 <-shiny::reactive(creating_venn(venn_data,breed.bc=breed.select.bc, bc.venn=names.bc.venn))
        output$venn_diagram <- shiny::renderPlot({ ## make routine
        InputPlot4()
      },width=width1,# changed 11.04.2023
      height=height1)

      output$download.venn.diagram_bc <- shiny::downloadHandler(
        filename=filename.venn,
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          ggsave(file, plot = InputPlot4(), device = "png",bg="white",width=width1.venn.plot,height=height1.venn.plot,units="in",dpi=300)
        })


      shiny::observeEvent(input$plot_click, {
        req(input$plot_click)

        data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$plot_click, table.bc= data.table())
        output$rangeMaps=DT::renderDataTable({
        file.name.tab=paste0(names.files,"-geneticMap_BTA-",filter)
        DT::datatable(data2[[1]][order(data2[[1]][,3]),],container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                        options = list(searching=FALSE,dom='Bfrtip',
                                       dom = 'Bt', buttons = list('pageLength', 'copy',list(extend='csv',title=file.name.tab),list(extend='excel',title=file.name.tab)),
                                       pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All'))))
        },server=FALSE)
        shinyjs::show("ButtonReset")
      })


      shiny::observeEvent(input$ButtonReset,{
        output$rangeMaps=DT::renderDataTable({
          file.name.tab=paste0(names.files,"-geneticMap_BTA-",filter,"-range-",input$rangeMap_bc[1],"-",input$rangeMap_bc[2])
          DT::datatable(data.table(),filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='excel',title=file.name.tab)), ## changed
                                                                                    pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
        },server=FALSE)
      })





     })

    ## show hide venn diagram
    shiny::observeEvent(input$venn_chr_bc1,{
      shinyjs::hide(id="venn_chr1")
      shinyjs::show(id="venn_chr2")

      output$venn_diagram <- shiny::renderPlot({
        InputPlot4()
      },width=width1,# changed 11.04.2023
      height=height1)
    })

    shiny::observeEvent(input$venn_chr_bc2,{
      shinyjs::show(id="venn_chr1")
      shinyjs::hide(id="venn_chr2")
    })
    ### end

    shiny::observeEvent(input$ButtonReset,{
      output$rangeMaps<-DT::renderDataTable({
        DT::datatable(data.table(),container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                      options = list(searching=FALSE,dom='Bfrtip',
                                     dom = 'Bt', buttons = list('pageLength', 'copy', 'csv', 'excel'),
                                     pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
      },server=FALSE)
      shinyjs::hide("ButtonReset")
    })


    output$download.venn.diagram_bc <- shiny::downloadHandler(
      filename=filename.venn,
      content = function(file) {
        showModal(modalDialog("Loading", footer=NULL))
        on.exit(removeModal())
        ggsave(file, plot = InputPlot4(), device = "png",bg="white",width=width1.venn.plot,height=height1.venn.plot,units="in",dpi=300) # added bg=white to ggsave 03.06.2024
      })
    ### end

   } ## Ende single chr

    ###########
    if(filter=="All")
    {
      shinyjs::hide(id="output1")
      shinyjs::show(id="output2")
      shinyjs::show(id="output3")
      shinyjs::hide(id="venn_all1")
      shinyjs::hide(id="venn_all2")
      shinyjs::show(id="show_bc_method_1")
      shinyjs::hide(id="show_bc_method_2")


      shiny::observeEvent(input$show_method_bc,{
        if(input$show_method_bc=="deterministic")
        {
          shinyjs::show(id="show_bc_method_1")
          shinyjs::hide(id="show_bc_method_2")

          meth1="deterministic"
          ll2=transformdata_genetic_map_bc(n=29,input=geneticMap.bc,meth=meth1,names.bc.plot=breed.select.bc)
          plot_gm_all_det=shiny::reactive(makePlot_all_geneticMaps_bc(ll=ll2,names.bc.plot=breed.select.bc,colo=color.shape.def))

          output$figure_det<-shiny::renderPlot({
            plot_gm_all_det()
          })%>%shiny::bindCache(breed.select.bc, filter,meth1)

          output$download.gm.bc <- shiny::downloadHandler(
            filename = paste0(names.files,"-genetic_map-deterministic-all.png"),
            content = function(file) {
              showModal(modalDialog("Loading", footer=NULL))
              on.exit(removeModal())
              ggsave(file, plot = plot_gm_all_det(), device = "png",width=20,height=40,units="in",dpi=300)
            })
        }
        else{
          shinyjs::hide(id="show_bc_method_1")
          shinyjs::show(id="show_bc_method_2")


          ## render traffic light
          if(length(breed.select.bc)==2)height2=45
          else height2=55
          output$TrafficLight_3 <- renderPlot({
            make.traff.light.bc()
          }, width=165, height=height2) ## vorher 80
          ######


          meth2="likelihood"
          ll3=transformdata_genetic_map_bc(n=29,input=geneticMap.bc,meth=meth2,names.bc.plot=breed.select.bc)
          plot_gm_all_lik=shiny::reactive(makePlot_all_geneticMaps_bc (ll=ll3,names.bc.plot=breed.select.bc,colo=color.shape.def))

          output$figure_lik<-shiny::renderPlot({
            plot_gm_all_lik()
          })%>%shiny::bindCache(breed.select.bc, filter,meth2)

          output$download.gm.bc2 <- shiny::downloadHandler(
            filename = paste0(names.files,"-genetic_map-likelihood-all.png"),
            content = function(file) {
              showModal(modalDialog("Loading", footer=NULL))
              on.exit(removeModal())
              ggsave(file, plot = plot_gm_all_lik(), device = "png",width=20,height=40,units="in",dpi=300)
            })
        }
      })

    }## end all
  })
}

## To be copied in the UI
# mod_bc_genetic_map_ui("bc_genetic_map_1")

## To be copied in the server
# mod_bc_genetic_map_server("bc_genetic_map_1")
