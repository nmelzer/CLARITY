# Module UI

#' @title mod_genetic_map_ui and mod_genetic_map_server
#' @description  A shiny Module to generate the outcome for the tabpanel Genetic map.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param filter external depends on choice by the chromosome
#'
#' @rdname mod_genetic_map
#'
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList
#' @import shiny
#' @import shinycssloaders
#' @import htmltools
#' @import gridExtra
#' @rawNamespace import(plotly, except = last_plot)
#' @note The module uses the mod_genetic_map_fct_functions.R.

mod_genetic_map_ui=function(id)
{
  ns=NS(id)
  tagList(
    br(),br(),
    fluidRow(id=ns("output1"),
             box(title= tags$b("Interactive graphical visualization: Genetic vs. physial distance"),status="danger",width=12,
                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                 column(width=8, plotlyOutput(ns("genetic"),width="auto",height="auto")%>% withSpinner(color="#0dc5c1"))),
             br(),
             br(),
             ########## new insert - take may out again
             box(title= tags$b("Genetic distance in selected interval"),status="danger",width=12,
                 solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                 fluidRow(column(width=4,uiOutput(ns("sliderRangeMap"))),
                          column(width=2,actionButton(ns("ButtonshowRangeMap"),"Plot selected intervall")),
                          column(width=2,actionButton(ns("ButtonAll"),"Reset to all")),

                 ),
                fluidRow(column(width=8,DT::dataTableOutput(outputId=ns("rangeMaps")))))
             ),
             fluidRow(id=ns("output2"), column(width=3,textInput(ns('height'), 'Plot Height', value="300") ),
               column(width=3, textInput(ns('width'), 'Width', value="100%")),
               column(width=4, sliderInput(ns('ncol'), 'Columns', min=1, max=4, value=4)),
              column(width=2, downloadLink(ns("dlURL"),label="Download all in a PDF."))
            ),
            fluidRow(id=ns("output3"), uiOutput(ns('plots'))
    )
 )
}


# Module Server
#' @rdname mod_genetic_map
#'
#' @keywords internal
#' @export
mod_genetic_map_server=function(input, output, session, filter)
{
  ns <- session$ns

  geneticMap=NULL
  X<-Y<-Approach<-NULL

  load(system.file("extdata","geneticMap.Rdata",package="CLARITY"))

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
        th(colspan=1,tags$a(href="#","Likelihood approach", onclick = "openTab('methodology')")),
        th(colspan = 2,tags$a(href="#","Deterministic approach", onclick = "openTab('methodology')") )
      ),
      tr(
        th(colspan=1,"Chromosome"),
        th(colspan=1,"Marker"),
        th(colspan=1,"(Mbp)"),
        th(colspan=1,"Inter-marker distance (Mbp)"),
        th(colspan=1,"Position (BP)"),
        th(colspan=1,"(cM)"),
        th(colspan=1,"(cM)"),
        th(colspan=1,"Recobination rate adjacent")
      )
    )
  ))


  if(filter!="All")
  {
    shinyjs::show(id="output1")
    shinyjs::hide(id="output2")
    shinyjs::hide(id="output3")

    chr <-filter

    df.p=geneticMap[which(geneticMap$Chr==chr),]

    ## initialize
    output$sliderRangeMap <- renderUI({
     dat=df.p
     sliderInput(ns("rangeMap"), "Range based on Mbp", min=dat$Mbp_position[1], max=dat$Mbp_position[length(dat$Mbp_position)], step=0.1, value=c(20,30))  ## changed from 0.1 to 1

    })

    output$genetic <- renderPlotly({
      pp=makePlot_geneticMaps(chr=chr,dat_maps=df.p)
      pp%>%toWebGL()
    })



   observeEvent(input$rangeMap,
   {
    req(input$rangeMap)
    range<-input$rangeMap


    range1 <- which(input$rangeMap[1]>=as.numeric(df.p$Mbp_position))
    range2 <- which(input$rangeMap[2]<=as.numeric(df.p$Mbp_position))
    if(length(range1)==0)range1=1
    if(length(range2)==0)range2=dim(dat)[1]

    data=df.p[range1[length(range1)]:range2[1],c(1:3,8,4:7)]

    output$rangeMaps=DT::renderDataTable({
        DT::datatable(data,filter="none" , container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf'),
                                                            pagelength = 10,lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
    })
   })


   observeEvent(input$ButtonshowRangeMap,
   {
     req(input$ButtonshowRangeMap)
      range<-input$rangeMap
      range1 <- which(input$rangeMap[1]>=as.numeric(df.p$Mbp_position))
      range2 <- which(input$rangeMap[2]<=as.numeric(df.p$Mbp_position))

      if(length(range1)==0)range1=1
      if(length(range2)==0)range2=dim(dat)[1]
      data=df.p[range1[length(range1)]:range2[1],]

        output$genetic <- renderPlotly({
        pp=makePlot_geneticMaps(chr=chr,dat_maps=data)
        pp%>% toWebGL()
      })
    })

   ### hier reset to all
   observeEvent(input$ButtonAll,
   {
       req(input$ButtonAll)
      # data=df.p[,c(1:3,8,4:7)]


        output$rangeMaps=DT::renderDataTable({
          DT::datatable(df.p[,c(1:3,8,4:7)],filter="none", container=sketch2, options=list(searching = FALSE,dom='Bfrtip',buttons = c('pageLength','copy', 'csv', 'excel', 'pdf'),
                           pagelength = 10, lengthMenu = list(c(10, 20 ,30, -1), c('10', '20','30','All'))),escape=FALSE,rownames=FALSE)
        })

        output$sliderRangeMap <- renderUI({
          sliderInput(ns("rangeMap"), "Range based on Mbp", min=data$Mbp_position[1], max=data$Mbp_position[length(data$Mbp_position)], ## sliderInput
                              step=0.1, value=c(data$Mbp_position[1],data$Mbp_position[length(data$Mbp_position)]))
        })

        output$genetic <- renderPlotly({
          pp=makePlot_geneticMaps(chr=chr,dat_maps=df.p)
          pp%>% toWebGL()
        })
    })
  }


  if(filter=="All")
  {
    shinyjs::hide(id="output1")
    shinyjs::show(id="output2")
    shinyjs::show(id="output3")

    ll2=prepareData(n=29,input=geneticMap)

    output$plots <- renderUI({
      makePlotContainers(n=29,ncol=input$ncol, height=input$height, width=input$width )
    })
    renderPlots(n=29, input=ll2,output)


    if(file.exists(system.file("figures","geneticMapComparisonAllChromosomes.pdf",package="CLARITY"))!=TRUE){
      ll2=prepareData(n=29,input=geneticMap)
      pl<-lapply(1:29,function(.x) ggplot(ll2[[.x]],aes(x=X, y=Y,col=Approach)) +
                   geom_point(size=0.5, na.rm=TRUE)+
                   ggtitle(paste0("BTA ",.x))+
                   scale_color_manual(values=c("dodgerblue2", "cadetblue3"),labels=c("Deterministic","Likelihood"))+
                   theme(plot.title = element_text(hjust = 0.5))+
                   labs(x="Physical length (Mbp)",y="Genetic distance (cM)")
          )
      gg= gridExtra::grid.arrange(grobs=pl,ncol=3)
      ggsave(system.file("figures","geneticMapComparisonAllChromosomes.pdf",package="CLARITY"), gg,width=20,height = 30,dpi=600)
    }

    output$dlURL <- downloadHandler(
      filename="geneticMapComparisonAllChromosomes.pdf",
      content=function(file){
        file.copy(system.file("figures","geneticMapComparisonAllChromosomes.pdf",package="CLARITY"),file)
      }
    )
  }
}

## To be copied in the UI
# mod_genetic_map_ui("mod_genetic_map_1")

## To be copied in the server
# callModule(mod_genetic_map_server,"mod_genetic_map_1")
