# Module UI

#' @title mod_genetic_function_ui and mod_genetic_function_server
#' @description  A shiny Module to generate the outcome for the tabpanel Genetic map functions.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_genetic_function
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders

#' @import htmltools
mod_genetic_function_ui=function(id)
  {
    ns=NS(id)

    tagList(
      br(),
      fluidRow(
        useShinyjs(),
        box(title= tags$b("Interactive graphical visualization"),status="danger",width=12,
            solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,

            ### show output when all chromosomes are selected
            fluidRow(id=ns("all_chromosome"),
                     br(),
                     column(6,"Due to the longer loading time not all chromosomes are directly shown."), column(6,""),
                     br(),br(),
                     column(6, "Remove a genetic map function in the figure by clicking on the corresponding name in the legend."),column(6,""),
                     br(),
                     br(),
                     column(width=6,
                            selectizeInput(inputId=ns('GenetMapchr1'),
                                           label = "Select chromosome",choices=c(seq(1,29,1)),selected=18,multiple=FALSE),
                            plotly::plotlyOutput(ns("genetic_functions3"))%>% withSpinner(color="#0dc5c1")
                     ),
                    column(width=6,
                            selectizeInput(inputId=ns('GenetMapchr2'),
                                          label = "Select chromosome",choices=c(seq(1,29,1)),selected=28,multiple=FALSE),#)#),
                            plotly::plotlyOutput(ns("genetic_functions4"))%>% withSpinner(color="#0dc5c1")
                    ),
              br(),
              br(),
              box(id=ns("test_bta_markers1"),status="info",width=6,br(),br(),
                     fluidRow(column(width=6,htmlOutput(ns("test_bta_marker1"),inline=TRUE),uiOutput(ns("downloadChr1"),inline=TRUE) )),
              ),
              box(id=ns("test_bta_markers2"),status="info",width=6,br(),br(),
                     fluidRow(column(width=6,htmlOutput(ns("test_bta_marker2"),inline=TRUE),uiOutput(ns("downloadChr2"),inline=TRUE) )),
              )
            ),

            ### show output when specific chromosome is selected
            fluidRow(id=ns("single_chromosome"),
                     br(),
                     column(8, "Remove a genetic-map function in the figure by clicking on the corresponding name in the legend."),
                     br(),
                     br(),
                     column(width=8,plotly::plotlyOutput(ns("genetic_functions5"),width="auto",height="auto")%>% withSpinner(color="#0dc5c1")), ## turn back to 8
                     fluidRow(br(),br()),
                     box(id=ns("test_bta_markers"),status="info",width=8,
                       column(width=10,htmlOutput(ns("test_bta_marker"),inline =TRUE),uiOutput(ns("downloadChr"),inline=TRUE)#
                          ))#)
            ),
        )),
      br(),
      fluidRow(
        box(title = tags$b("Genetic-map functions"),status="danger",width=12, ## make Link
            solidHeader = TRUE,collapsible = TRUE, collapsed=TRUE,
            br(),
            fluidRow(column(width=5, "Genetic-map function that fits best is highlighed in orange.")),
            br(),
            fluidRow(column(width=8, DT::dataTableOutput(ns("tableBestmapFunction")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
            fluidRow(column(width=5, "MSE - mean squared error"))
        )
      )
    )
}


# Module Server
#' @rdname mod_genetic_function
#'
#' @export
#' @keywords internal
mod_genetic_function_server=function(input, output, session, filter)
{
  ns <- session$ns

  max.to.plot=450000
  load(system.file("extdata", "bestmapfun.Rdata",package="CLARITY"))

  use=c(2,4,6,8)
  out[,use]=round(out[,use],7)

  pp=matrix(0,dim(out)[1],dim(out)[2])
  colnames(pp)=colnames(out)


  ## find the the best genetic-map function - it is necessary for coloring in the corresponding tables
  for(i in 1:dim(out)[1])
  {
    p=which(out[i,use]==min(out[i,use]))
    out[i,use[p]]=paste0(out[i,use[p]],"*")
    pp[i,use[p]]=out[i,use[p]]
  }
  out=as.data.frame(out)
  pp=as.data.frame(pp)

  ## Table header with internal link to methodology --
  thead<-tr<-th<-NULL
  sketch1 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=1,""),
        th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')")),
        th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')")),
        th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')")),
        th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')"))
      ),
      tr(
        th(colspan=1,"Chromosome"),
        th(colspan=1,"MSE"),
        th(colspan=1,"Parameter"),
        th(colspan=1,"MSE"),
        th(colspan=1,"Parameter"),
        th(colspan=1,"MSE"),
        th(colspan=1,"Parameter"),
        th(colspan=1,"MSE"),
        th(colspan=1,"Parameter")
      )
    )
  ))


  if(filter=="All")
  {
    shinyjs::show(id="all_chromosome")

    shinyjs::hide(id="single_chromosome")
    shinyjs::hide(id="test_bta_markers")
    shinyjs::hide(id="test_bta_markers1")
    shinyjs::hide(id="test_bta_markers2")

    shinyjs::hide(id="download_gm1")
    shinyjs::hide(id="download_gm2")


    ## adopted from https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
    toListen <- reactive({
      list(input$GenetMapchr1,input$GenetMapchr2)
    })

    observeEvent(toListen(),{
      chr <- input$GenetMapchr1
      outcome1= check_bta_marker_length(chromo=chr,max.plot=max.to.plot)

      output$test_bta_marker1<-shiny::renderText({
        zz=HTML(paste0('<i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i>'," The plot based on about ",round(outcome1[[2]]), "% of the marker comparisons." ))
      })

      output$downloadChr1 <- renderUI({
        downloadButton(ns("dlChr1"), "Full plot")
      })

      output$dlChr1 <- downloadHandler(
        filename=paste0("BTA-",chr,".png"),
        content=function(file){
          file.copy(system.file("figures",paste0("BTA-",chr,".png"),package="CLARITY"),file)
        }
      )


      chr2 <- input$GenetMapchr2
      outcome2=check_bta_marker_length(chromo=chr2,max.plot=max.to.plot)

      output$test_bta_marker2<-shiny::renderText({
        zz=HTML(paste0('<i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i>'," The plot based on about ",round(outcome2[[2]]), "% of the marker comparisons." ))
      })

      output$downloadChr2 <- renderUI({
        downloadButton(ns("dlChr2"), "Full plot")
      })

      output$dlChr2 <- downloadHandler(
        filename=paste0("BTA-",chr2,".png"),
        content=function(file){
            file.copy(system.file("figures",paste0("BTA-",chr2,".png"),package="CLARITY"),file)
        }
      )

      if(outcome1[[2]]!=100 && outcome2[[2]]==100){
        shinyjs::show(id="test_bta_markers1")
        shinyjs::hide(id="test_bta_markers2")
      }

      if(outcome1[[2]]==100 && outcome2[[2]]!=100){
        shinyjs::hide(id="test_bta_markers1")
        shinyjs::show(id="test_bta_markers2")
      }
      if(outcome1[[2]]!=100 && outcome2[[2]]!=100){
        shinyjs::show(id="test_bta_markers1")
        shinyjs::show(id="test_bta_markers2")
      }
      if(outcome1[[2]]==100 && outcome2[[2]]==100){
        shinyjs::hide(id="test_bta_markers1")
        shinyjs::hide(id="test_bta_markers2")
      }
    })

    observeEvent(input$GenetMapchr1,{
      req(input$GenetMapchr1)
      chr <- input$GenetMapchr1
      outcome1= check_bta_marker_length(chromo=chr,max.plot=max.to.plot)

      output$genetic_functions3 <- plotly::renderPlotly({
        chr=as.numeric(as.character(chr))
        pp2=makePlot(chromo=chr,df=outcome1[[1]],df.list=outcome1[[3]])
        pp2%>% toWebGL()
      })
    })

    observeEvent(input$GenetMapchr2 ,{
      req(input$GenetMapchr2)
      chr <- input$GenetMapchr2
      outcome2=check_bta_marker_length(chromo=chr,max.plot=max.to.plot)

        output$genetic_functions4 <- plotly::renderPlotly({
        chr=as.numeric(as.character(chr))
        plot2=makePlot(chromo=chr,df=outcome2[[1]],df.list=outcome2[[3]])
        plot2%>% toWebGL()
      })
    })

    output$tableBestmapFunction=DT::renderDataTable({
      DT::datatable(out,container=sketch1,filter="none",options=list(searching=FALSE,dom='Bfrtip',buttons = c('pageLength','copy', 'csv', 'excel', 'pdf'),pagelength = 10, lengthMenu = list(c(10, 15, -1), c('10', '20','All'))),escape=FALSE,rownames=FALSE)%>%
        DT::formatStyle(if(length(pp[which(pp$Haldane_scaled_mse!=0),2])!=0)"Haldane_scaled_mse",backgroundColor=DT::styleEqual(pp[which(pp[,2]!=0),2], rep("orange",length(pp[which(pp[,2]!=0),2]))))%>%
        DT::formatStyle(if(length(pp[which(pp$Rao_mse!=0),4])!=0)"Rao_mse",backgroundColor=DT::styleEqual(pp[which(pp[,4]!=0),4], rep("orange",length(pp[which(pp[,4]!=0),4]))))%>%
        DT::formatStyle(if(length(pp[which(pp$Felsenstein_mse!=0),6])!=0)"Felsenstein_mse",backgroundColor=DT::styleEqual(pp[which(pp[,6]!=0),6], rep("orange",length(pp[which(pp[,6]!=0),6])))) %>%
        DT::formatStyle(if(length(pp[which(pp$Karlin_mse!=0),8])!=0)"Karlin_mse",backgroundColor=DT::styleEqual(pp[which(pp[,8]!=0),8], rep("orange",length(pp[which(pp[,8]!=0),8]))))
    })
  }


  if(filter!="All")
  {
    shinyjs::hide(id="all_chromosome")
    shinyjs::show(id="single_chromosome")

    shinyjs::hide(id="test_bta_markers")
    shinyjs::hide(id="test_bta_markers1")
    shinyjs::hide(id="test_bta_markers2")
    shinyjs::hide(id="test_bta_markers3")


    chr=as.numeric(as.character(filter))
    outcome=check_bta_marker_length(chromo=chr,max.plot=max.to.plot)


    output$genetic_functions5 <- plotly::renderPlotly({
      plot2=makePlot(chromo=chr,df=outcome[[1]],df.list=outcome[[3]])
      plot2%>% toWebGL()
    })

    output$tableBestmapFunction=DT::renderDataTable({
        out2=out[as.numeric(as.character(filter)),]
        DT::datatable(out2,filter="none", container=sketch1, options=list(dom='Bt',buttons = c('copy', 'csv', 'excel', 'pdf')),escape=FALSE,rownames=FALSE)%>%
        DT::formatStyle(if(length(pp[which(pp$Haldane_scaled_mse!=0),2])!=0)"Haldane_scaled_mse",backgroundColor=DT::styleEqual(pp[which(pp[,2]!=0),2], rep("orange",length(pp[which(pp[,2]!=0),2]))))%>%
        DT::formatStyle(if(length(pp[which(pp$Rao_mse!=0),4])!=0)"Rao_mse",backgroundColor=DT::styleEqual(pp[which(pp[,4]!=0),4], rep("orange",length(pp[which(pp[,4]!=0),4]))))%>%
        DT::formatStyle(if(length(pp[which(pp$Felsenstein_mse!=0),6])!=0)"Felsenstein_mse",backgroundColor=DT::styleEqual(pp[which(pp[,6]!=0),6], rep("orange",length(pp[which(pp[,6]!=0),6])))) %>%
        DT::formatStyle(if(length(pp[which(pp$Karlin_mse!=0),8])!=0)"Karlin_mse",backgroundColor=DT::styleEqual(pp[which(pp[,8]!=0),8], rep("orange",length(pp[which(pp[,8]!=0),8]))))
   })

    if(outcome[[2]]!=100)
    {
      output$test_bta_marker<-shiny::renderUI({
        zz=HTML(paste0('<i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i>'," The plot based on about ",round(outcome[[2]]), "% of the marker comparisons." ))
      })
      shinyjs::show(id="test_bta_markers")

      output$downloadChr <- renderUI({
               downloadButton(ns("dlChr"), "Full plot")
      })

      ## downloadHnadler noch mit
      output$dlChr <- downloadHandler(
        filename=paste0("BTA-",chr,".png"),
          content=function(file){
          file.copy(system.file("figures",paste0("BTA-",chr,".png"),package="CLARITY"),file)
        }
      )
    }

    if(outcome[[2]]==100)
    {
      shinyjs::hide(id="test_bta_markers")
    }


  }

} ## End server


## To be copied in the UI
# mod_genetic_function_ui("mod_genetic_function_1")

## To be copied in the server
# callModule(mod_genetic_function_server,"mod_genetic_function_1")
