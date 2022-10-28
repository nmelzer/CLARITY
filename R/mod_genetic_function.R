# Module UI

#' @title mod_genetic_function_ui and mod_genetic_function_server
#' @description  A shiny module to generate the outcome for the tabpanel "Genetic-map functions" for the sidebar "Breed analysis".
#'
#' @details The module uses the function \link{makePlot_genetic_function} for plotting.
#'
#' @param id module id
#'
#' @rdname mod_genetic_function
#' @keywords internal, genetic function
#' @export
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @rawNamespace import(shinyjs, except = runExample)
#' @rawNamespace import(plotly, except = last_plot)
#' @seealso \link{makePlot_genetic_function}

mod_genetic_function_ui=function(id)
  {
    ns=shiny::NS(id)

    shiny::tagList(
      htmltools::br(),
      shiny::fluidRow(
        shinyjs::useShinyjs(),
        shinydashboard::box(title= tags$b("Interactive graphical visualization"),status="danger",width=12,
            solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,

            ### show output when all chromosomes are selected
            shiny::fluidRow(id=ns("all_chromosome"),
                     shiny::column(12,"Only two chromosomes can be chosen and approximately 200,000 data points are plotted."),
                     shiny::column(width = 12,style="padding-top:20px", ""),
                     shiny::column(12, "Remove a genetic map function in the figure by clicking on the corresponding name in the legend."),
                     shiny::column(width = 12,style="padding-top:20px", ""),
                     shiny::column(width=6,
                                   shiny::selectizeInput(inputId=ns('GenetMapchr1'),
                                           label = "Select chromosome",choices=c(seq(1,29,1)),selected=18,multiple=FALSE),
                                    plotly::plotlyOutput(ns("genetic_functions3"))%>% shinycssloaders::withSpinner(color="#0dc5c1")
                     ),
                    shiny::column(width=6,
                           shiny::selectizeInput(inputId=ns('GenetMapchr2'),
                                          label = "Select chromosome",choices=c(seq(1,29,1)),selected=28,multiple=FALSE),
                            plotly::plotlyOutput(ns("genetic_functions4"))%>% shinycssloaders::withSpinner(color="#0dc5c1")
                    )
            ),

            ### show output when specific chromosome is selected
            shiny::fluidRow(id=ns("single_chromosome"),
                     shiny::column(12, "Only about 200,000 data points are plotted."),
                     shiny::column(width = 12,style="padding-top:20px", ""),
                     shiny:: column(8, "Remove a genetic-map function in the figure by clicking on the corresponding name in the legend."),
                     shiny::column(width = 12,style="padding-top:20px", ""),
                     shiny:: column(width=8,plotly::plotlyOutput(ns("genetic_functions5"),width="auto",height="auto")%>% withSpinner(color="#0dc5c1")),
            )
      )),
      htmltools::br(),
      shiny::fluidRow(
        shinydashboard::box(title = tags$b("Genetic-map functions"),status="danger",width=12, ## make Link
            solidHeader = TRUE,collapsible = TRUE, collapsed=FALSE,
            shiny::column(width = 12,style="padding-top:30px", ""),
            shiny::fluidRow(shiny::column(width=5, "Genetic-map function that fits best is highlighed in orange.")),
            shiny::column(width = 12,style="padding-top:30px", ""),
            shiny::fluidRow(shiny::column(width=8, DT::dataTableOutput(ns("tableBestmapFunction")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
            shiny::fluidRow(shiny::column(width=5, "MSE - mean squared error"))
        )
      )
    )
}


# Module Server
#' @rdname mod_genetic_function
#' @param filter selected chromosome
#' @param breed.select selected breed
#' @export
#' @keywords internal
mod_genetic_function_server=function(id, filter, breed.select){
  shiny::moduleServer(id,function(input, output, session){
    ns <- session$ns

    out<-NULL

    load(system.file("extdata", paste0(breed.select,"/bestmapfun.Rdata"),package="CLARITY"))
    out.new=out

    use=c(2,4,6,8)
    out.new[,use]=round(out.new[,use],7)

    pp=matrix(0,dim(out)[1],dim(out.new)[2])
    colnames(pp)=colnames(out.new)

    ## find the the best genetic-map function - it is necessary for coloring in the corresponding tables
    for(i in 1:dim(out.new)[1])
    {
      p=which(out.new[i,use]==min(out.new[i,use]))
      out.new[i,use[p]]=paste0(out.new[i,use[p]],"*")
      pp[i,use[p]]=out.new[i,use[p]]
    }
    out.2=as.data.frame(out.new)
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
          lapply(rep(c('MSE', 'Parameter'), 4), th)
        )
      )
    ))

    if(filter=="All")
    {
      shinyjs::show(id="all_chromosome")
      shinyjs::hide(id="single_chromosome")

      shiny::observeEvent(input$GenetMapchr1,{
        chr <-input$GenetMapchr1

        store=NULL; outcome1=list()
        load(system.file("extdata",paste0(breed.select,"/curve-short-",chr,".Rdata"),package="CLARITY"))  ## 13.06.2022 - only markers are used to see shape of the curve
        outcome1= store

        file.name=paste0(breed.select,"_genetic-map-funcions_BTA-",chr)

        output$genetic_functions3 <- makePlot_genetic_function(chromo=chr,df.list=outcome1,name.file=file.name,breed.select=breed.select)
      })

      shiny::observeEvent(input$GenetMapchr2,{
        chr=input$GenetMapchr2

        store<-NULL; outcome2=list()
        load(system.file("extdata",paste0(breed.select,"/curve-short-",chr,".Rdata"),package="CLARITY"))  ## 13.06.2022 - only markers are used to see shape of the curve
        outcome2= store

        file.name=paste0(breed.select,"_genetic-map-funcions_BTA-",chr)
        output$genetic_functions4 <- makePlot_genetic_function(chromo=chr,df.list=outcome2,name.file=file.name,breed.select=breed.select)
      })

      output$tableBestmapFunction=DT::renderDataTable({
        title1=paste0(breed.select,"_genetic-map-functions_BTA-all")
        DT::datatable(out.2,container=sketch1,filter="none",options=list(searching=FALSE,dom='Bfrtip',buttons = list('pageLength','copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                                                       pagelength = 10, lengthMenu = list(c(10, 15, -1), c('10', '20','All'))),escape=FALSE,rownames=FALSE)%>%
        DT::formatStyle(if(length(pp[which(pp$Haldane_scaled_mse!=0),2])!=0)"Haldane_scaled_mse",backgroundColor=DT::styleEqual(pp[which(pp[,2]!=0),2], rep("orange",length(pp[which(pp[,2]!=0),2]))))%>%
        DT::formatStyle(if(length(pp[which(pp$Rao_mse!=0),4])!=0)"Rao_mse",backgroundColor=DT::styleEqual(pp[which(pp[,4]!=0),4], rep("orange",length(pp[which(pp[,4]!=0),4]))))%>%
        DT::formatStyle(if(length(pp[which(pp$Felsenstein_mse!=0),6])!=0)"Felsenstein_mse",backgroundColor=DT::styleEqual(pp[which(pp[,6]!=0),6], rep("orange",length(pp[which(pp[,6]!=0),6])))) %>%
        DT::formatStyle(if(length(pp[which(pp$Karlin_mse!=0),8])!=0)"Karlin_mse",backgroundColor=DT::styleEqual(pp[which(pp[,8]!=0),8], rep("orange",length(pp[which(pp[,8]!=0),8]))))
      }, server = FALSE)
    }
    else
    {
      shinyjs::hide(id="all_chromosome")
      shinyjs::show(id="single_chromosome")

      chr=filter
      store<-NULL; outcome=list()
      load(system.file("extdata",paste0(breed.select,"/curve-short-",chr,".Rdata"),package="CLARITY"))##
      outcome=store

      output$tableBestmapFunction=DT::renderDataTable({
        title1=paste0(breed.select,"_genetic-map-functions_BTA-",chr)
        out2=out.2[as.numeric(as.character(filter)),]

        DT::datatable(out2,filter="none", container=sketch1, options=list(dom='Bt',buttons = list('copy',list(extend='csv',title=title1),list(extend='excel',title=title1))),escape=FALSE,rownames=FALSE)%>%
          DT::formatStyle(if(length(pp[which(pp$Haldane_scaled_mse!=0),2])!=0)"Haldane_scaled_mse",backgroundColor=DT::styleEqual(pp[which(pp[,2]!=0),2], rep("orange",length(pp[which(pp[,2]!=0),2]))))%>%
          DT::formatStyle(if(length(pp[which(pp$Rao_mse!=0),4])!=0)"Rao_mse",backgroundColor=DT::styleEqual(pp[which(pp[,4]!=0),4], rep("orange",length(pp[which(pp[,4]!=0),4]))))%>%
          DT::formatStyle(if(length(pp[which(pp$Felsenstein_mse!=0),6])!=0)"Felsenstein_mse",backgroundColor=DT::styleEqual(pp[which(pp[,6]!=0),6], rep("orange",length(pp[which(pp[,6]!=0),6])))) %>%
          DT::formatStyle(if(length(pp[which(pp$Karlin_mse!=0),8])!=0)"Karlin_mse",backgroundColor=DT::styleEqual(pp[which(pp[,8]!=0),8], rep("orange",length(pp[which(pp[,8]!=0),8]))))
      }, server = FALSE)

      file.name=paste0(breed.select,"_genetic-map-funcions_BTA-",filter)
      output$genetic_functions5 <- makePlot_genetic_function(chromo=chr,df.list=outcome,name.file=file.name,breed.select=breed.select)
    }

  })
}## End server


## To be copied in the UI
# mod_genetic_function_ui("mod_genetic_function_1")

## To be copied in the server
# mod_genetic_function_server("mod_genetic_function_1")
