# bc_genetic_function UI Function
#' @title mod_bc_genetic_function_ui and mod_bc_genetic_function_server
#' @description A shiny module to generate the outcome for the tabpanel "Genetic-map functions" for the sidebar "Breed comparison".
#'
#' @details The module uses the function \link{makePlot_genetic_function_bc} for plotting.
#'  Moreover, the quality of the likelihood approach is shown here by means of a traffic light (also termed as
#'  likelihood quality signal; \link{make_traffic_light}). \link{make_barplot_bc}
#'
#' @rdname mod_bc_genetic_function
#' @param id module id
#' @importFrom ggplot2 ggsave
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @importFrom htmlwidgets onRender
#' @rawNamespace import(shinyjs, except = runExample)
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @seealso
#' * \link{makePlot_genetic_function_bc}
#' * \link{make_traffic_light}
#' * \link{make_barplot_bc}
#' @export
#'
mod_bc_genetic_function_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    shinyjs::hidden(
        shiny::fluidRow(id=ns("no.other.method"),
                    htmltools::br(),htmltools::br(),
                    shiny::column(width=12,htmltools::tags$h3(htmltools::HTML("<span style=color:red;margin-left: 6px;> Genetic-map functions are only available for the likelihood-based approach.</span>")))
    )),
    shinyjs::hidden(
        shiny::fluidRow(id=ns("only.likelihood1"),
          shinydashboard::box(title= tags$b("Interactive graphical visualization"),status="danger",width=12, solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
              shinyjs::hidden(
                  shiny::fluidRow(id=ns("all_chromosome_bc"),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=12,"For visualization of breed-specific estimates from SNP data, please apply 'Breed analysis'."),
                           shiny::column(width=12, "Remove genetic-map function in the figure by clicking on the corresponding name in the legend."),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=9,""),
                           shiny::column(width=2,htmltools::tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                        shiny::plotOutput(ns("TrafficLight_3"),width="70%",height="auto")),
                           shiny::column(width=6,shiny::selectInput(inputId=ns('GenetMapchr1'),label = "Select chromosome",choices=c(seq(1,29,1)),selected=18,multiple=FALSE,selectize=FALSE),
                                   plotly::plotlyOutput(ns("genetic_bc_functions3"))%>% shinycssloaders::withSpinner(color="#0dc5c1")),
                           shiny::column(width=6, shiny::selectInput(inputId=ns('GenetMapchr2'), label = "Select chromosome",choices=c(seq(1,29,1)),selected=28,multiple=FALSE,selectize=FALSE),
                                   plotly::plotlyOutput(ns("genetic_bc_functions4"))%>% shinycssloaders::withSpinner(color="#0dc5c1"))
              )),
              ### show output when specific chromosome is selected
              shinyjs::hidden(
                shiny::fluidRow(id=ns("single_chromosome_bc"),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=12,"For visualization of breed-specific estimates from SNP data, please apply 'Breed analysis'."),
                           shiny::column(width=12, "Remove a genetic-map function in the figure by clicking on the corresponding name in the legend."),
                           shiny::column(width=9,""),
                           shiny::column(width=3,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                         shiny::plotOutput(ns("TrafficLight_2"),width="70%",height="auto")),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=8,plotly::plotlyOutput(ns("genetic_bc_functions5"))%>% shinycssloaders::withSpinner(color="#0dc5c1"))
              ))
        )
    )),
    htmltools::br(),
    shinyjs::hidden(
        shiny::fluidRow(id=ns("only.likelihood2"),
              shinydashboard::box(title = tags$b("Genetic-map functions"),status="danger",width=12, solidHeader = TRUE,collapsible = TRUE, collapsed=FALSE,
                        htmltools::br(),
                        shinyjs::hidden(
                          shiny::fluidRow(id=ns("barplot_sh"),
                              shiny::column(width=12,shiny::actionButton(ns("barplot_bc1"), "Show barplot",style = "color: black;background-color: #87CEFA"))
                        )),
                        shinyjs::hidden(
                          shiny::fluidRow(id=ns("bar_bc"),
                             shiny::column(width=12, "Count of best fitting genetic-map function over chromosomes."),
                             shiny::column(width=12,style="padding-top:30px", ""),
                             shiny::column(width=12,shiny::actionButton(ns("barplot_bc2"), "Hide barplot",style="background-color: #87CEFA")
                                         ,shiny::downloadButton(ns("downloadBarplot"),label="Save barplot",style="background-color: #87CEFA",class="butt1")),
                             shiny::column(width=1,""),
                             shiny::column(width=5,shiny::plotOutput(ns("barplot")))
                        )),
                        shinyjs::hidden(
                          shiny::fluidRow(id=ns("show_line"),shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;")))
                        ),
                        htmltools::br(),
                        shiny::fluidRow(
                          shiny::column(width=5, "Genetic-map function that fits best is highlighed.")),
                          htmltools::br(),
                          shiny::fluidRow(
                                shiny::column(width=12, DT::dataTableOutput(ns("tableBestmapFunction")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
                          ),
                          shiny::fluidRow(shiny::column(width=5, "MSE - mean squared error"))
      ))
    )
  )
}


# Module Server
#' @rdname mod_bc_genetic_function
#'
#' @param filter character contains the selected chromosome
#' @param breed.infos data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param names.files string containing the concatenate breed names
#' @param make.traff.light.bc object containing the ggplot for the likelihood quality signal (\link{make_traffic_light})
#' @param approach data frame containing the predefined settings and names for the selected approach (\link{table_approach_information})
#' @param figure_rend2_gf reactive value indicates if the plot in the module has been fully rendered. Used on the main server to enable or disable "Breed comparison" sidebar elements.
#'
#' @export
mod_bc_genetic_function_server <- function(id,filter,breed.infos,names.files,make.traff.light.bc,approach,figure_rend2_gf){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## initialize
    out<-output.name.tab<-NULL
    use.colors=c("#FF8C00","#eeb422","#ffa07a")

    ## A delay was added to ensure the DOM is ready before executing shinyjs show and hide functions.
    ## Initialize which UI parts are shown.
    shinyjs::delay(3,
         if(match("Likelihood_male",approach$Approach,0)==0){   # likelihood approach not selected
               shinyjs::show(id=ns("no.other.method"),asis=TRUE)
               shinyjs::hide(id=ns("only.likelihood1"),asis=TRUE)
               shinyjs::hide(id=ns("only.likelihood2"),asis=TRUE)
         }else{
               shinyjs::hide(id=ns("no.other.method"),asis=TRUE)
               shinyjs::show(id=ns("only.likelihood1"),asis=TRUE)
               shinyjs::show(id=ns("only.likelihood2"),asis=TRUE)

               if(filter=="All"){
                     shinyjs::show(id=ns("all_chromosome_bc"),asis=TRUE)
                     shinyjs::hide(id=ns("bar_bc"),asis=TRUE)
                     shinyjs::show(id=ns("barplot_sh"),asis=TRUE)
                     shinyjs::show(id=ns("show_line"),asis=TRUE)
                     shinyjs::hide(id=ns("single_chromosome_bc"),asis=TRUE)
               }
               else{
                    shinyjs::show(id=ns("single_chromosome_bc"),asis=TRUE)
                    shinyjs::hide(id=ns("all_chromosome_bc"),asis=TRUE)
                    shinyjs::hide(id=ns("bar_bc"),asis=TRUE)
                    shinyjs::hide(id=ns("barplot_sh"),asis=TRUE)
                    shinyjs::hide(id=ns("show_line"),asis=TRUE)
               }
        }
    )


    ## React to when the first plot for all chromosome is fully rendered (see output$genetic_bc_function3) and
    ## update the figure_rend2_gf() to enable the "Breed comparison" sidebar elements
    observeEvent(input$genetic_bc_functions3_rendered_plot1_bc, {
      figure_rend2_gf(TRUE)
    })

    ## React to when the second plot for all chromosome is fully rendered (see output$genetic_bc_functions4) and
    ## update the figure_rend2_gf() to enable the "Breed comparison" sidebar elements
    observeEvent(input$genetic_bc_functions4_rendered_plot2_bc, {
      figure_rend2_gf(TRUE)
    })

    ## React to when plot for specific chromosome is fully rendered (see output$genetic_bc_functions5) and
    ## update the figure_rend2_gf() to enable the "Breed comparison" sidebar elements
    observeEvent(input$genetic_bc_functions5_rendered_breed_bc, {
      figure_rend2_gf(TRUE)
    })


    if(match("Likelihood_male",approach$Approach,0)==0) # likelihood approach not selected
    {
      figure_rend2_gf(TRUE)  #  enabling the "Breed comparison" sidebar elements
    }else
    {
      ### initialize
      out.2<-pp<-hepl_1<-c()

      # read.in data and prepare data
      naming=c()
      for(ij in 1:nrow(breed.infos))
      {
        load(system.file("extdata", paste0(breed.infos$Name[ij],"/bestmapfun.Rdata"),package="CLARITY")) ##
        out.new<-out

        use=c(2,4,6,8)
        out.new[,use]=round(out.new[,use],7)

        pp2=matrix(0,dim(out)[1],dim(out.new)[2])
        colnames(pp2)=colnames(out.new)

        ## for header output table csv or excel
        if(ij==1)naming=c("Chr",paste0(breed.infos$Name[ij][ij],"-", colnames(out.new)[-1]))
        else naming=c(naming,paste0(breed.infos$Name[ij],"-",colnames(out.new)[-1]))

        ## find the the best genetic-map function - it is necessary for coloring in the corresponding tables
        for(i in 1:dim(out.new)[1])
        {
          p=which(out.new[i,use]==min(out.new[i,use]))
          out.new[i,use[p]]=paste0(out.new[i,use[p]],"*") ## for table preparation - mark best function with *
          pp2[i,use[p]]=out.new[i,use[p]] ##
        }

        hepl_0=sapply(seq(2,ncol(pp2),2),function(ik)ifelse(pp2[,ik]!=0,use.colors[ij],""))
        hepl_1=cbind(hepl_1,hepl_0)

        if(ij==1){
          out.2=cbind(out.2,out.new)
          pp=cbind(pp,pp2)
        }
        else
        {
          out.2=cbind(out.2,out.new[,-1])
          pp=cbind(pp,pp2[,-1])
        }
      }
      colnames(out.2)=c(1:ncol(out.2))
      colnames(pp)=c(1:ncol(pp))

      ####  when specific chromosome is selected
      if(filter!="All")
      {
        ## render traffic light
        height2=ifelse(nrow(breed.infos)==2,45,55)
        output$TrafficLight_2 <- renderPlot({
          make.traff.light.bc()
        }, width=165, height=height2)

        ## prepare data for table
        table.show<-matrix(out.2[as.numeric(filter),],1,ncol(out.2))
        colnames(table.show)=c(1:ncol(table.show))

        help_3=pp[as.numeric(filter),seq(2,ncol(pp),2)]

        color=seq(2,2*4*nrow(breed.infos),2)[which(help_3=="0")]
        columns2hide=c(color,color-1)
        hepl_1=matrix(hepl_1[as.numeric(filter),],1,ncol(hepl_1))

        ## load data for plot
        chr<-as.numeric(as.character(filter))
        store<-NULL
        outcome<-lapply(1:nrow(breed.infos),function(ik){
          load(system.file("extdata",paste0(breed.infos$Name[ik],"/curve-short-",chr,".Rdata"),package="CLARITY"))
          a2=cbind(store[[2]],store[[3]],rep(ik,nrow(store[[2]])))
          colnames(a2)=c(paste0("X",1:4),paste0("Y",1:4),"Breed")
          a2
        })

        ## create and render plot
        output$genetic_bc_functions5 <- plotly::renderPlotly({
          output.filename=paste0(names.files,"-mapping-functions_BTA-",chr)
          pp0=makePlot_genetic_function_bc(chromo=chr,df.list=outcome,names.bc.plot=breed.infos$Name,name.file=output.filename)
          pp0%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_breed_bc', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
        })
      }
      else ## when all chromosome is selected
      {
        ## render traffic light
        height2=ifelse(nrow(breed.infos)==2,45,55)
        output$TrafficLight_3 <- renderPlot({
          make.traff.light.bc()
        }, width=165, height=height2)


        #### react to the input of the first chromosome selection: load, prepare and plot data and send signal when plot is fully rendered
        shiny::observeEvent(input$GenetMapchr1,{
          figure_rend2_gf(FALSE)
          chr1 <- input$GenetMapchr1
            store=NULL; outcome1=list()
            outcome1<-lapply(1:nrow(breed.infos),function(ik){
            load(system.file("extdata",paste0(breed.infos$Name[ik],"/curve-short-",chr1,".Rdata"),package="CLARITY"))
            a1=cbind(store[[2]],store[[3]],rep(ik,nrow(store[[2]])))
            colnames(a1)=c(paste0("X",1:4),paste0("Y",1:4),"Breed")
            a1
          })
            output$genetic_bc_functions3 <- plotly::renderPlotly({
            output.filename1=paste0(names.files,"mapping-functions_BTA-",chr1)
            pp2=makePlot_genetic_function_bc(chromo=chr1,df.list=outcome1,names.bc.plot=breed.infos$Name,name.file=output.filename1)
            pp2%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_plot1_bc', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
          })
        })

        #### react to the input of the second chromosome selection: load, prepare and plot data and send signal when plot is fully rendered
        shiny::observeEvent(input$GenetMapchr2,{
          figure_rend2_gf(FALSE)
          chr2 <- input$GenetMapchr2
          store=NULL; outcome2=list()
          outcome2<-lapply(1:nrow(breed.infos),function(ik){
            load(system.file("extdata",paste0(breed.infos$Name[ik],"/curve-short-",chr2,".Rdata"),package="CLARITY"))
            a=cbind(store[[2]],store[[3]],rep(ik,nrow(store[[2]])))
            colnames(a)<-c(paste0("X",1:4),paste0("Y",1:4),"Breed")
            a
          })

          output$genetic_bc_functions4 <- plotly::renderPlotly({
            chr2=as.numeric(as.character(input$GenetMapchr2))
            output.filename2=paste0(names.files,"mapping-functions_BTA-",chr2)
            pp3=makePlot_genetic_function_bc(chromo=chr2,df.list=outcome2,names.bc.plot=breed.infos$Name,name.file=output.filename2)
            pp3%>%plotly::toWebGL()%>% htmlwidgets::onRender( "
                  function(el, x) {
                    // send info to shiny when the figure is fully rendered
                    Shiny.setInputValue(el.id + '_rendered_plot2_bc', {
                    timestamp: new Date().toISOString()
                    });
                  }
                ")
          })
        })

        ## table
        table.show<-out.2

        ### preparation for histogram
        helper1=matrix(0,29,length(seq(2,2*4*nrow(breed.infos),2)))

        ## Determine count for each breed how often
        values<-unlist(lapply(seq(2,2*4*nrow(breed.infos),2),function(inn){
          length(pp[which(pp[,inn]!=0),inn])
        }))

        ## all for barplot
        Input_barplot<-shiny::reactive(make_barplot_bc(breed.infos=breed.infos,counts=values))

        output$barplot <- shiny::renderPlot({
          Input_barplot()
        },width=650,height=350)

        ## show / hide
        shiny::observeEvent(input$barplot_bc1,{
          shinyjs::hide(id="barplot_sh")
          shinyjs::show(id="bar_bc")
        })
        shiny::observeEvent(input$barplot_bc2,{
          shinyjs::show(id="barplot_sh")
          shinyjs::hide(id="bar_bc")
        })

        ## determine columns which has to be hide in the table
        help_3=as.matrix(pp[,seq(2,ncol(pp),2)])
        uses= t(as.matrix(values))
        colnames(uses)=colnames(table.show)[seq(2,ncol(table.show),2)]
        pp=which(uses[1,]==0)
        uses2=as.matrix(uses[,pp])
        columns2hide=match(rownames(uses2),colnames(table.show))
        columns2hide=c(columns2hide,match(rownames(uses2),colnames(table.show))-1)
      }

      ## create header for csv or EXCEL outcome
      names=paste0("'",naming,"'",collapse=",")
      names2=paste0("[",names,"];",collapse=",")

      createHeader <- c(
        "function(data, columnIdx) {",
        "  var headers =",names2,
        "  return headers[columnIdx];",
        "}"
      )

      ## Table header with internal link to methodology
      sketch12<-create_table_header_bc4(breed.names=breed.infos$Name)

      ## rename colnames for colvis - or make "show / hide column" - necessary when more than two breeds are considered
      output$tableBestmapFunction=DT::renderDataTable({
         output.name.tab=paste0(names.files,"_Best-Mapping-Function_BTA-",filter)
          DT::datatable(table.show,filter="none",container=sketch12,extensions=c("Buttons",'ColReorder'),options=list(searching=FALSE,dom='Bt',colReorder = TRUE,
                        buttons = list('pageLength','copy',
                                       list(extend='csv',title= output.name.tab, exportOptions = list(columns = ":visible",format = list(header = DT::JS(createHeader)))),
                                       list(extend='excel',title= output.name.tab, exportOptions = list(columns = ":visible",format = list(header = DT::JS(createHeader))))),
                        columnDefs = list(list(visible=FALSE, targets=columns2hide)),pagelength = 10, lengthMenu = list(c(10, 15, -1), c('10', '20','All'))),
                        escape=FALSE)%>%DT::formatStyle(columns=colnames(table.show)[seq(2,ncol(table.show),2)],backgroundColor = DT::styleEqual(help_3,hepl_1))
       },server=FALSE)

       ## download barplot
       output$downloadBarplot <- shiny::downloadHandler(
          filename =paste0(names.files,"_Best-Mapping-Function_Histogram.png") ,
          content = function(file){
              ggplot2::ggsave(file, plot = Input_barplot(), device = "png",width=9,height=5,units="in",dpi=300)
       })
    }
  })## Module server
}  #End

## To be copied in the UI
# mod_bc_genetic_function_ui("bc_genetic_function_1")

## To be copied in the server
# mod_bc_genetic_function_server("bc_genetic_function_1")
