# bc_genetic_function UI Function
#' @title mod_bc_genetic_function_ui and mod_bc_genetic_function_server
#' @description A shiny module to generate the outcome for the tabpanel "Genetic-map functions" for the sidebar "Breed comparison".
#'
#' @details The module uses the function \link{makePlot_genetic_function_bc} for plotting.
#' @rdname mod_bc_genetic_function
#' @param id module id
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#' @import ggplot2
#' @rawNamespace import(shinyjs, except = runExample)
#' @rawNamespace import(plotly, except = last_plot)
#' @export
#'
mod_bc_genetic_function_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(title= tags$b("Interactive graphical visualization"),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(shinyjs::useShinyjs(),id=ns("all_chromosome_bc"),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(12, "Remove a genetic map function in the figure by clicking on the corresponding name in the legend."),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=6,shiny::selectizeInput(inputId=ns('GenetMapchr1'),label = "Select chromosome",choices=c(seq(1,29,1)),selected=18,multiple=FALSE),
                          plotly::plotlyOutput(ns("genetic_functions3"))%>% shinycssloaders::withSpinner(color="#0dc5c1")
                   ),
                   shiny::column(width=6,
                           shiny::selectizeInput(inputId=ns('GenetMapchr2'), label = "Select chromosome",choices=c(seq(1,29,1)),selected=28,multiple=FALSE),#)#),
                          plotly::plotlyOutput(ns("genetic_functions4"))%>% shinycssloaders::withSpinner(color="#0dc5c1")
                   )

          ),

          ### show output when specific chromosome is selected
          shiny::fluidRow( useShinyjs(),id=ns("single_chromosome_bc"),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=12, "Remove a genetic-map function in the figure by clicking on the corresponding name in the legend."),
                           shiny::column(width=12,style="padding-top:30px", ""),
                           shiny::column(width=8,plotly::plotlyOutput(ns("genetic_functions5"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1")))
    )),
    htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(title = tags$b("Genetic-map functions"),status="danger",width=12, ## make Link
          solidHeader = TRUE,collapsible = TRUE, collapsed=FALSE,
          htmltools::br(),
          shiny::fluidRow(shinyjs::useShinyjs(),id=ns("barplot"),
                          shiny::column(width=12,shiny::actionButton(ns("barplot_bc1"), "Show barplot",style = "color: black;background-color: #87CEFA"))),
          shiny:: fluidRow(shinyjs::useShinyjs(),id=ns("bar_bc"),
                           shiny::column(width=12,shiny::actionButton(ns("barplot_bc2"), "Hide barplot",style="background-color: #87CEFA")
                                         ,shiny::downloadButton(ns("downloadBarplot"),label="Save barplot",style="background-color: #87CEFA")),
                           shiny::column(width=1,""),
                           shiny::column(width=5,shiny::plotOutput(ns("barplot"))),
          ),
          shiny::fluidRow(shinyjs::useShinyjs(),id=ns("show_line"),shiny::column(width=12,htmltools::hr(style = "border-top: 1px solid #68838B;"))),
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=5, "Genetic-map function that fits best are highlighed.")),
          htmltools::br(),
          shiny::fluidRow(shiny::column(width=12, DT::dataTableOutput(ns("tableBestmapFunction")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
          shiny::fluidRow(shiny::column(width=5, "MSE - mean squared error"))
      )
    )
  )
}


# Module Server
#' @rdname mod_bc_genetic_function
#'
#' @param filter selected chromosome
#' @param breed.select.bc vector containing the names of selected breeds
#' @param color.shape.def data frame containing the definition for coloring, shapes, size for plots
#' @param names.files string containing the concatenate breed names
#'
#' @export
mod_bc_genetic_function_server <- function(id,filter,breed.select.bc,color.shape.def,names.files){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    out<-NULL

    ### table preparation
    out.2=c();pp=c();hepl_1=c() ## coloring

    # max.to.plot=450000 - exclude takes too much time
    for(ij in 1:length(breed.select.bc))
    {
      load(system.file("extdata", paste0(breed.select.bc[ij],"/bestmapfun.Rdata"),package="CLARITY")) ##
      out.new=out

      use=c(2,4,6,8)
      out.new[,use]=round(out.new[,use],7)

      pp2=matrix(0,dim(out)[1],dim(out.new)[2])
      colnames(pp2)=colnames(out.new)

       ## find the the best genetic-map function - it is necessary for coloring in the corresponding tables
      for(i in 1:dim(out.new)[1])
      {
        p=which(out.new[i,use]==min(out.new[i,use]))

        out.new[i,use[p]]=paste0(out.new[i,use[p]],"*")
        pp2[i,use[p]]=out.new[i,use[p]]
      }

      hepl_0=sapply(seq(2,ncol(pp2),2),function(ik)ifelse(pp2[,ik]!=0,color.shape.def$color.table[ij],""))
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

    ## Table header with internal link to methodology --
    thead<-tr<-th<-NULL
    sketch1 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=1,""),
          lapply(colspan=8,breed.select.bc,th)
        ),
        tr(
          th(colspan=1,""),
          th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')")),
          th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')")),
          th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')")),
          th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')")),
          lapply(colspan=2,rep(c("Haldane scaled","Rao","Felsenstein","Liberman & Karlin"),length(breed.select.bc)-1),th)
        ),
        tr(
          th(colspan=1,"Chromosome"),
          lapply(rep(c('MSE', 'Parameter'), 4*length(breed.select.bc)), th) ## shorthened
        )
      )
    ))

    #### all for table preparation
    if(filter!="All"){
      shinyjs::hide(id="all_chromosome_bc")
      shinyjs::show(id="single_chromosome_bc")
      shinyjs::hide(id="bar_bc")
      shinyjs::hide(id="barplot")
      shinyjs::hide(id="show_line")

      out3=matrix(out.2[as.numeric(as.character(filter)),],1,ncol(out.2))
      colnames(out3)=c(1:ncol(out3))
      help_3=pp[as.numeric(as.character(filter)),seq(2,ncol(pp),2)]

      color=seq(2,2*4*length(breed.select.bc),2)[which(help_3=="0")]
      columns2hide=c(color,color-1)
      hepl_1=matrix(hepl_1[as.numeric(as.character(filter)),],1,ncol(hepl_1))

      chr=as.numeric(as.character(filter))
      store<-NULL; outcome=list()
      for(ik in 1:length(breed.select.bc)){
        load(system.file("extdata",paste0(breed.select.bc[ik],"/curve-short-",chr,".Rdata"),package="CLARITY"))

        outcome[[ik]]=cbind(store[[2]],store[[3]],rep(ik,nrow(store[[2]])))
        colnames(outcome[[ik]])=c(paste0("X",1:4),paste0("Y",1:4),"Breed")
      }

      output$genetic_functions5 <- plotly::renderPlotly({
        output.filename=paste0(names.files,"mapping-functions_BTA-",filter)
        plot2=makePlot_genetic_function_bc(chromo=chr,df.list=outcome,names.bc.plot=breed.select.bc,name.file=output.filename)
        plot2%>%plotly::toWebGL()
      })

    }
    else
    {
      shinyjs::show(id="all_chromosome_bc")
      shinyjs::hide(id="single_chromosome_bc")
      shinyjs::hide(id="barplot")
      shinyjs::show(id="bar_bc")
      shinyjs::show(id="show_line")


      shiny::observeEvent(input$GenetMapchr1,{
        chr <- input$GenetMapchr1
        store=NULL; outcome1=list()

        for(ik in 1:length(breed.select.bc)){
          load(system.file("extdata",paste0(breed.select.bc[ik],"/curve-short-",chr,".Rdata"),package="CLARITY"))##store##check_bta_marker_length(chromo=chr,max.plot=max.to.plot)

          outcome1[[ik]]=cbind(store[[2]],store[[3]],rep(ik,nrow(store[[2]])))
          colnames(outcome1[[ik]])=c(paste0("X",1:4),paste0("Y",1:4),"Breed")
        }

        output$genetic_functions3 <- plotly::renderPlotly({
          chr=as.numeric(as.character(input$GenetMapchr1))
          output.filename1=paste0(names.files,"mapping-functions_BTA-",chr)
          pp2=makePlot_genetic_function_bc(chromo=chr,df.list=outcome1,names.bc.plot=breed.select.bc,name.file=output.filename1)
          pp2%>%plotly::toWebGL()
        })

      })

      shiny::observeEvent(input$GenetMapchr2,{
        chr2 <- input$GenetMapchr2
        store<-NULL; outcome2=list()
        for(ik in 1:length(breed.select.bc)){
          load(system.file("extdata",paste0(breed.select.bc[ik],"/curve-short-",chr2,".Rdata"),package="CLARITY"))##store##check_bta_marker_length(chromo=chr,max.plot=max.to.plot)

          outcome2[[ik]]=cbind(store[[2]],store[[3]],rep(ik,nrow(store[[2]])))
          colnames(outcome2[[ik]])=c(paste0("X",1:4),paste0("Y",1:4),"Breed")
        }

        output$genetic_functions4 <- plotly::renderPlotly({
          chr2=as.numeric(as.character(input$GenetMapchr2))
          output.filename2=paste0(names.files,"mapping-functions_BTA-",chr2)
          plot2=makePlot_genetic_function_bc(chromo=chr2,df.list=outcome2,names.bc.plot=breed.select.bc,name.file=output.filename2)
          plot2%>%plotly::toWebGL()
        })
      })


      ## table preparation
      out3=out.2

      Breed=breed.select.bc
      Methods=c(rep("Haldane scaled",length(breed.select.bc)),rep("Rao",length(breed.select.bc)),rep("Felsenstein",length(breed.select.bc)),rep("Liberman & Karlin",length(breed.select.bc)))
      values=c();c=0; helper1=matrix(0,29,length(seq(2,2*4*length(Breed),2)))

      for(i in seq(2,2*4*length(Breed),2)){
        c=c+1
        values[c]=length(pp[which(pp[,i]!=0),i])
      }

      ord3=rep(1:4,length(Breed))
      ord3=sort(ord3,index.return=T)
      Values=values[ord3$ix]
      data=data.frame(Methods,Breed,Values)

      gg=cbind(breed.select.bc,color.shape.def$color1[1:length(breed.select.bc)])
      gg2=gg[order(gg[,1]),]

      help_3=as.matrix(pp[,seq(2,ncol(pp),2)])

      ## all for exclude columns
      uses= t(as.matrix(values))
      colnames(uses)=colnames(out3)[seq(2,ncol(out3),2)]
      pp=which(uses[1,]==0)
      uses2=as.matrix(uses[,pp])
      columns2hide=match(rownames(uses2),colnames(out3))
      columns2hide=c(columns2hide,match(rownames(uses2),colnames(out3))-1)
      ## end

      Input_barplot <-shiny::reactive(
        ggplot2::ggplot(data, aes(fill=Breed, y=Values, x=Methods)) +
        geom_bar(position="dodge", stat="identity")+
        scale_fill_manual(values=gg2[,2],labels=gg2[,1],guide = guide_legend(title = "Legend"))+
        xlab("Methods") +
        ylab("Frequency")+
        theme(legend.title = element_text(size=18), #change legend title font size
              legend.text = element_text(size=16),axis.text=element_text(size=14),axis.title=element_text(size=16))
      )

      output$barplot <- shiny::renderPlot({
        Input_barplot()
      })

      shiny::observeEvent(input$barplot_bc1,{
        shinyjs::hide(id="barplot")
        shinyjs::show(id="bar_bc")
      })

      shiny::observeEvent(input$barplot_bc2,{
        shinyjs::show(id="barplot")
        shinyjs::hide(id="bar_bc")
      })
    }

      ## rename colnames for colvis - or make "show / hide column" - necessary when more than two breeds are considered

      output$tableBestmapFunction=DT::renderDataTable({
         output.name.tab=paste0(names.files,"_Best-Mapping-Function_BTA-",filter)
          DT::datatable(out3,filter="none",container=sketch1 ,extensions=c("Buttons",'ColReorder'), options=list(searching=FALSE,dom='Bt',colReorder = TRUE,buttons = list('pageLength','copy',list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab) ),
                        columnDefs = list(list(visible=FALSE, targets=columns2hide)),pagelength = 10, lengthMenu = list(c(10, 15, -1), c('10', '20','All'))),
                        escape=FALSE)%>%DT::formatStyle(columns=colnames(out3)[seq(2,ncol(out3),2)],backgroundColor = DT::styleEqual(help_3,hepl_1))

     },server=FALSE)

      #hist
      output$downloadBarplot <- shiny::downloadHandler(
        filename =paste0(names.files,"_Best-Mapping-Function_Histogram.png") ,
        content = function(file){
          ggplot2::ggsave(file, plot =  Input_barplot(), device = "png",width=9,height=5,units="in",dpi=300)
        }
      )

  })## Module server
}  #End

## To be copied in the UI
# mod_bc_genetic_function_ui("bc_genetic_function_1")

## To be copied in the server
# mod_bc_genetic_function_server("bc_genetic_function_1")
