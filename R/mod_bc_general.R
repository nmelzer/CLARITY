# bc_general UI Function
#' @title mod_bc_general_ui and mod_bc_general_server
#'
#' @param id module id
#'
#' @description  A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed comparison".
#'
#' @details Within the module the data are prepared for the table and for plotting using the function \link{transformdata_general_bc} depending on user's selected chromosome.
#' The corresponding plots are created using the function \link{scatterPlot_general}.
#'  This module also shows a Venn diagram of corresponding markers. The corresponding input data are
#'  prepared using the function \link{process_venn_data} and plotted using the function \link{creating_venn}.
#'  Finally, the quality of the likelihood approach is also shown here by means of a traffic light (also termed as likelihood quality signal \link{make_traffic_light}).
#'
#' @rdname mod_bc_general
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @importFrom ggVennDiagram Venn
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput renderPlotly toWebGL
#' @importFrom ggplot2 ggsave
#' @rawNamespace import(shinyjs, except = runExample)
#'
#' @seealso
#' * \link{transformdata_general_bc} \cr
#' * \link{scatterPlot_general} \cr
#' * \link{process_venn_data} \cr
#' * \link{creating_venn} \cr
#' * \link{make_traffic_light}
#'
#' @export

mod_bc_general_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    htmltools::br(),htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(title = tags$b("Genetic map summary"),status="danger",width=12,solidHeader = TRUE,collapsible = TRUE, color="black",
          htmltools::br(),
          shiny::fluidRow(
            shiny::column(width=12,DT::dataTableOutput(ns("table0")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
          ),
          htmltools::br(),
          shiny::fluidRow(
            shiny::column(width=10,shiny::checkboxInput(ns("checkbox"), "Show/hide legend", TRUE),
                          shiny::uiOutput(ns("legend_html"))))
      )
    ),
    shiny::fluidRow(box(title=tags$b("Venn diagram of markers"), status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        shiny::column(width=12,shiny::downloadButton(outputId=ns("download.venn.diagram.bc"),label="Download venn diagram",style="background-color: #87CEFA",class="butt1")),
                        shiny::column(width=12,style='overflow-x: auto;',shiny::plotOutput(ns("venn_diagram_gm"),width = "20%",height="auto")))
    ),
    shiny::fluidRow(
      shinydashboard::box(title=htmltools::tags$b("Genetic versus physical length of the bovine autosomes"), status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(shiny::column(width=12,
                          htmltools::tags$b( htmltools::tags$h5("Interactive graphic: moving the mouse over the points will show the corresponding information.")))
          ),
         # shiny::fluidRow(shiny::column(width=4,"")),
          shiny::fluidRow(id=ns("showhideSignal"),shiny::column(width=9,""),
                          shiny::column(width=3,tags$a(href="#","Likelihood quality signal", onclick = "openTab('methodology')",style='text-decoration-line: underline;'),
                                     style='overflow-x: auto;', shiny::plotOutput(ns("TrafficLight_nn"),width="20%",height="auto") )
          ),
          shiny::fluidRow(shiny::column(width=4,"")),
          shiny::fluidRow(shiny::column(width=11,
                                          plotly::plotlyOutput(ns("plot1"),width="auto",height="auto")%>% shinycssloaders::withSpinner(color="#0dc5c1"))

          ),
          shiny::fluidRow(
            htmltools::br(),
            shiny::column(width=12, "cM: centiMorgan; Mbp: megabase pairs"),
            htmltools::br(),
          )
      )
    )

  )
}

# Module Server
#' @rdname mod_bc_general
#'
#' @param filter character contains the selected chromosome
#' @param geneticMap.bc  list containing the genetic maps for all selected breeds (each list element is a breed)
#' @param names.files character containing the concatenated breed names and is used as an addition to the file name
#' @param make.traff.light.bc object containing the ggplot for the likelihood quality signal (\link{make_traffic_light})
#' @param breed.infos data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param approach character containing the abbreviation for the selected approach
#' @param dt list whereby each list element contains for each selected breed the genetic map summary information
#'
#' @export

mod_bc_general_server <- function(id,filter,geneticMap.bc,names.files,make.traff.light.bc,breed.infos,approach,dt){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    InputPlot_gm_venn<-genetic_map_summary<-NULL

    # if the likelihood approach is selected than likelihood-quality signal is rendered and shown
    if(approach=="Lm"){
      shinyjs::show("showhideSignal")

      height2 <- ifelse(length(nrow(breed.infos))==2, 45, 55)

      output$TrafficLight_nn <- renderPlot({
        make.traff.light.bc()
      }, width=165, height=height2)
    }
    else shinyjs::hide("showhideSignal")

    ##### prepare data
    use=grep(approach,colnames(dt[[1]]))

    dt2<-lapply(1:nrow(breed.infos), function(ik){
              a=dt[[breed.infos$Name[ik]]][c(1:5,use)]
              colnames(a)[2:ncol(a)]=paste0(colnames(a)[2:ncol(a)],".",breed.infos$Abbreviation[ik])
              a
          })

    ### get the number/index - relevant for the final sort of the data.frame columns and exclusions
    label<-unlist(lapply(1:nrow(breed.infos), function(ik){
      seq(ik,ncol(dt2[[ik]])*nrow(breed.infos),nrow(breed.infos))
    }))

    dt<-do.call("cbind",dt2)
    dt=dt[,order(as.numeric(label))]
    dt<-dt[,-c(2:nrow(breed.infos))]


    # show/ hide legend
    shiny::observe({
      shinyjs::toggle(id = "legend_html", condition = input$checkbox)
    })

    #render legend
    output$legend_html<-shiny::renderUI({
      HTML(paste0(".",breed.infos$Abbreviation,": ",breed.infos$Name, "<br>"),
           "Chr: chromosome<br>  nSNP: number of SNPs<br> bp: chromosome length in base pairs<br>
                                  Gap: maximum gap size between pairs of adjacent markers in bp<br>
                                  Space: inter-marker space in kilobase (kb) <br> nRec: number of cross-overs detected <br>
                                  M: genetic length in Morgan <br>
                                  cM/Mb: centiMorgan per megabase pair <br>")
    })

    ####################
    # create custom table header
    sketch<-create_table_header_bc1(breed.infos,label,no.cols=ncol(dt2[[1]]),header.length=ncol(dt))

    ## table
    file.name=paste0(names.files,"_BTA-",filter,"-",approach)
    if(filter=="All")dt.table=dt
    else dt.table=dt[which(dt$Chr==filter),]

    output$table0=DT::renderDataTable({
        DT::datatable(dt.table,extensions = "Buttons",  container=sketch, filter="none",rownames=FALSE ,escape=FALSE,
                      options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                     buttons = list('pageLength', 'copy',list(extend='csv',title=file.name), list(extend='excel',title=file.name)),
                                     pagelength = 10, lengthMenu = list(c(5,10, 15, -1), c('5','10', '15','All'))))
      },server=FALSE)

    ## define output size for venn and plot
    if(nrow(breed.infos)==2)
    {
      width1= 400
      height1= 150
      width1.venn.plot=6
      height1.venn.plot=2.5
    }
    else
    {
      width1= 600
      height1= 350
      width1.venn.plot=9
      height1.venn.plot=6
    }

    ## prepare scatter plot
    data2<-transformdata_general_bc(data1 = dt2,breed.infos,filter)

    ## scatter plot
    output$plot1 <- plotly::renderPlotly({
      file.name=paste0(names.files,"-genetic_vs_physical_length_BTA-",filter,"-",approach)
      plot2<-scatterPlot_general(dat=data2,name.file=file.name)
      plot2%>%plotly::toWebGL()
    })

    ### all for venn
    if(filter=="All"){
      filename.venn<-paste0("Venn-Diagram-",names.files,"_BTA_-all-",approach,".png")
      venn.data.all<-lapply(1:nrow(breed.infos), function(i) geneticMap.bc[[i]]$Name)
    }else{
      filename.venn<-paste0("Venn-Diagram-",names.files,"_BTA-",filter,"-",approach,".png")
      venn.data.all<-lapply(1:nrow(breed.infos), function(i) geneticMap.bc[[i]][geneticMap.bc[[i]]$Chr%in%filter,]$Name)
    }

    names(venn.data.all)<-breed.infos$Abbreviation
    venn <- ggVennDiagram::Venn(venn.data.all)
    venn_data <-process_venn_data(venn)

    colors=create.colors(colo=breed.infos$Color)
    InputPlot_gm_venn<<-shiny::reactive(creating_venn(venn_data,long.names=breed.infos$Name, abbreviations=breed.infos$Abbreviation,venn.colors=colors))

    output$venn_diagram_gm <- shiny::renderPlot({
      InputPlot_gm_venn()
    }, width=width1,height=height1)

    output$download.venn.diagram.bc <- shiny::downloadHandler(
        filename = filename.venn ,
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          ggplot2::ggsave(file, plot = InputPlot_gm_venn(), bg="white",device = "png",width=width1.venn.plot,height=height1.venn.plot,units="in",dpi=300)
    })
  })
}

## To be copied in the UI
# mod_bc_general_ui("bc_general_1")

## To be copied in the server
# mod_bc_general_server("bc_general_1")
