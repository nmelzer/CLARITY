# bc_general UI Function
#' @title mod_bc_general_ui and mod_bc_general_server
#'
#' @param id module id
#'
#' @description  A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed comparison".
#'
#' @details Within the module the data are prepared for plotting using the function \link{transformdata_general_bc}. The plots are created using the functions
#' \link{scatterPlot_general_all_bc} or \link{scatterPlot_general_selected_bc} depending on user's selected chromosome.
#'  Corresponding hovering information and style are generated using the function \link{hovering}. This module also shows a Venn diagram of corresponding markers.
#'  The input data are prepared using the function \link{process_venn_data} and plotted using the function \link{creating_venn}.
#'
#' @rdname mod_bc_general
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @importFrom RVenn Venn
#' @rawNamespace import(shinyjs, except = runExample)
#'
#' @seealso  \link{transformdata_general_bc},\link{scatterPlot_general_all_bc}, \link{scatterPlot_general_selected_bc}, \link{hovering}, \link{process_venn_data} and \link{creating_venn}.
#'
#' @export

mod_bc_general_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::br(),htmltools::br(),
    shiny::fluidRow(
      box(title = tags$b("Genetic map summary"),status="danger",width=12,
          solidHeader = TRUE,collapsible = TRUE, color="black",
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
                        shiny::column(width=12,shiny::downloadButton(outputId=ns("download.venn.diagram.bc"),label="Download venn diagram",style="background-color: #87CEFA")),
                        shiny::column(width=6,shiny::plotOutput(ns("venn_diagram_gm"),width = "100%",
                                                    height = "300px")),column(width=6,""))
    ),
    shiny::fluidRow(
      box(title=tags$b("Genetic versus physical length of the bovine autosomes"), status="danger",width=12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
          shiny::fluidRow(shiny::column(width=8,
                          tags$b(tags$h5("Interactive graphic: moving the mouse over the points will show the corresponding information.")))
          ),
          htmltools::br(),htmltools::br(),
          ## layout for the two plots -  next to each others
          shiny::fluidRow(
            shiny::column(width=1),shiny::column(width=5,tags$h3("Deterministic approach")),
            shiny::column(width=1),shiny::column(width=5,tags$h3("Likelihood-based approach")),
            shiny::column(width=2,shiny::downloadButton(outputId=ns("DownloadPlot_gm_1"),"Save plot")),
            shiny::column(4),
            shiny::column(width=2,shiny::downloadButton(outputId=ns("DownloadPlot_gm_2"),"Save plot")),
            shiny::column(width=4,""),
            shiny::column(width=6,
                          shiny::plotOutput(ns("plot_gm_1"),hover = hoverOpts(ns("plot_hover_gm_1"), delay = 100, delayType = "debounce"),width="100%",height="auto"),
                          shiny::uiOutput(ns("hover_info_gm_1"))),
            shiny::column(width=6,
                          shiny::plotOutput(ns("plot_gm_2"),hover = hoverOpts(ns("plot_hover_gm_2"), delay = 100, delayType = "debounce"),width="100%",height="auto"),
                          shiny::uiOutput(ns("hover_info_gm_2")))
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
#' @param filter selected chromosome
#' @param breed.select.bc vector containing the names of selected breeds
#' @param geneticMap.bc  list containing the genetic maps for all selected breeds (each list element a breed)
#' @param color.shape.def data frame containing the definition for coloring, shapes, size for plots
#' @param names.bc.venn vector containing the first letter of the selected breed names
#' @param names.files string containing the concatenate breed names
#'
#' @export

mod_bc_general_server <- function(id,filter,breed.select.bc, geneticMap.bc,color.shape.def,names.bc.venn,names.files){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    InputPlot_gm_venn<-genetic_map_summary<-NULL

    dt2=list(); label=c()
    for(ik in 1:length(breed.select.bc))
    {
      load(system.file("extdata", paste0(breed.select.bc[ik],"/genetic_map_summary.Rdata"),package="CLARITY"))
      dt2[[ik]]=genetic_map_summary
      label=c(label,seq(ik,ncol(genetic_map_summary)*length(breed.select.bc),length(breed.select.bc)))
    }

    for(i in 1:length(breed.select.bc)){
      if(i==1)dt=dt2[[i]]
      else dt=cbind(dt,dt2[[i]])
    }

    dt=dt[,order(as.numeric(label))]
    dt=dt[,-c(2:length(breed.select.bc))]


    shiny::observe({
      shinyjs::toggle(id = "legend_html", condition = input$checkbox)
    })

    output$legend_html<-shiny::renderUI({
      HTML(paste0(".",names.bc.venn,": ",breed.select.bc, "<br>"),
           "Chr: chromosome<br>nSNP: number of SNPs<br> bp: chromosome length in base pairs<br>
                                  Gap: maximum gap size between pairs of adjacent markers in bp<br>
                                  Space: inter-marker space in kilobase (kb) <br> nRec: number of cross-overs detected <br>
                                  D(M): genetic length in Morgan estimated based on deterministic approach <br>
                                  L(M): genetic length in Morgan estimated with the likelihood-based approach <br>
                                  D(cM/Mb): centiMorgan per megabase pair for the deterministic approach <br>
                                  L(cM/Mb): centiMorgan per megabase pair for the likelihood-based approach")
    })

    ####################
    # a custom table container
    tr<-thead<-th<-NULL
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=1,""),
          lapply(colspan=5*length(breed.select.bc), "Physical Map", th),
          lapply(colspan=4*length(breed.select.bc), "Genetic Map", th),
        ),
        tr(
          th(colspan=1,""),
          th(colspan=5*length(breed.select.bc),""),
          th(colspan = 2*length(breed.select.bc),tags$a(href="#","Deterministic approach", onclick = "openTab('methodology')") ),
          th(colspan = 2*length(breed.select.bc), tags$a(href="#","Likelihood-based approach",onclick = "openTab('methodology')"))
        ),
        tr(
        th(colspan=1,"Chr"),
        lapply(colspan=1,paste0("nSNP.",names.bc.venn),th),
        lapply(colspan=1,paste0("bp.",names.bc.venn),th),
        lapply(colspan=1,paste0("Gap.",names.bc.venn),th),
        lapply(colspan=1,paste0("Space.",names.bc.venn),th),
        lapply(colspan=1,paste0("nRec.",names.bc.venn),th),
        lapply(colspan=1,paste0("D(M).",names.bc.venn),th),
        lapply(colspan=1,paste0("D(cM/Mb).",names.bc.venn),th),
        lapply(colspan=1,paste0("L(M).",names.bc.venn),th),
        lapply(colspan=1,paste0("L(cM/Mb).",names.bc.venn),th)
       )
      )
    ))

    file.name=paste0(names.files,"_BTA-",filter)

    if(filter=="All"){
      output$table0=DT::renderDataTable({
        DT::datatable(dt,extensions = "Buttons", filter="none",container=sketch,rownames=FALSE ,escape=FALSE, options = list(searching=FALSE,dom='Bfrtip',
                                                                          columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                         buttons = list('pageLength', 'copy',list(extend='csv',title=file.name), list(extend='excel',title=file.name)),
                                                                           pagelength = 10, lengthMenu = list(c(5,10, 15, -1), c('5','10', '15','All'))))
      },server=FALSE)
    }
    else
    {
      fg2=dt[which(dt$Chr==filter),]
      output$table0=DT::renderDataTable({
        DT::datatable(fg2,filter="none", extensions = "Buttons", container=sketch, rownames=FALSE,escape=FALSE, options = list(searching = FALSE,columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                        dom = 'Bt', buttons = list('copy',list(extend='csv',title=file.name), list(extend='excel',title=file.name))))
      })
    }

    ######################## All regarding plots
     ################ plots
    ## transform data for the plot

    data2=transformdata_general_bc(data1 = dt2,what="det",breed.select.bc,colo=color.shape.def)
    data3=transformdata_general_bc(data1 = dt2,what="lik",breed.select.bc,colo=color.shape.def)

    ## perform plots
    if(filter=="All"){

      ## venn diagrams
      filename.venn<-paste0(names.files,"_BTA_",filter,"-all-markers.png")

      venn.data.all<-lapply(1:length(breed.select.bc), function(i) geneticMap.bc[[i]]$Name)
      names(venn.data.all)<-names.bc.venn

      venn <- RVenn::Venn(venn.data.all)

      venn_data <-process_venn_data(venn)

      InputPlot_gm_venn<<-shiny::reactive(
        creating_venn(venn_data,breed.bc=breed.select.bc, bc.venn=names.bc.venn)
         )

      output$venn_diagram_gm <- shiny::renderPlot({
        InputPlot_gm_venn()
      })

      #  scatter PLot - deterministic
      InputPlot_gm_1=shiny::reactive(
          scatterPlot_general_all_bc(dat=data2,names.bc.plot=breed.select.bc,colo=color.shape.def)
          )

      file.name_gm_1=paste0(names.files,"-genetic_vs_physical_length-deterministic-approach.png")   ## 26.07.2022 added breed

      output$plot_gm_1 <- shiny::renderPlot({
        InputPlot_gm_1()
      },height=600 #, height = 800 ## here sizes can be changed of figures
      )

      #  scatter PLot - lik
      InputPlot_gm_2=shiny::reactive(
          scatterPlot_general_all_bc(dat=data3,names.bc.plot=breed.select.bc,colo=color.shape.def)
       )


      file.name_gm_2=paste0(names.files,"-genetic_vs_physical_length-likelihood-based-approach.png")   ## 26.07.2022 added breed

      output$plot_gm_2 <- shiny::renderPlot({
        InputPlot_gm_2()
      },height=600 #, height = 800 ## here sizes can be changed of figures
      )
    }

    if(filter!="All"){
      ## venn diagrams
      filename.venn<-paste0(names.files,"_BTA-",filter,"-all-markers.png")

      venn.data.all<-lapply(1:length(breed.select.bc), function(i) geneticMap.bc[[i]][which(geneticMap.bc[[i]]$Chr==filter),]$Name)
      names(venn.data.all)<-names.bc.venn

      venn <- RVenn::Venn(venn.data.all)
      venn_data <-process_venn_data(venn)

      InputPlot_gm_venn<<-shiny::reactive(creating_venn(venn_data,breed.bc=breed.select.bc, bc.venn=names.bc.venn))

      output$venn_diagram_gm <- shiny::renderPlot({
        InputPlot_gm_venn()
      })


      ## scatter plot - deterministic
      InputPlot_gm_1=shiny::reactive(scatterPlot_general_selected_bc(dat=data2,fil=as.numeric(as.character(filter)),names.bc.plot=breed.select.bc,colo=color.shape.def))

      file.name_gm_1=paste0(names.files,"-genetic_vs_physical_length_BTA-",filter,"_deterministic-approach.png") ## 26.07.2022 added breed

      output$plot_gm_1 <- shiny::renderPlot({
        InputPlot_gm_1()
      },height=600
      )

      #  scatter PLot - likelihood-based approach
      InputPlot_gm_2=shiny::reactive(scatterPlot_general_selected_bc(dat=data3,fil=as.numeric(as.character(filter)),names.bc.plot=breed.select.bc,colo=color.shape.def))

      file.name_gm_2=paste0(names.files,"-genetic_vs_physical_length_BTA-",filter,"-likelihood-based-approach.png") ## 26.07.2022 added breed

      output$plot_gm_2 <- shiny::renderPlot({
        InputPlot_gm_2()
      },height=600 #, height = 800 ## here sizes can be changed of figures
      )
    }

    ## add hover
    output$hover_info_gm_1 <- shiny::renderUI({
      if(!is.null(input$plot_hover_gm_1)){
        hovering(dat1=data2,hover=input$plot_hover_gm_1,what=3)
      }})

    output$hover_info_gm_2 <- shiny::renderUI({
      if(!is.null(input$plot_hover_gm_2)){
        hovering(dat1=data3,hover=input$plot_hover_gm_2,what=3)
      }})


    ## handle download plots
    output$DownloadPlot_gm_1 <- shiny::downloadHandler(
      filename = file.name_gm_1,
      content = function(file) {
        showModal(modalDialog("Loading", footer=NULL))
        on.exit(removeModal())
        ggsave(file, plot = InputPlot_gm_1(), device = "png",width=20,height=10,units="in",dpi=300)
      })

      output$DownloadPlot_gm_2 <- shiny::downloadHandler(
        filename = file.name_gm_2,
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          ggsave(file, plot = InputPlot_gm_2(), device = "png",width=20,height=10,units="in",dpi=300)
        }
    )

    output$download.venn.diagram.bc <- shiny::downloadHandler(
        filename = filename.venn ,###    distinguish between both maps
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          ggsave(file, plot = InputPlot_gm_venn(), device = "png",width=6,height=6,units="in",dpi=300)
        })
  })
}

## To be copied in the UI
# mod_bc_general_ui("bc_general_1")

## To be copied in the server
# mod_bc_general_server("bc_general_1")
