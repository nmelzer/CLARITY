# bc_hotspot UI Function
#'
#' @title mod_bc_hotspot_ui and mod_bc_hotspot_server
#' @description A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed comparison".
#'
#' @details Within the module the input data are prepared using the function \link{transformdata_hotspot}. To produce the hotspot plot, the function
#' \link{scatterPlot_hotspot} or \link{scatterPlot_hotspot_all} depending on the user choice of chromosome is used. The hover information are obtained from the function \link{hovering}.
#'  This module also shows a Venn diagram of corresponding hotspot markers. The input data are prepared
#'  using the function \link{process_venn_data} and plotted using the function \link{creating_venn}. The user may select a Venn diagram subset and the
#'  table will be reduced accordingly using the function \link{prepare_table_venn}.
#'
#'
#' @rdname mod_bc_hotspot
#'
#' @param id module id

#' @keywords internal
#' @import shiny
#' @importFrom shinydashboard box
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @rawNamespace import(shinyjs, except = runExample)
#' @importFrom rlang is_empty
#' @import htmltools
#'
#' @seealso \link{transformdata_hotspot}, \link{scatterPlot_hotspot}, \link{scatterPlot_hotspot_all}, \link{hovering},\link{process_venn_data},\link{creating_venn} and \link{prepare_table_venn}
#' @export
#'
mod_bc_hotspot_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')")," intervals.")),
      htmltools::br(),
      htmltools::br(),
      shinydashboard::box(width=12, title=tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
          h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return."),
          shiny::column(width=12,shiny::downloadButton(outputId=ns("downloadHotspot"),"Save plot")),
          shiny::column(width= 12, shiny::plotOutput(ns("plothotspot"),height="750px", dblclick = ns("plothotspot_dblclick"),brush = brushOpts(id = ns("plothotspot_brush"), resetOnNew = TRUE),
                     hover = hoverOpts(ns("plothotspot_hover"), delay = 400, delayType = "throttle",clip=TRUE)) %>% shinycssloaders::withSpinner(color="#0dc5c1"),shiny::uiOutput(ns("hoverhotspot_info")),style="width: calc(100% - 100px); !important;")

      )
    ),

    htmltools::br(),
    shiny::fluidRow(
      shinydashboard::box(width=12,title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",
          solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,

          shiny:: fluidRow(shiny::column(width=12,"Use the slider to define the threshold to hotspot region (standard deviation of recombination rate). This accordingly changes the figure and table.
                                         The default threshold is 2.5.", div(style="margin-bottom:30px")),
                          shiny::column(width=4,
                              shiny::sliderInput(ns("threshold"),"Threshold to define hotspot region",min = 0, max = 10, value = 2.5, step = 0.1),
                          div(style="margin-bottom:60px"))),
          htmltools::hr(style = "border-top: 1px solid #68838B;"),
          shiny:: fluidRow(shinyjs::useShinyjs(),id=ns("venn_hot1"),shiny::column(width=6,shiny::actionButton(ns("venn_hotspot1"), "Show interactively venn diagram",style = "color: black;
                            background-color: #87CEFA"))),
          shiny::fluidRow(shinyjs::useShinyjs(),id=ns("venn_hot2"),
                            shiny::column(width=10,shiny::downloadButton(ns("downloadVenn"),label="Save plot",style="background-color: #87CEFA"),shiny::actionButton(ns("venn_hotspot2"), "Hide venn diagram",style="background-color: #87CEFA"),
                                   shiny::actionButton(ns("ButtonAll_bc_hotspot"),"Reset table to all",style="background-color: #87CEFA")),
                            shiny::column(width=10,style="padding-top:30px", ""),
                            shiny::column(width=10,"When you click on a specific subset of interest, only the markers for that set are listed in the table."),
                            shiny::column(width=5,shiny::plotOutput(ns("venn_diagram"), click = ns("plot_click"),width = "100%",
                                                                         height = "300px"))

          ),
         htmltools:: br(),
         htmltools::hr(style = "border-top: 1px solid #68838B;"),
         htmltools::br(),

         shiny::fluidRow(shiny::column(width=10,DT::dataTableOutput(outputId=ns("tablehotspot")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
         shiny::fluidRow(shiny::column(9,shiny::checkboxInput(ns("checkbox3"), "Show/hide legend", TRUE), ## tried to make the HTML as htmlOutput but it does not work - must be checked later for a better solution
                           htmltools::p(id = "element30",HTML("Chr: chromosome<br>bp: chromosome position in base pairs<br>cM: chromosome position in centiMorgan based on <a href='#' onclick = openTab('methodology') >deterministic approach</a>")))
          )
         )
      )
    )
}

# bc_hotspot Server Functions
#' @rdname mod_bc_hotspot
#'
#' @param filter selected chromosome
#' @param breed.select.bc vector containing the names of selected breeds
#' @param color.shape.def data frame containing the definition for coloring, shapes, size for plots
#' @param names.bc.venn vector containing the first the letter of the selected breed name
#' @param names.files string containing the concatenate breed names
#'
#'@export
mod_bc_hotspot_server <- function(id,filter, breed.select.bc,color.shape.def,names.bc.venn,names.files){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    adjacentRecRate<-NULL

    ## show / hide legend
    shiny::observe({
      shinyjs::toggle(id = "element30", condition = input$checkbox3, asis=TRUE)
    })

    shinyjs::hide("ButtonAll_bc_hotspot")
    shinyjs::hide(id="venn_hot1")
    shinyjs::show(id="venn_hot2")

    #### make Table column
    ## Table header with internal link to methodology
    thead<-tr<-th<-NULL
    sketch2 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=3,""),
          th(colspan=length(breed.select.bc),"Genetic Map")

        ),
        tr(
         th(colspan=3,""),
         lapply(colspan=1,breed.select.bc, th)
        ),
        tr(
          th(colspan=1,"Marker"),
          th(colspan=1,"Chr"),
          th(colspan=1,"Position (bp)"),
          lapply(colspan=1, rep("Position (cM)", length(breed.select.bc)), th)
        )
      )
    ))

   #########
    shiny::observeEvent(input$threshold,
    {
          req(input$threshold)

          dat=list();  data12=list(); data123=list()
          data12.1=list(); legend1=c()
          venn.data.all=list()
          venn.data.chr=list()

          names=names.bc.venn

          for(i in 1:length(breed.select.bc)){

            load(system.file("extdata",paste0(breed.select.bc[i],"/adjacentRecRate.Rdata"),package="CLARITY"))
            dat[[i]]=transformdata_hotspot(data.trans=adjacentRecRate,value=input$threshold,color1=color.shape.def[i,1:2],shape1=color.shape.def[i,3:4],ord=color.shape.def[i,5:6])

            if(i==1)legend1=rbind(c(unlist(paste0("No hotspot ",breed.select.bc[i])),unlist(color.shape.def[i,c(2,4)]),unlist(color.shape.def[i,5])),c(unlist(paste0("Hotspot ",breed.select.bc[i])),unlist(color.shape.def[i,c(1,3)]),unlist(color.shape.def[i,6])))
            else legend1=rbind(legend1,c(unlist(paste0("No hotspot ",breed.select.bc[i])),unlist(color.shape.def[i,c(2,4)]),unlist(color.shape.def[i,5])),c(unlist(paste0("Hotspot ",breed.select.bc[i])),unlist(color.shape.def[i,c(1,3)]),unlist(color.shape.def[i,6])))

            data12[[i]]=dat[[i]][which(dat[[i]]$ord==sort(unique(dat[[i]]$ord))[1]),c(2,1,4,3,7)] ##snp, chr, bp, cm, ord
            data12[[i]]=data12[[i]][which(data12[[i]]$ord<100),1:4] ## selecting only hotspot markers

            venn.data.all[[names[i]]]=data12[[i]]$SNP  ##

            if(filter!="All")
            {
              data12.1[[i]]=data12[[i]][which(data12[[i]]$Chr==filter),]
              venn.data.chr[[names[i]]]=data12.1[[i]]$SNP
            }
          }
          colnames(legend1)<-c("label","breaks","shape","ord")
          legend1=as.data.frame(legend1)


          ranges <- shiny::reactiveValues(x = NULL, y = NULL)

          if(filter=="All")data12.1=data12

          ## make check that not an empty set is plotted - than also no venn diagram has to be shown
              use1=4; use2=5
              for(ik in 1:(length(breed.select.bc)-1))
              {
                use1=use1+1
                use2=use2+1

                if(ik ==1)
                {
                  if(rlang::is_empty(data12.1[[1]])==TRUE && rlang::is_empty(data12.1[[2]])==TRUE)data12.2=c()
                  if(rlang::is_empty(data12.1[[1]])==TRUE && rlang::is_empty(data12.1[[2]])==FALSE){
                    data12.2=data12.1[[2]]
                    use1=use1-1
                    use2=use2-1
                  }
                  if(rlang::is_empty(data12.1[[2]])==FALSE && rlang::is_empty(data12.1[[2]])==TRUE){
                    data12.2=data12.1[[1]]
                    use1=use1-1
                    use2=use2-1
                  }
                  if(rlang::is_empty(data12.1[[1]])==FALSE && rlang::is_empty(data12.1[[2]])==FALSE)
                  {
                    data12.2=merge(as.data.frame(data12.1[[1]]),as.data.frame(data12.1[[2]]),by.x=1,by.y=1,all.x=TRUE,all.y=TRUE)
                    data12.2[which(is.na(data12.2[,6])==F),3]=data12.2[which(is.na(data12.2[,6])==F),6]
                    data12.2[which(is.na(data12.2[,5])==F),2]=data12.2[which(is.na(data12.2[,5])==F),5]
                    data12.2=data12.2[,-c(use1,use2)]
                  }
                }
                else
                {
                  if(rlang::is_empty(data12.2)==TRUE && rlang::is_empty(data12.1[[ik+1]])==TRUE)data12.2=c()
                  if(rlang::is_empty(data12.2)==TRUE && rlang::is_empty(data12.1[[ik+1]])==FALSE){
                    data12.2=c()
                    use1=use1-1
                    use2=use2-1
                  }
                  else{
                  data12.2=merge(data12.2,as.data.frame(data12.1[[ik+1]]),by.x=1,by.y=1,all.x=TRUE,all.y=TRUE)
                  data12.2[which(is.na(data12.2[,use2])==F),3]=data12.2[which(is.na(data12.2[,use2])==F),use2]
                  data12.2[which(is.na(data12.2[,use1])==F),2]=data12.2[which(is.na(data12.2[,use1])==F),use1]
                  data12.2=data12.2[,-c(use1,use2)]
                  }
                }
              }
              len=dim(data12.2)[1]

          if(len>=50)
          {
            ll1=c(10, 25,50,-1)
            ll2=c(as.character(c(ll1[1:3])),"All")
          }
          if(len<50 && len>25)
          {
            ll1=c(10,25,len)
            ll2=c(as.character(c(ll1[1:2])),"All")
          }
          if(len<=25 && len>10 )
          {
            ll1=c(10,len)
            ll2=c(as.character(ll1[1]),"All")
          }
          if(len<=10)
          {
            ll1=c(len)
            ll2="All"
          }

          if(filter=="All"){
            output$tablehotspot= DT::renderDataTable({
            output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold)
             DT::datatable(data12.2,  container=sketch2,  extensions = c("Buttons"), rownames=FALSE ,options = list(searching=FALSE,dom='Bfrtip',
                                                                                columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                                dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                                pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
            },server=FALSE)
          }

          if(filter!="All")
          {
            output$tablehotspot=DT::renderDataTable({
              output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold)
              DT::datatable(data12.2, container=sketch2, filter="none" , options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                  pagelength = 10, lengthMenu = list(ll1, ll2)),escape=FALSE,rownames=FALSE) #
            },server=FALSE)
          }


              ### graphical output
              for(i in 1:length(breed.select.bc))
              {
                dat.0=dat[[i]]
                dat.0=cbind(dat.0,rep(breed.select.bc[i],nrow(dat.0)))
                if(i==1)dat0=dat.0
                else dat0=rbind(dat0,dat.0)
              }


              scale=200
              dat0$BP=as.numeric(as.character(dat0$BP))/1e+6
              dat0=cbind(dat0,dat0$Chr)
              colnames(dat0)[(ncol(dat0)-1):ncol(dat0)]=c("ChrTheta","Breed")
              dat0$ChrTheta=as.numeric(as.character(dat0$Chr))/scale + as.numeric(as.character(dat0$Theta))

              if(filter!="All")
              {
                dat.0=dat0[dat0$Chr==filter,]
                dat0=dat.0[order(as.numeric(dat.0$ord)),]

                pos=match(unique(dat0$coloring),legend1$breaks)

                tt=scatterPlot_hotspot(dat0,fil=filter,ranges,legend1=legend1[pos,])
                InputPlot=shiny::reactive(tt)

                output$plothotspot <- shiny::renderPlot({
                  scatterPlot_hotspot(dat0,fil=filter,ranges,legend1=legend1[pos,]) # needed for brush and hovering,

               })
              }
              else
              {
                dat0=dat0[order(as.numeric(dat0$ord)),]

                pos=match(unique(dat0$coloring),legend1$breaks)

                tt=scatterPlot_hotspot_all(dat0,ranges,scale,legend1=legend1[pos,])
                InputPlot=shiny::reactive(tt)

                output$plothotspot <- shiny::renderPlot({
                  scatterPlot_hotspot_all(dat0,ranges,scale,legend1=legend1[pos,]) ## needed for brush
                })
              }

              shiny::observeEvent(input$plothotspot_dblclick, {
                req(input$plothotspot_dblclick)
                brush <- input$plothotspot_brush
                if (!is.null(brush)) {
                  ranges$x <- c(brush$xmin, brush$xmax)
                  ranges$y <- c(brush$ymin, brush$ymax)
                }
                else {
                  ranges$x <- NULL
                  ranges$y <- NULL
                }
              })

              output$hoverhotspot_info<- shiny::renderUI({
                if(!is.null(input$plothotspot_hover)){
                  hovering(dat1=dat0,hover=input$plothotspot_hover,what=4)
                }
              })

          ### venn diagramm  -- enabling save
          if(filter=="All")venn <- RVenn::Venn(venn.data.all)
          else venn <- RVenn::Venn(venn.data.chr)

          venn_data <-process_venn_data(venn)

          InputPlot5=shiny::reactive(creating_venn(venn_data,breed.bc=breed.select.bc,bc.venn=names.bc.venn))
          output$venn_diagram <- shiny::renderPlot({
            InputPlot5()
          })


          ## select  specific set from venn diagram
          observeEvent(input$plot_click, {
            req(input$plot_click)
            data2=prepare_table_venn(venn.dat=venn_data,venn=venn,click=input$plot_click, table.bc=data12.2)

            output$tablehotspot <-DT::renderDataTable({
              output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold,"_set-",data2[[2]])
                DT::datatable(data2[[1]],container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                                                      options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                     dom = 'Bt', buttons = list('pageLength', 'copy',list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                     pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
            },server=FALSE)
            shinyjs::show("ButtonAll_bc_hotspot")
          })

          ### hier reset to all
          shiny::observeEvent(input$ButtonAll_bc_hotspot,{
            output.name.tab=paste0(names.files,"_hotspot_BTA-",filter,"_",input$threshold)
            output$tablehotspot <-DT::renderDataTable({
              DT::datatable(data12.2,container=sketch2, extensions = c("Buttons"),  rownames=FALSE,
                                                      options = list(searching=FALSE,dom='Bfrtip',columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                                                     dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title= output.name.tab),list(extend='excel',title= output.name.tab)),
                                                                     pagelength = 10, lengthMenu = list(c(5,10,15, -1), c(5,10,15,'All')) ))
            },server=FALSE)
            shinyjs::hide("ButtonAll_bc_hotspot")
          })


          ## download Plot
          output$downloadHotspot <- shiny::downloadHandler(
            filename = paste0(names.files,"_hotspots_BTA-",filter,"_", input$threshold,".png") ,
            content = function(file){
              ggplot2::ggsave(file, plot = InputPlot(), device = "png",width=20,height=10,units="in",dpi=300)
            }
          )

          #oVenn
          output$downloadVenn <- shiny::downloadHandler(
            filename = paste0(names.files,"_Venn_hotspots_BTA-",filter,"_", input$threshold,".png") ,
            content = function(file){
              ggplot2::ggsave(file, plot =  InputPlot5(), device = "png",width=6,height=6,units="in",dpi=300)
            }
          )

    }) ## end observe sliderInput threshhold

    shiny::observeEvent(input$venn_hotspot1,{
      shinyjs::hide(id="venn_hot1")
      shinyjs::show(id="venn_hot2")
    })

    shiny::observeEvent(input$venn_hotspot2,{
      shinyjs::show(id="venn_hot1")
      shinyjs::hide(id="venn_hot2")
    })



  })
}

## To be copied in the UI
# mod_bc_hotspot_ui("bc_hotspot_1")

## To be copied in the server
# mod_bc_hotspot_server("bc_hotspot_1")
