# Module UI

#' @title mod_hotspot_ui and mod_hotspot_server
#' @description A shiny module to generate the outcome for the tabpanel "General information" for the sidebar "Breed analysis".
#'
#' @details Within the module the corresponding data are prepared using the function \link{transformdata_hotspot}. The prepared data are used to plot the hotspot figure using the function
#' \link{scatterPlot_hotspot} or \link{scatterPlot_hotspot_all} depending on user chromosome choice. The hover information are obtained from the function \link{hovering}.
#'
#' @param id module id
#'
#' @rdname mod_hotspot
#'
#' @keywords internal, hotspot
#' @export
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @import htmltools
#'
#' @seealso \link{transformdata_hotspot}, \link{scatterPlot_hotspot}, \link{scatterPlot_hotspot_all} and \link{hovering}

mod_hotspot_ui=function(id)
{
  ns=shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')",style='text-decoration-line: underline;')," intervals.")),
      htmltools::br(),htmltools::br(),
      shinydashboard::box(width=12, title=tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
        h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return."),
        shiny::column(12,shiny::downloadButton(outputId=ns("downloadHotspot"),"Save plot",class="butt1")),
        shiny::column(12,shiny::plotOutput(ns("plothotspot"), height="750px", dblclick = ns("plothotspot_dblclick"),brush = brushOpts(id = ns("plothotspot_brush"), resetOnNew = TRUE),
        hover = hoverOpts(ns("plothotspot_hover"), delay = 400, delayType = "throttle",clip=TRUE)) %>% shinycssloaders::withSpinner(color="#0dc5c1"),shiny::uiOutput(ns("hoverhotspot_info")),style="width: calc(100% - 100px); !important;")
      )
   ),
   htmltools::br(),
   shiny::fluidRow(
     shinydashboard::box(title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",width=12,
      solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
      shiny::fluidRow(
        shiny::column(width=12, "Use the slider to define the threshold to hotspot region (standard deviation of recombination rate). This accordingly changes the figure and table. The default treshold is 2.5."),
        shiny::column(width=4, div(style="margin-bottom:30px"), sliderInput(ns("threshold"),"Threshold to define hotspot region",min = 0, max = 10, value = 2.5, step = 0.1),div(style="margin-bottom:60px")),column(width=8,"")
      ),
      htmltools::hr(style = "border-top: 1px solid #68838B;"),
      shiny::fluidRow(
        shiny::column(width=10,DT::dataTableOutput(outputId=ns("tablehotspot")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
        shiny::column(width=10,shiny::checkboxInput(ns("checkbox3"), "Show/hide legend", TRUE), ##
                      htmltools::p(id = "element3",HTML("Chr: chromosome<br>SNP: SNP name<br>cM: chromosome position in centiMorgan based on <a href='#' onclick = openTab('methodology')><u>deterministic approach</u></a>  <br>BP: chromosome position in base pairs<br>
                                        Theta: recombination rate<br>
                                        Dis: distance to preceeding marker")))
        )
      )
    )
  )
}


# Module Server
#' @rdname mod_hotspot
#' @param filter character contains the selected chromosome
#' @param breed.select character contains the name of the selected breed
#' @param color.shape.def data frame containing the definition for coloring, shapes, size for plots
#'
#' @keywords internal
#' @export
mod_hotspot_server=function(id, filter,breed.select,color.shape.def){
   shiny::moduleServer(id, function(input, output, session){

      ns <- session$ns

      adjacentRecRate<-NULL
      ## show / hide legend
      shiny::observe({
         shinyjs::toggle(id = "element3", condition = input$checkbox3, asis=TRUE)
      })


      shiny::observeEvent(input$threshold,{
         ranges <- shiny::reactiveValues(x = NULL, y = NULL)

         load(system.file("extdata",paste0(breed.select,"/adjacentRecRate.Rdata"),package="CLARITY"))
         if(length(which(is.na(adjacentRecRate$cM)==T))!=0)adjacentRecRate=adjacentRecRate[-which(is.na(adjacentRecRate$cM)==T),]
         if(class(adjacentRecRate$BP)=="numeric"){
           adjacentRecRate$BP=as.integer(adjacentRecRate$BP)
           adjacentRecRate$Dis=as.integer(adjacentRecRate$Dis)
         }

         dat=transformdata_hotspot(data.trans=as.data.frame(adjacentRecRate),value=input$threshold,color1=color.shape.def[1,1:2],shape1=color.shape.def[1,3:4],ord=color.shape.def[1,5:6])
         data12=dat[dat$ord%in%1,c(1:6)] ## selecting only hotspot markers
         data12$Chr=as.character(data12$Chr)


         len=length(dat$cols%in%1) ## may can be exclude

          ## part only for the table - Menu option -see L124
         if(filter!="all")
         {
            data12.1=data12[which(data12$Chr==filter),]
            len=dim(data12.1)[1]
         }
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

         if(filter=="All")
         {
            title1=paste0(breed.select,"_hotspot_BTA-all_",input$threshold)
            output$tablehotspot= DT::renderDataTable({
                DT::datatable(data12, extensions = c("Buttons"), rownames=FALSE ,options = list(searching=FALSE,dom='Bfrtip',
                                  columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                  dom = 'Bt', buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                                  pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All'))))
             }, server = FALSE)
         }
         else
         {
            title1=paste0(breed.select,"_hotspot_BTA-",filter,"_",input$threshold)
            output$tablehotspot=DT::renderDataTable({
               DT::datatable(data12.1,filter="none" , options=list(searching = FALSE,dom='Bfrtip',buttons = list('pageLength', 'copy', list(extend='csv',title=title1),list(extend='excel',title=title1)),
                      pagelength = 10, lengthMenu = list(ll1, ll2)),escape=FALSE,rownames=FALSE)
            }, server = FALSE)
         }

         ## some preparation for plot and hovering
         scale=200
         dat$BP=as.numeric(as.character(dat$BP))/1e+6
         dat=cbind(dat,dat$Chr)
         colnames(dat)[dim(dat)[2]]="ChrTheta"
         dat$ChrTheta=as.numeric(as.character(dat$Chr))/scale + as.numeric(as.character(dat$Theta))

         legend1=rbind(c(unlist(paste0("No hotspot")),unlist(color.shape.def[1,c(2,4,5)])),c(unlist(paste0("Hotspot")),unlist(color.shape.def[1,c(1,3,6)])))
         legend1=as.data.frame(legend1)
         colnames(legend1)<-c("label","breaks","shape","ord")

         if(filter!="All")
         {
            dat=dat[dat$Chr%in%filter,]
            pos=match(unique(dat$coloring),legend1$breaks)

            tt=scatterPlot_hotspot(dat,fil=filter,ranges,legend1=legend1[pos,])
             InputPlot=shiny::reactive(tt)

            output$plothotspot <- shiny::renderPlot({
              scatterPlot_hotspot(dat,fil=filter,ranges,legend1=legend1[pos,])# n
            })
         }
         else
         {
            pos=match(unique(dat$coloring),legend1$breaks)

            tt=scatterPlot_hotspot_all(dat,ranges,scale,legend1=legend1[pos,])
            InputPlot=shiny::reactive(tt)

            output$plothotspot <- shiny::renderPlot({
              scatterPlot_hotspot_all(dat,ranges,scale,legend1=legend1[pos,]) ## needed for brush
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
               hovering(dat1=dat,hover=input$plothotspot_hover,what=2)
            }
         })

         ## download Plot
         output$downloadHotspot <- shiny::downloadHandler(
            filename = paste0(breed.select,"_hotspots_BTA-",filter,"_", input$threshold,".png") ,
            if(filter=="All")filename = paste0(breed.select,"_hotspots_BTA-all_",input$threshold,".png"),
            content = function(file){
               ggplot2::ggsave(file, plot = InputPlot(), device = "png",width=20,height=10,units="in",dpi=300)
            }
         )
      })
   })
}

## To be copied in the UI
# mod_hotspot_ui("mod_hotspot_1")

## To be copied in the server
# mod_hotspot_server("mod_hotspot_1")

