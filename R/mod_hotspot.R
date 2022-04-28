# Module UI

#' @title mod_hotspot_ui and mod_hotspot_server
#' @description  A shiny Module to generate the outcome for the tabpanel Hotspot detection
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_hotspot
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import shinycssloaders
#'
#' @note The server used functions which are included in mod-hotspot_fct_functions as well as the general fct_general_hovering function.
mod_hotspot_ui=function(id)
{
  ns=NS(id)#
  tagList(
   fluidRow(
    column(width=12,h2("Genome-wide landscape of putative recombination",tags$a(href="#","hotspot", onclick = "openTab('methodology')")," intervals.")),
    br(),
    br(),
    box(width=10, title=tags$b("Recombination hotspots"),status="danger",solidHeader = TRUE,collapsible = TRUE,
      h5("Interactive graphic: brush and double click to zoom-in specific regions. Use double click to return."),
      br(),
      plotOutput(ns("plothotspot"),height="800px",width="100%", dblclick = ns("plothotspot_dblclick"),brush = brushOpts(id = ns("plothotspot_brush"), resetOnNew = TRUE),
      hover = hoverOpts(ns("plothotspot_hover"), delay = 100, delayType = "debounce")) %>% withSpinner(color="#0dc5c1"),uiOutput(ns("hoverhotspot_info"))
    ),
    column(width=2,downloadButton(outputId=ns("download555")), div(style="margin-bottom:60px"), sliderInput(ns("treshhold"),"Threshold to define hotspot region (standard deviation of recombination rate)",min = 0, max = 10, value = 2.5, step = 0.1),div(style="margin-bottom:60px"))
   ),
   br(),
   fluidRow(
    box(title = tags$b("Markers in hotspot intervals depending on threshold") ,status="danger",width=10,
      solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
      column(width=10,DT::dataTableOutput(outputId=ns("tablehotspot")),style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
        fluidRow(column(10,checkboxInput(ns("checkbox3"), "Show/hide legend", TRUE), ## tried to make the HTML as htmlOutput but it does not work - must be checked later for a better solution
          p(id = "element3",HTML("Chr: chromosome<br>SNP: SNP name<br>cM: chromosome position in centiMorgan based on <a href='#' onclick = openTab('methodology') >deterministic approach</a>  <br>BP: chromosome position in base pairs<br>
                                        Theta: recombination rate<br>
                                        Dis: distance to the next marker")))
        )
      )
    )
  )
}


# Module Server
#' @rdname mod_hotspot
#' @export
#' @keywords internal
mod_hotspot_server=function(input, output, session, val, filter)
{
  adjacentRecRate<-NULL
  ## show / hide legend
  observe({
    toggle(id = "element3", condition = input$checkbox3,asis=TRUE)
  })

  observeEvent(input$treshhold,
  {
   load(system.file("extdata","adjacentRecRate.Rdata",package="CLARITY"))

   value=input$treshhold

   ranges <- reactiveValues(x = NULL, y = NULL)


   dat=transformdata1(data.trans=adjacentRecRate,value=value)
   data12=dat[which(dat$cols==1),c(1:6)]
   data12$Chr=as.character(data12$Chr)

   len=length(which(dat$cols==1))

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


   if(filter=="All"){
      output$tablehotspot= DT::renderDataTable(
         data12, extensions = c("Buttons"), rownames=FALSE ,options = list(searching=FALSE,dom='Bfrtip',
                                    columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                    dom = 'Bt', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf'),
                                    pagelength = 10, lengthMenu = list(c(10,20,30, -1), c('10', '20','30','All')))
     )
   }

   if(filter!="All")
   {
     output$tablehotspot=DT::renderDataTable({
       DT::datatable(data12.1,filter="none" , options=list(searching = FALSE,dom='Bfrtip',buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf'),
                      pagelength = 10, lengthMenu = list(ll1, ll2)),escape=FALSE,rownames=FALSE) # table without search funtion dom='t'realized and paging false enable that only scroll is possible
     })
   }

   ## some preparation for plot and hovering
   scale=200
   dat$BP=as.numeric(as.character(dat$BP))/1e+6
   dat=cbind(dat,dat$Chr)
   colnames(dat)[dim(dat)[2]]="ChrTheta"
   dat$ChrTheta=as.numeric(as.character(dat$Chr))/scale + as.numeric(as.character(dat$Theta))


   if(filter!="All")
   {
     dat=dat[dat$Chr==filter,]
     tt=scatterPlot2(dat,fil=filter,ranges,scale)
     InputPlot=reactive(tt)

     output$plothotspot <- shiny::renderPlot({
       scatterPlot2(dat,fil=filter,ranges,scale) # needed for brush and hovering,

     })
   }

   if(filter=="All")
   {
     tt=scatterPlot1(dat,ranges,scale)
     InputPlot=reactive(tt)

     output$plothotspot <- renderPlot({
       scatterPlot1(dat,ranges,scale) ## needed for brush
     })
   }

  observeEvent(input$plothotspot_dblclick, {
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

   output$hoverhotspot_info<- renderUI({
        if(!is.null(input$plothotspot_hover)){
       hovering(dat1=dat,hover=input$plothotspot_hover,what=2)
        }
     })

   ## download Plot
 output$download555 <- downloadHandler(
    filename = paste0("Detected_hotspots_BTA-",filter,"_", value,".png") ,
    if(filter=="All")filename = paste0("Detected_hotspots_All_BTA_",value,".png"), #function() { paste(input$dataset, '.png', sep='') },
     content = function(file) {
      ggsave(file, plot = InputPlot(), device = "png",width=20,height=10,units="in",dpi=300)
    }
  )
  })
}

## To be copied in the UI
# mod_hotspot_ui("mod_hotspot_1")

## To be copied in the server
# callModule(mod_hotspot_server,"mod_hotspot_1")

