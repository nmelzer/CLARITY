#' The application User-Interface
#'
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @import shinydashboard
#' @rawNamespace import(shinyjs, except = runExample)
#' @import shinycssloaders
#' @import roxygen2
#' @import htmltools
#'
#' @export



## main ui function
app_ui <- function(request){

  #### general settings for layout
  css <- "
  .sidebar-menu (:last-child) {
    margin-bottom: 3000px;
  }"

  convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
      mi$attribs$class=NULL
    }
    mi
  }

  ## here extend when more breeds available
  breeds=c("Holstein")

  #####
  no.chr=c("","All",paste(1:29))





 # shiny::shinyOptions(cache = cachem::cache_disk("./app_cache/cache/")) ## not necessary - Marker plots to huge - resulting in trouble with the app (the app freezes)

  ## definition header of the dashboard
  dbHeader <- shinydashboard::dashboardHeader(title = "CLARITY",
                              tags$li(tags$a(href = 'https://www.fbn-dummerstorf.de/',
                                        tags$img(src = 'www/FBN_logo.jpg',width=50,height=50), target="blank",
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                        class = "dropdown"),


                              ## the following is necessary to obtain also on the left-side the blue box more length and this has also changed within the sidebar
                              tags$li(class = "dropdown",
                                      tags$style(".main-header {max-height: 70px}"),
                                      tags$style(".main-header .logo {height: 70px;}"),
                                      tags$style(".sidebar-toggle {height: 70px; padding-top: 1px !important;}"),
                                      tags$style(".navbar {min-height:70px !important}")
                              )
  )

  ## definition sidebar of the dashboard
  dbSidebar<-shinydashboard::dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 90px}"), ## change header box more space is needed
       shinydashboard::sidebarMenu(
        id="tabs",
        convertMenuItem(shinydashboard::menuItem("Information",tabName="info", startExpanded=TRUE,
                                 br(),
                                 "General",
                          shinydashboard:: menuSubItem("About the project",tabName="about"),
                          shinydashboard::menuSubItem("Contact",tabName="contact"),
                           "Resources",
                          shinydashboard::menuSubItem("Information about the data sets",tabName="infodatasets"),
                          shinydashboard::menuSubItem("Methodology",tabName="methodology")

                          ),"info"),

        tags$hr(),
        convertMenuItem(shinydashboard::menuItem("Results",tabName="single",startExpanded=FALSE,  ### Extension for other breeds
                          selectInput(inputId = 'breed', label = 'Breed:', choices= breeds),
                          selectInput(inputId = 'chromosome',label = "Chromosome",choices=no.chr)
                          ),"single")
      ),
    shinydashboard::sidebarMenuOutput("menu")
  )

  ## definition body of the dashboard
  dbBody<- shinydashboard::dashboardBody(
    #####################################################################################
    #### change the background
    tags$head(
      tags$style(HTML(css)),
      tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: #F0FFFF;}'))),
      tags$style(type='text/css',".nav-tabs {font-size: 18px} "), ## changing the font size of the navbar
     ### tags$script("www/js"="func.js")## new may take out again 28.03
    ),

    #####################################################################################
    #### enabling links between different pages or tabs in the app  - code was adopted from https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard/37170333
     tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    #####################################################################################
    #### define colors for the boxes
    tags$style(HTML("
                  .box.box-solid.box-primary>.box-header {
                    color:white;
                    background: E87722 #darkolivegreen
                    }
                    .box.box-solid.box-primary{
                    border-bottom-color:E87722#Lavender;
                    border-left-color: E87722#Lavender;
                    border-right-color: E87722;#Lavender;
                    border-top-color: E87722#Lavender; #darkolivegreen;
                    background: withe #Ivory
                    }
                    ")),

    tags$style(HTML("
                  .box.box-solid.box-danger>.box-header {
                    color:black;
                    background:lightskyblue
                    }
                    .box.box-solid.box-danger{
                    border-bottom-color:lightskyblue;
                    border-left-color:lightskyblue;
                    border-right-color:lightskyblue;
                    border-top-color: lightskyblue;
                    background:white
                    }
                    ")),

    ##### box without frame and no color
    tags$style(HTML("
                  .box.box-info>.box-header {
                  border: 0px solid #3DA0D1
                    }
                    .box.box-info{
                     border: 0px solid #3DA0D1;
                      shadow: none;
                      -webkit-box-shadow: none;
                      -moz-box-shadow: none
                    }
                    ")),

    #####################################################################################
    #### change color of +/- by the boxes
    tags$style(
      type = 'text/css',
      '.fa.fa-minus{
      font-weight: 900;
      color: black;
    }'
    ),
    tags$style(
      type = 'text/css',
      '.fa.fa-plus{
      font-weight: 900;
      color: black;
     }'
    ),

    #####################################################################################
    #### website elements
   shinydashboard::tabItems(
      shinydashboard::tabItem(tabName="info",br(),tags$b(tags$h2("Welcome to the CLARITY app for an interactive exploration of breed-specific genetic maps in cattle")), br(),br(),
              h4("Currently Holstein data can be investigated."),br(),
              tags$img(src = 'www/Cow.png',width="50%",height="40%")
      ),
      shinydashboard::tabItem(tabName="about",
              h3("About the project"),
              mod_about_the_project_ui("mod_about_the_project_1")
      ),
      shinydashboard::tabItem(tabName="infodatasets", ## if further data sets available than extend here
              h3("Information about the data set"),
              mod_datasets_ui("mod_datasets_1")

      ),
      shinydashboard::tabItem(tabName="contact",
              h3("Contact"),
              mod_contact_ui("mod_contact_1")

      ),
      shinydashboard::tabItem(tabName = "methodology",
              h3("Methodology"),
              mod_methodology_ui("mod_methodology_1")
      ),
      shinydashboard::tabItem(
        tabName="single",
        h3("Breed Analysis"),
        useShinyjs(),
        shiny::tabsetPanel(id="navbar",
                    shiny::tabPanel(title="General information", conditionalPanel("input$mapversion"!="", mod_general_ui("mod_general_1"))
                    ),
                    shiny::tabPanel(title="Genetic map", conditionalPanel("input$mapversion"!="", mod_genetic_map_ui("mod_genetic_map_1"))
                    ),
                    shiny::tabPanel(title="Misplaced markers", verbatimTextOutput("summary"), mod_misplaced_ui("mod_misplaced_1")
                    ),
                    shiny::tabPanel(title="Hotspot detection",conditionalPanel("input$mapversion"!="",mod_hotspot_ui("mod_hotspot_1"))
                    ),
                   shiny::tabPanel(title="Genetic-map functions",conditionalPanel("input$mapversion"!="", mod_genetic_function_ui("mod_genetic_function_1")))
        )
      )
      ### here it can be implemented the extension for breed comparison
      #  tabItem(
      #   tabName="breedcomparison", h2("not yet implemented")
      #  )

   )#end of tag list
  )

  ## Define UI
  shinyUI(
    tagList( ### necessary for the golem package: otherwise error during package testing: Failed tests (test-golem-recommended.R:3:3): app ui `ui` has class 'shiny.tag', not class 'shiny.tag.list'
              ## https://github.com/ColinFay/golemize/blob/master/shiny-dashboard/shiny-dashboard-golem/R/app_ui.R checked example

    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(
      dbHeader,
      dbSidebar,
      dbBody
    )
  )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
   'extdata', app_sys('extdata')
  )


  add_resource_path(
    'figures', app_sys('figures')
  )

 add_resource_path(
   'www', app_sys('app/www')
  )


#  addResourcePath( 'www' )

  tags$head(
    bundle_resources(
      path = app_sys('app/www','app/extdata'),
      app_title = 'CLARITY'
    ),
    tags$title("CLARITY"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
