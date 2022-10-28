#' The application User-Interface
#'
#' @title App ui
#' @description The application User-Interface (UI).
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @import shinydashboard
#' @rawNamespace import(shinyjs, except = runExample)
#' @import htmltools
#' @import metathis
#' @export

## general comment crawling at the moment blocked from shinyapps.io side last 2022 from their site


## main ui function
app_ui <- function(request){

  ################################################  general settings
  convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
      mi$attribs$class=NULL
    }
    mi
  }
 ##

  #####
  no.chr=c("","All",paste(1:29))
  breeds=c("", "Holstein","Fleckvieh")#,"BrownSwiss")

  ## options scipen added 13.06.2022
  options(scipen=999)


  ## definition header of the dashboard
  dbHeader <- shinydashboard::dashboardHeader(title = "CLARITY",
                                              htmltools::tags$li(htmltools::tags$a(href = 'https://www.fbn-dummerstorf.de/',
                                                                 htmltools::tags$img(src = 'www/FBN_logo.jpg',width=50,height=50, alt="FBN"), target="blank", ## added alt to get discernible name for lighthouse
                                                                 style = "padding-top:10px; padding-bottom:10px;"),
                                                                 class = "dropdown"),


                                              ## the following is necessary to obtain also on the left-side the blue box more length and this has also changed within the sidebar
                                              htmltools::tags$li(class = "dropdown",
                                                                 htmltools::tags$style(".main-header {max-height: 70px}"),
                                                                 htmltools::tags$style(".main-header .logo {height: 70px;}"),
                                                                 htmltools::tags$style(".sidebar-toggle {height: 70px; padding-top: 1px !important;}"),
                                                                 htmltools::tags$style(".navbar {min-height:70px !important}")
                                              )
  )

  ## definition sidebar of the dashboard
  dbSidebar<-shinydashboard::dashboardSidebar(
      tags$style(".left-side, .main-sidebar {float:top;padding-top:100px}"), ## change header box more space is needed  ## 70px   ".main-sidebar {float:top; padding-left:15px; padding-right:15px}"
       shinydashboard::sidebarMenu(
        id="tabs",
        convertMenuItem(shinydashboard::menuItem("Information",tabName="info", startExpanded=TRUE,
                                 "General",
                          shinydashboard::menuSubItem("About the project",tabName="about"),
                          shinydashboard::menuSubItem("Contact",tabName="contact"),
                           "Resources",
                          shinydashboard::menuSubItem(HTML("Information about <br> &nbsp; &emsp; the data sets"),tabName="infodatasets"), #
                          shinydashboard::menuSubItem("Misplaced markers",tabName="misplaced_markers"),
                          shinydashboard::menuSubItem("Methodology",tabName="methodology")
                          ),"info"),
        convertMenuItem(shinydashboard::menuItem("Breed analysis",tabName="single",startExpanded=FALSE,  ### Extension for other breeds
                          shiny::selectInput(inputId = 'breed', label = 'Breed',choices= breeds),
                          shiny::selectInput(inputId = 'chromosome',label = "Chromosome",choices="")## changed on 25.07.2022 that user cannot select any chromosome without
                          ),"single"),
        convertMenuItem(shinydashboard::menuItem("Breed comparison",tabName="breedcomparison",startExpanded=FALSE,  ### Extension for other breeds
                                                 shiny::selectInput(inputId = 'breed1', label = 'Select breeds', choices=breeds ,multiple=TRUE,selectize=TRUE),#
                                                 shiny::selectInput(inputId = 'chromosome1',label = "Chromosome",choices="") ## changed on 25.07.2022 that user cannot select any chromosome without
        ),"breedcomparison")
      ),
    shinydashboard::sidebarMenuOutput("menu")
  )

  ## definition body of the dashboard
  dbBody<- shinydashboard::dashboardBody(
    ###################################

    htmltools::tags$html(lang="en"), ## necessary to fulfill W3C
    #####################################################################################
    #### change the background
    htmltools::tags$head(
     # tags$style(HTML(css)),
       tags$style(type='text/css',".nav-tabs {font-size: 18px} "), ## changing the font size of the navbar
       tags$meta(name="author", content="Nina Melzer"),
       tags$meta(name="creation_date", content="27/10/2022"),
       tags$meta(name="url", content="https://nmelzer.shinyapps.io/clarity"),
    ),

    metathis::meta()%>%
      metathis::meta_general(
        application_name = "CLARITY",
        description = "CLARITY app for an interactive exploration of breed-specific genetic maps in cattle",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "An R project",
      ),


    htmltools::tags$head( htmltools::tags$style( htmltools::HTML('body */.content-wrapper, .right-side {background-color:#F0F6FB;},#     ##F0FFFF  #EEEEEE  #FFFFFF   ## #wewqeqwe /* body */.content-wrapper,
    .content-wrapper {float:top;}
    .wrapper {overflow-y: hidden;}
    .main-header {
      max-height: 200px !important;
    }
    .main-header .logo {
      line-height: 85px !important;
      padding: 0px 0px;
    }'))),

    #####################################################################################
    #### enabling links between different pages or tabs in the app  - code was adopted from https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard/37170333
    htmltools::tags$script(htmltools::HTML("
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

    htmltools::tags$style(htmltools::HTML("
                            .box.box-solid.box-primary>.box-header {
                              color:white;
                              background: E87722 #darkolivegreen
                            }
                            .box.box-solid.box-primary{
                              border-bottom-color:E87722#Lavender;
                              border-left-color: E87722#Lavender;
                              border-right-color: E87722;#Lavender;
                              border-top-color: E87722#Lavender;
                              background: withe #Ivory
                            }
                      ")),

    htmltools::tags$style(htmltools::HTML("
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
    htmltools::tags$style(htmltools::HTML("
                            .box.box-info>.box-header {
                              border: 0px solid #3DA0D1
                            }
                            .box.box-info{
                              border: 0px solid #3DA0D1;
                              shadow: none
                            }
                    ")),
    #####################################################################################
    #### change color of +/- by the boxes
    htmltools::tags$style(
      type = 'text/css',
      '.fa.fa-minus{
      font-weight: 900;
      color: black;
    }'
    ),
    htmltools::tags$style(
      type = 'text/css',
      '.fa.fa-plus{
      font-weight: 900;
      color: black;
     }'
    ),


  ### add space between menuItem sidebar
  tags$head(tags$style(".sidebar-menu li { margin-bottom: 30px; }")),

  ################ change sidebar - font size and color
  htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML(".main-sidebar { font-size: 17px; }
                 .treeview-menu>li>a { font-size: 15px!important; }
                 ")), #change the font size to 20
  ),

  htmltools::tags$head(htmltools::tags$style(htmltools::HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #073763; ## darkblue
                              color:#ffffff;
                              font-size:22;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #073763; ##darkblue
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #CFE2F3;## blue
        }


        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #000000; ## ## black
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #073763; ## darkblue
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #073763;  ## darkblue
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #68838B; ##a64d79
                              color: #ffffff;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #68838B;
                              color: #ffffff;
                              }
                              '))),


   ##### test changing tabset for breed analysis and breed comparison
  htmltools::tags$style(htmltools::HTML("
                          .tabbable > .nav > li > a {background-color: #CFE2F3;  color:black}
                          .tabbable > .nav > li > a[data-value='Genetic map'] {background-color: #CFE2F3;   color:black}
                          .tabbable > .nav > li > a[data-value='Hotspot detection'] {background-color: #CFE2F3;  color:black}
                          .tabbable > .nav > li > a[data-value='Genetic-map functions'] {background-color: #CFE2F3; color:black}
                          .tabbable > .nav > li[class=active]    > a {background-color: #073763; color:white}
   ")),

  ####################################################################################### test automatic window size
  #####################################################################################
    #### website elements - main panel
   shinydashboard::tabItems(
      shinydashboard::tabItem(tabName="info",
              shiny::fluidRow( shiny::column(width=12,""),
                               shiny::column(width=12,tags$b(tags$h3("Welcome to the CLARITY app for an interactive exploration of breed-specific genetic maps in cattle")), style="margin-top:1%; text-align:left;")),
              htmltools::br(),
              htmltools::br(),
              shiny::fluidRow(
                shiny::column(2, tags$img(src = 'www/Holstein.jpg',alt="Cow_picture_Holstein",width="100%", height="100%")),
                shiny::column(2),
                shiny::column(2, tags$img(src = 'www/Fleckvieh.jpg',alt="Cow_picture_Fleckvieh",width="100%", height="100%"))),
                shiny::fluidRow(
                shiny::column(2,tags$h3("Holstein")),column(2),column(2,tags$h3("Fleckvieh")))
      ),
      shinydashboard::tabItem(tabName="about",
                              htmltools::h3("About the project"),
                              mod_about_the_project_ui("mod_about_the_project_1")
      ),
      shinydashboard::tabItem(tabName="infodatasets", ## if further data sets available than extend here
                              htmltools::h3("Information about the data sets"),
                              mod_datasets_ui("mod_datasets_1")
      ),
      shinydashboard::tabItem(tabName="misplaced_markers",
                              htmltools::h3("Misplaced markers"),
                              mod_misplaced_ui("mod_misplaced_1")
      ),
      shinydashboard::tabItem(tabName="contact",
                              htmltools::h3("Contact"),
                              mod_contact_ui("mod_contact_1")
      ),
      shinydashboard::tabItem(tabName = "methodology",
                              htmltools::h3("Methodology"),
                              mod_methodology_ui("mod_methodology_1")
      ),
      ## Breed analysis
    shinydashboard::tabItem(
        tabName="single",
        htmltools::h3("Breed analysis"),
        shinyjs::useShinyjs(),
        shiny::tabsetPanel(id="navbar",
                    shiny::tabPanel(title="General information",  mod_general_ui("mod_general_1")
                    ),
                    shiny::tabPanel(title="Genetic map", mod_genetic_map_ui("mod_genetic_map_1")
                    ),
                    shiny::tabPanel(title="Hotspot detection",mod_hotspot_ui("mod_hotspot_1")
                    ),
                    shiny::tabPanel(title="Genetic-map functions",mod_genetic_function_ui("mod_genetic_function_1"))
        )
      ),
      #breed comparison
    shinydashboard::tabItem(
         tabName="breedcomparison",
         htmltools::h3("Breed comparison"),
         shinyjs::useShinyjs(),
         shiny::tabsetPanel(id="navbar2",
          shiny::tabPanel(title="General information",mod_bc_general_ui("bc_general_1")
          ),
          shiny::tabPanel(title="Genetic map",mod_bc_genetic_map_ui("bc_genetic_map_1")
          ),
         shiny::tabPanel(title="Hotspot detection",mod_bc_hotspot_ui("bc_hotspot_1")
          ),
         shiny::tabPanel(title="Genetic-map functions",mod_bc_genetic_function_ui("bc_genetic_function_1")
         )
        )
      )
   )#end of tag list
  )

  ## Define UI
  shiny::shinyUI(
    htmltools::tagList( ### necessary for the golem package: otherwise error during package testing: Failed tests (test-golem-recommended.R:3:3): app ui `ui` has class 'shiny.tag', not class 'shiny.tag.list'
              ## https://github.com/ColinFay/golemize/blob/master/shiny-dashboard/shiny-dashboard-golem/R/app_ui.R checked example

    # Leave this function for adding external resources
     golem_add_external_resources(),
     shinydashboard::dashboardPage(
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

  golem::add_resource_path(
   'extdata', app_sys('extdata')
  )


  golem::add_resource_path(
    'figures', app_sys('figures')
  )

 golem::add_resource_path(
   'www', app_sys('app/www')
  )


  htmltools::tags$head(
    bundle_resources(
      path = app_sys('app/www','app/extdata'),
      app_title = 'CLARITY'
    ),
    htmltools::tags$title("CLARITY"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
