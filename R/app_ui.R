#' The application User-Interface
#'
#' @title App ui
#' @description The application User-Interface (UI).
#'
#' @param request Internal parameter for '`shiny`'.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @import shinydashboard
#' @rawNamespace import(shinyjs, except = runExample)
#' @import htmltools
#' @import metathis
#' @export

## main ui function
app_ui <- function(request){

  ###################################################################################################################################
  ####  general settings
  convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
      mi$attribs$class=NULL
    }
    mi
  }

  options(scipen=999)

  ###################################################################################################################################
  #### definition header of the dashboard
  dbHeader <- shinydashboard::dashboardHeader(title = "CLARITY",
                                              htmltools::tags$li(htmltools::tags$a(href = 'https://www.fbn-dummerstorf.de/',
                                                                 htmltools::tags$img(src = 'www/FBN_Logo_Bildmarke_RGB_black.png',width=50,height=50, alt="FBN logo"), target="blank",
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

  ###################################################################################################################################
  #### definition sidebar of the dashboard
  dbSidebar<-shinydashboard::dashboardSidebar(
      useShinyjs(),
      #tags$style(".left-side, .main-sidebar {float:top;padding-top:100px}"), ## change header box more space is needed  ## 70px
       shinydashboard::sidebarMenu(
        id="tabs",
        convertMenuItem(shinydashboard::menuItem("Information",tabName="info", startExpanded=TRUE, icon =NULL,
                                                 "General",
                                                 shinydashboard::menuSubItem(HTML("<i class='fas fa-angles-right'></i> About the project"),tabName="about",icon=NULL),
                                                 shinydashboard::menuSubItem(HTML("<i class= 'fas fa-angles-right'></i> Contact &amp; <br>  &nbsp; &emsp; References"),tabName="contact",icon=NULL),
                                                 "Resources",
                                                 shinydashboard::menuSubItem(HTML("<i class='fas fa-angles-right'></i> Information about <br> &nbsp; &emsp; the data sets"),tabName="infodatasets",icon=NULL), # set icon NULL and included as icon in the test - aria - friendly
                                                 shinydashboard::menuSubItem(HTML("<i class='fas fa-angles-right'></i> Misplaced markers"),tabName="misplaced_markers",icon=NULL),
                                                 shinydashboard::menuSubItem(HTML("<i class='fas fa-angles-right'></i> Methodology"),tabName="methodology",icon=NULL)
                            ),"info"),
        convertMenuItem(shinydashboard::menuItem("Breed analysis",tabName="single",startExpanded=FALSE,  ### Extension for other breeds
                           shiny::div(id="breed_dis_able",
                                      shiny::selectInput(inputId = 'breed', label = 'Breed',choices= breeds,selectize=FALSE),
                                      shiny::selectInput(inputId = 'chromosome',label = "Chromosome",choices="",selectize=FALSE),
                                      shiny::checkboxGroupInput(inputId = "approachselection",label="Select approaches",selected=c("Deterministic_male","Likelihood_male"),choices=approaches),
                                      shiny::actionButton(inputId="run_single",label="Run selected approaches")
                            )),"single"),

        shinyjs::hidden(shiny::htmlOutput(outputId="notworking",inline=TRUE)),
        convertMenuItem(shinydashboard::menuItem("Breed comparison",tabName="breedcomparison",startExpanded=FALSE,  ### Extension for other breeds
                           shiny::div(id="bc_dis_able",
                                      shiny::selectizeInput(inputId = 'breed1', label = 'Select breeds', choices=breeds ,multiple=TRUE,options=list(maxItems=3)),
                                      shiny::actionButton(inputId="run_breeds",label="Run selected breeds"),
                                      shiny::selectInput(inputId = 'chromosome1',label = "Chromosome",choices="",selectize=FALSE),
                                      shiny::radioButtons(inputId = "approachselection_bc",label="Select approach",selected=c("Deterministic_male"),
                                                           choices=c("Deterministic_male"="Deterministic_male","Likelihood_male"="Likelihood_male","HMM_male"="HMM_male","HMM_female"="HMM_female","HMM_average"="HMM_average"))
                           )),"breedcomparison"),
        shinyjs::hidden(shiny::htmlOutput(outputId="notworking2",inline=TRUE)),
        shinyjs::hidden(shiny::htmlOutput(outputId="notworking3",inline=TRUE))
      ),
    shinydashboard::sidebarMenuOutput("menu")
  )

  ###################################################################################################################################
  #### definition body of the dashboard
  dbBody<- shinydashboard::dashboardBody(
     htmltools::tags$html(lang="en"), ## necessary to fulfill W3C

   ## metadata information
    htmltools::tags$head(
       tags$meta(name="author", content="Nina Melzer"),
       tags$meta(name="creation_date", content="2022-10-27"),
       tags$meta(name="modified_date", content="2026-02-12"),
       tags$meta(name="url", content="https://nmelzer.shinyapps.io/clarity"),
       tags$meta(name="version",content="3.0.0"),
       tags$meta(name="keywords", content="genetic map, physical map, recombination rate, genetic-map function, cattle breeds")
    ),
    metathis::meta()%>%
      metathis::meta_general(
        application_name = "CLARITY",
        description = "CLARITY app for an interactive exploration of breed-specific genetic maps in cattle",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "An R project",
      ) %>% meta_name("github-repo" = "nmelzer/CLARITY"),

    #### enabling links between different pages or tabs in the app  - code was adopted from https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard/37170333
    htmltools::tags$script(htmltools::HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")
    ),

   ##
   tags$head(tags$style(htmltools::HTML(".shiny-notification {
             position:fixed;
             top: calc(45%);
             left: calc(10%);
             background-color: #000000;
             color: #EE3B3B;
             font-size= 20px;
             }
             "
       )
     )
   ),
    ## include styles.css
    tags$link(rel="stylesheet",type="text/css",href="www/styles.css"),
    ## include favicon
    tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.ico")),

    ## for the sidebar toogle icon
    tags$head(
     tags$script(htmltools::HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var toggleButtons = document.querySelectorAll('[role=\"navigation\"]');
        toggleButtons.forEach(function(button) {
          var icon = button.querySelector('i');
          if (icon) {
            icon.setAttribute('aria-label', 'navigation');
            icon.setAttribute('role', 'button');
          }
        });
      });
    "))
   ),

   # Include JavaScript to change attributes by menuSubItem - works local but not really when online! - lighthouse test failed
   tags$head(tags$script(htmltools::HTML("
      $(document).ready(function() {
        $('a[data-value=\"info\"]').removeAttr('aria-expanded');
        $('a[data-value=\"info\"]').removeAttr('aria-selected');
        $('a[data-value=\"about\"]').removeAttr('aria-selected');
        $('a[data-value=\"contact\"]').removeAttr('aria-selected');
        $('a[data-value=\"infodatasets\"]').removeAttr('aria-selected');
        $('a[data-value=\"misplaced_markers\"]').removeAttr('aria-selected');
        $('a[data-value=\"methodology\"]').removeAttr('aria-selected');
      });
    "))
   ),

   ## for the sidebar menuSubItem to set "aria-selected=false" - failed Lighthouse test - additional added
   tags$head(
     tags$style(
       (htmltools::HTML(".sidebar-menu li.menu-open > .treeview-menu > li > a[aria-selected='false'] {} ")
     ))
   ),

  ###################################################################################################################################
  #### website elements - main panel
   shinydashboard::tabItems(
      shinydashboard::tabItem(tabName="info",
                                mod_startscreen_ui("startscreen_1")
      ),
      shinydashboard::tabItem(tabName="about",
                             mod_about_the_project_ui("mod_about_the_project_1")
      ),
      shinydashboard::tabItem(tabName="contact",
                              mod_contact_ui("mod_contact_1")
      ),
      shinydashboard::tabItem(tabName="infodatasets", ## if further data sets available than extend here
                              htmltools::h3("Information about the data sets"),
                              mod_datasets_ui("mod_datasets_1")
      ),
      shinydashboard::tabItem(tabName="misplaced_markers",
                              htmltools::h3("Misplaced markers"),
                              mod_misplaced_ui("mod_misplaced_1")
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
                    shiny::tabPanel(title="General information", mod_general_ui("mod_general_1")
                    ),
                    shiny::tabPanel(title="Genetic map", shiny::uiOutput("genetic_map_ui")
                    ),
                    shiny::tabPanel(title="Hotspot detection",shiny::uiOutput("hotspot_ui")
                    ),
                    shiny::tabPanel(title="Genetic-map functions",shiny::uiOutput("genetic_function_ui")
                    )
        )
    ),
    ## Breed comparison
    shinydashboard::tabItem(
       tabName="breedcomparison",
       htmltools::h3("Breed comparison"),
       shinyjs::useShinyjs(),
       shiny::tabsetPanel(id="navbar2",
          shiny::tabPanel(title="General information",mod_bc_general_ui("bc_general_1")
          ),
          shiny::tabPanel(title="Genetic map", shiny::uiOutput("bc_genetic_map_ui")#,
          ),
          shiny::tabPanel(title="Hotspot detection",shiny::uiOutput("bc_hotspot_ui")
          ),
         shiny::tabPanel(title="Genetic-map functions",shiny::uiOutput("bc_genetic_function_ui")
         )
      )
    )
   )#end of tag list
  ) ## End body of the dashboard

  ## Define UI
  shiny::shinyUI(
    htmltools::tagList( ### necessary for the golem package: otherwise error during package testing: Failed tests (test-golem-recommended.R:3:3): app ui `ui` has class 'shiny.tag', not class 'shiny.tag.list'
              ## https://github.com/ColinFay/golemize/blob/master/shiny-dashboard/shiny-dashboard-golem/R/app_ui.R checked example

    tags$head(tags$script(src="www/custom.js")),

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

