#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @export
#'
#'
app_server <- function( input, output, session ) {

  ###############################################################################################################################
  ##### Default settings for specific buttons

  shinyjs::hide(id="backtoanalysis") ## hide Link "back to Analysis" within the bibliography module - it will be made visible after a chromosome was selected


  ################################################################################################################################
  ##### Default settings for breed analysis page - all corresponding tabpanels are completely hidden when starting the app

  hideTab(inputId = "navbar","General information")
  hideTab(inputId = "navbar","Misplaced markers")
  hideTab(inputId = "navbar","Hotspot detection")
  hideTab(inputId = "navbar","Genetic map")
  hideTab(inputId = "navbar","Genetic maps")
  hideTab(inputId = "navbar","Genetic-map functions")

  ##################################################################################################################################
  ##### Settings start page

  # loading the cow picture
  output$startpicture=shiny::renderImage({
    filename <- "www/Cow.png"
    list(src = filename,width="60%",height="50%")
  }, deleteFile = FALSE)

  ###################################################################################################################################
  ##### All belonging to the part 'Information' - SidebarMneu
  ###################################################################################################################################

  ##### 1. about the project
  callModule(mod_about_the_project_server,"mod_about_the_project_1")

  ##### 2. information about dataset(s)
  callModule(mod_datasets_server,"mod_datasets_1")

  ##### 3. bibliography module is all included for the corresponding site
  callModule(mod_methodology_server,"mod_methodology_1")

  ##### 4. contact module is all included for the corresponding site
  #callModule(mod_contact_server,"mod_contact_1")
  mod_contact_server("mod_contact_1")

  ###################################################################################################################################
  ##### All belonging to the part 'Results' - SidebarMenu
  ###################################################################################################################################
    ##### 1. Breed selection - currently only one breed included. #####
  # Creating events when selecting a breed
  # activated the map selection - ## breed selection not relevant at the moment - can be reused when it is of interest
  observeEvent(input$breed,{
    if(input$breed=="")y<-character(0)
    if(input$breed!="")
    {
      choice <- NULL
      y <- input$breed
      req(input$breed)

      no.chr=c("","All",paste(1:29))
      #### change map to a list or vector containing the possible maps
      #      map="ARS-UCD1.2"
      #      updateSelectInput(session, 'mapversion', label = "Map", choices = map ,selected = map)
      shiny::updateSelectInput(session,'chromosome',label="Chromosome",choices=no.chr,selected="")
      #### disable download button, especially necessary when new breed selection is done
    }
  })

  ##### 2. Chromosome selection  #####
  ## can be extended here also for other maps
  observeEvent(input$chromosome,
  {
    if(input$chromosome==""){
      y<-character(0)
    }
    if(input$chromosome!=""){
      y <- input$chromsome
      req(input$chromosome)

      ## activate the download button
      shinyjs::show(id="backtoanalysis")  ## made link back to analysis visible within bibliography-site

      ## activate displaying the tabpanels for the Breed analyis
      shiny::showTab(inputId = "navbar","General information")
      shiny::showTab(inputId = "navbar","Misplaced markers")
      shiny::showTab(inputId = "navbar","Genetic map")      ## hier muss noch eine raus
      shiny::showTab(inputId = "navbar","Hotspot detection")
      shiny::showTab(inputId = "navbar","Genetic-map functions")

      ##### 1. module general - tabpanel
      callModule(mod_general_server,"mod_general_1",filter=input$chromosome)

      ##### 2. module genetic map - tabpanel
      if(input$chromosome=="All")init_make_plots(output,session) ## the following line is to initialize the multiple plots - otherwise within the module the rendering function does not work - may a better solution exists
      callModule(mod_genetic_map_server,"mod_genetic_map_1", filter=input$chromosome)

      ###### 3. module misplaced - tabpanel
      callModule(mod_misplaced_server,"mod_misplaced_1",filter=input$chromosome)

      ###### 4. module hotspot - tabpanel
      callModule(mod_hotspot_server,"mod_hotspot_1",filter=input$chromosome)

      ###### 5. module genetic function - tabpanel
      callModule(mod_genetic_function_server,"mod_genetic_function_1",filter=input$chromosome)  ##
    }
  })


  ### Extend here for breed comparison
}
