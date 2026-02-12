#' The application server-side
#'
#' @title App_server
#' @description The application server-side.
#' @param input,output,session Internal parameters for '`shiny`'.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom cachem cache_disk
#' @rawNamespace import(shinyjs, except = runExample)
#' @export
#'

app_server <- function( input, output, session) {

  ###################################################################################################################################
  ##### Default settings
  shiny::shinyOptions(cache = cachem::cache_disk("./app_cache/cache/")) ## enabling caching - also working on shinyio

  session$onSessionEnded(stopApp) ## Automatically stop a Shiny app when closing the browser tab adopted from Dean Attali

  ###################################################################################################################################
  ##### Default settings for buttons

  shinyjs::hide(id="backtoanalysis")
  shinyjs::hide(id="backtobc")
  shinyjs::hide(id="back_infodatasets")

  ###################################################################################################################################
  ##### Initialize

  ## for traffic light
  make.tl <- make.tl.bc <-NULL

  ## belongs to the linking of tabs within the app
  remember.prev.tab = shiny::reactiveValues()

  ## Initialize reactive values to disable and enable "Breed analysis" sidebar elements (NULL - initialize, TRUE - enable, FALSE - disable).
  ## depending on rendering status of the plots in the corresponding modules.
  figure_rendered_g <- shiny::reactiveVal(NULL) # general information
  figure_rendered_gm<-shiny::reactiveVal(NULL)  # genetic map
  figure_rendered <- shiny::reactiveVal(NULL)   # hotspot detection
  figure_rendered_gf <- shiny::reactiveVal(NULL)# genetic-map function

  ## Initialize reactive values to disable and enable "Breed comparison" sidebar elements (NULL - initialize, TRUE - enable, FALSE - disable).
  ## depending on rendering status of the plots in the corresponding modules.
  figure_rend2_g <- shiny::reactiveVal(NULL) # general information
  figure_rend2_gm<-shiny::reactiveVal(NULL)  # genetic map
  figure_rend2<- shiny::reactiveVal(NULL)    # hotspot detection
  figure_rend2_gf <- shiny::reactiveVal(NULL)# genetic-map function

  ## Track the last input type (0 - initialize, 1 - breed selection, 2 - chromosome, 3 - approach, 4 - selection cannot run) for "Breed analysis"
  sel_user<-shiny::reactiveVal(0)

  ## Track the last input type (0 - initialize, 1 - breed selection, 2 - chromosome, 3 - approach) for "Breed comparison"
  sel_user_bc<-shiny::reactiveVal(0)

  #### Reactive values to generate dynamic module IDs for "Breed analysis".
  number<-reactiveVal(0)
  module_id2<-reactiveVal() # for genetic map module
  module_id3<-reactiveVal() # for hotspot module
  module_id4<-reactiveVal() # for genetic-map function module

  #### Reactive values to generate dynamic module IDs for "Breed comparison".
  bc_number<-reactiveVal(0)
  module_bc_id2<-reactiveVal() # for genetic map
  module_bc_id3<-reactiveVal() # for hotspot
  module_bc_id4<-reactiveVal() # for genetic-map function

  ## Track the module states (0 - execute, 1 - already executed or initialized) for "Breed analysis"
  state.module <-reactiveValues(general=1,gm=1,hotspot=1,gf=1)

  ## for storing the last module ID for genetic map - Breed analysis
  old.gm.module.id<-reactiveVal()

  ## Track the module states (0 - execute, 1 - already executed or initialized) for "Breed comparison"
  state.bc.module<-reactiveValues(general=1,gm=1,hotspot=1,gf=1)

  ## for storing the last module ID for the genetic map - Breed comparison
  old.bc.gm.module.id<-reactiveVal()

  #### Track sidebar tab changes (0 - initialize; 1 - to Breed analysis; 2 - to Breed comparison).
  last.val.mn<-shiny::reactiveVal(0)

  ###################################################################################################################################
  ##### Default settings for "Breed analysis" and "Breed comparison" navbar pages - all corresponding tab panels are
  ##### completely hidden when starting the app

  shiny::hideTab(inputId = "navbar","General information")
  shiny::hideTab(inputId = "navbar","Hotspot detection")
  shiny::hideTab(inputId = "navbar","Genetic map")
  shiny::hideTab(inputId = "navbar","Genetic-map functions")

  shiny::hideTab(inputId = "navbar2","General information")
  shiny::hideTab(inputId = "navbar2","Genetic map")
  shiny::hideTab(inputId = "navbar2","Hotspot detection")
  shiny::hideTab(inputId = "navbar2","Genetic-map functions")


  ###################################################################################################################################
  #### Everything that belongs to the linking of tabs within the app and track switches to "Breed analysis" or "Breed comparison"
  #### in the sidebar
  ###################################################################################################################################

  shiny::observeEvent(input$tabs, {
    req(input$tabs)

    #store old current tab as last tab reactive value
    remember.prev.tab$last_tab = remember.prev.tab$current_tab
    remember.prev.tab$current_tab = input$tabs

    if(input$tabs=="methodology")
    {
      if(remember.prev.tab$last_tab=="infodatasets")shinyjs::show(id="back_infodatasets")
      else shinyjs::hide(id="back_infodatasets")

      if(remember.prev.tab$last_tab=="breedcomparison")shinyjs::show(id="backtobc")
      else shinyjs::hide(id="backtobc")

      if(remember.prev.tab$last_tab=="single")shinyjs::show(id="backtoanalysis")
      else shinyjs::hide(id="backtoanalysis")
    }

    if(input$tabs=="single"){
      ## Check if the last_tab was not "single" and inputs are not empty/null
      if(remember.prev.tab$last_tab!="single" && is.null(input$chromosome)!=TRUE && input$breed!="" && nrow(approach.info)!=0){
          ## change accordingly the state to 1 that a sidebar change to "Breed analysis" has happens
          last.val.mn(1)
          shinyjs::click("run_single")
      }
    }

    if(input$tabs=="breedcomparison")
    {
      ## Check if the last_tab was not "breedcomparison" and inputs are not empty/null
      if(remember.prev.tab$last_tab!="breedcomparison"){
        if(is.null(input$chromosome1)!=TRUE && length(input$breed1)!=0 && nrow(approach.sel)!=0){
          ## change accordingly the state to 2 that a sidebar change to "Breed comparison" has happens
          last.val.mn(2)
          shinyjs::click("run_breeds")
        }
      }
    }
  })

  ###################################################################################################################################
  ##### All belonging to the part 'Information' - SidebarMneu
  ###################################################################################################################################

  #### start screen
  mod_startscreen_ui("startscreen_1")

  #### 1. about the project
  mod_about_the_project_server("mod_about_the_project_1")

  #### 2. information about data set(s)
  mod_datasets_server("mod_datasets_1")

  #### 3. generally misplaced markers
  mod_misplaced_server("mod_misplaced_1")

  #### 3. bibliography module is all included for the corresponding site
  mod_methodology_server("mod_methodology_1")

  #### 4. contact module is all included for the corresponding site
  mod_contact_server("mod_contact_1")

  ###################################################################################################################################
  #### For Holstein-DE, the HMM approaches (male, female and average) are not available.
  #### Generate corresponding main UI output texts to inform user when selected.

  ## "Breed analysis"
  output$notworking<-shiny::renderText(paste('<span style=\"color:red; margin-left: 6px;\">HMM-based approach <br> &nbsp; for Holstein-DE <br> &nbsp; currently not included.
                                      <br> &nbsp; Results only for <br>&nbsp; deterministic and <br>&nbsp; likelihood-based <br> &nbsp;approach are available.</span>'))
  ## "Breed comparison"
  output$notworking2<-shiny::renderText(paste('<span style=\"color:red;margin-left: 6px;\">HMM-based approach <br> &nbsp; for Holstein-DE <br> &nbsp; currently not included.
                                      <br> &nbsp; Only the results <br> &nbsp;for the other selected <br> &nbsp;breeds are shown.</span>'))
  output$notworking3<-shiny::renderText(paste('<span style=\"color:red; margin-left: 6px;\">HMM-based approach <br> &nbsp; for Holstein-DE <br> &nbsp; currently not included.
                                      <br> &nbsp; Results only for <br>&nbsp; deterministic and <br>&nbsp; likelihood-based <br> &nbsp;approach are available.</span>'))


  ###################################################################################################################################
  ##### All belonging to the part 'Breed analysis' - SidebarMenu
  ###################################################################################################################################

  ## initialize
  approach.info<-approach.selected<-geneticMap<-geneticMap2<-add.approach.names<-NULL

  shinyjs::hide(id="chromosome")
  shinyjs::hide(id="approachselection")
  shinyjs::hide(id="run_single")

  ####  User inputs
  ## 1. Breed selection - Breed analysis
  shiny::observeEvent(input$breed,{
    if(input$breed==""){
      y<-character(0)
    }
    if(input$breed!="")
    {
      req(input$breed)
      breed.selected <- input$breed

      shinyjs::show(id="approachselection")
      shinyjs::show(id="chromosome")

      shiny::updateSelectInput(session, 'breed', label = "Breed", choices = breed.table$Name, selected = input$breed)

      make.tl <<- make_traffic_light(selected.breed=input$breed,dat.tl=BreedOverview[match(input$breed,BreedOverview$Breed),])

      ## take out when Holstein-DE is included
      if(input$breed=="Holstein-DE" && length(grep("HMM",input$approachselection))!=0)
      {
        shinyjs::show(id="notworking")
        approach.selected<<-approach.selected[-grep("HMM",approach.selected)]
      }else{
        shinyjs::hide(id="notworking")
        approach.selected<<-input$approachselection
      }

      approach.info<<-approach.table[match(approach.selected,approach.table$Approach),]
      add.approach.names<<-paste0(approach.info$Abbreviation,collapse="-")

      load(system.file("extdata",paste0(input$breed,"/geneticMap.Rdata"),package="CLARITY"))
      geneticMap2<<-geneticMap[,c(2,1,3,8,4,5,6,7,10,9,12,11,14,13)]

      if(input$chromosome=="" || is.null(input$chromosome)==TRUE)
      {
        shinyjs::hide(id="run_single")
        shiny::updateSelectInput(session,'chromosome',label="Chromosome",choices=no.chr,selected="")

        shiny::hideTab(inputId = "navbar","General information")
        shiny::hideTab(inputId = "navbar","Genetic map")
        shiny::hideTab(inputId = "navbar","Hotspot detection")
        shiny::hideTab(inputId = "navbar","Genetic-map functions")
      }
      else
      {
        if(length(approach.selected)==0){
          sel_user(4)  ## case Holstein-DE and HMM approaches selected
          shinyjs::click("run_single")
          shiny::hideTab(inputId = "navbar","General information")
          shiny::hideTab(inputId = "navbar","Genetic map")
          shiny::hideTab(inputId = "navbar","Hotspot detection")
          shiny::hideTab(inputId = "navbar","Genetic-map functions")
        }
        else{
            sel_user(1)
            shinyjs::click("run_single")
        }
      }
    }
  })

  ## 2. Chromosome selection - Breed analysis
  shiny::observeEvent(input$chromosome,{
    req(input$chromosome)

    if(input$chromosome!=""){
       if(length(approach.selected)==0 && input$breed=="Holstein-DE") sel_user(4)
       else sel_user(2)
       shinyjs::click("run_single")
    }
  })

  ## 3. Approach selection - Breed analysis
  shiny::observeEvent(input$approachselection,{
     shinyjs::show("run_single")
     approach.selected<<-input$approachselection

     ## check for that only three approaches can be selected
     if(length(input$approachselection)<=3){
         shiny::updateCheckboxGroupInput(session,inputId = "approachselection",label="Select approaches",choices=approach.table$Approach,selected=input$approachselection)
     }else{
         shiny::updateCheckboxGroupInput(session,inputId = "approachselection",label="Select approaches",choices=input$approachselection ,selected=input$approachselection[1:3])
         approach.selected<<-input$approachselection[1:3]
         shiny::showNotification("Only three approaches can be selected.")
     }
       ## take out when Holstein-DE is included
     if(input$breed=="Holstein-DE" && length(grep("HMM",input$approachselection))!=0)
     {
         approach.selected<<-approach.selected[-grep("HMM",approach.selected)]
         if(length(approach.selected)==0 || is.null(input$chromosome)==TRUE)
         {
           shinyjs::hide(id="run_single")
           shiny::hideTab(inputId = "navbar","General information")
           shiny::hideTab(inputId = "navbar","Genetic map")
           shiny::hideTab(inputId = "navbar","Hotspot detection")
           shiny::hideTab(inputId = "navbar","Genetic-map functions")
            sel_user(4)
         }else{
           shinyjs::show(id="run_single")
         }
         shinyjs::show(id="notworking")
      }else{
         shinyjs::hide(id="notworking")
     }

     approach.info<<-approach.table[match(approach.selected,approach.table$Approach),]
     add.approach.names<<-paste0(approach.info$Abbreviation,collapse="-")

     if(nrow(approach.info)>0){
        sel_user(3)
        shinyjs::show(id="run_single")
     }else{
        shinyjs::click("run_single") # included to catch Holstein-DE correctly
        shinyjs::hide(id="run_single")
        shiny::hideTab(inputId = "navbar","General information")
        shiny::hideTab(inputId = "navbar","Genetic map")
        shiny::hideTab(inputId = "navbar","Hotspot detection")
        shiny::hideTab(inputId = "navbar","Genetic-map functions")
     }
  }, ignoreInit=TRUE)


  #### React to when "run_single" button was clicked
  shiny::observeEvent(input$run_single,{
    req(input$run_single)

    shinyjs::hide(id="run_single")

    ## Check if user inputs are possible to execute
    if((length(input$approachselection)!=0 || is.null(input$approachselection)==FALSE)  && sel_user()!=4 && (length(input$chromosome)!=0 || is.null(input$chromosome)==FALSE)){

      ## Generate new module IDs when user change inputs by Breed analysis; otherwise reuse the current module ID.
      if(last.val.mn()!=1)
      {
        number(number()+1)

        ## Generate dynamic module ID and render UI for genetic map
        module_id2(paste0("genetic_map_",number()))
        output$genetic_map_ui<-renderUI({mod_genetic_map_ui(module_id2())})

        ## Generate dynamic module ID and render UI for hotspot detection
        module_id3(paste0("hotspot_", number()))
        output$hotspot_ui <- renderUI({mod_hotspot_ui(module_id3())})

        ## Generate a new dynamic module ID for genetic-map function only when a different breed or chromosome is selected to avoid re-rendering
        if(sel_user()<3)
        {
          module_id4(paste0("genetic_map_function_", number()))
          output$genetic_function_ui<-renderUI({mod_genetic_function_ui(module_id4())})
        }
      }

      ## show all corresponding navbar tabs
      shiny::showTab(inputId = "navbar","General information")
      shiny::showTab(inputId = "navbar","Genetic map")
      shiny::showTab(inputId = "navbar","Hotspot detection")
      shiny::showTab(inputId = "navbar","Genetic-map functions")

      ## initialize status navbar tabs (0 - execute, 1 - already executed or initialized)
      state.module$general<-1
      state.module$gm<-1
      state.module$hotspot<-1
      state.module$gf<-1

      ## reset the variable to default, when previous sidebar element was not 'single' - "Breed analysis"
      if(last.val.mn()==1)last.val.mn(0)

      ## Check which navbar tab is currently active and reset corresponding module status to 0 (execute) to trigger the module execution in the corresponding observe event (see below).
      if(is.null(input$navbar)==TRUE || input$navbar=="General information") state.module$general<-0
      else if(input$navbar=="Genetic map")state.module$gm<-0
      else if(input$navbar=="Hotspot detection")state.module$hotspot<-0
      else if(input$navbar=="Genetic-map functions")state.module$gf<-0

    }else{
      ## output warnings
      if(length(input$chromosome)==0 || is.null(input$chromosome)==TRUE)
      {
         shiny::showNotification("You have to select a chromosome.")
      }else{
         if(sel_user()==4)shiny::showNotification("For Holstein-DE, please select 'Deterministic_male' or 'Likelihood_male'.")
         else shiny::showNotification("You have to select at least on method.")
      }

      ## Set all reactive values which track the rendering status of the plots in the modules to TRUE to trigger
      ## the corresponding observe events (see below) and enable the "Breed analysis" sidebar.
      figure_rendered_g(TRUE)
      figure_rendered(TRUE)
      figure_rendered_gm(TRUE)
      figure_rendered_gf(TRUE)


      ## hide all corresponding navbar tabs
      shiny::hideTab(inputId = "navbar","General information")
      shiny::hideTab(inputId = "navbar","Genetic map")
      shiny::hideTab(inputId = "navbar","Hotspot detection")
      shiny::hideTab(inputId = "navbar","Genetic-map functions")
    }
  }, ignoreInit = TRUE)


  ##### React when a corresponding navbar tab is clicked.
  ##### Update the status of the corresponding module to trigger its module execution in the corresponding observe event (ensure plots are fully displayed).
  shiny::observeEvent(input$navbar,{
    req(input$navbar)

    if(input$navbar=="General information" && state.module$general==1) state.module$general<-0
    else if(input$navbar=="Genetic map" && state.module$gm==1)state.module$gm<-0
    else if(input$navbar=="Hotspot detection" && state.module$hotspot==1)state.module$hotspot<-0
    else if(input$navbar=="Genetic-map functions" && state.module$gf==1)state.module$gf<-0
  },ignoreInit = TRUE)


  #### Here module state changes are recognized and if module state is 0, the corresponding module is executed.
  ## React to state changes for the module general - Breed analysis
  shiny::observeEvent(state.module$general,{
    req(state.module$general)

    if(state.module$general==0){
      shinyjs::disable("breed_dis_able")

      # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed analysis" sidebar.
      figure_rendered_g(FALSE)

      mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed, make.traff.light=reactive(make.tl),
                         approach.info=approach.info,dt=geneticMap_summary[[input$breed]],add.approach.names=add.approach.names,figure_rendered_g = figure_rendered_g)

      ## change module status to 1 (executed)
      state.module$general=1
    }
  },ignoreInit = TRUE)

  ## React to state changes for module genetic map (gm) - Breed analysis
  shiny::observeEvent(state.module$gm,{
    req(state.module$gm)

    if(state.module$gm==0 && nrow(approach.info)>0){
        # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed analysis" sidebar.
        figure_rendered_gm(FALSE)

        ## Check if the last module ID differs from the current ID:
        ## different: a new module ID is used and state = 0 is passed to the module to indicate the slider has to be created
        ## equal: the module ID is reused and state = 1 is passed to the module to indicate the slider has already been created and user settings are retained.
        if(is.null(old.gm.module.id())==TRUE || old.gm.module.id()!=module_id2()){
              mod_genetic_map_server(module_id2(), filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap2,make.traff.light=reactive(make.tl),
                             approach.info=approach.info,add.approach.names=add.approach.names,figure_rendered_gm=figure_rendered_gm,state=0)
        }else{
              mod_genetic_map_server(module_id2(), filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap2,make.traff.light=reactive(make.tl),
                             approach.info=approach.info,add.approach.names=add.approach.names,figure_rendered_gm=figure_rendered_gm,state=1)
        }

        # store last module ID
        old.gm.module.id(module_id2())

        ## change module status to 1 (executed)
        state.module$gm=1
    }
  },ignoreInit = TRUE)

  ## React to state changes for module hotspot - Breed analysis
  shiny::observeEvent(state.module$hotspot,{
    req(state.module$hotspot)

    if(state.module$hotspot==0  && nrow(approach.info)>0){
        # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed analysis" sidebar.
        figure_rendered(FALSE)

        mod_hotspot_server(module_id3(),filter=input$chromosome,breed.select=input$breed,approach.info=approach.info,
                           add.approach.names=add.approach.names,figure_rendered = figure_rendered)

        ## change module status to 1 (executed)
        state.module$hotspot=1
    }

  },ignoreInit = TRUE)

  ## React to state changes for module genetic-map function (gf) - Breed analysis
  shiny::observeEvent(state.module$gf,{
    req(state.module$gf)

    if(state.module$gf==0){
        # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed analysis" sidebar.
        figure_rendered_gf(FALSE)

        mod_genetic_function_server(module_id4(),filter=input$chromosome,breed.select=input$breed,make.traff.light=reactive(make.tl),
                                    approach.info=approach.info,figure_rendered_gf=figure_rendered_gf)
        ## change module status to 1 (executed)
        state.module$gf=1
    }
  },ignoreInit = TRUE)


  #### React to state changes for reactive values tracking the rendering status of the plots in the corresponding modules to enable/disable Breed analysis sidebar elements.
  #### State changes can be triggered by the main server or in the corresponding module.
  ## Enable/Disable the sidebar for module general (g) - Breed analysis
  shiny::observeEvent(figure_rendered_g(),{
    req(input$navbar)

    if(figure_rendered_g()==TRUE ) shinyjs::enable("breed_dis_able")
    else shinyjs::disable("breed_dis_able")
  })

  ## Enable/Disable the sidebar for module genetic map (gm) - Breed analysis
  shiny::observeEvent(figure_rendered_gm(), {
   req(input$navbar)

   if(input$navbar =="Genetic map")
   {
        if(figure_rendered_gm()==TRUE)shinyjs::enable("breed_dis_able")
        else shinyjs::disable("breed_dis_able")
    }
  },ignoreInit = TRUE)

  ## Enable/Disable the sidebar for module hotspot - Breed analysis
  shiny::observeEvent(figure_rendered(), {
    req(input$navbar)

    if(input$navbar =="Hotspot detection")
    {
      if(figure_rendered()==TRUE)shinyjs::enable("breed_dis_able")
      else shinyjs::disable("breed_dis_able")
    }
  },ignoreInit = TRUE)

  ## Enable/Disable the sidebar for module genetic-map functions (gf) - Breed analysis
  shiny::observeEvent(figure_rendered_gf(), {
   req(input$navbar)

   if(input$navbar =="Genetic-map functions")
   {
      if(figure_rendered_gf()==TRUE)  shinyjs::enable("breed_dis_able")
      else shinyjs::disable("breed_dis_able")
   }
  },ignoreInit = TRUE)


  ###################################################################################################################################
  ##### All belonging to the part 'Breed comparison' - SidebarMenu
  ###################################################################################################################################

  ## initialize
  approach.sel<-breed.infos<-geneticMap.bc<-names.files<-names.bc<-run<-breed.selected<-approach.select.bc<-stop1<-NULL

  shinyjs::hide(id="chromosome1")
  shinyjs::hide(id="approachselection_bc")
  shinyjs::hide(id="run_breeds")

  ####  User inputs
  ## 1. Breed selection - Breed comparison
  shiny::observeEvent(input$breed1,{
    req(input$breed1)

    if(input$breed1[1]=="" || length(input$breed1)<=1)
    {
      ## only first selection
       if(input$chromosome1 == "" || is.null(input$chromosome1)==TRUE) shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")

        run="no"

        shinyjs::hide(id="notworking2")
        shinyjs::hide(id="notworking3")
        shinyjs::hide("run_breeds")
    }else
    {
      shinyjs::show(id="approachselection_bc")
      shinyjs::show(id="chromosome1")
      run="yes"
      breed.selected=input$breed1
    }

     ## stop - exclude part when Holstein-DE HMM is included
     stop1<<-0

     if(run=="yes"){
       sel_user_bc(1)
       shinyjs::hide(id="notworking3") ##

       ### take out when Holstein-DE included
       if(length(grep("Holstein-DE",breed.selected)!=0) && length(grep("HMM",approach.sel$Approach))!=0)
       {

         if(length(breed.selected)==2){
           stop1<<-1
           shinyjs::show(id="notworking3") #
           shiny::hideTab(inputId = "navbar2","General information")
           shiny::hideTab(inputId = "navbar2","Genetic map")
           shiny::hideTab(inputId = "navbar2","Hotspot detection")
           shiny::hideTab(inputId = "navbar2","Genetic-map functions")
         }
         else{
           breed.selected<-breed.selected[-grep("Holstein-DE",breed.selected)]
           shinyjs::show(id="notworking2")
           shinyjs::hide(id="notworking3")
         }
       }
       else{

           shinyjs::hide(id="notworking2")
       }

       names.bc <<- sort(breed.selected)

       ## prepare data for traffic light and make the plot
       make.tl.bc<<-make_traffic_light(selected.breed=names.bc,dat.tl=BreedOverview[match(names.bc,BreedOverview$Breed),])

       geneticMap.ll=list()
       geneticMap.ll = mapply(function(x) {
         load(system.file("extdata", paste0(x, "/geneticMap.Rdata"), package = "CLARITY"))
         geneticMap
       }, names.bc, SIMPLIFY = FALSE)

       breed.infos<<-breed.table[match(breed.selected,breed.table$Name),]
       geneticMap.bc <<-geneticMap.ll
       names.files<<-paste(names.bc,collapse="-")

       if(is.null(input$chromosome1)!=TRUE && input$chromosome1!="" && stop1==0) ## take out if when Holstein-DE is included only if here
       {
         sel_user_bc(1)
         shinyjs::show("run_breeds")
       }else{
         shinyjs::hide("run_breeds")
       }
     }
  }, ignoreInit = TRUE)

  ## 2. Chromosome selection - Breed comparison
  shiny::observeEvent(input$chromosome1,{
    req(input$chromosome1)

    if(input$chromosome1!="" && is.null(approach.sel)!=TRUE && nrow(approach.sel)>0)
    {
        ## take out when Holstein-DE is included
        if(stop1==0){
          sel_user_bc(2)
          shinyjs::click("run_breeds")
        }
    }else{
        shiny::hideTab(inputId = "navbar2","General information")
        shiny::hideTab(inputId = "navbar2","Genetic map")
        shiny::hideTab(inputId = "navbar2","Hotspot detection")
        shiny::hideTab(inputId = "navbar2","Genetic-map functions")
    }
  })

  ## 3. Approach selection - Breed comparison
  shiny::observeEvent(input$approachselection_bc,{
      req(input$approachselection_bc)

      approach.sel<<-approach.table[match(input$approachselection_bc,approach.table$Approach),]

      ### take out when Holstein-DE included
      breed.selected<<-input$breed1

      ### stop - exclude part when HMM approaches for Holstein-DE is available
      stop1<<-0
      if(length(grep("Holstein-DE",breed.selected)!=0) && length(grep("HMM",input$approachselection_bc))!=0)
      {
         if(length(breed.selected)==2)
         {
           shinyjs::show(id="notworking3")
           stop1<<-1
           shiny::hideTab(inputId = "navbar2","General information")
           shiny::hideTab(inputId = "navbar2","Genetic map")
           shiny::hideTab(inputId = "navbar2","Hotspot detection")
           shiny::hideTab(inputId = "navbar2","Genetic-map functions")
         }else{

           breed.selected<<-breed.selected[-grep("Holstein-DE",breed.selected)]

           shinyjs::hide(id="notworking3")
           shinyjs::show(id="notworking2")

           names.bc <<- sort(breed.selected) #

           geneticMap.ll=list()
           geneticMap.ll = mapply(function(x) {
              load(system.file("extdata", paste0(x, "/geneticMap.Rdata"), package = "CLARITY"))
              geneticMap
           }, names.bc, SIMPLIFY = FALSE)

           breed.infos<<-breed.table[match(breed.selected,breed.table$Name),]
           geneticMap.bc <<-geneticMap.ll
         }
       }else
      {
         shinyjs::hide(id="notworking2")
         shinyjs::hide(id="notworking3")

         names.bc <<- sort(breed.selected)

         ## must run here again otherwise it is not recognized for Holstein-DE
         if(is.null(names.bc)==FALSE){
           make.tl.bc<<-make_traffic_light(selected.breed=names.bc,dat.tl=BreedOverview[match(names.bc,BreedOverview$Breed),])
          }

         geneticMap.ll=list()
         geneticMap.ll = mapply(function(x) {
           load(system.file("extdata", paste0(x, "/geneticMap.Rdata"), package = "CLARITY"))
           geneticMap
         }, names.bc, SIMPLIFY = FALSE)

         breed.infos<<-breed.table[match(breed.selected,breed.table$Name),]
         geneticMap.bc <<-geneticMap.ll
      }

      if(is.null(input$chromosome1)!=TRUE && input$chromosome1!="" && stop1==0)
      {
        sel_user_bc(3)
        shinyjs::click("run_breeds")
      }else{
         shinyjs::hide("run_breeds")
      }
  })


  #### React to when "run_breeds" button was clicked - Breed comparison
  shiny::observeEvent(input$run_breeds,{
    req(input$run_breeds)

    shinyjs::hide("run_breeds")

    ## Check if user inputs are possible to execute
    if(stop1==0 && length(input$breed1)>1){

      ## Generate new modules IDs only when the same sidebar element is used otherwise reuse previous settings.
      if(last.val.mn()!=2){
         bc_number(bc_number()+1)

         ## Generate dynamic module ID and render UI for genetic map
         module_bc_id2(paste0("bc_genetic_map_",bc_number()))
         output$bc_genetic_map_ui<-renderUI({mod_bc_genetic_map_ui(module_bc_id2())})

         ## Generate dynamic module ID and render UI for hotspot detection
         module_bc_id3(paste0("bc_hotspot_",bc_number()))
         output$bc_hotspot_ui<-renderUI({mod_bc_hotspot_ui(module_bc_id3())})

         ## Generate a new dynamic module ID for genetic-map function only when a different breed or chromosome is selected.
         if(sel_user_bc()<3){
           module_bc_id4(paste("bc_genetic_function_",bc_number()))
           output$bc_genetic_function_ui <- renderUI({mod_bc_genetic_function_ui(module_bc_id4())})
         }
      }

      ## show all corresponding navbar2 tabs
      shiny::showTab(inputId = "navbar2","General information")
      shiny::showTab(inputId = "navbar2","Genetic map")
      shiny::showTab(inputId = "navbar2","Hotspot detection")
      shiny::showTab(inputId = "navbar2","Genetic-map functions")

      ## initialize status navbar tabs (0 - execute, 1 - already executed or initialized)
      state.bc.module$general<-1
      state.bc.module$gm<-1
      state.bc.module$hotspot<-1
      state.bc.module$gf<-1

      ## reset the variable to default, when previous sidebar element was not 'breedcomparison' -  "Breed comparison"
      if(last.val.mn()==2)last.val.mn(0)

      ## Check which navbar2 tab is currently active and reset corresponding module status to 0 (execute) to trigger the module execution in the corresponding observe event (see below).
      if(is.null(input$navbar2)==TRUE || input$navbar2=="General information") state.bc.module$general<-0
      else if(input$navbar2=="Genetic map")state.bc.module$gm<-0
      else if(input$navbar2=="Hotspot detection")state.bc.module$hotspot<-0
      else if(input$navbar2=="Genetic-map functions")state.bc.module$gf<-0

    }else{
      ## show notification
      if(length(input$chromosome1)!=0 || is.null(input$chromosome1)==FALSE) showNotification("You have to select at least two breeds.")

      ## Set all reactive values which track the rendering status of the plots in the modules to TRUE to trigger
      ## the corresponding observe events (see below) and enable the "Breed comparison" sidebar.
      figure_rend2_g(TRUE)
      figure_rend2(TRUE)
      figure_rend2_gm(TRUE)
      figure_rend2_gf(TRUE)

      ## hide all navbar2 tabs
      shiny::hideTab(inputId = "navbar2","General information")
      shiny::hideTab(inputId = "navbar2","Genetic map")
      shiny::hideTab(inputId = "navbar2","Hotspot detection")
      shiny::hideTab(inputId = "navbar2","Genetic-map functions")
    }
  }, ignoreInit = TRUE)


  ##### React when a corresponding navbar2 tab is clicked.
  ##### Update the status of the corresponding module to trigger its module execution in the corresponding observe event (ensure plots are fully displayed).
  shiny::observeEvent(input$navbar2,{
    req(input$navbar2)

    if(input$navbar2=="General information" && state.bc.module$general==1) state.bc.module$general<-0
    else if(input$navbar2=="Genetic map" && state.bc.module$gm==1)state.bc.module$gm<-0
    else if(input$navbar2=="Hotspot detection" && state.bc.module$hotspot==1)state.bc.module$hotspot<-0
    else if(input$navbar2=="Genetic-map functions" && state.bc.module$gf==1)state.bc.module$gf<-0
  },ignoreInit = TRUE)


  #### Here module state changes are recognized and if module state is 0, the corresponding module is executed.
  ## React to state changes for module general - Breed comparison
  shiny::observeEvent(state.bc.module$general,{
    req(state.bc.module$general)

    if(state.bc.module$general==0){
      shinyjs::disable("bc_dis_able")

      #Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed comparison" sidebar.
      figure_rend2_g(FALSE)

      mod_bc_general_server("bc_general_1",filter=input$chromosome1, geneticMap.bc=geneticMap.bc,
                            names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel$Abbreviation,
                            dt=geneticMap_summary[match(names.bc,names(geneticMap_summary))],figure_rend2_g=figure_rend2_g)

      ## change module status to 1 (executed)
      state.bc.module$general=1
    }
  },ignoreInit = TRUE)

  ## React to state changes for module genetic map (gm) - Breed comparison
  shiny::observeEvent(state.bc.module$gm,{
    req(state.bc.module$gm)

    if(state.bc.module$gm==0){
      # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed comparison" sidebar.
      figure_rend2_gm(FALSE)

      ## Check if the last module ID differs from the current ID:
      ## different: a new module ID is used and state = 0 is passed to the module to indicate the slider has to be created
      ## equal: the module ID is reused and state = 1 is passed to the module to indicate the slider has already been created and user settings are retained.
      if(is.null(old.bc.gm.module.id())==TRUE || old.bc.gm.module.id() != module_bc_id2())
      {
                   mod_bc_genetic_map_server(module_bc_id2(),filter=input$chromosome1, geneticMap.bc=geneticMap.bc,names.files=names.files,
                                  make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel,figure_rend2_gm=figure_rend2_gm,state=0)
      }else{
                   mod_bc_genetic_map_server(module_bc_id2(),filter=input$chromosome1, geneticMap.bc=geneticMap.bc,names.files=names.files,
                                  make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel,figure_rend2_gm=figure_rend2_gm,state=1)
      }
      # store last module ID
      old.bc.gm.module.id(module_bc_id2())

      ## change module status to 1 (executed)
      state.bc.module$gm=1
    }
  },ignoreInit = TRUE)

  ## React to state changes for module hotspot detection - Breed comparison
  shiny::observeEvent(state.bc.module$hotspot,{
    req(state.bc.module$hotspot)

    if(state.bc.module$hotspot==0){
      # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed comparison" sidebar.
      figure_rend2(FALSE)

      mod_bc_hotspot_server(module_bc_id3(),filter=input$chromosome1,breed.infos=breed.infos,
                        names.files,approach=approach.sel,figure_rend2=figure_rend2)

      ## change module status to 1 (executed)
      state.bc.module$hotspot=1
    }
  },ignoreInit = TRUE)

  ## React to state changes for module genetic-map function (gf) - Breed comparison
  shiny::observeEvent(state.bc.module$gf,{
    req(state.bc.module$gf)

    if(state.bc.module$gf==0)
    {
      # Set the reactive value for tracking the plot status to FALSE to trigger the corresponding observe event (see below) and disable the "Breed comparison" sidebar.
      figure_rend2_gf(FALSE)

      mod_bc_genetic_function_server(module_bc_id4(),filter=input$chromosome1,breed.infos=breed.infos,
                                 names.files,make.traff.light.bc=reactive(make.tl.bc),approach=approach.sel,figure_rend2_gf=figure_rend2_gf)

      ## change module status to 1 (executed)
      state.bc.module$gf=1
    }
  },ignoreInit = TRUE)


  #### React to state changes for reactive values tracking the rendering status of the plots in the corresponding modules to enable/disable Breed comparison sidebar elements.
  #### State changes can be triggered by the main server or in the corresponding module.
  ## Enable/Disable the sidebar for module general (g) - Breed comparison
  shiny::observeEvent(figure_rend2_g(), {
    req(input$navbar2)

    if(figure_rend2_g()==TRUE ) shinyjs::enable("bc_dis_able")
    else shinyjs::disable("bc_dis_able")
  })

  ## Enable/Disable the sidebar for module genetic map (gm) - Breed comparison
  shiny::observeEvent(figure_rend2_gm(), {
    req(input$navbar2)

    if(input$navbar2 =="Genetic map")
    {
      if(figure_rend2_gm()==TRUE)shinyjs::enable("bc_dis_able")
      else shinyjs::disable("bc_dis_able")
    }
  },ignoreInit = TRUE)

  ## Enable/Disable the sidebar for module hotspot detection - Breed comparison
  shiny::observeEvent(figure_rend2(), {
    req(input$navbar2)

    if(input$navbar2 =="Hotspot detection" )
    {
      if(figure_rend2()==TRUE) shinyjs::enable("bc_dis_able")
      else shinyjs::disable("bc_dis_able")
    }
  }, ignoreInit = TRUE)

  ## Enable/Disable the sidebar for module genetic-map functions (gf) - Breed comparison
  shiny::observeEvent(figure_rend2_gf(), {
    req(input$navbar2)

    if(input$navbar2 =="Genetic-map functions")
    {
      if(figure_rend2_gf()==TRUE) shinyjs::enable("bc_dis_able")
      else shinyjs::disable("bc_dis_able")
    }
  }, ignoreInit = TRUE)
}
