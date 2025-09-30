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
  #### setting colors and shapes for breeds
  breed.table=as.data.frame(cbind(Name=c("Holstein-CH","Holstein-DE","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin"),
                                  Abbreviation=c("HC","HD","F","BS","B","A","S","L"),
                                  Color=c("#E27a53","#336666","#CDCD00","#414487FF","#6695CC","#C8A2C8","#8B5A00","#5D4760"),
                                  Color2=c("#AAAAAA","#7D7D7D","#C3C3C3","#7F7D9C","#787275","#ADADC9","#808080","#C5C6D0")))

  ###################################################################################################################################
  ## setting colors and shape for approaches
  approach.table=as.data.frame(cbind(
                       "Approach"=c("Deterministic_male","Likelihood_male","HMM_male","HMM_female","HMM_average"),
                       "Name"=c("Deterministic approach (male)","Likelihood-based approach (male)","HMM-based approach (male)","HMM-based approach (female)","HMM-based approach (average)" ),
                       "Abbreviation"=c("Dm","Lm","Hm","Hf","Ha"),# used in files
                       "Color"=c("dodgerblue2","cadetblue3","#E69F00","#CC79A7","#CD4F39"), ## approach color for hotspot
                       "Color2"=c("#AAAAAA",NA,"#7D7D7D","#C3C3C3","#BABABA"),#, ##  approach color for no hotspot
                       "Shape1"=c(15,100,17,18,19)  ##
  ))

  no.chr<-c("All",paste(1:29))

  ###################################################################################################################################
  ##### Default settings for buttons

  shinyjs::hide(id="backtoanalysis")
  shinyjs::hide(id="backtobc")
  shinyjs::hide(id="back_infodatasets")

  ###################################################################################################################################
  ##### Initialize (see Everything that belongs to the linking of tabs within the app)

  remember.prev.tab = reactiveValues()

  ###################################################################################################################################
  ##### Default settings for breed analysis page - all corresponding tab panels are completely hidden when starting the app

  shiny::hideTab(inputId = "navbar","General information")
  shiny::hideTab(inputId = "navbar","Hotspot detection")
  shiny::hideTab(inputId = "navbar","Genetic map")
  shiny::hideTab(inputId = "navbar","Genetic-map functions")

  shiny::hideTab(inputId = "navbar2","General information")
  shiny::hideTab(inputId = "navbar2","Genetic map")
  shiny::hideTab(inputId = "navbar2","Hotspot detection")
  shiny::hideTab(inputId = "navbar2","Genetic-map functions")

  ###################################################################################################################################
  #### load and prepare data which are used for the likelihood quality signal
  make.tl <- make.tl.bc <- OverviewBreed<-NULL

  load(system.file("extdata","general/OverviewBreeds.Rdata",package="CLARITY"))
  BreedOverview=cbind(OverviewBreed, round(OverviewBreed$N2/100)*100,ceiling((OverviewBreed$n2/OverviewBreed$N2)/100)*100)
  colnames(BreedOverview)[7:8]=c("N2.new","Averag.n2")

  ###################################################################################################################################
  #### load all genetic map summaries
  genetic_map_summary<-NULL

  geneticMap_summary = mapply(function(x) {
    load(system.file("extdata", paste0(x, "/genetic_map_summary.Rdata"), package = "CLARITY"))
     map.sum=as.data.frame(genetic_map_summary)
     colnames(map.sum)[c(8,10,13,16,19)]=c("Dm_cM/Mb","Lm_cM/Mb", "Hm_cM/Mb","Hf_cM/Mb","Ha_cM/Mb")
     map.sum
  }, breed.table$Name, SIMPLIFY = FALSE)

  ###################################################################################################################################
  #### Everything that belongs to the linking of tabs within the app
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
  })

  ###################################################################################################################################
  ##### All belonging to the part 'Information' - SidebarMneu
  ###################################################################################################################################

  mod_startscreen_ui("startscreen_1")

  ##### 1. about the project
  mod_about_the_project_server("mod_about_the_project_1")

  ##### 2. information about dataset(s)
  mod_datasets_server("mod_datasets_1",BreedOverview=BreedOverview)

  ##### 3. generally misplaced markers
  mod_misplaced_server("mod_misplaced_1")

  ##### 3. bibliography module is all included for the corresponding site
  mod_methodology_server("mod_methodology_1")

  ##### 4. contact module is all included for the corresponding site
  mod_contact_server("mod_contact_1")

  ############################ for Holstein-DE the approaches: HMM_female and HMM_male are missing -inform user when selected - take out when Holstein-DE is included
  output$notworking<-renderText(paste('<span style=\"color:red; margin-left: 6px;\">HMM-based approach <br> &nbsp; for Holstein-DE <br> &nbsp; currently not included.
                                      <br> &nbsp; Results only for <br>&nbsp; deterministic and <br>&nbsp; likelihood-based <br> &nbsp;approach are available.</span>'))
  output$notworking2<-renderText(paste('<span style=\"color:red;margin-left: 6px;\">HMM-based approach <br> &nbsp; for Holstein-DE <br> &nbsp; currently not included.
                                      <br> &nbsp; Only the results <br> &nbsp;for the other selected <br> &nbsp;breeds are shown.</span>'))
  output$notworking3<-renderText(paste('<span style=\"color:red; margin-left: 6px;\">HMM-based approach <br> &nbsp; for Holstein-DE <br> &nbsp; currently not included.
                                      <br> &nbsp; Results only for <br>&nbsp; deterministic and <br>&nbsp; likelihood-based <br> &nbsp;approach are available.</span>'))

  ###################################################################################################################################
  ##### All belonging to the part 'Breed analysis' - SidebarMenu
  ###################################################################################################################################

  approach.info<-approach.selected<-geneticMap<-geneticMap2<-add.approach.names<-stop_approach<-NULL

  shinyjs::hide(id="chromosome")
  shinyjs::hide(id="approachselection")

  ### 1. Breed selection
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
        approach.selected<<-approach.selected[-grep("HMM",approach.selected)]
        shinyjs::show(id="notworking")
      }else{
        shinyjs::hide(id="notworking")
        approach.selected<<-input$approachselection
      }

      approach.info<<-approach.table[match(approach.selected,approach.table$Approach),]

      load(system.file("extdata",paste0(input$breed,"/geneticMap.Rdata"),package="CLARITY"))
      geneticMap2<<-geneticMap[,c(2,1,3,8,4,5,6,7,10,9,12,11,14,13)]

      if(input$chromosome=="" || is.null(input$chromosome)==TRUE)
      {
        shiny::updateSelectInput(session,'chromosome',label="Chromosome",choices=no.chr,selected="")

        shiny::hideTab(inputId = "navbar","General information")
        shiny::hideTab(inputId = "navbar","Genetic map")
        shiny::hideTab(inputId = "navbar","Hotspot detection")
        shiny::hideTab(inputId = "navbar","Genetic-map functions")
      }
      else
      {
         ## take the if out when Hol-De included
        if(nrow(approach.info)>0)
        {
          shiny::showTab(inputId = "navbar","General information")
          shiny::showTab(inputId = "navbar","Genetic map")
          shiny::showTab(inputId = "navbar","Hotspot detection")
          shiny::showTab(inputId = "navbar","Genetic-map functions")

          ##### 1. module general - tabpanel
          mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed, make.traff.light=reactive(make.tl),
                           approach.info=approach.info,dt=geneticMap_summary[[input$breed]],add.approach.names=add.approach.names)
          ##### 2. module genetic map - tabpanel
          mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap2,
                               make.traff.light=reactive(make.tl),approach.info=approach.info,add.approach.names=add.approach.names)
          ##### 3. module hotspot - tabpanel
          mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,approach.info=approach.info,add.approach.names=add.approach.names)
          ##### 4. module genetic function - tabpanel
          mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed,
                                    make.traff.light=reactive(make.tl),approach.info=approach.info)
        }else{
            shiny::hideTab(inputId = "navbar","General information")
            shiny::hideTab(inputId = "navbar","Genetic map")
            shiny::hideTab(inputId = "navbar","Hotspot detection")
            shiny::hideTab(inputId = "navbar","Genetic-map functions")
        }
      }
    }
  })

  ### 2. Chromosome selection
  shiny::observeEvent(input$chromosome,
  {
    if(input$chromosome==""){
      y<-character(0)
    }
    if(input$chromosome!="" && nrow(approach.info)>0){
      req(input$chromosome)

      ## activate displaying the tabpanels for the Breed analyis
      shiny::showTab(inputId = "navbar","General information")
      shiny::showTab(inputId = "navbar","Genetic map")
      shiny::showTab(inputId = "navbar","Hotspot detection")
      shiny::showTab(inputId = "navbar","Genetic-map functions")

      ##### 1. module general - tabpanel
      mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed,make.traff.light=reactive(make.tl),
                         approach.info=approach.info,dt=geneticMap_summary[[input$breed]],add.approach.names=add.approach.names)
      ##### 2. module genetic map - tabpanel
      mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap2,make.traff.light=reactive(make.tl),
                             approach.info=approach.info,add.approach.names=add.approach.names)
      ###### 3. module hotspot - tabpanel
      mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,approach.info=approach.info,add.approach.names=add.approach.names)
      ###### 4. module genetic function - tabpanel
      mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed,make.traff.light=reactive(make.tl),approach.info=approach.info)
    }else
    {
      shiny::hideTab(inputId = "navbar","General information")
      shiny::hideTab(inputId = "navbar","Genetic map")
      shiny::hideTab(inputId = "navbar","Hotspot detection")
      shiny::hideTab(inputId = "navbar","Genetic-map functions")
    }
  })

  ### 3. Approach selection
  shiny::observeEvent(input$approachselection,{

     if(is.null(input$approachselection)!=TRUE){
       if(input$breed[1]=="" || length(input$breed)==0){
         shinyjs::hide(id="chromosome")
       }else shinyjs::show(id="chromosome")

       approach.selected<<-input$approachselection

       ## check for that only 3 approaches can be selected
       if(length(input$approachselection)<3 && is.null(stop_approach)==FALSE){
         shiny::updateCheckboxGroupInput(session,inputId = "approachselection",label="Select approaches",choices=approach.table$Approach,selected=input$approachselection)
         stop_approach<<-NULL
       }
       else if(length(input$approachselection)==3 && is.null(stop_approach)==TRUE){
         shiny::updateCheckboxGroupInput(session,inputId = "approachselection",label="Select approaches",choices=input$approachselection[1:3],selected=input$approachselection[1:3])
         approach.selected<<-input$approachselection[1:3]
         stop_approach<<-1
        }

       ## take out when Holstein-DE is included
      if(input$breed=="Holstein-DE" && length(grep("HMM",input$approachselection))!=0)
      {
         approach.selected<<-approach.selected[-grep("HMM",approach.selected)]
         if(length(approach.selected)==0 || input$chromosome=="")
         {
           shiny::hideTab(inputId = "navbar","General information")
           shiny::hideTab(inputId = "navbar","Genetic map")
           shiny::hideTab(inputId = "navbar","Hotspot detection")
           shiny::hideTab(inputId = "navbar","Genetic-map functions")
         }else{
           shiny::showTab(inputId = "navbar","General information")
           shiny::showTab(inputId = "navbar","Genetic map")
           shiny::showTab(inputId = "navbar","Hotspot detection")
           shiny::showTab(inputId = "navbar","Genetic-map functions")
         }
         shinyjs::show(id="notworking")
      }else{
         shinyjs::hide(id="notworking")
      }
      ###

      approach.info<<-approach.table[match(approach.selected,approach.table$Approach),]
      add.approach.names<<-paste0(approach.info$Abbreviation,collapse="-")

      if(is.null(input$chromosome)!=TRUE && nrow(approach.info)>0)
      {
        shiny::showTab(inputId = "navbar","General information")
        shiny::showTab(inputId = "navbar","Genetic map")
        shiny::showTab(inputId = "navbar","Hotspot detection")
        shiny::showTab(inputId = "navbar","Genetic-map functions")

        ##### 1. module general - tabpanel
        mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed, make.traff.light=reactive(make.tl),
                         approach.info=approach.info,dt=geneticMap_summary[[input$breed]],add.approach.names=add.approach.names)
        ##### 2. module genetic map - tabpanel
        mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap2,make.traff.light=reactive(make.tl),
                               approach.info=approach.info,add.approach.names=add.approach.names)
        ##### 3. module hotspot - tabpanel
        mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,approach.info=approach.info,add.approach.names=add.approach.names)
        ##### 4. module genetic function - tabpanel
        mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed,make.traff.light=reactive(make.tl),approach.info=approach.info)
      }else{
        shiny::hideTab(inputId = "navbar","General information")
        shiny::hideTab(inputId = "navbar","Genetic map")
        shiny::hideTab(inputId = "navbar","Hotspot detection")
        shiny::hideTab(inputId = "navbar","Genetic-map functions")
      }
    }else{
       shinyjs::hide(id="chromosome")

      shiny::hideTab(inputId = "navbar","General information")
      shiny::hideTab(inputId = "navbar","Genetic map")
      shiny::hideTab(inputId = "navbar","Hotspot detection")
      shiny::hideTab(inputId = "navbar","Genetic-map functions")
    }
}, ignoreNULL=FALSE)

  ###################################################################################################################################
  ##### All belonging to the part 'Breed comparison' - SidebarMenu
  ###################################################################################################################################
  approach.sel<-breed.infos<-geneticMap.bc<-names.files<-names.bc<-run<-breed.selected<-approach.select.bc<-stop1<-stop2<-NULL

  shinyjs::hide(id="chromosome1")
  shinyjs::hide(id="approachselection_bc")

  ### 1. Breed selection - BC
  shiny::observeEvent(input$breed1,{
    req(input$breed1)

    if(input$breed1[1]=="" || length(input$breed1)<=1)
    {
       y<-character(0)

        shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")
        run="no"

        shinyjs::hide(id="chromosome1")
        shinyjs::hide(id="approachselection_bc")
        shinyjs::hide(id="notworking2")
        shinyjs::hide(id="notworking3")
        shiny::hideTab(inputId = "navbar2","General information")
        shiny::hideTab(inputId = "navbar2","Genetic map")
        shiny::hideTab(inputId = "navbar2","Hotspot detection")
        shiny::hideTab(inputId = "navbar2","Genetic-map functions")
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

       names.bc <<- breed.selected

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
         shiny::showTab(inputId = "navbar2","General information")
         shiny::showTab(inputId = "navbar2","Genetic map")
         shiny::showTab(inputId = "navbar2","Hotspot detection")
         shiny::showTab(inputId = "navbar2","Genetic-map functions")

         ##### 1. module general - tabpanel - bc
         mod_bc_general_server("bc_general_1",filter=input$chromosome1,geneticMap.bc=geneticMap.bc,
                               names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),
                               breed.infos=breed.infos, approach=approach.sel$Abbreviation,
                               dt=geneticMap_summary[match(names.bc,names(geneticMap_summary))])
         ##### 2. module genetic map - tabpanel -bc
         mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1, geneticMap.bc=geneticMap.bc,
                                   names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),
                                   breed.infos=breed.infos, approach=approach.sel)
         ##### 3. module hotspot - tabpanel -bc
         mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.infos=breed.infos,
                               names.files,approach=approach.sel)
         ##### 4. module genetic function - tabpanel -bc
         mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.infos=breed.infos,
                         names.files,make.traff.light.bc=reactive(make.tl.bc),approach=approach.sel)
       }
     }
  })

  ### 2. Chromosome selection - BC
  shiny::observeEvent(input$chromosome1,{
    req(input$chromosome1)

    if(input$chromosome1!="" && is.null(approach.sel)!=TRUE && nrow(approach.sel)>0)
    {
        ## take out when Holstein-DE is included
        if(stop2==0 && stop1==0){

          shiny::showTab(inputId = "navbar2","General information")
          shiny::showTab(inputId = "navbar2","Genetic map")
          shiny::showTab(inputId = "navbar2","Hotspot detection")
          shiny::showTab(inputId = "navbar2","Genetic-map functions")

          ##### 1. module general - tabpanel - bc
          mod_bc_general_server("bc_general_1",filter=input$chromosome1, geneticMap.bc=geneticMap.bc,
                             names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel$Abbreviation,
                             dt=geneticMap_summary[match(names.bc,names(geneticMap_summary))])
          ##### 2. module genetic map - tabpanel -bc
          mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1,  geneticMap.bc=geneticMap.bc,
                                  names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel)
          ##### 3. module hotspot - tabpanel -bc
          mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.infos=breed.infos,
                             names.files,approach=approach.sel)
          ##### 4. module genetic function - tabpanel -bc
          mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.infos=breed.infos,
                                      names.files,make.traff.light.bc=reactive(make.tl.bc),approach=approach.sel)
        }
      }else{
        shiny::hideTab(inputId = "navbar2","General information")
        shiny::hideTab(inputId = "navbar2","Genetic map")
        shiny::hideTab(inputId = "navbar2","Hotspot detection")
        shiny::hideTab(inputId = "navbar2","Genetic-map functions")
    }
})

  ### 3. Approach selection - BC
  shiny::observeEvent(input$approachselection_bc,{
      req(input$approachselection_bc)

      approach.sel<<-approach.table[match(input$approachselection_bc,approach.table$Approach),]

      ### take out when Holstein-DE included
      breed.selected<<-input$breed1

      ### stop - exclude part when HMM for HOL-DE is available
      stop2<<-0
      if(length(grep("Holstein-DE",breed.selected)!=0) && length(grep("HMM",input$approachselection_bc))!=0)
      {
         if(length(breed.selected)==2)
         {
           shinyjs::show(id="notworking3")
           stop2<<-1
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

         geneticMap.ll=list()
         geneticMap.ll = mapply(function(x) {
           load(system.file("extdata", paste0(x, "/geneticMap.Rdata"), package = "CLARITY"))
           geneticMap
         }, names.bc, SIMPLIFY = FALSE)

         breed.infos<<-breed.table[match(breed.selected,breed.table$Name),]
         geneticMap.bc <<-geneticMap.ll

       }

      if(is.null(input$chromosome1)!=TRUE && input$chromosome1!="" && stop2==0)
      {
        shiny::showTab(inputId = "navbar2","General information")
        shiny::showTab(inputId = "navbar2","Genetic map")
        shiny::showTab(inputId = "navbar2","Hotspot detection")
        shiny::showTab(inputId = "navbar2","Genetic-map functions")

        ##### 1. module general - tabpanel - bc
        mod_bc_general_server("bc_general_1",filter=input$chromosome1, geneticMap.bc=geneticMap.bc,
                              names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel$Abbreviation,
                              dt=geneticMap_summary[match(names.bc,names(geneticMap_summary))])
        ##### 2. module genetic map - tabpanel -bc
        mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1, geneticMap.bc=geneticMap.bc,
                                  names.files=names.files,make.traff.light.bc=reactive(make.tl.bc),breed.infos=breed.infos, approach=approach.sel)
        ##### 3. module hotspot - tabpanel -bc
        mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.infos=breed.infos,
                              names.files,approach=approach.sel)
        ##### 4. module genetic function - tabpanel -bc
        mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.infos=breed.infos,
                                       names.files,make.traff.light.bc=reactive(make.tl.bc),approach=approach.sel)
      }else{
        shiny::hideTab(inputId = "navbar2","General information")
        shiny::hideTab(inputId = "navbar2","Genetic map")
        shiny::hideTab(inputId = "navbar2","Hotspot detection")
        shiny::hideTab(inputId = "navbar2","Genetic-map functions")
      }
  })
}
