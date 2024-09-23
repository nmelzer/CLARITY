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
  #### setting colors for breed analysis for deterministic and likelihood-based approach
  color.breed.dl<-c("dodgerblue2","cadetblue3")

  #### setting colors and shapes for breed comparison
  color.shape <-as.data.frame(cbind(c("#a6cee3","#1f78b4","#b2df8a"),c("#c7c7c7","#616161","#333333"),
                                    c(15,17,18),c(15,17,18),c(100,110,120),c(1,10,20),c("#FF8C00","#eeb422","#ffa07a")))
  colnames(color.shape)<-c("color1","color2","shape1","shape2","ord1","ord2","color.table")
  color.shape.def <-as.data.frame(color.shape)

  ###################################################################################################################################
  #### provide breeds
  breed<-c("Holstein-CH","Holstein-DE","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin","Unifying-CH")

  #### provide breeds
  no.chr<-c("","All",paste(1:29))

  ###################################################################################################################################
  ##### Default settings for buttons

  shinyjs::hide(id="backtoanalysis")
  shinyjs::hide(id="backtobc")
  shinyjs::hide(id="back_infodatasets")

  ###################################################################################################################################
  ##### Initialize (see Everything that belongs to the linking of tabs within the app)

  remember.prev.tab = reactiveValues()
  remember = reactiveValues()

  ###################################################################################################################################
  ##### Default settings for breed analysis page - all corresponding tabpanels are completely hidden when starting the app

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
  make.tl <- make.tl.bc <- NULL

  BreedOverview<-NULL
  load(system.file("extdata","general/OverviewBreeds.Rdata",package="CLARITY")) ## contains table BreedOverview
  BreedOverview=cbind(BreedOverview, round(BreedOverview$N2/100)*100,ceiling((BreedOverview$n2/BreedOverview$N2)/100)*100)
  colnames(BreedOverview)[7:8]=c("N2.new","Averag.n2")

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

  ###################################################################################################################################
  ##### All belonging to the part 'Breed analysis' - SidebarMenu
  ###################################################################################################################################

  shinyjs::hide(id="chromosome")

  shiny::observeEvent(input$breed,{
    if(input$breed==""){
      y<-character(0)
    }
    if(input$breed!="")
    {
      req(input$breed)
      breed.selected <- input$breed

      shinyjs::show(id="chromosome")
      shiny::updateSelectInput(session, 'breed', label = "Breed", choices = breed ,selected = input$breed )

      dat.tl=BreedOverview[match(input$breed,BreedOverview$Breed),]
      make.tl <<- make_traffic_light(selected.breed=input$breed,dat.tl=dat.tl)

      if(input$chromosome=="")
      {
        shiny::updateSelectInput(session,'chromosome',label="Chromosome",choices=no.chr,selected="")

        shiny::hideTab(inputId = "navbar","General information")
        shiny::hideTab(inputId = "navbar","Genetic map")
        shiny::hideTab(inputId = "navbar","Hotspot detection")
        shiny::hideTab(inputId = "navbar","Genetic-map functions")
      }
      else
      {
        ##### 1. module general - tabpanel
        mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed,color.breed.dl=color.breed.dl, make.traff.light=reactive(make.tl))

        ##### 2. module genetic map - tabpanel
        geneticMap<-NULL
        load(system.file("extdata",paste0(input$breed,"/geneticMap.Rdata"),package="CLARITY"))
        if(class(geneticMap[,4])=="numeric")geneticMap[,4]=as.integer(round(geneticMap[,4],0))
        mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap,color.breed.dl=color.breed.dl,make.traff.light=reactive(make.tl))

        ###### 3. module hotspot - tabpanel
        mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,color.shape.def=color.shape.def)

        ###### 4. module genetic function - tabpanel
        mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed,make.traff.light=reactive(make.tl))
      }
    }
  })

  ##### 2. Chromosome selection  #####
  shiny::observeEvent(input$chromosome,
  {
    if(input$chromosome==""){
      y<-character(0)
    }
    if(input$chromosome!=""){
      req(input$chromosome)

      ## activate displaying the tabpanels for the Breed analyis
      shiny::showTab(inputId = "navbar","General information")
      shiny::showTab(inputId = "navbar","Genetic map")
      shiny::showTab(inputId = "navbar","Hotspot detection")
      shiny::showTab(inputId = "navbar","Genetic-map functions")

      ##### 1. module general - tabpanel
      mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed,color.breed.dl=color.breed.dl,make.traff.light=reactive(make.tl))

      ##### 2. module genetic map - tabpanel
      geneticMap<-NULL
      load(system.file("extdata",paste0(input$breed,"/geneticMap.Rdata"),package="CLARITY"))
      if(class(geneticMap[,4])=="numeric")geneticMap[,4]=as.integer(round(geneticMap[,4],0))
      mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap,color.breed.dl=color.breed.dl,make.traff.light=reactive(make.tl))

      ###### 3. module hotspot - tabpanel
      mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,color.shape.def=color.shape.def)

      ###### 4. module genetic function - tabpanel
      mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed,make.traff.light=reactive(make.tl))
    }
  })

  ###################################################################################################################################
  ##### All belonging to the part 'Breed comparison' - SidebarMenu
  ###################################################################################################################################
  shinyjs::hide(id="chromosome1")
  breed.names.short=matrix(c("Holstein-CH","Holstein-DE","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin","Unifying-CH","HC","HD","F","BS","B","A","S","L","UC"),nrow=2,byrow=T,ncol=9)

  observeEvent(input$breed1,{

    #store previous current selected breed as last selected breed reactive values
    remember$last = remember$current ##
    remember$current = input$breed1

    if(input$breed1[1]=="" || length(input$breed1)<=1)
    {
        req(input$breed1)
        y<-character(0)

        shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")

        shinyjs::hide(id="chromosome1")
        shiny::hideTab(inputId = "navbar2","General information")
        shiny::hideTab(inputId = "navbar2","Genetic map")
        shiny::hideTab(inputId = "navbar2","Hotspot detection")
        shiny::hideTab(inputId = "navbar2","Genetic-map functions")

    }
    ## Important when three breeds were selected and all were removed to start new selection. Make sure that the app does not hang up.
    if(length(input$breed1)<3 && length(input$breed1)>1 && is.null(remember$last)==TRUE)
    {
      shiny::updateSelectInput(session,'breed1',label="Select breeds",choices=breed,selected="")
      shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")

      shinyjs::hide(id="chromosome1")
      shiny::hideTab(inputId = "navbar2","General information")
      shiny::hideTab(inputId = "navbar2","Genetic map")
      shiny::hideTab(inputId = "navbar2","Hotspot detection")
      shiny::hideTab(inputId = "navbar2","Genetic-map functions")
    }
    ## Only three breeds allowed to be selected for the breed comparison
    if(length(input$breed1)>1 && length(input$breed1)<4 && is.null(remember$last)==FALSE)
    {
      req(input$breed1)

      shinyjs::show(id="chromosome1")
      names.bc <- sort(input$breed1)

      ## prepare data for traffic light and make the plot
      dat.tl=BreedOverview[match(names.bc,BreedOverview$Breed),]
      make.tl.bc<<-make_traffic_light(selected.breed=names.bc,dat.tl=dat.tl)

      ## update the input for the breeds
      if(length(input$breed1)==3){
        shiny::updateSelectInput(session,'breed1',label="Select breeds",choices=input$breed1,selected=input$breed1)
      }
      else{
        shiny::updateSelectInput(session,'breed1',label="Select breeds",choices=breed,selected=input$breed1)
      }
      ## if an chromosome selected than prepare all for the selected new breeds and selected chromosome
      if(input$chromosome1!="")
      {
        req(input$chromosome1)

        ##### 2. module genetic map - tabpanel
        geneticMap.ll=list()
        names.bc.venn1=c()

        for(ij in 1:length(names.bc))
        {
          load(system.file("extdata",paste0(names.bc[ij],"/geneticMap.Rdata"),package="CLARITY"))
          if(class(geneticMap[,4])=="numeric")geneticMap[,4]=as.integer(round(geneticMap[,4],0))
          geneticMap.ll[[names.bc[ij]]]=geneticMap
        }

        use.breed=match(names.bc,breed.names.short[1,])
        names.bc.venn1=breed.names.short[2,match(names.bc,breed.names.short[1,])] ## new insert 11.04.2023

        names.bc.venn <-names.bc.venn1
        geneticMap.bc <-geneticMap.ll
        names.files<-paste(names.bc,collapse="-")
        names.bc.venn <-names.bc.venn1
        names.files<-paste(names.bc,collapse="-")

        ###### 2. module general - tabpanel
        mod_bc_general_server("bc_general_1",filter=input$chromosome1,breed.select.bc=names.bc,geneticMap.bc=geneticMap.ll,
                               color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files,make.traff.light.bc=reactive(make.tl.bc))

        ###### 2. module genetic map - tabpanel
        mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1,breed.select.bc=names.bc, geneticMap.bc=geneticMap.ll,
                                   color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files,make.traff.light.bc=reactive(make.tl.bc))

        ###### 3. module hotspot - tabpanel
        mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,
                               names.bc.venn=names.bc.venn,names.files)

        ###### 4. module genetic function - tabpanel
        mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,
                                        names.files,make.traff.light.bc=reactive(make.tl.bc))
      }
   }
  },ignoreNULL = FALSE)

  shiny::observeEvent(input$chromosome1,{
      if(input$chromosome1!="")
      {
        req(input$chromosome1)
        y <- input$chromsome1

        names.bc <- sort(input$breed1)

        shiny::showTab(inputId = "navbar2","General information")
        shiny::showTab(inputId = "navbar2","Genetic map")
        shiny::showTab(inputId = "navbar2","Hotspot detection")
        shiny::showTab(inputId = "navbar2","Genetic-map functions")

        geneticMap.ll=list()
        names.bc.venn1=c()

        for(ij in 1:length(names.bc))
        {
            load(system.file("extdata",paste0(names.bc[ij],"/geneticMap.Rdata"),package="CLARITY"))
            if(class(geneticMap[,4])=="numeric")geneticMap[,4]=as.integer(round(geneticMap[,4],0))
            geneticMap.ll[[names.bc[ij]]]=geneticMap
        }

        names.bc.venn1=breed.names.short[2,match(names.bc,breed.names.short[1,])]
        names.bc.venn <-names.bc.venn1

        names.files<-paste(names.bc,collapse="-")

        mod_bc_general_server("bc_general_1",filter=input$chromosome1,breed.select.bc=names.bc, geneticMap.bc=geneticMap.ll,
                              color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files,make.traff.light.bc=reactive(make.tl.bc))

        mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1, breed.select.bc=names.bc, geneticMap.bc=geneticMap.ll,
                                  color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files,make.traff.light.bc=reactive(make.tl.bc))

        mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,
                              names.bc.venn=names.bc.venn,names.files)

        mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.select.bc=names.bc,
                                       color.shape.def=color.shape.def,names.files,make.traff.light.bc=reactive(make.tl.bc))
      }
  })
}
