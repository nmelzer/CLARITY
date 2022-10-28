#' The application server-side
#'
#' @title App_server
#' @description The application server-side.
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom cachem cache_disk
#' @rawNamespace import(shinyjs, except = runExample)
#' @export
#'
#'
app_server <- function( input, output, session) {

  ###############################################################################################################################
  ##### Default settings for specific buttons

  shinyjs::hide(id="backtoanalysis") ## hide Link "back to Analysis" within the bibliography module - it will be made visible after a chromosome was selected

  shiny::shinyOptions(cache = cachem::cache_disk("./app_cache/cache/")) ## enabling chaching - also working on shinyio
  session$onSessionEnded(stopApp) ## Automatically stop a Shiny app when closing the browser tab adopted from Dean Attali

  ################################################################################################################################
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
  ##### All belonging to the part 'Information' - SidebarMneu
  ###################################################################################################################################

  ##### 1. about the project
  mod_about_the_project_server("mod_about_the_project_1")

  ##### 2. information about dataset(s)
  mod_datasets_server("mod_datasets_1")

  ##### 3. generally misplaced markers
  mod_misplaced_server("mod_misplaced_1")

  ##### 3. bibliography module is all included for the corresponding site
  mod_methodology_server("mod_methodology_1")

  ##### 4. contact module is all included for the corresponding site
  mod_contact_server("mod_contact_1")


  ###################################################################################################################################
  ####
  ###################################################################################################################################
   shiny::observeEvent(input$tabs, {
    req(input$tabs)

    if(input$tabs=="info"){
      shinyjs::hide(id="backtobc")
      shinyjs::hide(id="backtoanalysis")
      shinyjs::hide(id="back_methodology")
    }
    if(input$tabs=="breedcomparison"){
      shinyjs::show(id="backtobc")
      shinyjs::hide(id="backtoanalysis")

      #### take out the following five lines, when more than two breeds are compared see also L180-266
      breeds=c("Holstein","Fleckvieh")
      shiny::updateSelectInput(session,'breed1',label="Select breeds",choices=breeds,selected=breeds)
      shinyjs::show(id="chromosome1")
      no.chr=c("","All",paste(1:29))
      shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")
      ##### end of taking out
    }
    if(input$tabs=="single"){
      shinyjs::hide(id="backtobc")
      shinyjs::show(id="backtoanalysis")
    }
     ## internal links within the app- tested locally it works, but not on shinyapps.io - so it was excluded
 #   if(input$tabs=="infodatasets")
#    {
#      shinyjs::hide(id="back_methodology2")
#    }
#    if(input$tabs=="misplaced_markers")
#    {
#      shinyjs::hide(id="back_methodology")
#    }
#    if(input$tabs=="methodology")
#    {
#      shinyjs::show(id="back_methodology")
#      shinyjs::show(id="back_methodology2")
#    }
  })
 ############################### definition
  ## setting colors for breed analysis for deterministic and likelihood-based approach
  color.breed.dl<-c("dodgerblue2","cadetblue3")

  ## coloring and shapes for the hotspot plots
  color.shape <-as.data.frame(cbind(c("#FF8C00","#eeb422","#ffa07a"),c("#c7c7c7","#616161","#333333"), ##c("orange","darkred","blue"), c("gray78","gray38","gray20") ## hexcode are needed
                                    c(15,17,18),c(15,17,18),c(100,110,6),c(1,10,3),c("#FF8C00","#eeb422","#ffa07a"))) ##c("orange","#FF7276","#ffa07a") bisque2","goldenrod2","lightsalmon"
  colnames(color.shape)<-c("color1","color2","shape1","shape2","ord1","ord2","color.table")
  color.shape.def <-as.data.frame(color.shape)

  ###################################################################################################################################
  ##### All belonging to the part 'Breed analysis' - SidebarMenu
  ###################################################################################################################################
  ##### 1. Extended for breeds #####
  shinyjs::hide(id="chromosome") ## show selectInput after a breed was selected

  # Creating outpanel after a breed was selected
  shiny::observeEvent(input$breed,{
    if(input$breed=="")y<-character(0)
    if(input$breed!="")
    {
      req(input$breed)
      breed.selected <- input$breed

      shinyjs::show(id="chromosome")
      no.chr=c("","All",paste(1:29))

      breed=c("Holstein","Fleckvieh")

      shiny::updateSelectInput(session, 'breed', label = "Breed", choices = breed ,selected = input$breed )

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
        mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed,color.breed.dl=color.breed.dl)

        ##### 2. module genetic map - tabpanel
        geneticMap<-NULL
        load(system.file("extdata",paste0(input$breed,"/geneticMap.Rdata"),package="CLARITY"))
        mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap,color.breed.dl=color.breed.dl)

        ###### 3. module hotspot - tabpanel
        mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,color.shape.def=color.shape.def)

        ###### 4. module genetic function - tabpanel
        mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed)
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
      y <- input$chromsome

      req(input$chromosome,input$breed)


      ## activate displaying the tabpanels for the Breed analyis
      shiny::showTab(inputId = "navbar","General information")
      shiny::showTab(inputId = "navbar","Genetic map")
      shiny::showTab(inputId = "navbar","Hotspot detection")
      shiny::showTab(inputId = "navbar","Genetic-map functions")

      ##### 1. module general - tabpanel
      mod_general_server("mod_general_1",filter=input$chromosome,breed.select=input$breed,color.breed.dl=color.breed.dl)

      ##### 2. module genetic map - tabpanel
      geneticMap<-NULL
      load(system.file("extdata",paste0(input$breed,"/geneticMap.Rdata"),package="CLARITY"))
      mod_genetic_map_server("mod_genetic_map_1", filter=input$chromosome,breed.select=input$breed, geneticMap=geneticMap,color.breed.dl=color.breed.dl)

      ###### 3. module hotspot - tabpanel
      mod_hotspot_server("mod_hotspot_1",filter=input$chromosome,breed.select=input$breed,color.shape.def=color.shape.def)

      ###### 4. module genetic function - tabpanel
      mod_genetic_function_server("mod_genetic_function_1",filter=input$chromosome,breed.select=input$breed)
    }
  })

  ### Extend here for breed comparison
  ###################################################################################################################################
  ##### All belonging to the part 'Breed comparison' - SidebarMenu
  ###################################################################################################################################

  ## make lines 181-266 working, when more than 2 breeds are possible to compare
 # shinyjs::hide(id="chromosome1")
 # call_bc=0

#  observeEvent(input$breed1,{
#    if(input$breed1=="" || length(input$breed1)<=1)
#    {
#        req(input$breed1)
#        y<-character(0)

#        if(length(input$breed1)<=1){
#          no.chr=c("","All",paste(1:29))
#          if(call_bc==0)shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")
#        }

#        if(call_bc==0)
#        {
#          shinyjs::hide(id="chromosome1")
#          shiny::hideTab(inputId = "navbar2","General information")
#          shiny::hideTab(inputId = "navbar2","Genetic map")
#          shiny::hideTab(inputId = "navbar2","Hotspot detection")
#          shiny::hideTab(inputId = "navbar2","Genetic-map functions")
#        }
#    }
#   if(length(input$breed1)>=2)
#     {
#      req(input$breed1)
#      choice <- NULL

      #breeds=c("Holstein","Fleckvieh","BrownSwiss")

#      shinyjs::show(id="chromosome1")

 #     names.bc <- sort(input$breed1)


 #     if(input$chromosome1=="" )
 #     {
#        no.chr=c("","All",paste(1:29))
#        shiny::updateSelectInput(session,'chromosome1',label="Chromosome",choices=no.chr,selected="")
#        if(call_bc==0){
#          shiny::hideTab(inputId = "navbar2","General information")
#          shiny::hideTab(inputId = "navbar2","Genetic map")
#          shiny::hideTab(inputId = "navbar2","Hotspot detection")
#          shiny::hideTab(inputId = "navbar2","Genetic-map functions")
#        }
#      }

 #     if(input$chromosome1!="")
#      {
#        req(input$chromosome1)

        ##### 2. module genetic map - tabpanel

#        geneticMap.ll=list()
#        names.bc.venn1=c()

 #       for(ij in 1:length(names.bc))
 #       {
#          load(system.file("extdata",paste0(names.bc[ij],"/geneticMap.Rdata"),package="CLARITY"))
#          geneticMap.ll[[names.bc[ij]]]=geneticMap
#          names.bc.venn1=c(names.bc.venn1,substring(names.bc[ij],1,1))
 #       }

#        names.bc.venn <-names.bc.venn1
#        geneticMap.bc <-geneticMap.ll
#        names.files<-paste(names.bc,collapse="-")
#        names.bc.venn <-names.bc.venn1
#        names.files<-paste(names.bc,collapse="-")

        ###### 2. module general - tabpanel
#        mod_bc_general_server("bc_general_1",filter=input$chromosome1,breed.select.bc=names.bc,geneticMap.bc=geneticMap.ll,
#                              color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files)

        ###### 2. module genetic map - tabpanel
#        mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1,breed.select.bc=names.bc, geneticMap.bc=geneticMap.ll,
 #                                 color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files)

        ###### 3. module hotspot - tabpanel
#        mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,
#                              names.bc.venn=names.bc.venn,names.files)

        ###### 4. module genetic function - tabpanel
 #       mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,names.files)
#      }
#    }
#    })

  shiny::observeEvent(input$chromosome1,{
      if(input$chromosome1!="")
      {
         y <- input$chromsome1
        call_bc=1

        req(input$chromosome1) #,input$breed1)

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
            geneticMap.ll[[names.bc[ij]]]=geneticMap
            names.bc.venn1=c(names.bc.venn1,substring(names.bc[ij],1,1))
        }
        names.bc.venn <-names.bc.venn1
        names.files<-paste(names.bc,collapse="-")

        mod_bc_general_server("bc_general_1",filter=input$chromosome1,breed.select.bc=names.bc, geneticMap.bc=geneticMap.ll,
                              color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files)

        mod_bc_genetic_map_server("bc_genetic_map_1",filter=input$chromosome1, breed.select.bc=names.bc, geneticMap.bc=geneticMap.ll,
                                  color.shape.def=color.shape.def,names.bc.venn=names.bc.venn,names.files=names.files)
        mod_bc_hotspot_server("bc_hotspot_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,
                              names.bc.venn=names.bc.venn,names.files)
        mod_bc_genetic_function_server("bc_genetic_function_1",filter=input$chromosome1,breed.select.bc=names.bc,color.shape.def=color.shape.def,names.files)
      }
})
}
