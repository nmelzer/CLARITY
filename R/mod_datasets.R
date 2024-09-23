# Module UI

#' @title mod_datasets_ui and mod_datasets_server
#' @description  A shiny module containing the relevant information about the data sets and their external preprocessing.
#'
#' @param id module id
#'
#' @rdname mod_datasets
#'
#' @keywords internal, data set
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @export

mod_datasets_ui <- function(id){
  ns <-  shiny::NS(id)

 shiny::tagList(
    shiny::fluidRow(
      htmltools::br(),htmltools::br(),
      shinydashboard::box(title =tags$b(tags$b("The German Holstein data set")),status="danger",width=12,collapsible=F,solidHeader = TRUE,
          tags$h4(HTML("Data were provided by the German Evaluation Center <a href = 'https://www.vit.de' target='_blank'> <u>vit</u></a> (IT solutions for animal production). Permission for data access was granted by the
                   <a href='https://www.fbf-forschung.de/' target='_blank'> <u>Association for Bioeconomy Research</u></a> (FBF, Bonn) as representative of German cattle breeders.")),
            htmltools::br(),
            tags$h4("This study used a large pedigree including 367,056 German Holstein cattle (i.e., 1053 half-sib families with sires born between 1979-2017).",
            HTML("Data included genotypes of the Illumina Bovine SNP50 genotype array mapped to the coordinates of the <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank'> <u>ARS-UCD1.2 assembly</u></a>.")),
            htmltools::br(),
            htmltools::tags$h4(tags$span(tags$a(href="#info","After preprocessing", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline;'),", recombination rates were eventually estimated based on 44,631 SNPs and across 876 half-sib families
               with sires having a minimum number of 39 progeny.",.noWS=c("after-begin", "before-end"))),

      ),
      htmltools::br(),htmltools::br(),
      shinydashboard::box(title =tags$b(tags$b("The German/Austrian Fleckvieh data set")),status="danger",width=12,collapsible=F,solidHeader = TRUE,
                        htmltools::tags$h4(htmltools::HTML("Data were provided by <a href = 'https://www.rinderzucht.at/zuchtdata.html' target='_blank'><u>ZuchtData</u></a> (Vienna, Austria) with
                          permission granted by the representatives of cattle breeders.")),
                        htmltools::br(),
                        htmltools::tags$h4(htmltools::HTML("The data set included 298,850 genotyped Fleckvieh cattle belonging to 6,866 half-sib families. Genotyping was performed with the Illumina Bovine SNP50
                          genotype array. Genotype data were provided for 40,144 SNPs and physical coordinates were mapped to the
                          <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank'><u>ARS-UCD1.2 assembly</u></a>.")),
                        htmltools::br(),
                       htmltools::tags$h4(tags$span(tags$a(href="#info","After preprocessing", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline;'),",
                                                    recombination rates were estimated based on 40,003 SNPs and across 1,577 half-sib families with sires having a minimum number of 30 progeny.",
                                                    .noWS=c("after-begin", "before-end")))


    ),
    htmltools::br(),htmltools::br(),
    shinydashboard::box(title =tags$b(tags$b("The Swiss cattle breeds")),status="danger",width=12,collapsible=F,solidHeader = TRUE,
                        htmltools::tags$h4(htmltools::HTML("Data of six cattle breeds were provided by <a href = 'https://qualitasag.ch/' target='_blank'><u> Qualitas AG </u></a> (Zug, Switzerland)
                                                           with permission granted by the representatives of Swiss cattle breeders.")),
                        htmltools::br(),
                        htmltools::tags$h4(htmltools::HTML(" The data set included <em> n<sub>1</sub> </em> genotyped cattle belonging to <em> N<sub>1</sub> </em>")," half-sib families.
                        Genotyping was performed with various genotype arrays and marker densities. Imputed genotype data were provided for about 120K SNPs and 90K SNPs for dairy/dual-purpose and beef breeds, respectively.
                        Physical coordinates were mapped to the", htmltools::HTML(" <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank' ><u>ARS-UCD1.2 assembly</u></a>.")),
                        htmltools::br(),
                        htmltools::tags$h4(htmltools::HTML("The dataset <q>Unifying-CH</q> comprises all Swiss cattle breeds plus some small cattle populations not considered separately
                                                            with genotype data merged before imputation.")),
                        htmltools::br(),
                        htmltools::tags$h4(tags$span(tags$a(href="#info","After preprocessing", onclick = "openTab('methodology')",.noWS="outside", style='text-decoration-line: underline;'),", recombination rates
                                                     were estimated for a subset of",.noWS=c("after-begin", "before-end")),htmltools::HTML("<em> p </em> SNPs, which is similar to the Illumina 50K array, and across <em> N<sub>2</sub> </em> half-sib families with sires having a minimum number of 30 progeny.")),
                        htmltools::br(),
                        shiny:: fluidRow(
                          htmltools::br(),htmltools::br(),
                          shiny::column(width=8,DT::dataTableOutput(ns("tableSwissData"),width="auto",height="auto"),style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
                        ),
                        shiny::fluidRow(
                          shiny::column(width=10,shiny::checkboxInput(ns("checkbox2"), "Show/hide legend", FALSE),
                                      htmltools::p(id = "element2",
                                                   htmltools::HTML("n<sub>1</sub>:"), "number of genotyped animals ",htmltools::br(),
                                                   htmltools::HTML("N<sub>1</sub>:"), "number of half-sib families",htmltools::br(),
                                                   htmltools::HTML("n<sub>2</sub>:"), "number of genotyped animals in half-sib families with at least 30 progeny",htmltools::br(),
                                                   htmltools::HTML("N<sub>2</sub>:"), "half-sib families with sires having at least 30 progenies",htmltools::br(),
                                                                    "p: number of SNPs",htmltools::br()
                                                   ))
                        )




    )
  )
 )
}

# Module Server
#' @rdname mod_datasets
#' @export
#' @keywords internal

mod_datasets_server=function(id, BreedOverview){
  shiny::moduleServer(id,function(input, output, session){

    thead<-tr<-th<-NULL
    sketch2 = htmltools::withTags(
      table(
        class = 'display',
        thead(
          tr(
            th(colspan=1,"Breed"),
            th(colspan=1,htmltools::HTML("n<sub>1</sub>")),
            th(colspan=1,htmltools::HTML("N<sub>1</sub>")),
            th(colspan=1,htmltools::HTML("n<sub>2</sub>")),
            th(colspan=1,htmltools::HTML("N<sub>2</sub>")),
            th(colspan=1,"p"),
          )
        )
      )
    )

    swissBreeds=c("Holstein-CH","BrownSwiss", "Braunvieh", "Simmental","Limousin","Angus","Unifying-CH") ## changed 22.08.2023
    data2=BreedOverview[match(swissBreeds,BreedOverview$Breed),]

    output$tableSwissData=DT::renderDataTable({
      DT::datatable(
        data2[,-c(ncol(data2)-1,ncol(data2))], extensions = c("Buttons"), container=sketch2,rownames=FALSE,options = list(searching=FALSE,
                                                                                                columnDefs = list(list(className = 'dt-left', targets = "_all")),dom = 'Bt',
                                                                                                buttons = list(list(extend='csv',title="General_information_SwissData"),
                                                                                                               list(extend='excel',title="General_information_SwissData"))
                                                                                               ))
    },server=FALSE)

    ## show / hide legend
    shiny::observe({
      shinyjs::toggle(id = "element2", condition = input$checkbox2,asis=TRUE)
    })

    output$try <- renderUI("")
  })
}

## To be copied in the UI
# mod_datasets_ui("mod_datasets_1")

## To be copied in the server
# mod_datasets_server("mod_datasets_1")
