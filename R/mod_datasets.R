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
      shinydashboard::box(title =tags$b(tags$b("The German Holstein data set (Holstein-DE)")),status="danger",width=12,collapsible=F,solidHeader = TRUE,
          tags$h4(HTML("Data were provided by the German Evaluation Center <a href = 'https://www.vit.de' target='_blank'> <u>vit</u></a> (IT solutions for animal production). Permission for data access was granted by the
                   <a href='https://www.fbf-forschung.de/' target='_blank'> <u>Association for Bioeconomy Research</u></a> (FBF, Bonn) as representative of German cattle breeders. Genotyping was performed with the Illumina Bovine SNP50 genotype array.")),
            tags$h4(tags$span(htmltools::tags$a(href="#info","After preprocessing", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline;'),
                              ", the dataset includes 367,056 genotyped German Holstein cattle belonging to 1,052 half-sib families.",.noWS=c("after-begin", "before-end")),
            htmltools::tags$h4("Recombination rates were estimated based on 44,631 SNPs. ", htmltools::tags$a(href="#", "Likelihood-based", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline')," and ",
                                          htmltools::tags$a(href="#","deterministic", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline'), " approach considered a subset of 366,544 genotyped progeny across 876 half-sib families
                                        with sires having more than 30 progeny.")),
      ),
      htmltools::br(),htmltools::br(),
      shinydashboard::box(title =tags$b(tags$b("The German/Austrian Fleckvieh data set (Fleckvieh)")),status="danger",width=12,collapsible=F,solidHeader = TRUE,
                        htmltools::tags$h4(htmltools::HTML("Data were provided by <a href = 'https://www.rinderzucht.at/zuchtdata.html' target='_blank'><u>ZuchtData</u></a> (Vienna, Austria) with
                          permission granted by the representatives of cattle breeders. Genotyping was performed with the Illumina Bovine SNP50 genotype array. ")),
                    #    htmltools::br(),
                        tags$h4(tags$span(htmltools::tags$a(href="#info","After preprocessing", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline;'),
                        ", the data set included 298,761 genotyped Fleckvieh cattle belonging to 6,865 half-sib families.",
                        .noWS=c("after-begin", "before-end"))),
                      #  htmltools::br(),
                       htmltools::tags$h4(tags$span("Recombination rates were estimated based on 40,003 SNPs. ", htmltools::tags$a(href="#", "Likelihood-based", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline')," and ",
                                                    htmltools::tags$a(href="#","deterministic", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline'), " approach considered a subset of 270,548 genotyped progeny
                                                    across 1,578 half-sib families with sires having a minimum number of 30 progeny." ))
    ),
    htmltools::br(),htmltools::br(),
    shinydashboard::box(title =tags$b(tags$b("The Swiss cattle breeds")),status="danger",width=12,collapsible=F,solidHeader = TRUE,
                        htmltools::tags$h4(htmltools::HTML("Data of six cattle breeds were provided by <a href = 'https://qualitasag.ch/' target='_blank'><u> Qualitas AG </u></a> (Zug, Switzerland)
                                                           with permission granted by the representatives of Swiss cattle breeders. Genotyping was performed with various genotype arrays and
                                                           marker densities ranging from 6,607 to 125,839 SNPs. A SNP sub-panel being similar to the Illumina Bovine SNP50 array was selected for further investigations.")),
                        htmltools::br(),
                        tags$h4(tags$span(htmltools::tags$a(href="#info","After preprocessing", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline;'),
                                          ", the datasets included",htmltools::HTML("<em> n<sub>1</sub> </em> genotyped cattle belonging to  <em> N<sub>1</sub> </em> half-sib families.",.noWS=c("after-begin", "before-end")))),

                        htmltools::tags$h4(tags$span(htmltools::HTML("Recombination rates were estimated based on <em>p</em> SNPs. "),  htmltools::tags$a(href="#", "Likelihood-based", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline')," and ",
                                                     htmltools::tags$a(href="#","deterministic", onclick = "openTab('methodology')",.noWS = "outside", style='text-decoration-line: underline'), htmltools::HTML(" approach considered
                                                     <em> n<sub>2</sub></em> genotyped progeny across <em>N<sub>2</sub></em> half-sib families with sires having a minimum number of 30 progeny."))),
                        htmltools::br(),
                        shiny:: fluidRow(
                          htmltools::br(),htmltools::br(),
                          shiny::column(width=12,DT::dataTableOutput(ns("tableSwissData"),width="50%",height="auto"),style = "height:auto; overflow-y: auto;overflow-x: auto;")
                        ),
                        shiny::fluidRow(
                          shiny::column(width=10,shiny::checkboxInput(ns("checkbox2"), "Show/hide legend", FALSE),
                                      htmltools::p(id = "element2",
                                                   htmltools::HTML("n<sub>1</sub>:"), "Number of genotyped animals ",htmltools::br(),
                                                   htmltools::HTML("N<sub>1</sub>:"), "Number of half-sib families",htmltools::br(),
                                                   htmltools::HTML("n<sub>2</sub>:"), "Number of genotyped animals in half-sib families with at least 30 progeny",htmltools::br(),
                                                   htmltools::HTML("N<sub>2</sub>:"), "Half-sib families with sires having at least 30 progenies",htmltools::br(),
                                                                    "p: Number of SNPs",htmltools::br()
                                                   ))
                        )




    )
  )
 )
}

# Module Server
#' @rdname mod_datasets
#'
#' @export
#' @keywords internal

mod_datasets_server=function(id){
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

    swissBreeds=c("Holstein-CH","BrownSwiss", "Braunvieh", "Simmental","Limousin","Angus")
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
  })
}

## To be copied in the UI
# mod_datasets_ui("mod_datasets_1")

## To be copied in the server
# mod_datasets_server("mod_datasets_1")
