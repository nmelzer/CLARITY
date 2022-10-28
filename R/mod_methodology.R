# Module UI

#' @title mod_methodology_ui and mod_methodology_server
#' @description  A shiny module containing all relevant information about the methodology or the terminology of the methods mentioned within the app.
#'
#' @param id module id
#'
#' @rdname mod_methodology
#'
#' @keywords internal, methodology
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @export

mod_methodology_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      htmltools::br(),htmltools::br(),
      shinydashboard::box(title=tags$b(tags$h3("Description of approaches")), status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,
            shiny::fluidRow(shiny::column(width=12,DT::dataTableOutput(ns("table1"),width="auto",height="auto")))
      ),
      htmltools::br(),htmltools::br(),
      shiny::fluidRow(id="backtoanalysis",
             shiny::column(5,""),
             shiny::column(5,htmltools::tags$a(href="#",htmltools::h3("Back to analyses"), onclick = "openTab('single')")) #
      ),
      fluidRow(id="backtobc",
             shiny::column(5,""),
             shiny::column(5,htmltools::tags$a(href="#",htmltools::h3("Back to breed comparison"), onclick = paste0("openTab('breedcomparison')")))
             #
      )
  ))
}

# Module Server
#' @rdname mod_methodology
#' @export

mod_methodology_server=function(id){
  shiny::moduleServer( id, function(input, output, session){

    ## took out links betweeen pages - does not work, when I upload the app to R shiny
    #,htmltools::tags$a(href="#",'data sets', onclick = "openTab('infodatasets')")
    ## creating output-table
    table.methodology=as.data.frame(rbind(
                                c(paste0(htmltools::tags$h4(htmltools::tags$b(htmltools::tags$em("Preprocessing")))),""),
                                c( paste0(htmltools::tags$h4(htmltools::HTML("The data sets have been preprocessed with the same workflow: After removing putatively misplaced markers, genotype data were filtered for minor allele frequency > 0.01
                                    and for Mendelian inconsistencies  both on marker and individual level using PLINK v1.9  <a href = 'https://doi.org/10.1086/519795' target='_blank' >(Purcell et al., 2007)</a> with recommended settings.
                                    Genotypes with a Mendelian inheritance error were set to"), em("'NA'"),HTML(" and missing values were imputed using Eagle v2.4.1 program <a href='https://doi.org/10.1038/ng.3679 ' target='_blank' >(Loh et al., 2016)</a>.
                                Please, see for a detailed description <a href = 'HTTPs://doi.org/10.1186/s12711-020-00593-z'>Qanbari and Wittenburg (2020)</a>."))),""),

                                c("",""),
                                c(paste0(htmltools::tags$h4(htmltools::tags$b(htmltools::tags$em("Approaches to estimate recombination frequencies")))),""),
                                 c(paste0(tags$h4("Deterministic approach")),paste0(htmltools::tags$h4(htmltools::HTML("Recombination rate between <em> adjacent </em> markers was estimated according to
                                   <a href='https://doi.org/10.1186/1297-9686-46-11' target='_blank' >Ferdosi et al. (2014a)</a>. The approach was available as R package
                                   <a href='https://cran.r-project.org/web/packages/hsphase/index.html' target='_blank'>hsphase</a> (<a href='https://doi.org/10.1186/1471-2105-15-172'target='_blank' > Ferdosi et al., 2014b</a>).
                                   Estimates of recombination rate were directly converted into genetic distances in Morgan units.")))),

                                 c(paste0(htmltools::tags$h4("Likelihood-based approach")),paste0(htmltools::tags$h4(htmltools::HTML("Recombination rate was estimated between <em>all</em> intra-chromosomal marker pairs using a likelihood-based approach
                                   (<a href='https://doi.org/10.1093/genetics/191.1.NP'  target='_blank' >Gomez-Raya, 2012</a>,
                                   <a href='https://doi.org/10.1186/2049-1891-4-30'  target='_blank' >Gomez-Raya et al., 2013</a> and <a href='https://doi.org/10.3389/fgene.2018.00186'  target='_blank' >Hampel et al., 2018</a>).
                                    Afterwards, genetic distances between adjacent markers were derived from a quadratic optimization approach <a href = 'https://doi.org/10.1186/s12711-020-00593-z' target='_blank' > (Qanbari and Wittenburg 2020)</a>.
                                    This procedure was implemented as R package <a href='https://cran.r-project.org/web/packages/hsrecombi/index.html' target='_blank' >hsrecombi</a>. <br> The approach was also used to reveal
                                    putatively misplaced markers which are provided under 'Misplaced markers'"), htmltools::HTML("<a href = 'https://doi.org/10.1111/age.13205' target='_blank' > (Qanbari et al., 2022) </a>.
                                   SNPs with a markedly high recombination rate to neighboring markers were localized following   <a href='https://doi.org/10.3389/fgene.2018.00186' target='_blank' >Hampel et al., 2018</a>."
                              )))),
                              c("",""),
                              c(paste0(htmltools::tags$h4(htmltools::tags$b(htmltools::tags$em("Post analysis")))),""),
                              c(paste0(htmltools::tags$h4( "Hotspot region")),paste0(htmltools::tags$h4(htmltools::HTML("Hotspot regions were derived on the basis of the deterministic approach. Initially, the recombination frequency threshold
                                    from <a href='https://doi.org/10.1371/journal.pgen.1005387'target='_blank' >Ma et al. (2015)</a>
                                    was used to identify hotspot regions (i.e., a region with a recombination rate exceeding 2.5 standard deviations
                                                                               (SD) from the genome-wide average of recombination rates). The threshold in SD units can be adjusted.")))),

                              c(paste0(htmltools::tags$h4("Genetic-map functions")),paste0(htmltools::tags$h4(htmltools::HTML("Various genetic-map functions have been fitted through smoothing the relationship
                                                                                                   between genetic map distances and recombination rate: <br>")),
                                                                                           htmltools::tags$h4(HTML("Haldane JBS. The combination of linkage values, and the calculation of distances between the loci of linked factors. J Genet. 1919;8:299-309 <br>")),
                                                                                           htmltools::tags$h4(htmltools::tags$a(href="https://doi.org/10.1159/000152856","Rao et al., 1977", target="_blank")),
                                                                                           htmltools::tags$h4(htmltools::tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1216865/","Felsenstein 1979", target="_blank")),
                                                                                           htmltools::tags$h4(htmltools::tags$a(href="https://doi.org/10.1016/0040-5809(84)90013-3","Liberman & Karlin 1984", target="_blank"))
                                                                                        ))
    ))

    ## adopted from https://stackoverflow.com/questions/56533474/r-how-to-remove-the-horizontal-line-between-the-header-and-the-body-in-a-dtd
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )

    jsc <- 'function(settings, json) {
      $("td:contains(\'Preprocessing\')").attr("colspan", "2").css("text-align", "left");
      $("tbody > tr:first-child > td:empty").remove();
      $("td:contains(\'and for Mendelian inconsistencies  both on marker and individual level using PLINK v1.9\')").attr("colspan", "2").css("text-align", "left");
      $("tbody > tr:first-child > td:empty").remove();
      $("td:contains(\'Approaches to estimate recombination frequencies\')").attr("colspan", "2").css("text-align", "left");
      $("tbody > tr:first-child > td:empty").remove();
      $("td:contains(\'Post analysis\')").attr("colspan", "2").css("text-align", "left");
      $("tbody > tr:first-child > td:empty").remove();

      var table = settings.oInstance.api();
      $(table.table().node()).removeClass("no-footer");

   }'

    ## Table header with internal link to methodology
    thead<-tr<-th<-NULL
    sketch2 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=2,paste0(tags$h4(tags$b(tags$em("Approaches to estimate recombination frequencies")))))
    ))))


    output$table1<-DT::renderDataTable(
      DT::datatable(table.methodology, colnames = rep("", ncol(table.methodology)), escape=FALSE,filter="none",  class = 'stripe',selection = "none",rownames=FALSE,
                    options = list(columnDefs = list(list(width = '200px', targets = c(0), className = 'dt-body-left')),
                    searching = FALSE,ordering=FALSE,dom="",pagelength = 11,lengthMenu = list(-1,"ALL"),
                    headerCallback = DT::JS(headerCallback),initComplete =DT::JS(jsc)))%>% DT::formatStyle(names(table.methodology),background = 'white')%>%DT::formatStyle(1:2, 'vertical-align'='top')%>%DT::formatStyle(1:2, 'text-align' = 'left')
    )
  })
}

## To be copied in the UI
# mod_methodology_ui("mod_methodology_1")

## To be copied in the server
# mod_methodology_server("mod_contact_1")

