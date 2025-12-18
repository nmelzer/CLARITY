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
             shiny::column(5,htmltools::tags$a(href="#",htmltools::h3("Back to breed analysis"), onclick = "openTab('single')", style='text-decoration-line: underline;'))
      ),
      shiny::fluidRow(id="backtobc",
             shiny::column(5,""),
             shiny::column(5,htmltools::tags$a(href="#",htmltools::h3("Back to breed comparison"), onclick = "openTab('breedcomparison')",style='text-decoration-line: underline;'))
      ),
      shiny::fluidRow(id="back_infodatasets",
               shiny::column(5,""),
               shiny::column(5,htmltools::tags$a(href="#info",htmltools::h3("Back to information about the data sets"), onclick = "openTab('infodatasets')", style='text-decoration-line: underline;'))
      )
  ))
}

# Module Server
#' @rdname mod_methodology
#' @export

mod_methodology_server=function(id){
  shiny::moduleServer( id, function(input, output, session){

    table.methodology=as.data.frame(rbind(
                                c(paste0(htmltools::tags$h4(htmltools::tags$b(htmltools::tags$em("Preprocessing")))),""),
                                c(paste0(htmltools::tags$h4(htmltools::HTML("The data sets have been preprocessed with a standardized workflow: Markers were ordered according to the <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank'> <u>ARS-UCD1.2 genome assembly</u></a>.
                                    Markers were excluded if they lie in problematic regions or if they match with candidates of misplacement found in Holstein-DE (<a href = 'https://doi.org/10.1111/age.13205' target='_blank'> <u>Qanbari et al. 2022</u></a>).
                                    Genotype data were filtered for minor allele frequency > 0.01 and for Mendelian inconsistencies using PLINK v1.9 (<a href = 'https://doi.org/10.1086/519795' target='_blank'><u>Purcell et al., 2007</u></a>).
                                    Families with more than 5% and SNPs with more than 10% Mendel errors were excluded; genotypes with a Mendelian inheritance error were set to"), em("'NA'"),".",HTML(" Only for Holstein-DE and Fleckvieh,
                                    the rarely missing genotypes (<0.02%) were imputed using Eagle v2.4.1 program (<a href='https://doi.org/10.1038/ng.3679 ' target='_blank'><u>Loh et al., 2016</u></a>) before running the deterministic or likelihood-based approach.
                                    Please, see for a detailed description <a href = 'HTTPs://doi.org/10.1186/s12711-020-00593-z' target='_blank'> <u>Qanbari and Wittenburg (2020)</u></a>."))),""),
                                c("",""),
                                c(paste0(htmltools::tags$h4(htmltools::tags$b(htmltools::tags$em("Approaches to estimate recombination frequencies")))),""),
                                 c(paste0(tags$h4("Deterministic approach")),paste0(htmltools::tags$h4(htmltools::HTML("Male recombination rate between <em> adjacent </em> markers was estimated according to
                                   <a href='https://doi.org/10.1186/1297-9686-46-11' target='_blank' > <u>Ferdosi et al. (2014a)</u></a>. The approach was available as R package
                                   <a href='https://cran.r-project.org/web/packages/hsphase/index.html' target='_blank'><u>hsphase</u></a> (<a href='https://doi.org/10.1186/1471-2105-15-172'target='_blank'><u>Ferdosi et al., 2014b</u></a>).
                                   Estimates of recombination rate were directly converted into genetic distances in Morgan units.")))),

                                 c(paste0(htmltools::tags$h4("Likelihood-based approach")),paste0(htmltools::tags$h4(htmltools::HTML("Male recombination rate was estimated between <em>all</em> intra-chromosomal marker pairs using a likelihood-based approach
                                   (<a href='https://doi.org/10.1093/genetics/191.1.NP'  target='_blank'><u>Gomez-Raya, 2012</u></a>,
                                   <a href='https://doi.org/10.1186/2049-1891-4-30'  target='_blank'><u>Gomez-Raya et al., 2013</u></a> and <a href='https://doi.org/10.3389/fgene.2018.00186'  target='_blank'><u>Hampel et al., 2018</u></a>).
                                    Afterwards, genetic distances between adjacent markers were derived from a quadratic optimization approach (<a href = 'https://doi.org/10.1186/s12711-020-00593-z' target='_blank'><u>Qanbari and Wittenburg 2020</u></a>).
                                    This procedure was implemented as R package <a href='https://cran.r-project.org/web/packages/hsrecombi/index.html' target='_blank'><u>hsrecombi</u></a>. <br> The approach was also used to reveal
                                    putatively misplaced markers which are provided under 'Misplaced markers'. To this end, SNPs with markedly high recombination rate to neighboring markers were localized following
                                    <a href='https://doi.org/10.3389/fgene.2018.00186' target='_blank'><u>Hampel et al. (2018)</u></a>.
                                   <br> <br>
                                   To provide users a recommendation for using the likelihood-based approach, a 'likelihood quality signal' was implemented with colors representing quality:
                                   <ul>
                                    <li> <i>green</i>: likelihood-based approach performs better than deterministic approach (N &ge; 50 & n &ge; 500 or N &ge; 10 & n &ge;1000)</li>
                                    <li> <i>orange</i>:  accuracy of the likelihood-based approach is still reasonable (&ge;80%; N &ge; 500 & n &ge; 100) </li>
                                    <li> <i>red</i>: it is not recommended to use the likelihood-based approach </li>
                                    </ul>
                                  <br>
                                  <font size=3>
                                  <ul>
                                    n = Number of genotyped progeny in each half-sib family <br>
                                    N = Number of half-sib families
                                  </ul>
                                 </font>"
                              )))),
                              c(paste0(htmltools::tags$h4("Hidden-Markov-Model (HMM) based approach")),paste0(htmltools::tags$h4(htmltools::HTML("Recombination rate between adjacent markers was estimated with the LINKPHASE3 approach
                                                        (<a href='https://doi.org/10.1093/bioinformatics/btu859'  target='_blank'><u>Druet and Georges 2015</u></a>).
                                                            This approach generates estimates of sex-average and additionally provides estimates of sex-specific recombination rates according to <a href='https://doi.org/10.1038/s41437-020-0341-9' target='_blank'><u> Zhang et al. (2020)</u></a>.
                                                            Source code was available from a <a href='https://github.com/tdruet/LINKPHASE3'  target='_blank'><u> Github repository</u></a>.
                                                            After an initial run of LINKPHASE3, markers which received an internal map confidence score of &ge; 0.986 and meioses with &le; 58 crossovers were further considered and LINKPHASE3 was restarted to obtain the final estimates.
                                                              Genetic map coordinates in Morgan units were obtained as accumulation of recombination rates. ")))),
                              c("",""),
                              c(paste0(htmltools::tags$h4(htmltools::tags$b(htmltools::tags$em("Post analysis")))),""),
                              c(paste0(htmltools::tags$h4( "Hotspot region")),paste0(htmltools::tags$h4(htmltools::HTML("Hotspot regions can be inspected on the basis of recombination rates retrieved from the deterministic or HMM-based approach. Initially, the recombination frequency threshold
                                    from <a href='https://doi.org/10.1371/journal.pgen.1005387'target='_blank'><u>Ma et al. (2015)</u></a>
                                    was used to identify hotspot regions (i.e., a region with a recombination rate exceeding 2.5 standard deviations
                                                                               (SD) from the genome-wide average of recombination rates). The threshold in SD units can be adjusted.")))),

                              c(paste0(htmltools::tags$h4("Genetic-map functions")),paste0(htmltools::tags$h4(htmltools::HTML("Various genetic-map functions have been fitted through smoothing the relationship
                                                                                                   between genetic map distance and recombination rate among intra-chromosomal pairs of SNPs obtained from the likelihood-based approach: <br>")),
                                                                                           htmltools::tags$h4(HTML("Haldane JBS. The combination of linkage values, and the calculation of distances between the loci of linked factors. J Genet. 1919; 8:299-309 <br>")),
                                                                                           htmltools::tags$h4(htmltools::tags$a(href="https://doi.org/10.1159/000152856","Rao et al., 1977", target="_blank",style='text-decoration-line: underline;')),
                                                                                           htmltools::tags$h4(htmltools::tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1216865/","Felsenstein 1979", target="_blank",style=' text-decoration-line: underline;')),
                                                                                           htmltools::tags$h4(htmltools::tags$a(href="https://doi.org/10.1016/0040-5809(84)90013-3","Liberman & Karlin 1984", target="_blank",style='text-decoration-line: underline;'))
                                                                                        ))
    ))

    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )

    jsc <- 'function(settings, json) {
      $("td:contains(\'Preprocessing\')").attr("colspan", "2").css("text-align", "left");
      $("tbody > tr:first-child > td:empty").remove();
      $("td:contains(\'The data sets have been preprocessed with a standardized workflow:\')").attr("colspan", "2").css("text-align", "left");
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
                    headerCallback = DT::JS(headerCallback),  initComplete =DT::JS(jsc)))%>% DT::formatStyle(names(table.methodology),background = 'white')%>%DT::formatStyle(1:2, 'vertical-align'='top')%>%DT::formatStyle(1:2, 'text-align' = 'left')

    )
  })
}

## To be copied in the UI
# mod_methodology_ui("mod_methodology_1")

## To be copied in the server
# mod_methodology_server("mod_contact_1")
