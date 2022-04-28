# Module UI

#' @title mod_methodology_ui and mod_methodology_server
#' @description  A shiny Module containing all relevant information about XXX.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_methodology
#' 
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import shiny
#' @import shinydashboard


mod_methodology_ui <- function(id){
  ns <- NS(id)

  fluidRow(
    br(), br(),
    box(title=tags$b(tags$h3("Description of approaches")), status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,
      fluidRow(
        column(3,tags$h3((tags$b("Method")))),
        column(9,tags$h3((tags$b("Short description"))))
      ),
      hr(),
      fluidRow(
        br(),
        column(8,(tags$h4(tags$b(tags$em("Approaches to estimate recombination frequencies")))))
      ),
      fluidRow(
        column(3,tags$h4("Deterministic approach")),
        column(9,tags$h4(HTML("Recombination rate between <em> adjacent </em> markers was estimated according to <a href='https://doi.org/10.1186/1297-9686-46-11' target='_blank' >Ferdosi et al. (2014a)</a>. The approach was available as R package  
              <a href='https://cran.r-project.org/web/packages/hsphase/index.html' target='_blank'>hsphase</a> (<a href='https://doi.org/10.1186/1471-2105-15-172'target='_blank' > Ferdosi et al., 2014b</a>). Estimates of recombination rate 
                were directly converted into genetic distances in Morgan units.")))
      ),
      
      br(),
      fluidRow(
        column(3,tags$h4("Likelihood approach")),
        column(9, tags$h4(HTML("Recombination rate was estimated between <em>all</em> intra-chromosomal marker pairs using a likelihood-based (<a href='https://doi.org/10.1093/genetics/191.1.NP'  target='_blank' >Gomez-Raya, 2012</a>, 
                          <a href='https://doi.org/10.1186/2049-1891-4-30'  target='_blank' >Gomez-Raya et al., 2013</a> and <a href='https://doi.org/10.3389/fgene.2018.00186'  target='_blank' >Hampel et al., 2018</a>).   
                  Afterwards, genetic distances between adjacent markers were derived from a quadratic optimization approach <a href = 'https://doi.org/10.1186/s12711-020-00593-z' target='_blank' >Qanbari and Wittenburg, (2020)</a>. This procedure was implemented as R package 
                          <a href='https://cran.r-project.org/web/packages/hsrecombi/index.html' target='_blank' >hsrecombi</a>.")))
      ),
      
      fluidRow(
        column(3),
        column(9, tags$h4(HTML("The approach was also used to reveal putative misplaced markers which are provided under 'Results' <a href = 'https://doi.org/10.1111/age.13205' target='_blank' >Qanbari and Wittenburg, (2022)</a>.")))
      ),
      hr(),
      br(),
      br(),
      fluidRow(
        column(3,tags$h4(tags$b(tags$em("Post analysis"))))
      ),
      fluidRow(
        column(3,tags$h4( "Hotspot region")),
        column(9, tags$h4(HTML("Hotspot regions were derived on the basis of the <a href='#' onclick = 'openTab('methodology')' >deterministic approach</a>. Initially, the recombination frequency threshold from <a href='https://doi.org/10.1371/journal.pgen.1005387'target='_blank' >Ma et al. (2015)</a> 
        was used to identify hotspot regions (i.e., a region with a recombination rate exceeding 2.5 standard deviations (SD) from the genome-wide average of recombination rates). The threshold in SD units can be adjusted.")))
      ),
      
      br(),
      fluidRow(
        column(3,tags$h4("Markers identified as being misplaced or residing in problematic regions")),
        column(9,tags$h4("The", tags$a(href="#","likelihood-based approach",onclick="openTab('methodology')"), HTML("enabled identifying putatively misplaced markers. SNPs with a markedly high recombination rate to neighboring markers were localized following  
                         <a href='https://doi.org/10.3389/fgene.2018.00186' target='_blank' >Hampel et al., 2018</a>. <br> 
                              Additional markers located in problematic regions <a href='https://journal.interbull.org/index.php/ib/article/view/1468' target='_blank' >(Null et al., 2019)</a> were reported. <br>
                         These SNPs were further investigated on the molecular level by aligning their probe sequences <a href='https://www.animalgenome.org/repository/cattle/UMC_bovine_coordinates/' target='_blank' >(Schnabel, 2018)</a> 
                  to the reference genome ARS-UCD1.2 (<a href='https://blast.ncbi.nlm.nih.gov/Blast.cgi' target='_blank'>NCBI Blast</a> assessed on 11.05.2021). The blast analysis was performed using default values.")))
      ),
      
     
      br(),
  #    hr(),
    #  fluidRow(
    #    br(),
     #   column(width=3),
    #    column(width=9,tags$h4(tags$b(tags$em("Genetic-map functions"))))
    #  ),
      fluidRow(
        column(width=3,tags$h4("Genetic-map functions")),
        column(width=9,tags$h4("Various genetic-map functions have been fitted through smoothing the relationship between genetic map distances and recombination rate:"))
      ),
     fluidRow(
      column(width=3),
      column(width=9,tags$h4("Haldane JBS. The combination of linkage values, and the calculation of distances between the loci of linked factors. J Genet. 1919;8:299-309."),br(),
           tags$h4(tags$a(href="https://doi.org/10.1159/000152856","Rao et al., 1977", target="_blank")), br(),
           tags$h4(tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1216865/","Felsenstein 1979", target="_blank")),br(),
           tags$h4(tags$a(href="https://doi.org/10.1016/0040-5809(84)90013-3","Liberman & Karlin 1984", target="_blank")))
     )
    ##
    # extend the box with additional information
    ), ## ending the box 
    br(),
    br(),
    ## partly change - renderUI is necessary
    fluidRow(id="backtoanalysis",
             column(5,""),
             column(5,tags$a(href="#",h3("Back to analyses"), onclick = "openTab('single')")) # onclick = "customHref('single');customHref('Misplaced Markers');"))
    )
  
  )
}


# Module Server
#' @rdname mod_methodology
#' @export
#' @keywords internal
mod_methodology_server=function(input, output, session){
}


## To be copied in the UI
# mod_methodology_ui("mod_methodology_1")

## To be copied in the server
# callModule(mod_methodology_server,"mod_methodology_1")

