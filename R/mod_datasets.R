# Module UI

#' @title mod_datasets_ui and mod_datasets_server
#' @description  A shiny Module containing all relevant information about the data sets. Currently, only one Holstein data set is included.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_datasets
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import shiny
#' @import shinydashboard

mod_datasets_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
  fluidRow(br(),br(),
           column(width=12,
  box(title =tags$b(tags$h3("The German Holstein data set")),status="primary",width=12,collapsible=F,solidHeader = TRUE,#style="text-align: justify;",
      
      tags$h4(HTML("Data were provided by the German Evaluation Center <a href = 'https://www.vit.de' target='_blank' >vit (IT solutions for animal production)</a>.  Permission for data access was granted by the 
              <a href='https://www.fbf-forschung.de/' target='_blank' >Association for Bioeconomy Research (FBF, Bonn)</a> as representative of German cattle breeders.")),
      br(),
            tags$h4("This study used a large pedigree including 367,056 German Holstein cattle (i.e., 1053 half-sib families with sires born between 1979-2017)."),
      br(),
            tags$h4(HTML("Data included genotypes of the Illumina Bovine SNP50 genotype array mapped to the coordinates of the <a href='https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2' target='_blank' >ARS-UCD1.2 assembly</a>.")),
      br(),
      tags$h4(HTML("Genotype data were filtered for minor allele frequency > 0.01 and for Mendelian inconsistencies
             both on marker (5% threshold) and individual (10% threshold) levels using PLINK v1.9 <a href = 'https://doi.org/10.1086/519795' target='_blank' >(Purcell et al., 2007)</a> . Genotypes with a Mendelian inheritance error were set to 'NA' and missing values were
              imputed using Eagle v2.3 program <a href='https://doi.org/10.1038/ng.3679 ' target='_blank' >(Loh et al., 2016)</a>.")), 
      br(),    
      tags$h4("Recombination rates were eventually estimated based on 44,696 SNPs and across 876 half-sib families, 
            with sires having a minimum number of 39 progeny."),
      br(),
      tags$h4(HTML("See <a href = 'https://doi.org/10.1186/s12711-020-00593-z' >Qanbari and Wittenburg (2020)</a> for a detailed description and"),
              tags$a(href="#","Methodology",onclick="openTab('methodology')"),"for the terminology of the methods mentioned in the app.")
  
  ))))
}


# Module Server

#' @rdname mod_datasets
#' @export
#' @keywords internal
mod_datasets_server=function(input, output, session){
}


## To be copied in the UI
# mod_datasets_ui("mod_datasets_1")

## To be copied in the server
# callModule(mod_datasets_server,"mod_datasets_1")
