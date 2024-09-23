# Module UI

#' @title mod_contact_ui and mod_contact_server
#' @description  A shiny module containing the contact information and references.
#'
#' @param id module id

#' @rdname mod_contact
#'
#' @keywords internal, data set
#' @import shiny
#' @importFrom shinydashboard box
#' @import htmltools
#' @export


mod_contact_ui = function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
    shiny::fluidRow(style='margin-left: 6px;',
             shiny::column(width=12), htmltools::h3("Contact")),
      shinydashboard::box(status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,
         shiny::fluidRow(
           shiny::column(width=6, htmltools::tags$h4(htmltools::HTML("Research Institute for Farm Animal Biology (FBN) <br>
                                  Wilhelm-Stahl-Allee 2 <br>
                                  18196 Dummerstorf <br> <br>
                                  Email: wittenburg@fbn-dummerstorf.de <br>
                                  URL link: <a href='https://www.fbn-dummerstorf.de/en/about-us/staff/mitarbeiter/doerte-wittenburg/' target='_blank'> <u>Wittenburg homepage</u></a>
                                  <br> <br>
                                  Email: melzer@fbn-dummerstorf.de <br>
                                  URL link: <a href='https://www.fbn-dummerstorf.de/en/about-us/staff/mitarbeiter/nina-melzer/' target='_blank'><u>Melzer homepage</u></a>"
          ))),
          shiny::column(width=6,"")
        ),
      ), br(),br(),
    fluidRow(style='margin-left: 6px;',
      column(width=12), htmltools::h3("References")),
    shinydashboard::box(status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,
                                        shiny::fluidRow(shiny::column(width=12,htmltools::HTML('<h4 style = "text-indent:10px;"> Papers </h4>'))),
                                        shiny::fluidRow(shiny::column(width=12,shiny::uiOutput(ns("referenceList")))),
                                        shiny::fluidRow(shiny::column(width=12,htmltools::HTML('<h4 style = "text-indent:10px;"> Software </h4>'))),
                                        shiny::fluidRow(shiny::column(width=12, shiny::uiOutput(ns("softwareList")))),
                          )
  ))
}


#' @rdname mod_contact
#' @export
#' @keywords internal

mod_contact_server = function(id){
  shiny::moduleServer( id, function(input, output, session){

    output$referenceList <- renderUI(htmltools::tags$h4(htmltools::HTML("
    <ul>
      <li> Wittenburg D, Ding X, Melzer N, Abdollahi Sisi N, Schwarzenbacher H, Seefried FR (2024) <i>A resource of Bovine Genetic Maps</i> (in preparation)</li>  &nbsp;
    <li>Melzer N, Qanbari S, Ding X and Wittenburg D (2023) <i>CLARITY: a Shiny app for interactive visualisation of the bovine physical-genetic
    map.</i> Front. Genet. 14:1082782. doi: <a href = 'https://doi.org/10.3389/fgene.2023.1082782' target='_blank'> <u>10.3389/fgene.2023.1082782</u></a> </li>  &nbsp;
    <li> Qanbari S, Schnabel RD,  Wittenburg, D (2022) <i>Evidence of Rare Misassemblies in the Bovine Reference Genome Revealed by Population Genetic Metrics</i>
    Anim. Genet.,  53, 498-505. <a href = 'https://doi.org/10.1111/age.13205' target='_blank'> <u> 10.1111/age.13205 </u></a> </li>  &nbsp;
    <li>Qanbari  S, Wittenburg D (2020) <i>Male recombination map of the autosomal genome in German Holstein.</i> Genet Sel Evol 52, 73.
    doi: <a href = 'https://doi.org/10.1186/s12711-020-00593-z' target='_blank'> <u>10.1186/s12711-020-00593-z </u></a></li>
      </ul>")))

    output$softwareList <- renderUI(htmltools::tags$h4(htmltools::HTML("
    <ul>
      <li> Melzer N (2024). <i>CLARITY: A Shiny app for interactive visualisation of the bovine physical-genetic map (v2.0.0)</i>.
            Zenodo. <a href ='https://doi.org/10.5281/zenodo.11620802' target='_blank'><u>https://doi.org/10.5281/zenodo.11620802</u></a>
            (available: <a href ='https://github.com/nmelzer/CLARITY' target='_blank'> <u>https://github.com/nmelzer/CLARITY </u></a>)</li> &nbsp;
      <li> Melzer N (2023). <i>CLARITY: A Shiny app for interactive visualisation of the bovine physical-genetic map (v1.0.1)</i>.
            Zenodo. <a href ='https://doi.org/10.5281/zenodo.11620802' target='_blank'><u>https://doi.org/10.5281/zenodo.11620802</u></a>
            (available: <a href ='https://github.com/nmelzer/CLARITY' target='_blank'> <u>https://github.com/nmelzer/CLARITY </u></a>)</li> &nbsp;
      <li> Wittenburg D (2023) <i>A pipeline for processing genotype data and an R script for composing the CLARITY app input data. </i>Github Release v1.0.0, doi:
           <a href ='https://doi.org/10.5281/zenodo.11543150' target='_blank'> <u> 10.5281/zenodo.11543150</u></a>
           (<a href ='https://github.com/wittenburg/hsrecombi' target='_blank'><u>https://github.com/wittenburg/hsrecombi </u></a>) </li> &nbsp;
      <li> Wittenburg D (2020) <i>hsrecombi: Estimation of recombination rate and maternal LD in half-sibs.</i>, v1.0.1, available at:
           <a href ='https://cran.r-project.org/package=hsrecombi' target='_blank'><u> https://cran.r-project.org/package=hsrecombi </u></a> </li>
    </ul>")))

  })
}

## To be copied in the UI
# mod_contact_ui("mod_contact_1")

## To be copied in the server
# mod_contact_server("mod_contact_1")


