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
      ), br(),br(),br(),
    fluidRow(style='margin-left: 6px; margin-top: 180px;',
      column(width=12), htmltools::h3("References")),
    shinydashboard::box(status="primary",width=12, solidHeader = TRUE,collapsible = FALSE,
                                        shiny::fluidRow(shiny::column(width=12,htmltools::HTML('<h4 style = "text-indent:10px;"> <b>Papers</b> </h4>'))),
                                        shiny::fluidRow(shiny::column(width=12,shiny::uiOutput(ns("referenceList")))),
                                        shiny::fluidRow(shiny::column(width=12,htmltools::HTML('<h4 style = "text-indent:10px;"> <b>Software</b> </h4>'))),
                                        shiny::fluidRow(shiny::column(width=12, shiny::uiOutput(ns("softwareList")))),
                                        shiny::fluidRow(shiny::column(width=12,htmltools::HTML('<h4 style = "text-indent:10px;"> <b>Datasets</b> </h4>'))),
                                        shiny::fluidRow(shiny::column(width=12, shiny::uiOutput(ns("dataList"))))
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
     <li>Wittenburg D, Ding X, Melzer N, Abdollahi Sisi N, Seefried FR (2025) <i> A comparative study on recombination activity in cattle</i> (in revision)</li>  &nbsp;

    <li>Melzer N, Qanbari S, Ding X and Wittenburg D (2023) <i>CLARITY: a Shiny app for interactive visualisation of the bovine physical-genetic
    map.</i> Front. Genet. 14:1082782. doi: <a href = 'https://doi.org/10.3389/fgene.2023.1082782' target='_blank'> <u> https://doi.org/10.3389/fgene.2023.1082782 </u></a> </li>  &nbsp;

    <li> Qanbari S, Schnabel RD,  Wittenburg, D (2022) <i>Evidence of rare misassemblies in the bovine reference genome revealed by population genetic metrics.</i>
    Anim. Genet.,  53, 498-505. <a href = 'https://doi.org/10.1111/age.13205' target='_blank'> <u> https://doi.org/10.1111/age.13205 </u></a> </li>  &nbsp;

    <li>Qanbari  S, Wittenburg D (2020) <i>Male recombination map of the autosomal genome in German Holstein.</i> Genet Sel Evol 52, 73.
    <a href = 'https://doi.org/10.1186/s12711-020-00593-z' target='_blank'> <u> https://doi.org/10.1186/s12711-020-00593-z </u></a></li>
      </ul>")))


    output$softwareList <- renderUI(htmltools::tags$h4(htmltools::HTML("
    <ul>
      <li> Melzer N (2025) <i> CLARITY: A Shiny app for interactive visualisation of the bovine physical-genetic map (v3.0.0).</i> Zenodo.<a href ='https://doi.org/10.5281/zenodo.11620801' target='_blank'> <u>https://doi.org/10.5281/zenodo.11620801</u></a>;
            Available at <a href ='https://github.com/nmelzer/CLARITY' target='_blank'> <u>https://github.com/nmelzer/CLARITY </u></a> </li> &nbsp;

      <li> Wittenburg D (2025) <i> wittenburg/hsrecombi: Estimation of Recombination Rate and Maternal LD in Half-Sibs (v2.0.1).</i> Zenodo. <a href = 'https://doi.org/10.5281/zenodo.17962688' target='_blank'> <u>https://doi.org/10.5281/zenodo.17962688</u></a>;
            Available at <a href ='https://github.com/wittenburg/hsrecombi/tree/v2.0.1' target='_blank'> <u>https://github.com/wittenburg/hsrecombi/tree/v2.0.1 </u></a></li> &nbsp

      <li> Melzer N (2024) <i>CLARITY: A Shiny app for interactive visualisation of the bovine physical-genetic map (v2.0.0)</i>. Zenodo. <a href ='https://doi.org/10.5281/zenodo.13832239' target='_blank'><u> https://doi.org/10.5281/zenodo.13832239</u></a>;
      Available at <a href ='https://github.com/nmelzer/CLARITY/tree/v2.0.0' target='_blank'> <u>https://github.com/nmelzer/CLARITY/tree/v2.0.0 </u></a> </li>&nbsp;


      <li> Melzer N (2023) <i>CLARITY: A Shiny app for interactive visualisation of the bovine physical-genetic map (v1.0.1). </i> Zenodo. <a href ='https://doi.org/10.5281/zenodo.11620802' target='_blank'><u>https://doi.org/10.5281/zenodo.11620802</u></a>;
            Available at <a href ='https://github.com/nmelzer/CLARITY/tree/v1.0.1' target='_blank'> <u>https://github.com/nmelzer/CLARITY/tree/v1.0.1 </u></a></li> &nbsp;


     <li> Wittenburg D (2023) <i>wittenburg/hsrecombi: First release of hsrecombi pipeline (v1.0.0).</i> Zenodo.<a href = 'https://doi.org/10.5281/zenodo.11543150' target='_blank'> <u>https://doi.org/10.5281/zenodo.11543150</u></a> </li> &nbsp


      <li> Wittenburg D (2023) <i>hsrecombi: Estimation of recombination rate and maternal LD in half-sibs </i> (v1.0.1).
            Available at <a href ='https://cran.r-project.org/package=hsrecombi' target='_blank'><u> https://cran.r-project.org/package=hsrecombi </u></a> </li>&nbsp

      <li> Wittenburg D (2021) <i>hsrecombi: Estimation of recombination rate and maternal LD in half-sibs </i> (v0.3.4).
            Available at <a href ='https://cran.r-project.org/package=hsrecombi' target='_blank'><u> https://cran.r-project.org/package=hsrecombi </u></a> </li>&nbsp

   </ul>")))


   output$dataList <- renderUI(htmltools::tags$h4(htmltools::HTML("
    <ul>
     <li> Wittenburg D (2025) <i>Data derived from sex-specific recombination rate analysis in cattle (v2.0).</i> Zenodo. <a href ='https://doi.org/10.5281/zenodo.17909700' target='_blank'> <u>https://doi.org/10.5281/zenodo.17909700</u></a> </li> &nbsp
     <li> Wittenburg D (2023) <i>Data derived from male recombination rate analysis in cattle (v1.0). Zenodo.</i> <a href ='https://doi.org/10.5281/zenodo.17186014' target='_blank'> <u>https://doi.org/10.5281/zenodo.17186014</u></a> </li> &nbsp
    </ul>")))

  })
}


# <li> Melzer N (2025) <i>CLARITY: A Shiny app for interactive visualisation of the bovine physical-genetic map (v3.0.0)</i>.
# Available at <a href ='https://doi.org/10.5281/zenodo.17186014' target='_blank'> <u>Zenodo</u></a>;


## To be copied in the UI
# mod_contact_ui("mod_contact_1")

## To be copied in the server
# mod_contact_server("mod_contact_1")


