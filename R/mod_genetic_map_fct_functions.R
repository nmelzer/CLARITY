#' function
#' @title Prepare genetic map data for plotting for breed analysis
#'
#' @description The function prepares the genetic map information for a single breed and selected approaches.
#'
#' @param input data frame containing the genetic map information
#' @param approach.info data frame containing the predefined settings for selected approaches (\link{table_approach_information})
#'
#' @return The function returns list containing the desired information in data frame (\link{data_plot_geneticMap}) for each chromosome.
#' @export
#' @seealso \link{mod_genetic_map_server}

transformdata_genetic_map=function(input,approach.info)
{
 Mbp_position<-Chr<-Name<-NULL

 col.nam=colnames(input)[match(paste0(approach.info$Abbreviation,"_cM"),colnames(input))]

 a<- lapply(1:nrow(approach.info),function(appr)
 {
   dat<-input%>%dplyr::select(Chr,X=Mbp_position,Y=col.nam[appr],Marker=Name)
   dat<-dat%>%dplyr::mutate(Legend=approach.info$Approach[appr],Color=approach.info$Color[appr])
   dat$X=as.numeric(dat$X)
   dat$Y=as.numeric(dat$Y)
   dat
 })

 ll1<-do.call("rbind",a)
 ll<-split(ll1,f=~Chr)
 ll
}

#' function
#' @title Prepare genetic map data for plotting for breed comparison
#'
#' @description The function prepares the genetic map information for a single approach and chosen breeds.
#'
#' @param input list of genetic maps for the selected breeds
#' @param breed.infos data frame containing the predefined settings for selected breeds (\link{table_breed_information})
#' @param approach character containing the selected approach
#'
#' @return The function returns a list containing the desired information in data frame for each chromosome (\link{data_plot_geneticMap})
#' @export
#' @seealso \link{mod_bc_genetic_map_server}

transformdata_genetic_map_bc=function(input,breed.infos,approach)
{
  Mbp_position<-Chr<-Name<-NULL

  col.nam=colnames(input[[1]])[grep(approach$Abbreviation,colnames(input[[1]]))[1]]

  ll1<-lapply(1:nrow(breed.infos),function(ij)
  {
    dat2<-input[[ij]]%>%dplyr::select(Chr,X=Mbp_position,Y=col.nam,Marker=Name)

    dat2<-dat2%>%dplyr::mutate(Legend=breed.infos$Name[ij],Color=breed.infos$Color[ij])
    dat2
  })

  input2<-do.call("rbind",ll1)
  ll<-split(input2,f=~Chr)
  ll
}

#' function
#' @title Creating the genetic map plot for selected chromosome
#' @description The function creates the plot for a single chromosome for selected approaches (breed analysis) or the selected breeds (breed comparison)
#'
#' @param dat data frame containing the genetic map data \link{data_plot_geneticMap}
#' @param name.file character contains the name for image file
#'
#' @rawNamespace import(plotly, except = last_plot)
#' @return The function returns a plotly object.
#'
#' @export
#' @seealso
#' * \link{mod_genetic_map_server} \cr
#' * \link{mod_bc_genetic_map_server}

makePlot_geneticMaps<-function(dat,name.file)
{
  dat=dat[order(dat$Legend),]

  p <- plotly::plot_ly(dat, x = ~X, y = ~Y,color=~Legend, marker=list(size=8),type = "scatter", mode="markers", text=~Marker, hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): ", sprintf("%.3f", dat$Y),"<br>","Physical length (Mbp):", sprintf("%.3f", dat$X),"<br>","<extra></extra>"),
                       colors=~unique(dat$Color))%>%
    plotly::layout(hovermode="closest",plot_bgcolor = '#F5F5F5',
                   xaxis = list(
                     title=list(text="Physical length (Mbp)",font=list(family="Arial",size=16)),
                     tickfont = list(size = 14),
                     showgrid = TRUE,
                     gridcolor = 'white',
                     fixedrange = TRUE
                   ),
                   yaxis = list(
                     title=list(text="Genetic distance (cM)",font=list(family="Arial",size=16)),
                     tickfont = list(size = 14),
                     showgrid = TRUE,
                     gridcolor = 'white',
                     fixedrange = TRUE
                   ),
                   legend=list(title=list(text="Legend",font=list(size=16,family="Arial")),font=list(family="Arial",size=15)))%>%
    plotly::config(displayModeBar=TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud','zoom2d', 'autoScale2d', 'hoverClosestCartesian','hoverCompareCartesian', 'pan2d',
                                                                                           'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d','resetScale2d'), toImageButtonOptions= list(filename = name.file))
  p
}

#' function
#' @title Creating genetic map plots for all chromosomes
#'
#' @description The function creates the plots for all chromosomes and arranges them together in a single plot.
#'
#' @param ll.gm.s list each list element contains a data frame (\link{data_plot_geneticMap}) for a chromosome for selected approaches (breed analysis) or the selected breeds (breed comparison)
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots plot_layout
#' @import gridExtra
#'
#' @return The function returns a patchwork object (containing multiple ggplot objects)).
#' @export
#' @seealso \link{mod_genetic_map_server}

makePlot_all_geneticMaps <-function(ll.gm.s)
{
  X<-Y<-Legend<-NULL

  ll.gm.s.sort=ll.gm.s[[1]][order(ll.gm.s[[1]]$Legend),]
  colo=unique(ll.gm.s.sort$Color)
  uni.approaches=unique(ll.gm.s.sort$Legend)

  pl <- mapply(function(x) {
      ggplot2::ggplot(ll.gm.s[[x]][order(ll.gm.s[[x]]$Legend),],aes(x=X, y=Y,col=Legend)) +
      ggplot2::geom_point(size=3, na.rm=TRUE)+
      ggplot2::ggtitle(paste0("BTA ",x))+
      ggplot2::scale_color_manual(values=colo,labels=uni.approaches)+
      ggplot2::theme(axis.ticks=ggplot2::element_line(colour = "black", linewidth = 2),plot.title = ggplot2::element_text(hjust = 0.5),
            text = element_text(size = 19),legend.position="none")+
      labs(x="Physical length (Mbp)",y="Genetic distance (cM)")
  }, 1:29, SIMPLIFY = FALSE)

  gg2=patchwork::wrap_plots(pl, ncol = 3) + patchwork::plot_layout(guides = 'collect') & ggplot2::theme(legend.position='top')
  gg2
}


#' function
#' @title Creates table header for genetic map table for specific selected chromosome for breed analysis
#' @param approach.info data.frame containing relevant \link{table_approach_general}
#' @param no.cols integer number of columns for genetic map used
#' @return The function returns a shiny.tag.
#'
#' @noRd
create_table_header2 <- function(approach.info,no.cols) {

  need.col3<-c()

  pl4=lapply(1:nrow(approach.info),function(inn){
    if(approach.info$Approach[inn]!="Likelihood_male"){
      need.col3<-c(need.col3,paste0(approach.info$Abbreviation[inn]," position <br> (cM)"))
      need.col3<-c(need.col3,paste0(approach.info$Abbreviation[inn]," recombination <br> rate adjacent"))
    } else {
      need.col3<-c(need.col3,paste0(approach.info$Abbreviation[inn]," position <br> (cM)"))
    }
  })
  need.col2<-do.call("c",pl4)

  thead<-tr<-th<-NULL
  sketch2 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=2,""),
        th(colspan=3,"Physical Map "),
        th(colspan=no.cols,"Genetic Map")
      ),
      tr(
        th(colspan=5,""),
        lapply(1:nrow(approach.info), function(inn) {
          if(approach.info$Approach[inn]!="Likelihood_male") {
            th(colspan = 2, htmltools::tags$a(href="#", approach.info$Name[inn], onclick = "openTab('methodology')", style='text-decoration-line: underline'))
          } else {
            th(colspan = 1, htmltools::tags$a(href="#", approach.info$Name[inn], onclick = "openTab('methodology')", style='text-decoration-line: underline;'))
          }
        }),
      ),
      tr(
        th(colspan=1,"Chromosome"),
        th(colspan=1,"Marker"),
        th(colspan=1,"Position (Mbp)"),
        th(colspan=1,"Inter-marker distance (Mbp)"),
        th(colspan=1,"Position (BP)"),
        lapply(1:length(need.col2), function(inn) {
          th(colspan=1, htmltools::HTML(need.col2[inn]))
        })
      )
    )
  ))
  sketch2
}

#' function
#' @title Creates table header for genetic map table when all chromosomes are selected for breed analysis
#' @param approach.info data.frame containing relevant \link{table_approach_general}
#' @param no.cols integer number of columns for genetic map used
#' @return The function returns a character vetor.
#'
#' @noRd
create_table_header2_all <- function(approach.info,no.cols) {
  need.col3<-c()

  pl4=lapply(1:nrow(approach.info),function(inn){
    if(approach.info$Approach[inn]!="Likelihood_male"){
      need.col3<-c(need.col3,paste0(approach.info$Abbreviation[inn]," position (cM)"))
      need.col3<-c(need.col3,paste0(approach.info$Abbreviation[inn]," recombination rate adjacent"))
     } else {
     need.col3<-c(need.col3,paste0(approach.info$Abbreviation[inn]," position (cM)"))
    }
  })
  need.col2<-do.call("c",pl4)
  need.col3=c("Chromosome","Marker","Position (Mbp)","Inter-marker distance (Mbp)","Position (BP)",need.col2)
  need.col3
}

#' function
#' @title Creates table header for genetic map table when specific selected chromosome for breed comparison
#' @param breed.infos data.frame containing relevant \link{table_breed_general}
#' @return The function returns a shiny.tag.
#' @noRd
create_table_header_bc2<-function(breed.infos)
{
  thead<-tr<-th<-NULL
  sketch2 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=2,""),
        th(colspan=2,"Physical Map"),
        th(colspan=nrow(breed.infos),"Genetic Map"
        ),
        tr(
          th(colspan=1,"Marker"),
          th(colspan=1,"Chromosome"),
          th(colspan=1,"Position (Mbp)"),
          th(colspan=1,"Position (bp)"),

          lapply(1:nrow(breed.infos), function(inn) {
            th(colspan=1,HTML(paste(breed.infos$Name[inn],"<br> Position (cM)")))
          })
        )
      )
    ))
  )
  sketch2
}

#' function
#' @title Creates table header for genetic map table when all chromosomes are selected for breed comparison
#' @param breed.infos data.frame containing relevant \link{table_breed_general}
#' @return The function returns a character vector.
#' @noRd
create_table_header_bc2_all<-function(breed.infos)
{
  names=lapply(1:nrow(breed.infos), function(inn) {
    paste0(breed.infos$Name[inn]," Position (cM)")
  })

  header.p1=do.call("c",names)
  header.p2=c("Marker","Chromosome","Position (Mbp)","Position (bp)", header.p1)
  header.p2
}
