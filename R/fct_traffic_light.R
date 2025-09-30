#' traffic_light
#'
#' @title Creating the likelihood quality signal plot
#' @description The function creates the likelihood quality signal plot.
#'
#' @param selected.breed vector containing the full names of selected breeds
#' @param dat.tl data frame containing the breed size information for selected breeds (\link{data_traffic_light})
#'
#' @import ggplot2
#' @importFrom gridExtra tableGrob
#' @importFrom dplyr tibble
#' @importFrom patchwork wrap_plots
#'
#' @seealso
#' * \link{mod_general_server} \cr
#' * \link{mod_genetic_map_server} \cr
#' * \link{mod_genetic_function_server} \cr
#' * \link{mod_bc_general_server} \cr
#' * \link{mod_bc_genetic_map_server}\cr
#' * \link{mod_bc_genetic_function_server}
#'
#' @return The function returns the plotted likelihood quality signal (ggplot object or patchwork object (containing multiple ggplot objects)).
#' @export

make_traffic_light<-function(selected.breed,dat.tl)
{
  x<-y<-colo<-NULL

  # prepare data for traffic light
  plot.tl.l<-lapply(1:nrow(dat.tl),function(.inn){
    plot.tl=dplyr::tibble(x=c(1,1.2,1.4),y=rep(1,3), colo=rep("black",3))
    plot.tl[1,3]=ifelse(dat.tl$N2.new[.inn]>=50 && dat.tl$Averag.n2[.inn] >=500, "#009E73", "black")
    if(plot.tl[1,3]=="black")
    {
      plot.tl[2,3]=ifelse(dat.tl$N2.new[.inn]>=500 && dat.tl$Averag.n2[.inn] >=100, "#E69F00", "black")
      if(plot.tl[2,3]=="black")plot.tl[3,3]="red"
    }
    plot.tl
  })

  # create plot
  pl<-lapply(1:nrow(dat.tl),function(.x) {
      p<-ggplot2::ggplot(plot.tl.l[[.x]],ggplot2::aes(x=x, y=y)) +
          geom_point(size=4.2, colour=plot.tl.l[[.x]]$colo)+
          xlim(0.84,1.5)+
          ylim(0.99,1.01)+
          theme_bw()+
           theme(
            panel.border = element_rect(colour = 'black', linewidth = ifelse(nrow(dat.tl)==2,2,1.8)),
            panel.background = element_rect(fill = 'gray', linewidth = 3),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.background = element_rect(colour = "transparent")
           )
      if(nrow(dat.tl)!=1){
        p<-p+annotation_custom(gridExtra::tableGrob(selected.breed[.x], rows=NULL,theme=ttheme_minimal()), xmin=unit(1,"npc"),xmax = unit(2.9,"npc"))
        p<-p+theme(plot.margin = unit(c(0.0, 2.8, 0.0, .0),"cm"))
      }
      p
    })
  if(nrow(dat.tl)==1)p2<-pl[[1]]
  else p2<-patchwork::wrap_plots(pl,nrow=nrow(dat.tl))
  p2
}
