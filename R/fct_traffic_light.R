#' traffic_light
#'
#' @title Creating the likelihood quality signal plot
#' @description The function creates likelihood quality signal plot.
#' @return The function returns the plotted likelihood quality signal (ggplot object).
#'
#' @param selected.breed vector containing the full names of selected breeds
#' @param dat.tl data frame containing Breed, n1 (number of genotyped animals), N1 (number of half-sib families), n2 (number of genotyped animals in half-sib families with at least 30 progeny), N2 (half-sib families with sires having at least 30 progenies), and p (number of SNPs).

#' @import ggplot2
#' @import gridExtra
#' @import ggpubr
#' @seealso \link{mod_general_server}, \link{mod_genetic_map_server}, \link{mod_genetic_function_server}, \link{mod_bc_general_server}, \link{mod_bc_genetic_map_server} and \link{mod_bc_genetic_function_server}
#'
#' @export

make_traffic_light<-function(selected.breed,dat.tl)
{
  plot.tl<-plot.tl.l<-x<-y<-colo<-NULL

  if(nrow(dat.tl)==1)
  {
    plot.tl=tibble(x=c(1,1.2,1.4),y=rep(1,3), colo=rep("black",3))

    plot.tl[1,3]=ifelse(dat.tl$N2.new>=50 && dat.tl$Averag.n2 >=500, "#009E73", "black")
    if(plot.tl[1,3]=="black")
    {
      plot.tl[1,3]=ifelse(dat.tl$N2.new>=10 && dat.tl$Averag.n2 >=1000, "#E69F00", "black")

      if(plot.tl[1,3]=="black")plot.tl[2,3]=ifelse(dat.tl$N2.new>=500 && dat.tl$Averag.n2 >=100, "#E69F00", "black")

      if(plot.tl[2,3]=="black")plot.tl[3,3]="red"
    }
      p=ggplot(plot.tl,aes(x, y)) +
      geom_point(size=4.2, colour=plot.tl$colo) +
        xlim(0.84, 1.5)+
        ylim(0.99,1.01)+
        theme_bw()+
        theme(panel.border = element_rect(colour = 'black', size = 2),
              panel.background = element_rect(fill = 'gray', size = 3),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()
              )
      p
  }
  else
  {
    plot.tl.l=list()
    for(inn in 1:nrow(dat.tl))
    {
      plot.tl=as.data.frame(tibble(x=c(1,1.2,1.4),y=rep(1,3), colo=rep("black",3)))

      plot.tl[1,3]=ifelse(dat.tl$N2.new[inn]>=50 && dat.tl$Averag.n2[inn] >=500, "#009E73", "black")
      if(plot.tl[1,3]=="black")
      {
        plot.tl[2,3]=ifelse(dat.tl$N2.new[inn]>=500 && dat.tl$Averag.n2[inn] >=100, "#E69F00", "black")

        if(plot.tl[2,3]=="black")plot.tl[3,3]="red"
        # plot.tl[3,3]=ifelse(dat.tl$N2<500 && dat.tl$Averag.n2 < 100, "red", "black")
      }


      plot.tl.l[[inn]]=plot.tl
    }

    pl<-lapply(1:nrow(dat.tl),function(.x) ggplot2::ggplot(plot.tl.l[[.x]],aes(x=x, y=y)) +
      geom_point(size=4.2, colour=plot.tl.l[[.x]]$colo)+
      xlim(0.84, 1.5)+
      ylim(0.99,1.01)+
      theme_bw()+

      annotation_custom(tableGrob(selected.breed[.x], rows=NULL,theme=ttheme_minimal()), xmin=unit(1,"npc"),xmax = unit(2.9,"npc"))+ # theme - change the background color

      theme(plot.margin = unit(c(0.0, 3.4, 0.0, .0),"cm"),
            panel.border = element_rect(colour = 'black', size = 3),
            panel.background = element_rect(fill = 'gray', size = 3),
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

    )
    p = gridExtra::grid.arrange(grobs=pl,nrow=nrow(dat.tl))
    p2=ggpubr::as_ggplot(p)
    p2
  }
}
