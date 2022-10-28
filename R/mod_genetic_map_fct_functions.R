#' function
#' @title Prepare genetic map data for plotting
#'
#' @description The function prepares the genetic map information for a single breed.
#'
#' @param n number of chromosomes
#' @param input data frame containing the genetic map information
#'
#' @return The function returns a list containing the desired information for each chromosome.
#' @export
#' @seealso \link{mod_genetic_map_server}

transformdata_genetic_map=function(n,input)
{
 ll=list()
  for(i in 1:n)
  {
    df.p = input[which(input$Chr==i),]

    a=cbind(df.p$"Chr",df.p$"Mbp_position",df.p$"cM_deterministic")
    colnames(a)=c("chr","X","Y")

    b=cbind(df.p$"Chr",df.p$"Mbp_position",df.p$"cM_likelihood")
    colnames(b)=c("chr","X","Y")

    df.p2=rbind(a,b)

    colnames(df.p2)=c("chr","X","Y")
    Approach=c(rep("deterministic",nrow(df.p)),rep("likelihood",nrow(df.p)))
    df.p2=as.data.frame(cbind(df.p2,Approach))
    df.p2$chr=as.numeric(df.p2$chr)
    df.p2$X=as.numeric(df.p2$X)
    df.p2$Y=as.numeric(df.p2$Y)

    ll[[i]]=df.p2
 }
 ll
}


#' function
#' @title Prepare genetic map data for plotting for breed comparison
#'
#' @description The function prepares the genetic map information for more than one breed.
#' @param n number of chromosomes
#' @param input list of genetic maps for the selected breeds
#' @param meth name of approach ("deterministic", "likelihood-based")
#' @param names.bc.plot contains the names of the selected breeds
#'
#' @return The function returns a list containing the desired information for all selected breeds for each chromosome.
#' @export
#' @seealso \link{mod_bc_genetic_map_server}

transformdata_genetic_map_bc=function(n,input,meth,names.bc.plot)
{

  ll=list()
  for(i in 1:n)
  {
    for(ij in 1:length(names.bc.plot))
    {
     df.p = input[[ij]][which(input[[ij]]$Chr==i),]
     print(dim(df.p))
     if(meth=="deterministic")a=cbind(df.p$"Chr",df.p$"Mbp_position",df.p$"cM_deterministic")
     else a=cbind(df.p$"Chr",df.p$"Mbp_position",df.p$"cM_likelihood")

     colnames(a)<-c("chr","X","Y")

     Breed=rep(ij,nrow(a))
     df.p2=as.data.frame(cbind(a,Breed))

     if(ij==1) df.22=as.data.frame(df.p2)
     else df.22=as.data.frame(rbind(df.22,df.p2))

     if(ij ==length(names.bc.plot)){
        df.0=df.22
        df.0$chr=as.numeric(df.0$chr)
        df.0$X=as.numeric(df.0$X)
        df.0$Y=as.numeric(df.0$Y)
        df.0$Breed=as.character(df.0$Breed)

        bind.breeds=df.0
     }
    }
    ll[[i]]=bind.breeds
  }
  ll
}



#' function
#' @title Creating the genetic map plot for breed analysis
#' @description The function creates the plot for a single chromosome for one breed.
#'
#' @param chr selected chromosome
#' @param dat_maps genetic map data
#' @param name.file name for image file
#' @param colo vector of predefined colors
#'
#' @rawNamespace import(plotly, except = last_plot)
#' @return The function returns a plotly object.
#' @export
#' @seealso \link{mod_genetic_map_server}

makePlot_geneticMaps<-function(chr,dat_maps,name.file,colo)
{
   if(length(which(is.na(dat_maps$cM_likelihood)==TRUE))!=0)dat_maps=dat_maps[-which(is.na(dat_maps$cM_likelihood)==TRUE),]

   p <- plotly::plot_ly(dat_maps, x = dat_maps$"Mbp_position", y = dat_maps$"cM_deterministic", type = "scatter",size=20, text=dat_maps[,2],hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"
   ), mode = "markers",height=800,name="Deterministic",color=I(colo[1]))%>% layout(title=list(text=paste0("BTA ",chr),font=18), xaxis=list(title="Physical length (Mbp)"),yaxis=list(title="Genetic distance (cM)"),
                                                                                                margin=list(t=100)) %>% config(displayModeBar=TRUE,displaylogo = FALSE, modeBarButtonsToRemove = list(
                                                                                              'sendDataToCloud','zoom2d', 'autoScale2d', 'hoverClosestCartesian','hoverCompareCartesian', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d','resetScale2d'),
                                                                                                toImageButtonOptions= list(filename = name.file)) ### Ooms, Jeroen. 2018. Rsvg: Render Svg Images into Pdf, Png, Postscript, or Bitmap Arrays. https://CRAN.R-project.org/package=rsvg. adopted from website: https://plotly-r.com/control-modebar.html 05.08.2021

   p=add_markers(p,x=dat_maps$"Mbp_position",y=dat_maps$"cM_likelihood",color=I(colo[2]), text=dat_maps[,2],hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"),name="Likelihood")
   p
}



#' function
#' @title Creating the genetic map plot for breed comparison
#'
#' @description The function creates the plot for a single chromosome for more than one breed.
#'
#' @param chr selected chromosome
#' @param dat_maps.list list, each list element contains the genetic map data for a breed
#' @param colo vector containing the pre-defined colors
#' @param plot.pos contains the column which should be used
#' @param what contains the approach name ("deterministic" or "likelihood-based") to create plot title
#' @param name.file name of output file
#'
#' @rawNamespace import(plotly, except = last_plot)
#' @return The function return a plotly object.
#' @export
#' @seealso \link{mod_bc_genetic_map_server}

makePlot_geneticMaps_bc<-function(chr,dat_maps.list,colo,plot.pos,what,name.file)
{
  p <- plotly::plot_ly(dat_maps.list[[1]], x = dat_maps.list[[1]]$"Mbp_position", y = dat_maps.list[[1]][,plot.pos], type = "scatter",size=20, text=dat_maps.list[[1]][,2],hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"
  ), mode = "markers",height=800,name=names(dat_maps.list)[1],color=I(colo[1]))%>% plotly::layout(title=list(text=paste0("BTA ",chr," - ", what),font=18), xaxis=list(title="Physical length (Mbp)"),yaxis=list(title="Genetic distance (cM)")) %>%
                                                                                        plotly::config(displayModeBar=TRUE,displaylogo = FALSE, modeBarButtonsToRemove = list(
                                                                                          'sendDataToCloud','zoom2d', 'autoScale2d', 'hoverClosestCartesian','hoverCompareCartesian', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d','resetScale2d'),
                                                                                          toImageButtonOptions= list(filename = name.file)) ### Ooms, Jeroen. 2018. Rsvg: Render Svg Images into Pdf, Png, Postscript, or Bitmap Arrays. https://CRAN.R-project.org/package=rsvg. adopted from website: https://plotly-r.com/control-modebar.html 05.08.2021

  for(j in 2:length(colo))p=plotly::add_markers(p,x=dat_maps.list[[j]]$"Mbp_position",y=dat_maps.list[[j]][,plot.pos],color=I(colo[j]), text=dat_maps.list[[j]][,2],hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"),name=names(dat_maps.list)[j])
  p
}



#############################################################
#' function
#' @title Creating genetic map plots for all chromosomes for breed analysis
#'
#' @description The function creates the plots for all chromosomes for a breed and grid arrange them. To obtain only one
#' legend for all plots, the function \link{get_only_legend} is called inside.
#'
#' @param ll.gm.s list, each list element contains a data frame, including chromosome, position in Mbp and cM, as well as approach for a chromosome
#' @param colo vector containing the pre-defined colors
#'
#' @import ggplot2
#' @import gridExtra
#'
#' @return The function return a grid with arranged ggplots.
#' @export
#' @seealso \link{mod_genetic_map_server}
#' @note adopted from https://www.geeksforgeeks.org/add-common-legend-to-combined-ggplot2-plots-in-r/

makePlot_all_geneticMaps <-function(ll.gm.s,colo)
{
  X<-Y<-Approach<-NULL
  pl<-lapply(1:29,function(.x) ggplot2::ggplot(ll.gm.s[[.x]],aes(x=X, y=Y,col=Approach)) +
               geom_point(size=3, na.rm=TRUE)+
               ggtitle(paste0("BTA ",.x))+
               scale_color_manual(values=colo,labels=c("Deterministic","Likelihood"))+
               theme(axis.ticks=element_line(colour = "black", size = 2),plot.title = element_text(hjust = 0.5),
                     text = element_text(size = 19),legend.position="none")+
               labs(x="Physical length (Mbp)",y="Genetic distance (cM)")
  )
  gg= gridExtra::grid.arrange(grobs=pl,ncol=3)

    plot1_legend <- ggplot2::ggplot(ll.gm.s[[1]],aes(x=X, y=Y,col=Approach)) +
                  geom_point(size=3, na.rm=TRUE)+
                  ggtitle(paste0("BTA 1"))+
                  scale_color_manual(values=colo,labels=c("Deterministic","Likelihood"))+
                  theme(axis.ticks=element_line(colour = "black", size = 2),plot.title = element_text(hjust = 0.5),
                        text = element_text(size = 19),legend.position="top")+
                  labs(x="Physical length (Mbp)",y="Genetic distance (cM)")

  # extract legend from plot1 using above function
  legend <- get_only_legend(plot1_legend)

  gg2=gridExtra::grid.arrange(legend,gg,nrow=2,heights = c(0.2, 20.5))
  gg2
}

############  plot for all chromosomes - breed comparison
#' function
#' @title Extracting legend
#' @description The function extracts the legend of a ggplot object.
#'
#' @param plot ggplot object
#'
#' @import ggplot2
#' @return The function return the legend from a ggplot object.
#' @export
#' @seealso The function is called from \link{makePlot_all_geneticMaps_bc}.
#' @note adopted from https://www.geeksforgeeks.org/add-common-legend-to-combined-ggplot2-plots-in-r/

get_only_legend <- function(plot) {
  plot_table <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

#' function
#' @title Creating all genetic map plots for all chromosomes for more than one breed
#'
#' @description The function creates the plots for all chromosomes for more than one breed and grid arrange them. To obtain only one
#' legend for all plots, the function \link{get_only_legend} is called inside.
#'
#' @param ll list, whereby each list element contains a data frame including chromosome number, the position in Mbp and cM, as well as breed information for a chromosome
#' @param names.bc.plot vector containing names of selected breeds
#' @param colo vector containing the pre-defined colors
#'
#' @import ggplot2
#' @import gridExtra
#' @note adopted from https://www.geeksforgeeks.org/add-common-legend-to-combined-ggplot2-plots-in-r/
#' @return The function return a grid with arranged ggplots.
#' @export
#' @seealso \link{mod_bc_genetic_map_server}

makePlot_all_geneticMaps_bc <-function(ll,names.bc.plot,colo)
{
  X<-Y<-Breed <-NULL
  pl<-lapply(1:29,function(.x) ggplot2::ggplot(ll[[.x]],aes(x=X, y=Y,col=Breed)) +
               geom_point(size=3, na.rm=TRUE)+
               ggtitle(paste0("BTA ",.x))+
               scale_color_manual(values=colo[1:length(names.bc.plot),1],labels=names.bc.plot)+
               theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 19),
                     plot.title = element_text(hjust = 0.5),legend.position="none")+
               labs(x="Physical length (Mbp)",y="Genetic distance (cM)")
  )

  gg= gridExtra::grid.arrange(grobs=pl,ncol=3)

  plot1_legend <- ggplot2::ggplot(ll[[1]],aes(x=X, y=Y,col=Breed)) +
    geom_point(size=3, na.rm=TRUE)+
    scale_color_manual(values=colo[1:length(names.bc.plot),1],labels=names.bc.plot)+
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 19),
          plot.title = element_text(hjust = 0.5),legend.position="top")

  # extract legend from plot1 using above function
  legend <- get_only_legend(plot1_legend)

  gg2=gridExtra::grid.arrange(legend,gg,nrow=2,heights = c(0.2, 20.5))
  gg2
}

