#' function
#' @title Transform data for Hotspot plot
#'
#' @description The function used the threshold to determine hotspot markers and add accordingly the colors, shapes and numbering.
#'
#' @param data.trans data frame (containing: Chr, SNP, cM, BP, Theta, Dis)
#' @param value numeric contains the user specified threshold to determine the hotspot
#' @param color1 vector containing the predefined used colors
#' @param shape1 vector containing the predefined used shapes
#' @param ord vector containing the predefined numbering
#'
#' @importFrom stats sd
#' @export
#' @returns The function returns a data frame (containing: Chr, SNP, cM, BP, Theta, Dis, ord, size, coloring, and shape).
#' @seealso \link{mod_hotspot_server} and \link{mod_bc_hotspot_server}

transformdata_hotspot=function(data.trans,value=value,color1=color1,shape1=shape1,ord=ord)  # change - make one input together with app_ui
{
  data.trans$Theta= as.numeric(as.character(data.trans$Theta))

  threshold <- mean(data.trans$Theta) + value * sd(data.trans$Theta)

  data.trans$cols <- unlist(ifelse(data.trans$Theta > threshold, ord[2], ord[1]) )
  data.trans$cols=as.numeric(data.trans$cols) ## necessary otherwise continuous and is mixed
  colnames(data.trans)[dim(data.trans)[2]]="ord"

  data.trans=cbind(data.trans,data.trans$ord)
  colnames(data.trans)[dim(data.trans)[2]]="size"
  data.trans$size=unlist(ifelse(data.trans$Theta > threshold, 3, 1))
  data.trans=cbind(data.trans,as.character(data.trans$ord))
  colnames(data.trans)[dim(data.trans)[2]]="coloring"
  data.trans$coloring=unlist(ifelse(data.trans$Theta > threshold, color1[1],color1[2]))
  data.trans=cbind(data.trans,as.character(data.trans$ord))
  colnames(data.trans)[dim(data.trans)[2]]="shape"
  data.trans$shape=as.numeric(unlist(ifelse(data.trans$Theta > threshold, shape1[1],shape1[2])))

  data.trans
}

#' function
#' @title Creating the Hotspot scatter plot for all chromosomomes
#'
#' @description The function creates the hotspot plot for all chromosomes.
#' @details Note: legend1, the column breaks contains the color for the corresponding label, and column ord contains numbers used for sorting to keep the sequence (corresponds to ord in dat1).
#'
#' @param dat1 data frame (containing: Chr, SNP, cM, BP, Theta, Dis, ord, size, coloring, and shape)
#' @param ranges reactive values containing the brush information
#' @param scale numeric contains the number to scale the plot
#' @param legend1 data frame containing the legend information (contains: label, breaks, shape, and ord)
#'
#' @import ggplot2
#' @return The function returns a ggplot object.
#' @export
#' @seealso \link{mod_hotspot_server} and \link{mod_bc_hotspot_server}

scatterPlot_hotspot_all <- function(dat1,ranges,scale,legend1) ## input all in function direct
{
  legend1=legend1[order(as.numeric(legend1$ord)),]
  label1= legend1$label
  breaks1= legend1$breaks
  shape1= as.numeric(legend1$shape)

  BP<-ChrTheta<-coloring<-ord<-NULL

  p=ggplot2::ggplot(dat1, aes(x = BP, y = ChrTheta)) +
  geom_point(aes(colour = as.factor(ord)),shape=dat1$shape,size=dat1$size)+
  scale_colour_manual(name="Legend",values = breaks1,labels=label1)+
  guides(colour = guide_legend(override.aes = list(size=5,shape=shape1)))+
    theme(panel.spacing = unit(0.1, "points"))+
    xlim(0,round(max(dat1$BP)+1))+
    scale_y_continuous(name="Chromosome", breaks=seq(1/scale,29/scale,1/scale),labels=seq(1,29,1),limits=c(0, 30/scale))+
    xlab("Chromosome position (Mbp)") +
    ylab("Chromosome") +
    coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+ ##new added necessary for hover
    theme(
      panel.background = element_rect(fill = 'White'),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black"),legend.position="right")+
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 22))
  p
}


#' function
#' @title Creating the hotspot scatter plot for a specific chromosomome
#' @description The function creates the hotspot plot for a specific selected chromosome.
#'
#' @param dat1 data frame (containing: Chr, SNP, cM, BP, Theta, Dis, ord, size, coloring, and shape)
#' @param fil character contains the selected chromosome
#' @param ranges reactive values containing the brush information
#' @param legend1 data frame containing the legend information (contains: label, breaks, shape, and ord)
#'
#' @details Note: legend1, the column breaks contains the color for the corresponding label, and column ord contains numbers used for sorting to keep the sequence (corresponds to ord in dat1).
#' @import ggplot2
#'
#' @return The function returns a ggplot object.
#' @export
#' @seealso \link{mod_hotspot_server} and \link{mod_bc_hotspot_server}
#'
scatterPlot_hotspot <- function(dat1,fil,ranges,legend1)
{
  BP<-Theta<-coloring<-ord<-NULL

  legend1=legend1[order(as.numeric(legend1$ord)),]
  label1= legend1$label
  breaks1= legend1$breaks
  shape1= as.numeric(legend1$shape)

   p<-ggplot2::ggplot(dat1, aes(x = BP, y = Theta)) +
     geom_point(aes(colour = as.factor(ord)),shape=dat1$shape,size=dat1$size)+
     scale_colour_manual(name="Legend",values = breaks1,labels=label1)+
     guides(colour = guide_legend(override.aes = list(size=5,shape=shape1)))+
     theme(panel.spacing = unit(0.1, "points"))+
    xlim(0,round(max(dat1$BP)+1))+
    xlab("Chromosome position (Mbp)") +
    ylab("Recombination rate")+
    ggtitle(paste0("BTA ",fil))+
    coord_cartesian(xlim = ranges$x, ylim = c(0,max(dat1$Theta)+0.0001), expand = FALSE)+ ##new added necessary for hover
    theme(
      panel.background = element_rect(fill = 'White'),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),legend.position="right")+
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 22))+
    theme(plot.title = element_text(hjust = 0.5))

  p
}
