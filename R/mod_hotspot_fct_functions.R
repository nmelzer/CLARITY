
#' @importFrom stats sd
#' @noRd


transformdata1=function(data.trans=data.trans,value=value)
{
  data.trans$Theta= as.numeric(as.character(data.trans$Theta))

  threshold <- mean(data.trans$Theta) + value * sd(data.trans$Theta)
  data.trans$cols <- ifelse(data.trans$Theta > threshold, 1, 8)    #6   ## 8 - grau
  data.trans$cols=as.factor(data.trans$cols) ## necessary otherwise continuous and is mixed

  data.trans=cbind(data.trans,data.trans$cols)
  colnames(data.trans)[dim(data.trans)[2]]="size"
  data.trans$size=ifelse(data.trans$Theta > threshold, 3, 1)  # 2, 0.1
  data.trans=cbind(data.trans,as.character(data.trans$cols))
  colnames(data.trans)[dim(data.trans)[2]]="coloring"
  data.trans$coloring=ifelse(data.trans$Theta > threshold, "orange","darkgray")

  return(data.trans)
}


scatterPlot1 <- function(dat1=dat1,ranges=ranges,scale=scale)
{
  BP<-ChrTheta<-coloring<-NULL
  p=ggplot(dat1, aes(x = BP, y = ChrTheta)) +
    geom_point(aes(colour = coloring),size=dat1$size) +
    guides(colour = guide_legend(reverse=T,override.aes = list(size=5)))+  ## for reordering the legend
    scale_colour_identity(guide="legend",name="",labels=c("No hotspot","Hotspot")) +  ## that only the two chosen colors are working and also guide=legend is necessary otherwise no legend is plotted
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
  return(p)
}

scatterPlot2 <- function(dat1,fil,ranges,scale)
{
   BP<-Theta<-coloring<-NULL
   p=ggplot(dat1, aes(x = BP, y = Theta)) +
    geom_point(aes(colour = coloring),size=dat1$size) +
    guides(colour = guide_legend(reverse=T,override.aes = list(size=5)))+  ## for reordering the legend
    scale_colour_identity(guide="legend",name="",labels=c("No hotspot","Hotspot")) +  ## that only the two chosen colors are working and also guide=legend is necessary otherwise no legend is plotted
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
   return(p)
}


