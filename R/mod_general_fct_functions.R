#' functions 
#'
#' @title transformdata
#' 
#' 
#' @param data1 contains data frame which should be transformed
#' 
#' @description The function transform the corresponding columns bp into Mbp and M into cM.
#'
#' @return A dataframe.
#' 
#' @export
transformdata=function(data1=data1)
{
  data1[,1]=as.numeric(as.character(data1[,1]))/1000000 
  data1[,2]=as.numeric(as.character(data1[,2]))*100
  data1=cbind(data1,paste("chr ",1:nrow(data1),sep=""))
  colnames(data1)=c("bp","cM","chr")
  data1=as.data.frame(data1)
  data1$bp=as.numeric(data1$bp)
  data1$cM=as.numeric(data1$cM)
  return(data1)
}


# functions
#' @title Creating the Scatter plot
#'
#' @description The corresponding scatter plot is plotted, when all chromosome are selected.
#' 
#' @param dat contains the data frame which is used for plotting
#' @param colo contains the colors which are used
#' 
#' @import ggplot2
#' 
#' @noRd
scatterPlot <- function(dat,colo)
{
    bp<-cM<-Approach<-NULL
    p=ggplot(dat, aes(x=bp, y = cM,col=Approach)) +
    geom_point(size=6,alpha=1)+
    scale_color_manual(values=colo,labels=c("Deterministic","Likelihood"))+
    labs(x="Physical length (Mbp)",y="Genetic distance (cM)")  +
    guides(colour = guide_legend(override.aes = list(size=5)))+   
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 25))
    return(p)
  
}


# functions
#' @title Creating the Scatter plot
#' 
#' @param dat contains the data frame which is used for plotting
#' @param fil filter means here selected chromosome
#' @param colo contains the colors which are used
#'
#' @description The corresponding scatter plot is plotted when a specific chromosome was selected. All chromosomes are presented, whereby the selected one is drawn bigger 
#' @import ggplot2
#' 
#' @noRd
scatterPlot0 <- function(dat,fil,colo)
{
    bp<-cM<-Approach<-NULL
    p=ggplot(dat, aes(x=bp, y = cM,col=Approach)) + 
    geom_point(size = ifelse(1:nrow(dat) == fil, 8, 4),alpha=ifelse(1:nrow(dat) == fil,1,0.3  ))+
    geom_point(size = ifelse(1:nrow(dat) == (nrow(dat)/2)+fil, 8, 4),alpha=ifelse(1:nrow(dat) == (nrow(dat)/2)+fil,1,0.3  ))+
    scale_color_manual(values=colo,labels=c("Deterministic","Likelihood"))+
    labs(x="Physical length (Mbp)",y="Genetic distance (cM)")  +
    guides(colour = guide_legend(override.aes = list(size=5)))+ 
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 25))
    return(p)
}
