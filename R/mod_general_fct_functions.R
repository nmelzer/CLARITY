#' function
#' @title Transform data for breed analysis
#'
#' @description The function transforms the data for plotting: basepairs (bp) into megabasepairs (Mbp) and Morgan (M) into centiMorgan (cM).
#' @return The function returns a data frame with three columns: basepairs (bp), centiMorgan (cM) and chromosome (chr).
#'
#' @param data1 data frame
#' @seealso \link{mod_general_server}
#'
#' @export
transformdata_general=function(data1)
{
  data1[,1]=as.numeric(as.character(data1[,1]))/1000000
  data1[,2]=as.numeric(as.character(data1[,2]))*100
  data1=cbind(data1,1:nrow(data1))
  colnames(data1)=c("bp","cM","chr")
  data1=as.data.frame(data1)
  data1$bp=as.numeric(data1$bp)
  data1$cM=as.numeric(data1$cM)
  data1
}


#' function
#' @title Transform data for breed comparison
#'
#' @param data1 data frame
#' @param what character contains the name of the approach
#' \itemize{
#' \item det - is used for deterministic approach
#' \item lik - is used for likelihood-based approach
#' }
#' @param breed.select.bc vector containing the selected breeds
#' @param colo vector containing the predefined colors for each breed
#' @param filter character contains the selected chromosome
#'
#' @description The function transforms the data for plotting: 1) basepairs (bp) into megabasepairs (Mbp) and Morgan (M) into centiMorgan (cM) for selected breeds,
#' 2) adds breed specific colors, and
#' 3) adds geom_point information for the ggplot accordingly to the selected chromosome: alpha values ("0.4" not selected chromosome and "1" selected chromosome)
#'  and size values ("4" not selected chromosome and "8" selected chromosome)
#'
#'
#' @return The function returns a data frame with six columns: basepair (bp), centiMorgan (cM), chromosome (chr), breed, alpha, size and color.
#'
#' @seealso \link{mod_bc_general_server}
#' @export
transformdata_general_bc=function(data1,what,breed.select.bc,colo,filter)
{

  dat.0=c()
  for(i in 1:length(breed.select.bc))
  {
    if(what=="det")use=c(3,7)
    else use=c(3,9)

    dat=data1[[i]][-nrow(data1[[i]]),use]
    dat[,1]=as.numeric(as.character(dat[,1]))/1000000
    dat[,2]=as.numeric(as.character(dat[,2]))*100
    dat=cbind(dat,1:nrow(dat))
    dat=cbind(dat,rep(breed.select.bc[i],nrow(dat)),rep(1,nrow(dat)),rep(4,nrow(dat)))
    colnames(dat)=c("bp","cM","chr","breed","alpha","size")
    dat=as.data.frame(dat)
    dat$alpha=as.numeric(dat$alpha)
    dat$size=as.numeric(dat$size)
    if(filter!="All")dat$size[as.numeric(filter)]=8
    dat$bp=as.numeric(dat$bp)
    dat$cM=as.numeric(dat$cM)
    color=rep(colo[i,1],nrow(dat))
    dat=cbind(dat,color)
    if(i==1) dat.0=dat
    else dat.0=rbind(dat.0,dat)
  }
  dat.0
}


#' function
#' @title Creating the scatter plot for all chromosomes within breed analysis
#' @description The scatter plot is created, when all chromosomes are selected within breed analysis.
#'
#' @param dat data frame containing basepairs (bp), centiMorgan (cM), chromosome (chr), color and approach
#' @param colo vector containing the predefined colors
#'
#' @import ggplot2
#' @return The function returns a ggplot object.
#'
#' @seealso \link{mod_general_server}
#'
#' @export
scatterPlot_general_all <- function(dat,colo)
{
    bp<-cM<-Approach<-NULL
    p=ggplot2::ggplot(dat, aes(x=bp, y = cM,col=Approach)) +
    geom_point(size=6,alpha=1)+
    scale_color_manual(values=colo,labels=c("Deterministic","Likelihood"))+
    labs(x="Physical length (Mbp)",y="Genetic distance (cM)")  +
    guides(colour = guide_legend(override.aes = list(size=5)))+
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 18))
    p
}

#' function
#' @title Creating the scatter plot for selected chromosome within breed analysis
#'
#' @param dat data frame containing basepairs (bp), centiMorgan (cM), chromosome (chr), color and approach
#' @param fil selected chromosome
#' @param colo vector containing the predefined colors
#'
#' @description The corresponding scatter plot is created, when a specific chromosome was selected within breed analysis.
#' @import ggplot2
#'
#' @return The functions returns a ggplot object.
#' @seealso \link{mod_general_server}
#' @export
scatterPlot_general_selected <- function(dat,fil,colo)
{
    bp<-cM<-Approach<-NULL
    p=ggplot2::ggplot(dat, aes(x=bp, y = cM,col=Approach)) +
    geom_point(size = ifelse(1:nrow(dat) == fil, 8, 4))+#,alpha=ifelse(1:nrow(dat) == fil,1,0.3  ))+
    geom_point(size = ifelse(1:nrow(dat) == (nrow(dat)/2)+fil, 8, 4))+#,alpha=ifelse(1:nrow(dat) == (nrow(dat)/2)+fil,1,0.3  ))+
    scale_color_manual(values=colo,labels=c("Deterministic","Likelihood"))+
    labs(x="Physical length (Mbp)",y="Genetic distance (cM)")  +
    guides(colour = guide_legend(override.aes = list(size=5)))+
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 18))
   p
}

#' function
#' @title Creating the scatter plot for all chromosomes for breed comparison
#' @description The corresponding scatter plot is created, when all chromosome are selected for the breed comparison.
#' @param dat data frame containing basepairs (bp), centiMorgan (cM), chromosome (chr), breed and color
#' @param names.bc.plot vector containing the names of selected breeds
#' @param colo vector containing the predefined colors for the breeds
#' @import ggplot2
#'
#' @return The functions returns a ggplot object.
#' @seealso \link{mod_bc_general_server}
#' @export
scatterPlot_general_all_bc <- function(dat,names.bc.plot,colo)
{
  bp<-cM<-breed<-NULL
  p=ggplot2::ggplot(dat, aes(x=bp, y = cM,col=breed)) +
    geom_point(size=6,alpha=1)+
    scale_color_manual(values=colo[1:length(names.bc.plot),1],labels=names.bc.plot)+
    labs(x="Physical length (Mbp)",y="Genetic distance (cM)")  +
    guides(colour = guide_legend(override.aes = list(size=5)))+
    theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 25))
  p

}

#' function
#' @title Creating the scatter plot for selected chromosome for breed comparison
#'
#' @description The corresponding scatter plot is created, when a specific chromosome is selected for the breed comparison.
#' @param dat data frame containing basepairs (bp), centiMorgan (cM), chromosome (chr), breed, alpha, size and color
#' @param names.bc.plot vector containing the names of selected breeds
#' @param colo vector containing the predefined colors
#'
#' @import ggplot2
#' @return The functions returns a ggplot object.
#'
#' @seealso \link{mod_bc_general_server}
#' @export
scatterPlot_general_selected_bc <- function(dat,names.bc.plot,colo)
{
  bp<-cM<-Approach<-breed<-NULL

  p=ggplot2::ggplot(dat, aes(x=bp, y = cM, col=breed)) +
  geom_point(size = dat$size,alpha=dat$alpha)+
  scale_color_manual(values=colo[1:length(names.bc.plot),1],labels=names.bc.plot)+
  labs(x="Physical length (Mbp)",y="Genetic distance (cM)")  +
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 25))
  p
}

