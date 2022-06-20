# functions
#' @title make plot containers
#'
#' @description The function initialize the framework for the expected number of plots, or make Plot container
#' function to make a bunch of plots
#' @import ggplot2
#' @import shiny
#' @import shinycssloaders
#'
#' @note ## adopted from https://stackoverflow.com/questions/33469773/multiple-plots-in-r-shiny and
#' https://gist.github.com/wch/5436415
#' @noRd

makePlotContainers <- function(n, ncol=2, prefix="plot", height=100, width="100%", ...) {

 shiny::validateCssUnit(width)
 shiny::validateCssUnit(height)

  ## Construct plotOutputs
  if(ncol==1)use=seq.int(n)
  if(ncol>1)
  {
     use=c()
     for(ij in 1:ncol)use=c(use,seq(ij,n,ncol))
  }
  lst <- lapply(use, function(i)
    plotOutput(sprintf("%s_%g", prefix, i), height=height, width=width)%>% withSpinner(color="#0dc5c1"))

  ## Make columns
  lst <- lapply(split(lst, (seq.int(n)-1)%/%(29/ncol)), function(x) column(12/ncol, x)) ##
  do.call(tagList, lst)
}

# functions
#' @title prepareData
#'
#' @description The function prepares a data frame for the genetic map visualization.
#' @import ggplot2
#'
#' @param n number of chromosomes
#' @param input is a list containing the corresponding information for each chromosome
#' @param output internal
#'
#' @note ## adopted from https://stackoverflow.com/questions/33469773/multiple-plots-in-r-shiny and
#' https://gist.github.com/wch/5436415
#' @noRd
renderPlots <- function(n, input, output, prefix="plot") {
   X<-Y<-Approach<-NULL
    for (i in seq.int(n)) {
    local({
     ii <- i  # need i evaluated here
      output[[sprintf("%s_%g",prefix, ii)]] <- renderPlot({
        ggplot(input[[ii]],aes(x=X, y=Y,col=Approach)) +
          geom_point(size=3, na.rm=TRUE)+
          ggtitle(paste0("BTA ",ii))+
          scale_color_manual(values=c("dodgerblue2", "cadetblue3"),labels=c("Deterministic","Likelihood"))+
          theme(axis.ticks=element_line(colour = "black", size = 2),text = element_text(size = 19),plot.title = element_text(hjust = 0.5))+
          labs(x="Physical length (Mbp)",y="Genetic distance (cM)")

      })
    })
    }
}


# functions
#' @title prepareData
#'
#' @description The function prepares a data frame for the genetic map visualization.
#'
#' @param n number of chromosomes
#' @param input data frame
#'
#' @return ll is a list containing for each chromosome the desired information to enable the corresponding ggplot.
#' @noRd
prepareData=function(n,input)
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
  return(ll)
}



makePlot_geneticMaps<-function(chr,dat_maps)
{
   if(length(which(is.na(dat_maps$cM_likelihood)==TRUE))!=0)dat_maps=dat_maps[-which(is.na(dat_maps$cM_likelihood)==TRUE),]

  # p <- plot_ly(dat_maps, x = dat_maps$"Mbp_position", y = dat_maps$"cM_deterministic", type = "scatter",size=20, text=dat_maps[,2],hovertemplate = paste("<b>%{text}</b><br>","%{yaxis.title.text}: %{y:.}<br>","%{xaxis.title.text}: %{x:.}<br>","<extra></extra>"
  # ), mode = "markers",height=800,name="Deterministic",color=I("dodgerblue2"))%>% layout(title=paste0("BTA ",chr),titlefont=list(size=26),
   #                                                                                   xaxis=list(title="Physical length (Mbp)",titlefont=list(size=24),tickfont = list(size = 23)),
  #                                                                                    yaxis=list(title="Genetic distance (cM)",titlefont=list(size=24),tickfont = list(size = 23)),
  #                                                                                    margin=list(t=100)) %>% config(displayModeBar=TRUE,displaylogo = FALSE, modeBarButtonsToRemove = list(
  #                                                                                      'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian','hoverCompareCartesian')) ### Ooms, Jeroen. 2018. Rsvg: Render Svg Images into Pdf, Png, Postscript, or Bitmap Arrays. https://CRAN.R-project.org/package=rsvg. adopted from website: https://plotly-r.com/control-modebar.html 05.08.2021

   p <- plot_ly(dat_maps, x = dat_maps$"Mbp_position", y = dat_maps$"cM_deterministic", type = "scatter",size=20, text=dat_maps[,2],hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"
   ), mode = "markers",height=800,name="Deterministic",color=I("dodgerblue2"))%>% layout(title=list(main=paste0("BTA ",chr),font.main=3, xlab="Physical length (Mbp)",y.lab= "Genetic distance (cM)",font.lab=2),
                                                                                                margin=list(t=100)) %>% config(displayModeBar=TRUE,displaylogo = FALSE, modeBarButtonsToRemove = list(
                                                                                           'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian','hoverCompareCartesian')) ### Ooms, Jeroen. 2018. Rsvg: Render Svg Images into Pdf, Png, Postscript, or Bitmap Arrays. https://CRAN.R-project.org/package=rsvg. adopted from website: https://plotly-r.com/control-modebar.html 05.08.2021

   p=add_markers(p,x=dat_maps$"Mbp_position",y=dat_maps$"cM_likelihood",color=I("cadetblue3"), text=dat_maps[,2],hovertemplate = paste("<b>%{text}</b><br>","Genetic distance (cM): %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"),name="Likelihood")

  return(p)
}



# functions
#' @title initialize to make plots
#'
#' @description The function was included, since without calling makePlotContainers and renderPlots within
#'  the main server is does not worked within the module. May a better solution exists.
#'
#' @param output internal
#' @param session internal
#'
#' @noRd
init_make_plots=function(output,session, geneticMap)
{
  output$plots <- renderUI({makePlotContainers(n=29)})
  ll3=prepareData(n=29,input=geneticMap)
  renderPlots(n=29,input=ll3, output)
}

