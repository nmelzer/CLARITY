#' functions
#'
#' @title makePlot
#' @description A fct function to plot the
#' @return Returns the plot to the mod_genetic_funtion.
#'
#'
#' @noRd
#'
makePlot=function(chromo=chromo,df.list=df.list)  ## has to be extended then for breed
{
  genetic_functions=c("Haldane scaled","Rao","Felsenstein","Liberman & Karlin")

  xs=as.data.frame(df.list[[2]])
  ys=as.data.frame(df.list[[3]])

  # Plotly object
  p <- plotly::plot_ly(as.data.frame(df.list[[1]]), x = ~dist_M, y = ~theta, type = "scatter", mode = "markers",hoverinfo="none",name="SNP data",color=I("lightgray"))%>%
    layout(title=paste0("BTA ",chromo),xaxis=list(title="Genetic distance (M)"),yaxis=list(title="Recombination rate"))%>%
    config(displayModeBar=TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list( 'sendDataToCloud', 'autoScale2d','hoverClosestCartesian',
                                                                                    'hoverCompareCartesian'))
  ########################################### new add selection of chromosome

  pp=c("darkred","orange","blue","darkblue")
  names=c("","darkred","orange","blue","darkblue")
  for(i in 1:4)p=add_lines(p,x=xs[,i],y=ys[,i],color=I(pp[i]),name=genetic_functions[i])

  return(p)
}




#check_bta_marker_length=function(chromo=chromo,max.plot=max.plot)
#{
  ##
 # store=NULL

  #load(system.file("extdata",paste0("curve-short-",chromo,".RData"),package="CLARITY"))  ## 13.06.2022 - only markers are used to see shape of the curve

  #df2= as.data.frame(store[[1]])
  #colnames(df2)=c("x","y")

##  gg=sqrt((df[2:dim(df)[1],1]-df[1:(dim(df)[1]-1),1])^2  + (df[2:dim(df)[1],2]-df[1:(dim(df)[1]-1),2])^2  )  ## take out marker determinations to time intensive -better to connect to a database
##  cc=1; c2=0.0
##  if(dim(df)[1]>=max.plot)
##  {
##    while(cc!=0)
#    {
#      pos=which(gg > (0.03-c2))
#      if(length(pos)>=max.plot)cc=0
##      else c2=c2+0.001
  #  }
  #  df2=df[pos,]
  #}
#  else  df2=df

 # prozent=(dim(df2)[1]*100)/dim(df)[1] ##

 # return(list(df2,prozent,store))
#}


