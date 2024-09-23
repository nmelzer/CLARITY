#' function
#'
#' @title Creating plots of genetic-map functions for breed analysis
#'
#' @description For a selected chromosome the scatter plot of recombination rates vs. distances and curves of all genetic-map functions ("Haldane scaled","Rao","Felsenstein","Liberman & Karlin")
#' are generated.
#'
#' @details The plots are cached to improve the app performance.
#'
#' @param chromo character contains the selected chromosome
#' @param df.list list first element contains a matrix of recombination rate and distance for marker pairs, second element includes a matrix of X-values for all genetic-map functions,
#' third element includes a matrix of Y-values for all genetic-map functions.
#' @param name.file character contains name of output file.
#' @param breed.select character contains the selected breed
#'
#' @importFrom shiny bindCache
#' @rawNamespace import(plotly, except = last_plot)
#' @return The functions returns an already rendered plotly object.
#' @export
#' @seealso \link{mod_genetic_function_server}
#'
makePlot_genetic_function=function(chromo=chromo,df.list=df.list,name.file,breed.select)  ##
{
  a<-plotly::renderPlotly({
  genetic_functions=c("Haldane scaled","Rao","Felsenstein","Liberman & Karlin")

  xs=as.data.frame(df.list[[2]])
  ys=as.data.frame(df.list[[3]])

  p <- plotly::plot_ly(as.data.frame(df.list[[1]]), x = ~dist_M, y = ~theta, type = "scatter", mode = "markers",hoverinfo="none",name="SNP data",color=I("lightgray"))%>%
    plotly::layout(title=paste0("BTA ",chromo),xaxis=list(title="Genetic distance (M)"),yaxis=list(title="Recombination rate"))%>%
    plotly::config(displayModeBar=TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d','hoverClosestCartesian',
                                                                                    'hoverCompareCartesian'), toImageButtonOptions= list(filename = name.file))
  pp=c("darkred","orange","blue","darkblue")

  for(i in 1:4)p=plotly::add_lines(p,x=xs[,i],y=ys[,i],color=I(pp[i]),name=genetic_functions[i])

  p%>% plotly::toWebGL()
  })%>%shiny::bindCache(breed.select,chromo,cache="app")

  a
}


#' function
#' @title Creating plots of genetic-map functions for breed comparison
#' @description For a selected chromosome the curves of all genetic-map functions ("Haldane scaled","Rao","Felsenstein","Liberman & Karlin")
#' are generated for all selected breeds.
#'
#' @param df.list list whereby the first list element contains a matrix of recombination rate and distance for marker pairs, second list element includes a matrix of X-values for all genetic-map functions,
#' third list element includes a matrix of Y-values for all genetic-map functions.
#' @param chromo numeric contains the selected chromosome
#' @param names.bc.plot vector containing the selected breeds
#' @param name.file character name of output file
#'
#' @rawNamespace import(plotly, except = last_plot)
#' @return The function returns a plotly object.
#' @export
#' @seealso \link{mod_bc_genetic_function_server}

makePlot_genetic_function_bc=function(chromo,df.list,names.bc.plot,name.file)  ##
{
  genetic_functions=c("Haldane scaled","Rao","Felsenstein","Liberman & Karlin") ##
  line_type=c("","dash","dot")
  pp=c("darkred","orange","blue","darkblue")

  for(ih in 1:length(names.bc.plot))
  {
    if(ih==1)
    {
     p <- plotly::plot_ly(as.data.frame(df.list[[ih]]), x = ~X1, y = ~Y1, type = "scatter", mode = "lines",hoverinfo="none",name=paste(names.bc.plot[ih],genetic_functions[ih],sep="-"),color=I("darkred"))%>%
      plotly::layout(title=paste0("BTA ",chromo),xaxis=list(title="Genetic distance (M)"),yaxis=list(title="Recombination rate"))%>%
      plotly::config(displayModeBar=TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list( 'sendDataToCloud', 'autoScale2d','hoverClosestCartesian',
                                                                                    'hoverCompareCartesian'), toImageButtonOptions= list(filename = name.file))
   for(ii in 2:4)
     {
        p=plotly::add_lines(p,x=df.list[[ih]][,ii],y=df.list[[ih]][,(ii+4)],color=I(pp[ii]),name=paste(names.bc.plot[ih],genetic_functions[ii],sep="-"))
  }}
    else
    {

      for(ii in 1:4) p=plotly::add_lines(p,x=df.list[[ih]][,ii],y=df.list[[ih]][,(ii+4)],color=I(pp[ii]),name=paste(names.bc.plot[ih],genetic_functions[ii],sep="-"),line=list(dash=line_type[[ih]]))
    }
  }
  p
}

