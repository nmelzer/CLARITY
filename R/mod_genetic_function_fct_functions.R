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
#' @param name.file character contains name of output file
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
  })%>%shiny::bindCache(breed.select,chromo,"likelihood",cache="app")

  a
}

#'function
#'
#' @title Create barplot for genetic-map functions for selected breeds
#' @description The function combines counts and breed information together, whereby the order based on genetic-map function.
#'
#' @param breed.infos data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param counts numeric containing for each selected breed the determined frequencies for each genetic-map function
#'
#' @import ggplot2
#' @return The function returns a ggplot object.
#' @seealso \link{mod_bc_genetic_function_server}
#' @export
#'

make_barplot_bc<-function(breed.infos,counts)
{
 Breeds<-NULL

  ## order regarding methods
  Methods=c(rep("Haldane scaled",nrow(breed.infos)),rep("Rao",nrow(breed.infos)),rep("Felsenstein",nrow(breed.infos)),rep("Liberman & Karlin",nrow(breed.infos)))
  ord3=rep(1:4,nrow(breed.infos))
  ord3=sort(ord3,index.return=T)
  Counts.ord=counts[ord3$ix]
  plot.data=data.frame(Methods,Breeds=breed.infos$Name,Counts.ord)

  ## create general plot information - order important that the legend and plot is correct
  gen_info=data.frame(Names=breed.infos$Name,Color=breed.infos$Color)
  gen_information=gen_info[order(gen_info[,1]),]

  ## plot barplot
  p2<-ggplot2::ggplot(plot.data, aes(fill=Breeds, y=Counts.ord, x=Methods)) +
    geom_bar(position="dodge", stat="identity")+
    scale_fill_manual(values=gen_information$Color,labels=gen_information$Names,guide = guide_legend(title = "Legend"))+
    xlab("Methods") +
    ylab("Frequency")+
    theme(legend.title = element_text(size=18), #change legend title font size
          legend.text = element_text(size=16),axis.text=element_text(size=14),axis.title=element_text(size=16))

  p2
}

#' function
#' @title Creating plots of genetic-map functions for breed comparison
#' @description For a selected chromosome the curves of all genetic-map functions ("Haldane scaled","Rao","Felsenstein","Liberman & Karlin")
#' are generated for all selected breeds.
#'
#' @param df.list list whereby the first list element contains a matrix of recombination rate and distance for marker pairs, second list element includes a matrix of X-values for all genetic-map functions,
#' third list element includes a matrix of Y-values for all genetic-map functions.
#' @param chromo numeric contains the selected chromosome
#' @param names.bc.plot character vector containing the selected breeds
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
      }
    }
    else
    {
      for(ii in 1:4) p=plotly::add_lines(p,x=df.list[[ih]][,ii],y=df.list[[ih]][,(ii+4)],color=I(pp[ii]),name=paste(names.bc.plot[ih],genetic_functions[ii],sep="-"),line=list(dash=line_type[[ih]]))
    }
  }
  p
}

#' function
#' @title Create table header for genetic map function table for breed analysis
#' @return The function returns a shiny.tag.
#' @noRd
create_table_header4 <- function()
{
  thead<-tr<-th<-NULL
  sketch1 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan=1,""),
      th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
      th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
      th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
      th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')",style='text-decoration-line: underline;'))
    ),
    tr(
      th(colspan=1,"Chromosome"),
      lapply(rep(c('MSE', 'Parameter'), 4), th)
    )
  )
  ))
  sketch1
}

#' function
#' @title Create table header for genetic map function table for breed comparison
#' @param breed.names character containing the selected breed names
#' @return The function returns a shiny.tag.
#' @noRd
create_table_header_bc4 <- function(breed.names)
{
  thead<-tr<-th<-NULL
  if(length(breed.names)==2)
  {
    sketch111 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=1,""),
          lapply(colspan=8,breed.names,th)
        ),
        tr(
          th(colspan=1,""),
          th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')",style='text-decoration-line: underline;'))
        ),
        tr(
          th(colspan=1,"Chromosome"),
          lapply(rep(c('MSE', 'Parameter'), 4*length(breed.names)), th)
        )
      )
    ))
  }
  else{
    sketch111 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan=1,""),
          lapply(colspan=8,breed.names,th)
        ),
        tr(
          th(colspan=1,""),
          th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Haldane scaled", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Rao", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Felsenstein", onclick = "openTab('methodology')",style='text-decoration-line: underline;')),
          th(colspan=2,tags$a(href="#","Liberman & Karlin", onclick = "openTab('methodology')",style='text-decoration-line: underline;'))
        ),
        tr(
          th(colspan=1,"Chromosome"),
          lapply(rep(c('MSE', 'Parameter'), 4*length(breed.names)), th)
        )
      )
    ))
  }
  sketch111
}
