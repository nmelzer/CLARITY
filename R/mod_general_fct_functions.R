#' function
#' @title Transform data for breed analysis
#'
#' @description The function transforms the data for plotting:
#' 1) basepairs (bp) into megabasepairs (Mbp) and Morgan (M) into centiMorgan (cM) for selected approaches,
#' 2) adds approach specific colors and approach names, and
#' 3) adds size values ("10" not selected chromosome and "18" selected chromosome; "14" when all chromosomes are selected)
#' Moreover, the transformed data are also used in function \link{create.output} to create the correlation output.
#'
#' @param data1 data frame containing the genetic map summary for selected approaches for chosen breed
#' @param approach.info data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#' @param filter character contains the selected chromosome
#'
#' @importFrom dplyr mutate if_else row_number
#'
#' @return The function returns a data.frame (\link{data_plot_general})
#'
#' @seealso
#' * \link{mod_general_server} \cr
#' * \link{create.output}
#' @export
transformdata_general=function(data1,approach.info,filter)
{
  use=grep("_M",colnames(data1))

  dat1<-lapply(1:length(use),function(i)
  {
    dat=as.data.frame(data1[,c(3,use[i],1)])
    colnames(dat)=c("Mbp","cM","Chr")
    dat <- dat%>%dplyr::mutate(
      Mbp=as.numeric(dat$Mbp)/1000000,
      cM= as.numeric(dat$cM)*100,
      Color=approach.info$Color[i],
      What=approach.info$Approach[i]
    )
    if(filter!="All")dat <- dat %>% dplyr::mutate(Size = dplyr::if_else(dplyr::row_number() %in% as.numeric(filter), 18, 10))
    else  dat <- dat %>% dplyr::mutate(Size = 14)
  })

  dat1.table=do.call("rbind",dat1)
  dat1.table
}

#' function
#' @title Transform data for breed comparison
#'
#' @param data1 list each list element represents a chosen breed and contains a data frame with genetic map summary for selected approach.
#' @param breed.infos data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param filter character contains the selected chromosome
#'
#' @importFrom dplyr mutate if_else row_number
#'
#' @description The function transforms the data for plotting:
#' 1) basepairs (bp) into megabasepairs (Mbp) and Morgan (M) into centiMorgan (cM) for selected breeds,
#' 2) adds breed specific colors and breed name, and
#' 3) adds size values ("10" not selected chromosome and "18" selected chromosome; 14 when all chromosomes are selected)
#'
#' @return The function returns a data frame (\link{data_plot_general})
#'
#' @seealso \link{mod_bc_general_server}
#' @export
transformdata_general_bc=function(data1,breed.infos,filter)
{
  use=c(3,grep("_M",colnames(data1[[1]])),1)

  dat0<-lapply(1:nrow(breed.infos), function(i)
  {
    dat=data1[[i]][-nrow(data1[[i]]),use]
    colnames(dat)=c("Mbp","cM","Chr")
    dat <- dat%>%dplyr::mutate(
      Mbp=as.numeric(dat$Mbp)/1000000,
      cM=as.numeric(dat$cM)*100,
      Color=breed.infos$Color[i],
      What=breed.infos$Name[i]
    )
    if(filter!="All")dat <- dat %>% dplyr::mutate(Size=dplyr::if_else(dplyr::row_number() %in% as.numeric(filter), 18, 10))
    else dat<-dat %>% dplyr::mutate(Size = 14)

  })
  dat0
  dat0.table=do.call("rbind",dat0)
  dat0.table
}

#' function
#' @title Creating the general scatter plot
#' @description The corresponding scatter plot is created, when a specific chromosome or all chromosomes was selected within breed analysis or breed comparison.
#'
#' @param dat data frame containing the corresponding plot information (\link{data_plot_general})
#' @param name.file character containing the name used for the generated plot
#'
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return The functions returns a plotly object.
#'
#' @seealso
#'  * \link{mod_general_server} \cr
#'  * \link{mod_bc_general_server}
#' @export

scatterPlot_general <- function(dat,name.file)
{
  dat=dat[order(dat$What),]

  hover_info<-paste(
    "<b>Chromosome: <b>",dat$Chr,"<br>",
    "<b>Physical distance (Mbp): <b>",round(dat$Mbp,2),"<br>",
    "<b>Genetic distance (cM): <b>",round(dat$cM,2),"<br>"
    )

  p <- plotly::plot_ly(data=dat, x = ~Mbp, y = ~cM,color=~What, type = "scatter", mode="markers",marker=list(size=~Size),hoverinfo="text",hovertext=hover_info, fill=~'',colors=~unique(dat$Color))%>%
       plotly::layout(hovermode="closest",plot_bgcolor = '#F5F5F5',
                   xaxis = list(
                     title=list(text="Physical length (Mbp)",font=list(family="Arial",size=16)),
                     tickfont = list(size = 14),
                     showgrid = TRUE,
                     gridcolor = 'white',
                     fixedrange = TRUE
                   ),
                   yaxis = list(
                     title=list(text="Genetic distance (cM)",font=list(family="Arial",size=16)),
                     tickfont = list(size = 14),
                     showgrid = TRUE,
                     gridcolor = 'white',
                     fixedrange = TRUE
                   ),
                   legend=list(title=list(text="Legend",font=list(size=18,family="Arial")),font=list(family="Arial",size=16)))%>%
        plotly::config(displayModeBar=TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud','zoom2d', 'autoScale2d', 'hoverClosestCartesian','hoverCompareCartesian', 'pan2d',
                                                                                            'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d','resetScale2d'), toImageButtonOptions= list(filename = name.file))
p
}

#' function
#' @title Creating the correlation output for selected approaches (breed analysis)
#' @description The Pearson correlations between physical and genetic length are calculated for all selected approaches and summarized in a common output.
#'
#' @param data.trans data frame \link{data_plot_general}
#' @param approach.info data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#'
#' @importFrom stats cor
#' @return The function returns a character string.
#' @seealso \link{mod_general_server}
#' @export

create.output<-function(data.trans,approach.info)
{
  zz<-unlist(sapply(1:nrow(approach.info),function(inn) (round(stats::cor(data.trans$Mbp[data.trans$What%in%approach.info$Approach[inn]],data.trans$cM[data.trans$What%in%approach.info$Approach[inn]],method="pearson"),3))))

  if(nrow(approach.info)==1){
    output =paste0("The Pearson correlation (r) between physical and genetic length was r=",zz[1]," for the  <a href='#",approach.info$Name[1],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[1]),"</u></a>.")
  }else if(nrow(approach.info)==2){
    output =paste0("The Pearson correlation (r) between physical and genetic length was r=",zz[1]," for the  <a href='#",approach.info$Name[1],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[1]),"</u></a> and
                   r=",zz[2]," for the <a href='#",approach.info$Name[2],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[2]),"</u></a>.")
  }else if (nrow(approach.info)==3){
    output =paste0("The Pearson correlation (r) between physical and genetic length was r=",zz[1]," for the <a href='#",approach.info$Name[1],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[1]),",
                    </u></a> r=",zz[2]," for the <a href='#",approach.info$Name[2],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[2]),"</u></a> and r=",zz[3],"
                   for the  <a href='#",approach.info$Name[3],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[3]),"</u></a>.")
  }else{
    output =paste0("The Pearson correlation (r) between physical and genetic length was r=",zz[1]," for the <a href='#",approach.info$Name[1],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[1]),",
                    </u></a> r=",zz[2]," for the <a href='#",approach.info$Name[2],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[2]),", </u></a> r=",zz[3],"
                   for the  <a href='#",approach.info$Name[3],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[3]),"</u></a> and
                    r=",zz[4]," for the <a href='#",approach.info$Name[4],"' onclick = openTab('methodology')><u>",tolower(approach.info$Name[4]),"</u></a>.")
  }
  output
}

#' function
#' @title Creates table header for the general table for breed analysis
#'
#' @param approach.info data frame containing the predefined settings and names for the selected approaches \link{table_approach_general}
#' @return The function returns a shiny.tag.
#'
#' @noRd
create_table_header1 <- function(approach.info) {
  tr<-thead<-th<-NULL

  need.col2<-c()
  need.col2= unlist(sapply(1:nrow(approach.info), function(inn)
    (if(approach.info$Abbreviation[inn]!="Lm")paste0(approach.info$Abbreviation[inn],c("_nRec","_M","_cM/Mb"))
     else paste0(approach.info$Abbreviation[inn],c("_M","_cM/Mb")))
  ))

  sketch0 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=1,""),
        th(colspan=4,"Physical Map"),
        th(colspan=length(need.col2),"Genetic Map"),
      ),
      tr(
        th(colspan=1,""),
        th(colspan=4,""),
        lapply(1:nrow(approach.info), function(inn) {
          if(approach.info$Approach[inn]!="Likelihood_male") {
            th(colspan = 3, htmltools::tags$a(href="#", approach.info$Name[inn], onclick = "openTab('methodology')", style='text-decoration-line: underline'))
          } else {
            th(colspan = 2, htmltools::tags$a(href="#", approach.info$Name[inn], onclick = "openTab('methodology')", style='text-decoration-line: underline;'))
          }
        }),
      ),
      tr(
        th(colspan=1,"Chr"),
        th(colspan=1,"nSNP"),
        th(colspan=1,"bp"),
        th(colspan=1,"Gap"),
        th(colspan=1,"Space"),

        lapply(1:length(need.col2), function(inn) {
          th(colspan=1, need.col2[inn])
        })
      )
    )
  ))

  sketch0
}

#' function
#' @title Creates table header for general table for breed comparison
#'
#' @param breed.infos data frame containing the predefined settings and names for the selected breed  (\link{table_breed_general})
#' @param label numeric vector containing the
#' @param no.cols numeric
#' @param header.length numeric how many columns are necessary for the genetic map part
#' @return The function returns a shiny.tag.
#'
#' @noRd
create_table_header_bc1<-function(breed.infos,label,no.cols,header.length)
{

  label.new=c("nSNP","bp","Gap","Space","nRec","M","cM/Mb")

  var.label=unlist(lapply(1:nrow(breed.infos), function(ik){
    if(no.cols==7)c("Chr",paste0(label.new[-5],".",breed.infos$Abbreviation[ik]))
    else c("Chr",paste0(label.new,".",breed.infos$Abbreviation[ik]))
  }))

  var.label1=var.label[order(as.numeric(label))]
  var.label=var.label1[-c(2:nrow(breed.infos))]

  pp=(5*nrow(breed.infos)-(nrow(breed.infos)-1))

  tr<-thead<-th<-NULL
  sketch0=htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=1,""),
        lapply(colspan=pp-1, "Physical Map", th),
        lapply(colspan=(header.length-pp), "Genetic Map", th),
      ),
      tr(
        lapply(1:length(var.label), function(inn) {
          th(colspan=1, var.label[inn])
        })
      )
    )
  ))
  sketch0
}

#' function
#' @title Creates the table legend for the general table within breed analysis
#'
#' @param approach.info data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#' @return The function returns a character vector containing the legend for the created table in \link{mod_general_server}.
#'
#' @noRd
create.legend.table<-function(approach.info)
{
  variable.explanation<-lapply(1:nrow(approach.info), function(x){
    a<-b<-c<-c()
    if(approach.info$Approach[x]!="Likelihood_male")a=paste0(approach.info$Abbreviation[x],"_nRec: number of cross-overs detected based on ",approach.info$Name[x],"<br>")
    b=paste0(approach.info$Abbreviation[x],"_M: genetic length in Morgan estimated with the ",approach.info$Name[x],"<br>")
    c=paste0(approach.info$Abbreviation[x],"_cM/Mb: centiMorgan per megabase pair for the " ,approach.info$Name[x],"<br>")
    c(a,b,c)
  })

  combine<- paste0(c("Chr: chromosome<br>nSNP: number of SNPs<br>", "bp: chromosome length in base pairs<br>",
                     "Gap: maximum gap size between pairs of adjacent markers in bp<br>", "Space: inter-marker space in kilobase (kb) <br>",
                     unlist(variable.explanation)))
  combine
}
