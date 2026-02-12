#' function
#' @title Merge revealed hotspot data for breed analysis and breed comparison
#'
#' @param dat.in list data.frame containing relevant \link{table_approach_general}
#' @param no.selected integer containing  the number for selected breeds or approaches
#'
#' @note Only two or three sets can be combined.
#'
#' @return The function returns a data.frame.
#' @noRd
combine_dat<-function(dat.in,no.selected)
{
  dat.in2<-merge(dat.in[[1]],dat.in[[2]],by.x=2,by.y=2,all.x=TRUE,all.y=TRUE)## marker merged
  if(length(which(is.na(dat.in2$Chr.x)==TRUE))!=0)dat.in2[which(is.na(dat.in2$Chr.x)==TRUE),2:3]=dat.in2[which(is.na(dat.in2$Chr.x)==TRUE),5:6]
  if(no.selected==3)dat.in2=merge(dat.in2,dat.in[[3]],by.x=1,by.y=2,all.x=TRUE,all.y=TRUE)
  if(length(which(is.na(dat.in2$Chr.x)==TRUE))!=0)dat.in2[which(is.na(dat.in2$Chr.x)==TRUE),2:3]=dat.in2[which(is.na(dat.in2$Chr.x)==TRUE),8:9]

  take.out1=grep("Chr",colnames(dat.in2))[-1]
  take.out2=grep("BP",colnames(dat.in2))[-1]

  dat.in3<-dat.in2[,-c(take.out1,take.out2)]
  dat.in3=dat.in3[order(dat.in2$BP.x),]

  dat.in3
}

#' function
#' @title Transform data for hotspot plot for breed analysis
#'
#' @description The function used the threshold to determine hotspot SNP markers and generate corresponding output data for plot (1), table (2) and Venn diagram (3) for a selected breed and selected approaches.
#' 1) Transform BP into Mbp and add accordingly colors, sizes and shapes. If all chromosomes are selected then an additional
#' column is generated "ChrTheta" - chromosome number + theta. The later is scaled depending on the size of Theta.
#' In addition, the maximum length of chromosomes and maximum theta are determined.
#' 2) To create the corresponding table only the detected hotspot SNP markers for each approach are used.
#' 3) For the number of selected approaches, creates corresponding sets containing the hotspot SNP markers required to create the Venn diagram.
#'
#' @param data.trans data frame (containing: SNP,BP,D_cM,D_Theta,Hm_cM,Hm_Theta,Hf_cM,Hf_Theta,Dis)
#' @param value numeric contains the user specified threshold to determine the hotspot
#' @param filter character contains the selected chromosome
#' @param infos data.frame data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#' @param choice character contains the information on whether only hotspots or all points are plotted
#'
#' @note The function only works for a maximum of three breeds.
#'
#' @importFrom stats sd
#' @export
#' @returns The function returns a list containing: \cr
#' * 1. data frame containing information for hotspot plot (\link{data_plot_hotspot}-specific selected chromosome or \link{data_plot_hotspot_all} - all chromosomes is selected) \cr
#' * 2. data.frame for table use created for table (\link{data_table_hotspot}) \cr
#' * 3. list containing revealed hotspot SNP markers for each approach for the Venn diagram \cr
#' * 4. numeric contains the chromosome end of the longest chromosome and the maximum theta for hotspot plot (\link{data_plot_hotspot}-specific selected chromosome or \link{data_plot_hotspot_all} - all chromosomes is selected)
#' @seealso
#' * \link{mod_hotspot_server} \cr
#' * \link{mod_bc_hotspot_server}

transformdata_hotspot=function(data.trans,value,filter,infos,choice)  #
{
  data.trans2<-venn.list<-dat.table<-list()
  Theta<-NULL
  plot.info<-matrix(NA,nrow(infos),2)
  colnames(plot.info)<-c("Length","maxTheta")

  if(filter!="All")data.trans = data.trans[which(as.numeric(filter)==data.trans$Chr),]

  shapes=c(15,16,17)
  for(i in 1:nrow(infos))
   {
      data1=as.data.frame(data.trans[,c(1:3,grep(paste0(infos$Abbreviation[i],"_"),colnames(data.trans)))])
      data1$Chr<-as.numeric(data1$Chr)
      colnames(data1)[4:5]<-c("cM","Theta")

      plot.info[i,1]<-max(data1$BP/1000000,na.rm = TRUE)
      plot.info[i,2]<-max(data1$Theta,na.rm=TRUE)

      if(length(which(is.na(data1[,4])==T))!=0)data1=data1[-which(is.na(data1[,4])==T),] # remove row including NA

      threshold <- mean(data1[,5]) + value * sd(data1[,5])
      if(choice=="Yes")data1<-data1%>%dplyr::filter(Theta>threshold)

      data1$Mbp<-as.numeric(as.character(data1$BP/1000000))
      data1$what <- as.character(unlist(ifelse(data1[,5] > threshold, paste0("Hotspot - ",infos$Approach[i]),paste0("No hotspot - ",infos$Approach[i]))))
      data1$size<-as.numeric(unlist(ifelse(data1[,5] > threshold, 7, 3)))
      data1$coloring=as.character(unlist(ifelse(data1[,5] > threshold, infos$Color[i],infos$Color2[i])))
      if(nrow(data1)>0)data1$shape=as.numeric(shapes[i])
      else data1 = data1 %>% dplyr::mutate(shape = NA) ## necessary if no hotspot available and only hotspot to plot selected

      data.trans2[[i]]<-data1
      dat.table[[i]]=as.data.frame(data1[data1$size%in%7,1:4])
      venn.list[[infos$Abbreviation[i]]]<-data1$SNP[data1$size%in%7]
  }

  plot.info2<-c(max(plot.info[,1]),max(plot.info[,2]))

  ## merge data for table output
  if(nrow(infos)>1)data2<-combine_dat(dat.in=dat.table,no.selected=nrow(infos))
  else data2<-dat.table[[1]][,c(2,1,3,4)]

  data.trans4 <- do.call("rbind",data.trans2)

  if(filter=="All" && nrow(data.trans4)>0)
  {
    scale<-ifelse(max(data.trans4$Theta>0.0099),10,100)
    if(scale==10)scale<-ifelse(max(data.trans4$Theta>0.05),10,20)
    data.trans4$ChrTheta=data.trans4$Chr+(data.trans4$Theta*scale)
  }

  data.list=list(data.trans4,data2[order(data2$Chr),],venn.list,plot.info2)
  data.list
}


#' function
#' @title Transform data for Hotspot plot for breed comparison
#'
#' @description The function used the threshold to determine hotspot SNP markers and generate corresponding output data for plot (1), table (2) and Venn diagram (3) for an selected approach and selected breeds.
#' 1) Transform BP into Mbp and add accordingly colors, sizes and shapes. If all chromosomes are selected then an additional
#' column is generated "ChrTheta" - chromosome number + theta . The later is scaled in depending on the size of Theta.
#' In addition, the maximum length of chromosomes and maximum theta are determined.
#' 2) To create the corresponding table only the detected hotspot SNP markers for each approach are used.
#' 3) For the number of selected approaches, create corresponding sets containing the hotspot SNP markers required to create the Venn diagram.
#'
#' @param data.trans list each list element represents a selected breed and contains data frame (containing: SNP,BP,D_cM,D_Theta,Hm_cM,Hm_Theta,Hf_cM,Hf_Theta,Dis)
#' @param value numeric contains the user specified threshold to determine the hotspot
#' @param filter character contains the selected chromosome
#' @param infos data.frame data frame containing the predefined settings and names for the selected breeds (\link{table_breed_information})
#' @param approach data.frame data frame containing the predefined settings and names for the selected approaches (\link{table_approach_information})
#' @param choice character contains the information on whether only hotspots or all points are plotted
#'
#' @note The function only works for a maximum of three approaches.
#'
#' @importFrom stats sd
#'
#' @export
#' @returns The function returns a list containing: \cr
#' * 1. data frame containing information for hotspot plot (\link{data_plot_hotspot}-specific selected chromosome or \link{data_plot_hotspot_all} - all chromosomes is selected) \cr
#' * 2. data.frame for table use created for table (\link{data_table_hotspot}) \cr
#' * 3. list containing revealed hotspot SNP markers for each approach for the Venn diagram
#' * 4. numeric contains the chromosome end of the longest chromosome and the maximum theta for hotspot plot (\link{data_plot_hotspot}-specific selected chromosome or \link{data_plot_hotspot_all} - all chromosomes is selected)
#' @seealso
#' * \link{mod_hotspot_server} \cr
#' * \link{mod_bc_hotspot_server}

transformdata_hotspot_bc=function(data.trans,value,filter,infos,approach,choice)
{
  data.trans3<-dat.table<-venn.list<-list()
  Theta<-NULL
  plot.info<-matrix(NA,nrow(infos),2)
  colnames(plot.info)<-c("Length","maxTheta")

  shapes=c(15,16,17)
  for(i in 1:nrow(infos))
  {
    data.trans2=data.trans[[i]]
    if(filter!="All")data.trans2 = data.trans2[which(as.numeric(filter)==data.trans2$Chr),]

    data1=as.data.frame(data.trans2[,c(1:3,grep(paste0(approach$Abbreviation[1],"_"),colnames(data.trans2)))])
    data1$Chr<-as.numeric(data1$Chr)
    colnames(data1)[4:5]<-c("cM","Theta")
    if(length(which(is.na(data1[,4])==T))!=0)data1=data1[-which(is.na(data1[,4])==T),] # remove row including NA

    threshold <- mean(data1[,5]) + value * sd(data1[,5])

    plot.info[i,1]<-max(data1$BP/1000000,na.rm = TRUE)
    plot.info[i,2]<-max(data1$Theta,na.rm=TRUE)

    if(choice=="Yes")data1<-data1%>%dplyr::filter(Theta>threshold)

    data1$Mbp=as.numeric(as.character(data1$BP))/1000000
    data1$what <- as.character(unlist(ifelse(data1[,5] > threshold, paste0("Hotspot - ",infos$Name[i]),paste0("No hotspot - ",infos$Name[i]))))
    data1$size<-as.numeric(unlist(ifelse(data1[,5] > threshold, 7, 2)))
    data1$coloring=as.character(unlist(ifelse(data1[,5] > threshold, infos$Color[i],infos$Color2[i])))

    if(nrow(data1)>0)data1$shape=as.numeric(shapes[i])
    else data1 = data1 %>% dplyr::mutate(shape = NA) ## necessary if no hotspot available and only hotspot to plot selected

    data.trans3[[i]]<-data1
    dat.table[[i]]=data1[which(data1$size==7),1:4]

    venn.list[[infos$Abbreviation[i]]]<-data1$SNP[data1$size%in%7]
  }

  data2<-combine_dat(dat.in=dat.table,no.selected=nrow(infos))
  plot.info2<-c(max(plot.info[,1]),max(plot.info[,2]))
  data.trans4 <- do.call("rbind",data.trans3)
  if(filter=="All" && nrow(data.trans4)>0)
  {
    scale<-ifelse(max(data.trans4$Theta>0.0099),10,100)
    if(scale==10)scale<-ifelse(max(data.trans4$Theta>0.05),10,20)
    data.trans4$ChrTheta=data.trans4$Chr+(data.trans4$Theta*scale)
  }

  data.list=list(data.trans4,data2[order(data2$Chr),],venn.list, plot.info2)
  data.list
}


#' function
#' @title Creating the hotspot scatter plot for all chromosomomes
#'
#' @description The function creates the hotspot plot for all chromosomes.
#'
#' @param dat1 data frame \link{data_plot_hotspot_all}
#' @param file.name character name of the output file
#' @param max.plot numeric contains the chromosome end of the longest chromosome and the maximum theta
#'
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return The function returns a plotly object.
#' @export
#' @seealso
#' * \link{mod_hotspot_server}
#' * \link{mod_bc_hotspot_server}

scatterPlot_hotspot_all <- function(dat1,file.name,max.plot)
{
  BP<-ChrTheta<-coloring<-NULL

  dat1=dat1[order(dat1$what),] ## order necessary otherwise legend and coloring is wrong

  p<- plotly::plot_ly(data=dat1, x = ~Mbp, y = ~ChrTheta, color=~what, type = "scatter", mode="markers",marker=list(size=~size),hoverinfo="text", fill=~'',text=I(dat1$SNP), symbol=~shape,
                      colors=unique(dat1$coloring),
                      hovertemplate = paste("<b>Chromosome:</b> ", sprintf("%.0f", dat1$Chr),"<br>","<b>BTA:</b> %{text} <br>","<b>Theta:</b>", sprintf("%.3f", dat1$Theta),"<br>",
                                            "<b>Physical length (Mbp):</b>", sprintf("%.3f", dat1$Mbp),"<br><extra></extra>"), height=1000)%>%
      plotly::layout(xaxis=list(title="Chromosome position (Mbp)",range=c(0,max.plot[1]+0.3)),yaxis=list(title="Chromosome",range=c(0,30),tickvals=c(1:29),ticktext=1:29),showlegend=TRUE) %>%
      plotly::config(displayModeBar=TRUE,toImageButtonOptions= list(filename = file.name))
  p
}

#' function
#' @title Creating the hotspot scatter plot for a specific chromosomome
#' @description The function creates the hotspot plot for a specific selected chromosome.
#'
#' @param dat1 data frame \link{data_plot_hotspot}
#' @param file.name character containing the file name
#' @param max.plot  numeric contains the chromosome end of the longest chromosome and the maximum theta
#'
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return The function returns a plotly object.
#' @export
#' @seealso
#' * \link{mod_hotspot_server}
#' * \link{mod_bc_hotspot_server}

scatterPlot_hotspot <- function(dat1,file.name,max.plot)
{
  BP<-Theta<-coloring<-ord<-NULL

  dat1=dat1[order(dat1$what),] ## order necessary otherwise legend and coloring is wrong #

  p<- plotly::plot_ly(data=dat1, x = ~Mbp, y = ~Theta, color=~what, type = "scatter", mode="markers",marker=list(size=~size),hoverinfo="text", fill=~'',text=I(dat1$SNP), symbol=~shape,
                     colors=unique(dat1$coloring),
                     hovertemplate = paste("<b>BTA: %{text}</b><br>","Theta: %{y:.}<br>","Physical length (Mbp): %{x:.}<br>","<extra></extra>"))%>%  # text
      plotly::layout(legend=list(title=list(text="Legend",font=list(size=15,family="Arial")),font=list(family="Arial",size=15)),
                     title=list(text=paste0("BTA ",unique(dat1$Chr)),font=18),
                     xaxis=list(range=c(0,max.plot[1]+0.3),title="Chromosome position (Mbp)"),
                     yaxis=list(range=c(0,max.plot[2]+(max.plot[2]/4)),title="Recombination rate"),
                     showlegend=TRUE) %>%
      plotly::config(displayModeBar=TRUE,toImageButtonOptions= list(filename = file.name))
  p
}

#' function
#' @title Creates table header for hotspot table for breed analysis
#' @return The function returns a shiny.tag.
#' @noRd
#'
create_table_header3 <- function(approach.info)
{
  thead<-tr<-th<-NULL
  sketch3 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=1,""),
        th(colspan=2,"Physical Map"),
        th(colspan=nrow(approach.info),"Genetic Map")
      ),
      tr(
        th(colspan=1,"Marker"),
        th(colspan=1,"Chromosome"),
        th(colspan=1,"Position (bp)"),

        lapply(1:nrow(approach.info), function(inn) {
          th(colspan=1,HTML(paste(approach.info$Name[inn],"<br> Position (cM)")))
        })
      )
    )
  ))
  sketch3
}


#' function
#' @title Creates table header for hotspot table for breed comparison
#' @param breed.select.bc character containing the selected breed names
#' @return The function returns a shiny.tag.
#' @noRd
create_table_header_bc3 <- function(breed.select.bc)
{
  thead<-tr<-th<-NULL
  sketch2 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan=1,""),
        th(colspan=2,"Physical Map"),
        th(colspan=length(breed.select.bc),"Genetic Map")
      ),
      tr(
        th(colspan=1,"Marker"),
        th(colspan=1,"Chromosome"),
        th(colspan=1,"Position (bp)"),

        lapply(1:length(breed.select.bc), function(inn) {
          th(colspan=1,HTML(paste(breed.select.bc[inn],"<br> Position (cM)")))
        })
      )
    )
  ))
  sketch2
}
