#' function
#' @title Prepare data for Venn plot
#' @description This function prepares the data for the Venn plot.
#' Moreover the shape of the Venn diagram is set within the function
#' (two sets: shape is set to 201; three sets: shape is set to 301).
#'
#' @param venn object of class "Venn"
#'
#' @importFrom ggVennDiagram process_data
#'
#' @return The function returns the processed Venn data object (containing the plot data).
#' @seealso
#' * \link{mod_bc_general_server} \cr
#' * \link{mod_bc_genetic_map_server} \cr
#' * \link{mod_bc_hotspot_server} \cr
#' * \link{mod_hotspot_server}
#' @export
process_venn_data=function(venn)
{
  shapes.id=ifelse(length(names(venn))==2,"201","301") ## determine shape of venn diagram
  venn.data=ggVennDiagram::process_data(venn,shape_id = shapes.id)
  venn.data
}

#'function
#'@title Creating colors for venn.diagram
#'@description The function creates the colors for the intersection subsets for the Venn diagram to be plotted based on the predefined colors for approaches or breeds.
#'Venn diagram colors are possible to create for two or three sets.
#'
#'@param colo character vector containing the predefined colors for selected breeds or approaches
#'
#'@importFrom grDevices colorRampPalette
#'
#'@return The function returns a character vector containing all colors for Venn diagram to be plotted
#'@seealso
#' * \link{mod_hotspot_server} \cr
#' * \link{mod_bc_general_server} \cr
#' * \link{mod_bc_genetic_map_server} \cr
#' * \link{mod_bc_hotspot_server}
#'@export
create.colors<-function(colo)
{
  def.sets=cbind(c(1,1,2,4),c(2,3,3,5))
  use=ifelse(length(colo)==2,1,4)
  for(ik in 1:use)colo=c(colo,grDevices::colorRampPalette(colo[c(def.sets[ik,1],def.sets[ik,2])])(10)[5])
  colo
}

#' function
#' @title Creating the Venn diagram plot
#' @description This function is used to create the legend text and the text used for each Venn set. The Venn diagram plot is then created.
#'
#' @param venn_data object of class "Venn" processed data (containing the plot data; created in \link{process_venn_data})
#' @param long.names vector containing the full names of selected breeds or approaches
#' @param abbreviations vector containing the predefined abbreviations for selected breeds or approaches
#' @param venn.colors vector containing the colors for the Venn sets (\link{create.colors})
#' @return The function returns the plotted Venn diagram (ggplot object).
#' @import ggplot2
#' @importFrom ggVennDiagram venn_regionedge venn_regionlabel
#' @seealso
#' * \link{mod_bc_general_server} \cr
#' * \link{mod_bc_genetic_map_server} \cr
#' * \link{mod_bc_hotspot_server} \cr
#' * \link{mod_hotspot_server}
#' @export

creating_venn=function(venn_data,long.names,abbreviations,venn.colors)
{
  X<-Y<-NULL

  ## create legend for venn
  labelling.venn=paste(abbreviations,long.names,sep="-")

  def.sets=cbind(c(1,1,2),c(2,3,3))
  use=ifelse(length(abbreviations)==2,1,3)

  for(ik in 1:use)labelling.venn=c(labelling.venn, paste(abbreviations[def.sets[ik,1]],abbreviations[def.sets[ik,2]],sep=".."))
  if(length(long.names)!=2)labelling.venn=c(labelling.venn,paste(abbreviations[1],abbreviations[2],abbreviations[3],sep="..")) ## last set

  ## create abbreviation for venn plot
  labelling.venn2=labelling.venn
  findsymbol=regexpr("-",labelling.venn2)
  pos=which(findsymbol>=0)
  labelling.venn2[pos] = substring(labelling.venn2[pos],1,findsymbol[pos]-1)

  ## plot venn
  pp=ggplot2::ggplot() +
    ggplot2::geom_polygon(aes(X, Y, fill=id, group=id),
                 data = ggVennDiagram::venn_regionedge(venn_data),  linewidth = 0.5,colour = "darkgray",
                 show.legend=NA) +
    ggplot2::geom_text(aes(X,Y , label = paste(labelling.venn2,count,sep="\n")),color="black" , size=6,
              data = ggVennDiagram::venn_regionlabel(venn_data)) +
    ggplot2::theme_void()+
    ggplot2::scale_fill_manual(name="Legend", labels=labelling.venn,
                      values = venn.colors,breaks=c(unique(venn_data$regionEdge$id)))+
    ggplot2::theme(legend.title = element_text(size=18),
          legend.text = element_text(size=16))
  pp
}


#' function
#' @title Creating a reduced table
#' @description The function creates a reduced table based on the selected Venn diagram SNP marker subset.
#'
#' @param venn.dat object containing the processed Venn data (containing the plot data; created in \link{process_venn_data})
#' @param venn object of class "Venn"
#' @param click list containing the Venn diagram information from the user mouse click
#' @param table.bc data frame containing the information which are used for the output table depending from the called module \cr
#'                  (\link{data_table_hotspot} for hotspot detection, or \link{data_table_geneticMap_bc} for genetic map (only breed comparison))
#'
#' @importFrom shiny nearPoints
#' @importFrom ggVennDiagram process_region_data
#'
#' @return The function returns a list:\cr
#' 1. element contains the reduced table accordingly to the selected unique Venn set \cr
#' 2. element contains which Venn subset was selected.
#' @seealso
#' * \link{mod_bc_genetic_map_server}  \cr
#' * \link{mod_bc_hotspot_server} \cr
#' * \link{mod_hotspot_server}
#'
#' @export

prepare_table_venn=function(venn.dat,venn,click,table.bc)
{
  as=shiny::nearPoints(venn.dat$regionLabel, click, threshold = 1000)
  venn.set.name=as$name[1]
  use=match(venn.set.name, ggVennDiagram::process_region_data(venn)$name)

  data.ptv=as.data.frame(ggVennDiagram::process_region_data(venn)$item[[use]])
  dat.merge=merge(table.bc, data.ptv[,1],by.x=1,by.y=1)
  list(dat.merge,venn.set.name)
}
