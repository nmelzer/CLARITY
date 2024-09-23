#' function
#' @title Prepare data for Venn plot
#' @description This function prepares the data for the Venn plot.
#'
#' @return The function returns the processed Venn data object (containing the plot data).
#'
#' @param venn object of class "Venn"
#'
#' @importFrom ggVennDiagram process_data

#' @seealso \link{mod_bc_general_server}, \link{mod_bc_genetic_map_server} and \link{mod_bc_hotspot_server}
#' @export
# function complete rewritten 03.06.2024
process_venn_data=function(venn)
{
  shapes.id=ifelse(length(names(venn))==2,"201","301") ## determine shape of venndiagram
  venn.data=ggVennDiagram::process_data(venn,shape_id = shapes.id) ##

  venn.data
}

#' function
#' @title Creating Venn diagram
#' @description The function creates Venn diagram plot.
#'
#' @param venn_data object of class "Venn" processed data (containing the plot data)
#' @param breed.bc vector containing the full names of selected breeds
#' @param bc.venn vector containing only the first letter of the selected breeds
#' @return The function returns the plotted Venn diagram (ggplot object).
#' @import ggplot2
#' @importFrom ggVennDiagram venn_regionedge venn_regionlabel
#' @seealso \link{mod_bc_general_server}, \link{mod_bc_genetic_map_server} and \link{mod_bc_hotspot_server}
#' @export

creating_venn=function(venn_data,breed.bc, bc.venn) ## currently only for three breeds
{
  X<-Y<-NULL

  venn.diagram.colors<-list()
  venn.diagram.colors[[1]]<-""
  venn.diagram.colors[[2]]<-c("#a6cee3","#1f78b4","#b2df8a")
  venn.diagram.colors[[3]]<- c("#a6cee3","#1f78b4","#b2df8a","#528B8B","#6E8B3D","#698B69","#2F4F4F")

  labelling.venn=c()
  for(inn in 1:length(breed.bc))labelling.venn[inn]=paste(bc.venn[inn],breed.bc[inn],sep="-") ### over breeds
  count.inn= inn
  for(inn in 1:(length(breed.bc)-1))
  {
    for(inn2 in (inn+1):(length(breed.bc)))
    {
      count.inn=count.inn+1
      labelling.venn[count.inn]=paste(bc.venn[inn],bc.venn[inn2],sep="..") ### pairwise
    }
  }
  if(length(breed.bc)!=2)labelling.venn[count.inn+1]=paste(bc.venn[1],bc.venn[2],bc.venn[3],sep="..")

  ## create abbreviation to plot in
  labelling.venn2=labelling.venn
  findsymbol=regexpr("-",labelling.venn2)
  pos=which(findsymbol>=0)
  labelling.venn2[pos] = substring(labelling.venn2[pos],1,findsymbol[pos]-1)

  ## plot
   pp=ggplot() +
    geom_polygon(aes(X, Y, fill=id, group=id),
                 data = ggVennDiagram::venn_regionedge(venn_data),  linewidth = 0.5,colour = "darkgray",
                 show.legend=NA) +
    geom_text(aes(X,Y , label = paste(labelling.venn2,count,sep="\n")),color="black" , size=8,
              data = ggVennDiagram::venn_regionlabel(venn_data)) +
    theme_void()+
    scale_fill_manual(name="Legend", labels=labelling.venn,
                      values = venn.diagram.colors[[length(breed.bc)]],breaks=c(unique(venn_data$regionEdge$id)))+  ## important breaks zu haben andernfalls geht die sortierung flöten
    theme(legend.title = element_text(size=18),
          legend.text = element_text(size=16))

  pp
}

#' function
#' @title Creating a reduced table
#' @description The function creates a reduced table based on the selected Venn diagram subset. The function is used by \link{mod_bc_genetic_map_server} and \link{mod_bc_hotspot_server}.
#'
#' @param venn.dat object containing the processed Venn data (containing the plot data)
#' @param venn object of class "Venn"
#' @param click list containing the selected Venn diagram subset
#' @param table.bc data frame containing the information: SNP, Chr, SNP position (bp), SNP position (cM), whereby the last column is repeated for the number of breed to be compared.
#'
#' @importFrom shiny nearPoints
#' @importFrom ggVennDiagram process_region_data
#'
#' @return The function returns a list: 1. element is a table containing the unique set and 2. element contains which Venn subset was selected.
#' @seealso \link{mod_bc_genetic_map_server} and \link{mod_bc_hotspot_server}
#' @export

prepare_table_venn=function(venn.dat,venn,click,table.bc)
{
  as=shiny::nearPoints(venn.dat$regionLabel, click, threshold = 1000)
  hh=as$name[1]

  use=match(hh, ggVennDiagram::process_region_data(venn)$name)
  data=as.data.frame(ggVennDiagram::process_region_data(venn)$item[[use]])

  dat.merge=merge(table.bc, data[,1],by.x=1,by.y=1)

  list(dat.merge,hh)
}
