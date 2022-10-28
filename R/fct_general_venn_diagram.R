#' function
#' @title Prepare data for Venn plot
#' @description This function prepares the data for the Venn plot.
#'
#' @return The function returns the processed Venn data object (containing: x, y, name, count, geometry).
#'
#' @param venn object of class "Venn"
#'
#' @importFrom ggVennDiagram process_data
#' @importFrom  sf st_point_on_surface
#' @rawNamespace import(dplyr, except = combine)
#' @importFrom purrr map_dbl
#' @seealso \link{mod_bc_general_server}, \link{mod_bc_genetic_map_server} and \link{mod_bc_hotspot_server}
#' @export

process_venn_data=function(venn)
{
  x<-y<-name<-count<-centroid <- geometry<-map_dbl<-NULL

  venn.data <- ggVennDiagram::process_data(venn)@region %>%
    dplyr::mutate(centroid = sf::st_point_on_surface(geometry),
           x = purrr::map_dbl(centroid, 1),
           y = purrr::map_dbl(centroid, 2)) %>%
    dplyr::select(x, y, name, count, geometry)
  venn.data
}

#' function
#' @title Creating Venn diagram
#' @description The function creates Venn diagram plot.
#'
#' @param venn_data  processed Venn data object (containing: x, y, name, count, geometry)
#' @param breed.bc  vector containing the full names of selected breeds
#' @param bc.venn vector containing only the first letter of the selected breeds
#'
#' @return The function returns the plotted Venn diagram.
#' @import ggplot2
#' @seealso \link{mod_bc_general_server}, \link{mod_bc_genetic_map_server} and \link{mod_bc_hotspot_server}
#' @export

creating_venn=function(venn_data,breed.bc, bc.venn) ## currently only for three breeds
{
  x<-y<-name<-count<-NULL

  ## venn.diagram colors up to three breeds are defined -  so it has to be extended when more than three breeds are available - part has to be improved
  venn.diagram.colors<-list()
  venn.diagram.colors[[1]]<-""
  venn.diagram.colors[[2]]<-c("#FF8C00","#eeb422","#ffe4c4")
  venn.diagram.colors[[3]]<-c("#FF8C00","#eeb422","#ffa07a","#ffe4c4","#f5f5dc","#ffc125","#faebd7")

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


  pp=venn_data %>%
  ggplot2::ggplot(aes(x, y, label = paste(name,count,sep="\n "))) +
  geom_sf(aes(fill=name), show.legend = TRUE)+
  geom_sf(fill=venn.diagram.colors[[length(breed.bc)]])+#"
  geom_sf_text(aes(label =paste(name,count,sep="\n ")),color="black", data = venn_data,size=6) +
  guides(colour = guide_legend(override.aes = list(size=12)))+
  theme_void()+
  scale_fill_manual(name="Legend",labels=labelling.venn,
                    values = venn.diagram.colors[[length(breed.bc)]])+
    theme(legend.title = element_text(size=20),
          legend.text = element_text(size=18))

  pp
}

#' function
#' @title Creating a reduced table
#' @description The function creates a reduced table based on the selected Venn diagram subset. The function is used by \link{mod_bc_genetic_map_server}.
#'
#' @param venn.dat processed Venn data object (containing: x, y, name, count, geometry)
#' @param venn object of class "Venn"
#' @param click selected Venn diagram subset
#' @param table.bc data frame
#'
#' @import shiny
#' @importFrom sf st_drop_geometry
#' @importFrom ggVennDiagram process_region_data
#'
#' @return The function returns a list: 1. element is a table containing the unique set and 2. element contains which Venn subset was selected.
#' @seealso \link{mod_bc_genetic_map_server} and  \link{mod_bc_hotspot_server}
#' @export

prepare_table_venn=function(venn.dat,venn,click,table.bc)
{
  as=shiny::nearPoints(sf::st_drop_geometry(venn.dat), click, threshold = 1000)
  hh=as$name[1]
  use=match(hh, ggVennDiagram::process_region_data(venn)$name)
  data=as.data.frame(ggVennDiagram::process_region_data(venn)$item[[use]])

  dat.merge=merge(table.bc, data[,1],by.x=1,by.y=1)

  list(dat.merge,hh)
}
