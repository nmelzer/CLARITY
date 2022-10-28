#' function
#'
#' @title Creating hover information
#' @description The hover function determines the nearest point and creates style of the hover outcome.
#'
#' @param dat1 data frame which was used to create the plot
#' @param hover list containing information from the scatter plot to show the corresponding point information
#'  when mouse is moved over the plot.
#' @param what specifies the outcome as well as the hovering style for the wellpanel where  \enumerate{
#' \item represents the \link{mod_general_server}
#' \item represents the \link{mod_hotspot_server}
#' \item represents the \link{mod_bc_general_server}
#' \item represents the \link{mod_bc_hotspot_server}
#' }
#' @return The function returns the hover information with corresponding style format.
#' @note The code was adopted from https://gitlab.com/-/snippets/16220 (Pawel github)
#'
#' @import shiny
#' @import htmltools
#' @seealso \link{mod_general_server}, \link{mod_hotspot_server}, \link{mod_bc_general_server}, and \link{mod_bc_hotspot_server}
#' @export

######################
hovering<-function(dat1,hover,what)
{
  point2 <- shiny::nearPoints(dat1, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  if(nrow(point2) == 0)return(NULL)

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct2 <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct2 <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px2 <- hover$range$left + left_pct2 * (hover$range$right - hover$range$left)
  top_px2 <- hover$range$top + top_pct2 * (hover$range$bottom - hover$range$top)

  # actual tooltip created as wellPanel
  if(what==1 || what==3){
    if(what==1)
    {
    style1<-paste0("position:absolute; z-index:100; background-color: rgb(30, 144, 255) ; ","left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
    style2<-paste0("position:absolute; z-index:100; background-color: rgb(95, 158, 160); ","left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
    if(point2$Approach=="deterministic")style3=style1
    if(point2$Approach!="deterministic")style3=style2
    }
    else
    {
      style3=paste0("position:absolute; z-index:100; background-color: ", point2$color," ; ","left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
    }
    shiny::wellPanel(
      style=style3,  ## ,
      htmltools::p(htmltools::HTML(paste0("<b> Chromosome: </b>",point2$chr , "<br/>",
                    "<b> Mbp: </b>", point2$bp, "<br/>",
                    "<b> Genetic Distance: </b>", point2$cM, "<br/>"
      )))
    )
  }
 else {
   if(what==2)
   {
    style4 <- paste0("position:absolute; z-index:100; background-color:",point2$coloring ,"; ",
                     "left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
   }
   else{
     style4=paste0("position:absolute; z-index:100; background-color: ",point2$coloring," ; ","left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
   }
   shiny::wellPanel(
      style = style4,
      htmltools::p(htmltools::HTML(paste0("<b> Chromosome: </b>", point2$Chr, "<br/>",
                    "<b> Marker: </b>",point2$SNP,"<br/>",
                    "<b> Mbp: </b>", point2$BP, "<br/>",
                    "<b> Theta: </b>", round(point2$Theta,6), "<br/>"
      )))
    )
  }
}
