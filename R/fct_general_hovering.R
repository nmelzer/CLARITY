# general_hovering function
#'
#' @title Create Hover
#' @description A fct function. Determining the nearest point and creates style of the hover outcome.
#'
#' @param dat1 A data frame.
#' @param hover A list containing corresponding information from the scatter plot enabling to show the corresponding point information when mouse is moved over a plotted point within the scatter plot.
#' @param what Specify the outcome by the wellpanel depends on from which modules is called.
#'
#' @return Hover information
#' @note The code was adopted form https://gitlab.com/-/snippets/16220 (Pawel github)
#' @note The function is used by the modules: mod_general and mod_hotspot.
#' @noRd

hovering<-function(dat1,hover,what)
{
  nearPoints(dat1, hover)
  point2 <- nearPoints(dat1, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  if (nrow(point2) == 0)return(NULL)

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct2 <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct2 <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px2 <- hover$range$left + left_pct2 * (hover$range$right - hover$range$left)
  top_px2 <- hover$range$top + top_pct2 * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top



#  print(point2$Approach) ## moved inside otherwise trouble with styles and error was shown - fixed bug 17.06
#  ifelse(point2$Approach=="deterministic",style3=style1,style3=style2)
#  if(point2$Approach=="deterministic")style3=style1
#  else style3=style2

  # actual tooltip created as wellPanel
  if(what==1){
    style1<-paste0("position:absolute; z-index:100; background-color: rgb(30, 144, 255) ; ","left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
    style2<-paste0("position:absolute; z-index:100; background-color: rgb(95, 158, 160); ","left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
    if(point2$Approach=="deterministic")style3=style1
    if(point2$Approach!="deterministic")style3=style2


    wellPanel(
      style=style3,  ## ,
      p(HTML(paste0("<b> Chromosome: </b>",point2$chr , "<br/>",
                    "<b> Mbp: </b>", point2$bp, "<br/>",
                    "<b> Genetic Distance: </b>", point2$cM, "<br/>"#,
                    # "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
      )))
    )
  }
  else{
    style4 <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                     "left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
    wellPanel(
      style = style4,
      p(HTML(paste0("<b> Chromosome: </b>", point2$Chr, "<br/>",
                  "<b> Marker: </b>",point2$SNP,"<br/>",
                  "<b> Mbp: </b>", point2$BP, "<br/>",
                  "<b> Theta: </b>", round(point2$Theta,6), "<br/>"#,  round now theta 17.06  round(point2$ChrTheta - (point2$Chr/300),6) -for scaling
                  # "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
      )))
    )
  }
}
