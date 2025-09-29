############# Internal pre-defined data
#' Data
#'
#' ```{r, echo=FALSE}
#' table1 <- utils::read.csv("inst/internal_data_description/table-breed-information.csv",header=T,sep=",")
#' knitr::kable(table1)
#' ```
#' @name table_breed_information
#' @docType data
#' @keywords data
#' @details Table contains all predefined names and settings for all breeds to enable standardized outputs.
#' Table information is used in all app modules.
#'
#' @md
#' @seealso created in \link{app_server}
NULL

############# Internal pre-defined approach data
#' Data
#'
#' ```{r, echo=FALSE}
#' table2 <- utils::read.csv("inst/internal_data_description/table-approach-information.csv",header=T,sep=",")
#' knitr::kable(table2)
#' ```
#' @name table_approach_information
#' @docType data
#' @keywords data
#' @details Table contains all predefined names and settings for all approaches to enable standardized outputs.
#' Table information is used in all app modules.
#'
#' @md
#' @seealso created in \link{app_server}
NULL

############# Created plot data in general (breed analysis and breed comparison)
#' Data
#'
#' ```{r, echo=FALSE}
#' table3 <- utils::read.csv("inst/internal_data_description/data_plot_general.csv",header=T,sep=",")
#' knitr::kable(table3)
#' ```
#' @name data_plot_general
#' @docType data
#' @keywords data
#' @description Data is described as required for the \link{scatterPlot_general}.
#'  Moreover this also forms the basis for the correlation output (see \link{create.output}).
#'
#' @md
#' @seealso
#'  * created in \link{transformdata_general_bc} or \link{transformdata_general} \cr
#'  * used in \link{scatterPlot_general} and \link{create.output}
NULL

############# Created plot data in genetic map (breed analysis and breed comparison)
#' Data
#'
#' ```{r, echo=FALSE}
#' table4 <- utils::read.csv("inst/internal_data_description/data_plot_geneticMap.csv",header=T,sep=",")
#' knitr::kable(table4)
#' ```
#' @name data_plot_geneticMap
#' @docType data
#' @keywords data
#' @description Data is described as required for \link{makePlot_geneticMaps} and \link{makePlot_all_geneticMaps}.
#'
#' @md
#' @seealso
#'  * created in \link{transformdata_general_bc} or \link{transformdata_general} \cr
#'  * used in \link{makePlot_geneticMaps} and \link{makePlot_all_geneticMaps}
NULL


############ Created data table in genetic map (breed comparison)
#' Data
#'
#' ```{r, echo=FALSE}
#' table5 <- utils::read.csv("inst/internal_data_description/table-genetic-map-bc.csv",header=T,sep=",")
#' knitr::kable(table5)
#' ```
#' @name data_table_geneticMap_bc
#' @docType data
#' @keywords data
#' @description Data is described as required for the \link{prepare_table_venn}.
#'
#' @md
#' @seealso
#' * created in \link{transformdata_general_bc} \cr
#' * used in \link{prepare_table_venn}
NULL

############ Created data table in hotspot (breed analysis and breed comparison)
#' Data
#'
#' ```{r, echo=FALSE}
#' table6 <- utils::read.csv("inst/internal_data_description/table-hotspot.csv",header=T,sep=",")
#' knitr::kable(table6)
#' ```
#' @name data_table_hotspot
#' @docType data
#' @keywords data
#' @description Data is described as required for the \link{prepare_table_venn}.
#'
#' @md
#' @seealso
#'  * created in \link{transformdata_hotspot} or \link{transformdata_hotspot_bc} \cr
#'  * used \link{prepare_table_venn}
NULL

########### Created plot data in hotspot (breed analysis and breed comparison) for single selected chromosome
#' Data
#'
#' ```{r, echo=FALSE}
#' table15 <- utils::read.csv("inst/internal_data_description/data_plot_hotspot.csv",header=T,sep=",")
#' knitr::kable(table15)
#' ```
#' @name data_plot_hotspot
#' @docType data
#' @keywords data
#' @description Data is described as required for the \link{scatterPlot_hotspot}.
#'
#' @seealso
#' * created in \link{transformdata_hotspot}  and \link{transformdata_hotspot_bc} \cr
#' * used in \link{scatterPlot_hotspot}
#' @md
NULL

########### Created plot data in hotspot (breed analysis and breed comparison) for all selected chromosome
#' Data
#'
#' ```{r, echo=FALSE}
#' table16 <- utils::read.csv("inst/internal_data_description/data_plot_hotspot_all.csv",header=T,sep=",")
#' knitr::kable(table16)
#' ```
#' @name data_plot_hotspot_all
#' @docType data
#' @keywords data
#' @description Data is described as required for the \link{scatterPlot_hotspot_all}.
#'
#' @seealso
#' * created in \link{transformdata_hotspot} or \link{transformdata_hotspot_bc} \cr
#' * used in \link{scatterPlot_hotspot_all}
#' @md
NULL

########## Input data for traffic light
#' Data
#'
#' ```{r, echo=FALSE}
#'  out=cbind(c(1:8), c("Breed","n1","N1","n2","N2","p","N2.new","Averag.n2"),
#'  c("","Number of genotyped animals",
#'  "Number of half-sib families","Number of genotyped animals in half-sib families with at least 30 progeny",
#'  "Half-sib families with sires having at least 30 progenies","Number of SNPs","Contains the value of N2 rounded to the nearest multiple 100","Contains the ratio of n2 to N2 to the nearest multiple of 100."),
#'  c("character", "integer","integer","integer","integer","integer","numeric", "numeric"))
#'  colnames(out)<-c("Column.No", "Column.Name", "Meaning", "Class")
#'  knitr::kable(as.data.frame(out))
#' ```
#' @name data_traffic_light
#' @docType data
#' @keywords data
#'
#' @description Data is described as required for the \link{make_traffic_light}.
#'
#' @seealso \link{make_traffic_light}
#' @md
NULL
