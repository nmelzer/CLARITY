% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_general_fct_functions.R
\name{transformdata_general_bc}
\alias{transformdata_general_bc}
\title{Transform data for breed comparison}
\usage{
transformdata_general_bc(data1, what, breed.select.bc, colo, filter)
}
\arguments{
\item{data1}{data frame}

\item{what}{character contains the name of the approach
\itemize{
\item det - is used for deterministic approach
\item lik - is used for likelihood-based approach
}}

\item{breed.select.bc}{vector containing the selected breeds}

\item{colo}{vector containing the predefined colors for each breed}

\item{filter}{character contains the selected chromosome}
}
\value{
The function returns a data frame with six columns: basepair (bp), centiMorgan (cM), chromosome (chr), breed, alpha, size and color.
}
\description{
The function transforms the data for plotting: 1) basepairs (bp) into megabasepairs (Mbp) and Morgan (M) into centiMorgan (cM) for selected breeds,
2) adds breed specific colors, and
3) adds geom_point information for the ggplot accordingly to the selected chromosome: alpha values ("0.4" not selected chromosome and "1" selected chromosome)
 and size values ("4" not selected chromosome and "8" selected chromosome)
}
\details{
function
}
\seealso{
\link{mod_bc_general_server}
}
