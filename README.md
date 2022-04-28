
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CLARITY

*CLARITY*: An R Shiny app for an interactive exploration of
breed-specific genetic maps in cattle

## Background

The arrangement of markers on the genome can be defined in either
physical or linkage terms. While a physical map represents the
inter-marker distances in base pairs, a genetic (or linkage) map
pictures the recombination rate between pairs of markers.
High-resolution genetic maps are key elements for genomic research, such
as fine-mapping of quantitative trait loci, but they are also needed for
creating and updating chromosome-level assemblies of whole-genome
sequences.

## Objectives

Based on published results on a large pedigree of German Holstein
cattle, we aim at providing a platform that allows users to
interactively explore the bovine genetic and physical map. We developed
the R Shiny app *CLARITY* that provides access to the genetic map built
on the Illumina Bovine SNP50 genotyping array with markers ordered
according to the physical coordinates of the most recent bovine genome
assembly ARS-UCD1.2. The user is able to interconnect the physical and
genetic map for a whole chromosome or a specific chromosomal region and
can inspect a landscape of recombination hotspots. Moreover, the user
can investigate which of the frequently used genetic-map functions
locally fits best. We further provide auxiliary information about
markers being putatively misplaced in the ARS-UCD1.2 release. The
corresponding output tables and figures can be downloaded in various
formats. We are working on integrating data from different breeds to
facilitate comparison of different genome features and to promote this
app for education and research purposes.

In this package version, only Holstein data can be investigated (more
data are underway).

Key features:

  - Summary statistics for all chromosomes
  - Genetic map
  - Misplaced markers
  - Hotspot detection
  - Genetic-map functions

## Installation

To get the current development version of CLARITY from github, please
type:

``` r
# install.packages("devtools")
devtools::install_github("nmelzer/CLARITY")
```

## Example

To start the R Shiny app, please type:

``` r
library(CLARITY)
run_app()
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>
# CLARITY 
