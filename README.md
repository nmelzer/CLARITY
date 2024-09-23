*CLARITY*: A Shiny app for interactive visualisation of the bovine
physical-genetic map
================
N. Melzer, D. Wittenburg
(October 20, 2022)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Source

An online version of CLARITY is available at
<https://nmelzer.shinyapps.io/clarity/>

A pipeline for creating a breed-specific genetic map from genotypes of
half-siblings is available at <https://github.com/wittenburg/hsrecombi>

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

Based on published results on a large pedigree of German Holstein cattle
and newly obtained results with German/Austrian Fleckvieh cattle, we aim
at providing a platform that allows users to interactively explore the
bovine genetic and physical map. We developed the R Shiny app “CLARITY”
that provides access to the genetic map built on the Illumina Bovine
SNP50 genotyping array with markers ordered according to the physical
coordinates of the most recent bovine genome assembly ARS-UCD1.2. The
user is able to interconnect the physical and genetic map for a whole
chromosome or a specific chromosomal region and can inspect a landscape
of recombination hotspots. Moreover, the user can investigate which of
the frequently used genetic-map functions locally fits best. We further
provide auxiliary information about markers being putatively misplaced
in the ARS-UCD1.2 release. The corresponding output tables and figures
can be downloaded in various formats. By ongoing data integration from
different breeds, the app also facilitates comparison of different
genome features, providing a valuable tool for education and research
purposes.

Key features:

-   Summary statistics for all chromosomes
-   Genetic map
-   Hotspot detection
-   Genetic-map functions
-   Misplaced markers

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

## Cite
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11620802.svg)](https://doi.org/10.5281/zenodo.11620802)
