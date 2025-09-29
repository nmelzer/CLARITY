*CLARITY*: A Shiny app for interactive visualisation of the bovine
physical-genetic map
================
N. Melzer, D. Wittenburg
(September 22, 2025)

<!-- badges: start 
[![R CMD Check](https://github.com/nmelzer/CLARITY/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nmelzer/CLARITY/actions/workflows/R-CMD-check.yaml)
⁠<!-- badges: end -->

## Source

An online version of CLARITY is available at
<https://nmelzer.shinyapps.io/clarity/>

A pipeline for creating a breed-specific male genetic map from genotypes
of paternal half-siblings is available at
<https://github.com/wittenburg/hsrecombi>

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

Frequencies of recombination events have been studied in 8 European
cattle breeds comprising dairy, dual-purpose and beef breeds (German
Holstein, Swiss Holstein, German/Austrian Fleckvieh, Brown Swiss,
Original Braunvieh, Simmental, Limousin, Angus). Here, we provide a
platform that allows users to interactively explore the bovine genetic
and physical map in each breed and to compare between breeds. We
developed the R Shiny app “CLARITY” that provides access to the genetic
map built on the Illumina Bovine SNP50 genotyping array (or similar
panel) with markers ordered according to the physical coordinates of the
most recent bovine genome assembly
[ARS-UCD1.2](https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2).
The user is able to interconnect the physical and genetic map for a
whole chromosome or a specific chromosomal region and can inspect a
landscape of recombination hotspots. Comparison of genetic maps
estimated from three different approaches is implemented: male, female
and sex-average genetic maps from Hidden-Markov model, male genetic maps
from likelihood-based approach, and male genetic maps from deterministic
approach. Moreover, the user can investigate which of the frequently
used genetic-map functions locally fits best based on the
likelihood-based approach. We further provide auxiliary information
about markers being putatively misplaced in the
[ARS-UCD1.2](https://bovinegenome.elsiklab.missouri.edu/downloads/ARS-UCD1.2)
release. Genetic maps and corresponding output tables and figures can be
downloaded in various formats. The app facilitates comparison of
different genome features among cattle breeds, providing a valuable tool
for education and research purposes.

Key features:

- Summary statistics for all chromosomes
- Male, female, and sex-average genetic maps
- Hotspot detection
- Genetic-map functions
- Misplaced markers

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

## Acknowledgement

This work was supported by the grant from the German Federal Ministry of
Education and Research (BMBF, [FKZ031L0166
CompLS](https://www.gesundheitsforschung-bmftr.de/de/clarity-die-entwicklung-einer-kombinierten-physisch-genetischen-karte-fur-eine-9212.php)).

## Cite

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13832239.svg)](https://doi.org/10.5281/zenodo.13832239)
