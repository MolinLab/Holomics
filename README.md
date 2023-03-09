
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Holomics

<img align="right" src="inst/app/www/img/logo.png" width=300>

Holomics is a R Shiny application, which enables its users to perform
single- and multi-omics analyses. Therefore, Holomics provides a
user-friendly interface to upload the different datasets, choose and
perform the various analyses and then presents the results to the user.
To perform the various analyses Holomics uses the R package mixOmics.
This means Holomics takes the input files, forwards them to the mixOmics
algorithms and takes the resulting plots/tables and presents them to
user.

mixOmics provides numerous analyses algorithms, but Holomics only
integrates the two single-omics algorithms “Principle Component Analysis
(PCA)” and “Partial Least Squares Discriminant Analysis (PLS-DA)”, one
pairwise omics analysis called “sparse Partial Least Squares (sPLS)” and
one multi-omics analysis called “Data Integration Analysis for Biomarker
discovery using Latent variable approaches for Omics studies (DIABLO)”.

## Installation

### CRAN

    install.packages("Holomics")

### Github

    # Install devtools if it is not already installed
    install.packages("devtools")
    library(devtools)

    # Install Holomics package 
    install_github("evasehr/Holomics")

## Start application

Either with

    library(Holomics)
    runApp()

or

    Holomics::runApp()

## Workflow

To make use of all the functionality provided, the below described
workflow should be followed. Firstly, the user needs to upload the
datasets to be able to use them for the (single) omics analysis.
Afterwards, the user should go with the datasets into the single-omics
analysis, where key variables are identified and the datasets are
filtered accordingly. After the single-omics analyses, the user can
apply the multi-omics analyses to identify correlations between 2-n
datasets. NOTE: If the user already uploaded pre-filtered (ideally by
Holomics at an earlier time) datasets, it is possible to go directly
into the multi-omics analysis.

<img src="vignettes/images/workflow.png" width="100%" />

## Further information

For further information on how to use Holomics please have a look at our
vignette.
