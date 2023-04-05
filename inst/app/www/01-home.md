# Welcome to Holomics
Holomics is an R Shiny application that allows its users to perform single- and multi-omics analyses by providing a user-friendly interface to upload the different omics datasets and select and run the implemented algorithms. Holomics is built mainly on the R package <a class='mixOmics-link' href="http://mixomics.org/" rel="noreferrer noopener" target="_blank">mixOmics</a>. The app takes the input files, passes them to the mixOmics algorithms and presents the resulting graphs and tables to the user. <br>
mixOmics provides numerous analysis algorithms for the integrative analysis of omics datasets and the two single-omics algorithms "Principle Component Analysis" (PCA) and "Partial Least Squares Discriminant Analysis" (PLS-DA), the pairwise-omics analysis "sparse Partial Least Squares" (sPLS) and the multi-omics framework DIABLO ("Data Integration Analysis for Biomarker discovery using Latent variable approaches for Omics studies") have been implemented in Holomics so far.

### Functionality overview

<ul>
  <li>Perform single-omics analyses and get an overview over your data</li>
  <li>Filter datasets by their key variables</li>
  <li>Perform multi-omics analyses and identify correlations between the datasets</li>
  <li>Save (most) result plots as images in print quality</li>
</ul>

<img src="img/workflow.png" alt="Workflow image" width="80%"/>


### Workflow
To make use of all the functionality provided by Holomics, a certain workflow should be followed.

<p class="workflow-step">Input datasets: </p> The needed datasets for the analyses have to be uploaded via the "Data upload" page. These datasets include the collected omics data itself (e.g. count tables) and a separate file(s) containing the allocated classes of the samples in the omics data. <br>

<p class="workflow-step">Single-omics analyses: </p> With the single-omics analyses you can get a first impression of your data. You can identify the key variables of your data and filter accordingly.   <br>

<p class="workflow-step">Filtered datasets: </p> With the single-omics algorithm PLS-DA you have the opportunity to automatically filter your data by the results of the analysis. The filtered data will be directly saved in the Holomics application and can be used in the multi-omics analyses. Additionally, you will get the filtered data provided as an excel file.  <br>
As an alternative you can directly go into the multi-omics analyses if you already uploaded pre-filtered datasets and do not want to filter them any further. <br>

<p class="workflow-step">Multi-omics analyses: </p> With the multi-omics analyses you are finally able to identify potential correlations between your datasets <br>

### Acknowledgement
Holomics has been developed at the <a class='mixOmics-link' href="https://www.ait.ac.at/" rel="noreferrer noopener" target="_blank">AIT - Austrian Institute of Technology</a> within the research project <a class='mixOmics-link' href="https://metabolomics-ifa.boku.ac.at/omics40project/" rel="noreferrer noopener" target="_blank">OMICs 4.0</a>, which is funded by the Federal State of Lower Austria as part of the RTI-Strategy Lower Austria.