# Feature selection 

### PCA-based feature selection for multi-omics
The feature selection functionality using the PCA analysis is separated into three parts to reduce the given dataset in an ideal manner:

<ol type="1">
  <li><b>Get optimal number of components (m):</b> A mixOmics-based function calculates the explained variance for a large number of components and Holomics then takes the number of components which firstly explains at least 80% of the dataset variance.</li>
  <li><b>Get optimal number of features per component (n):</b> Through cross-validation and testing different numbers of features to keep per component, the optimal number of features per component are calculated.</li>
  <li><b>Reduce dataset:</b> The optimal n features of the optimal m components get extracted from the original dataset and are saved in a new dataframe.</li>
</ol>

For more information please have a look at the different documentations of mixOmics, such as <a class='mixOmics-link' href="http://mixomics.org/case-studies/spca-multidrug-case-study/" rel="noreferrer noopener" target="_blank">sPCA Multidrug Case Study</a>.

### PLS-DA-based feature selection for multi-omics
The feature selection functionality using PLS-DA is separated into two parts:

<ol type="1">
  <li><b>Get optimal number of components (m) and optimal number of features per component (n):</b> A mixOmics-based function takes the number of components (provided by the user, from the "Analysis parameters" tab) and performs cross-validation to get the optimal number of components and the therefore optimal number of features per component. The components that are tested in this cross-validation calculation range from 1 to the number set by the user. This means sometimes a higher number provided by the user can lead to different results than a lower number.</li>
  <li><b>Reduce dataset:</b> The optimal n features of the optimal m components get extracted from the original dataset and are saved in a new one.</li>
</ol>

For more information please have a look at the different documentations of mixOmics, such as <a class='mixOmics-link' href="http://mixomics.org/case-studies/splsda-srbct-case-study/" rel="noreferrer noopener" target="_blank">sPLSDA SRBCT Case Study</a>.

# Tuning 
### sPLS and DIABLO parameter tuning
A mixOmics-based function takes the provided number of components (set by the user, from the "Analysis parameters" tab) and performs cross-validation to get the Q2 score per component. The tuning step determines the correlation between the actual and anticipated components by varying the amount of features chosen for each dataset. Finally, the last number of components having a total Q2 greater than 0.0975 is selected as the ideal number of components and the number of features with the highest correlation is the optimal number of features. The components that are tested in this cross-validation calculation range from 1 to the number given by the user. This means sometimes a higher number provided by the user can lead to different results than a lower number.

The DIABLO tuning process, similar to sPLS tuning, takes the user-selected components and fits a DIABLO model up to the number of components using n-fold cross-validation and without feature selection. The number of components is determined by utilizing the centroids.dist metric and the overall BER. Again, n-fold cross-validation and the centroids.dist metric are used to determine the number of features per dataset.

The analysis results using the tuned parameters are then given on the right side of the page.
For more information please have a look at the different documentations of mixOmics, such as <a class='mixOmics-link' href="http://mixomics.org/case-studies/spls-liver-toxicity-case-study/" rel="noreferrer noopener" target="_blank">sPLS Liver Toxicity Case Study</a> and 
<a class='mixOmics-link' href="http://mixomics.org/mixdiablo/diablo-tcga-case-study/" rel="noreferrer noopener" target="_blank">DIABLO TCGA Case Study</a>.
