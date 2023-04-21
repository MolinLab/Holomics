# Filtering 

### PCA filtering for multi-omics
The filtering functionality using the PCA analysis is separated into three parts to filter the given dataset ideally:

<ol type="1">
  <li><b>Get optimal number of components (m):</b> A by mixOmics provided function calculates the explained variance for a large number of components and Holomics then takes the number of components which firstly explains at least 80% of the dataset variance.</li>
  <li><b>Get optimal number of features per component (n):</b> Through cross-validation and testing different numbers of features to keep per component, the optimal number of features per component get calculated.</li>
  <li><b>Filter dataset:</b> The optimal n features of the optimal m components get extracted from the original dataset and are saved in a new one.</li>
</ol>

For more information please have a look at the different documentations of mixOmics, such as <a class='mixOmics-link' href="https://mixomicsteam.github.io/Bookdown/pca.html#tuning-parameters" rel="noreferrer noopener" target="_blank">PCA - tuning parameters</a>

### PLS-DA filtering for multi-omics
The filtering functionality using the PLS-DA analysis is separated into two parts to filter the given dataset ideally:

<ol type="1">
  <li><b>Get optimal number of components (m) and optimal number of features per component (n):</b> A by mixOmics provided function takes the by the user provided number of components (from the "Analysis parameters" tab) and performs cross-validation to get the optimal number of components and the therefore optimal number of features per component. The components that are tested in this cross-validation calculation range from 1 to the number given by the user. This means sometimes a higher number provided by the user can lead to different results than a lower number.</li>
  <li><b>Filter dataset:</b> The optimal n features of the optimal m components get extracted from the original dataset and are saved in a new one.</li>
</ol>

For more information please have a look at the different documentations of mixOmics, such as <a class='mixOmics-link' href="https://mixomicsteam.github.io/Bookdown/plsda.html#tuning:sPLSDA" rel="noreferrer noopener" target="_blank">PLS-DA - tuning parameters</a>.
# Tuning 

### sPLS and DIABLO parameter tuning
The parameter tuning functionality of the sPLS analysis is separated into two parts:

<ol type="1">
  <li><b>Get optimal number of components:</b> A by mixOmics provided function takes the by the user provided number of components (from the "Analysis parameters" tab) and performs cross-validation to get the optimal number of components and the therefore optimal number of features per component. The components that are tested in this cross-validation calculation range from 1 to the number given by the user. This means sometimes a higher number provided by the user can lead to different results than a lower number. </li>
  <li><b>Get optimal number of features per component:</b> Again with cross-validation and testing different numbers of features to keep per component, the optimal number of features per component get calculated.</li>
</ol>

The analysis results using the tuned parameters will then be presented on the right side of the page.
For more information please have a look at the different documentations of mixOmics, such as <a class='mixOmics-link' href="https://mixomicsteam.github.io/Bookdown/pls.html#tuning:PLS" rel="noreferrer noopener" target="_blank">PLS - tuning parameters</a> and 
<a class='mixOmics-link' href="https://mixomicsteam.github.io/Bookdown/diablo.html#tuning-parameters-1" rel="noreferrer noopener" target="_blank">DIABLO - tuning parameters</a>.
