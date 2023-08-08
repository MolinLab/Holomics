# Plots

### Scree plot 
The scree plot demonstrates the number of explained variance of every "Principal Component". The plot helps to identify the optimal number of components, as the components should cover at least 80% of the data's variance.

### Sample plot
The sample plot shows the the samples as points in the 2D space spanned by the selected components. It enables the user to get an impression on how the data are clustered. For more information please have a look at the documentation of mixOmics, specifically at <a class='mixOmics-link' href="http://mixomics.org/graphics/plotindiv/" rel="noreferrer noopener" target="_blank">plotIndiv() – Sample plot</a>.

### Correlation circle plot
The correlation circle presents the features as points in the 2D space spanned by the selected components, whereas the coordinates of the features are the calculated correlation values with these components. For more information please have a look at <a class='mixOmics-link' href="http://mixomics.org/graphics/plotvar/" rel="noreferrer noopener" target="_blank">plotVar() – Correlation circle plot</a>.

### Loading plot 
The loadings bar plot shows the contribution of (every) feature to the calculation of the selected component. The greater the absolute contribution value the more important the feature is. For more information please have a look at <a class='mixOmics-link' href="http://mixomics.org/graphics/plotloadings/" rel="noreferrer noopener" target="_blank">plotLoadings() – Loadings Bar plot</a>.

### Selected features table
The table with the selected features shows the numerical contribution of each features to the selected component.
For more information please have a look at <a class='mixOmics-link' href="https://rdrr.io/cran/mixOmics/man/selectVar.html" rel="noreferrer noopener" target="_blank">selectVar: Output of selected features</a>.

### Clustered image map
The clustered image map (CIM), shown on the sPLS analysis page, presents per cell color the correlation between a defined pair of features of the two datasets. Neighboring regions with the same color indicate a possible relationship between the features of the datasets. The variant for the DIABLO analysis presents the multi-omics molecular signature expression for each sample. For more information please have a look at <a class='mixOmics-link' href="http://mixomics.org/graphics/cim/" rel="noreferrer noopener" target="_blank">cim() – Clustered Image Maps</a>.


### Arrow plot
The arrow plot visualises the agreement between the samples across the different datasets. The arrow tail shows the position of the sample point spanned in the space of the first dataset and the tip of the arrow the position of the point in the last dataset. Any points along the arrow (means more than two datasets are being compared) are also visualized. In general, short arrows indicate a high agreement between the datasets contrary to long arrows. For more information please have a look at <a class='mixOmics-link' href="http://mixomics.org/graphics/plotarrow-2/" rel="noreferrer noopener" target="_blank">plotArrow() – Arrow plot</a>.

### Diablo plot
The diablo plot visualises the general correlation of the selected component taking into account all the different datasets.
For more information please have a look at <a class='mixOmics-link' href="https://www.rdocumentation.org/packages/mixOmics/versions/6.3.2/topics/plotDiablo" rel="noreferrer noopener" target="_blank">plotDiablo: Graphical output for the DIABLO framework</a>. 

### Circos plot
The circos plot visualises the correlations between the different features of the selected datasets. For more information please have a look at <a class='mixOmics-link' href="http://mixomics.org/graphics/circos/" rel="noreferrer noopener" target="_blank">circosPlot() – Circos plot</a>. 

### Relevance network graph
The relevance network graph visualises the same correlations between the different features of the selected datasets as the circos plot. Additionally, the network is interactive, so the user can focus on specific features (nodes) and interact with them. For more information about the calculation of the network please have a look at <a class='mixOmics-link' href="http://mixomics.org/graphics/network/" rel="noreferrer noopener" target="_blank">network() – Relevance Network Graph</a>.


