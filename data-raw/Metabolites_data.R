#prepare metabolites data set
library(readxl)
library(mixOmics)

# Read metabolites table
df_metabolites <- as.data.frame(readxl::read_excel("data-raw/t0_metabolites.xlsx", sheet = 1, col_names = TRUE))
rownames(df_metabolites) <- df_metabolites[,1]   #all rows, first column
df_metabolites <- df_metabolites[, -1]  #delete first column
df_metabolites <- df_metabolites[grep("^V[1256]", rownames(df_metabolites)), ] #pick only columns from variety 1, 2, 5 and 6

# used for pca and pls-da
data.metabolomic <- df_metabolites

#used for spls and diablo
met_var <- nearZeroVar(data.metabolomic, freqCut = 90/10, uniqueCut = 20)
data.metabolomic_small <- data.metabolomic[,-met_var$Position]

usethis::use_data(data.metabolomic, overwrite = TRUE) 
usethis::use_data(data.metabolomic_small, overwrite = TRUE) 
