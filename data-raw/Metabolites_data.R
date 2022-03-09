#prepare metabolites data set
library(readxl)

# Read metabolites table
df_metabolites <- as.data.frame(readxl::read_excel("data-raw/t0_metabolites.xlsx", sheet = 1, col_names = TRUE))
rownames(df_metabolites) <- df_metabolites[,1]   #all rows, first column
df_metabolites <- df_metabolites[, -1]  #delete first column
df_metabolites <- df_metabolites[grep("^V[1256]", rownames(df_metabolites)), ] #pick only columns from variety 1, 2, 5 and 6

data.metabolites <- df_metabolites

usethis::use_data(data.metabolites, overwrite = TRUE) 