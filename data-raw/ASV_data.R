#prepare asv data
library(readxl)
library(mixOmics)
source("data-raw/Labels_data.R")

# Read ASV table
df_asv <- as.data.frame(readxl::read_excel("data-raw/t0_asv.xlsx", sheet = 1, col_names = TRUE))
rownames(df_asv) <- df_asv[,1]   #all rows, first column
df_asv <- df_asv[,-1]
df_asv <- df_asv[, grep("^V[1256]", names(df_asv))] #pick only columns from variety 1, 2, 5 and 6


# transpose asv matrix to get the ASV in columns
t_asv <- as.data.frame(t(df_asv)) + 1

# Pre-filtering
low.count.removal = function(data, percent=0.01){
  keep.asv = which(colSums(data)*100/(sum(colSums(data)))>percent)
  data.filter = data[,keep.asv]
  return (data.filter)
}

#used for pca and pls-da - using mixmc 
data.filtered <- low.count.removal(t_asv, percent = 0.01)
data.microbiomic <- logratio.transfo(as.matrix(data.filtered), logratio = "CLR")

#used for spls and diablo
plsda.result <- plsda(data.microbiomic, storability, ncomp = 4, logratio = "CLR", scale = TRUE)
grid.keepX = c(seq(5,50, 2))
set.seed(30)
tune.splsda.result <- tune.splsda(data.microbiomic,Y = storability, ncomp = 4, logratio = 'CLR', test.keepX = grid.keepX, 
                                  validation = c('Mfold'), 
                                  folds = 5,
                                  dist = 'max.dist',
                                  nrepeat = 100,
                                  progressBar = TRUE)
ncomp <- tune.splsda.result$choice.ncomp$ncomp
keepX <- tune.splsda.result$choice.keepX[1:ncomp]

splsda.result <- splsda(data.microbiomic, Y = storability, ncomp = ncomp, keepX = keepX, logratio = 'CLR')

sel_ASV = c()
for (comp in 1:ncomp){
  loadings <- plotLoadings(splsda.result, comp = comp, method = 'mean', contrib = 'max')
  sel_ASV <- c(sel_ASV, rownames(loadings))
}

#use not mixmc pipelined data
unfiltered_data <- as.data.frame(t(df_asv))
ASV_cols <- (names(unfiltered_data) %in% sel_ASV)
data.microbiomic_small <- unfiltered_data[, ASV_cols]

asv_var <- nearZeroVar(data.microbiomic_small, freqCut = 75/13, uniqueCut = 32)
data.microbiomic_small <- data.microbiomic_small[,-asv_var$Position]

usethis::use_data(data.microbiomic, overwrite = TRUE) 
usethis::use_data(data.microbiomic_small, overwrite = TRUE) 
