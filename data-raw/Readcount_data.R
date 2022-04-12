#prepare transcriptomics data
library(readxl)
library(edgeR)
library(mixOmics)
source("data-raw/Labels_data.R")

# Read read count table
df_readcounts <- as.data.frame(readxl::read_excel("data-raw/t0_readcounts.xlsx", sheet = 1, col_names = TRUE))
rownames(df_readcounts) <- df_readcounts[,1]   #all rows, first column
df_readcounts <- df_readcounts[,-1]
df_readcounts <- df_readcounts[, grep("^V[1256]", names(df_readcounts))] #pick only columns from variety 1, 2, 5 and 6

# Filtering lowly expressed genes
dge_list <- edgeR::DGEList(counts = df_readcounts)
keep <- edgeR::filterByExpr(dge_list)
dge_list <- dge_list[keep, ]
filt <- as.data.frame(dge_list$counts)  #still about 14000 genes remaining

# Filtering by Median Absolute Deviation
data.transcriptomic <- as.data.frame(t(CancerSubtypes::FSbyMAD(filt, cut.type = "topk", value = 10000)))

#for PLS-DA and DIABLO
plsda.result <- plsda(data.transcriptomic, storability, ncomp = 4, scale = TRUE)
grid.keepX <- c(5:10,  seq(20, 100, 10))
set.seed(30)
tune.splsda.result <- tune.splsda(data.transcriptomic,Y = storability, ncomp = 4, test.keepX = grid.keepX, 
                                  validation = c('Mfold'), 
                                  folds = 5,
                                  dist = 'max.dist',
                                  nrepeat = 100,
                                  progressBar = TRUE)
ncomp <- tune.splsda.result$choice.ncomp$ncomp
keepX <- tune.splsda.result$choice.keepX[1:ncomp]

splsda.result <- splsda(data.transcriptomic, Y = storability, ncomp = ncomp, keepX = keepX)

sel_tran = c()
for (comp in 1:ncomp){
  loadings <- plotLoadings(splsda.result, comp = comp, method = 'mean', contrib = 'max')
  sel_tran <- c(sel_tran, rownames(loadings))
}

tran_cols <- (names(data.transcriptomic) %in% sel_tran)
data.transcriptomic_small <- data.transcriptomic[, tran_cols]

usethis::use_data(data.transcriptomic, overwrite = TRUE) 
usethis::use_data(data.transcriptomic_small, overwrite = TRUE) 