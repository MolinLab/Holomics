#prepare transcriptomics data
library(readxl)
library(edgeR)

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

usethis::use_data(data.transcriptomic, overwrite = TRUE) 