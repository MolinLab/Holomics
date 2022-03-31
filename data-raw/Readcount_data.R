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

#for PLS-DA and DIABLO
sel_tran <- c("BVRB_2g046340", "BVRB_5g107280", "BVRB_4g077370", "BVRB_7g177270", "BVRB_6g134160", "BVRB_6g133990", "BVRB_2g032390", "BVRB_1g014110", "BVRB_6g155460", "BVRB_6g144370", "BVRB_5g106260", "BVRB_2g026930", "BVRB_2g043050", "BVRB_2g043190", "BVRB_7g180710", "BVRB_9g211050", "BVRB_7g162660", "BVRB_9g215340", "BVRB_7g162550", "BVRB_1g021670", "BVRB_2g023940", "BVRB_9g222850", "BVRB_1g011840", "BVRB_1g017530", "BVRB_3g049390", "BVRB_4g073470", "BVRB_8g183860", "BVRB_5g110420", "BVRB_7g169160","BVRB_1g007960", "BVRB_6g137610", "BVRB_9g205340", "BVRB_7g161240", "BVRB_8g186240", "BVRB_4g085890", "BVRB_1g014760", "BVRB_2g040110", "BVRB_1g012380", "BVRB_1g009810", "BVRB_1g002710", "BVRB_5g119570", "BVRB_4g095530", "BVRB_3g059880", "BVRB_4g093150", "BVRB_4g089630", "BVRB_7g167230", "BVRB_7g161280", "BVRB_6g130290", "BVRB_4g081350", "BVRB_5g122560")
tran_cols <- (names(data.transcriptomic) %in% sel_tran)
data.transcriptomic_small <- data.transcriptomic[, tran_cols]

usethis::use_data(data.transcriptomic, overwrite = TRUE) 
usethis::use_data(data.transcriptomic_small, overwrite = TRUE) 