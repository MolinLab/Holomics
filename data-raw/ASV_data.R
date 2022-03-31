#prepare asv data
library(readxl)

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

#used for pca and pls-da
data.microbiomic <- low.count.removal(t_asv, percent = 0.01)

#used for spls and diablo
sel_ASV <- c("ASV_643", "ASV_525", "ASV_861", "ASV_955", "ASV_745", "ASV_1505", "ASV_296", "ASV_541", "ASV_298", "ASV_611", "ASV_587", "ASV_1215", "ASV_649", "ASV_1279", "ASV_1229", "ASV_1568", "ASV_585", "ASV_1690", "ASV_350", "ASV_778", "ASV_683", "ASV_418", "ASV_1256", "ASV_1442", "ASV_1045", "ASV_1161", "ASV_152", "ASV_662", "ASV618", "ASV_44", "ASV_229", "ASV_1827")
ASV_cols <- (names(data.microbiomic) %in% sel_ASV)
data.microbiomic_small <- data.microbiomic[, ASV_cols]

usethis::use_data(data.microbiomic, overwrite = TRUE) 
usethis::use_data(data.microbiomic_small, overwrite = TRUE) 