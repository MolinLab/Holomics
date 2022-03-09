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

data.microbiomic <- low.count.removal(t_asv, percent = 0.01)

usethis::use_data(data.microbiomic, overwrite = TRUE) 