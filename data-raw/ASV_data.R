## code to prepare `ASV_data` dataset goes here
library(readxl)
library(mixOmics)

# Micriobiome ASV data analysis

# Read ASV table
asv_table <- read.table("data-raw/asv_table_16S.csv", sep = ";")
df_asv <- as.data.frame(asv_table)
rownames(df_asv) <- asv_table[,1]
colnames(df_asv) <- asv_table[1,]
df_asv <- df_asv[-1,-1]
df_asv <- df_asv[,colnames(df_asv)!="taxonomy"]

# transpose asv matrix to get the ASV in columns
t_asv <- as.data.frame(t(df_asv))
colnames(t_asv) <- rownames(df_asv)
rownames(t_asv) <- colnames(df_asv)
t_asv <- t_asv[1:80,]
i <- c(1:4398)
t_asv[ , i] <- apply(t_asv[ , i], 2, function(x) as.numeric(as.character(x)))

# Read taxonomy metadata
taxonomy_asv <- read.table("data-raw/taxon_seq_table.csv", sep = ";") 
df_taxonomy <- as.data.frame(t(taxonomy_asv))
rownames(df_taxonomy) <- taxonomy_asv[1,]
colnames(df_taxonomy) <- taxonomy_asv[,1]
taxonomy <- df_taxonomy[-1,-1]

# Storability group
asv_storability <- readxl::read_excel("data-raw/map_file.xlsx", sheet = 1, col_names = TRUE)
Y = as.factor(asv_storability$Storability)
summary(Y)

# Adding the Offset
off_asv <- t_asv + 1
dim(off_asv)
sum(which(off_asv == 0)) # no zeros in data

# Pre-filtering
low.count.removal = function(data, percent=0.01){
  keep.asv = which(colSums(data)*100/(sum(colSums(data)))>percent)
  data.filter = data[,keep.asv]
  return(list(data.filter = data.filter, keep.asv = keep.asv))
}

result.filter <- low.count.removal(off_asv, percent = 0.01)
data.filter <- result.filter$data.filter
length(result.filter$keep.asv) #1515 variables kept after filtering
lib.size <- apply(data.filter,1,sum) # compute library size for mapped reads
barplot(lib.size) # we want relatively similar lib sizes, no obvious outlier samples

# Centered Log Ratio
data.clr <- mixOmics::logratio.transfo(as.matrix(data.filter), logratio = 'CLR')



usethis::use_data(data.filter,Y, overwrite = TRUE)
