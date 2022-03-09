#prepare labels for data
library(readxl)

#storability
df <- as.data.frame(readxl::read_excel("data-raw/Metatable_MultiOmics.xlsx", sheet = 1, col_names = TRUE))
df_t0 <- df[df$Timepoint == "T0",]  #get only data from timepoint 0
df_var <- df_t0[grep("[1256]", df_t0$Variety),]

storability <- df_var$Storability
usethis::use_data(storability,overwrite = TRUE)