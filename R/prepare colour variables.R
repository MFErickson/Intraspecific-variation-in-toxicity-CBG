# For this one I will get only Saturation and luminescence variables and avarage them, then I add to the spreadsheet preserving the number of rows from toxicity assays


# Library

library(dplyr)

#Import
dt <- read.csv("data/raw/clean_toxdat.csv")
col <- read.csv("data/processed/col_dat.csv")
mt.dt <- read.csv("data/raw/processed_data_marilia.csv")

## Prep for merge
CGB <- dt[dt$Binomial == "Zizina otis", ]
CGB$SS <- paste(CGB$Site, CGB$Sex, sep = " ")


mt.dt <- mt.dt[mt.dt$Binomial == "Zizina otis", ]
mt.dt <- mt.dt[, c("ID", "Site", "Sex", "Climate")]
mt.dt$SS <- paste(mt.dt$Site, mt.dt$Sex, sep = " ")

col <- col[, c("MSPEC", "Lum_Mean.hd", "Lum_StdDev.hd", "Sat_Mean.hd", "Sat_StdDev.hd", 
             "Lum_Mean.fd", "Lum_StdDev.fd", "Sat_Mean.fd", "Sat_StdDev.fd", 
             "Lum_Mean.hv", "Lum_StdDev.hv", "Sat_Mean.hv", "Sat_StdDev.hv", 
             "Lum_Mean.fv", "Lum_StdDev.fv", "Sat_Mean.fv", "Sat_StdDev.fv")]
colnames(col)[colnames(col) == "MSPEC"] <- "ID"

# merge colour & metadata
col2 <- merge(col, mt.dt, by = "ID", all= TRUE) 
col2 <- col2[!is.na(col2$"Lum_Mean.hd"), ] #removes everything that does not have colour metrics

# Sum & avarage

col3 <- col2 %>%
  group_by(SS) %>%
  summarise(across(
    where(is.numeric) & !c(ID), # Exclude 'ID' and 'X' if they're numeric
    mean, 
    na.rm = TRUE
  ))


CGB_col <- CGB %>%
  left_join(col3, by = "SS")


#Save

write.csv(CGB_col, "data/processed/CGB_col.csv", row.names = FALSE)
