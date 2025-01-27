###For this one I merged all the avarage colour variables to the respective avarage death per site. 

### Library

library(dplyr)

###Import data set

dt <- read.csv("data/raw/clean_toxdat.csv")
col <- read.csv("data/processed/col_dat.csv")
mt.dt <- read.csv("data/raw/processed_data_marilia.csv")

head(col)

#Filter CBGS

CGB <- dt[dt$Binomial == "Zizina otis", ]

CGB$SS <- paste(CGB$Site, CGB$Sex, sep = " ")

#Average colour data per site & sex

mt.dt <- mt.dt[mt.dt$Binomial == "Zizina otis", ]
mt.dt <- mt.dt[, c("ID", "Site", "Sex", "Climate")]

colnames(col)[colnames(col) == "MSPEC"] <- "ID"

col2 <- merge(col, mt.dt, by = "ID", all= TRUE) 
col2 <- col2[!is.na(col2$"R.fd"), ]
col2$SS <- paste(col2$Site, col2$Sex, sep = " ")

col3 <- col2 %>%
  group_by(SS) %>%
  summarise(across(
    where(is.numeric) & !c(ID, X), # Exclude 'ID' and 'X' if they're numeric
    mean, 
    na.rm = TRUE
  ))

# Merge CBG with colour data
CGB_col <- CGB %>%
  left_join(col3, by = "SS")

#Save

write.csv(CGB_col, "data/processed/CGB.csv", row.names = FALSE)
