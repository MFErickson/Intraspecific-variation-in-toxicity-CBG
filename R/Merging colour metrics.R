#Use this script to get all text files with the same from a folder within the computer and merge them together

wd <- setwd("C:/Users/46090444/OneDrive/AAAAAA PhD/Scoring aposematism/Images for ImageGeorge/CGBs")

# Input file name
nm <- "Human_Colour_Data.txt"


files <- list.files(pattern = nm, recursive = TRUE, full.names = TRUE)
l <- lapply(files, read.table, header = TRUE, sep = "\t")

df <- do.call(rbind, l)

# Remove "rename" from MSPEC column
df$MSPEC <- sub("rename", "", df$MSPEC)

# Remmove DMG rows
df <- df[df$ROI != "dmg", ]

# Check for duplicate ROI values
mspecRoi <- paste(df$MSPEC, df$ROI)
print(mspecRoi[duplicated(mspecRoi)])

# Convert from long to wide format
cols <- names(df)
vars <- cols[!cols %in% c("MSPEC", "FILE", "ROI")]
wide <- reshape(df, direction = "wide", v.names = vars, timevar = "ROI", idvar = "MSPEC", drop = "FILE")

# Save
result <- sub(".txt", "-merged.csv", nm)
write.csv(wide, result, row.names = FALSE)

setwd(wd)


##################################
hmn <-read.csv("C:/Users/46090444/OneDrive/GitHub/Intraspecific-variation-in-toxicity-CBG/data/raw/Human_Colour_Data-merged.csv")
brd_s <- read.csv("C:/Users/46090444/OneDrive/GitHub/Intraspecific-variation-in-toxicity-CBG/data/raw/Batch_RNL_Bluetit 0.05_Simple-merged.csv")
brd_p <- read.csv("C:/Users/46090444/OneDrive/GitHub/Intraspecific-variation-in-toxicity-CBG/data/raw/Batch_RNL_Bluetit 0.05_Pattern-merged.csv")

Col_dat <- Reduce(function(x, y) merge(x, y, by = "MSPEC", all = TRUE), list(hmn, brd_s, brd_p))


write.csv(Col_dat, "C:/Users/46090444/OneDrive/GitHub/Intraspecific-variation-in-toxicity-CBG/data/processed/col_dat.csv")

write.csv(wide, result, row.names = FALSE)
